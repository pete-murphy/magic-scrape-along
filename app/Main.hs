{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Applicative (optional)
import Control.Arrow ((<<<), (>>>))
import Control.Concurrent (QSem, forkIO, newEmptyMVar, newQSem, putMVar, signalQSem, takeMVar, threadDelay, waitQSem)
import Control.Lens (ifor_)
import Control.Monad (forever, replicateM_, unless)
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState (..), evalStateT)
import Data.Aeson (ToJSON, encode)
import Data.ByteString qualified as BS
import Data.Char (isUpper, toLower)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IORef (IORef, modifyIORef', newIORef)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String (IsString)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Time (Day, UTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Deriving.Aeson (CustomJSON (..), FieldLabelModifier, StringModifier (..), StripPrefix)
import Example qualified
import GHC.Generics (Generic)
import GHC.IO.Handle.FD (withFile)
import GHC.IORef (readIORef)
import GHC.TypeLits (Symbol)
import Network.HTTP.Client (Manager, ManagerSettings (..), Request (..), Response (..), brRead, newManager, parseRequest, withResponse)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hContentLength, hUserAgent)
import Pipes (Consumer, Producer, await, each, runEffect, (>->))
import Pipes.Prelude qualified as P
import Pipes.Safe (MonadThrow)
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.Directory (createDirectory, getCurrentDirectory)
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode))
import Text.HTML.Scalpel (Config (..), attr, chroot, chroots, defaultDecoder, hasClass, scrapeURLWithConfig, text, (//), (@:), (@=))
import Text.Read (readMaybe)
import Worker qualified

baseURL :: PageURL
baseURL = "https://www.magicreadalong.com"

audioDirectory :: FilePath
audioDirectory = "audio-4"

-- main = Example.main''
main = Worker.main

main' :: IO ()
main' = do
  let settings =
        tlsManagerSettings
          { managerModifyRequest = \req -> do
              req' <- managerModifyRequest tlsManagerSettings req
              pure (req' {requestHeaders = (hUserAgent, "scalpel/0.6.2.1") : requestHeaders req'})
          }

  manager <- newManager settings
  createDirectory audioDirectory
  cwd <- getCurrentDirectory
  fileDownloadProgress <- newIORef Map.empty

  _ <- forkIO do
    forever do
      threadDelay 100_000
      clearScreen
      setCursorPosition 0 0
      p <- readIORef fileDownloadProgress
      ifor_ p \(_, filename) progress -> do
        putStrLn
          ( fill 40 '.' (take 40 filename)
              <> ": "
              <> if progress == 1
                then "✅"
                else renderProgress progress <> " " <> take 5 (show (progress * 100)) <> "%"
          )
  qSem <- newQSem 10

  flip runReaderT (Env manager cwd qSem fileDownloadProgress) do
    runEffect do
      scraper "/"
        >-> P.tee (P.map podcastAudioURL >-> replicateM_ 3 downloadAudioURL)
        >-> P.map (encode >>> TL.decodeUtf8 >>> TL.unpack)
        >-> writeJSON

fill :: Int -> a -> [a] -> [a]
fill n x xs = xs <> replicate (n - length xs) x

renderProgress :: Double -> String
renderProgress n = "[" <> replicate (round (n * 20)) '█' <> replicate (20 - round (n * 20)) ' ' <> "]"

data Env = Env
  { envManager :: Manager,
    envCWD :: FilePath,
    envQSem :: QSem,
    envFileDownloadProgress :: IORef (Map (UTCTime, String) Double)
  }

scraper ::
  (MonadIO m, MonadFail m, MonadReader Env m) =>
  PageURL ->
  Producer Podcast m ()
scraper =
  fix \loop path -> do
    manager <- asks (Just <<< envManager)
    let config = Config {manager, decoder = defaultDecoder}
        scrape = scrapeURLWithConfig @String config (unPageURL (baseURL <> path))
    Just ((podcasts, nextPath) :: ([Podcast], Maybe PageURL)) <- liftIO do
      scrape do
        podcasts <- chroots "article" do
          podcastTitle <- text "h1"
          podcastURL <- attr "href" ("h1" // "a")
          podcastAudioURL <- AudioURL <$> attr "data-url" ("div" @: [hasClass "sqs-audio-embed"])
          podcastDate <- iso8601ParseM =<< attr "datetime" ("time" @: [hasClass "published"])
          podcastLinks <- chroot "ul" do
            chroots "li" do
              linkTitle <- text "a"
              linkURL <- PageURL <$> attr "href" "a"
              pure Link {..}
          podcastTags <- chroot ("footer" // "div" @: [hasClass "tags"]) do
            chroots "a" do
              tagTitle <- text "a"
              tagURL <- attr "href" "a"
              pure Tag {..}
          pure Podcast {..}

        nextPath <- optional (PageURL <$> attr "href" ("a" @: ["id" @= "nextLink"]))

        pure (podcasts, nextPath)

    each podcasts
    for_ nextPath loop

writeJSON ::
  (MonadReader Env m, MonadIO m) =>
  Consumer String m ()
writeJSON = do
  cwd <- asks envCWD
  forever do
    podcastJSON <- await
    liftIO do
      appendFile
        (cwd </> audioDirectory </> "podcasts.ndjson")
        (podcastJSON ++ "\n")

downloadAudioURL ::
  (MonadReader Env m, MonadIO m, MonadThrow m) =>
  Consumer AudioURL m ()
downloadAudioURL = do
  forever do
    qSem <- asks envQSem
    lock <- liftIO newEmptyMVar

    url <- unAudioURL <$> await
    now <- liftIO getCurrentTime
    let filename =
          TL.unpack (last (TL.splitOn "/" (TL.pack url)))
        key = (now, filename)

    req <- parseRequest url
    manager <- asks envManager
    cwd <- asks envCWD
    progress <- asks envFileDownloadProgress

    _ <- liftIO do
      forkIO do
        withFile (cwd </> audioDirectory </> filename) WriteMode \h -> do
          withResponse req manager \resp -> do
            flip evalStateT 0 do
              let bodyReader = responseBody resp
                  maybeContentLength =
                    responseHeaders resp
                      & lookup hContentLength
                      <&> (BS.fromStrict >>> TL.decodeUtf8 >>> TL.unpack)
                      >>= readMaybe @Int

              fix \loop -> do
                bs <- liftIO (brRead bodyReader)
                downloaded <- get
                let nextDownloaded = downloaded + BS.length bs
                for_ maybeContentLength \contentLength -> liftIO do
                  let ratio = fromIntegral nextDownloaded / fromIntegral contentLength
                  modifyIORef' progress (Map.insert key ratio)
                put nextDownloaded
                unless (BS.null bs) do
                  liftIO (BS.hPut h bs)
                  loop
        signalQSem qSem

    liftIO (waitQSem qSem)

data Podcast = Podcast
  { podcastTitle :: String,
    podcastURL :: String,
    podcastDate :: Day,
    podcastAudioURL :: AudioURL,
    podcastLinks :: [Link],
    podcastTags :: [Tag]
  }
  deriving stock (Show, Generic)
  deriving (ToJSON) via MyJSON "podcast" Podcast

data Link = Link
  { linkTitle :: String,
    linkURL :: PageURL
  }
  deriving stock (Show, Generic)
  deriving (ToJSON) via MyJSON "link" Link

data Tag = Tag
  { tagTitle :: String,
    tagURL :: String
  }
  deriving stock (Show, Generic)
  deriving (ToJSON) via MyJSON "tag" Tag

newtype AudioURL = AudioURL {unAudioURL :: String}
  deriving stock (Show)
  deriving newtype (ToJSON, IsString)

newtype PageURL = PageURL {unPageURL :: String}
  deriving stock (Show)
  deriving newtype (ToJSON, IsString, Semigroup, Monoid)

type MyJSON (prefix :: Symbol) = CustomJSON '[FieldLabelModifier '[StripPrefix prefix, ToLower]]

data ToLower

instance StringModifier ToLower where
  -- Have not tested this
  getStringModifier str =
    case span isUpper str of
      ("", post) -> post
      (pre, "") -> map toLower pre
      ([c], post) -> toLower c : post
      (reverse -> (c : cs), post) -> map toLower (reverse cs) ++ [c] ++ post
      (x, y) -> error (show (x, y))