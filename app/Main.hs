{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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

import Conduit (MonadThrow)
import Control.Applicative (optional)
import Control.Arrow ((<<<), (>>>))
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks)
import Data.Aeson (ToJSON, encode)
import Data.ByteString qualified as BS
import Data.Char (isUpper, toLower)
import Data.Foldable (for_)
import Data.String (IsString)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Time (Day)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Deriving.Aeson (CustomJSON (..), FieldLabelModifier, StringModifier (..), StripPrefix)
import GHC.Generics (Generic)
import GHC.IO.Handle.FD (withFile)
import GHC.TypeLits (Symbol)
import Network.HTTP.Client (Manager, ManagerSettings (..), Request (..), Response (..), brRead, newManager, parseRequest, withResponse)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hUserAgent)
import Pipes (Pipe, Producer, await, each, runEffect, (>->))
import Pipes.Prelude qualified as P
import System.IO (IOMode (WriteMode))
import Text.HTML.Scalpel (Config (..), attr, chroot, chroots, defaultDecoder, hasClass, scrapeURLWithConfig, text, (//), (@:), (@=))

main :: IO ()
main = do
  let settings =
        tlsManagerSettings
          { managerModifyRequest = \req -> do
              req' <- managerModifyRequest tlsManagerSettings req
              pure (req' {requestHeaders = (hUserAgent, "scalpel/0.6.2.1") : requestHeaders req'})
          }

  manager <- newManager settings

  flip runReaderT (Env manager) do
    runEffect do
      scraper "/"
        >-> P.tee
          ( P.map podcastAudioURL
              >-> downloadAudioURL
              >-> P.map (BS.fromStrict >>> TL.decodeUtf8 >>> TL.unpack)
              >-> P.stdoutLn
          )
        >-> P.map (encode >>> TL.decodeUtf8 >>> TL.unpack)
        >-> P.stdoutLn

newtype Env = Env {envManager :: Manager}

scraper ::
  (MonadIO m, MonadFail m, MonadReader Env m) =>
  PageURL ->
  Producer Podcast m ()
scraper =
  fix \loop path -> do
    manager <- asks (Just . envManager)
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
              linkURL <- (mappend baseURL <<< PageURL) <$> attr "href" "a"
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

baseURL :: PageURL
baseURL = "https://www.magicreadalong.com"

downloadAudioURL ::
  (MonadReader Env m, MonadIO m, MonadThrow m) =>
  Pipe AudioURL BS.ByteString m ()
downloadAudioURL = forever do
  url <- unAudioURL <$> await
  liftIO (threadDelay 100_000)
  let filename =
        TL.unpack (last (TL.splitOn "/" (TL.pack url)))

  req <- parseRequest url
  manager <- asks envManager

  liftIO do
    putStrLn ("Downloading " <> filename)
    withFile filename WriteMode \h -> do
      withResponse req manager \(responseBody -> bodyReader) -> do
        fix \loop -> do
          bs <- brRead bodyReader
          if BS.null bs
            then pure ()
            else do
              BS.hPut h bs
              loop

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