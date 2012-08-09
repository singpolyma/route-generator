{-# LANGUAGE OverloadedStrings #-} -- Whatever, it's just an example

module Application where

import Network.Wai
import Network.HTTP.Types (ok200, notFound404)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LZ

import Data.Monoid (mappend)

textToUTF8 txt = LZ.fromChunks [T.encodeUtf8 txt]

showUTF8 :: (Show a) => a -> LZ.ByteString
showUTF8 = textToUTF8 . T.pack . show

home _ = return $ responseLBS ok200 [("Content-Type", "text/plain")] "Hello World"

test val _ = return $ responseLBS ok200 [("Content-Type", "text/plain")] (textToUTF8 val)

on404 _ = return $ responseLBS notFound404 [("Content-Type", "text/plain")] "Not Found"

test2 :: Integer -> Application
test2 val _ = return $ responseLBS ok200 [("Content-Type", "text/plain")] (showUTF8 val)

test3 val env = return $ responseLBS ok200 [("Content-Type", "text/plain")] (textToUTF8 val `mappend` "\n\n" `mappend` showUTF8 (pathInfo env))
