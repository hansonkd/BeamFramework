{-# LANGUAGE OverloadedStrings #-}

module Web.Beam.Utils.Cookie
  ( setCookie
  , setCookie'
  , getCookie
  , getCookies
  , removeCookie
  ) where

import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import Data.Time.Calendar
import Data.Time.Clock
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Blaze.ByteString.Builder as B

import Web.Cookie
import Web.Scotty.Trans

import Web.Beam.Types

renderCookie' = T.decodeUtf8 . B.toLazyByteString . renderSetCookie
lazyToStrict = BS.concat . BSL.toChunks

getDefaultCookie :: BeamActionM a SetCookie
getDefaultCookie = return def -- Populate with settings...

setCookie :: Text -> Text -> BeamActionM a ()
setCookie k v = getDefaultCookie >>= (setCookie' k v)

setCookie' :: Text -> Text -> SetCookie -> BeamActionM a ()
setCookie' k v sc = setHeader "Set-Cookie" cookieText
  where cookie = sc { setCookieName = lazyToStrict $ T.encodeUtf8 k
                    , setCookieValue = lazyToStrict $ T.encodeUtf8 v
                    }
        cookieText = renderCookie' cookie
        
getCookies :: BeamActionM a CookiesText
getCookies = (reqHeader "Cookie") >>= (return . parseFunc . (fromMaybe T.empty))
  where parseFunc = parseCookiesText . lazyToStrict . T.encodeUtf8

getCookie :: Text -> BeamActionM a (Maybe Text)
getCookie k = getCookies >>= 
    (return . (fmap T.fromStrict) . (lookup (T.toStrict k)))

removeCookie :: Text -> BeamActionM a ()
removeCookie k = do
  defCookie <- getDefaultCookie
  let utcLongAgo = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)
      expiredCookie = defCookie {setCookieExpires = Just utcLongAgo}
  setCookie' k T.empty expiredCookie