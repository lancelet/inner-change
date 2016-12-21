{-# LANGUAGE OverloadedStrings #-}

module Network.InnerChange.Proxy where

import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as LBS
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Encoding
import           Debug.Trace               (trace, traceIO)

import qualified Network.HTTP.Client       as Client
import qualified Network.HTTP.Client.TLS   as TLS
import qualified Network.HTTP.Conduit      as HConduit
import qualified Network.HTTP.Types        as Types
import qualified Network.HTTP.Types.Header as Types
import qualified Network.Wai               as Wai
import qualified Network.Wai.Handler.Warp  as Warp

newtype Host = Host Text

runProxyApp :: IO ()
runProxyApp = do
    manager <- Client.newManager TLS.tlsManagerSettings
    let
        port = 2319
        host = Host "www.blender.org"
        httpsPort = 443
        app = proxyApp manager host httpsPort
        warpSettings = Warp.setPort port
                     . Warp.setHost "*"
                     $ Warp.setNoParsePath True Warp.defaultSettings
    Warp.runSettings warpSettings app

proxyApp :: Client.Manager
         -> Host
         -> Warp.Port
         -> Wai.Application
proxyApp manager host port request sendResponse = do
    request' <- translateRequest host port request
    traceIO "Request:"
    traceIO $ show request'
    response' <- Client.httpLbs request' manager
    traceIO "Response:"
    traceIO $ show response'
    let response = translateResponse response'
    sendResponse response

translateResponse :: Client.Response LBS.ByteString
                  -> Wai.Response
translateResponse cr = Wai.responseLBS status headers body
  where
    status  = Client.responseStatus cr
    headers = filter dropResponseHeaders $ Client.responseHeaders cr
    body    = Client.responseBody cr
    dropResponseHeaders (k,_) = k `notElem` ["content-encoding"]

translateRequest :: Host                 -- ^ destination host
                 -> Warp.Port            -- ^ destination port
                 -> Wai.Request          -- ^ incoming Wai request
                 -> IO (Client.Request)  -- ^ outgoing client request
translateRequest (Host host) port wr = do
    body <- HConduit.RequestBodyBS
            <$> LBS.toStrict
            <$> Wai.strictRequestBody wr
    let
        hostbs = Encoding.encodeUtf8 host
        headers = filter dropRequestHeaders (Wai.requestHeaders wr)
        dropRequestHeaders (k,_) = k `notElem` ["host"]
        request' = Client.defaultRequest
            { Client.method         = Wai.requestMethod wr
            , Client.secure         = True
            , Client.host           = hostbs
            , Client.port           = port
            , Client.path           = Wai.rawPathInfo wr
            , Client.requestHeaders = headers
            , Client.queryString    = Wai.rawQueryString wr
            , Client.requestBody    = body
            }
    return request'
