{-# LANGUAGE OverloadedStrings #-}

module Network.InnerChange.Proxy where

import           Control.Exception           (SomeException, toException, handle)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy        as LBS
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Text.Encoding          as Encoding
import           Debug.Trace                 (trace, traceIO)
import           System.Environment          (getEnv)

import qualified Blaze.ByteString.Builder    as BBB
import qualified Data.Conduit                as Conduit

import qualified Network.HTTP.Client         as Client
import qualified Network.HTTP.Client.Conduit as HCConduit
import qualified Network.HTTP.Client.TLS     as TLS
import qualified Network.HTTP.Conduit        as HConduit
import qualified Network.HTTP.Simple         as HSimple
import qualified Network.HTTP.Types          as Types
import qualified Network.HTTP.Types.Header   as Types
import qualified Network.Wai                 as Wai
import qualified Network.Wai.Conduit         as WConduit
import qualified Network.Wai.Handler.Warp    as Warp

newtype Host = Host Text

{-
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
-}

runConduitProxyApp :: IO ()
runConduitProxyApp = do
    manager <- Client.newManager TLS.tlsManagerSettings
    host <- (Host . Text.pack) <$> getEnv "REMOTE_HOST"
    let
        port = 2319
        httpsPort = 443
        app = conduitProxyApp manager host httpsPort
        warpSettings = Warp.setPort port
                     . Warp.setHost "*"
                     $ Warp.setNoParsePath True Warp.defaultSettings
    Warp.runSettings warpSettings app

{-
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
-}

conduitProxyApp :: Client.Manager
                -> Host
                -> Warp.Port
                -> Wai.Application
conduitProxyApp manager host port request sendResponse = do
    request' <- toSimpleRequest host port request
    traceIO "Request:"
    traceIO $ show request'
    -- TODO: deal with errors (handle...)
    let
        errorResponse :: SomeException -> Wai.Response
        errorResponse = defaultExceptionResponse . toException
    handle (sendResponse . errorResponse) $
        Client.withResponse request' manager $ \res -> do
        let
            status = HConduit.responseStatus res
            body = Conduit.mapOutput (Conduit.Chunk . BBB.fromByteString)
                                     . HCConduit.bodyReaderSource
                                     $ HConduit.responseBody res
            headers = HConduit.responseHeaders res
        traceIO "Responding with a chunk:"
        traceIO $ show status
        traceIO $ show headers
        sendResponse $ WConduit.responseSource status headers body

{-
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
-}

defaultExceptionResponse :: SomeException -> Wai.Response
defaultExceptionResponse e =
    Wai.responseLBS Types.internalServerError500
      [ (Types.hContentType, "text/plain; charset=utf-8") ]
      $ LBS.fromChunks [ BS.pack $ show e ]

toSimpleRequest :: Host                  -- ^ destination host
                -> Warp.Port             -- ^ destination port
                -> Wai.Request           -- ^ incoming Wai request
                -> IO (HSimple.Request)  -- ^ outgoing client request
toSimpleRequest (Host host) port wr = do

    -- parse base request from the passed-in host and raw query string
    -- TODO: probably don't parseRequest, but instead built it up
    let
        hostbs = Encoding.encodeUtf8 host
        -- baseRequestBs = hostbs <> Wai.rawQueryString wr
    -- baseRequest <- (HConduit.parseRequest . BS.unpack) baseRequestBs

    -- configure body
    let
        srb = WConduit.sourceRequestBody wr
        body = case Wai.requestBodyLength wr of
            Wai.ChunkedBody ->
                HConduit.requestBodySourceChunkedIO srb
            Wai.KnownLength l ->
                HConduit.requestBodySourceIO (fromIntegral l) srb

    -- configure headers
    let
        headers = filter dropRequestHeaders (Wai.requestHeaders wr)
        dropRequestHeaders (k,_) = k `notElem`
            [ "host"
            -- remove content-encoding and content-length if decompressed
            {-, "content-encoding"
              , "content-length" -} ]

    -- translate other parameters of the request
    let
        request = HConduit.defaultRequest
            { HConduit.host           = hostbs
            , HConduit.port           = port
            , HConduit.secure         = True
            , HConduit.method         = Wai.requestMethod wr
            , HConduit.path           = Wai.rawPathInfo wr
            , HConduit.queryString    = Wai.rawQueryString wr
            , HConduit.requestHeaders = headers
            , HConduit.redirectCount  = 0  -- ?
            , HConduit.decompress     = const False
            , HConduit.requestBody    = body
            }
    
    return request
