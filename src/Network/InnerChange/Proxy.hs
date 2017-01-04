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


runConduitProxyApp :: IO ()
runConduitProxyApp = do
    manager <- Client.newManager TLS.tlsManagerSettings
    -- TODO: pass-in the remote host and port
    host <- (Host . Text.pack) <$> getEnv "REMOTE_HOST"
    let
        port = 2319
        httpsPort = 443
        app = conduitProxyApp manager host httpsPort
        warpSettings = Warp.setPort port
                     . Warp.setHost "*"
                     $ Warp.setNoParsePath True Warp.defaultSettings
    Warp.runSettings warpSettings app


conduitProxyApp :: Client.Manager
                -> Host
                -> Warp.Port
                -> Wai.Application
conduitProxyApp manager host port request sendResponse = do
    request' <- toSimpleRequest host port request
    traceIO "Request:"
    traceIO $ show request'
    let
        errorResponse :: SomeException -> Wai.Response
        errorResponse = defaultExceptionResponse . toException
    handle (sendResponse . errorResponse) $
        Client.withResponse request' manager $ \response -> do
        traceIO "Responding with a chunk:"
        traceIO $ show (HConduit.responseStatus response)
        traceIO $ show (HConduit.responseHeaders response)
        sendResponse (toWaiConduitResponse response)

defaultExceptionResponse :: SomeException -> Wai.Response
defaultExceptionResponse e =
    Wai.responseLBS Types.internalServerError500
      [ (Types.hContentType, "text/plain; charset=utf-8") ]
      $ LBS.fromChunks [ BS.pack $ show e ]


toWaiConduitResponse :: Client.Response Client.BodyReader -> WConduit.Response
toWaiConduitResponse cr = WConduit.responseSource status headers body
  where
    status  = HConduit.responseStatus cr 
    headers = HConduit.responseHeaders cr
    body = Conduit.mapOutput (Conduit.Chunk . BBB.fromByteString)
                             . HCConduit.bodyReaderSource
                             $ HConduit.responseBody cr


toSimpleRequest :: Host                  -- ^ destination host
                -> Warp.Port             -- ^ destination port
                -> Wai.Request           -- ^ incoming Wai request
                -> IO (HSimple.Request)  -- ^ outgoing client request
toSimpleRequest (Host host) port wr = do
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
        dropRequestHeaders (k,_) = k `notElem` [ "host" ]

    -- translate other parameters of the request
    return HConduit.defaultRequest
            { HConduit.host           = Encoding.encodeUtf8 host
            , HConduit.port           = port
            , HConduit.secure         = True
            , HConduit.method         = Wai.requestMethod wr
            , HConduit.path           = Wai.rawPathInfo wr
            , HConduit.queryString    = Wai.rawQueryString wr
            , HConduit.requestHeaders = headers
            , HConduit.decompress     = const False
            , HConduit.requestBody    = body
            }
