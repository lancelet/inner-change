module Main where

import Network.InnerChange.Proxy (runConduitProxyApp)

main :: IO ()
main = runConduitProxyApp

{-
Should be able to reverse-proxy some other server, eg:
  ich rproxy --host exchange.url.com --port 8080 --ews-port 8080

And also serve a test Exchange instance, eg:
  ich serve --port 8080 --ews-port 8080 --rest-port 2319
-}
