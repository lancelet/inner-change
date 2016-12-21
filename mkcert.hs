#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Turtle.Prelude

main :: IO ExitCode
main = do
    tlsDirExists <- testdir ".tls"
    if (not tlsDirExists)
        then
        do
            echo "Creating TLS certificate and key in .tls directory"
            mkdir ".tls"
            proc "openssl"
                 [ "req"
                 , "-x509"
                 , "-newkey"
                 , "rsa:4096"
                 , "-keyout"
                 , ".tls/key.pem"
                 , "-out"
                 , ".tls/cert.pem"
                 , "-days"
                 , "365"
                 , "-nodes"
                 , "-subj"
                 , "/CN=localhost"
                 ] empty
        else
        do
            echo ".tls directory exists - exiting"
            return ExitSuccess
