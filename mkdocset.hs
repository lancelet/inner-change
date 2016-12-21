#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Turtle.Prelude

main :: IO ExitCode
main = do
    exists <- testdir "./inner-change.docset"
    when exists $ do
        echo "Docset already exists - removing"
        rmtree "./inner-change.docset"
    shell "stack exec -- haddocset -t inner-change.docset create" empty
    shell "stack build --haddock" empty
    shell ( "stack exec -- haddocset -t inner-change.docset "
            <> "add $(stack path --snapshot-pkg-db)/*.conf" ) empty
    shell ( "stack exec -- haddocset -t inner-change.docset "
            <> "add $(stack path --local-pkg-db)/*.conf" ) empty
