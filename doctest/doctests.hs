module Main (main) where

import Test.DocTest

main :: IO ()
main =
    doctest
        [ "-isrc"
        , "src/"
        , "-XTypeOperators"
        , "-XGeneralizedNewtypeDeriving"
        , "-XStandaloneDeriving"
        , "-XMultiParamTypeClasses"
        , "-XFlexibleContexts"
        , "-XFlexibleInstances"
        , "-XGADTs"
        , "-XTypeFamilies"
        , "-XTypeFamilyDependencies"
        , "-XFunctionalDependencies"
        , "-XRankNTypes"
        , "-XUnicodeSyntax"
        , "-XDeriveFoldable"
        , "-XDeriveFunctor"
        , "-XDeriveTraversable"
        , "-XConstraintKinds"
        , "-XScopedTypeVariables"
        , "-XKindSignatures"
        , "-XDataKinds"
        , "-XBangPatterns"]
