import Test.DocTest

main = doctest [ "-isrc/Network/InnerChange"
               , "src/Network/InnerChange/SoapTypes.hs" ]
