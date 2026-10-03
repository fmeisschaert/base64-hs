module Main (main) where

import Control.Monad (unless)
import System.Exit (exitFailure)

import Base64

-- Each test has a name and whether it passed.
tests :: [(String, Bool)]
tests =
    [ ("decode is lazy on infinite strict input",
        take 6 (decode [] (cycle "TWFu")) == [77,97,110,77,97,110])
    , ("decode is lazy on infinite lenient input",
        take 6 (decode [Lenient] (cycle "TW\nFu ")) == [77,97,110,77,97,110])
    , ("decode doesn't force input past a complete group",
        take 3 (decode [] ("TWFu" ++ undefined)) == [77,97,110])
    , ("decodeResult stops after the first padded value",
        let r = decodeResult [Padding] "TQ==TWE="
        in decoded r == [77] && decodeRest r == "TWE=" && decodeState r == (0,0))
    , ("decodeResult keeps a trailing single character",
        let r = decodeResult [Padding] "TWFuT"
        in decoded r == [77,97,110] && decodeRest r == "T" && decodeState r == (1,19))
    , ("encode and decode round-trip",
        let bs = [0..255]
        in decode [Padding] (encode [Padding] bs) == bs)
    ]

main :: IO ()
main = do
    let failures = [name | (name, ok) <- tests, not ok]
    mapM_ (putStrLn . ("FAIL: " ++)) failures
    putStrLn $ show (length tests - length failures) ++ "/" ++ show (length tests)
        ++ " tests passed"
    unless (null failures) exitFailure
