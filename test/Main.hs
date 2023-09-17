{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Main (main) where
import Kisevalter.KisevalterTypes (KisevalterManifest, Printable (PrintBinaryWithHash))
import Kisevalter.TemplateHaskell.Interface (listLiteralThroughTH)
import Data.String (IsString)

main :: IO ()
main = print asset

asset :: [KisevalterManifest String String]
asset = $(listLiteralThroughTH [PrintBinaryWithHash ("hello" :: String) "./test/assets/LICENSE"])