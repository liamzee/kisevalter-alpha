{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}

module Kisevalter.Utilities.ManifestPrinter (appendToDestFromList) where

import Control.Exception (IOException, handle)
import Crypto.Hash (SHA256 (SHA256), hashWith)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.ByteString qualified as BS
import Data.Foldable (Foldable (fold))
import Data.List (intersperse)
import Data.String (fromString)
import Kisevalter.KisevalterTypes
    ( FileToBeEncoded
    , LocalFile
    , Printable 
        ( PrintBinaryAndLocalFileWithoutHash
        , PrintBinaryWithHash
        , PrintBinaryWithHashAndLocalFile
        , PrintBinaryWithoutHash)
    )

{- | This module is aimed at providing a way
    for users to print out a manifest containing a file to disk,
    which they can then copy paste into their files
    as literals, allowing them to avoid usage
    of TemplateHaskell.
-}

-- | A synonym to improve readability.

type DestinationFile = FilePath

{- | As it says on the label.
    The IO function is keyed such that it will automatically
    go to the end of the specified file, append a newline, then
    output the designed output.
-}
appendToDestFromList ::
    forall assetId.
    (Show assetId) =>
    DestinationFile ->
    [Printable assetId] ->
    IO ()
appendToDestFromList destinationFile input = do
    BS.appendFile destinationFile "\n    [    "
    sequence_ . intersperse printSpacers $ makeEntry <$> input
    BS.appendFile destinationFile "\n    ]"
  where
    printSpacers :: IO ()
    printSpacers =
        BS.appendFile
            destinationFile
            "\n    ,    "

    makeEntry :: Printable assetId -> IO ()
    makeEntry = \case
        PrintBinaryWithHash assetId filePathAsset ->
            handle
                do accessExceptionHandler assetId filePathAsset
                do
                    term <- BS.readFile filePathAsset
                    BS.appendFile
                        destinationFile
                        do
                            fold
                                [ "KisevalterPackagedBinary "
                                , fromString $ show term
                                , " (Just \""
                                , hash term
                                , "\") "
                                , fromString $ show assetId
                                ]
        PrintBinaryWithoutHash assetId filePathAsset ->
            handle
                do accessExceptionHandler assetId filePathAsset
                do
                    term <- BS.readFile filePathAsset
                    BS.appendFile
                        destinationFile
                        do
                            fold
                                [ "KisevalterPackagedBinary "
                                , fromString $ show term
                                , " Nothing "
                                , fromString $ show assetId
                                ]
        PrintBinaryWithHashAndLocalFile localFile assetId filePathAsset ->
            handle
                do
                    accessExceptionHandlerWithLocalFile
                        localFile
                        assetId
                        filePathAsset
                do
                    term <- BS.readFile filePathAsset
                    BS.appendFile
                        destinationFile
                        do
                            fold
                                [ "KisevalterPackagedBinary \""
                                , fromString localFile
                                , "\" (Just \""
                                , hash term
                                , "\") "
                                , fromString $ show term
                                , " "
                                , fromString $ show assetId
                                ]
        PrintBinaryAndLocalFileWithoutHash localFile assetId filePathAsset ->
            handle
                do
                    accessExceptionHandlerWithLocalFile
                        localFile
                        assetId
                        filePathAsset
                do
                    term <- BS.readFile filePathAsset
                    BS.appendFile
                        destinationFile
                        do
                            fold
                                [ "KisevalterPackagedBinary \""
                                , fromString localFile
                                , "\" Nothing "
                                , fromString $ show term
                                , " "
                                , fromString $ show assetId
                                ]

    accessExceptionHandler ::
        assetId ->
        FileToBeEncoded ->
        IOException ->
        IO ()
    accessExceptionHandler assetId filePathAsset except = do
        BS.putStr
            $ fold
                [ "\nAn IO error:\n"
                , fromString $ show except
                , "\noccurred while evaluating \""
                , fromString filePathAsset
                , "\" or "
                , fromString $ show assetId
                , "\n"
                , "A null will be printed instead.\n\n"
                ]
        BS.appendFile destinationFile "KisevaterManifestNull"

    accessExceptionHandlerWithLocalFile ::
        LocalFile ->
        assetId ->
        FileToBeEncoded ->
        IOException ->
        IO ()
    accessExceptionHandlerWithLocalFile localFile assetId filePathAsset except = do
        BS.putStr
            $ fold
                [ "\nAn IO error:\n"
                , fromString $ show except
                , "\noccurred while evaluating \""
                , fromString filePathAsset
                , "\" or "
                , fromString $ show assetId
                , " or \""
                , fromString $ show localFile
                , "\n"
                , "A null will be printed instead.\n\n"
                ]
        BS.appendFile destinationFile "KisevaterManifestNull"

    hash :: BS.ByteString -> BS.ByteString
    hash = convertToBase Base16 . hashWith SHA256