module Kisevalter.KisevalterTypes
    ( KisevalterManifest
        ( KisevalterLocalFile
        , KisevalterPackagedBinary
        , KisevalterDeployedBinary
        , KisevalterCustomFormat
        , KisevalterManifestNull
        )
    , LocalFile
    , Hash
    , Printable
        ( PrintBinaryWithHash
        , PrintBinaryWithoutHash
        , PrintBinaryWithHashAndLocalFile
        , PrintBinaryAndLocalFileWithoutHash
        )
    , EncodedFile
    , FileToBeEncoded
    ) where

import qualified Data.ByteString as BS

type Hash = BS.ByteString
type EncodedFile = BS.ByteString
type LocalFile = FilePath
type FileToBeEncoded = FilePath

{- | KisevalterManifest is the core type of Kisevalter,
    intended to be read by the process function. -}

data KisevalterManifest customFormat assetId
    = KisevalterLocalFile LocalFile (Maybe Hash)
    {- ^ Checks whether a local file exists, and if
        the hash exists, whether the file matches the hash. -}
    | KisevalterPackagedBinary EncodedFile (Maybe Hash) assetId
    {- ^ Not for user use, TH generation only.
        Passes the encoded file to the
        user via a success tag if it matches the hash,
        or if the hash does not exist. -}
    | KisevalterDeployedBinary LocalFile (Maybe Hash) EncodedFile assetId
    {- ^ Not for user use, TH generation only.
        Checks if the file exists, and if the hash exists,
        whether it matches the hash. If not, it checks
        whether the encoded file matches the hash if such
        exists, and if the hash does not exist, or
        if the hash matches, it deploys the encoded file
        to the local file path. -}
    | KisevalterCustomFormat customFormat assetId
    {- ^ Option for users to implement their own manifest format,
        passing the appropriate handler to the higher order function
        as needed. I cannot possibly cover every imaginable behavior,
        and this functions as a "failsafe" for users to have a custom
        manifest implementation. -}
    | KisevalterManifestNull
    {- ^ Simplification used for errors with manifest printers
        and the Template Haskell macros. -}
    deriving Show

data Printable assetId
    = PrintBinaryWithHash assetId FileToBeEncoded
    | PrintBinaryWithoutHash assetId FileToBeEncoded
    | PrintBinaryWithHashAndLocalFile LocalFile assetId FileToBeEncoded
    | PrintBinaryAndLocalFileWithoutHash LocalFile assetId FileToBeEncoded
