{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# LANGUAGE BlockArguments      #-}
module Kisevalter.IOActions
    ( KisevalterResult (..)
    , MemoryAsset
    , processSimple
    , KisevalterError (..)
    ) where

import           Control.Exception.Base     (catch)
import           Crypto.Hash                (SHA256 (SHA256), hashWith)
import           Data.Bool                  (bool)
import           Data.ByteArray.Encoding    (Base (Base16), convertToBase)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import           Data.Sequence              (Seq)
import qualified Data.Sequence              as Seq
import           Kisevalter.KisevalterTypes (Hash, KisevalterManifest (..),
                                             LocalFile)
import           System.Directory           (doesFileExist)
import           System.IO                  (Handle, stderr)

type MemoryAsset = ByteString

{- | We wish to convert a traversable of KisevalterManifests to a
    traversable of KisevalterResults, for the end user to use,
    while retaining error messages that might result during processing. -}

data KisevalterResult customError assetId
    = KisevalterSuccessFilePath LocalFile
    | KisevalterSuccessMemoryAsset MemoryAsset assetId
    | KisevalterFailureFilePath (Seq (KisevalterError customError)) FilePath
    | KisevalterFailureById (Seq (KisevalterError customError)) assetId
    | KisevalterWarningWithFilePath
        (Seq (KisevalterError customError)) LocalFile
    | KisevalterWarningWithMemoryAsset
        (Seq (KisevalterError customError)) MemoryAsset assetId
    | KisevalterUnsupportedCustomType assetId
    | KisevalterNullResult

data KisevalterError customError
    = KisevalterLocalFileFailedHash
    | KisevalterLocalFileDoesNotExist
    | KisevalterLocalFileIOError IOError
    | KisevalterEncodedFileFailedHash
    | KisevalterOtherError
    | KisevalterCustomError customError
    deriving (Eq, Show)

{- | Main user-facing function.
    Traverses a traversable data structure of (KisevalterManifest)s,
    without supporting custom manifests, choice of error handles,
    custom manifest handlers, or custom errors, outputting
    under IO a traversable of (KisevalterResult)s. -}

processSimple
    :: (Traversable t, Show assetId, Eq customError)
    => t (KisevalterManifest customFormat assetId)
    -> IO (t (KisevalterResult customError assetId))
processSimple = traverse $ processNoCustomManifest stderr

{- | Process is the workhorse of Kisevalter.
    It processes the specific form of the sum type
    KisevalterManifest customManifest assetId
    and executes the logic appropriate to the form.

    It includes a field for handling custom manifests.

    Currently, this is supposed to use the handle to
    immediately print out to the intended handle the
    errors generated on processing. Unfortunately,
    I haven't implemented the functionality yet.

    Not for end-user use, and is not exported. -}

process
    :: forall assetId customManifest customError .
       (Show assetId, Eq customError)
    => (  customManifest
       -> assetId
       -> IO ( KisevalterResult customError assetId )
       )
    -> Handle
    -> KisevalterManifest customManifest assetId
    -> IO (KisevalterResult customError assetId)
process customFun handle = \case
    KisevalterLocalFile localFile maybeHash ->
            localFileErrorToResults localFile
        <$> checkLocalFile localFile maybeHash

    KisevalterPackagedBinary encodedFile maybeHash assetId ->
            packagedBinaryErrorToResults assetId encodedFile
        <$> checkPackagedBinary encodedFile maybeHash

    KisevalterDeployedBinary localFile maybeHash encodedFile assetId ->
        checkLocalFile localFile maybeHash >>= \localResult ->
            if | null localResult -> pure $ KisevalterSuccessFilePath localFile

               | KisevalterLocalFileFailedHash `elem` localResult -> pure
                $ KisevalterWarningWithFilePath localResult localFile

               | KisevalterLocalFileDoesNotExist `elem` localResult ->
                (`catch` deployedBinaryWriteFileExceptionHandler) do
                packagedResult <- checkPackagedBinary encodedFile maybeHash
                BS.writeFile localFile encodedFile
                pure $ KisevalterWarningWithFilePath (localResult <> packagedResult) localFile

               | otherwise ->
                error $ "KisevalterDeployedBinary case failed, \
                \ detailed error message not implemented." <> show assetId

    KisevalterCustomFormat a assetId->
        customFun a assetId

    KisevalterManifestNull ->
        pure KisevalterNullResult
  where
    (>||<) :: Foldable t => IO (t a) -> IO (t a) -> IO (t a)
    firstAction >||< secondAction = do
        foldable <- firstAction
        if null foldable
            then pure foldable
            else secondAction
    infixl 4 >||<

    checkLocalFile
        :: LocalFile
        -> Maybe Hash
        -> IO ( Seq ( KisevalterError customError ) )
    checkLocalFile localFile maybeHash =
        (`catch` checkLocalFileErrorHandlers) $
        doesFileExist localFile >>= bool
            do pure $ pure KisevalterLocalFileDoesNotExist
            case maybeHash of
                Nothing -> pure Seq.empty
                Just aHash -> do
                    result <- BS.readFile localFile
                    if hash result == aHash
                        then pure Seq.empty
                        else pure $ pure KisevalterLocalFileFailedHash

    localFileErrorToResults
        :: LocalFile
        -> Seq (KisevalterError customError)
        -> KisevalterResult customError assetId
    localFileErrorToResults localFile possibleErrors
        | null possibleErrors = KisevalterSuccessFilePath localFile

        | do \case;KisevalterLocalFileDoesNotExist -> True;
                   KisevalterLocalFileIOError _ -> True;
                   _ -> False
          `any` possibleErrors =
            KisevalterFailureFilePath possibleErrors localFile

        | KisevalterLocalFileFailedHash `elem` possibleErrors =
            KisevalterWarningWithFilePath possibleErrors localFile

        | otherwise =
            error
            $ "unexpected result evaluating the local file:\n"
                <> localFile
                <> "\nCheck the function process in IOActions."

    checkLocalFileErrorHandlers
        :: IOError -> IO (Seq (KisevalterError customError))
    checkLocalFileErrorHandlers = pure . pure . KisevalterLocalFileIOError

    hash :: MemoryAsset -> MemoryAsset
    hash = convertToBase Base16 . hashWith SHA256

    packagedBinaryErrorToResults
        :: assetId
        -> MemoryAsset
        -> Seq (KisevalterError customError)
        -> KisevalterResult customError assetId
    packagedBinaryErrorToResults assetId encodedFile results
        | null results = KisevalterSuccessMemoryAsset encodedFile assetId

        | KisevalterEncodedFileFailedHash `elem` results =
            KisevalterWarningWithMemoryAsset results encodedFile assetId

        | otherwise =
            error
            $ "unexpected result evaluating the packaged file:\n"
            <> show assetId
            <> "\nCheck the function process in IOActions."

    checkPackagedBinary
        :: MemoryAsset
        -> Maybe Hash
        -> IO ( Seq ( KisevalterError customError ) )
    checkPackagedBinary encodedFile = \case
        Nothing -> pure Seq.empty
        Just aHash -> if hash encodedFile == aHash
            then pure Seq.empty
            else pure $ pure KisevalterEncodedFileFailedHash

    deployedBinaryWriteFileExceptionHandler
        :: IOError -> IO (KisevalterResult customError assetId)
    deployedBinaryWriteFileExceptionHandler = error "todo"

{- | ProcessNoCustomManifest is the standard interface to process.
    It is not for end-user use either. -}

processNoCustomManifest
    :: (Show assetId, Eq customError)
    => Handle
    -> KisevalterManifest customManifest assetId
    -> IO (KisevalterResult customError assetId)
processNoCustomManifest = process . const
    $ pure . KisevalterUnsupportedCustomType
