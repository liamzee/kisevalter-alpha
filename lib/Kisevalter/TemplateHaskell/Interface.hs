{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TemplateHaskellQuotes #-}


module Kisevalter.TemplateHaskell.Interface where

import           Crypto.Hash                (SHA256 (SHA256), hashWith)
import           Data.ByteArray.Encoding    (Base (Base16), convertToBase)
import qualified Data.ByteString            as BS
import           Kisevalter.KisevalterTypes (KisevalterManifest (KisevalterDeployedBinary, KisevalterPackagedBinary),
                                             Printable (PrintBinaryAndLocalFileWithoutHash, PrintBinaryWithHash, PrintBinaryWithHashAndLocalFile, PrintBinaryWithoutHash))
import           Language.Haskell.TH.Syntax (Exp (AppE, ConE, ListE, LitE),
                                             Lift, Lit (StringL), Q,
                                             qAddDependentFile, runIO)
import Control.Applicative (liftA2)

listLiteralThroughTH ::
    forall assetId customFormat.
    (Show assetId, Lift assetId) =>
    [Printable assetId] ->
    Q Exp
listLiteralThroughTH = fmap ListE . traverse process
  where
    process :: Printable assetId -> Q Exp
    process = \case
        PrintBinaryWithHash assetId filePath -> do
            file <- getFile filePath
            assetExp <- [|assetId|]
            pure $!
                    ConE 'KisevalterPackagedBinary
                `AppE` LitE (byteStringToStringLiteral file)
                `AppE` (ConE 'Just `AppE` LitE (byteStringToStringLiteral $ hash file))
                `AppE` assetExp

        PrintBinaryWithoutHash assetId filePath -> do
            file <- getFile filePath
            assetExp <- [|assetId|]
            pure $!
                    ConE 'KisevalterPackagedBinary
                `AppE` LitE (byteStringToStringLiteral file)
                `AppE` ConE 'Nothing
                `AppE` assetExp

        PrintBinaryWithHashAndLocalFile localFile assetId embeddedFile -> do
            file <- getFile embeddedFile
            (assetExp, localExp) <- liftA2 (,) [|assetId|] [|localFile|]
            pure $!
                    ConE 'KisevalterDeployedBinary
                `AppE` localExp
                `AppE` (ConE 'Just `AppE` LitE (byteStringToStringLiteral file))
                `AppE` LitE (byteStringToStringLiteral file)
                `AppE` assetExp

        PrintBinaryAndLocalFileWithoutHash localFile assetId embeddedFile -> do
            file <- getFile embeddedFile
            (assetExp, localExp) <- liftA2 (,) [|assetId|] [|localFile|]
            pure $!
                    ConE 'KisevalterDeployedBinary
                `AppE` localExp
                `AppE` ConE 'Nothing
                `AppE` LitE (byteStringToStringLiteral file)
                `AppE` assetExp

    getFile :: FilePath -> Q BS.ByteString
    getFile filePath = do
        qAddDependentFile filePath
        runIO do BS.readFile filePath

    byteStringToStringLiteral :: BS.ByteString -> Lit
    byteStringToStringLiteral = StringL . show

    hash :: BS.ByteString -> BS.ByteString
    hash = convertToBase Base16 . hashWith SHA256
