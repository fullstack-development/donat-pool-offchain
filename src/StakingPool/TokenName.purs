module StakingPool.TokenName where

import Contract.Prelude hiding (length)

import Contract.Monad (Contract, liftContractM)
import Contract.Value as Value
import Data.BigInt (BigInt, fromString, toString)
import Data.String (Pattern(..), codePointFromChar, drop, dropWhile, length, stripPrefix, takeWhile)
import Ext.Contract.Time (TimeStamp)
import Ext.Contract.Value (mkTokenName, tokenNameSizeLimit, tokenNameToString)

receiptPrefix :: String
receiptPrefix = "DPStake"

type Receipt =
  { time :: TimeStamp
  , amount :: BigInt
  }

receiptTokenNameString :: Receipt -> Maybe String
receiptTokenNameString receipt =
  let
    tn = receiptPrefix
      <> toString receipt.time.epoch
      <> "."
      <> toString receipt.time.dayOfEpoch
      <> "."
      <> toString receipt.amount
  in
    if length tn > tokenNameSizeLimit then Nothing else Just tn

receiptTokenName :: Receipt -> Maybe Value.TokenName
receiptTokenName receipt = receiptTokenNameString receipt >>= mkTokenName

mkReceiptTokenNameM :: Receipt -> Contract Value.TokenName
mkReceiptTokenNameM =
  liftContractM "Impossible to make a staking receipt token name" <<< receiptTokenName

parseReceiptTokenName :: Value.TokenName -> Maybe Receipt
parseReceiptTokenName tokenName = do
  tokenNameStr <- tokenNameToString tokenName
  receiptData <- stripPrefix (Pattern receiptPrefix) tokenNameStr
  let
    epochStr = takeWhile (\c -> c /= dotPoint) $ receiptData
    withoutEpoch = drop 1 $ dropWhile (\c -> c /= dotPoint) $ receiptData
    dayStr = takeWhile (\c -> c /= dotPoint) withoutEpoch
    amountStr = drop 1 $ dropWhile (\c -> c /= dotPoint) withoutEpoch
  (parsedEpoch :: BigInt) <- fromString epochStr
  (parsedDay :: BigInt) <- fromString dayStr
  (parsedAmt :: BigInt) <- fromString amountStr
  pure { time: { epoch: parsedEpoch, dayOfEpoch: parsedDay }, amount: parsedAmt }
  where
  dotPoint = codePointFromChar '.'

parseReceiptTokenNameM :: Value.TokenName -> Contract Receipt
parseReceiptTokenNameM =
  liftContractM "Impossible to parse a staking receipt token name" <<< parseReceiptTokenName
