module Test.Plutip.UtxoDistribution where

import Prelude

import Control.Alternative (guard)
import Control.Monad.State.Trans (StateT(StateT), runStateT)
import Ctl.Internal.Serialization.Types (PrivateKey)
import Ctl.Internal.Wallet.Key
  ( KeyWallet
  , PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey
  , privateKeysToKeyWallet
  )
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))
import Type.Prelude (Proxy(Proxy))

-- | UTxO amount in Lovelaces
type UtxoAmount = BigInt

-- | A list of UTxOs for a single wallet
type InitialUTxOs = Array UtxoAmount

-- | A wrapper that allows to specify a stake key to attach to a
-- | generated pre-funded Address.
data InitialUTxOsWithStakeKey =
  InitialUTxOsWithStakeKey PrivateStakeKey InitialUTxOs

-- | A spec for distribution of UTxOs between wallets.
type InitialUTxODistribution = Array InitialUTxOs

-- | A type class that implements a type-safe interface for specifying UTXO
-- | distribution for wallets.
-- | Number of wallets in distribution specification matches the number of
-- | wallets provided to the user.
class UtxoDistribution distr wallets | distr -> wallets where
  encodeDistribution :: distr -> Array (Array UtxoAmount)
  decodeWallets :: distr -> Array PrivateKey -> Maybe wallets
  decodeWallets'
    :: distr
    -> Array PrivateKey
    -> Maybe (wallets /\ Array PrivateKey)
  keyWallets :: Proxy distr -> wallets -> Array KeyWallet

instance UtxoDistribution Unit Unit where
  encodeDistribution _ = []
  decodeWallets d p = decodeWalletsDefault d p
  decodeWallets' _ pks = Just $ unit /\ pks
  keyWallets _ _ = []

instance UtxoDistribution InitialUTxOs KeyWallet where
  encodeDistribution amounts = [ amounts ]
  decodeWallets d p = decodeWalletsDefault d p
  decodeWallets' _ pks = Array.uncons pks <#>
    \{ head: key, tail } ->
      (privateKeysToKeyWallet (PrivatePaymentKey key) Nothing) /\ tail
  keyWallets _ wallet = [ wallet ]

instance UtxoDistribution InitialUTxOsWithStakeKey KeyWallet where
  encodeDistribution (InitialUTxOsWithStakeKey _ amounts) = [ amounts ]
  decodeWallets d p = decodeWalletsDefault d p
  decodeWallets' (InitialUTxOsWithStakeKey stake _) pks = Array.uncons pks <#>
    \{ head: key, tail } ->
      privateKeysToKeyWallet (PrivatePaymentKey key) (Just stake) /\
        tail
  keyWallets _ wallet = [ wallet ]

instance UtxoDistribution (Array InitialUTxOs) (Array KeyWallet) where
  encodeDistribution = encodeDistributionArray
  decodeWallets d = decodeWalletsDefault d
  decodeWallets' = decodeWallets'Array
  keyWallets = keyWalletsArray

instance UtxoDistribution (Array InitialUTxOsWithStakeKey) (Array KeyWallet) where
  encodeDistribution = encodeDistributionArray
  decodeWallets d = decodeWalletsDefault d
  decodeWallets' = decodeWallets'Array
  keyWallets = keyWalletsArray

encodeDistributionArray
  :: forall (distr :: Type)
   . UtxoDistribution distr KeyWallet
  => Array distr
  -> Array (Array UtxoAmount)
encodeDistributionArray = (_ >>= encodeDistribution)

decodeWallets'Array
  :: forall (distr :: Type)
   . UtxoDistribution distr KeyWallet
  => Array distr
  -> Array PrivateKey
  -> Maybe (Array KeyWallet /\ Array PrivateKey)
decodeWallets'Array = runStateT <<< traverse (StateT <<< decodeWallets')

keyWalletsArray
  :: forall (distr :: Type)
   . Proxy distr
  -> Array KeyWallet
  -> Array KeyWallet
keyWalletsArray _ wallets = wallets

instance
  ( UtxoDistribution headSpec headWallets
  , UtxoDistribution restSpec restWallets
  ) =>
  UtxoDistribution (Tuple headSpec restSpec)
    (Tuple headWallets restWallets) where
  encodeDistribution (distr /\ rest) =
    encodeDistribution distr <> encodeDistribution rest
  decodeWallets d p = decodeWalletsDefault d p
  decodeWallets' (distr /\ rest) = runStateT do
    headWallets <- StateT $ decodeWallets' distr
    restWallets <- StateT $ decodeWallets' rest
    pure (headWallets /\ restWallets)
  keyWallets _ (headWallets /\ restWallets) =
    keyWallets (Proxy :: Proxy headSpec) headWallets
      <> keyWallets (Proxy :: Proxy restSpec) restWallets

decodeWalletsDefault
  :: forall distr wallets
   . UtxoDistribution distr wallets
  => distr
  -> Array PrivateKey
  -> Maybe wallets
decodeWalletsDefault d p = do
  wallets /\ remainingPKeys <- decodeWallets' d p
  guard $ Array.null remainingPKeys
  pure wallets
