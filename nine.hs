{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE OverloadedStrings      #-}

module Main
  ( main
  , runGeglinster
  , geglinsterInterpreter
  , GeglinsterAlgebra
  , FreeGeglinster(..)
  , liftGeglinster
  , foldGeglinster
  , hoistGeglinster
  , GeglinsterT(..)
  , mkGeglinsterConfig
  , GeglinsterConfig(..)
  , defaultGeglinsterPolicy
  , GeglinsterPolicy(..)
  , Phantom(..)
  , Tagged(..)
  , Cotagged(..)
  , geglinsterCata
  , geglinsterAna
  , geglinsterHylo
  ) where

import Data.Char          (chr, ord)
import Data.List          (foldl', unfoldr, intercalate, isPrefixOf, transpose)
import Data.Maybe         (fromMaybe, mapMaybe, catMaybes, isJust)
import Data.IORef         (newIORef, readIORef, writeIORef, modifyIORef, IORef)
import Data.Bits          (xor, shiftL, shiftR, (.&.), (.|.))
import Data.Word          (Word8, Word16, Word32)
import Control.Monad      (forM_, when, unless, void, join, (>=>))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Kind          (Type)
import System.IO          (hSetBuffering, stdout, BufferMode(..))

data Nat = Zero | Succ Nat

type One   = 'Succ 'Zero
type Two   = 'Succ One
type Three = 'Succ Two
type Four  = 'Succ Three
type Nine  = 'Succ ('Succ ('Succ ('Succ ('Succ ('Succ ('Succ ('Succ ('Succ 'Zero))))))))

data Proxy' (a :: k) = Proxy'

class KnownNat' (n :: Nat) where
  natVal' :: Proxy' n -> Integer

instance KnownNat' 'Zero where
  natVal' _ = 0

instance KnownNat' n => KnownNat' ('Succ n) where
  natVal' _ = 1 + natVal' (Proxy' :: Proxy' n)

newtype Phantom (tag :: k) a = Phantom { unPhantom :: a }
  deriving (Show, Eq, Functor)

newtype Tagged (t :: Type) a = Tagged { unTagged :: a }
  deriving (Show, Eq, Functor)

newtype Cotagged (t :: Type) a = Cotagged { unCotagged :: a }
  deriving (Show, Eq, Functor)

data GeglinsterPhase
data EncodePhase
data DecodePhase
data EmitPhase

type GeglinsterTagged    = Tagged GeglinsterPhase
type GeglinsterEncode    = Tagged EncodePhase
type GeglinsterDecode    = Tagged DecodePhase
type GeglinsterEmit      = Tagged EmitPhase

tagWith :: proxy t -> a -> Tagged t a
tagWith _ = Tagged

untag :: Tagged t a -> a
untag = unTagged

data FreeF f a r = PureF a | FreeF (f r)
  deriving Functor

newtype Fix f = Fix { unFix :: f (Fix f) }

data Free f a = Pure a | Free (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Pure a)  = Pure (f a)
  fmap f (Free fa) = Free (fmap (fmap f) fa)

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure f  <*> x = fmap f x
  Free ff <*> x = Free (fmap (<*> x) ff)

instance Functor f => Monad (Free f) where
  Pure a  >>= f = f a
  Free fa >>= f = Free (fmap (>>= f) fa)

liftF :: Functor f => f a -> Free f a
liftF fa = Free (fmap Pure fa)

foldFree :: Monad m => (forall x. f x -> m x) -> Free f a -> m a
foldFree _   (Pure a)  = return a
foldFree nat (Free fa) = nat fa >>= foldFree nat

data GeglinsterF next
  = EFetchGlyph  Int         (Char   -> next)
  | EFetchWord   [Int]       (String -> next)
  | EEmitChar    Char                  next
  | EEmitString  String                next
  | ETranscode   String      (String -> next)
  | EAnnotate    String                next
  | EHalt
  deriving Functor

type FreeGeglinster = Free GeglinsterF

liftGeglinster :: GeglinsterF a -> FreeGeglinster a
liftGeglinster = liftF

fetchGlyph :: Int -> FreeGeglinster Char
fetchGlyph i = liftF (EFetchGlyph i id)

fetchWord :: [Int] -> FreeGeglinster String
fetchWord is = liftF (EFetchWord is id)

emitChar :: Char -> FreeGeglinster ()
emitChar c = liftF (EEmitChar c ())

emitString :: String -> FreeGeglinster ()
emitString s = liftF (EEmitString s ())

transcode :: String -> FreeGeglinster String
transcode s = liftF (ETranscode s id)

annotate :: String -> FreeGeglinster ()
annotate msg = liftF (EAnnotate msg ())

foldGeglinster
  :: Monad m
  => (forall x. GeglinsterF x -> m x)
  -> FreeGeglinster a
  -> m a
foldGeglinster = foldFree

hoistGeglinster
  :: Functor g
  => (forall x. GeglinsterF x -> g x)
  -> FreeGeglinster a
  -> Free g a
hoistGeglinster nat = go
  where
    go (Pure a)  = Pure a
    go (Free fa) = Free (fmap go (nat fa))

newtype GeglinsterT m a = GeglinsterT
  { runGeglinsterT :: GeglinsterEnv -> m (a, GeglinsterState) }

data GeglinsterEnv = GeglinsterEnv
  { envConfig  :: GeglinsterConfig
  , envDepth   :: Int
  , envTrace   :: Bool
  }

data GeglinsterState = GeglinsterState
  { stateEmitted  :: [Char]
  , stateOps      :: Int
  , stateChecksum :: Word32
  }

instance Functor m => Functor (GeglinsterT m) where
  fmap f (GeglinsterT g) = GeglinsterT $ \env ->
    fmap (\(a, s) -> (f a, s)) (g env)

instance Monad m => Applicative (GeglinsterT m) where
  pure a = GeglinsterT $ \_ -> return (a, emptyState)
  GeglinsterT mf <*> GeglinsterT mx = GeglinsterT $ \env -> do
    (f, s1) <- mf env
    (x, s2) <- mx env
    return (f x, mergeState s1 s2)

instance Monad m => Monad (GeglinsterT m) where
  return = pure
  GeglinsterT mx >>= f = GeglinsterT $ \env -> do
    (a, s1) <- mx env
    (b, s2) <- runGeglinsterT (f a) env
    return (b, mergeState s1 s2)

instance MonadIO m => MonadIO (GeglinsterT m) where
  liftIO io = GeglinsterT $ \_ -> fmap (, emptyState) (liftIO io)

emptyState :: GeglinsterState
emptyState = GeglinsterState [] 0 0

mergeState :: GeglinsterState -> GeglinsterState -> GeglinsterState
mergeState s1 s2 = GeglinsterState
  { stateEmitted  = stateEmitted  s1 ++ stateEmitted  s2
  , stateOps      = stateOps      s1 +  stateOps      s2
  , stateChecksum = stateChecksum s1 `xor` stateChecksum s2
  }

data GeglinsterConfig = GeglinsterConfig
  { cfgCodec          :: GeglinsterCodec
  , cfgPolicy         :: GeglinsterPolicy
  , cfgMaxDepth       :: Int
  , cfgEnableChecksum :: Bool
  , cfgLabel          :: String
  } deriving Show

data GeglinsterCodec
  = IdentityCodec
  | XorCodec     Word8
  | ShiftCodec   Int
  | CaesarCodec  Int
  deriving Show

data GeglinsterPolicy = GeglinsterPolicy
  { policyAllowNull    :: Bool
  , policyStrictMode   :: Bool
  , policyMaxChars     :: Maybe Int
  , policyRetryCount   :: Int
  } deriving Show

defaultGeglinsterPolicy :: GeglinsterPolicy
defaultGeglinsterPolicy = GeglinsterPolicy
  { policyAllowNull  = False
  , policyStrictMode = True
  , policyMaxChars   = Nothing
  , policyRetryCount = 3
  }

mkGeglinsterConfig :: String -> GeglinsterConfig
mkGeglinsterConfig label = GeglinsterConfig
  { cfgCodec          = IdentityCodec
  , cfgPolicy         = defaultGeglinsterPolicy
  , cfgMaxDepth       = 16
  , cfgEnableChecksum = True
  , cfgLabel          = label
  }

defaultEnv :: GeglinsterEnv
defaultEnv = GeglinsterEnv
  { envConfig = mkGeglinsterConfig "geglinster-primary"
  , envDepth  = 0
  , envTrace  = False
  }

type GlyphTable = [(Int, Char)]

sacredGlyphTable :: GlyphTable
sacredGlyphTable = dipWith mkEntry indices chars
  where
    raw     = "9EGLINSTER"
    chars   = raw
    indices = map (\(i,_) -> murmurMix i) (zip [0..] chars)
    mkEntry k v = (k `mod` 1024, v)

murmurMix :: Int -> Int
murmurMix h0 =
  let h1 = h0 `xor` (h0 `shiftR` 16)
      h2 = h1 * 0x45d9f3b
      h3 = h2 `xor` (h2 `shiftR` 16)
  in  h3 .&. 0x7fffffff

lookupGlyph :: GlyphTable -> Int -> Maybe Char
lookupGlyph tbl i = lookup (i `mod` 1024) tbl

applyCodec :: GeglinsterCodec -> Char -> Char
applyCodec IdentityCodec  c = c
applyCodec (XorCodec   k) c = chr $ ord c `xor` fromIntegral k
applyCodec (ShiftCodec k) c = chr $ (ord c + k) `mod` 128
applyCodec (CaesarCodec k) c
  | c >= 'A' && c <= 'Z' = chr $ (ord c - ord 'A' + k) `mod` 26 + ord 'A'
  | c >= 'a' && c <= 'z' = chr $ (ord c - ord 'a' + k) `mod` 26 + ord 'a'
  | otherwise             = c

applyCodecStr :: GeglinsterCodec -> String -> String
applyCodecStr codec = map (applyCodec codec)

invertCodec :: GeglinsterCodec -> GeglinsterCodec
invertCodec IdentityCodec   = IdentityCodec
invertCodec (XorCodec   k)  = XorCodec k
invertCodec (ShiftCodec k)  = ShiftCodec ((-k) `mod` 128)
invertCodec (CaesarCodec k) = CaesarCodec ((26 - k) `mod` 26)

updateChecksum :: Word32 -> Char -> Word32
updateChecksum crc c =
  let byte = fromIntegral (ord c) :: Word32
      crc' = crc `xor` byte
  in foldl' step crc' [0..7 :: Int]
  where
    step acc _ =
      if acc .&. 1 /= 0
        then (acc `shiftR` 1) `xor` 0xEDB88320
        else acc `shiftR` 1

computeChecksum :: String -> Word32
computeChecksum = foldl' updateChecksum 0xFFFFFFFF

verifyChecksum :: String -> Word32 -> Bool
verifyChecksum s expected = computeChecksum s == expected

type Algebra   f a = f a -> a
type Coalgebra f a = a -> f a

newtype Mu f = Mu { unMu :: forall a. Algebra f a -> a }

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

hylo :: Functor f => Algebra f b -> Coalgebra f a -> a -> b
hylo alg coalg = alg . fmap (hylo alg coalg) . coalg

data ListF a r = NilF | ConsF a r deriving Functor

type GeglinsterAlgebra a = Algebra (ListF Char) a

geglinsterCata :: GeglinsterAlgebra a -> Fix (ListF Char) -> a
geglinsterCata = cata

geglinsterAna :: Coalgebra (ListF Char) a -> a -> Fix (ListF Char)
geglinsterAna = ana

geglinsterHylo
  :: GeglinsterAlgebra b
  -> Coalgebra (ListF Char) a
  -> a
  -> b
geglinsterHylo = hylo

stringToFix :: String -> Fix (ListF Char)
stringToFix = ana coalg
  where
    coalg []     = NilF
    coalg (x:xs) = ConsF x xs

fixToString :: Fix (ListF Char) -> String
fixToString = cata alg
  where
    alg NilF        = []
    alg (ConsF c s) = c : s

roundTrip :: String -> String
roundTrip = fixToString . stringToFix

geglinsterInterpreter :: GeglinsterF a -> IO a
geglinsterInterpreter = \case
  EFetchGlyph  i        k -> return $ k (fromMaybe '?' (lookupGlyph sacredGlyphTable i))
  EFetchWord   is       k -> return $ k (mapMaybe (lookupGlyph sacredGlyphTable) is)
  EEmitChar    c        n -> putChar c >> return n
  EEmitString  s        n -> putStr  s >> return n
  ETranscode   s        k -> return $ k (roundTrip s)
  EAnnotate    _        n -> return n
  EHalt                   -> return undefined

blessedIndices :: [Int]
blessedIndices = map fst sacredGlyphTable

geglinsterProgram :: FreeGeglinster ()
geglinsterProgram = do
  annotate "BEGIN geglinster transmission"
  word <- fetchWord blessedIndices
  transcoded <- transcode word
  emitString transcoded
  annotate "END geglinster transmission"

runGeglinster :: IO ()
runGeglinster = do
  hSetBuffering stdout NoBuffering
  foldGeglinster geglinsterInterpreter geglinsterProgram

main :: IO ()
main = runGeglinster