-- |
-- Module      : Streamly.Internal.Data.Stream.TypeGen
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Template Haskell macros to create useful stream types.
--
-- __Create a type with a serial zipping applicative property:__
--
-- @
-- import Control.Monad.Catch (MonadThrow(..))
-- import Control.Monad.IO.Class (MonadIO(..))
-- import Control.Monad.Reader.Class (MonadReader(..))
-- import Control.Monad.Trans.Class (MonadTrans(..))
-- import Streamly.Data.Stream (Stream)
--
-- import qualified Streamly.Data.Stream
-- import qualified Streamly.Data.Stream as Stream
-- import qualified Streamly.Internal.Data.Stream.TypeGen as StreamTypeGen
--
-- applySerial :: Monad m => Stream m (a -> b) -> Stream m a -> Stream m b
-- applySerial = Stream.zipWith ($)
--
-- \$(StreamTypeGen.mkZippingType \"ZipSerial\" "applySerial" False)
-- @
--
-- __Create a type with a concurrent zipping applicative property:__
--
-- @
-- import Control.Monad.Catch (MonadThrow(..))
-- import Control.Monad.IO.Class (MonadIO(..))
-- import Control.Monad.Reader.Class (MonadReader(..))
-- import Control.Monad.Trans.Class (MonadTrans(..))
-- import Streamly.Data.Stream (Stream)
--
-- import qualified Streamly.Data.Stream
-- import qualified Streamly.Data.Stream.Concurrent
-- import qualified Streamly.Data.Stream.Concurrent as Concur
-- import qualified Streamly.Internal.Data.Stream.TypeGen as StreamTypeGen
--
-- applyConcurrent :: Monad m => Stream m (a -> b) -> Stream m a -> Stream m b
-- applyConcurrent = Concur.zipWith ($)
--
-- \$(StreamTypeGen.mkZippingType \"ZipConcurrent\" "applyConcurrent" True)
-- @
--
-- __Create a type with serial interleave properties:__
--
-- @
-- import Control.Monad.Catch (MonadThrow(..))
-- import Control.Monad.IO.Class (MonadIO(..))
-- import Control.Monad.Reader.Class (MonadReader(..))
-- import Control.Monad.Trans.Class (MonadTrans(..))
-- import Streamly.Data.Stream (Stream)
--
-- import qualified Control.Monad
-- import qualified Streamly.Data.Stream
-- import qualified Streamly.Data.Stream as Stream
-- import qualified Streamly.Internal.Data.Stream.TypeGen as StreamTypeGen
--
-- bindInterleave :: Stream m a -> (a -> Stream m b) -> Stream m b
-- bindInterleave = flip (Stream.concatMapWith Stream.interleave)
--
-- \$(StreamTypeGen.mkNestingType \"InterleaveSerial\" "bindInterleave" False)
-- @
--
-- __Create a type with eager concurrency properties:__
--
-- @
-- import Control.Monad.Catch (MonadThrow(..))
-- import Control.Monad.IO.Class (MonadIO(..))
-- import Control.Monad.Reader.Class (MonadReader(..))
-- import Control.Monad.Trans.Class (MonadTrans(..))
-- import Streamly.Data.Stream (Stream)
-- import Streamly.Data.Stream.Concurrent (MonadAsync)
--
-- import qualified Control.Monad
-- import qualified Streamly.Data.Stream
-- import qualified Streamly.Data.Stream.Concurrent
-- import qualified Streamly.Data.Stream.Concurrent as Concur
-- import qualified Streamly.Internal.Data.Stream.TypeGen as StreamTypeGen
--
-- bindConcurrent :: MonadAsync m => Stream m a -> (a -> Stream m b) -> Stream m b
-- bindConcurrent = flip (Concur.concatMapWith (Concur.eager True))
--
-- \$(StreamTypeGen.mkNestingType \"Parallel\" "bindConcurrent" True)
-- @
--
module Streamly.Internal.Data.Stream.TypeGen
    ( mkZippingType
    , mkNestingType
    ) where

--------------------------------------------------------------------------------
-- Developer notes
--------------------------------------------------------------------------------

-- Debugging helpers,
--
-- >>> import Language.Haskell.TH
-- >>> import Streamly.Internal.Data.Stream.TypeGen
-- >>> expr <- runQ (mkNestingType "Parallel" "bindConcurrent" True)
-- >>> putStrLn $ pprint expr

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

-- $setup
-- >>> :m
-- >>> import Language.Haskell.TH
-- >>> import Streamly.Internal.Data.Stream.TypeGen

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

toTypeStr :: String -> String
toTypeStr typ = "mk" ++ typ

unTypeStr :: String -> String
unTypeStr typ = "un" ++ typ

--------------------------------------------------------------------------------
-- Names
--------------------------------------------------------------------------------

_m :: Name
_m = mkName "m"

_a :: Name
_a = mkName "a"

_r :: Name
_r = mkName "r"

_Stream :: Name
_Stream = mkName "Stream"

_fmap :: Name
_fmap = mkName "fmap"

_pure :: Name
_pure = mkName "pure"

_return :: Name
_return = mkName "return"

_strm :: Name
_strm = mkName "strm"

_strm1 :: Name
_strm1 = mkName "strm1"

_strm2 :: Name
_strm2 = mkName "strm2"

_Functor :: Name
_Functor = mkName "Functor"

_Applicative :: Name
_Applicative = mkName "Applicative"

_Monad :: Name
_Monad = mkName "Monad"

_MonadTrans :: Name
_MonadTrans = mkName "MonadTrans"

_MonadIO :: Name
_MonadIO = mkName "MonadIO"

_MonadThrow :: Name
_MonadThrow = mkName "MonadThrow"

_MonadReader :: Name
_MonadReader = mkName "MonadReader"

_lift :: Name
_lift = mkName "lift"

_ask :: Name
_ask = mkName "ask"

_local :: Name
_local = mkName "local"

_throwM :: Name
_throwM = mkName "throwM"

_liftIO :: Name
_liftIO = mkName "liftIO"

_f :: Name
_f = mkName "f"

_f1 :: Name
_f1 = mkName "f1"

_dotOp :: Name
_dotOp = mkName "."

_apOp :: Name
_apOp = mkName "<*>"

_bindOp :: Name
_bindOp = mkName ">>="

--------------------------------------------------------------------------------
-- TH helpers
--------------------------------------------------------------------------------

makeTypeDec :: String -> Q [Dec]
makeTypeDec dtNameStr = do
    typ <-
        newtypeD
            (return [])
            _Type
            [plainTV _m, plainTV _a]
            Nothing
            (normalC
                 (mkName dtNameStr)
                 [ bangType
                       (bang noSourceUnpackedness noSourceStrictness)
                       (appT (appT (conT _Stream) (varT _m)) (varT _a))
                 ])
            []
    let streamType = appT (appT (conT _Stream) (varT _m)) (varT _a)
        nameType = appT (appT (conT _Type) (varT _m)) (varT _a)
    mkTypSig <- sigD _toType (appT (appT arrowT streamType) nameType)
    mkTyp <- funD _toType [clause [] (normalB (conE _Type)) []]
    unTypSig <- sigD _unType (appT (appT arrowT nameType) streamType)
    unTyp <-
        funD
            _unType
            [clause [conP _Type [varP _strm]] (normalB (varE _strm)) []]
    return [typ, mkTypSig, mkTyp, unTypSig, unTyp]

    where

    _Type = mkName dtNameStr
    _toType = mkName (toTypeStr dtNameStr)
    _unType = mkName (unTypeStr dtNameStr)

makeStreamFunctor :: String -> Q Dec
makeStreamFunctor dtNameStr = do
    instanceD
        (pure <$> appT (conT _Monad) (varT _m))
        (appT (conT _Functor) (appT (conT _Type) (varT _m)))
        [ pragInlD _fmap Inline FunLike AllPhases
        , funD
              _fmap
              [ clause
                    [varP _f, conP _Type [varP _strm]]
                    (normalB
                         (appE
                              (conE _Type)
                              (appE (appE (varE _fmap) (varE _f)) (varE _strm))))
                    []
              ]
        ]

    where

    _Type = mkName dtNameStr

mkStreamApplicative :: String -> [String] -> String -> String -> Q Dec
mkStreamApplicative dtNameStr ctxM pureDefStr apDefStr =
    instanceD
        (Prelude.mapM (\c -> appT (conT (mkName c)) (varT _m)) ctxM)
        (appT (conT _Applicative) (appT (conT _Type) (varT _m)))
        [ pragInlD _pure Inline FunLike AllPhases
        , funD
              _pure
              [ clause
                    []
                    (normalB
                         (infixE
                              (Just (conE _Type))
                              (varE _dotOp)
                              (Just (varE _pureDef))))
                    []
              ]
        , pragInlD _apOp Inline FunLike AllPhases
        , funD
              _apOp
              [ clause
                    [conP _Type [varP _strm1], conP _Type [varP _strm2]]
                    (normalB
                         (appE
                              (conE _Type)
                              (appE
                                   (appE (varE _apDef) (varE _strm1))
                                   (varE _strm2))))
                    []
              ]
        ]

    where

    _Type = mkName dtNameStr
    _pureDef = mkName pureDefStr
    _apDef = mkName apDefStr

mkStreamMonad :: String -> [String] -> String -> Q Dec
mkStreamMonad dtNameStr ctxM bindDefStr =
    instanceD
        (Prelude.mapM (\c -> appT (conT (mkName c)) (varT _m)) ctxM)
        (appT (conT _Monad) (appT (conT _Type) (varT _m)))
        [ pragInlD _bindOp Inline FunLike AllPhases
        , funD
              _bindOp
              [ clause
                    [conP _Type [varP _strm1], varP _f]
                    (normalB
                         (letE
                              [ funD
                                    _f1
                                    [ clause
                                          [varP _a]
                                          (normalB
                                               (appE
                                                    (varE _unType)
                                                    (appE (varE _f) (varE _a))))
                                          []
                                    ]
                              ]
                              (appE
                                   (conE _Type)
                                   (appE
                                        (appE (varE _bindDef) (varE _strm1))
                                        (varE _f1)))))
                    []
              ]
        ]

    where

    _Type = mkName dtNameStr
    _unType = mkName (unTypeStr dtNameStr)
    _bindDef = mkName bindDefStr

mkSimpleStreamInstances :: String -> Q [Dec]
mkSimpleStreamInstances dtNameStr = do
    trans <-
        instanceD
            (pure [])
            (appT (conT _MonadTrans) (conT _Type))
            [ pragInlD _lift Inline FunLike AllPhases
            , funD
                  _lift
                  [ clause
                        []
                        (normalB
                             (infixE
                                  (Just (conE _Type))
                                  (varE _dotOp)
                                  (Just (varE _lift))))
                        []
                  ]
            ]
    io <-
        instanceD
            (sequence
                 [ appT (conT _Monad) (appT (conT _Type) (varT _m))
                 , appT (conT _MonadIO) (varT _m)
                 ])
            (appT (conT _MonadIO) (appT (conT _Type) (varT _m)))
            [ pragInlD _liftIO Inline FunLike AllPhases
            , funD
                  _liftIO
                  [ clause
                        []
                        (normalB
                             (infixE
                                  (Just (conE _Type))
                                  (varE _dotOp)
                                  (Just
                                       (infixE
                                            (Just (varE _lift))
                                            (varE _dotOp)
                                            (Just (varE _liftIO))))))
                        []
                  ]
            ]
    throw <-
        instanceD
            (sequence
                 [ appT (conT _Monad) (appT (conT _Type) (varT _m))
                 , appT (conT _MonadThrow) (varT _m)
                 ])
            (appT (conT _MonadThrow) (appT (conT _Type) (varT _m)))
            [ pragInlD _throwM Inline FunLike AllPhases
            , funD
                  _throwM
                  [ clause
                        []
                        (normalB
                             (infixE
                                  (Just (conE _Type))
                                  (varE _dotOp)
                                  (Just
                                       (infixE
                                            (Just (varE _lift))
                                            (varE _dotOp)
                                            (Just (varE _throwM))))))
                        []
                  ]
            ]
    reader <-
        instanceD
            (sequence
                 [ appT (conT _Monad) (appT (conT _Type) (varT _m))
                 , appT (appT (conT _MonadReader) (varT _r)) (varT _m)
                 ])
            (appT
                 (appT (conT _MonadReader) (varT _r))
                 (appT (conT _Type) (varT _m)))
            [ pragInlD _ask Inline FunLike AllPhases
            , funD _ask [clause [] (normalB (appE (varE _lift) (varE _ask))) []]
            , pragInlD _local Inline FunLike AllPhases
            , funD
                  _local
                  [ clause
                        [varP _f, conP _Type [varP _strm]]
                        (normalB
                             (appE
                                  (conE _Type)
                                  (appE
                                       (appE (varE _local) (varE _f))
                                       (varE _strm))))
                        []
                  ]
            ]
    return [trans, io, throw, reader]

    where

    _Type = mkName dtNameStr
    _unType = mkName (unTypeStr dtNameStr)

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

-- | Create a type with a zip-like applicative.
--
-- >>> expr <- runQ (mkZippingType "ZipSerial" "apZipSerial" False)
-- >>> putStrLn $ pprint expr
-- newtype ZipSerial m a = ZipSerial (Stream m a)
-- mkZipSerial :: Stream m a -> ZipSerial m a
-- mkZipSerial = ZipSerial
-- unZipSerial :: ZipSerial m a -> Stream m a
-- unZipSerial (ZipSerial strm) = strm
-- instance Monad m => Functor (ZipSerial m)
--     where {-# INLINE fmap #-}
--           fmap f (ZipSerial strm) = ZipSerial (fmap f strm)
-- instance Monad m => Applicative (ZipSerial m)
--     where {-# INLINE pure #-}
--           pure = ZipSerial . Streamly.Data.Stream.repeat
--           {-# INLINE (<*>) #-}
--           (<*>) (ZipSerial strm1) (ZipSerial strm2) = ZipSerial (apZipSerial strm1 strm2)
-- instance MonadTrans ZipSerial
--     where {-# INLINE lift #-}
--           lift = ZipSerial . lift
-- instance (Monad (ZipSerial m), MonadIO m) => MonadIO (ZipSerial m)
--     where {-# INLINE liftIO #-}
--           liftIO = ZipSerial . (lift . liftIO)
-- instance (Monad (ZipSerial m),
--           MonadThrow m) => MonadThrow (ZipSerial m)
--     where {-# INLINE throwM #-}
--           throwM = ZipSerial . (lift . throwM)
-- instance (Monad (ZipSerial m), MonadReader r m) => MonadReader r
--                                                                (ZipSerial m)
--     where {-# INLINE ask #-}
--           ask = lift ask
--           {-# INLINE local #-}
--           local f (ZipSerial strm) = ZipSerial (local f strm)
mkZippingType
    :: String -- ^ Name of the type
    -> String -- ^ Function to use for (\<*\>)
    -> Bool   -- ^ 'True' if (\<*\>) has concurrent properties
    -> Q [Dec]
mkZippingType dtNameStr apOpStr isConcurrent = do
    typeDec <- makeTypeDec dtNameStr
    functorI <- makeStreamFunctor dtNameStr
    applicativeI <-
        mkStreamApplicative
            dtNameStr
            classConstraints
            "Streamly.Data.Stream.repeat"
            apOpStr
    simpleInstances <- mkSimpleStreamInstances dtNameStr
    return $ typeDec ++ [functorI, applicativeI] ++ simpleInstances

    where

    classConstraints =
        if isConcurrent
        then ["Streamly.Internal.Data.Stream.Concurrent.MonadAsync"]
        else ["Monad"]

-- | Create a type with specific stream combination properties.
--
-- >>> expr <- runQ (mkNestingType "Parallel" "bindConcurrent" True)
-- >>> putStrLn $ pprint expr
-- newtype Parallel m a = Parallel (Stream m a)
-- mkParallel :: Stream m a -> Parallel m a
-- mkParallel = Parallel
-- unParallel :: Parallel m a -> Stream m a
-- unParallel (Parallel strm) = strm
-- instance Monad m => Functor (Parallel m)
--     where {-# INLINE fmap #-}
--           fmap f (Parallel strm) = Parallel (fmap f strm)
-- instance Streamly.Internal.Data.Stream.Concurrent.MonadAsync m => Monad (Parallel m)
--     where {-# INLINE (>>=) #-}
--           (>>=) (Parallel strm1) f = let f1 a = unParallel (f a)
--                                       in Parallel (bindConcurrent strm1 f1)
-- instance Streamly.Internal.Data.Stream.Concurrent.MonadAsync m => Applicative (Parallel m)
--     where {-# INLINE pure #-}
--           pure = Parallel . Streamly.Data.Stream.fromPure
--           {-# INLINE (<*>) #-}
--           (<*>) (Parallel strm1) (Parallel strm2) = Parallel (Control.Monad.ap strm1 strm2)
-- instance MonadTrans Parallel
--     where {-# INLINE lift #-}
--           lift = Parallel . lift
-- instance (Monad (Parallel m), MonadIO m) => MonadIO (Parallel m)
--     where {-# INLINE liftIO #-}
--           liftIO = Parallel . (lift . liftIO)
-- instance (Monad (Parallel m),
--           MonadThrow m) => MonadThrow (Parallel m)
--     where {-# INLINE throwM #-}
--           throwM = Parallel . (lift . throwM)
-- instance (Monad (Parallel m), MonadReader r m) => MonadReader r
--                                                               (Parallel m)
--     where {-# INLINE ask #-}
--           ask = lift ask
--           {-# INLINE local #-}
--           local f (Parallel strm) = Parallel (local f strm)
mkNestingType
    :: String -- ^ Name of the type
    -> String -- ^ Function to use for (>>=)
    -> Bool   -- ^ 'True' if (>>=) has concurrent properties
    -> Q [Dec]
mkNestingType dtNameStr bindOpStr isConcurrent = do
    typeDec <- makeTypeDec dtNameStr
    functorI <- makeStreamFunctor dtNameStr
    monadI <- mkStreamMonad dtNameStr classConstraints bindOpStr
    applicativeI <-
        mkStreamApplicative
            dtNameStr
            classConstraints
            "Streamly.Data.Stream.fromPure"
            "Control.Monad.ap"
    simpleInstances <- mkSimpleStreamInstances dtNameStr
    return $ typeDec ++ [functorI, monadI, applicativeI] ++ simpleInstances

    where

    classConstraints =
        if isConcurrent
        then ["Streamly.Internal.Data.Stream.Concurrent.MonadAsync"]
        else ["Monad"]
