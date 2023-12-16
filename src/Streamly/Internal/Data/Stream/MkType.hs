{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module      : Streamly.Internal.Data.Stream.MkType
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Stream.MkType
    (
    -- * Imports for Examples
    -- $setup

    -- * Template Haskell Macros
      mkZipType
    , mkCrossType

    -- * Re-exports
    , MonadIO(..)
    , MonadThrow(..)
    , MonadReader(..)
    , MonadTrans(..)
    , ap
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

import Control.Monad (ap)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Prelude hiding (repeat)

-- $setup
-- >>> :m
-- >>> import Language.Haskell.TH
-- >>> import qualified Streamly.Data.Stream.Prelude as Stream
-- >>> import Streamly.Internal.Data.Stream.MkType

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

{-# INLINE singleton #-}
singleton :: a -> [a]
singleton x = [x]

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
_Stream = mkName "Stream.Stream"

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

_IsList :: Name
_IsList = mkName "IsList"

_IsString :: Name
_IsString = mkName "IsString"

_Eq :: Name
_Eq = mkName "Eq"

_Ord :: Name
_Ord = mkName "Ord"

_Traversable :: Name
_Traversable = mkName "Traversable"

_Identity :: Name
_Identity = mkName "Identity"

_Read :: Name
_Read = mkName "Read"

_Show :: Name
_Show = mkName "Show"

_show :: Name
_show = mkName "show"

_readPrec :: Name
_readPrec = mkName "readPrec"

_Semigroup :: Name
_Semigroup = mkName "Semigroup"

_Monoid :: Name
_Monoid = mkName "Monoid"

_Foldable :: Name
_Foldable = mkName "Foldable"

--------------------------------------------------------------------------------
-- Simple derivations
--------------------------------------------------------------------------------

-- Requires TypeFamilies and UndecidableInstances
derivIsListIdent :: Name -> Q Dec
derivIsListIdent _Type =
    standaloneDerivD
        (pure [])
        (appT (conT _IsList) (foldl1 appT [conT _Type, conT _Identity, varT _a]))

derivIsStringIdent :: Name -> Q Dec
derivIsStringIdent _Type =
    standaloneDerivD
        (singleton <$> [t|$(varT _a) ~ Char|])
        (appT
             (conT _IsString)
             (foldl1 appT [conT _Type, conT _Identity, varT _a]))

derivEqIdent :: Name -> Q Dec
derivEqIdent _Type =
    standaloneDerivD
        (singleton <$> [t|Eq $(varT _a)|])
        (appT (conT _Eq) (foldl1 appT [conT _Type, conT _Identity, varT _a]))

derivOrdIdent :: Name -> Q Dec
derivOrdIdent _Type =
    standaloneDerivD
        (singleton <$> [t|Ord $(varT _a)|])
        (appT (conT _Ord) (foldl1 appT [conT _Type, conT _Identity, varT _a]))

{-
derivTraversableIdent :: Name -> Q Dec
derivTraversableIdent _Type =
    standaloneDerivD
        (pure [])
        (appT
             (conT _Traversable)
             (foldl1 appT [conT _Type, conT _Identity]))
-}

showInstance :: Name -> Q Dec
showInstance _Type =
    instanceD
        (singleton <$> appT (conT _Show) (varT _a))
        (appT (conT _Show) (foldl1 appT [conT _Type, conT _Identity, varT _a]))
        [ pragInlD _show Inline FunLike AllPhases
        , funD
              _show
              [ clause
                    [conP _Type [varP _strm]]
                    (normalB (appE (varE _show) (varE _strm)))
                    []
              ]
        ]

readInstance :: Name -> Q Dec
readInstance _Type =
    instanceD
        (singleton <$> appT (conT _Read) (varT _a))
        (appT (conT _Read) (foldl1 appT [conT _Type, conT _Identity, varT _a]))
        [ pragInlD _readPrec Inline FunLike AllPhases
        , funD
              _readPrec
              [ clause
                    []
                    (normalB
                        (foldl1 appE [varE _fmap, conE _Type, varE _readPrec])
                    )
                    []
              ]
        ]

functorInstance :: Name -> Q Dec
functorInstance _Type = do
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

monadtransInstance :: Name -> Q Dec
monadtransInstance _Type =
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
                              (Just (varE (mkName "Stream.fromEffect")))))
                    []
              ]
        ]

monadioInstance :: Name -> Q Dec
monadioInstance _Type =
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
                                        (Just (varE (mkName "Stream.fromEffect")))
                                        (varE _dotOp)
                                        (Just (varE _liftIO))))))
                    []
              ]
        ]

monadthrowInstance :: Name -> Q Dec
monadthrowInstance _Type =
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
                                        (Just (varE (mkName "Stream.fromEffect")))
                                        (varE _dotOp)
                                        (Just (varE _throwM))))))
                    []
              ]
        ]

{-
monadreaderInstance :: Name -> Q Dec
monadreaderInstance _Type =
    instanceD
        (sequence
             [ appT (conT _Monad) (appT (conT _Type) (varT _m))
             , appT (appT (conT _MonadReader) (varT _r)) (varT _m)
             ])
        (appT (appT (conT _MonadReader) (varT _r)) (appT (conT _Type) (varT _m)))
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
                              (appE (appE (varE _local) (varE _f)) (varE _strm))))
                    []
              ]
        ]
-}

--------------------------------------------------------------------------------
-- Type declaration
--------------------------------------------------------------------------------

typeDec :: String -> [Name] -> Q [Dec]
typeDec dtNameStr toDerive = do
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
            [derivClause Nothing (conT <$> toDerive) | not (null toDerive)]
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

--------------------------------------------------------------------------------
-- Main deivations
--------------------------------------------------------------------------------

mkStreamApplicative :: Bool -> String -> [String] -> String -> String -> Q Dec
mkStreamApplicative isMonad dtNameStr ctxM pureDefStr apDefStr =
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
              [ if isMonad
                then apClauseMonad
                else apClauseApplicative
              ]
        ]

    where

    _Type = mkName dtNameStr
    _pureDef = mkName pureDefStr
    _apDef = mkName apDefStr
    apClauseMonad = clause [] (normalB (varE _apDef)) []
    apClauseApplicative =
        clause
            [conP _Type [varP _strm1], conP _Type [varP _strm2]]
            (normalB
                 (appE
                      (conE _Type)
                      (appE
                           (appE (varE _apDef) (varE _strm1))
                           (varE _strm2))))
            []

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

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

flattenDec :: [Q [Dec]] -> Q [Dec]
flattenDec [] = pure []
flattenDec (ma:mas) = do
    a <- ma
    as <- flattenDec mas
    pure (a ++ as)

-- | Create a type with a zip-like applicative.
--
-- >>> expr <- runQ (mkZipType "ZipStream" "zipApply" False)
-- >>> putStrLn $ pprint expr
-- newtype ZipStream m a
--     = ZipStream (Stream.Stream m a)
--     deriving Foldable
-- mkZipStream :: Stream.Stream m a -> ZipStream m a
-- mkZipStream = ZipStream
-- unZipStream :: ZipStream m a -> Stream.Stream m a
-- unZipStream (ZipStream strm) = strm
-- deriving instance IsList (ZipStream Identity a)
-- deriving instance a ~
--                   GHC.Types.Char => IsString (ZipStream Identity a)
-- deriving instance GHC.Classes.Eq a => Eq (ZipStream Identity a)
-- deriving instance GHC.Classes.Ord a => Ord (ZipStream Identity a)
-- instance Show a => Show (ZipStream Identity a)
--     where {{-# INLINE show #-}; show (ZipStream strm) = show strm}
-- instance Read a => Read (ZipStream Identity a)
--     where {{-# INLINE readPrec #-}; readPrec = fmap ZipStream readPrec}
-- instance Monad m => Functor (ZipStream m)
--     where {{-# INLINE fmap #-};
--            fmap f (ZipStream strm) = ZipStream (fmap f strm)}
-- instance Monad m => Applicative (ZipStream m)
--     where {{-# INLINE pure #-};
--            pure = ZipStream . Stream.repeat;
--            {-# INLINE (<*>) #-};
--            (<*>) (ZipStream strm1) (ZipStream strm2) = ZipStream (zipApply strm1 strm2)}
mkZipType
    :: String -- ^ Name of the type
    -> String -- ^ Function to use for (\<*\>)
    -> Bool   -- ^ 'True' if (\<*\>) requires MonadAsync constraint (concurrent)
    -> Q [Dec]
mkZipType dtNameStr apOpStr isConcurrent =
    flattenDec
        [ typeDec dtNameStr [_Foldable | not isConcurrent]
        , sequence
              $ if not isConcurrent
                then [ derivIsListIdent _Type
                     , derivIsStringIdent _Type
                     , derivEqIdent _Type
                     , derivOrdIdent _Type
                     -- , derivTraversableIdent _Type
                     , showInstance _Type
                     , readInstance _Type
                     ]
                else []
        , sequence
              [ functorInstance _Type
              , mkStreamApplicative
                    False
                    dtNameStr
                    classConstraints
                    "Stream.repeat"
                    apOpStr
              ]
        ]

    where

    _Type = mkName dtNameStr
    classConstraints =
        if isConcurrent
        then ["Stream.MonadAsync"]
        else ["Monad"]

-- | Create a type with specific stream combination properties.
--
-- >>> expr <- runQ (mkCrossType "Parallel" "parBind" True)
-- >>> putStrLn $ pprint expr
-- newtype Parallel m a = Parallel (Stream.Stream m a)
-- mkParallel :: Stream.Stream m a -> Parallel m a
-- mkParallel = Parallel
-- unParallel :: Parallel m a -> Stream.Stream m a
-- unParallel (Parallel strm) = strm
-- instance Monad m => Functor (Parallel m)
--     where {{-# INLINE fmap #-};
--            fmap f (Parallel strm) = Parallel (fmap f strm)}
-- instance Stream.MonadAsync m => Monad (Parallel m)
--     where {{-# INLINE (>>=) #-};
--            (>>=) (Parallel strm1) f = let f1 a = unParallel (f a)
--                                        in Parallel (parBind strm1 f1)}
-- instance Stream.MonadAsync m => Applicative (Parallel m)
--     where {{-# INLINE pure #-};
--            pure = Parallel . Stream.fromPure;
--            {-# INLINE (<*>) #-};
--            (<*>) = ap}
-- instance (Monad (Parallel m), MonadIO m) => MonadIO (Parallel m)
--     where {{-# INLINE liftIO #-};
--            liftIO = Parallel . (Stream.fromEffect . liftIO)}
-- instance (Monad (Parallel m),
--           MonadThrow m) => MonadThrow (Parallel m)
--     where {{-# INLINE throwM #-};
--            throwM = Parallel . (Stream.fromEffect . throwM)}

mkCrossType
    :: String -- ^ Name of the type
    -> String -- ^ Function to use for (>>=)
    -> Bool   -- ^ 'True' if (>>=) requires MonadAsync constraint (concurrent)
    -> Q [Dec]
mkCrossType dtNameStr bindOpStr isConcurrent =
    flattenDec
        [ typeDec dtNameStr [_Foldable | not isConcurrent]
        , sequence
              $ if not isConcurrent
                then [ derivIsListIdent _Type
                     , derivIsStringIdent _Type
                     , derivEqIdent _Type
                     , derivOrdIdent _Type
                     -- , derivTraversableIdent _Type
                     , showInstance _Type
                     , readInstance _Type
                     ]
                else []
        , sequence $
              [ functorInstance _Type
              , mkStreamMonad dtNameStr classConstraints bindOpStr
              , mkStreamApplicative
                    True
                    dtNameStr
                    classConstraints
                    "Stream.fromPure"
                    "ap"
              , monadioInstance _Type
              , monadthrowInstance _Type
              -- , monadreaderInstance _Type
              ] ++ [monadtransInstance _Type | not isConcurrent]
        ]

    where

    _Type = mkName dtNameStr
    classConstraints =
        if isConcurrent
        then ["Stream.MonadAsync"]
        else ["Monad"]
