{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Qecs.ComponentTracker where

import Data.Bits
import Data.Coerce
import Data.Foldable (foldl')
import Data.Vector.Primitive.Mutable qualified as VP
import Data.WideWord (Word128, Word256)
import Data.Word
import Debug.Trace
import GHC.Generics
import Language.Haskell.TH
import Optics.Core
import Qecs.Component
import Qecs.Entity
import Unsafe.Coerce
import GHC.Types (Any)

newtype ComponentTracker = ComponentTracker Any

newtype EntityComponents = EntityComponents Any

data ComponentTrackerCode = ComponentTrackerCode
  { create :: Code Q (Int -> IO ComponentTracker),
    readEntityComponents :: Code Q (ComponentTracker -> Entity -> IO EntityComponents),
    writeEntityComponents :: Code Q (ComponentTracker -> Entity -> EntityComponents -> IO ()),
    modifyEntityComponents :: Code Q (ComponentTracker -> Entity -> (EntityComponents -> EntityComponents) -> IO ()),
    hasComponent :: Code Q (ComponentId -> EntityComponents -> Bool),
    getComponentIds :: Code Q (EntityComponents -> [ComponentId]),
    setComponent :: Code Q (ComponentId -> EntityComponents -> EntityComponents),
    unsetComponent :: Code Q (ComponentId -> EntityComponents -> EntityComponents),
    containsComponents :: Code Q (EntityComponents -> EntityComponents -> Bool),
    orComponents :: Code Q (EntityComponents -> EntityComponents -> EntityComponents),
    emptyComponents :: Code Q EntityComponents
  }
  deriving (Generic)

componentIdsToEntityComponents :: ComponentTrackerCode -> [ComponentId] -> Code Q EntityComponents
componentIdsToEntityComponents componentTrackerCode componentIds =
  [||
  foldl'
    (flip $$(componentTrackerCode ^. #setComponent))
    $$(componentTrackerCode ^. #emptyComponents)
    componentIds
  ||]

createComponentTrackerCode :: Int -> ComponentTrackerCode
createComponentTrackerCode componentAmount
  | componentAmount <= 32 = componentTrackerTemplate [||\a -> let !x = a in x :: Word32||]
  | componentAmount <= 64 = componentTrackerTemplate [||id @Word64||]
  | componentAmount <= 128 = componentTrackerTemplate [||id @Word128||]
  | componentAmount <= 256 = componentTrackerTemplate [||id @Word256||]
  | otherwise = error "Cannot handle more than 256 components for now"

componentTrackerTemplate :: (Bits a, Show a, FiniteBits a, VP.Prim a, Num a) => Code Q (a -> a) -> ComponentTrackerCode
componentTrackerTemplate fixType =
  ComponentTrackerCode
    { create =
        [||
        \entityAmount ->
          unsafeCoerce <$> VP.replicate entityAmount ($$fixType 0)
        ||],
      readEntityComponents =
        [||
        \tracker (Entity e) ->
          $$toEntityComponents <$> VP.unsafeRead (unsafeCoerce tracker) (fromIntegral e)
        ||],
      writeEntityComponents =
        [||
        ( \(!tracker) (!(Entity e)) (!entityComponents) -> do
            (VP.write (unsafeCoerce tracker) (fromIntegral e) ($$fromEntityComponents entityComponents) :: IO ())
        )
        ||],
      modifyEntityComponents =
        [||
        \tracker (Entity e) f ->
          VP.unsafeModify (unsafeCoerce tracker) ($$fromEntityComponents . f . $$toEntityComponents) (fromIntegral e)
        ||],
      hasComponent =
        [||
        \(ComponentId cId) entityComponents ->
          testBit ($$fromEntityComponents entityComponents) cId
        ||],
      getComponentIds =
        [||
        \entityComponents ->
          coerce $
            filter
              (testBit ($$fromEntityComponents entityComponents))
              [0 .. finiteBitSize ($$fromEntityComponents undefined) - 1]
        ||],
      setComponent =
        [||
        \(ComponentId cId) entityComponents ->
          $$toEntityComponents $ setBit ($$fromEntityComponents entityComponents) cId
        ||],
      unsetComponent =
        [||
        \(ComponentId cId) entityComponents ->
          $$toEntityComponents $ clearBit ($$fromEntityComponents entityComponents) cId
        ||],
      containsComponents =
        [||
        \a b ->
          $$fromEntityComponents a .&. $$fromEntityComponents b == $$fromEntityComponents a
        ||],
      orComponents =
        [||
        \a b ->
          $$toEntityComponents ($$fromEntityComponents a .|. $$fromEntityComponents b)
        ||],
      emptyComponents = [||$$toEntityComponents 0||]
    }
  where
    fromEntityComponents = [|| \a ->
      let !x = a
      in $$fixType $ unsafeCoerce @EntityComponents x||]
    toEntityComponents = [|| \a ->
        let !x = a
        in unsafeCoerce @_ @EntityComponents $ $$fixType x
      ||]