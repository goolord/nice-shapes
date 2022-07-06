{-# LANGUAGE DataKinds, ExtendedDefaultRules #-}
{-# LANGUAGE MagicHash #-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact (interactIO, Event (..), Key (..), MouseButton (..), KeyState (..), Controller (..))
-- import Vivid
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Control.Monad.Writer.CPS (execWriter, MonadWriter (..))
import Data.Foldable (for_)
import Control.Concurrent (threadDelay)
import GHC.Base (Int (..), (+#))

data Mouse = Hovering Point | MB Point MouseButton KeyState
  deriving Show

data World = World
  { solidShapes :: IntMap SolidShape
  , soundShapes :: IntMap SoundShape
  , newShape :: Maybe Point -- first point save
  , mouse :: Mouse
  , selectedShape :: Shape
  }

type ShapeId = Int

data Shape
  = SolidCircle
  | SolidSquare
  | SolidTriangle
  | SolidRope
  | SolidLine
  | SoundCircle
  | SoundSquare
  | SoundTriangle
  deriving (Enum, Bounded, Eq)

data SolidShape =
    SoundShape
  | Rope Point Point ShapeId
  | SLine Point Point

data SoundShape =
    SCircle GenericShape
  | SSquare GenericShape
  | STriangle GenericShape

data GenericShape = GenericShape
  { pos :: Point
  , dXY :: Point
  , size :: Float
  }

fps :: Int
fps = 60

dt :: Float
dt = 1.0

velocity :: GenericShape -> Point -> Point
velocity gs currentAcceleration = dXY gs `sumPoints` currentAcceleration `timesPoints` (dt, dt)

position :: GenericShape -> GenericShape
position gs =
  gs { pos = sumPoints (pos gs) (dXY gs) `timesPoints` (dt, dt) }

{- |
/Subject to fusion./
-}
imap :: (Int -> a -> b) -> [a] -> [b]
imap f ls = go 0# ls
  where
    go i (x:xs) = f (I# i) x : go (i +# 1#) xs
    go _ _      = []
{-# NOINLINE [1] imap #-}

shapeSelectorUI :: Shape -> Picture
shapeSelectorUI shape = Pictures $ imap (\i x -> translate (fromIntegral i * sz) 0.0 $ shapeToButton x) [minBound..maxBound]
  where
  sz = 40.0
  shp = 30.0
  shapeToButton s = (if shape == s then color white else color black) $ rectangleWire sz sz <> case s of
    SolidCircle   -> circleSolid (shp/2)
    SolidSquare   -> rectangleSolid shp shp
    SolidTriangle -> polygon [(negate $ shp/2,negate $ shp/2), (0, shp/2), (shp/2, negate $ shp/2)]
    SolidRope     -> line [(0.0,negate $ shp/2), (0.0, shp/2)] -- todo: make squigly
    SolidLine     -> line [(0.0,negate $ shp/2), (0.0, shp/2)] -- todo: make squigly
    SoundCircle   -> circleSolid (shp/2)
    SoundSquare   -> rectangleWire shp shp
    SoundTriangle -> line [(negate $ shp/2,negate $ shp/2), (0, shp/2), (shp/2, negate $ shp/2), (negate $ shp/2,negate $ shp/2)]

-- TODO: interactively add shapes to the physics simulation
-- assign shapes to synthesizers using vivid
-- automatically boot SuperCollider

-- shapes:
-- circle -> sine
-- triangle -> triangle
-- square -> square
--
-- pitch is a function of size: size maybe subject to change
-- and maybe pitch is clamped to a scale or mode based on some settings
--
-- rope: connect shapes
-- maybe some motion machines, rotaries etc
--
-- maybe the bg should have some kind of rain/ocean-like white noise faintly in the bg
-- for ambience

newWorld :: World
newWorld = World
  { solidShapes = mempty
  , soundShapes = mempty
  , newShape = Nothing
  , mouse = Hovering (0.0, 0.0)
  , selectedShape = SolidLine
  }

mousePosPt :: Mouse -> Point
mousePosPt (Hovering p) = p
mousePosPt (MB p _ _) = p

main :: IO ()
main = do
  interactIO
    (InWindow "Wave" (800, 600) (100, 100)) -- main window
    (makeColorI 0x28 0x28 0x28 0xff) -- bg
    newWorld
    (\world -> do
      pure $ execWriter $ do
        tell $ shapeSelectorUI (selectedShape world)
        tell $ case newShape world of
          Just p -> color white $ line [p, mousePosPt $ mouse world]
          Nothing -> Blank
        for_ (IntMap.elems $ solidShapes world) $ \ss ->
          tell $ case ss of
            SLine p1 p2 -> color white $ line [p1, p2]
            Rope p1 p2 sid ->
              let shape = IntMap.lookup sid (soundShapes world)
                  p2' = case shape of
                    Just (SCircle gs) -> pos gs
                    Just (SSquare gs) -> pos gs
                    Just (STriangle gs) -> pos gs
                    Nothing -> error "delete this rope i guess"
              in color white $ line [p1, sumPoints p2 p2']
    )
    (\e world -> do
      case e of
        -- EventResize dims -> do
        --   writeIORef (windowSize state) dims
        --   pure world2
        EventMotion p -> do
          pure $ world { mouse = Hovering p }
        EventKey (MouseButton mb) keyState _mods p -> do
          let world2 = world { mouse = MB p mb keyState }
          case keyState of
            Down -> case newShape world2 of
              Nothing -> pure $ world2 { newShape =  Just p }
              Just p -> pure world2
            Up -> case newShape world2 of
              Nothing -> pure world2
              Just p -> pure world2
                { newShape = Nothing
                , solidShapes = insertNew (SLine p (mousePosPt $ mouse world)) $ solidShapes world
                }
        _ -> pure world
    )
    (\(Controller redraw _modifyViewPort) -> do
      -- redraw every 1/60 of a second
      threadDelay 16666
      redraw
      -- clearCache world
      -- modifyViewPort $ \vp -> do
      --   (x, y) <- readIORef (windowSize state)
      --   pure vp {
      --     viewPortTranslate =
      --       ( fromIntegral $ negate $ x `div` 2
      --       , fromIntegral          $ y `div` 2
      --       )
      --   }
    )

sumPoints :: Point -> Point -> Point
sumPoints (x1, y1) (x2, y2) = (x1+x2, y1+y2)

timesPoints :: Point -> Point -> Point
timesPoints (x1, y1) (x2, y2) = (x1*x2, y1*y2)

insertNew :: a -> IntMap a -> IntMap a
insertNew a im = IntMap.insert (IntMap.size im + 1) a im
