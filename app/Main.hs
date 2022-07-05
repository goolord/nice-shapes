{-# LANGUAGE DataKinds, ExtendedDefaultRules #-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss.Interface.IO.Interact (interactIO, Event (..), Key (..), MouseButton (..), KeyState (..), Controller (..))
-- import Vivid
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Control.Monad.Writer.CPS (execWriter, MonadWriter (..))
import Data.Foldable (for_)

data Mouse = Hovering Point | MB Point MouseButton KeyState
  deriving Show

data World = World
  { solidShapes :: IntMap SolidShape
  , soundShapes :: IntMap SoundShape
  , newShape :: Maybe Point -- first point save
  , mouse :: Mouse
  }

type ShapeId = Int

data SolidShape =
    SoundShape
  | Rope Point Point ShapeId
  | SLine Point Point

data SoundShape =
    SCircle Float
  | SSquare Float
  | STriangle Float

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
        tell $ case newShape world of
          Just p -> color white $ line [p, mousePosPt $ mouse world]
          Nothing -> Blank
        for_ (IntMap.elems $ solidShapes world) $ \ss ->
          tell $ case ss of
            SLine p1 p2 -> color white $ line [p1, p2]
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
    (\(Controller _redraw _modifyViewPort) -> do
      -- update every n seconds
      -- clearCache world
      -- modifyViewPort $ \vp -> do
      --   (x, y) <- readIORef (windowSize state)
      --   pure vp {
      --     viewPortTranslate =
      --       ( fromIntegral $ negate $ x `div` 2
      --       , fromIntegral          $ y `div` 2
      --       )
      --   }
      pure ()
    )


insertNew :: a -> IntMap a -> IntMap a
insertNew a im = IntMap.insert (IntMap.size im + 1) a im
