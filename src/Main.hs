{- Copyright (C) 2015 Yuval Langer

   This file is part of Yasam Sim.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Affero General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>. -}


{-: LANGUAGE OverloadedParenthesis :-}
{-# LANGUAGE TemplateHaskell #-}


module Main where


import           Control.Applicative              (liftA3, (<$>))
import           Control.Arrow                    ((>>>))
import           Control.Lens
    ( both
    , makeLenses
    , over
    , (%=)
    , (&)
    , (*~)
    , (+=)
    , (+~)
    , (^.)
    , _2
    )
import           Control.Monad                    (forM, (<=<), (>=>))
import           Control.Monad.State.Strict       (execState)
import           Data.Char                        (toLower)
import           Data.List                        (sortOn)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (catMaybes, fromJust)
import           Data.Monoid                      ((<>))
import           Data.Set
    ( Set
    , delete
    , empty
    , insert
    , member
    )
import           Debug.Trace                      (traceIO)
import           Graphics.Gloss.Data.Color        (white)
import           Graphics.Gloss.Data.Picture
    ( color
    , polygon
    , rectanglePath
    , translate
    )
import           Graphics.Gloss.Data.Vector       (Vector)
import           Graphics.Gloss.Interface.IO.Game
    ( Display (InWindow)
    , Event (EventKey)
    , Key (Char)
    , KeyState (Down, Up)
    , Picture
    , black
    , playIO
    )
import           Graphics.Gloss.Juicy             (loadJuicyPNG)


sceneWidth, sceneHeight :: Int
sceneWidth  = 640
sceneHeight = 480

data Entity
    = Hero
    | Crate [Entity]
    | Fist
    | Club
    | Medipack
    deriving (Eq, Ord, Show)

data Direction = North
         | West        | East
               | South
    deriving (Eq, Ord, Show)

type PictureAssets
    = Map.Map (Entity, Direction, Int) Picture

data Character
    = Character
    { _characterPosition    :: Vector
    , _characterZ           :: Float
    , _characterPower       :: Int
    , _characterSpeed       :: Float
    , _characterWeapon      :: Entity
    , _characterHealth      :: Int
    , _characterInventory   :: [Entity]
    , _characterEnergy      :: Int
    , _characterState       :: CharacterActionState
    , _characterOrientation :: Direction
    , _characterDirection   :: Set Direction
    }


data CharacterActionState
    = Walk
    | Jump
    | Stand


type Item = (Vector, Entity)


data Game
    = Game
    { _player               :: Character
    , _civilians            :: [Character]
    , _items                :: [Item]
    , _player1KeyboardState :: Set KeyboardButton
    , _player2KeyboardState :: Set KeyboardButton
    , _gameTime             :: Float
    }


data KeyboardButton = ButtonNorth
    | ButtonWest                  | ButtonEast
                    | ButtonSouth
    | ButtonPunch
    deriving (Eq, Ord, Show)


makeLenses ''Game
makeLenses ''Character


drawScene ::
    PictureAssets
    -> Game
    -> IO Picture
drawScene pictureAssets game = return (background <> sortedPictures)
    where
    background =
        rectanglePath
            (fromIntegral sceneWidth)
            (fromIntegral sceneHeight)
        & (polygon >>> color white)
    sortedPictures = mconcat . map snd . reverse $ sortOn fst pictures
    pictures = playerPicture' : itemsPicture'
    itemsPicture' =
      [( y
       , (pictureAssets Map.! (entity, North, 0))
         & uncurry translate xy
         )
      | (xy@(x, y), entity) <- game^.items
      ]
    playerPicture' =
      ( snd (game^.player.characterPosition)
      , heroSprite & uncurry translate playerDrawPosition
      )
    playerDrawPosition =
      game ^. player
      & (\p ->
        (p ^. characterPosition)
        & (_2 +~ p ^. characterZ))
    heroSprite =
        case game ^. player . characterOrientation of
            North -> cycleSprite (game^.player.characterDirection) Hero North 4 4
            South -> cycleSprite (game^.player.characterDirection) Hero South 4 4
            West  -> cycleSprite (game^.player.characterDirection) Hero West  4 4
            East  -> cycleSprite (game^.player.characterDirection) Hero East  4 4
    cycleSprite ::
        Set Direction -- ^ Directions to which the sprite moves
        -> Entity     -- ^ Name of sprite cycle
        -> Direction  -- ^ Direction sprite is facing
        -> Int        -- ^ Number of sprites in cycle
        -> Float      -- ^ Number of times per second a the sprites will change
        -> Picture
    cycleSprite directions entity direction numberOfFrames fps
        | empty == directions = pictureAssets Map.! (entity, direction, 0)
        | otherwise           = pictureAssets Map.! (entity, direction, floor ((game^.gameTime) * fps) `mod` numberOfFrames)


handleInput ::
    Event
    -> Game
    -> IO Game
handleInput event@(EventKey (Char 'w') keyState _ _) game = do
  traceOldStateIO event game
  let newGame = handleDirectionKey game North ButtonNorth keyState
  traceNewStateIO newGame
  return newGame
handleInput event@(EventKey (Char 's') keyState _ _) game = do
  traceOldStateIO event game
  let newGame = handleDirectionKey game South ButtonSouth keyState
  traceNewStateIO newGame
  return newGame
handleInput event@(EventKey (Char 'a') keyState _ _) game = do
  traceOldStateIO event game
  let newGame = handleDirectionKey game West ButtonWest keyState
  traceNewStateIO newGame
  return newGame
handleInput event@(EventKey (Char 'd') keyState _ _) game = do
  traceOldStateIO event game
  let newGame = handleDirectionKey game East ButtonEast keyState
  traceNewStateIO newGame
  return newGame
handleInput _ game = return game


traceOldStateIO :: Event -> Game -> IO ()
traceOldStateIO event game = return ()
  --traceIO $ "EventKey:\n" ++ "\n" ++
  --  "\t" ++ show event ++ "\n" ++
  --  "\t" ++ show (game^.player.characterDirection) ++ "\n" ++
  --  "\t" ++ show (game^.player1KeyboardState) ++ "\n" ++
  --  "\t" ++ show (game^.gameTime) ++ "\n"


traceNewStateIO :: Game -> IO ()
traceNewStateIO newGame = return ()
  -- traceIO $ "\t" ++ show (newGame^.player.characterDirection) ++ "\n" ++
  --   "\t" ++ show (newGame^.player1KeyboardState)

handleDirectionKey ::
  Game
  -> Direction      -- ^ The direction associated with the button.
  -> KeyboardButton -- ^ The button whose state had changed.
  -> KeyState       -- ^ Whether the button went Up or Down.
  -> Game
handleDirectionKey
  game
  direction
  button
  keyState =
    over (player . characterDirection) characterDirectionUpdate
    . over player1KeyboardState player1KeyboardStateUpdate
    . over (player . characterOrientation) characterOrientationUpdate $ game
  where
  characterDirectionUpdate = case keyState of
    Down -> insert direction . delete oppositeDirection
    Up   -> delete direction

  player1KeyboardStateUpdate = case keyState of
    Down -> insert button
    Up   -> delete button

  characterOrientationUpdate = case keyState of
    Down -> const direction
    Up   -> id

  oppositeDirection = turnAroundDirection direction


turnAroundDirection :: Direction -> Direction
turnAroundDirection North = South
turnAroundDirection West  = East
turnAroundDirection East  = West
turnAroundDirection South = North


stepGame ::
    Float
    -> Game
    -> IO Game
stepGame time =
    return . execState (do
        player %= moveCharacter time
        gameTime += time)

moveCharacter ::
    Float
    -> Character
    -> Character
moveCharacter time character =
    character & characterPosition +~ (
        (xDirection, yDirection)
        & ((both *~ time)
            >>> (both *~ character ^. characterSpeed)))
    where
    oldCharacterDirection = character ^. characterDirection :: Set Direction
    xDirection
        | member West  oldCharacterDirection = -1
        | member East oldCharacterDirection =  1
        | otherwise = 0
    yDirection
        | member North oldCharacterDirection =  1
        | member South oldCharacterDirection = -1
        | otherwise = 0


initialPlayer :: Character
initialPlayer
    = Character
    { _characterPosition    = (-200, 0)
    , _characterZ           = 0
    , _characterPower       = 30
    , _characterSpeed       = 75
    , _characterWeapon      = Club
    , _characterHealth      = 100
    , _characterEnergy      = 100
    , _characterState       = Stand
    , _characterOrientation = East
    , _characterDirection   = empty
    }


initialGame :: Game
initialGame
    = Game
    { _player    = initialPlayer
    , _civilians = []
    , _items     = [((0, 0), Club)]
    , _player1KeyboardState = empty
    , _player2KeyboardState = empty
    , _gameTime = 0
    }


loadPictureAssets :: IO PictureAssets
loadPictureAssets =
  let cartesianProduct = liftA3 ((,,)) entities directions [0..4]
      entities = [Hero, Club]
      directions = [North, South, East, West]
  in ( Map.fromList
     . (uncurry zip)
     . (\(a, b) -> (a, catMaybes b))
     . unzip
     ) <$>
     (forM cartesianProduct $ \(entity, direction, frameIndex) -> do
         maybePicture <- loadPictureAsset entity direction frameIndex
         return ((entity, direction, frameIndex), maybePicture))

loadPictureAsset ::
  Entity
  -> Direction
  -> Int
  -> IO (Maybe Picture)
loadPictureAsset entity direction frameIndex =
  loadJuicyPNG (
      "assets/"
      ++ show entity
      ++ show direction
      ++ show frameIndex
      ++ ".png")


main :: IO ()
main = do
    assets <- loadPictureAssets
    print . map fst . Map.toList $ assets
    playIO
        display
        backColor
        30
        initialGame
        (drawScene assets)
        handleInput
        stepGame
    where
    display = InWindow "Yasam Sim" (1, 1) (sceneWidth, sceneHeight)
    backColor = black
    fps = 30
