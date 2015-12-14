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


import           Control.Applicative              (liftA2, (<$>))
import           Control.Arrow                    ((>>>))
import           Control.Lens
import           Control.Monad                    (forM, (<=<), (>=>))
import           Control.Monad.State.Strict       (execState)
import           Data.Char                        (toLower)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromJust)
import           Data.Monoid                      ((<>))
import           Data.Set                         (Set, delete, empty, insert,
                                                   member)
import           Debug.Trace                      (traceIO)
import           Graphics.Gloss.Data.Color        (white)
import           Graphics.Gloss.Data.Picture      (color, polygon,
                                                   rectanglePath, translate)
import           Graphics.Gloss.Data.Vector       (Vector)
import           Graphics.Gloss.Interface.IO.Game (Display (InWindow),
                                                   Event (EventKey), Key (Char),
                                                   KeyState (Down, Up), Picture,
                                                   black, playIO)
import           Graphics.Gloss.Juicy             (loadJuicyPNG)


sceneWidth, sceneHeight :: Int
sceneWidth  = 640
sceneHeight = 480


data ImageAsset
    = HeroFacingLeft
    | HeroFacingRight
    | HeroFacingBack
    | HeroFacingFront
    deriving (Eq, Ord, Show)


imageAssetName ::
    ImageAsset
    -> String
imageAssetName = (toLower <$>) . show


data Direction
    = DirectionLeft
    | DirectionRight
    | DirectionUp
    | DirectionDown
    deriving (Eq, Ord, Show)

data Character
    = Character
    { _characterPosition    :: Vector
    , _characterZ           :: Float
    , _characterPower       :: Int
    , _characterSpeed       :: Float
    , _characterWeapon      :: Weapon
    , _characterHealth      :: Int
    , _characterEnergy      :: Int
    , _characterState       :: CharacterActionState
    , _characterOrientation :: Direction
    , _characterDirection   :: Set Direction
    }


data CharacterActionState
    = Walk
    | Jump
    | Stand


data Weapon
    = Fist
    | Club


data Item
    = Crate
    | Medipack


data Game
    = Game
    { _player               :: Character
    , _civilians            :: [Character]
    , _items                :: [Item]
    , _player1KeyboardState :: Set KeyboardButton
    , _player2KeyboardState :: Set KeyboardButton
    , _gameTime             :: Float
    }


data KeyboardButton
    = ButtonUp
    | ButtonDown
    | ButtonLeft
    | ButtonRight
    | ButtonPunch
    deriving (Eq, Ord, Show)


makeLenses ''Game
makeLenses ''Character


drawScene ::
    Map.Map String Picture
    -> Game
    -> IO Picture
drawScene assets game = return (background <> playerPicture')
    where
    background =
        rectanglePath
            (fromIntegral sceneWidth)
            (fromIntegral sceneHeight)
        & (polygon >>> color white)
    playerPicture' =
        (assets Map.! heroDirectedLens)
        & uncurry translate playerDrawPosition
    playerDrawPosition =
        game ^. player
        & (\p ->
            (p ^. characterPosition)
            & (_2 +~ p ^. characterZ))
    heroDirectedLens =
        case game ^. player . characterOrientation of
            DirectionLeft  -> cycleSprite (game^.player.characterDirection) HeroFacingLeft  4 4
            DirectionRight -> cycleSprite (game^.player.characterDirection) HeroFacingRight 4 4
            DirectionDown  -> cycleSprite (game^.player.characterDirection) HeroFacingBack  4 4
            DirectionUp    -> cycleSprite (game^.player.characterDirection) HeroFacingFront 4 4
    cycleSprite ::
        Set Direction -- Directions to which the sprite moves
        -> ImageAsset -- Name of sprite cycle
        -> Int        -- Number of sprites in cycle
        -> Float      -- Number of times per second a the sprites will change
        -> String
    cycleSprite directions asset n fps
        | empty == directions = show (asset 0)
        | otherwise           = show (asset (floor ((game^.gameTime) * fps) `mod` n))


handleInput ::
    Event
    -> Game
    -> IO Game
handleInput event game =
    case event of
        EventKey (Char 'w') keyState _ _ -> do
            traceOldStateIO
            let newGame = handleDirectionKey DirectionUp ButtonUp keyState
            traceNewStateIO newGame
            return newGame
        EventKey (Char 's') keyState _ _ -> do
            traceOldStateIO
            let newGame = handleDirectionKey DirectionDown ButtonDown keyState
            traceNewStateIO newGame
            return newGame
        EventKey (Char 'a') keyState _ _ -> do
            traceOldStateIO
            let newGame = handleDirectionKey DirectionLeft ButtonLeft keyState
            traceNewStateIO newGame
            return newGame
        EventKey (Char 'd') keyState _ _ -> do
            traceOldStateIO
            let newGame = handleDirectionKey DirectionRight ButtonRight keyState
            traceNewStateIO newGame
            return newGame
        _ -> return game
    where
    traceOldStateIO =
        traceIO $ "EventKey:\n" ++ "\n" ++
            "\t" ++ show event ++ "\n" ++
            "\t" ++ show (game^.player.characterDirection) ++ "\n" ++
            "\t" ++ show (game^.player1KeyboardState) ++ "\n" ++
            "\t" ++ show (game^.gameTime) ++ "\n"
    traceNewStateIO newGame =
        traceIO $ "\t" ++ show (newGame^.player.characterDirection) ++ "\n" ++
            "\t" ++ show (newGame^.player1KeyboardState)
    handleDirectionKey
        :: Direction      -- ^ The direction associated with the button.
        -> KeyboardButton -- ^ The button whose state had changed.
        -> KeyState       -- ^ Whether the button went Up or Down.
        -> Game
    handleDirectionKey
        direction
        button
        keyState =
            over (player . characterDirection) characterDirectionUpdate
            . over player1KeyboardState player1KeyboardStateUpdate
            . over (player . characterOrientation) characterOrientationUpdate $ game
        where
        characterDirectionUpdate = case keyState of
            Down -> insert direction . delete directionOpposite
            Up   -> delete direction
        player1KeyboardStateUpdate = case keyState of
            Down -> insert button
            Up   -> delete button
        characterOrientationUpdate = case keyState of
            Down -> const direction
            Up   -> id
        directionOpposite
            | direction == DirectionLeft  = DirectionRight
            | direction == DirectionRight = DirectionLeft
            | direction == DirectionUp    = DirectionDown
            | otherwise                   = DirectionUp


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
        | member DirectionLeft  oldCharacterDirection = -1
        | member DirectionRight oldCharacterDirection =  1
        | otherwise = 0
    yDirection
        | member DirectionUp   oldCharacterDirection =  1
        | member DirectionDown oldCharacterDirection = -1
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
    , _characterOrientation = DirectionRight
    , _characterDirection   = empty
    }


initialGame :: Game
initialGame
    = Game
    { _player    = initialPlayer
    , _civilians = []
    , _items     = []
    , _player1KeyboardState = empty
    , _player2KeyboardState = empty
    , _gameTime = 0
    }


loadPictureAssets :: IO (Map.Map String Picture)
loadPictureAssets =
    (Map.unions <$>) . forM (liftA2 (,) cycleNames [0..4]) $ \(name, i) ->
        Map.singleton (name ++ show i) . fromJust
            <$> loadJuicyPNG ("assets/" ++ name ++ show i ++ ".png")
    where
    cycleNames = ["heroFacingFront", "heroFacingBack", "heroFacingRight", "heroFacingLeft"]


main :: IO ()
main = do
    assets <- loadPictureAssets
    playIO
        (InWindow "Yasam Sim" (1, 1) (sceneWidth, sceneHeight))
        black
        30
        initialGame
        (drawScene assets)
        handleInput
        stepGame
    where
    display = InWindow "Yasam Sim" (1, 1) (sceneWidth, sceneHeight)
    backColor = black
    fps = 30
