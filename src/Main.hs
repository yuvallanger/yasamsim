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


import Control.Lens
import Debug.Trace
	( traceIO
	)
import Data.Set
	( Set
	, empty
	, insert
	, delete
	, member
	)
import Data.Monoid
	( (<>)
	, mempty
	)
import Graphics.Gloss.Data.Picture
	( blank
	, color
	, rectangleWire
	, rectanglePath
	, polygon
	, translate
	)
import Graphics.Gloss.Data.Color
	( violet
	, white
	)
import Graphics.Gloss.Data.Vector
	( Vector
	)
import Graphics.Gloss.Interface.IO.Game
	( playIO
	, Event
		( EventKey
		)
	, Key
		( Char
		)
	, KeyState
		( Down
		, Up
		)
	, Display (InWindow)
	, Picture
	, black
	)


sceneWidth, sceneHeight :: Int
sceneWidth  = 640
sceneHeight = 480


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
	{ _player                :: Character
	, _civilians             :: [Character]
	, _items                 :: [Item]
	, _player1KeyboardState  :: Set KeyboardButton
	, _player2KeyboardState  :: Set KeyboardButton
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


drawScene :: Game -> IO Picture
drawScene game = return (background <> playerPicture)
	where
	background = drawBackground game
	playerPicture = drawPlayer game

drawBackground :: Game -> Picture
drawBackground game = color white . polygon $ rectanglePath (fromIntegral sceneWidth) (fromIntegral sceneHeight)

drawPlayer :: Game -> Picture
drawPlayer game = translate x (y+z) . color violet $ rectangleWire 25 35
	where
	oldPlayer = game^.player
	(x, y)    = oldPlayer^.characterPosition
	z         = oldPlayer^.characterZ


handleInput :: Event -> Game -> IO Game
handleInput event game =
	case event of
		EventKey (Char 'w') keyState _ _ -> do
			traceOldStateIO
			let newGame = handleDirectionKey
				game
				DirectionDown
				DirectionUp
				ButtonUp
				keyState
			traceNewStateIO newGame
			return newGame
		EventKey (Char 's') keyState _ _ -> do
			traceOldStateIO
			let newGame = handleDirectionKey
				game
				DirectionUp
				DirectionDown
				ButtonDown
				keyState
			traceNewStateIO newGame
			return newGame
		EventKey (Char 'a') keyState _ _ -> do
			traceOldStateIO
			let newGame = handleDirectionKey
				game
				DirectionRight
				DirectionLeft
				ButtonLeft
				keyState
			traceNewStateIO newGame
			return newGame
		EventKey (Char 'd') keyState _ _ -> do
			traceOldStateIO
			let newGame = handleDirectionKey
				game
				DirectionLeft
				DirectionRight
				ButtonRight
				keyState
			traceNewStateIO newGame
			return newGame
		_ -> return game
	where
	traceOldStateIO =
		traceIO $ "EventKey:\n" ++ "\n" ++
			"\t" ++ show event ++ "\n" ++
			"\t" ++ show (game^.player.characterDirection) ++ "\n" ++
			"\t" ++ show (game^.player1KeyboardState) ++ "\n"
	traceNewStateIO newGame =
		traceIO $ "\t" ++ show (newGame^.player.characterDirection) ++ "\n" ++
			"\t" ++ show (newGame^.player1KeyboardState)
		

handleDirectionKey
	:: Game
	-> Direction      -- ^ The direction at the back of the character.
	-> Direction      -- ^ The direction at the front of the character.
	-> KeyboardButton -- ^ The button we wish to change.
	-> KeyState       -- ^ Whether the button went Up or Down.
	-> Game
handleDirectionKey
	game
	directionFrom
	directionTo
	buttonTo
	keyState = game & case keyState of
		Down ->
			over player1KeyboardState
				(insert buttonTo)
			. over (player . characterDirection)
				(insert directionTo . delete directionFrom)
		Up ->
			over (player . characterDirection)
				(delete directionTo)
			. over player1KeyboardState
				(delete buttonTo)


stepGame :: Float -> Game -> IO Game
stepGame time game =
	return $ player %~ moveCharacter time $ game

moveCharacter
	:: Float
	-> Character
	-> Character
moveCharacter time character =
	character & characterPosition +~ (
		(xDirection, yDirection)
		& (both *~ character ^. characterSpeed)
		. (both *~ time))
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
	, _characterSpeed       = 30
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
	}


main :: IO ()
main = do
	playIO
		(InWindow "Yasam Sim" (1, 1) (sceneWidth, sceneHeight))
		black
		30
		initialGame
		drawScene
		handleInput
		stepGame
