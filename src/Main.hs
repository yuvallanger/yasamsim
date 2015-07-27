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
	oldPlayer = player game
	playerPos = characterPosition oldPlayer
	z         = characterZ        oldPlayer
	(x, y)    = playerPos


handleInput :: Event -> Game -> IO Game
handleInput event game =
	case event of
		EventKey (Char 'w') keyState _ _ -> do
			traceIO "EventKey: "
			traceIO $ "\t" ++ show event
			traceIO $ "\t" ++ (show . characterDirection $ player game)
			traceIO $ "\t" ++ (show $ player1KeyboardState game)
			let newGame = handleDirectionKey
				game
				DirectionDown
				DirectionUp
				ButtonDown
				ButtonUp
				keyState
			traceIO $ "\t" ++ (show . characterDirection $ player newGame)
			traceIO $ "\t" ++ (show $ player1KeyboardState newGame)
			return newGame
		EventKey (Char 's') keyState _ _ -> do
			traceIO "EventKey: "
			traceIO $ "\t" ++ show event
			traceIO $ "\t" ++ (show . characterDirection $ player game)
			traceIO $ "\t" ++ (show $ player1KeyboardState game)
			let newGame = handleDirectionKey
				game
				DirectionUp
				DirectionDown
				ButtonUp
				ButtonDown
				keyState
			traceIO $ "\t" ++ (show . characterDirection $ player newGame)
			traceIO $ "\t" ++ (show $ player1KeyboardState newGame)
			return newGame
		EventKey (Char 'a') keyState _ _ -> do
			traceIO "EventKey: "
			traceIO $ "\t" ++ show event
			traceIO $ "\t" ++ (show . characterDirection $ player game)
			traceIO $ "\t" ++ (show $ player1KeyboardState game)
			let newGame = handleDirectionKey
				game
				DirectionRight
				DirectionLeft
				ButtonRight
				ButtonLeft
				keyState
			traceIO $ "\t" ++ (show . characterDirection $ player newGame)
			traceIO $ "\t" ++ (show $ player1KeyboardState newGame)
			return newGame
		EventKey (Char 'd') keyState _ _ -> do
			traceIO "EventKey: "
			traceIO $ "\t" ++ show event
			traceIO $ "\t" ++ (show . characterDirection $ player game)
			traceIO $ "\t" ++ (show $ player1KeyboardState game)
			let newGame = handleDirectionKey
				game
				DirectionLeft
				DirectionRight
				ButtonLeft
				ButtonRight
				keyState
			traceIO $ "\t" ++ (show . characterDirection $ player newGame)
			traceIO $ "\t" ++ (show $ player1KeyboardState newGame)
			return newGame
		_ -> return game

handleDirectionKey
	:: Game
	-> Direction
	-> Direction
	-> KeyboardButton
	-> KeyboardButton
	-> KeyState
	-> Game
handleDirectionKey
	game
	directionFrom
	directionTo
	buttonFrom
	buttonTo
	keyState =
	case keyState of
		Down -> game
			{ player =
				oldPlayer1
				{ characterDirection =
					insert directionTo (
					    delete
							directionFrom
							oldPlayer1Direction
					)
				}
			, player1KeyboardState =
				insert buttonTo oldPlayer1KeyboardState
			}
		Up -> game
			{ player =
				oldPlayer1
				{ characterDirection =
					delete
						directionTo
						oldPlayer1Direction
				}
			, player1KeyboardState =
				delete
					buttonTo
					oldPlayer1KeyboardState
			}
	where
	oldPlayer1 = player game
	oldPlayer1Direction = characterDirection oldPlayer1
	oldPlayer1KeyboardState = player1KeyboardState game


stepGame :: Float -> Game -> IO Game
stepGame time game = do
	return newGame
	where
	oldPlayer = player game
	newPlayer = moveCharacter time oldPlayer
	newGame  = game { player = newPlayer }

moveCharacter
	:: Float
	-> Character
	-> Character
moveCharacter time character
	= character { characterPosition = newPosition }
	where
	(oldX, oldY) = characterPosition character
	oldCharacterDirection = characterDirection character
	oldCharacterSpeed = characterSpeed character
	speedX
		| member DirectionLeft oldCharacterDirection  = -oldCharacterSpeed
		| member DirectionRight oldCharacterDirection =  oldCharacterSpeed
		| otherwise = 0
	speedY
		| member DirectionUp oldCharacterDirection   =  oldCharacterSpeed
		| member DirectionDown oldCharacterDirection = -oldCharacterSpeed
		| otherwise = 0
	newPosition = (oldX + (time * speedX), oldY + (time * speedY))


initialPlayer :: Character
initialPlayer
	= Character
	{ characterPosition    = (-200, 0)
	, characterZ           = 0
	, characterPower       = 30
	, characterSpeed       = 30
	, characterWeapon      = Club
	, characterHealth      = 100
	, characterEnergy      = 100
	, characterState       = Stand
	, characterOrientation = DirectionRight
	, characterDirection   = empty
	}


initialGame :: Game
initialGame
	= Game
	{ player    = initialPlayer
	, civilians = []
	, items     = []
	, player1KeyboardState  = empty
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
