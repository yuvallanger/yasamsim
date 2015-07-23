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


module Main where


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
import Graphics.Gloss.Interface.IO.Game
	( playIO
	, Event
	, Display (InWindow)
	, Picture
	, black
	)


sceneWidth, sceneHeight :: Int
sceneWidth  = 640
sceneHeight = 480


data Point
	= Point
	{ pointX :: Float
	, pointY :: Float
	, pointZ :: Float
	}


data Orientation
	= FacingLeft
	| FacingRight
	| FacingAway
	| FacingPlayer

data Character
	= Character
	{ characterPosition    :: Main.Point
	, characterPower       :: Int
	, characterSpeed       :: Int
	, characterWeapon      :: Weapon
	, characterHealth      :: Int
	, characterEnergy      :: Int
	, characterState       :: CharacterState
	, characterOrientation :: Orientation
	}


data CharacterState
	= Walk
	| Jump
	| Stand


data Weapon
	= Fist
	| Club


data Item
	= Crate
	| Medipack


data World
	= World
	{ player    :: Character
	, civilians :: [Character]
	, items     :: [Item]
	}


drawScene :: World -> IO Picture
drawScene world = return (background <> player)
	where
	background = drawBackground world
	player = drawPlayer world

drawBackground :: World -> Picture
drawBackground world = color white . polygon $ rectanglePath (fromIntegral sceneWidth) (fromIntegral sceneHeight)

drawPlayer :: World -> Picture
drawPlayer world = translate x y . color violet $ rectangleWire 25 35
	where
	playerPos = characterPosition . player $ world
	x = pointX playerPos
	y = pointY playerPos + pointZ playerPos


handleInput :: Event -> World -> IO World
handleInput event world = do
	putStrLn "handleInput"
	return world


stepGame :: Float -> World -> IO World
stepGame time world = do
	putStrLn "stepGame"
	return world


initialPlayer :: Character
initialPlayer
	= Character
	{ characterPosition
		= Main.Point
		{ pointX = -200
		, pointY = 0
		, pointZ = 0
		}
	, characterPower       = 30
	, characterSpeed       = 30
	, characterWeapon      = Club
	, characterHealth      = 100
	, characterEnergy      = 100
	, characterState       = Stand
	, characterOrientation = FacingRight
	}


initialWorld :: World
initialWorld
	= World
	{ player    = initialPlayer
	, civilians = undefined
	, items     = undefined
	}


main :: IO ()
main = do
	playIO
		(InWindow "Yasam Sim" (1, 1) (sceneWidth, sceneHeight))
		black
		30
		initialWorld
		drawScene
		handleInput
		stepGame
