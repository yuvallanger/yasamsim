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
import Graphics.Gloss.Interface.IO.Game
	( playIO
	, Event
	, Display (InWindow)
	, Picture
	, black
	)


data Point
	= Point
	{ pointX :: Int
	, pointY :: Int
	, pointZ :: Int
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


data Weapon
	= Fist
	| Club


data Item
	= Crate
	| Medipack


data World
	= World
	{ player    :: CharacterState
	, civilians :: [CharacterState]
	, items     :: [Item]
	}


drawScene :: World -> IO Picture
drawScene w = return (background)
	where
	background = mempty


handleInput :: Event -> World -> IO World
handleInput event world = do
	putStrLn "handleInput"
	return world


stepGame :: Float -> World -> IO World
stepGame time world = do
	putStrLn "stepGame"
	return world


initialWorld :: World
initialWorld
	= World
	{ player    = undefined
	, civilians = undefined
	, items     = undefined
	}


main :: IO ()
main = do
	playIO
		(InWindow "Yasam Sim" (1, 1) (500, 500))
		black
		10
		initialWorld
		drawScene
		handleInput
		stepGame
