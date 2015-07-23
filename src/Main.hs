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
    { pointX :: !Int
    , pointY :: !Int
    , pointZ :: !Int
    }


data Orientation
    = FacingLeft
    | FacingRight
    | FacingAway
    | FacingPlayer

data Character
    = Character
    { characterPosition    :: !Main.Point
    , characterPower       :: !Int
    , characterSpeed       :: !Int
    , characterWeapon      :: !Weapon
    , characterHealth      :: !Int
    , characterEnergy      :: !Int
    , characterState       :: !CharacterState
    , characterOrientation :: !Orientation
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
