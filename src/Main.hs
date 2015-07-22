module Main where


import Graphics.Gloss.Interface.IO.Game


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
drawScene = undefined


handleInput :: a
handleInput = undefined


stepGame :: a
stepGame = undefined


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
        azure
        10
        initialWorld
        drawScene
        handleInput
        stepGame
