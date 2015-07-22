module Main where


data Point
    = Point
    { pointX :: !Int
    , pointY :: !Int
    , pointZ :: !Int
    }


data Character
    = Character
    { characterPosition :: !Point
    , characterPower    :: !Int
    , characterSpeed    :: !Int
    , characterWeapon   :: !Weapon
    , characterHealth   :: !Int
    , characterEnergy   :: !Int
    , characterState    :: !CharacterState
    }


data CharacterState
    = Walk
    | Jump


data Weapon
    = Fist
    | Club


main :: IO ()
main = undefined
