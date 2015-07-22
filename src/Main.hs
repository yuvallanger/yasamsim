module Main where


data Point
    = Point
    { pointX :: !Int
    , pointY :: !Int
    , pointZ :: !Int
    }


data Civilian
    = Civilian
    { civilianHealth   :: !Int
    , civilianPosition :: !Point
    , civilianPower    :: !Int
    , civilianSpeed    :: !Int
    , civilianWeapon   :: !Weapon
    }


data Player
    = Player
    { playerPosition :: !Point
    , playerSpeed    :: !Int
    , playerPower    :: !Int
    , playerHealth   :: !Int
    , playerEnergy   :: !Int
    , playerWeapon   :: !Weapon
    , player         :: !CharacterState
    }


data CharacterState
    = Walk
    | Jump


data Weapon
    = Fist
    | Club


main :: IO ()
main = undefined
