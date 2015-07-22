module Main where


data Point
    = Point
    { pointX :: !Int
    , pointY :: !Int
    }


data Civilian
    = Civilian
    { civilianHealth :: !Int
    , civilianPower  :: !Int
    , civilianSpeed  :: !Int
    , civilianWeapon :: !Weapon
    }


data Player
    = Player
    { playerCoord  :: !Point
    , playerSpeed  :: !Int
    , playerPower  :: !Int
    , playerHealth :: !Int
    , playerEnergy :: !Int
    , playerWeapon :: !Weapon
    }


data Weapon
    = Fist
    | Club


main :: IO ()
main = undefined
