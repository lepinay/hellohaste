module Main where
import Haste
import Haste.Serialize
import Haste.JSON
import Haste.Prim
import Control.Applicative



data Sex =
    Male
    | Female
    deriving (Show)

instance Serialize Sex where
  toJSON = fail "Not implemented"
  parseJSON (Str json) =
    case fromJSStr json of
        "male" -> return Male
        "female" -> return Female

data Address = Address
    {
        street::Maybe String,
        city::Maybe String,
        country::Maybe String
    }
    deriving (Show)

instance Serialize Address where
  toJSON = fail "Not implemented"
  parseJSON json =
    Address
    <$> json .:? (toJSStr "street")
    <*> json .:? (toJSStr "city")
    <*> json .:? (toJSStr "country")

data Pet = Pet
    {
        petname::String
    }
    deriving (Show)

instance Serialize Pet where
  toJSON = fail "Not implemented"
  parseJSON json = Pet <$> json .: (toJSStr "petname")

instance Serialize Person where
  toJSON = fail "Not implemented"
  parseJSON j =
    Person
    <$> j .: (toJSStr "name")
    <*> j .:? (toJSStr "firstname")
    <*> j .:? (toJSStr "age")
    <*> j .:? (toJSStr "home")
    <*> j .:? (toJSStr "sex")
    <*> j .: (toJSStr "pets")

data Person = Person
    {
        name::String,
        firstname::Maybe String,
        age::Maybe Int,
        home::Maybe Address,
        sex::Maybe Sex,
        pets::[Pet]
    }
    deriving (Show)

main =
    let ex1 = "{'name':'bob','firstname':'plop','home':{'city':'paris'},'sex':'male','pets':[{'petname':'bingles'}]}"
        ex2 = "{'name':'bob','firstname':'plop','pets':[]}"
        (Right j) = decodeJSON $ toJSStr ex2
        (Right p) = (fromJSON j)::Either String Person
    in putStrLn $ show p


