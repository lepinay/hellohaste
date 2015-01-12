module Main where
import Haste
import Haste.JSON
import Haste.Prim
import Control.Applicative
import MySerialize


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
        (Parser (Right p)) =
            Person
            <$> j .: (toJSStr "name")
            <*> j .:? (toJSStr "firstname")
            <*> j .:? (toJSStr "age")
            <*> j .:? (toJSStr "home")
            <*> j .:? (toJSStr "sex")
            <*> j .: (toJSStr "pets")
    in putStrLn $ show p


