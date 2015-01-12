module Main where
import Haste
import Haste.JSON
import Haste.Prim
import Control.Applicative
import MyModule


data Address = Address
    {
        street::Maybe String,
        city::Maybe String,
        country::Maybe String
    }
    deriving (Show)

data Person = Person
    {
        name::String,
        firstname::String,
        age::Maybe Int,
        home::Maybe Address
    }
    deriving (Show)


--child::JSON -> String -> (a->b) ->
--child field ctor =
--    case j .:? (toJSStr field) of
--                    Parser(Right (Just json )) ->
--                        Just
--                        <$>



--do
--    home <- child j "home"
--    Address
--    <$> home .:? (toJSStr "street")
--    <*> home .:? (toJSStr "city")
--    <*> home .:? (toJSStr "country")

main =
    let ex1 = "{'name':'bob','firstname':'plop','home':{'city':'paris'}}"
        ex2 = "{'name':'bob','firstname':'plop'}"
        (Right j) = decodeJSON $ toJSStr ex1
        (Parser (Right p)) =
            Person
            <$> j .: (toJSStr "name")
            <*> j .: (toJSStr "firstname")
            <*> j .:? (toJSStr "age")
            <*> do
                    phome <- j .:? (toJSStr "home")
                    case phome of
                        Just home ->
                            Just
                            <$> (Address
                            <$> home .:? (toJSStr "street")
                            <*> home .:? (toJSStr "city")
                            <*> home .:? (toJSStr "country"))
                        _ -> Parser $ Right Nothing
    in putStrLn $ show p


