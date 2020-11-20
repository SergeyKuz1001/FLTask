module Main where

import Control.Applicative (some, empty)
import Control.Monad (when, replicateM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, except)
import Control.Monad.Trans.Reader (ReaderT(..), ask)
import Control.Monad.Trans.State.Lazy (StateT, execStateT, gets, modify)
import Data.Functor ((<&>))
import Data.List (isInfixOf)
import Data.List.Utils (replace)
import Data.Map.Lazy (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Lazy as Map (findWithDefault, fromListWith)
import Text.Read (readEither)

type Production = (String, String)

type ProdGroup = Int

type Grammar = Map ProdGroup [Production]

type GrammarEnv = ReaderT Grammar (StateT [String] Maybe) ()

(.>) :: (a -> b) -> (b -> c) -> a -> c
f .> g = g . f

withLeft :: Either e a -> e' -> Either e' a
withLeft (Left _) e  = Left e
withLeft (Right a) _ = Right a

readIntEither :: String -> Either String Int
readIntEither str = readEither str `withLeft`
    ("Incorrect number \"" ++ str ++ "\"")

check :: Bool -> String -> Either String ()
check True  _ = Right ()
check False e = Left e

parse :: String -> Either String (Int, Int, Int)
parse expr = do
    let (num1, other) = span (/= '+') expr
    a <- readIntEither num1
    check (other /= "") "There isn't '+'"
    let (num2, num3) = span (/= '=') $ tail other
    b <- readIntEither num2
    check (num3 /= "") "There isn't '='"
    c <- readIntEither (tail num3)
    check (a + b == c) "Incorrect equation"
    return (a, b, c)

asString :: Grammar -> GrammarEnv -> String
asString gr =
    flip runReaderT gr .>
    flip execStateT ["S"] .>
    fromMaybe ["Oops, interesting situation..."] .>
    reverse .>
    map (++ "\n") .>
    concat

applyProd :: ProdGroup -> GrammarEnv
applyProd pg = do
    grammar <- ask
    let prods = Map.findWithDefault [] pg grammar
    currSent <- lift $ gets head
    let prods' = filter (\(h, _) -> h `isInfixOf` currSent) prods
    when (prods' == []) empty
    let (h, b) = head prods'
    let nextSent = replace h b currSent
    lift $ modify (nextSent : )

derivation :: Grammar -> Int -> Int -> String
derivation gr a b = asString gr $ do
    applyProd 0
    replicateM_ a $ do
        applyProd 2
        some $ applyProd 4
        some $ applyProd 5
        some $ applyProd 4
    replicateM_ b $ do
        applyProd 3
        some $ applyProd 5
        some $ applyProd 4
    some $ applyProd 1
    return ()

printExcept :: ExceptT String IO String -> IO ()
printExcept = runExceptT .> (>>= either putStr putStr)

getGrammar :: FilePath -> IO Grammar
getGrammar filePath =
    readFile filePath <&> ((
        lines .>
        map (\str ->
            let (pgNum, other) = span (/= ')') str
                pg = read pgNum
                other' = dropWhile (== ' ') $ tail other
                (h', b') = span (/= '-') other'
                h = map head $ words $ init h'
                b = map head $ words $ drop 3 b'
                p = (h, b)
            in  (pg, [p])
          ) .>
        Map.fromListWith (++)
      ) :: (String -> Map ProdGroup [Production]))

main :: IO ()
main = printExcept $ do
    expr <- lift $ getLine
    grammar <- lift $ getGrammar "grammar_0.txt"
    (a, b, c) <- except $ parse expr
    return $ derivation grammar a b
