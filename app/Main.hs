module Main where

import Control.Applicative (some, many, empty)
import Control.Monad (join, when, replicateM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, except)
import Control.Monad.Trans.Reader (ReaderT(..), ask)
import Control.Monad.Trans.State.Lazy (StateT, execStateT, gets, modify)
import Data.Functor ((<&>))
import Data.List (isInfixOf)
import Data.List.Utils (replace)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map (findWithDefault, fromListWith)
import Data.Maybe (fromMaybe)
import Text.Parsec (Parsec, ParseError, parse, sepBy, endBy, try)
import Text.Parsec.Char (digit, char, string, newline, anyChar)

type Production = (String, String)

type ProdGroup = Int

type Grammar = Map ProdGroup [Production]

type GrammarEnv = ReaderT Grammar (StateT [String] Maybe) ()

type ExceptIO a = ExceptT ParseError IO a

printExceptIO :: ExceptIO String -> IO ()
printExceptIO = runExceptT .> (>>= either print putStr)

(.>) :: (a -> b) -> (b -> c) -> a -> c
f .> g = g . f

space :: Parsec String () Char
space = char ' '

parserExpr :: Parsec String () (Int, Int, Int)
parserExpr = do
    a <- read <$> some digit
    char '+'
    b <- read <$> some digit
    char '='
    c <- read <$> some digit
    when (a + b /= c) $
        fail "Incorrect equation"
    return (a, b, c)

parserProd :: Parsec String () (ProdGroup, [Production])
parserProd = do
    pg <- read <$> some digit
    char ')'
    many space
    h <- some $ try $ anyChar <* some space
    string "->"
    b <- many $ try $ some space *> anyChar
    many space
    return (pg, [(h, b)])

parserGrammar :: Parsec String () Grammar
parserGrammar = Map.fromListWith (++) <$> parserProd `endBy` newline

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

readGrammar :: FilePath -> ExceptIO Grammar
readGrammar filePath = do
    text <- lift $ readFile filePath
    except $ parse parserGrammar "" text

getExpr :: ExceptIO (Int, Int, Int)
getExpr = do
    expr <- lift getLine
    except $ parse parserExpr "" expr

main :: IO ()
main = printExceptIO $ do
    grammar <- readGrammar "grammar_0.txt"
    (a, b, c) <- getExpr
    return $ derivation grammar a b
