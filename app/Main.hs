module Main where

import Control.Applicative (some, many, (<|>))
import Control.Monad (guard, when, replicateM_)
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
import Text.Parsec (Parsec, ParseError, parse, endBy1, endBy, try, between, choice)
import Text.Parsec.Char (digit, char, string, newline, spaces, anyChar)

type Production = (String, String)

type ProdGroup = Int

type Grammar = Map ProdGroup [Production]

type Var = Char

type MapVars = Map Var Int

data StObj =
      ApplyProd ProdGroup
    | Repeat Var Strategy
    | RepeatSome Strategy

type Strategy = [StObj]

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
        fail $
            "Incorrect equation because " ++
            show a ++
            " + " ++
            show b ++
            " = " ++
            show (a + b)
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

parserVar :: Parsec String () Var
parserVar = char 'a' <|> char 'b'

parserProdGroup :: Parsec String () ProdGroup
parserProdGroup = read <$> some digit

parserStBlock :: Parsec String () Strategy
parserStBlock = between (char '[') (char ']') parserStrategy

parserStObj :: Parsec String () StObj
parserStObj = choice [
    ApplyProd <$> parserProdGroup,
    Repeat <$> parserVar <*> (char ':' *> parserStBlock),
    RepeatSome <$> parserStBlock
  ]

parserStrategy :: Parsec String () Strategy
parserStrategy = spaces *> parserStObj `endBy1` spaces

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
    guard (prods' /= [])
    let (h, b) = head prods'
    let nextSent = replace h b currSent
    lift $ modify (nextSent : )

derivation :: Strategy -> (Int, Int) -> GrammarEnv
derivation [] _ = return ()
derivation (ApplyProd pg : stObjs) ab = do
    applyProd pg
    derivation stObjs ab
derivation (Repeat var st : stObjs) ab@(a, b) = do
    replicateM_ amt $
        derivation st ab
    derivation stObjs ab
    where
        amt = case var of
            'a' -> a
            'b' -> b
derivation (RepeatSome st : stObjs) ab = do
    some $ derivation st ab
    derivation stObjs ab

readGrammar :: FilePath -> ExceptIO Grammar
readGrammar filePath = do
    text <- lift $ readFile filePath
    except $ parse parserGrammar "" text

readStrategy :: FilePath -> ExceptIO Strategy
readStrategy filePath = do
    text <- lift $ readFile filePath
    except $ parse parserStrategy "" text

getExpr :: ExceptIO (Int, Int, Int)
getExpr = do
    expr <- lift getLine
    except $ parse parserExpr "" expr

main :: IO ()
main = printExceptIO $ do
    grammar0 <- readGrammar "grammar_0.txt"
    strategy0 <- readStrategy "grammar_0_st.txt"
    grammar1 <- readGrammar "grammar_1.txt"
    strategy1 <- readStrategy "grammar_1_st.txt"
    (a, b, _) <- getExpr
    return $ "Type 0:\n" ++ (asString grammar0 $ derivation strategy0 (a, b)) ++ "Type 1:\n" ++ (asString grammar1 $ derivation strategy1 (a, b))
