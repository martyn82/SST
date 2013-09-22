module Week3Bas 	
where
import Data.Maybe;
import Data.List;
import System.Random;
import Control.Monad;
import Text.Parsec.Error;
import Text.ParserCombinators.Parsec;
import Text.Parsec.Expr;
import Text.Parsec.Token;
import Text.ParserCombinators.Parsec.Language;
import Control.Applicative ((<*>), (*>), (<$>))
import Data.Char;
import Week3;

randomIntegerStream :: [IO Int]
randomIntegerStream = (randomRIO (1,10) : randomIntegerStream)

genIntList :: IO [Int]
genIntList = sequence randomIntegerStream 

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] []    = True
isPermutation (x:xs) ys | elem x ys  = isPermutation xs (delete x ys)
                        | otherwise   = False

                      
--after various experiments, using Parsec to create the
--parser. The expression features cover the neg, impl and equi
--formulas, solving associativity, precedence and fixity.
--we combine the parser created by parsec with our own conj and disj
--parser, and combine that newly created parser with the quantifier parser

--use the token parser builder to define operators and identifiers.
--this allows handling whitespace and comments more easily.
def = emptyDef{ commentStart = "/*"
              , commentEnd = "*/"
              , identStart = oneOf (['a'..'z'] ++ ['B'..'D'] ++ ['F'..'Z'])
              , identLetter = oneOf (['a'..'z'] ++ ['B'..'D'] ++ ['F'..'Z'])
              , opStart = oneOf "~*+=EA"
              , opLetter = oneOf "~*+=EA"
              , reservedOpNames = ["<=>", "==>"]
              }                        

--the token parser created              
TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , commaSep1 = m_commaSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser def              

--table to handle prefix and infix expressions, this tells parsec
--how to define the operators.
table = [ [prefix "~" Neg],
          [namedPrefix "A" Forall],
          [namedPrefix "E" Exists],
          [Infix  (m_reservedOp "==>" >> return Impl) AssocLeft],
          [Infix  (m_reservedOp "<=>" >> return Equi) AssocLeft]
           ]

prefix          opName fun = Prefix (do{ m_reservedOp opName; return fun })
namedPrefix     opName fun = Prefix (do{ m_reservedOp opName; name <- m_identifier; return (fun name) })

commaSymbol = spaces >> char ',' >> spaces

compositePrefix opName fun = do{ m_reservedOp opName; forms <- m_parens (sepBy expr commaSymbol); return (fun forms);}

--identifier in term: x  
var = do    
        name <- m_identifier
        return (V name)
        
--function as formula: f(x,y)
atomFunc = do 
        name <- m_identifier
        terms <- m_parens (sepBy term commaSymbol)
        return (Atom name terms) <?> "atom function"

--function in term: f(x,y)
termFunc = do 
        name <- m_identifier
        terms <- m_parens (sepBy term  commaSymbol)
        return (F name terms) <?> "function"

--two terms: x = y as a formula
termsEq = do
        l <- term
        m_reservedOp "="
        r <- term
        return (Eq l r) <?> "term eq"

term = try termFunc <|> var
        
atomic = m_parens expr 
     <|> try termsEq
     <|> atomFunc
     <|> expr --this recursive descent step is needed because we add the composite expressions later;
              --the parsers generated from the 'buildExpressionParser' don't allow for conjunction
              --and disjunction.
              
expr = compositePrefix "*" Conj
   <|> compositePrefix "+" Disj 
   <|> buildExpressionParser table atomic
   
parseExpr input = parse expr "" input