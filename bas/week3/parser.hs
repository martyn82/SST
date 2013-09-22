module Parser 	
where
import Data.Either;
import Data.Functor.Identity;
import Text.ParserCombinators.Parsec;
import Text.Parsec.Expr;
import Text.Parsec.Token;
import Text.Parsec.Prim hiding (try)
import Text.ParserCombinators.Parsec.Language;
import Control.Applicative ((<*>), (*>), (<$>))
import Week3;
 
rightVal x = head $ rights [x]

formula3 = rightVal $ parseFormula "AxAy((R(x,y))==>(R(y,x)))"
formula4 = rightVal $ parseFormula "AxAyEz((R(x,y)==>((R(y,x))<=>(R(x,z)))))"
formula5 = rightVal $ parseFormula "AxAy (R(x,y))"
formula6 = rightVal $ parseFormula "*(R(x,y), x=z, +(y=z,x=y))"

--after various experiments, using Parsec to create the
--parser. The expression features cover the neg, impl and equi
--formulas, solving associativity, precedence and fixity.
--also shows nice errors!
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
           
prefix :: String -> (a -> a) -> Operator [Char] u Identity a
prefix          opName fun = Prefix (do{ m_reservedOp opName; return fun })

namedPrefix :: String -> (String -> a -> a) -> Operator [Char] u Identity a
namedPrefix opName fun = Prefix (do{ m_reservedOp opName; name <- m_identifier; return (fun name) })

commaSymbol :: ParsecT [Char] u Identity ()
commaSymbol = spaces >> char ',' >> spaces

compositePrefix :: [Char] -> ([Formula] -> Formula) -> ParsecT [Char] u Identity Formula
compositePrefix opName fun = do{ m_reservedOp opName; forms <- m_parens (sepBy expr commaSymbol); return (fun forms);}

--identifier in term: x  
var :: ParsecT [Char] t Identity Term
var = do    
        name <- m_identifier
        return (V name)
        
--function as formula: f(x,y)
atomFunc :: ParsecT [Char] t Identity Formula
atomFunc = do 
        name <- m_identifier
        terms <- m_parens (sepBy term commaSymbol)
        return (Atom name terms) <?> "atom function"

--function in term: f(x,y)
termFunc :: ParsecT [Char] t Identity Term
termFunc = do 
        name <- m_identifier
        terms <- m_parens (sepBy term  commaSymbol)
        return (F name terms) <?> "function"

--two terms: x = y as a formula
termsEq :: ParsecT [Char] t Identity Formula
termsEq = do
        l <- term
        m_reservedOp "="
        r <- term
        return (Eq l r) <?> "term eq"
        
term :: ParsecT [Char] t Identity Term
term = try termFunc <|> var
        
atomic :: ParsecT [Char] u Identity Formula
atomic = m_parens expr 
     <|> try termsEq
     <|> atomFunc
     <|> expr --this recursive descent step is needed because we add the composite expressions later;
              --the parsers generated from the 'buildExpressionParser' don't allow for conjunction
              --and disjunction.

expr :: ParsecT [Char] u Identity Formula
expr = compositePrefix "*" Conj
   <|> compositePrefix "+" Disj 
   <|> buildExpressionParser table atomic
   
parseFormula :: [Char] -> Either ParseError Formula
parseFormula input = parse expr "" input

					  