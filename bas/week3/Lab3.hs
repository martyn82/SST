module Week3Bas 	
where
import Data.Maybe;
import Data.Either;
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

genIntList :: Int -> IO [Int]
genIntList length = sequence $ take length randomIntegerStream 

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] []    = True
isPermutation [] _     = False
isPermutation _ []     = False
isPermutation (x:xs) ys | elem x ys  = isPermutation xs (delete x ys)
                        | otherwise   = False
 
 
permutationTest1 x y = length x == length y
permutationTest2 x y = sort x == sort y
permutationTest3 x y = intersect x y == x 
 
rightVal x = head $ rights [x]
 
formula3 = rightVal $ parseFormula "AxAy((R(x,y))==>(R(y,x)))"

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
   
parseFormula input = parse expr "" input
        
-- data Formula = Atom Name [Term]
               -- | Eq Term Term
               -- | Neg  Formula 
               -- | Impl Formula Formula
               -- | Equi Formula Formula
               -- | Conj [Formula]
               -- | Disj [Formula] 
               -- | Forall Name Formula
               -- | Exists Name Formula
               -- deriving (Eq,Ord)
        
        
parseTerm :: String -> Either ParseError Term
parseTerm input = parse term "Unknown" input
    

value = (eof >> return []) <|>
        (try (string "\n ") >> ((++) <$> return "\n " <*> value)) <|>
        (many (noneOf "\n"))
                    
kvp = do
        k <- many (noneOf ":")
        char ':'
        v <- value
        eof <|> (char '\n' >> return ())
        return (k,v)

kvps =   do
         first <- kvp
         result <- (eof >> return []) <|> kvps
         return (first:result)  

parseTst :: String -> Either ParseError [(String, String)]
parseTst input = parse kvps "Unknown" input
						
-- def = emptyDef{ identStart = letter
              -- , opStart = oneOf "-"
              -- , reservedOpNames = ["==>", "==", "<=>", ":="]
			  -- , reservedNames   = ["conj", "disj"]
              -- }

-- TokenParser{ parens = m_parens
           -- , identifier = m_identifier
           -- , reservedOp = m_reservedOp
           -- , reserved = m_reserved
           -- , semiSep1 = m_semiSep1
           -- , whiteSpace = m_whiteSpace } = makeTokenParser def

-- table = [ [Prefix (m_reservedOp "-"  >> return Neg)]
        -- , [Prefix (m_reserved "conj" >> return (Conj)) AssocLeft]
        -- , [Prefix (m_reserved "disj" >> return (Conj)) AssocLeft]
        -- ]

-- expression = buildExpressionParser table term <?> "expression"
-- term       = parens expression
				
					   
					   
					   

-- -- newtype P s t = P ([s] -> [(t, [s])])
-- -- (<|>) :: P s b -> P s b -> P s b
-- -- (<|>) (P p1) (P p2) = P (\s -> p1 s ++ p2 s)
-- -- (<*>) :: P s (b -> a) -> P s b -> P s a
-- -- (<*>) (P p1) (P p2) = P (\s -> [(v1 v2, rest2) | (v1, rest1) <- p1 s, (v2, rest2) <- p2 rest1])
-- -- (<$>) :: (b -> a) -> (P s b) -> (P s a)
-- -- (<$>) f p2	 = successParser f <*> p2

-- -- infix 7 <$>
-- -- infixl 5 <*>
-- -- infixr 3 <|>
-- -- infixl 3 `opt`
-- -- infixl 5 (<*);(*>)
-- -- infixl 7 (<$)
-- -- f <$ p = const 	<$> pReturn f 	<*> p
-- -- p <* q = const 	<$> p 			<*> q
-- -- p *> q = id 	<$  p 			<*> q

-- -- pParens :: Parser s a ! Parser s a
-- -- pParens p = id <$ pSym '(' <$> p <$ pSym ')'
-- -- opt :: Parser s a -> a -> Parser s a

-- -- p `opt` v = p <|> successParser v
-- -- parens = (max:(1+)) <$> pParens parens <*> parens `opt` 0

-- -- unP (P p) = p
-- -- parseSymbol s []        = []
-- -- parseSymbol s [c]		| s == c    = [(s, [])]
						-- -- | otherwise = []
-- -- parseSymbol s (c:cs)	| s == c    = [(s, cs)]
						-- -- | otherwise = []
	
-- -- symbolParser :: (Eq s) => s -> P s s
-- -- symbolParser s = P (parseSymbol s)

-- -- successParser :: a -> P s a
-- -- successParser a = P (\s -> [(a, s)])	


-- -- parentheses :: P Char Int
-- -- parentheses = ((\_ b _ d -> max (1 + b) d) <$> symbolParser '(' <*> parentheses <*> symbolParser ')'<*> parentheses) <|> successParser 0





					  