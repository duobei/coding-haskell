-- https://www.codewars.com/kata/54f1fdb7f29358dd1f00015d

module ApplicativeParser where

import Data.Char
import Prelude hiding (fmap)

-- | An ambiguous parser.
-- | But I think this parser definition is subject to severe performance issues
newtype Parser a = P { unP :: String -> [(String, a)] }

-- | Change the result of a parser.
pmap :: (a -> b) -> Parser a -> Parser b
pmap f p = P $ \s -> map (\(s, a) -> (s, f a)) $ unP p s 

-- | Operator version of 'pmap'.
(<#>) :: (a -> b) -> Parser a -> Parser b
(<#>) = pmap

-- | Parse a value and replace it.
(<#) :: a -> Parser b -> Parser a
(<#) a = pmap (const a)

infixl 4 <#>
infixl 4 <#

-- | Parse a character only when a predicate matches.
predP :: (Char -> Bool) -> Parser Char
predP p = P f
        where f (c:cs) = if p c then [(cs, c)] else [] 
              f _ = []

-- | Succeed only when parsing the given character.
charP :: Char -> Parser Char
charP c = predP (== c)

-- | Inject a value into an identity parser.
inject :: a -> Parser a
inject x = P $ \s -> [(s, x)]

-- | Given a parser with a function value and another parser, parse the function
-- first and then the value, return a parser which applies the function to the
-- value.
(<@>) :: Parser (a -> b) -> Parser a -> Parser b
pf <@> px = P $ \s -> do (rest, f) <- unP pf s
                         (rest', a) <- unP px rest
                         return (rest', f a)

(<@) :: Parser a -> Parser b -> Parser a
pa <@ pb = const <#> pa <@> pb

(@>) :: Parser a -> Parser b -> Parser b
pa @> pb = flip const <#> pa <@> pb

infixl 4 <@
infixl 4 @>
infixl 4 <@>

-- | Parse a whole string.
stringP :: String -> Parser String
stringP (c:cs) = (:) <#> charP c <@> stringP cs
stringP [] = inject []

-- | Construct a parser that never parses anything.
emptyP :: Parser a
emptyP = P $ \s -> []

-- | Combine two parsers: When given an input, provide the results of both parser run on the input.
(<<>>) :: Parser a -> Parser a -> Parser a
p <<>> q = P $ \s -> unP p s ++ unP q s
                         

infixl 3 <<>>

-- | Apply the parser zero or more times.
many :: Parser a -> Parser [a]
many p = (P $ \s -> [(s, [])]) <<>> some p

-- | Apply the parser one or more times.
some :: Parser a -> Parser [a]
some p = (:) <#> p <@> many p
--some p = pmap (:[]) p <<>> (:) <#> p <@> some p


-- | Apply a parser and return all ambiguous results, but only those where the input was fully consumed.
runParser :: Parser a -> String -> [a]
runParser p cs = map snd . filter ((== "") . fst) $ unP p cs

-- | Apply a parser and only return a result, if there was only one unambiguous result with output fully consumed.
runParserUnique :: Parser a -> String -> Maybe a
runParserUnique p cs = case runParser p cs of
                       [a] -> Just a
                       _ -> Nothing

-- | Kinds of binary operators.
data BinOp = AddBO | MulBO deriving (Eq, Show)

-- | Some kind of arithmetic expression.
data Expr = ConstE Int
          | BinOpE BinOp Expr Expr
          | NegE Expr
          | ZeroE
          deriving (Eq, Show)

evalExpr :: Expr -> Int
evalExpr e = case e of
               ZeroE -> 0
               (ConstE x) -> x
               (NegE e)   -> negate . evalExpr $ e
               (BinOpE op e1 e2) -> ev op (evalExpr e1) (evalExpr e2)
                  where
                    ev AddBO = (+)
                    ev MulBO = (*)

-- | Parse arithmetic expressions, with the following grammar:
--
--     expr         ::= const | binOpExpr | neg | zero
--     const        ::= int
--     binOpExpr    ::= '(' expr ' ' binOp ' ' expr ')'
--     binOp        ::= '+' | '*'
--     neg          ::= '-' expr
--     zero         ::= 'z'
-- 

parseExpr :: String -> Maybe Expr
parseExpr = runParserUnique expr
  where
    expr = const' <<>> bindOpExpr <<>> neg' <<>> zero
    const' = (ConstE . read) <#> some (predP (isDigit))
    neg' = charP '-' @> (NegE <#> expr)
    zero = ZeroE <# charP 'z'
    bindOp = (AddBO <# charP '+') <<>> (MulBO <# charP '*')
    f e1 o e2 = BinOpE o  e1 e2
    bindOpExpr = f <#> (charP '(' @> expr <@ charP ' ') <@> bindOp <@> (charP ' ' @> expr <@ charP ')')
