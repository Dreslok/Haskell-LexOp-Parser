module Parser where

import Data.Char

data Lexeme = LName String | LNumber Double | LOp String | LPunct String | LKey String | LString String 
    deriving Show
     
ops = "+-*/<>|="

punct = "(),;{}"

opKeys = ["="]

wordKeys = ["if", "then", "else", "def", "while"]


statements :: Parser Lexeme [Statement]
statements = do s <- parseStatement 
                return [s]
         ||| do parsePunct "{"
                r <- sep parseStatement $ parsePunct ";"
                parsePunct "}"
                return r

stringConst :: String -> [Lexeme]
stringConst s = case rest of 
                    ('"':rest2) -> LString body: lexString rest2
                    _ -> error "Quote string error"
    where (body, rest) = span (/= '"') s 
    

lexString :: String -> [Lexeme]

lexString "" = []  -- No more left
lexString (' ':cs) = lexString cs -- Ignore spaces
lexString('#':cs) = lexString $ dropWhile (/= '\n') cs -- Comment
lexString inp@(c:cs)
   | c == '"' = stringConst cs 
     
   | isDigit c = let (digits, rest) = span isDigit inp in
                    case rest of
                        ('.':cs') -> let(digits2, rest2) = span isDigit cs' in
                                         LNumber (read $ (digits ++ ['.'] ++ digits2)) : lexString rest2
                        _ ->  LNumber (read digits) : lexString rest
   | isAlpha c = let (name, rest) = span isAlpha inp in
                    if name `elem` wordKeys then LKey name : lexString rest
                    else
                        LName name : lexString rest
   | c `elem` ops = let (op, rest) = span (`elem` ops) inp in 
                         (if op `elem` opKeys then LKey else
                          LOp) op : lexString rest
   | c `elem` punct = LPunct [c] : lexString cs
   | otherwise = error $ "Lex error at " ++ inp

data Exp =
   Var String |
   Num Double | -- Optional decimal type
   Add Exp Exp |
   Sub Exp Exp |
   Mul Exp Exp |
   Div Exp Exp |
   Minus Exp |
   Call String [Exp] |
   LessThan Exp Exp |
   GreaterThan Exp Exp |
   LessEqual Exp Exp |
   GreaterEqual Exp Exp |
   EqualEqual Exp Exp |
   NotEqual Exp Exp | 
   And Exp Exp |
   Or Exp Exp 

data Statement =
       IfStatement Exp [Statement] [Statement] |
       WhileStatement Exp [Statement] |
       AssignStatement String Exp |
       DefStatement String [String] [Statement] |
       ExpStatement Exp 
       deriving Show 

instance Show Exp where
  showsPrec d e = case e of 
                   Var s -> showString s
                   Num i -> showsPrec d i
                   Add e1 e2 -> showParen (d > 6)  $
                                  showsPrec 7 e1 . showString "+" . showsPrec 7 e2
                   Sub e1 e2 -> showParen (d > 6)  $
                                  showsPrec 7 e1 . showString "-" . showsPrec 7 e2
                   Mul e1 e2 -> showParen (d > 7)  $
                                  showsPrec 8 e1 . showString "*" . showsPrec 8 e2
                   Div e1 e2 -> showParen (d > 7)  $
                                  showsPrec 8 e1 . showString "/" . showsPrec 8 e2
                   Or e1 e2 -> showParen (d > 2)  $
                                  showsPrec 3 e1 . showString "||" . showsPrec 3 e2
                   And e1 e2 -> showParen (d > 3)  $
                                  showsPrec 4 e1 . showString "&&" . showsPrec 4 e2
                   EqualEqual e1 e2 -> showParen (d > 4)  $
                                  showsPrec 5 e1 . showString "==" . showsPrec 5 e2
                   GreaterThan e1 e2 -> showParen (d > 4)  $
                                  showsPrec 5 e1 . showString ">" . showsPrec 5 e2
                   GreaterEqual e1 e2 -> showParen (d > 4)  $
                                  showsPrec 5 e1 . showString ">=" . showsPrec 5 e2
                   LessThan e1 e2 -> showParen (d > 4)  $
                                  showsPrec 5 e1 . showString "<" . showsPrec 5 e2
                   LessEqual e1 e2 -> showParen (d > 4)  $
                                  showsPrec 5 e1 . showString "<=" . showsPrec 5 e2
                   Minus e   -> showParen (d > 6) $
                                showString "-" . showsPrec 7 e  
                   Call fn args -> showString fn . showString "(" . showArgs args
showArgs [] = showString ")"
showArgs [a] = shows a . showString ")"
showArgs (a:args) = shows a . showString "," . showArgs args                   
   
data Parser inp a = Parser ([inp] -> [(a, [inp])])

instance Functor (Parser inp) where
    fmap f m = do x <- m
                  return (f x)
        
ap :: Monad m => m (a -> b) -> m a -> m b 
ap mf ma = do f <- mf
              a <- ma
              return (f a)
              
instance Applicative (Parser inp) where
    pure = return
    (<*>) = ap

instance Monad (Parser inp) where
    return x = Parser (\inp -> [(x, inp)])
    act >>= k = Parser $ \toks -> 
                           [(r', toks'')  | (r, toks') <- runParser act toks,
                                            (r', toks'') <- runParser (k r) toks']

runParser :: Parser inp a -> [inp] -> [(a, [inp])]
runParser (Parser p) inp = p inp

Parser p1 ||| Parser p2 = Parser (\toks -> p1 toks ++ p2 toks)

tok :: (inp -> Maybe a) -> Parser inp a
tok f = Parser (\ts -> if null ts then [] else
                          case f (head ts) of
                            Nothing -> []
                            Just v -> [(v, tail ts)])
                                      
-- many, many1 :: Parser inp a -> Parser inp [a]

-- skipMany, skipMany1 :: Parser inp a -> Parser inp ()

-- optional :: Parser inp a -> Parser inp ()


{-
Grammar:
exp = mulexp | mulexp addop exp
mulexp = negexp | negexp mulop mulexp
negexp = '-' negexp | aexp
aexp = '(' exp ')' | num | name
addop = '+' | '-'
mulop = '*' | '/'

-}
parseExp :: Parser Lexeme Exp
parseExp = parseAndExp ||| do e1 <- parseAndExp
                              parseOp "||" ()
                              e2 <- parseExp
                              return $ Or e1 e2                              
 

parseMulExp :: Parser Lexeme Exp
parseMulExp = parseNegExp ||| do e1 <- parseNegExp 
                                 op <- parseMulOp
                                 e2 <- parseMulExp
                                 return $ op e1 e2 

parseNegExp :: Parser Lexeme Exp
parseNegExp = parseAExp ||| do parseOp "-" ()
                               e <- parseNegExp
                               return $ Minus e
parseAddExp :: Parser Lexeme Exp
parseAddExp = parseMulExp ||| do e1 <- parseMulExp
                                 op <- parseAddOp
                                 e2 <- parseAddExp
                                 return $ op e1 e2

 

parseRelExp :: Parser Lexeme Exp
parseRelExp = parseAddExp ||| do e1 <- parseAddExp
                                 op <- parseRelateOp
                                 e2 <- parseRelExp
                                 return $ op e1 e2


parseAndExp :: Parser Lexeme Exp
parseAndExp = parseRelExp ||| do e1 <- parseRelExp
                                 parseOp "&&" ()
                                 e2 <- parseAndExp
                                 return $ And e1 e2 

                                 
parseStatement = parseIf ||| parseWhile ||| parseAssign ||| parseDef ||| parseExpStatement

parseIf :: Parser Lexeme Statement
parseIf = do parseKeyword "if"
             r <- parseExp
             parseKeyword "then"
             th <- statements
             ttr <- try $ parseKeyword "else"
             case ttr of 
                        Nothing -> return $ IfStatement r th []
                        _ -> do th1 <- statements 
                                return $ IfStatement r th th1
                                
parseWhile :: Parser Lexeme Statement 
parseWhile = do parseKeyword "while"
                r <- parseExp
                parseKeyword "do"
                th <- statements
                return $ WhileStatement r th
                
parseDef :: Parser Lexeme Statement
parseDef = do parseKeyword "def"
              Var v <- parseVar 
              parsePunct "("
              th <- sep parseVar $ parsePunct ","
              parsePunct ")"
              th2 <- statements 
              return $ DefStatement v (map(\(Var v) -> v) th) th2 
              
parseExpStatement :: Parser Lexeme Statement
parseExpStatement = do e1 <- parseExp
                       return $ ExpStatement e1
                       
parseAssign :: Parser Lexeme Statement
parseAssign  = do (Var v) <- parseVar 
                  parseKeyword "="
                  th <- parseExp
                  return $ AssignStatement v th

try :: Parser inp a -> Parser inp (Maybe a)
try (Parser p) = Parser (\i -> let r = p i in
                           case r of [] -> [(Nothing, i)]
                                     l  -> [(Just res, i') | (res, i') <- l])
                                     
sep :: Parser inp a -> Parser inp b -> Parser inp [a]
sep thing s = do r <- thing
                 nxt <- try s
                 case nxt of
                      Nothing -> return [r]
                      Just _ -> do rst <- sep thing s
                                   return (r:rst)                      

parsePunct :: String -> Parser Lexeme ()
parsePunct p = tok (\t -> case t of LPunct s | s == p -> Just ()
                                    _ -> Nothing)

                                    

parseOp :: String -> a -> Parser Lexeme a
parseOp str v = tok (\t -> case t of LOp s | s == str -> Just v
                                     _ -> Nothing)
                      
parseKeyword :: String -> Parser Lexeme ()                              
parseKeyword str = tok (\t -> case t of LKey s | s == str -> Just ()
                                        _ -> Nothing)


parseNum :: Parser Lexeme Exp
parseNum = tok (\t -> case t of LNumber n -> Just $ Num n
                                _ -> Nothing)

parseVar :: Parser Lexeme Exp
parseVar = tok (\t -> case t of LName n -> Just $ Var n
                                _ -> Nothing)


              
                               
parseAExp :: Parser Lexeme Exp
parseAExp = parseNum ||| parseVar |||
   do parsePunct "("
      e <- parseExp
      parsePunct ")"
      return e
    |||
   do (Var fn) <- parseVar
      parsePunct "("
      args <- sep parseExp (parsePunct ",")
      parsePunct ")"
      return $ Call fn args
      

      

parseAddOp, parseMulOp, parseRelateOp :: Parser Lexeme (Exp -> Exp -> Exp)
parseRelateOp = parseOp "==" EqualEqual ||| parseOp "/=" NotEqual ||| 
                parseOp "<" LessThan ||| parseOp "<=" LessEqual ||| 
                parseOp ">" GreaterThan ||| parseOp ">=" GreaterEqual
parseAddOp  = parseOp "+" Add ||| parseOp "-" Sub
parseMulOp  = parseOp "*" Mul ||| parseOp "/" Div


tst p s = runParser p (lexString s)

tste s = [r | (r, []) <- tst parseExp s]

tsts s = [r | (r, []) <- tst parseDef s]

p1 :: Parser Lexeme [Exp]
p1 = sep parseVar (parseOp "+" ())

p2 :: Parser Lexeme [Exp]
p2 = (do v <- parseVar
         return [v]) |||
     (do v <- parseVar
         parseOp "+" ()
         r <- p2
         return (v:r))
