+-----------------------------------------------------------------------------+
|                                                                             |
|   Seminar Theoretische Informatik                                           |
|   "Funktionale Programmierung mit Haskell"                                  |
|   Universität Potsdam SS 2014                                               |
|   Tim Richter / Mario Frank                                                 |
|   Kapitel 12: "An automatic calculator"                                     |
|   Philip Ullrich / Henrik Jürges
+-----------------------------------------------------------------------------+


Gliederung
-------------------------------------------------------------------------------
0) Endlich mal was praktisches
1) Basics für die gute Küche
2) Pfannen, Töpfe und Zutaten
3)

0) Endlich mal was praktisches
-------------------------------------------------------------------------------

> module Calculon where
> import Parser
> import Data.List (nub)
> import Debug.Trace


Wir wollen einen automatischen Beweiser implementieren.

1) Basics für die gute Küche
-------------------------------------------------------------------------------

Idee:
< calculate :: [Law] -> Expr -> Calculation

Der Prozess soll vollautomatisch sein.

Zur besseren Behandlung, brauchen wir ein paar einfache Hilfsfunktionen.

< parseLaw :: String -> Law
< parseExpr :: String -> Expr
< printExpr :: Expr -> String
< printCalc :: Calculation -> String

//Woraus bestehen Laws?
//-> Name und Gleichung

Ein einfacher Term-Minimierer:
< simplify :: [Law] -> String -> IO ()
< simplify laws = putStr . printCalc . calculate laws . parseExpr

-- simplify bsp.

1. Bedingung
Es wird immer nur in eine Richtung gegangen, also von links nach recht
oder anders herum. Aber nicht Abwechselnd -> Oszillieren

2. Bedingung
Rekursive Definitionen -> Endlosschleifen

=> Was Tun?
1. Bedingung -> Gleichung spalten und einzelne Terme vereinfachen.
(Bis sie sich treffen)
2. Bedingung -> Priorisierung der Regeln oder Generalisierung

--------------------------------------------------

> pairs = map parseLaw [
>       "definition fst: fst.pair(f, g) = f",
>       "definition snd: snd.pair(f, g) = g",
>       "definition cross: cross(f,g) = pair(f.fst, g.snd)",
>       "pair absorption: pair(f, g).h = pair(f.h, g.h)" ]

> laws = filters ++ ifs ++others

> filters
>    = map parseLaw [
>      "definition filter: filter p = concat.map(box p)",
>      "definition box:    box p = if(p, wrap, nil)"
>    ]

> ifs
>    = map parseLaw [
>      "if over composiition:  if(p,f,g).h = if(p.h, f.h, g.h)",
>      "composition over if:   h.if(p,f,g) = if(p, h.f, h.g)"
>    ]

> others
>    = map parseLaw [
>      "nil constant:      nil.f = nil",
>      "nil natural:       map f.nil = nil",
>      "wrap natural:      map f.wrap = wrap.f",
>      "concat natural:    map f.concat = concat.map(map f)",
>      "map functor:       map f.map g = map(f.g)"
>    ]


? prove laws "filter p.map f = map f.filter(p.f)"

   filter p.map f
 =   {definition filter}
   concat.map(box p).map f
 =   {map functor}
   concat.map(box p.f)
 =   {definition box}
   concat.map(if(p, wrap.f, nil).f)
 =   {if over composition}
   concat.map(if(p, wrap.f, nil.f))
 =   {nil constant}
   concat.map(if(p.f, wrap.f, nil))
 =   {wrap natural}
   concat.map(if(p.f, map f.wrap, nil))
 =   {nil natural}
   concat.map(if(p.f, map f.wrap, map f.nil))
 =   {composition over if}
   concat.map(map f.if(p.f, wrap, nil))
 =   {definition box}
   concat.map(map f.box(p.f))
 =   {map functor}
   concat.map(map f).map(box(p.f))
 =   {concat natural}
   map f.concat.map(box(p.f))
 =   {definition filter}
   map f.filter(p.f)

--------------------------------------------------

1 Was haben damit erreicht?
Einfachheit
- Nachteil: Terminiert nicht immer

2 Unser Ansatz: Prioritäten
1. Regeln wie beim Pattern Matching (Wer zuerst kommt, malt zuerst)
2. Der erste Teilausdruck wird ersetzt
3. Telausdrücke vor Regeln
4. Regeln können in Basics und Others geteilt werden

Unsere angepassten Funktionen:
Simplifier:
< simplify laws = putStr . printCalc . calculate (basic, others) . parseExpr
<                 where (basic, others) = partition basicLaw laws

Prover:
< prove :: [Law] -> String -> IO ()
< prove laws = putStr . printCalc .proveEqn (basic, others) . parseEqn
<              where (basic, others) = partition basicLaw laws

Calculater:
< calculate :: ([Law], [Law]) -> Expr -> Calculation

2) Pfannen, Töpfe und Zutaten
-------------------------------------------------------------------------------

Datentypen zu unseren erdachten Funktionen
--------------------------------------------------

- Expression

> data Expr = Var VarName | Con ConName[Expr] | Compose[Expr]
>           deriving (Eq, Ord)
> type VarName = Char
> type ConName = String
>

> instance Show Expr where
>    show (Var name) = [name]
>    show (Con name []) = name
>    show (Con name (x:[])) = name ++ " " ++ show x
>    show (Con name xs) = name ++ "(" ++ (toStr xs ", ") ++ ")"
>    show (Compose xs) = toStr xs "."

> toStr [] _ = ""
> toStr (x:[]) _ = show x
> toStr (x:xs) i = show x ++ i ++ toStr xs i

-> foldr f e kann nicht dargestellt werden, darum foldr (f, e)

- Law

> type Law = (LawName, Expr, Expr)
> type LawName = String

- Calculation

> type Calculation = (Expr, [Step])
> type Step = (LawName, Expr)

Komposition von Ausdrücken
--------------------------------------------------

> compose :: [Expr] -> Expr
> compose xs  = if singleton xs
>               then head xs
>               else Compose(concat(map decompose xs))
>
> decompose :: Expr -> [Expr]
> decompose(Var v)        = [Var v]
> decompose(Con f xs)     = [Con f xs]
> decompose(Compose xs)   = xs

Komplexität von Ausdrücken
--------------------------------------------------

> complexity :: Expr -> Int
> complexity (Var v)      = 1
> complexity (Con f xs)   = 1
> complexity (Compose xs) = length xs

Der Parser für Expressions
--------------------------------------------------

> parseExpr :: String -> Expr
> parseExpr = applyParser expr
>
> expr :: Parser Expr
> expr = do xs <- somewith (symbol ".") term
>           return (compose xs)
>
> term :: Parser Expr
> term = do space
>           c <- letter
>           cs <- many alphanum
>           if null cs
>              then return (Var c)
>             else do xs <- argument
>                     return (Con (c:cs) xs)
>
> argument :: Parser [Expr]
> argument = tuple `orelse` (notuple `orelse` return [])
>
> tuple :: Parser [Expr]
> tuple = do symbol "("
>            xs <- somewith (symbol ",") expr
>            symbol ")"
>            return xs
>
> notuple :: Parser [Expr]
> notuple = do space
>              c <- letter
>              cs <- many alphanum
>              if null cs
>                 then return [Var c]
>                else return [Con (c:cs) []]

Der Parser für Gleichungen
--------------------------------------------------

> parseEqn :: String -> (Expr, Expr)
> parseEqn = applyParser eqn
>
> eqn :: Parser (Expr, Expr)
> eqn = do space
>          x <- expr
>          symbol "="
>          y <- expr
>          return (x, y)

Der Law Parser
--------------------------------------------------

> parseLaw :: String -> Law
> parseLaw = applyParser law
>
> law :: Parser Law
> law = do space
>          name <- some (sat (/= ':'))
>          symbol ":"
>          (x, y) <- eqn
>          return (name, x, y)
>
> basicLaw :: Law -> Bool
> basicLaw (name, lhs, rhs) = (complexity lhs > complexity rhs)

-- bis hierhin richtig

Die Berechnung
--------------------------------------------------

> conclusion :: Calculation -> Expr
> conclusion (x, steps) = if null steps then x else  snd (last steps)
>
> paste :: Calculation -> Calculation -> Calculation
> paste lhc rhc = (fst lhc, snd lhc ++ link x y ++ shuffle rhc)
>                 where x = conclusion lhc
>                       y = conclusion rhc
>
> link :: Expr -> Expr -> [Step]
> link x y = if x == y then [] else [("... ??? ...",y)]
>
> shuffle :: Calculation -> [Step]
> shuffle (expr, stepList) = snd (foldl shunt (expr, []) stepList)
>                   where shunt (stepList, rs) (r, y) = (y, (r, expr):rs)
>
> printCalc :: Calculation -> String
> printCalc (x, ss) = "||  " ++ show x ++ "\n||" ++
>                     concat (map printStep ss) ++ "\n"
>
> printStep :: Step -> String
> printStep (why, x) = "=  {" ++ why ++ "} \n||  " ++
>                      show x ++ "\n||"

Unsere vorher erdachten Funktionen
--------------------------------------------------

> simplify :: [Law] -> String -> IO ()
> simplify laws = putStr . printCalc . calculate (basic,others) . parseExpr
>                 where (basic,others) = partition basicLaw laws
>
> partition :: (a -> Bool) -> [a] -> ([a],[a])
> partition p xs = (filter p xs, filter (not . p) xs)
>
> prove :: [Law] -> String -> IO ()
> prove laws = putStr . printCalc . proveEqn (basic,others) . parseEqn
>               where (basic,others) = partition basicLaw laws
>
> proveEqn :: ([Law],[Law]) -> (Expr, Expr) -> Calculation
> proveEqn laws (lhs,rhs) = paste (calculate laws lhs) (calculate laws rhs)


3) Abschmecken
-------------------------------------------------------------------------------

Unsere Substition

> type Subst = [(VarName, Expr)]

Teilausdrücke und ihr Ort

> type SubExpr = (Location, Expr)
> data Location = All | Seg Int Int | Pos Int Location deriving (Show, Eq)

Binding gibt einen Ausdruck zurück der mit einer Variable substituiert
wird. Leere Liste ist Identität

> binding :: Subst -> VarName -> Expr
> binding [] v          = Var v
> binding ((u,x):s) v   = if u == v then x else binding s v

Substitution an Ausdruck binden.

> applySub :: Subst -> Expr -> Expr
> applySub s (Var v) = binding s v
> applySub s (Con f xs) = Con f (map (applySub s) xs)
> applySub s (Compose xs) = compose (map (applySub s) xs)



> extend :: Subst -> (VarName, Expr) -> [Subst]
> extend s (v,x)
>     | y == x = [s]
>     | y == Var v = [(v,x):s]
>     | otherwise = []
>         where y = binding s v


TODO phils shit kapitel matching seite 388

> match :: (Expr, Expr) -> [Subst]
> match (Var x,y) = [[(x,y)]]
> match (Con f xs, Var v) = []
> match (Con f xs, Compose ys) = []
> match (Con f xs, Con g ys) = if f == g then matchlist (zip xs ys) else []
> match (Compose xs, Var v) = []
> match (Compose xs, Con g ys) = []
> match (Compose xs, Compose ys) = concat (map matchlist (align xs ys))


> xmatch :: Subst -> (Expr,Expr) -> [Subst]
> xmatch s (Var v,x)                = extend s (v,x)
> xmatch s (Con f xs, Var v)        = []
> xmatch s (Con f xs, Compose ys)   = []
> xmatch s (Con f xs, Con g ys)
>   = if f == g then xmatchlist s (zip xs ys) else []
> xmatch s (Compose xs, Var v)      = []
> xmatch s (Compose xs, Con g ys)   = []
> xmatch s (Compose xs, Compose ys)
>   = concat (map (xmatchlist s) (align xs ys))



> matchlist :: [(Expr, Expr)] -> [Subst]
> matchlist = concat . map unify . cplist . map match

> xmatchlist :: Subst -> [(Expr, Expr)] -> [Subst]
> xmatchlist s xys = concat [union s t | t <- matchlist xys]

TODO phils shit kapitel subexpression and rewriting

>
> subexprs :: Expr -> [SubExpr]
> subexprs (Var v) = [(All, Var v)]
> subexprs (Con f xs) = [(All, Con f xs)] ++ subterms xs
> subexprs (Compose xs) = [(All, Compose xs)] ++ segments xs ++ subterms xs


> subterms :: [Expr] -> [SubExpr]
> subterms xs
>   = [(Pos j loc, y) | j <- [0..n-1], (loc,y) <- subexprs (xs !! j)]
>       where n = length xs
>
> segments :: [Expr] -> [SubExpr]
> segments xs
>   = [(Seg j k, Compose (take k (drop j xs))) | k <- [2..n-1], j <- [0..n-1]]
>       where n = length xs



> replace :: Expr -> Location -> Expr -> Expr
> replace x All y = y
> replace (Con f xs) (Pos j loc) y
>   = Con f (take j xs ++ [replace (xs !! j) loc y] ++ drop (j+1) xs)
> replace (Compose xs) (Pos j loc) y
>   = compose (take j xs ++ [replace (xs !! j) loc y] ++ drop (j+1) xs)
> replace (Compose xs) (Seg j k) y
>   = compose (take j xs ++ [y] ++ drop (j+k) xs)
>
>

Die wichtigste Funktion

> calculate :: ([Law],[Law]) -> Expr -> Calculation
> calculate laws x = (x,repeatedly (rewrites laws) x)
>
>
> rewrites :: ([Law], [Law]) -> Expr -> [Step]
> rewrites (basic, others) x
>   = concat ([rewrite law sx x | law <- basic, sx <- subexprs x]
>     ++ [rewrite law sx x | sx <- subexprs x, law <- others])
>
> rewrite :: Law -> SubExpr -> Expr -> [Step]
> rewrite (name, lhs, rhs) (loc, y) x
>   = [(name, replace x loc (applySub s rhs)) | s <- match (lhs, y)]


> repeatedly :: (Expr -> [Step]) -> Expr -> [Step]
> repeatedly rws x
>   = if null steps then [] else (n,y) : repeatedly rws y
>       where steps = rws x
>             (n,y) = head steps



> unify :: [Subst] -> [Subst]
> unify [] = [[]]
> unify (s:ss) = concat [union s t | t <- unify ss]

> union :: Subst -> Subst -> [Subst]
> union s t = if compatible s t then [nub (s ++ t)] else []

> compatible :: Subst -> Subst -> Bool
> compatible (x:xs) [] = True
> compatible [] (y:ys) = False
> compatible [] [] = True
> compatible ((n1, e1):xs) ((n2, e2):ys) = if n1 == n2 && e1 <= e2
>                                          then compatible xs ys
>                                          else False

4) Genießen
-------------------------------------------------------------------------------

5) hilsfunktion und constanten
-------------------------------------------------------------------------------

>
> exampleExprSimplify = "cross(f, g).pair(h, k)"
> sim1 = parseExpr exampleExprSimplify
> sim2 = parseExpr "pair(f.fst, g.snd).pair(h, k)"
>
> exampleLawsSimplify = partition basicLaw pairs
>
>
> exampleExprProve = "filter p.map f = map f.filter(p.f)"
> exampleLawsProve = partition basicLaw laws
> rewB = [(law, sx, x) | law <- fst (exampleLawsSimplify), sx <- subexprs x]
>   where x = parseExpr exampleExprSimplify
> rewA = [(law, sx, x) | sx <- subexprs x, law <- snd (exampleLawsSimplify)]
>   where x = parseExpr exampleExprSimplify

> simple :: [Expr] -> Bool
> simple xs = singleton xs && simpleton(head xs)

- liste ist nicht leer

> singleton :: [a] -> Bool
> singleton xs = not (null xs) && null (tail xs)

- gibt zurück ob regel einfach oder nicht

> simpleton :: Expr -> Bool
> simpleton (Var v)   = True
> simpleton (Con f xs) = null xs
> simpleton (Compose xs) = False

> align :: [Expr] -> [Expr] -> [[(Expr,Expr)]]
> align xs ys = [zip xs (map compose zs) | zs <- parts (length xs) ys]


> parts :: Int -> [a] -> [[[a]]]
> parts 0 []            = [[]]
> parts 0 (x : xs)      = []
> parts (n) []          = []
> parts (n) (x : xs)    = map (new x) (parts (n - 1) xs) ++
>                         map (glue x) (parts n xs)
>
>
> new :: a -> [[a]] -> [[a]]
> new x yss = [x] : yss
>
>
> glue :: a -> [[a]] -> [[a]]
> glue x (ys : yss) = (x : ys) : yss

> cup :: [Subst] -> [Subst] -> [Subst]
> cup ss ts = concat [union s t | s <- ss, t <- ts]

Verkettet alle n Listen in der Liste mit jedem anderen Element in der
Liste einmal.

> cplist :: [[a]] -> [[a]]
> cplist [] = [[]]
> cplist (xs:xss) = [y:ys| y <- xs, ys <- cplist xss]

Testesser
--------------------------------------------------

> printLaw xs = mapM_ putStrLn zs
>   where zs = map (\(x, y, z) ->
>                    x ++ ":\t" ++ show y ++ " :=: " ++ show z) xs

> printList xs = mapM_ putStrLn xs
> testLaw = printLaw pairs
> testCpList = cplist [[1,2,3],[1,5,4],[5,77,89]]

> test2 = putStr . printCalc . calculate (partition basicLaw pairs) . parseExpr
> test3 = putStr . printCalc . calculate (partition basicLaw laws) . parseExpr
> test4 = subexprs $ parseExpr "filter p.map f"
