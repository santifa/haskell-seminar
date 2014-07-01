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

laws = filters ++ ifs ++others

filters
   = map parseLaw [
     "definition filter: filter p = concat.map(box p)",
     "definition box:    box p = if(p, wrap, nil)"
   ]

ifs
   = map parseLaw [
     "if over composiition:  if(p,f,g).h = if(p.h, f.h, g.h)",
     "composition over if:   h.if(p,f,g) = if(p, h.f, h.g)"
   ]

others
   = map ParseLaw [
     "nil constant:      nil.f = nil",
     "nil natural:       map f.nil = nil",
     "wrap natural:      map f.wrap = wrap.f",
     "concat natural:    map f.concat = concat.map(map f)",
     "map functor:       map f.map g = map(f.g)"
   ]


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

> module Calculon where
> import Parser hiding (Con, term, Expr)

Datentypen zu unseren erdachten Funktionen
- Expression

> data Expr = Var VarName | Con ConName[Expr] | Compose[Expr] deriving (Eq)
> type VarName = Char
> type ConName = String

-> foldr f e kann nicht dargestellt werden, darum foldr (f, e)

- Law

> type Law = (LawName, Expr, Expr)
> type LawName = String

- Calculation

> type Calculation = (Expr, [Step])
> type Step = (LawName, Expr)

--------------------------------------------------
- Komposition von Ausdrücken

> compose :: [Expr] -> Expr
> compose xs  = if singleton xs
>               then head xs
>               else Compose(concat(map decompose xs))
>
> decompose :: Expr -> [Expr]
> decompose(Var v)        = [Var v]
> decompose(Con f xs)     = [Con f xs]
> decompose(Compose xs)   = xs

- Komplexität von Ausdrücken

> complexity :: Expr -> Int
> complexity (Var v)      = 1
> complexity (Con f xs)   = 1
> complexity (Compose xs) = length xs

--------------------------------------------------

Ausgabe für unsere Expressions

> printExpr :: Expr -> String
> printExpr (Var v)   = [v]
> printExpr (Con f xs)
>     | null xs       = f
>     | simple xs     = f ++ " " ++  printExpr(head xs)
>     | otherwise     = f ++ "(" ++
>                         joinWith ", " (map printExpr xs) ++ ")"
> printExpr (Compose xs) = joinWith "." (map printExpr xs)

--------------------------------------------------

> simple :: [Expr] -> Bool
> simple xs = singleton xs && simpleton(head xs)

- joinWith aus Kapitel 5.5 und weitere Hilfsfunktionen

> simpleton :: Expr -> Bool
> simpleton (Var v)   = True
> simpleton (Con f xs) = null xs
> simpleton (Compose xs) = False
>
> joinWith :: String -> [String] -> String
> joinWith x = foldr1 (j)
>   where xs `j` ys = xs ++ x ++ ys
>
> singleton :: [a] -> Bool
> singleton xs = not (null xs) && null (tail xs)

--------------------------------------------------

Der Parser für Expressions

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

> parseLaw :: String -> Law
> parseLaw = applyParser law
>
> law :: Parser Law
> law = do space
>          name <- some (sat (/= ','))
>          symbol ":"
>          (x, y) <- eqn
>          return (name, x, y)
>
> basicLaw :: Law -> Bool
> basicLaw (name, lhs, rhs) = (complexity lhs > complexity rhs)

Die Berechnung

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
> shuffle (x, ss) = snd (foldl shunt (x, []) ss)
>                   where shunt (x, rs) (r, y) = (y, (r,x):rs)
>
> printCalc :: Calculation -> String
> printCalc (x, ss) = "||  " ++ printExpr x ++ "||" ++ concat (map printStep ss)
>
> printStep :: Step -> String
> printStep (why, x) = "=  {" ++ why ++ "} ||  " ++
>                      printExpr x ++ "||"



3) Abschmecken
-------------------------------------------------------------------------------

4) Genießen
-------------------------------------------------------------------------------
