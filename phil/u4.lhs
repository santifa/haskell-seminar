\documentclass{article}
%include lhs2TeX.fmt
%include polycode.fmt
\usepackage[ngerman]{babel}
\usepackage{ucs}
\usepackage[utf8x]{inputenc}
\usepackage{fullpage}
\usepackage{url}
\setlength{\parindent}{0pt}
\setlength{\parskip}{1.8ex}
\begin{document}
\begin{verbatim}
----------------------------------------------------------------------
--
--  Seminar Theoretische Informatik                         \ \
--  "Funktionale Programmierung mit Haskell"                 \ \ ---
--  Universität Potsdam SS 2014                              / /\ --
--  Tim Richter / Mario Frank                               / /  \
--
-- Übungsblatt 4 (Bäume)
-- Abgabe bis: 09.07.2014
-- Name: Philip Ullrich 753088
-- Erreichte Punkte:      / 28 
----------------------------------------------------------------------
\end{verbatim}

> module Uebung4 where
> import Data.List (intercalate)
> import Data.Random
> import Data.Random.Source.DevRandom
> import Data.Random.Extras

\section{Binärbäume}

Wir betrachten folgenden Typ von Binärbäumen

> data BTree a = Empty | Branch a (BTree a) (BTree a) deriving Show

wie zum Beispiel

> bt :: BTree Char
> bt = Branch 'a' (Branch 'b' Empty (Branch 'r' Empty Empty))  
>                 (Branch 'c' (Branch 'n' Empty Empty) 
>                             (Branch 'h' Empty Empty))

\subsection{|leafs| und |nodes|}

Definieren Sie Funktionen

< leafs, nodes :: BTree a -> Int

die die Anzahl der Blätter resp. inneren Knoten eines |BTree|s berechnen. ({\bf 2pt})

> leafs :: Btree a -> Int
> leafs Empty = 1
> leafs Branch _ x y = (leafs x) + (leafs y)

> nodes :: BTree a -> Int
> nodes Empty = 0
> nodes Branch _ x y = 1 + (leafs x) + (leafs y)


\subsection{|leafs bt = nodes bt + 1|}

Beweisen Sie, dass für |bt :: BTree a| die Gleichung

< leafs bt = 1 + nodes bt

gilt. Wie üblich ist jede Gleichung mit einer Begründung zu versehen (!),
arithmetische Umformungen und auch |1 + undefined = undefined| dürfen mit 
''Arithmetik'' begründet werden. ({\bf 2pt})

\subsection{|foldBT|}

Wie für alle algebraischen Datentypen gibt es auch für |BTree| eine |fold|-Funktion, 
die ein einfaches Rekursionsprinzip zur Definition von Funktionen auf |BTree|s kapselt:

> foldBT :: b -> (a -> b -> b -> b) -> BTree a -> b
> foldBT e f Empty = e
> foldBT e f (Branch l lt rt) = f l lt' rt' where
>      lt' = foldBT e f lt
>      rt' = foldBT e f rt

Schreiben Sie |leafs|, |nodes| und die Funktionen

> height :: BTree a -> Int
> height Empty            = 0
> height (Branch _ lt rt) = 1 + max (height lt) (height rt)
>
> flattenBT :: BTree a -> [a]
> flattenBT Empty            = []
> flattenBT (Branch x lt rt) = ll ++ [x] ++ rl where
>    [ll,rl] = (map flattenBT) [lt,rt]

als Instanzen von |foldBT|! ({\bf 2pt})

\subsection{Balancierte Bäume und Paramorphismen}

Ein Binärbaum heisst ''balanciert'', wenn für jeden inneren Knoten die 
Anzahlen der |nodes| im linken und rechten Teilbaum höchstens um |1| 
differieren. Der Balanciertheitstest

< isBalanced :: BTree a -> Bool

lässt sich offenbar nicht als |foldBT|-Instanz schreiben: 
|isBalanced (Branch lt x rt) | hängt nicht nur von den boolschen 
Werten |isBalanced lt| und |isBalanced rt| ab, sondern
auch von |nodes lt| und |nodes rt|, die Funktion |f| in 
|foldBT| bekommt aber die Teilbäume selbst nicht übergeben...

Das lässt sich ändern: Verallgemeinern Sie |foldBT| zu

< paramBT :: b -> (a -> (b,BTree a) -> (b,BTree a) -> b) -> BTree a -> b
< paramBT = ...

({\bf 1pt}) und schreiben Sie |isBalanced| (unter Benutzung von |nodes|) 
als ''Paramorphismus'', d.h. als Instanz von |paramBT| ({\bf 1pt}):

< isBalanced = paramBT True f where
<    f (bl,tl) _ (br,tr) = ...

''Paramorphismen'' lassen sich für alle algebraischen Datentypen definieren.
Für Listen haben wir (in Verallgemeinerung von |foldr|):

> paramL :: (a -> (b,[a]) -> b) -> b -> [a] -> b
> paramL f e []     = e
> paramL f e (x:xs) = f x (paramL f e xs,xs)

Damit kann z.B. auch |dropWhile| implementiert werden. Vervollständigen Sie 
folgende Definition: ({\bf 1pt})

< dropWhile' :: (a -> Bool) -> [a] -> [a]
< dropWhile' p = paramL f [] where
<      f x (dxs,xs) = ...

\section{Funktoren}

Die |map| Funktion von Listen lässt sich für viele algebraische Datentypen 
verallgemeinern. Haskell definiert dafür eine Typklasse

< class Functor f where
<   fmap :: (a -> b) -> f a -> f b

Es gibt folgende Gesetze, die jede Instanz von Funktor
erfüllen sollte (die ''functor laws''):

<   fmap id      = id                   --{ Functor-id }
<   fmap (g . h) = (fmap g) . (fmap h)  --{ Functor-compose }

Schreiben Sie eine Funktorinstanz für |BTree| ({\bf 1pt}) und beweisen 
Sie folgende Eigenschaften Ihrer Definition von |fmap| ({\bf 2pt}):

Für (beliebige) |a|,|b|, |f::a -> b|, |x::a|, |lt,rt::Btree a| gelten

< (fmap f) undefined        = undefined        --{ Prop1 }
< (fmap f) Empty            = Empty            --{ Prop2 }
< (fmap f) (Branch x lt rt) =
<         Branch (f x) (fmap f lt) (fmap f rt) --{ Prop3 }

Machen Sie sich klar, dass die functor laws leicht aus diesen 
Eigenschaften folgen, Sie müssen das aber nicht ausführen. 

Schauen Sie sich andere Funktor-Instanzen aus dem Prelude an, 
insbesondere |Maybe| und |((->) r)|!

\section{Abstract syntax trees}

Arithmetische Ausdrücke mit |+|,|-| und |*| können durch eine
Baumstruktur |Exp a| dargestellt werden:

> data Ops   = Plus | Minus | Mult 
> data Exp a = Val a | Op (Exp a) Ops (Exp a)

Beispiel

> expr = Op (Val 7) Plus (Op (Op (Val 4) Minus (Val 23)) Mult (Val 2))

1. Schreiben Sie eine Funktion

< display :: (Show a) => Exp a -> String

die eine mit '(' und ')') korrekt geklammerte String-Darstellung, z.B.
|display expr = "(7 + ((4 - 23) * 2))"| liefert! ({\bf 1pt}) 

2. Schreiben Sie eine Funktion

< eval :: (Num a) => Exps a -> a

die den Wert eines Ausdrucks nach den üblichen Regeln der Arithmetik 
berechnet! (z.B. |eval exp1 = -31|) ({\bf 1pt})

3. Modifizieren Sie |Ops| und |Exps| so, dass auch Quotienten 
dargestellt werden können und implementieren Sie eine Evaluierungsfunktion

< eval2 :: (Integral a) => Exp2 a -> Maybe a

|eval2| soll Quotienten mittels |div| auswerten und total sein. Kommt 
irgendwo im Argumentausdruck eine Division durch |0| vor, soll |Nothing| 
zurückgeben werden.({\bf 2pt})


\section{Unfolds}

Das duale Konzept zu folds sind ''unfolds''. Eine unfold-Funktion für 
einen Datentypen kapselt die einfachste Methode, Funktionen {\em in} 
diesen Typen zu definieren. Für Listen haben wir z.B.

> unfoldL :: (b -> Maybe (a,b)) -> b -> [a]
> unfoldL f y = case f y of
>       Nothing     -> []
>       Just (x,y') -> x : (unfoldL f y')

|f| ''entscheidet", ob aus dem ''seed'' |y::b| eine leere oder 
nichtleere Liste ensteht und gibt im letzteren Fall den |head| der 
Liste und einen neuen seed zur Berechnung des |tail|s an.

Schreiben Sie eine Funktion

< intsfrom :: Int -> [Int]

mit |intsfrom k = [k,k+1,...]| als Instanz von |unfoldL|. ({\bf 1pt})

Vervollständigen Sie folgende Definition so, dass |iterate'| gleich
der Prelude-Funktion |iterate| ist: ({\bf 1pt})

< iterate' :: (a -> a) -> a -> [a]
< iterate' f = unfoldL g where g x = ... 

Vervollständigen Sie ({\bf 1pt}):

< wrapAt :: Int -> [a] -> [[a]]
< wrapAt n = unfoldL g where
<            g ...

Für beliebiges |n::Int| sollen dabei gelten

< concat . (wrapAt n)                             = id
< (all (<=n)) . (map length) . (wrapAt n)         = const True
< (all (==n)) . (map length) . inits . (wrapAt n) = const True

Geben Sie Funktionen

< f1, f2, f3, f4, f5 :: Bool -> Maybe (Int,Bool)

an, sodass

< unfoldL f1 True = []              -- E: leer
< unfoldL f2 True = repeat 5        -- C: konstante unendliche Liste
< unfoldL f3 True = [7]             -- O: ein Element
< unfoldL f4 True = cycle [1,2]     -- A: alternierende unendliche Liste
< unfoldL f5 True = 3 : repeat 5    -- OC: O gefolgt von C

gilt.({\bf 2pt}) Begründen Sie, dass jede (totale) Funktionen |g :: Bool -> [Int]|, 
die sich als unfoldL schreiben lässt, |True| auf eine Liste abbildet, die
von einer der obigen Arten (E,C,O,A oder OC) ist. Von welcher Art kann 
jeweils |g False| sein? ({\bf 2pt})

\section{RoseTrees und Spiele}

> data RoseTree a = Node { label :: a, subs :: [RoseTree a] }

> instance (Show a) => Show (RoseTree a) where
>   show = showPrefix "" where
>     showPrefix pre (Node x rts) = 
>       concatMap f (show x) ++ 
>       concatMap (showPrefix (pre ++ "  ")) rts  where
>         f '\n' = "\n" ++ pre
>         f x    = [x]

Implementieren Sie |foldRT| und |unfoldRT| ! ({\bf 2pt})

Rosetrees können zur Darstellung von Spielbäumen benutzt werden. 
Für |a| wird der Typ aller Spielsituationen genommen und |Node x xs| 
beschreibt mögliche Spielverläufe, die in Sitation |x| beginnen.

Beispiel: der Typ |Board| beschreibt die möglichen Spielsituationen beim 
Tic-Tac-Toe.

> data Field  = X | O | E deriving (Eq)
> type MaybePlayer = Field   

Wir ''missbrauchen'' Field als Ersatz für |Maybe Player|, wobei Player ein
zweielementiger Typ mit zu |X| und |O| korrespondierenden Konstruktoren wäre und
|Maybe Player| also, wie Field, drei endliche Elemente hätte.

> newtype Board = B { board :: [[Field]] } deriving (Eq)

Eigentlich brauchen wir 3x3-Matrizen, aber da Haskell keine abhängigen Typen
hat, kann das nicht im Typen dargestellt werden. Der newtype wrapper wird für 
die Show-Instanz benötigt:

> instance Show Field where
>   show X = "X"; show O = "O"; show E = " "
>
> instance Show Board where
>   show (B b) =
>     prefix 
>     ++ intercalate (rowsep ++ prefix) (map ((intercalate colsep).(map show)) b)
>     ++ prefix ++ info (whosnext b) (winner b) ++ "\n" where
>     prefix   = "\n "
>     rowsep   = "\n-----------"
>     colsep   = " | "
>     info E E = "Remis"
>     info p E = show p ++ "s turn"
>     info E p = show p ++ " wins"

Hier Funktionen, die einen eventuellen Gewinner einer Spielsituation bestimmen

> winner  :: [[Field]] -> MaybePlayer
> winner b = case map (wins b) [X,O] of
>        [True,_] -> X
>        [_,True] -> O
>        _        -> E
>   where      
>     wins b p = 
>       allp `elem` diag b : diag (reverse b) : (b ++ (transpose b)) where
>         allp = [p,p,p]
>         transpose = foldr (zipWith (:)) [[],[],[]]
>         diag      = flip (zipWith (!!)) [0..]

und berechnen, wer am Zug ist ist. |X| soll immer beginnen und |O|
nachziehen. Falls alle Felder belegt sind oder es einen ''echten'' 
Sieger gibt, ist |whosenext == E|:

> whosnext :: [[Field]] -> MaybePlayer
> whosnext b | (count E b)  == 0 || winner b /= E  = E
>            | (count X b) > (count O b)           = O
>            | otherwise                           = X
>   where count p = sum . map (length . (filter (==p)))

Vervollständigen Sie die folgende Definition zur Berechnung
aller möglichen Folgesituationen ({\bf 1pt})

< nextBoards :: Board -> [Board]
< nextBoards (B b) = case (whosnext b) of
<     E -> []
<     p -> ((map (B . (wrapAt 3))) . (paramL g []) . concat) b  where
<           g f (nBfs,fs) = ... 

und implementieren Sie die Funktion

< tSubTree :: Board -> RoseTree Board
< tSubTree = ...

zur Berechnung des Spielbaums.

Zur Bestimmung eines guten nächsten Zuges implementieren Sie
die Funktionen |canWin| und |canEnforceRemis|, die entscheiden,
ob ein Spielbaum einem Spieler ein Erzwingen des Sieges
oder eines Remis ermöglicht ({\bf 2pt}) (Der Wert beider Funktionen
für |p == E| ist unwichtig ...)

< canWin :: MaybePlayer -> RoseTree Board -> Bool
< canWin p = foldRT f where
<    f (B b) [] = ... 
<     ...

< canEnforceRemis :: MaybePlayer -> RoseTree Board -> Bool
< canEnforceRemis p = foldRT f where
<    f (B b) [] = ...
<     ...

und eine Funktion, die zu einem solchen Prädikat
auf |RoseTree Board| diejenigen Folgesituationen 
berechnet, deren Spielbäume das Prädikat erfüllen ({\bf 1pt}):

< pMoves :: (RoseTree Board -> Bool) -> Board -> [Board] 
< pMoves pred = ...

Dann kann eine Funktion zur Auswahl eines guten Zug so implementiert
werden

< goodMove :: MaybePlayer -> Board -> IO (Maybe Board)
< goodMove p (B b) =
<          if winMoves /= []
<          then pickMove winMoves
<          else if remisMoves /= []
<               then pickMove remisMoves
<               else return Nothing
<          where
<            winMoves    = pMoves (canWin p) (B b)
<            remisMoves  = pMoves (canEnforceRemis p) (B b)
<            pickMove bs = runRVar (choice (map Just bs)) DevRandom

und schliesslich können wir Tic-Tac-Toe spielen:

< tictactoe :: IO ()
< tictactoe = game "Wanna play a round of Tic-Tac-Toe?" where
<    game m = do 
<    goon <- askYN m
<    if not goon then return ()
<    else do 
<       ibegin <- askYN "May I begin?"
<       play (if ibegin then O else X) start
<    where
<      start = B [[E,E,E],[E,E,E],[E,E,E]]
<      askYN m = do
<        putStr (m ++ " (y/n) ")
<        answer <- getLine
<        case answer of
<          "y" -> return True
<          "n" -> return False
<          _   -> askYN m
<      play p (B b)  
<          | (whosnext b) == E = finish p b
<          | (whosnext b) == p = getPlayerMove p b
<          | otherwise         = chooseMyMove p b
<      getPlayerMove p b = do
<        putStr ( show (B b) ++ "\nYour move? Give a number from 1-9! " )
<        pos <- getLine
<        let i = (read pos) in
<          if i `elem` [1..9] && (concat b)!!(i-1) == E
<          then let b' = wrapAt 3 (take (i-1) (concat b) ++ [p] 
<                        ++ (drop i (concat b))) in play p (B b')
<          else getPlayerMove p b
<      chooseMyMove p b = do
<        putStrLn "Let me think..."
<        b' <- goodMove (other p) (B b) 
<           where other X = O; other O = X
<        case b' of
<          Nothing -> finMessage b "Ok, you win."
<          (Just b'') -> play p b''
<      finish p b 
<          | (winner b)==p = finMessage b "Ok, you win." 
<          | (winner b)==E = finMessage b "Remis."
<          | otherwise     = finMessage b "Gotcha!"
<      finMessage b m = do
<         putStrLn ((show (B b)) ++ m) 
<         game "Again?" 

Leider verlieren menschliche Gegner schnell die Lust. Wer mag, kann 
|goodMove| so modifizieren, dass auch gelegentlich schlechte Züge gewählt
werden.

\end{document}
