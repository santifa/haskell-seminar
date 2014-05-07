\documentclass{article}
%include lhs2TeX.fmt
%include polycode.fmt
\usepackage[ngerman]{babel}
\usepackage{ucs}
\usepackage[utf8x]{inputenc}
\usepackage{fullpage}
\setlength{\parindent}{0pt}
\begin{document}

\setcounter{section}{1}
\section{ Einfache Typen }

\begin{code}
module Uebung2 where
import Prelude hiding (div, subtract)
\end{code}

Wir definieren noch einmal den Datentypen |Nat| und die Konversionen von und nach |Int|:
\begin{code}
data Nat = O | S Nat deriving (Show, Eq)
\end{code}

\begin{code}
intToNat :: Int -> Nat
intToNat 0 = (O)
intToNat n = S ( intToNat (n-1) )
\end{code}

\begin{code}
natToInt :: Nat -> Int
natToInt O = 0
natToInt (S n) = 1 + natToInt n
\end{code}

\subsection{Datentypen und Operationen}

\begin{enumerate}

\item Definieren Sie eine Funktion |intToNatg| analog zu |intToNat| unter Benutzung
      von guards. Stellen Sie sicher, dass |intToNatg| bei Eingabe einer negativen
      Zahl eine Fehlermeldung ausgibt. (1pt)

\begin{code}
intToNatg :: Int -> Nat
intToNatg n
  | n < 0 = error ("Negative numbers are not allowed.")
  | n == 0 = (O)
  | otherwise = S ( intToNatg (n-1))
\end{code}

\item Definieren Sie die Subtraktion und die (ganzzahlige) Division
      |subtract, div :: Nat -> Nat -> Nat| so, dass für endliche Elemente |n,m :: Nat|
      (endliche Elemente sind |O, S O, S (S O),...|) folgende Eigenschaften gelten:

\begin{code}
instance Ord Nat where
  n < m = natToInt n < natToInt m
  n <= m = natToInt n <= natToInt m

substract :: Nat -> Nat -> Nat
substract O _ = O
substract n O = n
substract (S n) (S m) = substract n m

div :: Nat -> Nat -> Nat
div n m
  | m == O = error ("Devision through zero is not allowed.")
  | n < m = O
  | n == m = (S O)
  | otherwise = S (div (substract n m) m)

\end{code}

\begin{enumerate}
    \item |subtract (n+m) n = m |
    \item |div (n*m) n = m|
\end{enumerate}

Beweisen Sie diese Eigenschaften! (3pt)
\begin{enumerate}
\item | substract (n+m) n = m |
\item | substract (n+m) n |
\item | {def substract} |
\item | substract m 0 |
\item | m |

\item | div (n*m) n = m |
\item | div (n*m) n |
\item | {def div} |
\item | div n m mit n < m |
\item | m |
\begin{enumerate}


\item Viele Funktionen auf dem Datentyp |Nat| kann man durch folgendes Rekursionsprinzip
      definieren:

\begin{code}
foldNat :: a -> (a -> a) -> Nat -> a
foldNat start next O     = start
foldNat start next (S n) = next ( foldNat start next n)
\end{code}

        Es ist z.B. |id = foldNat O S| und | natToInt = foldNat 0 (+1) |. Definieren Sie
        Funktionen | isEven |, die für gerade Zahlen |True| und sonst |False| zurückgibt,
        und |isZero|, die für |O| |True| und sonst |False| zurückgibt, mit Hilfe von
        |foldNat| (2pt):

\begin{code}
isEven :: Nat -> Bool
isEven n = (foldNat 0 (+1) n) `mod` 2 == 0

isZero :: Nat -> Bool
isZero n = (foldNat 0 (+1) n) == 0
\end{code}

\item{Vervollständigen Sie die Definition der Implikation: (1pt)

\begin{code}
implies :: Bool -> Bool -> Bool
True  `implies` x  = x
False `implies` _  = True
\end{code}


\item Beweisen Sie, dass |Jane| und |Dick| verschiedene Typen sind, indem Sie zeigen, dass
      |j2j| nicht die identische Funktion auf |Jane| ist! Definieren Sie dazu eine Funktion
      |test :: Jane -> Bool| und geben Sie |t :: Jane| so an, dass |test| sich auf |t|
      anders verhält als auf |j2j t|.(1pt)

\begin{code}
data    Jane   = MkJane Int deriving Show
newtype Dick   = MkDick Int deriving Show

j2d :: Jane -> Dick
j2d (MkJane x) = (MkDick x)

d2j :: Dick -> Jane
d2j (MkDick x) = (MkJane x)

j2j :: Jane -> Jane
j2j            = d2j . j2d
\end{code}
\end{enumerate}


\subsection{Strictness}
\begin{enumerate}
\addtocounter{enumi}{5}
\item Im Seminar hatten wir die boolschen Operatoren |and| und |or| strikt in einer, aber
      nicht-strikt in der anderen Komponente definiert. Kann man |and| und |or| auch strikt
      in beiden Argumenten definieren? Falls ja, implementieren Sie diese, falls nein,
      begründen Sie!(1pt)

\begin{code}
and' :: Bool -> Bool -> Bool
and' x y
  | x == y = True
  | otherwise = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' True False = True
or' _ _ = True
\end{code}

\item Kann man eine nicht-strikte Funktion |not'| definieren, die sich auf True und False wie
      |not| verhält? Begründen Sie! (1pt)

|not'| müsste für einen Fall(True oder False) strikt sein kann es aber nicht da es nur ein Argument hat.
\end{enumerate}

\subsection{Tupels und Either}

Wir hatten die folgenden Gesetze:\\
1)   |fst . pair (f, g)        =  f|\\
2)   |snd . pair (f, g)        =  g|\\
3)   |pair (f, g) . h          =  pair (f.h,g.h)|\\
4)   |cross (f,g) . pair (h,k) =  pair (f.h,g.k)|\\
i)   |scase (f,g) . Left       = f|\\
ii)  |scase (f,g) . Right      = g|\\
iii) |h . scase (f,g)          = scase (h.f,h.g)|\\
iv)  |scase (f,g) . plus(h,k)  = scase (f.h,g.k)|

\begin{enumerate}
\addtocounter{enumi}{7}

\item  Beweisen Sie iv) unter Benutzung von i)-iii) ( analog zum Beweis von 4) im letzten
       Seminar).(1pt)

scase (f,g) . plus(h,k)
= { i) && ii) }
scase(f,g) (. Left . h) (. Right . k)
= {}
scase(f.h, g.k)

\item Beweisen Sie |cross(f,g) . cross(h,k) = cross (f.h,g.k)|. (2pt)

cross(f,g) . cross(h,k)
= {1) und 2) }
cross(f.fst, g.snd) . cross(h,k)
= { 3) }
cross(f.fst.pair(h,k), g.snd.pair(h,k))
=
cross (f.h, g.k)

\item Schreiben Sie eine Funktion |testEither :: Either Bool Char -> a| in einen geeigneten
      Typen |a|, die sich auf |Left undefined|, |Right undefined| und |undefined|
      unterschiedlich verhält! (1pt)

Gibt bei Left undefined l | Right undefined r | undefined undefined aus.
\begin{code}
testEither :: Either Bool Char -> Char
testEither (Left _) = 'l'
testEither (Right _) = 'r'
\end{code}

\item Ein Datum kann durch ein Tripel (Jahr,Monat,Tag) angegeben werden. Wir definieren das
      Typsynonym

\begin{code}
type Date = (Int,Int,Int)
\end{code}

      Implementieren Sie eine Funktion |age:: Date -> Date -> Int| so, dass
      |age birthday current| das Alter einer Person angibt, wenn |birthday| der Geburtstag
      der Person und |current| das aktuelle Datum ist. (2pt)

\begin{code}
age :: Date -> Date -> Int
age (by,bm,bd) (cy,cm,cd)
  | by <= cy && bm <= cm = cy - by
  | by <= cy = cy - by - 1
  | otherwise = error("Younger then expected.")
\end{code}

\end{enumerate}

\subsection{Fibonacci}

\begin{enumerate}
\addtocounter{enumi}{11}

\item Implementieren Sie eine Funktion |fib :: Int -> Integer|, sodass |(fib n)| das
      $n$-te Glied der Fibonacci-Folge ist: es sei |fib 0 = 0|, |fib 1 = 1| und für alle
      endlichen (auch negativen!) |n::Int| gelte\\ |fib n = fib (n-1) + fib (n-2)| . (2pt)

\begin{code}
fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n
  | n < 0 = fib (abs n)
  | otherwise = fib (n-1) + fib (n-2)
\end{code}

\end{enumerate}
\end{document}
