\documentclass{article}
%include lhs2TeX.fmt
%include polycode.fmt
\usepackage[ngerman]{babel}
\usepackage{ucs}
\usepackage[utf8x]{inputenc}
\usepackage{fullpage}
\usepackage{url}
\begin{document}
\begin{verbatim}
----------------------------------------------------------------------
--
--  Seminar Theoretische Informatik                         \ \
--  "Funktionale Programmierung mit Haskell"                 \ \ ---
--  Universität Potsdam SS 2014                              / /\ --
--  Tim Richter / Mario Frank                               / /  \
--
-- Übungsblatt 3 (Listen)
-- Abgabe bis: 28.05.2014, 23:59
-- Name:
-- Erreichte Punkte:      / 14
----------------------------------------------------------------------
\end{verbatim}

\begin{code}
module Uebung3 where
import Prelude

data Nat = O | S Nat deriving (Show)

intToNat	:: Int -> Nat
intToNat n
	| (n < 0)   = undefined
	| (n == 0)  = O
	| otherwise = S ( intToNat (n-1) )

natToInt	:: Nat -> Int
natToInt O	= 0
natToInt (S n)	= 1 + ( natToInt n )
\end{code}

\section{Vorbemerkung}
Beweise sind stets im Stil des ''equational reasoning'', wie es im Buch
eingeführt wird, zu führen. Insbesondere ist für \textbf{jede} Gleichung
in geschweiften Klammern hinter dem Gleichheitszeichen eine Begründung anzugeben.
Abweichung von dieser Form führt zu Punktverlust!

\section{Listenoperationen (1+2+2+2 pt)}

\subsection{Last}
Gegeben seien die folgenden Definitionen für die Funktion,
die das letzte Element einer Liste zurückgibt:

\begin{code}
last' (x:xs) = if null xs then x else last' xs
last'' (x:xs) = if xs == [] then x else last'' xs
\end{code}

Der Unterschied zwischen |last'| und |last''| besteht darin, dass sich die
Typen unterscheiden.

\begin{code}
last' :: [a] -> a
last'' :: (Eq a) => [a] -> a
\end{code}

Die Typ der Funktion |last''| beschränkt die Eingabe auf Listen von
Elementen, die zum Typ |Eq| gehören.

Geben Sie einen Ausdruck |n| an, auf dem sich |last'| und |last''| unterschiedlich
verhalten. (1 pt)

\subsection{Verkettung}
Beweisen Sie, dass folgende Gleichung gilt: (2 pt)

\textbf{concat (xss ++ yss) = concat xss ++ concat yss}\\

\noindent concat ist dabei als |foldr (++) []| und (++) durch Rekursion über das erste 
Argument definiert (siehe Prelude). Schreiben Sie diese Definitionen aus und 
geben Sie den einzelnen Regeln Label, um im Beweis auf sie referenzieren zu können!\\

\subsection{Halbieren einer Liste}
Definieren Sie eine Funktion 
\begin{code}
splitHalf :: [a] -> (([a],[a]), Maybe a)
splitHalf = undefined
\end{code}

\noindent die eine Liste ''in der Mitte teilt'': Ist |splitHalf as = ((bs,cs), m)|, so sollen die
Listen bs und cs gleich lang sein und es soll, falls |m = Just a| ist, |as = bs ++ [a] ++ cs| und 
sonst |as = bs ++ cs| gelten. (2 pt)

\subsection{Bedingtes Umkehren einer Liste}
Definieren Sie eine Funktion 
\begin{code}
revEven :: [a] -> [a]
revEven = undefined
\end{code}

\noindent die alle Listen gerader Länge umkehrt und alle anderen Listen unverändert lässt. Benutzen 
Sie |splitHalf| und ''pattern guards'' (siehe \url{http://www.haskell.org/haskellwiki/Pattern_guard}).
Verwenden Sie weder |where| noch |let|! (2 pt)

\section{Map und Filter (1+1+2 pt)}

\subsection{Map}

\begin{enumerate}
 \item Geben Sie den Typen von map map an! (1 pt)
 \item Beweisen Sie, dass folgende Gleichungen gelten:\\
\textbf{map f (xs ++ ys) = map f xs ++ map f ys} (1 pt)\\
\textbf{map f . concat = concat . map (map f)} (2 pt)\\
        
\noindent Führen Sie den Beweis der zweiten Gleichung, indem Sie unter Benutzung 
der ersten Gleichung und geeigneter Gesetze zeigen, dass beide Seiten 
sich als dieselbe |foldr|-Instanz schreiben lassen.
\end{enumerate}

\subsection{Filter (1 pt)}

\noindent Die Prelude-Funktion |filter :: (a -> Bool) -> [a] -> [a]| kann mit Hilfe von 
concat und map konstruiert werden:

\begin{code}
filter' = concat . map box
		where box x = undefined
\end{code}

\noindent Vervollständigen Sie die Definition für |box|. (1 pt)

\section{Folds (3 pt)}
\noindent Die Funktionen |takeWhile| und |dropWhile| sind den Funktionen take und drop ähnlich. 
Statt eines Int-Indexes nehmen sie jedoch einen Boole'schen Ausdruck als
ersten Parameter. Das Ergebnis von

\textbf{takeWhile p xs}

\noindent
ist das längste Initial-Segment von |xs|, dessen Elemente sämtlich die Bedingung 
|p| erfüllen. Zum Beispiel ist

\textbf{takeWhile even [2,4,7,4,1,6] = [2,4]}

\begin{enumerate}
	\item{Definieren Sie |filter p| als Instanz von |foldr| (1 pt)}
	\item{Definieren Sie |takeWhile| als Instanz von |foldr| (1 pt)}
	\item{Kann |dropWhile| als Instanz einer Fold-Funktion definiert werden? 
        Implementieren oder begründen Sie! (1 pt)}
\end{enumerate}

\section{Hyperoperationen (just for fun)}

\noindent
Ausgehend von der Beobachtung, dass Multiplikation ''iterierte Addition''
und Potenzieren ''iteriertes Multiplizieren'' ist, definiert man
die Folge der \textbf{Hyperoperation} (siehe Wikipedia: \url{http://en.wikipedia.org/wiki/Hyperoperation}). 
Definieren Sie unter Verwendung von |foldr1| und |foldNat| 

\begin{code}
foldNat :: a -> (a -> a) -> Nat -> a
foldNat s f O     = s
foldNat s f (S m) = f $ foldNat s f m
\end{code}

\noindent eine Funktion

\begin{code}
hop :: Nat -> Integer -> Integer -> Integer
hop = undefined
\end{code}

\noindent derart, dass | a `hop n` b | für |b >= 1| das Ergebnis der Anwendung der (n+1)ten (sic!)
Hyperoperation auf das Paar |(a,b)| ist! Für |b < 1| darf die Funktion undefiniert sein.

\end{document}
