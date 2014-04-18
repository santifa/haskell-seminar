% ----------------------------------------------------------------------
% --
% --  Seminar Theoretische Informatik
% --  "Funktionale Programmierung mit Haskell"
% --  Universität Potsdam SS 2014
% --  Tim Richter / Mario Frank
% --
% ----------------------------------------------------------------------
%
\documentclass{article}
%include lhs2TeX.fmt
%include polycode.fmt
\usepackage[ngerman]{babel}
\usepackage{ucs}
\usepackage[utf8x]{inputenc}
\usepackage{fullpage}
\setlength{\parindent}{0pt}
\begin{document}

-------------------------------------------------------------------------------
\section{Natürliche Zahlen}

Wir wollen die natürlichen Zahlen, also die positiven ganzen Zahlen inklusive
der Null definieren.
Dazu führen wir erst einmal einen Modulnamen ein und importieren die
Prelude-Umgebung.

\begin{code}
module Uebung where
import Prelude
\end{code}

Jetzt können wir den Datentypen Nat deklarieren.

\begin{code}
data Nat = O | S Nat
         deriving Show
\end{code}

Zur Vereinfachung führen wir eine Transformationsfunktion von Int nach Nat ein.

\begin{code}
intToNat :: Int -> Nat
intToNat 0 = O
intToNat n = S ( intToNat (n-1) )
\end{code}

Auch eine Transformation von Nat nach Int sollte nicht fehlen.

\begin{code}
natToInt :: Nat -> Int
natToInt O = 0
natToInt (S n) = 1 + ( natToInt n )
\end{code}

%\begin{code}
%showNat :: Nat -> String
%showNat O = "O"
%showNat(S O) = "S O"
%showNat(S(S n)) = "S ("++ showNat(S n)++")"
%\end{code}

-------------------------------------------------------------------------------
\subsection{Addition}

Vervollständigen Sie die folgende Definition der Addition add für Nat.

\begin{code}
add :: Nat -> Nat -> Nat
add m O = m
add m (S n) = add (S m) n
\end{code}

\begin{code}
test1 = "add (intToNat 3) (intToNat 4) = " ++ show (add (intToNat 3) (intToNat 4))
\end{code}

-------------------------------------------------------------------------------
\subsection{Multiplikation}

Implementieren Sie die Multiplikation multiply für Nat.

\begin{code}
multiply :: Nat -> Nat -> Nat
multiply m (S n) = mul O m n

mul :: Nat -> Nat -> Nat -> Nat
mul x m O = add x m
mul x m (S n) = mul (add x m) m n
\end{code}
Die zweite Funktion mul könnte überflüssig werden

\begin{code}
test2 = "multiply (intToNat 8) (intToNat 256) = " ++
        show (natToInt ( multiply (intToNat 8) (intToNat 256)))
\end{code}

-------------------------------------------------------------------------------
\subsection{Gleichheit von Nat}

Definieren Sie die Gleichheit equals von Elementen aus Nat.
Geben Sie auch die Typdeklaration an!

\begin{code}
equals :: Nat -> Nat -> Bool
equals O O = True
equals _ O = False
equals O _ = False
equals (S m) (S n) = equals m n
\end{code}

\begin{code}
test3 = "equals (intToNat 8) (intToNat 256) = " ++
        show ( equals (intToNat 8) (intToNat 256))
test4 = "equals (intToNat 32) (intToNat 32) = " ++
        show ( equals (intToNat 32) (intToNat 32))
\end{code}

-------------------------------------------------------------------------------
\subsection{Ordnungsrelation}

Definieren Sie die Ordnungsrelation lessThen für Nat.

\begin{code}
lessThen :: Nat -> Nat -> Bool
lessThen O O = False
lessThen O _ = True
lessThen _ O = False
lessThen (S m) (S n) = lessThen m n

\end{code}

\begin{code}
test5 = "lessThen (intToNat 8) (intToNat 256) = " ++
        show ( lessThen (intToNat 8) (intToNat 256))
test6 = "lessThen (intToNat 32) (intToNat 32) = " ++
        show ( lessThen (intToNat 32) (intToNat 32))
\end{code}

-------------------------------------------------------------------------------
\section{Listen von Nat}

Die natürlichen Zahlen sind eine schöne Sache. Aber was können wir damit machen?
Wir können damit beispielsweise Listen erzeugen.

-------------------------------------------------------------------------------
\subsection{Länge von Listen}

Definieren sie die Länge lengthOf einer Liste von Nat wobei das Ergebnis vom
Typ Nat sein soll.

\begin{code}
instance Eq Nat where
  x == y = x `equals` y

lengthOf :: [Nat] -> Nat
lengthOf list = len list O

len :: [Nat] -> Nat -> Nat
len list length = if list == []
           then length
           else let t = tail list
                in len t (S length)
\end{code}


\begin{code}
test8 = "lengthOf ([O,O,O,S(S(S(O))),O,S(S(O))] = " ++
        show ( lengthOf ([O,O,O,S(S(S(O))),O,S(S(O))]))
\end{code}

-------------------------------------------------------------------------------
\subsection{Elementzugriff bei Listen}

Definieren Sie eine Funktion get, die das n-te Element aus einer Liste
von Nat zurückgibt. Der Index soll dabei vom Typ Int sein und das erste
Listenelement soll den Index 0 haben.

\begin{code}
get :: [Nat] -> Int -> Nat
get [] _ = undefined
get list 1 = head list
get list index = get (tail list) (index-1)

\end{code}

\begin{code}
test9 = "get ([O,O,O,S(S(S(O))),O,S(S(O))]) 3 = " ++
        show ( get ([O,O,O,S(S(S(O))),O,S(S(O))]) 4)
\end{code}

-------------------------------------------------------------------------------
\section{Tests}

\begin{code}
test = sequence_ (map putStrLn [
        test1,
        test2,
        test3,
        test4,
        test5,
        test6,
        test8,
        test9])
\end{code}
\end{document}
