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
0) Introduction
1) Vorüberlegung
2)
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
< printExpr :: String -> String
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


2) Pfannen, Töpfe und Zutaten?
-------------------------------------------------------------------------------


3) Abschmecken
-------------------------------------------------------------------------------

4) Genießen
-------------------------------------------------------------------------------
