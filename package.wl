(* ::Package:: *)

(* :Title:Teoria dei colori*)
(* :Context:EsercizioColori`*)
(* :Author:Ghidoni, Di Ubaldo, Valenza, Pes*)(* :Summary:Package per la definizione e la risoluzione della classe di esercizi riguardo colori. La classe di esercizi presa in esame riguarda il riconoscimento di colori primari, secondari, terziari e complementari(?).*)
(* :Copyright:*)
(* :Package Version:1.0*)
(* :Mathematica Version:12.2*)
(* :Keywords:cerchio cromatico di Itten, gradiente(?), colori primari, secondari, terziari e complementari(?)*)
(* :Sources:https://reference.wolfram.com/
	,https://community.wolfram.com/
	https://mathematica.stackexchange.com/*)

BeginPackage["ColorTheory`"]

ShowExercise1::usage = "ShowExercise[] restituisce il primo esercizio all'utente"

Begin["`Private`"]

ShowExercise1[] := DynamicModule[
	{risposta},
		Column[{
			RandomChoice[coloriGiusti],
			InputField[Dynamic[risposta], Number, FieldSize->10],
			Dynamic@If[risposta == 2, "giusto!", "sbagliato"]
		}]
]

primari = {Yellow, Blue, Red}
secondari = {Darker[Green], Purple, Orange}
secondari2 = {Yellow, Darker[Green], Blue, Purple, Red, Orange, Yellow}
terziari = Map[Blend, Partition[secondari2, 2, 1]]
coloriGiusti = RotateRight[Riffle[Drop[secondari2,-1],terz],2]

End[]
(* inserire protect *)
EndPackage[]
