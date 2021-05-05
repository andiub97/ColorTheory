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
			InputField[Dynamic[risposta], Number, FieldSize->10, ContinuousAction->True],
			Dynamic@If[risposta == 1, "giusto!", "sbagliato"]
		}]
]
End[]
(* inserire protect *)
EndPackage[]
