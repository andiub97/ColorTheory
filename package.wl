(* ::Package:: *)

(* DA MODIFICARE *)

(*:Title:ColorTheory*)
(*:Context:ColorTheory`*)
(*:Author: ... *)
(*:Summary: ... *)
(*:Copyright: ... *)
(*:Package Version: ... *)
(*:Mathematica Version:12.2*)
(*:History: ... *)
(*:Sources: ... *)
(*:Limitations: ...*)
(*:Discussion:USES LINE*)


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




