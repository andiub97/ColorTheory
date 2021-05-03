(* ::Package:: *)

(* ::Package::*)
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

BeginPackage["Colori`"]
CartesianMap::usage="CartesianMap[f,{x0,x1,dx},{y0,y1,dy}] plots the image of the Cartesian coordinate lines under the function f ."
Begin["`Private`"]
