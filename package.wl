(* ::Package:: *)

(* :Title : ColorTheory *)
(* :Context : ColorTheory` *)
(* :Author : GS *)
(* :Summary : a function with a local variable that has global scope *)
(* :Copyright : GS 2021 *)
(* :Package Version : 2 *)
(* :Mathematica Version : 12.2 *)
(* :History : Versions 1, April 2021 *)
(* :Keywords : scope , nesting , bad programming style *)
(* :Sources : book / link *)
(* :Warnings : as the name implies , this is an example of bad programming *)
(* :Discussion : *)
(* :Requirements : *)
(* This function returns the sum of the first n powers of x *)
BeginPackage["ColorTheory`"]


IttenWheel::usage= "IttenWheel[] stampa la ruota dei colori di Itten, con colori primari, secondari e 12 colori terziari"

Begin["`Private`"]




(*colori primari e secondari necessari alla creazione del cerchio di itten (parte interna).
Il giallo \[EGrave] ripetuto al fine dei ottenere tutte le sfumature necessarie applicando , in seguito, la funzione Blend*)
secondariWheel={Yellow, Darker[Green], Blue, Purple, Red, Orange, Yellow};
(*creazione dell'esagono interno del cercio dei colori di Itten.*)
InnerGeometry[]:=
	Module[ {o,a,b,c,d,e,h,p,q,r,ptsY,ptsb,ptsR,ptsO,ptsG,ptsV,ptsB},
	(*definizione dei punti che servono a creare i sei settori all'interno dell'esagono*)
		o={0,0};
		a={-Sqrt[3]/4,-1/4};
		b={Sqrt[3]/4,-1/4};
		c={0,1/2};
		d={-Sqrt[3]/8,1/8};
		e={Sqrt[3]/8,1/8};
		h={0,-1/4};
		p={0,-1/2};
		q={-Sqrt[3]/4,1/4};
		r={Sqrt[3]/4,1/4};
		
		ptsY={o,e, c, d};
		ptsB={o, d, a,h };
		ptsR={o,e,b,h};
		ptsO={c,r,b};
		ptsG={c,q,a};
		ptsV={a,b,p};
		
		(*chiama la funzione ausialiare SectorWheel per ognuno dei sei colori da rappresentare.*)
		Return[
			MapThread[SectorWheel,{{ptsY, ptsO, ptsR, ptsV, ptsB, ptsG}, (*punti da unire*)
									Range[1,11,2], (*generazione delle etichette dei colori come numeri interi*)
									Drop[Reverse[secondariWheel],-1] (*generazione dei colori nel giusto ordine, 
																	a partire dalla lista dei colori gi\[AGrave] esistente:
																	si rovescia la lista e si toglie l'ultimo elemento che 
																	consiste in una ripetizione del giallo*)
								}]
		]
	]

(*Funzione ausiliare. Dati: una lista di punti, una etichetta e un colore la funzione crea un poligono 
colorato al cui centro viene sovrapposta la sua etichetta (testo). 
Resituisce una lisa.*)
SectorWheel[pts_,txt_,color_]:=
	{Style[Polygon[pts],color], (*creazione del poligono*)
	Style[Text[txt, RegionCentroid[Polygon[pts]]], (*aggiunta del testo nel centro del poligono*)
	Bold,  FontSize->14 (*stile del testo*)
	]}
	
(*Funzione che crea l'anello esterno del cercio dei colori di itten (con 12 colori).
Input: lista di colori , lista di etichette, raggio interno, raggio esterno dell'anello*)
OuterGeometry[colorList_,lbl_,r1_:0.5,r2_:0.8] := 
	Join[Table[{
		Style[
		(*la funzione Annulus crea un singolo settore dell'anello.*)
					(*centro, raggio interno e esterno, angolo iniziale e finale*)
			Annulus[{0,0},{r1,r2},{i-1/12 Pi+Pi/2,i+2/12*Pi-1/12Pi+Pi/2}],
			colorList[[i/(2/12*Pi)+1]]], (*colore del settore*)
		Style[
			Text[lbl[[i/(2/12*Pi)+1]], (*etichetta*)
			RegionCentroid[Annulus[{0,0},{r1,r2},{i-1/12 Pi+Pi/2,i+2/12*Pi-1/12Pi+Pi/2}]]], (*centro dell'area*)
			FontSize->14, Bold (*stile del testo*)
		]},
		{i,0,2*Pi-2/12*Pi,2/12*Pi}]];


(*funzione che crea il cerchio di Itten*)
IttenWheel[]:=
	Module[{terziariWheel,coloriEstesi,lbls},
		(*creazione dei colori terziari del cerchio tramite la unione delle coppie di colori primari e secondari*)
		terziariWheel=Map[Blend, Partition[secondariWheel,2,1]]; (*Partiton crea una lista di coppie di colori *)
		(*creazione della lista dei 12 colori del cerchio di Itten,
		 alternando la lista dei secondari estesi con quella dei terziari*)
		coloriEstesi=Riffle[Drop[secondariWheel,-1],terziariWheel];
		lbls=Reverse[RotateLeft[ Range[12],1]]; (*creazione delle etichette/numerazione dei colori*)
		Graphics[{InnerGeometry[], OuterGeometry[coloriEstesi,lbls]}] (*creazione oggetto grafico 
																	che unisce la parte interna e l'anello esterno
																	del cerchio dei colori*)
	]



End[]
(* inserire protect *)
EndPackage[]
