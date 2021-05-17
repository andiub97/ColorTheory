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


IttenWheel::usage= "IttenWheel[] stampa la ruota dei colori di Itten, con colori primari, secondari e 12 colori terziari. Opzioni: 'Mouseover'-> True | False. Valore di default \[EGrave] False, e crea un cerchio statico.Specificando l'opzione a True verr\[AGrave] creato un cercio interattivo che mostra il tipo di colore se il mouse viene portato sui settori.
					"
ShowExercise1::usage = "ShowExercise[] restituisce il primo esercizio all'utente"

Begin["`Private`"]


primari = {Yellow, Blue, Red}
secondari = {Darker[Green], Purple, Orange}
secondari2 = {Yellow, Darker[Green], Blue, Purple, Red, Orange, Yellow}
terziari = Map[Blend, Partition[secondari2, 2, 1]]
coloriGiusti = RotateRight[Riffle[Drop[secondari2,-1],terziari],2]



CheckAnswer[risposta_, colore_] := 
  (risposta == "Primario" && MemberQ[primari, colore]) ||(risposta == "Secondario" && MemberQ[secondari, colore]) ||
(risposta == "Terziario" && MemberQ[terziari, colore]);

ShowAnswer[colore_] :=
Which[MemberQ[primari, colore], "Primario", MemberQ[secondari, colore], "Secondario", MemberQ[terziari, colore], "Terziario"];

ShowExercise1[] := DynamicModule[
	{ risposta,
	coloreRandom,
	messaggioUtente,
	buttonPrimarioColor,
	buttonSecondarioColor,
	buttonTerziarioColor,
	cleanImage,
	showhide,
	i, toshow, listaOggetti},
	messaggioUtente = "";
	risposta = "";
	coloreRandom = RandomChoice[coloriGiusti];
	buttonPrimarioColor = Null;
	buttonSecondarioColor = Null;
	buttonTerziarioColor = Null;
	cleanImage = Import[FileNameJoin[{NotebookDirectory[],"clean.png"}], ImageSize -> 30];
	(*inizializzazioni per bottone che mostra e nasconde il grafico*)
	showhide=Transpose@{#,ToExpression@#}&@{"Nascondi il cerchio di Itten","Mostra il cerchio di Itten"};
	i=1;
	listaOggetti={Graphics[ImageSize->{350,400}], Show[IttenWheel[ImageSize->Medium], ImageSize->{350,400},ImageMargins->{{100,100},{2,2}}]};
	toshow=listaOggetti[[1]];
	
	Row[{
		Row[{
			(*quadrato colorato*)
			Row[{
				Column[{
					Dynamic@Which[
						risposta == "", Style[messaggioUtente,FontSize->40, White], 
						CheckAnswer[risposta, coloreRandom], Style["Corretto!", FontSize->40, White ], 
						True, Style["Sbagliato", FontSize-> 40, White]],
				Row[{
					Button["Primario", risposta = "Primario", Background-> buttonPrimarioColor],
					Button["Secondario", risposta = "Secondario", Background-> buttonSecondarioColor],
					Button["Terziario", risposta = "Terziario", Background ->buttonTerziarioColor]
					}
				]}, Spacings->10
			] },
			(*setting del quadrato colorato*)
           Background-> Dynamic@coloreRandom , ImageSize->{450, 450},
           Alignment->Center, Frame -> True, FrameMargins->100],
           Column[{
			Button[cleanImage,risposta = ""; messaggioUtente = ""],
			Button["Nuovo esercizio", coloreRandom = RandomChoice[coloriGiusti]; risposta = ""; messaggioUtente = ""],
			Button["Mostra risposta", risposta = ""; messaggioUtente =ShowAnswer[coloreRandom]]
		}]
		(*setting area grafica che contiene l'esercizio*)
		},Alignment->Right, ImageSize-> {600, 600}, Background-> RGBColor["#f2f7ff"]],
		
		
		Row[{
			
			Column[{
				Button[Dynamic@ToString@showhide[[1+Mod[i,2],1]],
					toshow=listaOggetti[[1+Mod[i++,2]]],
					ImageSize->150, ImageMargins->{{200,200},{2,2}}, FrameMargins->Large, Alignment->Center],
				Dynamic@Show[toshow] 
			}]
		}, ImageSize-> {600, 600}, Background-> RGBColor["#f2f7ff"], FrameMargins->100
		]
			
		}
		]

	];



(*colori primari e secondari necessari alla creazione del cerchio di itten (parte interna).
Il giallo \[EGrave] ripetuto al fine dei ottenere tutte le sfumature necessarie applicando , in seguito, la funzione Blend*)
secondariWheel={Yellow, Darker[Green], Blue, Purple, Red, Orange, Yellow};
(*creazione dell'esagono interno del cercio dei colori di Itten.*)
InnerGeometry[over_:False]:=
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
		(*Punti aree colori primari e secondari*)
		ptsY={o,e, c, d};
		ptsB={o, d, a,h };
		ptsR={o,e,b,h};
		ptsO={c,r,b};
		ptsG={c,q,a};
		ptsV={a,b,p};
		
		If[
			over,
			Return[
				MapThread[SectorWheelOver,{{ptsY, ptsO, ptsR, ptsV, ptsB, ptsG}, (*punti da unire*)
									Range[1,11,2], (*generazione delle etichette dei colori come numeri interi*)
									
									(*Moltiplico la lista {"Primario","Secondario"} per una lista contenente tre 1,
									in modo da creare tre liste uguali alla coppia di stringhe. Le unisco con Join per
									ottenere un'alternanza di stringhe lunga 6.
									Sono le etichette da mostrare in caso di Mouseover*)
									Join@@{#}[[ConstantArray[1,3]]]&[{"Primario","Secondario"}],
									Drop[Reverse[secondariWheel],-1] (*generazione dei colori nel giusto ordine, 
																	a partire dalla lista dei colori gi\[AGrave] esistente:
																	si rovescia la lista e si toglie l'ultimo elemento che 
																	consiste in una ripetizione del giallo*)
								}]
			],
		(*caso statico*)
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
	]

(*Funzione ausiliare. Dati: una lista di punti, una etichetta e un colore la funzione crea un poligono 
colorato al cui centro viene sovrapposta la sua etichetta (testo). 
Resituisce una lisa.*)
SectorWheel[pts_,txt_,color_]:=
	{Style[Polygon[pts],color], (*creazione del poligono*)
	Style[Text[txt, RegionCentroid[Polygon[pts]]], (*aggiunta del testo nel centro del poligono*)
	Bold,  FontSize->18 (*stile del testo*)
	]}
	
(*Funzione ausiliare. Dati: una lista di punti, una etichetta e un colore la funzione crea un poligono 
colorato al cui centro viene sovrapposta la sua etichetta (testo) che cambia in seguito ad evento mouseover*)
SectorWheelOver[pts_,txt_, txtOver_,color_]:=
	Graphics[
	{
	(*Mouseover gestisce l'evento che si attiva quando il mouse passa sopra alla regione geometrica*)
		Mouseover[
		(*oggetto originale, che viene mostrato se il muose non si trova sopra l'oggetto*)
			{Style[Polygon[pts],color], (*creazione del poligono*)
			Style[Text[txt, RegionCentroid[Polygon[pts]]], (*aggiunta del testo nel centro del poligono*)
			Bold,  FontSize->18]},(*stile del testo*)
			
		(*oggetto modificato che viene mostrato quando il muose si trova sopra l'oggetto*)
			{Style[Polygon[pts],color], (*creazione del poligono*)
			Style[Text[txtOver, RegionCentroid[Polygon[pts]]], (*aggiunta del testo nel centro del poligono*)
			Bold, FontSize->14]}
		]
	}
	]
	
	
	
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
			FontSize->18, Bold (*stile del testo*)
		]},
		{i,0,2*Pi-2/12*Pi,2/12*Pi}]];
		
		
(*Funzione che crea l'anello esterno del cercio dei colori di itten (con 12 colori).
Input: lista di colori , lista di etichette (che cambiano con mouseover), raggio interno, raggio esterno dell'anello*)
OuterGeometryOver[colorList_,lbl_,r1_:0.5,r2_:0.8] := 
	Table[
		Graphics[{
			Mouseover[
				{Style[
				(*la funzione Annulus crea un singolo settore dell'anello.*)
						(*centro, raggio interno e esterno, angolo iniziale e finale*)
					Annulus[{0,0},{r1,r2},{i-1/12 Pi+Pi/2,i+2/12*Pi-1/12Pi+Pi/2}],
					colorList[[i/(2/12*Pi)+1]]], (*colore del settore*)
				Style[
					Text[lbl[[i/(2/12*Pi)+1]], (*etichetta*)
					RegionCentroid[Annulus[{0,0},{r1,r2},{i-1/12 Pi+Pi/2,i+2/12*Pi-1/12Pi+Pi/2}]]], (*centro dell'area*)
					FontSize->18, Bold ]},(*stile del testo*)
					
				{Style[
				(*la funzione Annulus crea un singolo settore dell'anello.*)
						(*centro, raggio interno e esterno, angolo iniziale e finale*)
					Annulus[{0,0},{r1,r2},{i-1/12 Pi+Pi/2,i+2/12*Pi-1/12Pi+Pi/2}],
					colorList[[i/(2/12*Pi)+1]]], (*colore del settore*)
				Style[
					Text["Terziario", (*etichetta*)
					RegionCentroid[Annulus[{0,0},{r1,r2},{i-1/12 Pi+Pi/2,i+2/12*Pi-1/12Pi+Pi/2}]]], (*centro dell'area*)
					FontSize->14, Bold]}(*stile del testo*)
			]
		}],
		{i,0,2*Pi-2/12*Pi,2/12*Pi}];



(*funzione che crea il cerchio di Itten*)
(*mouseover \[Rule] Se True crea un cerchio interattivo al mouseover, se False crea un cerchio statico.
Valore di default= False*)
(*ImageSize \[Rule] valori accettati:Medium,Large, default = Large. Definisce la dimensione
del cerchio di Itten che verr\[AGrave] visualizzato*)

IttenWheel::badBool = "`1` non \[EGrave] un valore valido per l'opzione Mouseover (valori accettati: True, False)";
IttenWheel::badSize = "`1` non \[EGrave] un valore valido per l'opzione ImageSize (valori accettati: Medium, Large)";

Options[IttenWheel]={Mouseover->False, ImageSize->Large}
(*mouseover:True|False:False*)
IttenWheel[OptionsPattern[]]:=
	Module[{terziariWheel,coloriEstesi,lbls},
		(*se non \[EGrave] stato passato un valore che non \[EGrave] True o False, si d\[AGrave] errore. Altrimenti si crea il cerchio*)		
		If[
		SameQ[OptionValue[Mouseover],True]|| SameQ[OptionValue[Mouseover],False] && 
		MemberQ[{Medium, Large}, OptionValue[ImageSize]],

			(*creazione dei colori terziari del cerchio tramite la unione delle coppie di colori primari e secondari*)
			terziariWheel=Map[Blend, Partition[secondariWheel,2,1]]; (*Partiton crea una lista di coppie di colori *)
			(*creazione della lista dei 12 colori del cerchio di Itten,
			alternando la lista dei secondari estesi con quella dei terziari*)
			coloriEstesi=Riffle[Drop[secondariWheel,-1],terziariWheel];
			lbls=Reverse[RotateLeft[ Range[12],1]]; (*creazione delle etichette/numerazione dei colori*)
			If[
				OptionValue[Mouseover],
				(*caso interattivo*)
				Show[InnerGeometry[OptionValue[Mouseover]],OuterGeometryOver[coloriEstesi,lbls], ImageSize->OptionValue[ImageSize]],
				(*caso statico*)
				Graphics[{InnerGeometry[], OuterGeometry[coloriEstesi,lbls]}, ImageSize->OptionValue[ImageSize]] (*creazione oggetto grafico 
																che unisce la parte interna e l'anello esternodel cerchio dei colori*)
			],													
	(*messaggi di errore in caso di valore non accettabile per le options*)
			If[ !SameQ[OptionValue[Mouseover],True]&& !SameQ[OptionValue[Mouseover],False] ,
				Message[IttenWheel::badBool, OptionValue[Mouseover]] ];
			If[ !MemberQ[{Medium, Large}, OptionValue[ImageSize]],
				Message[IttenWheel::badSize, OptionValue[ImageSize]] ];
		]
		
	
	]






End[]
(* inserire protect *)
EndPackage[]
