(* ::Package:: *)

(* :Title:Teoria dei colori*)
(* :Context:EsercizioColori`*)
(* :Author:Ghidoni, Di Ubaldo, Valenza, Pes*)(* :Summary:Package per la definizione e la risoluzione della classe di esercizi riguardo colori. La classe di esercizi presa in esame riguarda il riconoscimento di colori primari, secondari, terziari e complementari(?).*)
(* :Copyright:*)
(* :Package Version:1.0*)
(* :Mathematica Version:12.2*)
(* :Keywords:cerchio cromatico di Itten, colori primari, secondari, terziari e complementari(?)*)
(* :Sources:https://reference.wolfram.com/
	,https://community.wolfram.com/
	https://mathematica.stackexchange.com/*)

BeginPackage["ColorTheory`"]


IttenWheel::usage= "IttenWheel[] stampa la ruota dei colori di Itten, con colori primari, secondari e 12 colori terziari. Opzioni: 'Mouseover'-> True | False. Valore di default \[EGrave] False, e crea un cerchio statico.Specificando l'opzione a True verr\[AGrave] creato un cercio interattivo che mostra il tipo di colore se il mouse viene portato sui settori.
					"
ShowExercise1::usage = "ShowExercise[] restituisce il primo esercizio all'utente (identificazione delle classi dei colori)"
Paintings::usage = "Paintings[] crea un'area grafica contenente, in una riga, la parte interna del cerchio di Itten (colori primari e secondari). I settori colorati possono essere cliccati per visualizzare dei famosi dipinti di quel colore. Con un secondo click sullo stesso colore si nascondono le immagini. Posizionando il mouse sull'immagine verr\[AGrave] mostrato titolo e autore tramite tooltip"

ShowExercise2::usage = "Restituisce il secondo esercizio: riordinamento dei 6 colori primari e secondari nella successione dei colori dell'arcobaleno"
Arcobaleno::usage = "Creazione dell'animazione di un arcobaleno che viene eseguita automaticamente una sola volta. Input: True (default) o False (arco colorato con una scala di grigi)"

Begin["`Private`"]


(*definizioni dei colori utili*)
primari = {Yellow, Blue, Red}
secondari = {Darker[Green], Purple, Orange}
secondari2 = {Yellow, Darker[Green], Blue, Purple, Red, Orange, Yellow}
terziari = Map[Blend, Partition[secondari2, 2, 1]]
coloriGiusti = RotateRight[Riffle[Drop[secondari2,-1],terziari],2]


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

(* stringhe per il mouseover della ruota dei 12 colori*)
lblsOver = Flatten[ConstantArray[{"Primario", "Terziario","Secondario","Terziario"},3]];
(*caricamento immagine per il bottone "canella"*)
cleanImage = Import[FileNameJoin[{NotebookDirectory[],"clean.png"}], ImageSize -> 30];



(*Controlla la correttezza della risposta del primo esercizio*)
CheckAnswer[risposta_, colore_] := 
  (risposta == "Primario" && MemberQ[primari, colore]) ||(risposta == "Secondario" && MemberQ[secondari, colore]) ||
(risposta == "Terziario" && MemberQ[terziari, colore]);

(*Mostra la soluzione dell'esercizio 1*)
ShowAnswer[colore_] :=
Which[MemberQ[primari, colore], "Primario", MemberQ[secondari, colore], "Secondario", MemberQ[terziari, colore], "Terziario"];

(*esercizio 1, per la chiamata da frontend*)
ShowExercise1[] := DynamicModule[
	{ risposta,
	coloreRandom,
	messaggioUtente,
	buttonPrimarioColor,
	buttonSecondarioColor,
	buttonTerziarioColor,
	showhide,
	i, toshow, listaOggetti},
	
	messaggioUtente = "";(*stringa da stampare: se l'esercizio \[EGrave] corretto o la sua soluzione*)
	risposta = ""; (*variabile che memorizza la risposta dell'utente*)
	coloreRandom = RandomChoice[coloriGiusti]; (* variabile dinamica che sceglie un colore casuale tra i 12 del cerchio di Itten*)
	buttonPrimarioColor = Null;
	buttonSecondarioColor = Null;
	buttonTerziarioColor = Null;

	(*inizializzazioni per bottone che mostra e nasconde il grafico*)
	showhide={"Nascondi il cerchio di Itten","Mostra il cerchio di Itten"};
	i=1; (*indice per il numero di click*)
	(*lista degli oggetti grafici da mostrare sulla destra dell'esrcizio: area vuota e cerchio di itten\[Rule] per mostrare e nascondere il "suggerimento" del cerchio*)
	listaOggetti={Graphics[ImageSize->{300,400}], 
			Show[IttenWheel[ImageSize->Medium], ImageSize->{300,400},ImageMargins->{{40,400},{2,2}}]};
	toshow=listaOggetti[[1]]; (*se non esplicitamente richiesto tramite bottone, il cerchio \[EGrave] nascosto quindi si mostra l'area grafica vuota*)
	
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
           Alignment->Center, Frame -> True, FrameMargins->80],
           Column[{
			Button[cleanImage,risposta = ""; messaggioUtente = ""],
			Button["Nuovo esercizio", coloreRandom = RandomChoice[coloriGiusti]; risposta = ""; messaggioUtente = ""],
			Button["Mostra risposta", risposta = ""; messaggioUtente =ShowAnswer[coloreRandom]]
		}]
		(*setting area grafica che contiene l'esercizio*)
		},Alignment->Right, ImageSize-> {550, 600}, Background-> RGBColor["#f2f7ff"]],
		
		
		Row[{
			(*colonna contenente bottone e cerchio di itten*)
			Column[{
				Button[
					(* scelgo la label da mostrare (nascondi o mostra il cerchio), in base allo stato del cerchio che \[EGrave] dato dal numero di click nella variabile "i".
					Se il numero di click \[EGrave] 0 o pari, e quindi "i" \[EGrave] dispari (inizializzata a 1) il cerchio \[EGrave] nascosto e il bottone deve essere "mostrare". se il numero di click
					 \[EGrave] dispari ("i" pari) allora il cerchio \[EGrave] visibile e il bottone deve essere "nascondi"
					*)
					Dynamic@ToString@showhide[[1+Mod[i,2]]],
					toshow=listaOggetti[[1+Mod[i++,2]]], (*come commento sopra, si seleziona l'oggetto da mostrare: area vuota o cerchio di itten*)
					ImageSize->150, ImageMargins->{{100,100},{2,2}}, FrameMargins->Large, Alignment->Center],
				(*area che si aggiorna dinamicamente per mostrare e nascondere il cerchio*)
				Dynamic@Show[toshow] 
			}]
		}, ImageSize-> {400, 600}, Background-> RGBColor["#f2f7ff"]
		]
			
		}
		]

	];



(*colori primari e secondari necessari alla creazione del cerchio di itten (parte interna).
Il giallo \[EGrave] ripetuto al fine dei ottenere tutte le sfumature necessarie applicando , in seguito, la funzione Blend*)
secondariWheel={Yellow, Darker[Green], Blue, Purple, Red, Orange, Yellow};
(*creazione dell'esagono interno del cercio dei colori di Itten.*)
InnerGeometry[over_:False]:=
	
		If[
			over,
		(*caso dinamico*)
		(*se si vuole un cerchio dinamica, con mouseover chiamo la funzione SectorWheelOver per ogni colore*)
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
	

(*Funzione ausiliare. Dati: una lista di punti, una etichetta e un colore la funzione crea un poligono 
colorato al cui centro viene sovrapposta la sua etichetta (testo). 
Resituisce una lisa.*)
SectorWheel[pts_,txt_,color_]:=
	{Style[Polygon[pts],color], (*creazione del poligono*)
	Style[Text[txt, RegionCentroid[Polygon[pts]]], (*aggiunta del testo nel centro del poligono*)
	Bold,  FontSize->14 (*stile del testo*)
	]}
	
(*Funzione ausiliare. Dati: una lista di punti, due etichette e un colore la funzione crea un poligono 
colorato al cui centro viene sovrapposta la sua etichetta (testo) che cambia in seguito ad evento mouseover*)
SectorWheelOver[pts_,txt_, txtOver_,color_]:=
	Graphics[
	{
	(*Mouseover gestisce l'evento che si attiva quando il mouse passa sopra alla regione geometrica*)
		Mouseover[
		(*oggetto originale, che viene mostrato se il muose non si trova sopra l'oggetto*)
			{Style[Polygon[pts],color], (*creazione del poligono*)
			Style[Text[txt, RegionCentroid[Polygon[pts]]], (*aggiunta del testo nel centro del poligono*)
			Bold,  FontSize->14]},(*stile del testo*)
			
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
					               (*-1/12Pi per "ruotare" di mezzo settore,
					                +Pi/2 per portare i colori nella giusta posizione, con  il giallo in alto*)
			Annulus[{0,0},{r1,r2},{i-1/12 Pi+Pi/2,i+2/12*Pi-1/12Pi+Pi/2}],
			colorList[[i/(2/12*Pi)+1]]], (*colore del settore*)
		Style[
			Text[lbl[[i/(2/12*Pi)+1]], (*etichetta*)
			RegionCentroid[Annulus[{0,0},{r1,r2},{i-1/12 Pi+Pi/2,i+2/12*Pi-1/12Pi+Pi/2}]]], (*centro dell'area in cui mettere il testo*)
			FontSize->14, Bold (*stile del testo*)
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
					FontSize->14, Bold ]},(*stile del testo*)
					
				{Style[
				(*la funzione Annulus crea un singolo settore dell'anello.*)
						(*centro, raggio interno e esterno, angolo iniziale e finale*)
					Annulus[{0,0},{r1,r2},{i-1/12 Pi+Pi/2,i+2/12*Pi-1/12Pi+Pi/2}],
					colorList[[i/(2/12*Pi)+1]]], (*colore del settore*)
				Style[
					Text[lblsOver[[i/(2/12*Pi)+1]], (*etichetta*)
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

(*messaggi da stampare in caso di errore nl valore di Mouseover*)
IttenWheel::badBool = "`1` non \[EGrave] un valore valido per l'opzione Mouseover (valori accettati: True, False)";
IttenWheel::badSize = "`1` non \[EGrave] un valore valido per l'opzione ImageSize (valori accettati: Medium, Large)";

Options[IttenWheel]={Mouseover->False, ImageSize->Large};
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
	
(*creazione di una riga grafica di imaggini di dipinti.
imgs: lista di immagini;
size: lista di dimensioni per le immagini;
path: path in cui si trovano le immagini;
reg: lista di pattern da rimuovere nel nome del file*)
RigaImmagini[imgs_, size_, path_, reg_]:=
	GraphicsRow[
	(*crazione dell'immagine con Tooltip*)
		MapThread[		
			Tooltip, (*funzione da applicare*)
			{MapThread[Import, {imgs, size}], (*lista di immagini importare con dimensione specificata*)
			Fold[StringDelete,imgs,{".jpg",path,reg}]} (*recupero di nome e autore delle opere
														A partire dal path dell'immagine tolgo, in sequenza: l'estenzione,
														il path della cartella in cui si trovano le immagini "path", 
														un pattern iniziale, come "o-" che indica il colore dell'opera e 
														serve per caricare le immagini per colore.
														*)
	]]

(*funzione che crea cerchio interno + opere d'arte, chiamabile dal frontend*)
Paintings[]:=
	Module[{imgPath,pttCol, imgList,imgSize, 
		yPaintings,gPaintings,bPaintings,pPaintings,rPaintings, oPaintings,
		yClick,oClick,rClick,pClick,bClick,gClick,allpics,empty},
		imgPath=FileNameJoin[{NotebookDirectory[],"img"}];(*path in cui si trovano le immagini*)
	(*pattern per la suddivisione delle immagini per colore tramite il loro nome file*)
		pttCol={"y-*","g-*","b-*","p-*","r-*","o-*"};
	(*Per ogni colore primario e secondario si crea una lista dei path delle immagini*)
		imgList=MapThread[FileNames, {pttCol, ConstantArray[1,Length[pttCol]]imgPath}];
	(*dimensione immagini, utile per la chiamata di funzione RigaImmagini*)
		imgSize={ ImageSize->200, ImageSize->200, ImageSize->200}; 
		
		(*creazione delle righe di immagini per ogni colore primario e secondario*)
		yPaintings=RigaImmagini[imgList[[1]],imgSize,imgPath, "y-"];
		gPaintings=RigaImmagini[imgList[[2]],imgSize,imgPath, "g-"];
		bPaintings=RigaImmagini[imgList[[3]],imgSize,imgPath, "b-"];
		pPaintings=RigaImmagini[imgList[[4]],imgSize,imgPath, "p-"];
		rPaintings=RigaImmagini[imgList[[5]],imgSize,imgPath, "r-"];
		oPaintings=RigaImmagini[imgList[[6]],imgSize,imgPath, "o-"];
		(*creazione dei settori interni del cerchio di itten *)
		yClick=SectorWheel[ptsY,"giallo",Yellow];
		oClick=SectorWheel[ptsO,"arancio",Orange];
		rClick=SectorWheel[ptsR,"rosso",Red];
		pClick=SectorWheel[ptsV,"viola",Purple];
		bClick=SectorWheel[ptsB,"blu",Blue];
		gClick=SectorWheel[ptsG,"verde",Darker[Green]];
		(*area grafica vuota da sostituire alla chiusura delle immagini*)
		empty=GraphicsRow[ {Graphics[]} , ImageSize->{850,650}];
		
		Return[
			DynamicModule[{col1=empty},
			(*riga di due elementi grafici*)
			GraphicsRow[{
				(*colori primari e secondari*)
				Graphics[{
					(*EventHandler gestisce l'evento click del mouse sulla zona grafica (yClick = settore giallo dell'esagono di Itten)
						per ogni colore, quando cliccato, si ha una sostiuizione della variabile "col1":
													sostituisco qualsiasi cosa che non sia la riga delle immagini del colore in esame 
													con le immagini stesse; sostituisco la riga delle immagini del colore in esame con 
													l'area grafica vuota. *)
					EventHandler[yClick,{"MouseClicked":>(col1=col1/. {Except[yPaintings]->yPaintings,yPaintings->empty})}],
					EventHandler[gClick,{"MouseClicked":>(col1=col1/. {Except[gPaintings]->gPaintings,gPaintings->empty})}],
					EventHandler[bClick,{"MouseClicked":>(col1=col1/. {Except[bPaintings]->bPaintings,bPaintings->empty})}],
					EventHandler[pClick,{"MouseClicked":>(col1=col1/. {Except[pPaintings]->pPaintings,pPaintings->empty})}],
					EventHandler[rClick,{"MouseClicked":>(col1=col1/. {Except[rPaintings]->rPaintings,rPaintings->empty})}],
					EventHandler[oClick,{"MouseClicked":>(col1=col1/. {Except[oPaintings]->oPaintings,oPaintings->empty})}]}
					],
					(*area aggiornata dinamicamente*)
				Dynamic[col1]
			}, ImageSize->{900,350}, Alignment->Left]
			]
		]	

	]
		




(*Creazione dell'animazione di un arcobaleno con il color schema deciso dal parametro passato.
L'animazione viene eseguita automaticamente una sola volta*)
Arcobaleno[color_:True]:=
	Module[ {colSchema, txt},
		(*colore e testo da mostrare nel caso di arcobaleno "non colorato"/esercizio sbagliato*)
		colSchema = "PigeonTones";
		txt = "Riprova...";
		(*se si vuole un arcobaleno colorato nel caso di esercizio corretto*)
		If[color,
			colSchema = "Rainbow";
			txt = "Arcobaleno corretto!"
		];
		Return[
			Animate[ (*animazione*)
			(*plot di una semicirconferenza, dove x=angolo finale che varia durante l'animazione.
			r=raggio\[Rule] cera un anello*)
			ParametricPlot[ {r Cos[x],r Sin[x]},{x,0.001,m},{r,0.5,1},
			(*colore che varia con il raggio; definizione dello stile di presentazione dell'animazione e dell'area da rappresentare*)
				ColorFunction->(ColorData[colSchema][#4]&),Frame->False,Axes->False,BoundaryStyle->None,PlotRange->{{-1,1},{0,1}},
				(*aggiunta del testo sopra all'arcobaleno*)
				PlotLabel->Style[txt, FontSize->24, Black, Bold]
			],
			(*definizione di variazione dell'angolo. AppearenceElements\[Rule] None nasconde i comandi di animazione*)
			{m,0, Pi, AppearanceElements->None}, 
			(*velocit\[AGrave] dell'animazione e numero di ripetizione automatiche*)
			AnimationRate->0.7, AnimationRepetitions->1, Paneled->False]
		]
	]
(*funzione ausiliaria che controlla la correttezza della risposta dell'utente per l'esercizio 2 (arcobaleno).
Parametri: indici=lista degli indici inseriti dall'utente, coloriRand=lista dei colori ordinati casualmente
per una particolare istanza di esercizio.*)
CheckEs2[indici_, coloriRand_]:=
	If[
	(*si controlla se almeno una cella di input \[EGrave] lasciata vuota e se almeno un indice non \[EGrave] intero*)
		MemberQ[indici, ""] || !ArrayQ[indici,_,IntegerQ] || !AllTrue[indici,Positive] || Max[indici]> 6,
		(*allora sicuramente l'esercizio \[EGrave] sbagliato*)
		False,
		(*alternativamente, si controlla che la lista dei colori dell'arcobaleno sia uguale a quella dei colori random riordinati dall'utente*)
		MatchQ[RotateLeft[Drop[secondari2,1],3], coloriRand[[indici]] ]		
	]
(*funzione ausiliaria. prepara i sei dischi colorati per l'esercizio 3*)
CreatePaletteEs2[colori_]:=
		GraphicsRow[
				Graphics/@Table[
							{Style[Disk[], colori[[i]]], Style[Text[i], Bold, FontSize->14, White]}
							,{i,Length[colori]}]
			]
		
	(*funzione ausiliaria. restituisce la lista degli indici che corrispondono all'ordine corretto dei colori per creare l'arcobaleno*)
ShowSolutionEs2[colori_]:=
	Flatten[{
	(*restituisce la posizione del colore indicato all'interno della lista di colori con ordine randomico*)
		Position[colori,Red],
		Position[colori,Orange],
		Position[colori,Yellow],
		Position[colori,Darker[Green]],
		Position[colori,Blue],
		Position[colori,Purple]
	}]

(*esercizio 2: riordinamento dei 6 colori primari e secondari *)
ShowExercise2[] := DynamicModule[
	{ rainbowCol, randCol, palette, animazione, primo,secondo,terzo,quarto,quinto,sesto, risposta},
	(*colori dell'arcobaleno*)
	rainbowCol=RotateLeft[Drop[secondari2,1],3];
	randCol = RandomSample[rainbowCol];	(*riordino casuale dei colori*)
	palette = CreatePaletteEs2[randCol]; (*creazione dischi colorati*)
	animazione="";
	risposta="";
	primo=secondo=terzo=quarto=quinto=sesto="";
	Row[{
		Column[{
			 Dynamic@palette,
			 (*creazione riga di input field per l'utente*)
			Row[{
				InputField[Dynamic@primo,Number, ImageSize->{90,30}],
				InputField[Dynamic@secondo,Number, ImageSize->{90,30}],
				InputField[Dynamic@terzo,Number, ImageSize->{90,30}],
				InputField[Dynamic@quarto,Number, ImageSize->{90,30}],
				InputField[Dynamic@quinto,Number, ImageSize->{90,30}],
				InputField[Dynamic@sesto,Number, ImageSize->{90,30}]
			}],
			Dynamic@animazione
		}],
	
		Column[{
		(*bottone invia risposta*)
			Button[Style["Invia",FontSize->12],
					(*controllo la correttezza della risposta, creo l'animazione*)
					risposta=CheckEs2[{primo,secondo,terzo,quarto,quinto,sesto}, randCol]; animazione=Arcobaleno[risposta]],
					(*bottone di pulizia: cancello le celle di input, il risultato della risposta e l'animazione*)
			Button[cleanImage, primo = ""; secondo=""; terzo=""; quarto=""; quinto=""; sesto=""; risposta=""; animazione=""],
			(*creazione di un nuovo esercizio: pulizia delle variabili, nuovo riordinamento dei colori*)
			Button[Style["Nuovo esercizio",FontSize->12], randCol = RandomSample[rainbowCol]; 
					primo = ""; secondo=""; terzo=""; quarto=""; quinto=""; sesto="";risposta="";palette=CreatePaletteEs2[randCol];animazione=""],
					(*mostra soluzione dell'esercizio corrente*)
			Button[Style["Mostra soluzione",FontSize->12],{primo,secondo,terzo,quarto,quinto,sesto}= ShowSolutionEs2[randCol]; animazione=""]
		}]	
	}]
	

	];



End[]
(* inserire protect *)
EndPackage[]
