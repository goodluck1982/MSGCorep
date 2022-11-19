(* ::Package:: *)

(* A package for irreducible corepresentations of magnetic space group using the conventions in the book of 
   C.J. Bradley and A.P. Cracknell, "The Mathematical Theory of Symmetry in Solids", 
   called "the BC book" afterwards. *)
   
(* Package Name: MSGCorep *)
(* Author: Gui-Bin Liu *)
(* Package verseion: see MSGCorep`Private`Version *)
(* Dependency: SpaceGroupIrep, for version see MSGCorep`Private`SpaceGroupIrepDependency *)
(* Mathematica version: >=11.2 *)
(* License: GPLv3 http://www.gnu.org/licenses/gpl-3.0.txt *)


With[{p=DirectoryName[$InputFileName]}, If[!MemberQ[$Path,p],AppendTo[$Path, p]]];
MSGCorep`Private`Version={1,0,0};                    (*Specify version here.*)
MSGCorep`Private`SpaceGroupIrepDependency={1,2,3};   (*Specify dependency here.*)

Check[BeginPackage["MSGCorep`", {"SpaceGroupIrep`","LittleGroupIrepData`"}],
  Print["MSGCorep package (version ",StringRiffle[ToString/@MSGCorep`Private`Version,"."],") needs the SpaceGroupIrep (version>=",
        StringRiffle[ToString/@MSGCorep`Private`SpaceGroupIrepDependency,"."],") package, please download and install it first.\n",
        "URL: https://github.com/goodluck1982/SpaceGroupIrep"]; Abort[],
  {Needs::nocont, Get::noopen}]

(*---------- check dependency ---------*)
Module[{v1,v2,sv1,sv2,sv,out=False},
  v1=MSGCorep`Private`SpaceGroupIrepDependency; v2=SpaceGroupIrep`Private`Version;
  If[ValueQ[SpaceGroupIrep`Private`Version], If[Last@Sort[{v1,v2}]!=v2,out=True;sv2=StringRiffle[v2,"."]], out=True;sv2="<=1.2.2" ];
  If[out, sv1=StringRiffle[v1,"."]; sv=StringRiffle[MSGCorep`Private`Version,"."];
    Print[Style["Warning:",Red]," The current version of SpaceGroupIrep "<>sv2<>" does not meet the dependency >="<>sv1<>" required "<>
          "by MSGCorep v"<>sv<>"."," Please update SpaceGroupIrep to the latest version and reload it manually by <<SpaceGroupIrep`."] ]; 
];
Remove[v1,v2,sv1,sv2,sv,out];
(*-------------------------------------*)

Unprotect@@Names["MSGCorep`*"];
ClearAll@@Names["MSGCorep`*"];

Get["Usage.wl"];
Get["MSGData.wl"];

Begin["`Private`"]
cont="MSGCorep`Private`";


(* ::Section:: *)
(*Magnetic Space Group (MSG) Symbols*)


(* ::Subsection:: *)
(*MSGSymStd  (MSGSymBNS)*)


(* ::Subsubsection::Closed:: *)
(*MSGSymStd *)


Options[MSGSymStd]={"TeX"->False, "BC"->False};
MSGSymStd[symtext_String, OptionsPattern[]]:=
 Module[{tmp,tex,bar,sub,p1,p2,p3,chs,prime,subp},
  tex=(OptionValue["TeX"]===True);
  sub[s1_,s2_]:=If[tex, s1<>"_{"<>s2<>"}", Subscript[s1,s2]];
  bar[s1_]:=If[tex, "\\bar{"<>s1<>"}", OverBar[s1]];
  prime[s1_]:=If[tex, s1<>"'", Superscript[s1,"\[Prime]"]];
  subp[s1_,s2_]:=If[tex, s1<>"_"<>s2<>"'", Subsuperscript[s1,s2,"\[Prime]"]];
  chs=Characters[symtext];
  AppendTo[chs,"END"];
  p1=Position[chs,"-"];   
  If[p1!={}, p1=p1[[1,1]]; 
    chs=Join[chs[[1;;p1-1]], {bar[chs[[p1+1]]]}, chs[[p1+2;;]]];
  ];
  p2=Position[chs,"_"];
  While[p2!={}, p2=p2[[1,1]];
    If[p2==2&&chs[[3]]==="2",
      chs=Prepend[chs[[5;;]], sub[chs[[1]], "2"<>chs[[4]]]],
      If[chs[[p2+2]]==="'", 
        chs=Join[chs[[1;;p2-2]], {subp[chs[[p2-1]],chs[[p2+1]]]}, chs[[p2+3;;]]],  
        chs=Join[chs[[1;;p2-2]], {sub[chs[[p2-1]],chs[[p2+1]]]}, chs[[p2+2;;]]]
      ];
    ];
    p2=Position[chs,"_"];
  ];
  p3=Position[chs,"'"];
  While[p3!={}, p3=p3[[1,1]];
    chs=Join[chs[[1;;p3-2]], {prime[chs[[p3-1]]]}, chs[[p3+1;;]]];
    p3=Position[chs,"'"];
  ];
  If[tex, StringJoin["$",chs[[;;-2]],"$"], Row[chs[[;;-2]]]]
]

(*We take the BNS notation (N. V. Belov, N. N. Neronova and T. S. Smirnova) of MSGs as standard*)
MSGSymStd[{sgno_, num_}, OptionsPattern[]]:=Module[{nums, pos, text,bc},
  {nums,pos}=checkMSGinput[{sgno,num},"MSGSymStd"];
  bc=If[OptionValue["BC"]===True, "BC",""];
  text=ToExpression["type"<>ToString[pos[[1,1]]]<>"MSGSymText"<>bc][{sgno,num}]; 
  MSGSymStd[text,"TeX"->OptionValue["TeX"]]
]

MSGSymStd[sgno_Integer, OptionsPattern[]]/;1<=sgno<=230:=Module[{nums,nums2,nums3,syms,bc,ogsyms},
  nums={type1num[sgno],type2num[sgno],type3nums[sgno],type4nums[sgno]}//Flatten;
  bc="BC"->OptionValue["BC"];
  syms={"BNS:", ToString[sgno]<>"."<>ToString[#], MSGSymStd[{sgno,#},bc]}&/@nums;
  ogsyms={"OG:", StringRiffle[ToString/@#,"."], MSGSymOG[#]}&/@(BNStoOG[{sgno,#}]&/@nums);
  Flatten/@({getMSGType[{sgno,#}]&/@nums, syms, ogsyms}//Transpose)/.{1->"I", 2->"II", 3->"III", 4->"IV"}
]


(* ::Subsubsection::Closed:: *)
(*MSGSymBNS*)


MSGSymBNS=MSGSymStd;  (* define an alias *)


(* ::Subsection::Closed:: *)
(*MSGSymBC*)


Options[MSGSymBC]={"TeX"->False};
MSGSymBC[{sgno_Integer,num_Integer}, OptionsPattern[]]:=
  MSGSymStd[{sgno,num},"TeX"->OptionValue["TeX"], "BC"->True]
MSGSymBC[sgno_Integer, OptionsPattern[]]:=MSGSymStd[sgno, "BC"->True]


(* ::Subsection::Closed:: *)
(*MSGSymOG*)


Options[MSGSymOG]={"TeX"->False};
MSGSymOG[sgno_Integer, OptionsPattern[]]/;1<=sgno<=230:=Module[{nums,types,tmp,bnsnums},
  nums=Select[OGDataList[[All,1]],#[[1]]==sgno&];
  bnsnums=OGtoBNS/@nums;   types=getMSGType/@bnsnums;
  tmp={types, {"OG:", StringRiffle[ToString/@#,"."], MSGSymOG[#]}&/@nums,
              {"BNS:", StringRiffle[ToString/@#,"."], MSGSymStd[#]}&/@bnsnums}//Transpose;
  Flatten/@tmp/.{1->"I", 2->"II", 3->"III", 4->"IV"}
]

MSGSymOG[{p_Integer,q_Integer,r_Integer}, OptionsPattern[]]/;1<=p<=230&&1<=r<=1651:=Module[{nums},
  nums=Select[OGDataList[[All,1]],#[[1]]==p&];
  If[Select[nums,#=={p,q,r}&]=={}, Print["MSGSymOG: Error, {p,q,r} should be in ",nums]; Abort[]];
  MSGSymStd[MSGSymTextOG[{p,q,r}], "TeX"->OptionValue["TeX"]]
]


(* ::Subsection::Closed:: *)
(*showMSGSym*)


Options[showMSGSym]={"family"->"BNS", "BCstyle"->1, "color"->True};
showMSGSym[OptionsPattern[]]:=showMSGSym[All, (#->OptionValue[#])&/@{"family","BCstyle","color"}]
showMSGSym[listOrSpan_, OptionsPattern[]]:=Module[{sglist,tab,head,tmp1,tmp2,pos,outBC,
  lt=0.96,cls,bgs},  
  sglist=If[IntegerQ[listOrSpan], {listOrSpan}, Range[230][[listOrSpan]]];
  head={Column[{"BNS","Number"}], Column[{"BNS","Symbol"}], Column[{"BNS(BC)","Symbol"}],
        Column[{"OG","Number"}],Column[{"OG","Symbol"}], "Type"};
  If[!MemberQ[{1,2,3,4},OptionValue["BCstyle"]],
    Print["showMSGSym: The option \"BCstyle\" should be in {1,2,3,4} which controls how to ",
      "show the BNS symbol in BC orientation if it is the same with the standard BNS symbol:\n",
      "1, show \[LeftArrow] to mean the same with the left symbol\n2, does not show\n3, show in gray ",
      "color\n4, show in black color"]; Abort[],
    outBC[bc_]:=Switch[OptionValue["BCstyle"], 1, "\[LeftArrow]", 2, "", 3, Style[bc, Gray], 4, bc]
  ];
  If[OptionValue["family"]=!="OG", (* for BNS *)
    tmp1=MSGSymStd/@sglist; 
    pos={FoldList[Plus,1,#][[;;-2]],FoldList[Plus,#]}&[Length/@tmp1]+1;     
    tmp1=(Join@@tmp1)[[All,{3,4,6,7,1}]]\[Transpose];
    tmp2=MSGSymBC[ToExpression/@StringSplit[#,"."]]&/@tmp1[[1]];
    tmp2=If[#1===#2, outBC[#2], Style[#2,Red]]&@@@Transpose[{tmp1[[2]],tmp2}];
    tab=Prepend[tmp1,tmp2][[{2,3,1,4,5,6}]]\[Transpose];
    , (*---------------else: OG ---------------*)
    tmp1=MSGSymOG/@sglist; 
    pos={FoldList[Plus,1,#][[;;-2]],FoldList[Plus,#]}&[Length/@tmp1]+1;     
    tmp1=(Join@@tmp1)[[All,{3,4,6,7,1}]]\[Transpose];
    tmp2=MSGSymBC[ToExpression/@StringSplit[#,"."]]&/@tmp1[[3]];
    tmp2=If[#1===#2, outBC[#2], Style[#2,Red]]&@@@Transpose[{tmp1[[4]],tmp2}];   
    tab=Append[tmp1,tmp2][[{1,2,3,4,6,5}]]\[Transpose];
    head=head[[{4,5,1,2,3,6}]]; 
  ];
  tab=Prepend[tab,head];
  cls=Which[1<=#<=2, Lighter[Red,lt], 3<=#<=15, Lighter[Blue,lt], 16<=#<=74, Lighter[Orange,lt],
            75<=#<=142, Lighter[Cyan,lt], 143<=#<=167, Lighter[Yellow,lt],
            168<=#<=194, Lighter[Green,lt], 195<=#<=230, Lighter[Purple,lt]]&;
  bgs=Table[{pos[[All,i]],{1,-1}}->cls[sglist[[i]]],{i,Length[sglist]}];
  bgs=Prepend[bgs,{{1,1},{1,-1}}->Lighter[Gray,lt]];
  Grid[tab, Alignment->Left, ItemSize->Full, Spacings->{2,0.3}, Frame->True, 
            Dividers->{{},Prepend[#->Directive[Thin,Gray]&/@pos[[1,2;;]],2->True]},
            Background->If[OptionValue["color"]===True,{None,None,bgs},{}] 
      ]
]


(* ::Section:: *)
(*Elements of MSG and MLG (Magnetic little group)*)


(* ::Subsection::Closed:: *)
(*Elements for type-2 MSG*)


Options[getType2MSGElem]={"double"->False};
getType2MSGElem[sgno_Integer,OptionsPattern[]]:=
  getType2MSGElem[{sgno,type2num[sgno]},"double"->OptionValue["double"]]
getType2MSGElem[{sgno_Integer,num_Integer},OptionsPattern[]]:=Module[{tmp,MSG,G},
  tmp=type2num[sgno];
  If[num!=tmp, Print["getType2MSGElem: for type-2 MSG ",sgno,".num, num should be ",tmp]; Abort[]];
  G=getLGElem[sgno,"\[CapitalGamma]"];
  MSG=Join[Append[#,0]&/@G, Append[#,1]&/@G];
  If[OptionValue["double"]===True, Join[MSG,{"bar"<>#1,#2,#3}&@@@MSG], MSG]
]


(* ::Subsection::Closed:: *)
(*Elements for type-3 MSG*)


RotNameIndex=SpaceGroupIrep`Private`RotNameIndex;

(* Get the type-III MSG elements from the "colored" generators in BC-Tab. 7.2 *)
Options[getType3MSGElem]={"double"->False};
getType3MSGElem[sgno_Integer,cgens_List,OptionsPattern[]]/;AllTrue[cgens,StringQ]:=Module[
  {gens0,gens,brav,times,MSG0,SG,MSG,tmp},
  gens0=SGGenElem[sgno][[1,All,1]];   
  If[!SubsetQ[gens0,cgens], 
    Print["getType3MSGElem:  cgens=",cgens," is not a subset of the generators ",
      "(rotation parts) ",gens0," in the BC-Tab. 3.7"];
    Abort[];
  ];
  gens=Join[{#,0}&/@Complement[gens0,cgens], {#,1}&/@cgens];
  brav=getSGLatt[sgno];
  times[{R1_,a1_},{R2_,a2_}]:={getRotName[brav,getRotMat[brav,R1].getRotMat[brav,R2]],Mod[a1+a2,2]};
  MSG0=SortBy[generateGroup[gens,{"E",0},times],RotNameIndex[#[[1]]]&];
  tmp=Association[Rule@@@MSG0];
  SG=getLGElem[sgno,"\[CapitalGamma]"];
  MSG={#[[1]],#[[2]],tmp[#[[1]]]}&/@SG;
  If[OptionValue["double"]===True, Join[MSG,{"bar"<>#1,#2,#3}&@@@MSG], MSG]
]
getType3MSGElem[{sgno_Integer,num_Integer},OptionsPattern[]]:=Module[{tmp},
  tmp=BCTab7d2[sgno]//Keys;
  If[tmp=={}, Print["getType3MSGElem: There is no type-3 MSG begin with ",sgno]; Abort[],
     If[!MemberQ[tmp,num], 
         Print["getType3MSGElem: for type-3 MSG ",sgno,".num, num should be in ",
                Row[{#,"(", MSGSymStd[{sgno,#}],")"}]&/@tmp];
         Abort[]]
  ];
  getType3MSGElem[sgno,BCTab7d2[sgno,num],"double"->OptionValue["double"]]
]


(* ::Subsection::Closed:: *)
(*Elements for type-4 MSG*)


Options[getType4MSGElem]={"double"->False};
getType4MSGElem[{sgno_Integer,num_Integer},OptionsPattern[]]:=Module[{tmp,brav,BWlatt,A,MSG,G},
  tmp=type4nums[sgno];
  If[tmp=={}, Print["getType4MSGElem: There is no type-4 MSG begin with ",sgno]; Abort[],
     If[!MemberQ[tmp,num], 
         Print["getType4MSGElem: for type-4 MSG ",sgno,".num, num should be in ",
                Row[{#,"(",MSGSymStd[{sgno,#}],")"}]&/@tmp];
         Abort[]]
  ];
  brav=getSGLatt[sgno];  
  (*For orthorhombic system, we have to use the MSG symbol conforming to the BC orientation*)
  If[16<=sgno<=74 (*orth system*),  
    BWlatt=StringTake[type4MSGSymTextBC[{sgno,num}],3],
    BWlatt=StringTake[type4MSGSymText[{sgno,num}],3]
  ];
  A=type4AUTrans[brav,BWlatt];
  G=Append[#,0]&/@getLGElem[sgno,"\[CapitalGamma]"];
  MSG=Join[G, MapAt[modone,MSGSeitzTimes[brav][A,#]&/@G,{All,2}]];
  If[OptionValue["double"]===True, Join[MSG,{"bar"<>#1,#2,#3}&@@@MSG], MSG]
]


(* ::Subsection::Closed:: *)
(*getMSGElem: get elements of MSG*)


Options[getMSGElem]={"double"->False};
getMSGElem[{sgno_, num_},OptionsPattern[]]:=Module[{tmp,MSG,G,nums,pos,fun},
  {nums,pos}=checkMSGinput[{sgno,num},"getMSGElem"];
  pos=pos[[1,1]];
  fun=ToExpression[cont<>"getType"<>ToString[pos]<>"MSGElem"];
  If[pos==1,
    Append[#,0]&/@getLGElem[sgno,"\[CapitalGamma]","DSG"->OptionValue["double"]],
    fun[{sgno,num},"double"->OptionValue["double"]]
  ]
]


(* ::Subsection::Closed:: *)
(*getMLGElem: get elements of MLG*)


(* Get the magnetic little group. MSG is the element list of the magnetic space group.*)
getMLGElem[brav_String, MSG_List, k_]/;VectorQ[k]:=Module[{rotk,sele,LG,mLGu,mLGau},
  rotk={getRotMatOfK[brav,StringReplace[#[[1]],"bar"->""]].k, #}&/@MSG;
  sele=Select[rotk,If[#[[2,3]]==0,keqmod[k,#[[1]]],keqmod[-k,#[[1]]]]&];
  mLG=sele[[All,2]];  mLGu=Select[mLG,#[[3]]==0&];  mLGau=Select[mLG,#[[3]]==1&];
  Join[mLGu,mLGau]  (*put all unitary elements in front*)
]

Options[getMLGElem]={"double"->False};
getMLGElem[{sgno_, num_},k_,OptionsPattern[]]:=
 Module[{ks,brav,tmp,kco,BZs,MSG},
  MSG=getMSGElem[{sgno,num},"double"->OptionValue["double"]];
  brav=getSGLatt[sgno]; 
  kco=If[StringQ[k], kBCcoord[sgno,k][[1,1]], k];
  getMLGElem[brav,MSG,kco]
]


(* ::Subsection::Closed:: *)
(*showMSGSeitz*)


Options[showMSGSeitz]={"format"->"std", "fullbar"->True, "antiUcolor"->Red};
showMSGSeitz[{R_,v_,au_},OptionsPattern[]]:=Module[{Rv,color,c},
  Rv=showSeitz[{R,v}, "format"->OptionValue["format"], "fullbar"->OptionValue["fullbar"]];
  color=OptionValue["antiUcolor"];
  If[!ColorQ[color], Print["showMSGSeitz: option \"antiUcolor\" should be a color."]; Abort[]];  
  Switch[OptionValue["format"],
    "std"|"simple", If[au==0, Rv, Style[Superscript[Rv,"\[Prime]"],color]],
    "TeX", c="\\color[rgb]"<>ToString@(List@@color);
           If[au==0, Rv, "${"<>c<>StringTake[Rv,{2,-2}]<>"'}$"]
  ]
];


(* ::Section:: *)
(*Common utilities*)


(* ::Subsection::Closed:: *)
(*checkMSGinput*)


checkMSGinput[{sgno_,num_}, fun_String]:=Module[{nums,pos,spc,tab,sty1},
  If[!IntegerQ[sgno]||sgno<1||sgno>230, 
      Print[fun<>": {sgno,num} should be any of the 1651 integer pairs listed as follow:\n",
            Partition[MSGSymText//Keys//Sort,UpTo[10]]//Grid[#,Alignment->Left,Spacings->{0.5,0.2}]&];
      Abort[]; 
  ];
  nums={type1num[sgno],type2num[sgno],type3nums[sgno],type4nums[sgno]};
  pos=Position[nums,num];
  If[pos=={}, 
     spc=StringReplace[fun,_->" "]<>"  ";
     sty1={{}, #->GrayLevel[0.7]&/@{2,3,Length[nums[[3]]]+3}};
     tab=Grid[#,Alignment->Left,Frame->True,FrameStyle->Thin, Dividers->sty1,Spacings->2]&;
     Print[fun<>": for MSG ",sgno,".num, num should be in ",nums,".\n",spc<>"Refer to:\n", spc,
           MSGSymStd[sgno][[All,{3,4,1}]]//tab]; 
     Abort[]   
  ];
  {nums,pos}
]


(* ::Subsection::Closed:: *)
(*Multiplication for (double) magnetic space group*)


MSGSeitzTimes[brav_][{Rname1_,v1_,antiU1_}, {Rname2_,v2_,antiU2_}]/;SubsetQ[{1,0},{antiU1,antiU2}]:=
  Append[SeitzTimes[brav][{Rname1,v1},{Rname2,v2}], Mod[antiU1+antiU2,2]]
MSGSeitzTimes[brav_][Rvau1_,Rvau2_,more__]:=Fold[MSGSeitzTimes[brav],Rvau1,{Rvau2,more}]
MSGinvSeitz[brav_][{Rname1_,v1_,antiU1_}]/;MemberQ[{1,0},antiU1]:=
  Append[invSeitz[brav][{Rname1,v1}],antiU1]
MSGpowerSeitz[brav_][{Rname1_,v1_,antiU1_},n_Integer]/;MemberQ[{1,0},antiU1]&&n>=0:=
  Append[powerSeitz[brav][{Rname1,v1},n],Mod[n*antiU1,2]]
(*NOTE: For double MSG, theta^2\[Equal]barE, where theta is the time reversal *)
DMSGSeitzTimes[brav_][{Rname1_,v1_,antiU1_}, {Rname2_,v2_,antiU2_}]/;SubsetQ[{1,0},{antiU1,antiU2}]:=
  Module[{au=antiU1+antiU2, Rv},
    Rv=DSGSeitzTimes[brav][{Rname1,v1},{Rname2,v2}];
    If[au<2, Append[Rv,au], Append[DSGSeitzTimes[brav][Rv,{"barE",{0,0,0}}],0]]
  ]
DMSGSeitzTimes[brav_][Rvau1_,Rvau2_,more__]:=Fold[DMSGSeitzTimes[brav],Rvau1,{Rvau2,more}]
DMSGinvSeitz[brav_][{Rname1_,v1_,antiU1_}]/;MemberQ[{1,0},antiU1]:=
  With[{iRv=DSGinvSeitz[brav][{Rname1,v1}]}, 
    If[antiU1==0, Append[iRv,0], Append[DSGSeitzTimes[brav][iRv,{"barE",{0,0,0}}],1]]]
DMSGpowerSeitz[brav_][{Rname1_,v1_,antiU1_},n_Integer]/;MemberQ[{1,0},antiU1]&&n>=0:=
  Module[{Rvn},
    Rvn=DSGpowerSeitz[brav][{Rname1,v1},n];
    Switch[Mod[n*antiU1,4],
      0, Append[Rvn,0],  1, Append[Rvn,1],
      2, Append[DSGSeitzTimes[brav][Rvn,{"barE",{0,0,0}}],0],
      3, Append[DSGSeitzTimes[brav][Rvn,{"barE",{0,0,0}}],1]
    ]
  ]


(* ::Subsection::Closed:: *)
(*Multiplication for (double) magnetic point group*)


Options[checkMagRotInput]={"double"->False};
checkMagRotInput[R_, OptionsPattern[]]:=Module[{rots,rots2},
  rots=Keys[getSpinRotOp];
  If[OptionValue["double"]=!=True, rots=rots[[;;Length[rots]/2]]];
  rots2=Join[rots, #<>"'"&/@rots];
  If[StringQ[R]&&MemberQ[rots2,R]||ListQ[R]&&Length[R]==2&&MemberQ[rots,R[[1]]]&&MemberQ[{0,1},R[[2]]],
    Return@If[StringQ[R], If[StringTake[R,-1]=="'", {StringTake[R,{1,-2}],1},{R,0}], R],
    Print["The input ",R," should be one of the two forms:\n",
      "(1) The string of rotation name for an element in the magnetic point group, i.e. \"Rname\" or \"Rname'\".\n",
      "(2) {\"Rname\",0} for unitary rotation (equivalent to \"Rname\") or {\"Rname\",1} for antiunitary rotation ",
      "(equivalent to \"Rname'\").\nAnd Rname should be in the list\n",rots,".\n"
      ]; Abort[];
  ]
]

MagRotTimes[R1_,R2_]:=Module[{rot1,rot2},
  rot1=checkMagRotInput[R1];
  rot2=checkMagRotInput[R2];
  {RotTimes[rot1[[1]],rot2[[1]]], Mod[rot1[[2]]+rot2[[2]],2]}
]
MagRotTimes[R1_,R2_,more__]:=Fold[MagRotTimes,R1,{R2,more}]
invMagRot[R_]:=With[{rot=checkMagRotInput[R]}, MapAt[invRot,rot,1]]
powerMagRot[R_,n_Integer]:=With[{rot=checkMagRotInput[R]}, {powerRot[rot[[1]],n],Mod[rot[[2]]*n,2]}]

DMagRotTimes[R1_,R2_]:=Module[{rot1,rot2,rot,au},
  rot1=checkMagRotInput[R1,"double"->True];
  rot2=checkMagRotInput[R2,"double"->True];
  rot=DRotTimes[rot1[[1]],rot2[[1]]];   au=rot1[[2]]+rot2[[2]]; 
  If[au==2, rot=DRotTimes[rot,"barE"]; au=0];
  {rot,au}
]
DMagRotTimes[R1_,R2_,more__]:=Fold[DMagRotTimes,R1,{R2,more}]
invDMagRot[R_]:=Module[{rot=checkMagRotInput[R,"double"->True],r,au}, 
  r=invDRot[rot[[1]]];  au=rot[[2]];
  If[au==1, r=DRotTimes[r,"barE"]];
  {r,au}
]
powerDMagRot[R_,n_Integer]:=Module[{rot=checkMagRotInput[R,"double"->True],r}, 
  r=powerDRot[rot[[1]],n];  
  Switch[Mod[n*rot[[2]],4],
      0, {r,0}, 1, {r,1}, 2, {DRotTimes[r,"barE"],0}, 3, {DRotTimes[r,"barE"],1}
  ]
]


(* ::Subsection::Closed:: *)
(*getMagKStar*)


Options[getMagKStar]={"cosets"->False};
getMagKStar[{sgno_, mno_}, kin_, OptionsPattern[]]:=Module[{k,MSG,brav,kall,star0,star,cosets},
  MSG=getMSGElem[{sgno,mno}];   brav=getSGLatt[sgno];
  k=If[!StringQ[kin], kin, kBCcoord[sgno,kin][[1,1]]];
  kall={If[#[[3]]==0,1,-1]*getRotMatOfK[brav,#[[1]]].k, #}&/@MSG;
  star0=Gather[kall,keqmod[#1[[1]],#2[[1]]]&];
  star=star0[[All,1,1]];   cosets=star0[[All,All,2]];
  If[OptionValue["cosets"]===True, {star,cosets}, star]
]



(* ::Subsection::Closed:: *)
(*getMSGType*)


getMSGType[{sgno_, mno_}]:=checkMSGinput[{sgno,mno},"getMSGType"][[2,1,1]]


(* ::Section:: *)
(*Magnetic point group (MPG)*)


(* ::Subsection::Closed:: *)
(*common*)


checkMPGinput[mpg_, fun_String]:=Module[{mpgno,mpgs,err=False,adjust},
  If[IntegerQ[mpg]&&1<=mpg<=32,
    Print[fun<>": Use any item as input listed as follow:\n",
      TableForm[MapAt[InputForm,Select[MPGinfo[[All,2;;5]],#[[1,1]]==mpg&],{All,2;;4}],TableDepth->2]];
    Abort[]; 
  ];
  mpgno=Position[MPGinfo[[All,2;;5]],mpg,{2}];
  If[mpgno!={}, mpgno=mpgno[[1,1]], err=True];
  If[err,
    adjust=MPGinfo[[All,2;;5]];
    adjust=MapAt[InputForm,adjust,{All,2;;4}];
    adjust=MapAt[Grid[{{#}},ItemSize->3.8,Alignment->Left]&, adjust, {All,1}];
    adjust=MapAt[Grid[{{#}},ItemSize->3,Alignment->Left]&, adjust, {All,2}];
    adjust=MapAt[Grid[{{#}},ItemSize->5.5,Alignment->Left]&, adjust, {All,3}];
    adjust=MapAt[Grid[{{#}},ItemSize->5.5,Alignment->Left]&, adjust, {All,4}];
    mpgs=TableForm@Partition[Row[#,"|"]&/@adjust,UpTo[3]];
    Print[fun<>": To specify a mangnetic point group, use any one as input listed as follow:\n",mpgs];
    Abort[]
  ];
  mpgno
]


(* ::Subsection::Closed:: *)
(*getMPGElem and showMagRot*)


Options[getMPGElem]={"double"->False};
getMPGElem[mpg_, OptionsPattern[]]:=Module[{n,msg,elms},
  n=checkMPGinput[mpg,"getMPGElem"]; msg=MPGinfo[[n,8]];
  elms=getMLGElem[msg,"\[CapitalGamma]"][[All,{1,3}]]; 
  If[OptionValue["double"]===True, elms=Join[elms, MapAt["bar"<>#&, elms, {All,1}]]];
  elms
]


Options[showMagRot]={"format"->"std", "fullbar"->False, "antiUcolor"->Red};
showMagRot[R_,OptionsPattern[]]:=Module[{R0,hasbar,fmt,rot,fullbar,au,color},
  fmt=OptionValue["format"];  fullbar=OptionValue["fullbar"];
  If[!MemberQ[{"text","simple","std","TeX"},fmt],
    Print["showMagRot: option \"format\" should be in ",InputForm/@{"text","simple","std","TeX"}]; Abort[];
  ];
  rot=checkMagRotInput[R,"double"->True];  au=rot[[2]];
  If[fmt=="text", Return@If[au==0, rot[[1]], rot[[1]]<>"'"]];
  R0=showRot[rot[[1]],"format"->fmt,"fullbar"->fullbar];
  If[au==0, Return[R0]];
  R0=Switch[fmt, "simple"|"std", Superscript[R0,"\[Prime]"], "TeX", "{"<>R0<>"}'"];
  color=OptionValue["antiUcolor"];  If[color===None, Return[R0]];
  Switch[fmt, "simple"|"std", Style[R0,color],
    "TeX", "{\\color[rgb]"<>ToString@(List@@color)<>R0<>"}" ]  
]


(* ::Subsection::Closed:: *)
(*showMPGinfo*)


Options[showMPGinfo]={"long"->True, "color"->True, "elem"->False};
showMPGinfo[OptionsPattern[]]:=showMPGinfo[All,(#->OptionValue[#])&/@{"long","color","elem"}]
showMPGinfo[range_, OptionsPattern[]]:=Module[{stab,lt=0.90,bgc,bgs,cls,ncol,ltab,note,idx,nout,ifelm},
  bgc[n1_,n2_,color_]:=#->color&/@Range[n1,n2];
  idx=range;
  If[IntegerQ[idx], idx=If[idx==0,All,{idx}]];
  If[!(VectorQ[idx,IntegerQ]||Head[idx]===Span||idx===All), 
    Print["showMPGinfo: range can be an integer, a list of integers, a span, or just All ",
          "to specify the magnetic point groups to show"]; Abort[]];
  Check[idx=Range[Length[MPGinfo]][[DeleteCases[idx,0]]],
    Print["showMPGinfo: out of range [0,",Length[MPGinfo],"]!"]; Abort[],
    {Part::partw,Part::take}];
    
  stab={Row[{"(",#[[1]],")"}],#[[3]],MSGSymStd[#[[5]]]}&/@MPGinfo[[idx]];
  stab=Grid[{#}, ItemSize->{{3,3,Full}}, Alignment->Left, Spacings->0]&/@stab;
  cls=Join[bgc[1,5,Lighter[Red,lt]], bgc[6,16,Lighter[Blue,lt]], bgc[17,28,Lighter[Orange,lt]],
           bgc[29,59,Lighter[Cyan,lt]], bgc[60,75,Lighter[Yellow,lt]],
           bgc[76,106,Lighter[Green,lt]],bgc[107,122,Lighter[Purple,lt]]]//Association;
  ncol=5;  nout=Length[idx];
  bgs=Thread[Flatten[Table[{i,j},{i,Ceiling[nout/ncol]},{j,ncol}],1][[;;nout]] -> cls/@idx];
  stab=Partition[stab, UpTo[ncol]]//Grid[#,Alignment->Left, Spacings->{2,0.3},
                       Background->If[OptionValue["color"],{None,None,bgs},{}]]&;
  If[OptionValue["long"]==False, Return[stab]];
  
  ifelm=If[OptionValue["elem"]===True,#,Nothing]&;
  ltab={#[[1]], #[[3]], #[[4]], MSGSymStd[#[[5]]], 
        Row[{MSGSymStd[PGinfo[[#[[6]],3]]]," (",MSGSymStd[PGinfo[[#[[6]],2]]],")"}],
        #[[7]], MSGSymStd[#[[8]]], ifelm[showMagRot/@getMPGElem[#[[2]]]]}&/@MPGinfo[[idx]];
  ltab=Prepend[ltab, {Column[{"Seq.","No."}], Column[{"Number","(short)"}], Column[{"Number","(long)"}],
                     Column[{"Symbol"}], Column[{"Unitary","subgroup H"}],
                     Column[{"Type"}], "First MSG",ifelm["Elements of the MPG"]}];
  cls=Prepend[Thread[Range[2,nout+1] -> Values[cls][[idx]]], 1->Lighter[Gray,lt]];
  ltab=Grid[ltab, Alignment->Left, Spacings->{2,0.3}, Frame->True, Dividers->{{},{2->True}},
                  Background->If[OptionValue["color"],{{},cls},{}]];
  note="The symbol of a magnetic point group (MPG) here is determined by the BNS symbol of\n"<>
       "the first magnetic space group (MSG) with this MPG. And the order of MPGs also follows\n"<>
       "the order of BNS MSG numbers. The symbols and long numbers here are consistent with\n"<>
       "those on the BCS website. Note: the symbols and order of the type-3 MPGs here are not\n"<>
       "always the same with those in BC-Tab. 7.1. For example, the mmm' in BC-Tab. 7.1 is \n"<>
       "m'mm here, and 2/m' precedes 2'/m in BC-Tab. 7.1 while the order is reversed here.";
  Column[{ltab,note}]
]


(* ::Subsection::Closed:: *)
(*getMPGCorep*)


Options[getMPGCorep]={"double"->False,"trace"->False};
getMPGCorep[mpg_,OptionsPattern[]]:=Module[{n,msgno,smap,dmap,MPG,subno,H,Hstd,A,nsir,subir,dbl,
  matN,cordict,type,ir,nir,cor,lb,ncor,clb,ch,AH,elmidx,times,Hidx,AH2idx,inv,iAuA,i,j,subidx,
  paired,aiA,aA,iAa,tmp,totrace},
  n=checkMPGinput[mpg,"getMPGCorep"];
  (*map the elements of the unitary subgroup to those of getPGElem*)
  Switch[n,
    22, smap=dmap={"\[Sigma]y"->"\[Sigma]z","bar\[Sigma]y"->"bar\[Sigma]z"},
    26, smap=dmap={"C2x"->"C2z","\[Sigma]y"->"\[Sigma]x","\[Sigma]z"->"\[Sigma]y","barC2x"->"barC2z","bar\[Sigma]y"->"bar\[Sigma]x","bar\[Sigma]z"->"bar\[Sigma]y"},
    46|50, smap=dmap={"\[Sigma]da"->"\[Sigma]y","\[Sigma]db"->"\[Sigma]x","bar\[Sigma]da"->"bar\[Sigma]y","bar\[Sigma]db"->"bar\[Sigma]x"},
    56, smap=dmap={"C2a"->"C2y","C2b"->"C2x","\[Sigma]da"->"\[Sigma]y","\[Sigma]db"->"\[Sigma]x","barC2a"->"barC2y",
                   "barC2b"->"barC2x","bar\[Sigma]da"->"bar\[Sigma]y","bar\[Sigma]db"->"bar\[Sigma]x"},
    73, smap={"\[Sigma]d1"->"\[Sigma]v2","\[Sigma]d2"->"\[Sigma]v3","\[Sigma]d3"->"\[Sigma]v1"};   dmap={"\[Sigma]d1"->"bar\[Sigma]v2","\[Sigma]d2"->"bar\[Sigma]v3",
        "\[Sigma]d3"->"\[Sigma]v1","bar\[Sigma]d1"->"\[Sigma]v2","bar\[Sigma]d2"->"\[Sigma]v3","bar\[Sigma]d3"->"bar\[Sigma]v1"},
    89, smap={"C21pp"->"C23p","C22pp"->"C21p","C23pp"->"C22p"}; dmap={"C21pp"->"C23p","C22pp"->"barC21p",
        "C23pp"->"barC22p","barC21pp"->"barC23p","barC22pp"->"C21p","barC23pp"->"C22p"},
    104,smap={"C21pp"->"C23p","C22pp"->"C21p","C23pp"->"C22p","\[Sigma]v1"->"\[Sigma]d3","\[Sigma]v2"->"\[Sigma]d1","\[Sigma]v3"->"\[Sigma]d2"};
        dmap={"C21pp"->"C23p","C22pp"->"barC21p","C23pp"->"barC22p","\[Sigma]v1"->"\[Sigma]d3","\[Sigma]v2"->"bar\[Sigma]d1","\[Sigma]v3"->"bar\[Sigma]d2",
        "barC21pp"->"barC23p","barC22pp"->"C21p","barC23pp"->"C22p","bar\[Sigma]v1"->"bar\[Sigma]d3","bar\[Sigma]v2"->"\[Sigma]d1","bar\[Sigma]v3"->"\[Sigma]d2"},
    _,  smap=dmap={}
  ];
  msgno=MPGinfo[[n,2]];   subno=MPGinfo[[n,6]];
  dbl=OptionValue["double"]===True;
  MPG=getMPGElem[msgno,"double"->dbl];
  subir=getPGIrepTab[subno,"double"->dbl,"trace"->False];
  lb=subir["label"];   ir=subir["irep"];   nir=Length[ir];  Hstd=subir["elem"];
  cordict=<||>;
  cordict["number"]=MPGinfo[[n,{2,3}]];    cordict["symbol"]=MPGinfo[[n,5]];
  cordict["USubG"]=PGinfo[[subno,1;;3]];
  nsir=PGinfo[[subno,5]]; 
  totrace=Map[If[MatrixQ[#],Simplify@Tr[#],#]&,#,{2}]&;
  If[msgno[[2]]==1, (*------for type-1 MPG-------*)
    cordict["elem"]={#,0}&/@Hstd;
    cordict["A"]=None;    cordict["N"]=None;
    cordict["label"]=lb;  cordict["type"]=Table["x",nir];
    cordict["subidx"]=Range[nir];
    cordict["sindex"]=Range[nsir];
    cordict["dindex"]=Complement[Range[nir],Range[nsir]];
    cordict["corep"]=If[OptionValue["trace"]===True, totrace[ir], ir];  
    Return[cordict]
  ];
  ch=totrace[ir];
  H=Select[MPG,#[[2]]==0&];  AH=Select[MPG,#[[2]]==1&];  A=AH[[1]];
  elmidx=Association@Table[Hstd[[i]]->i,{i,Length[Hstd]}];
  elmidx=elmidx/@(H[[All,1]]/.If[dbl,dmap,smap]);
  ch=ch[[All,elmidx]];    ir=ir[[All,elmidx]];
  times=If[dbl,DMagRotTimes,MagRotTimes];
  Hidx=Association@Thread[H->Range[Length[H]]];
  AH2idx=Hidx[times[#,#]]&/@AH;
  type=Simplify[Total[#[[AH2idx]]]/Length[H]&/@ch]/.{1->"a",-1->"b",0->"c"};
  inv=If[dbl,invDMagRot,invMagRot];
  iAuA=Hidx@times[times[inv[A],#],A]&/@H;
  matN=Table[None,nir];
  For[i=1,i<=nir,i++, If[type[[i]]=="c", Continue[]];
    matN[[i]]=findMatrixN[ir[[i]],ir[[i,iAuA]]\[Conjugate]]
  ];
  subidx={};  paired=Table[False,nir];
  For[i=1,i<=nir,i++, If[paired[[i]], Continue[]];
    If[type[[i]]!="c", subidx=Append[subidx,i],
      j=First@Select[Range[i+1,nir],Simplify[Conjugate@ch[[i,iAuA]]==ch[[#]]]&];
      subidx=Append[subidx,{i,j}];  paired[[j]]=True;
    ]
  ];
  ncor=Length[subidx];   clb=cor=Table[{},ncor];
  aiA=Hidx@times[#,inv[A]]&/@AH;
  aA=Hidx@times[#,A]&/@AH;
  iAa=Hidx@times[inv[A],#]&/@AH;
  For[i=1,i<=ncor,i++, j=subidx[[i]];
    If[IntegerQ[j],
      tmp=If[MatrixQ[ir[[j,1]]], #.matN[[j]]&/@ir[[j,aiA]], ir[[j,aiA]]];
      If[type[[j]]=="a",
        cor[[i]]=Join[ir[[j]], tmp]; clb[[i]]=lb[[j]], (*---next line else: b---*)
        cor[[i]]=Join[ArrayFlatten[{{#,0},{0,#}}]&/@ir[[j]],ArrayFlatten[{{0,-#},{#,0}}]&/@tmp];
        clb[[i]]=#<>#&/@lb[[j]]
      ], (*---------else: for type c----------*)
      tmp=Mulliken2str@lb[[j[[1]],1]];
      If[StringTake[tmp,1]=="1", 
        tmp=str2Mulliken@StringTake[tmp,{2,-1}],
        If[StringLength[tmp]>4&&StringTake[tmp,4]=="bar1", 
          tmp=str2Mulliken["bar"<>StringTake[tmp,{5,-1}]], (*else*) tmp=StringJoin[lb[[j,1]]]
        ];
      ];          
      clb[[i]]={tmp,StringJoin[lb[[j,2]]]};
      j=j[[1]];
      cor[[i]]=Join[ArrayFlatten[{{#1,0},{0,#2}}]&@@@Transpose[{ir[[j]],ir[[j,iAuA]]\[Conjugate]}],
                    ArrayFlatten[{{0,#1},{#2,0}}]&@@@Transpose[{ir[[j,aA]],ir[[j,iAa]]\[Conjugate]}] ];
    ];
  ];
  
  tmp=If[IntegerQ[#],#,#[[1]]]&/@subidx;
  cordict["elem"]=Join[H,AH];
  cordict["A"]=A;        cordict["N"]=matN[[tmp]];
  cordict["label"]=clb;   cordict["type"]=type[[tmp]];
  cordict["subidx"]=subidx;
  tmp=Position[subidx,nsir][[1,1]];
  cordict["sindex"]=Range[tmp];
  cordict["dindex"]=Complement[Range[ncor],Range[tmp]];
  cordict["corep"]=If[OptionValue["trace"]=!=True,cor,totrace[cor]];  
  cordict
]


(* ::Subsection::Closed:: *)
(*showMPGCorep*)


Options[showMPGCorep]={"double"->True,"rotmat"->True,"elem"->All,"corep"->All,"trace"->False,
                        "spin"->"downup","cartesian"->False,"linewidth"->2};
showMPGCorep[mpg_, OptionsPattern[]]:=Module[{n,mpgno,mpgcor,label,cor,elmopt,tmp,elmidx,dbl,type,
  elems,nelm,elmerr,nscor,ndcor,ncor,sidx,didx,txtirl,iropt,irerr,iridx,row1,rots1,rots2,brav,tab,
  nstart,grid,sty1,sty2,bg0,bg1,bg1a,bg2,bg3,bg4,bg5,spl,box},
  (*-------check option "double"---------*)
  dbl=OptionValue["double"];
  If[!MemberQ[{True,False,Full},dbl],
    Print["showMPGCorep: \"double\" should be one of True (default), False, or Full:\n",
          "True:  For coreps of double magnetic point groups. Only half of all elements, i.e. the ones without bar, are shown.\n", 
          "Full:  For coreps of double magnetic point groups. All elements are shown.\n", 
          "False: For coreps of single magnetic point groups.\n"];
    Abort[]
  ];

  n=checkMPGinput[mpg,"showMPGCorep"];  mpgno=MPGinfo[[n,2]];
  mpgcor=getMPGCorep[mpgno,"double"->OptionValue["double"]=!=False, "trace"->OptionValue["trace"]];
  elems=mpgcor["elem"];   cor=mpgcor["corep"];   label=mpgcor["label"];
  sidx=mpgcor["sindex"];  didx=mpgcor["dindex"]; type=mpgcor["type"];
  nscor=Length[sidx];  ndcor=Length[didx];  ncor=nscor+ndcor;
  nelm=Length[elems];
  If[dbl=!=False,
    tmp=Select[elems,StringTake[#[[1]]<>"xxx",3]!="bar"&];
    tmp=Join[tmp, Select[elems,StringTake[#[[1]]<>"xxx",3]=="bar"&]];
    tmp=Association@Table[tmp[[i]]->i,{i,nelm}]/@elems;
    elems=elems[[tmp]];
    cor=cor[[All,tmp]]
  ];

  (*-------check option "elem"---------*)
  elmopt=OptionValue["elem"];
  elmerr:=Print["showMPGCorep: \"elem\" aims to reorder the columns and it can be:\n",
           "A list of rotation names (elements). OR\n",
           "A list of integers (or a span) for their sequence numbers. Refer to\n",
            Grid[Partition[Grid[{#},ItemSize->{{1.5,Full}},Alignment->Left]&/@
                 Transpose@{Range[nelm],InputForm[showMagRot[#,"format"->"text"]]&/@elems},UpTo[10]], 
                 ItemSize->Full, Frame->All, Alignment->Left, FrameStyle->Gray] ];
  If[elmopt===0, elmopt=All];
  If[StringQ[elmopt]||IntegerQ[elmopt], elmopt={elmopt}];
  elmidx=elmopt;
  If[VectorQ[elmidx,StringQ], 
    tmp=showMagRot[#,"format"->"text"]&/@elems;
    If[SubsetQ[tmp,elmidx], elmidx=Position[tmp,#][[1,1]]&/@elmidx, elmerr; Abort[]], (*next line else*)
    If[!(VectorQ[elmidx,IntegerQ]||Head[elmidx]===Span||elmidx===All), elmerr; Abort[]]
  ]; 
  Check[elmidx=Range[nelm][[DeleteCases[elmidx,0]]], 
    Print["showMPGCorep: out of range! There are ",nelm," elements in total."]; Abort[], 
    {Part::partw,Part::take}];

  (*-------check option "corep"---------*)
  iropt=OptionValue["corep"];
  irerr:=Print["showMPGCorep: \"corep\" aims to reorder the coreps (rows) and it can be:\n",
           "An integer, a list of integers, a span, or just All (default) for the corep sequence numbers. ",
           "The range is 1 to ", ncor, "."];
  If[iropt===0, iropt=All];
  iridx=If[StringQ[iropt]||IntegerQ[iropt], {iropt}, iropt];
  If[!(VectorQ[iridx,IntegerQ]||Head[iridx]===Span||iridx===All), irerr; Abort[]]
  Check[iridx=Range[ncor][[DeleteCases[iridx,0]]], 
    Print["showMPGCorep: out of range! There are ",ncor," coreps in total."]; Abort[], 
    {Part::partw,Part::take}];
  
  If[dbl===True&&elmopt===All, nelm=nelm/2; elmidx=elmidx[[;;nelm]]];
  cor=cor[[All,elmidx]];    elems=elems[[elmidx]];

  tab=Map[formatRepMat,cor,{2}];
  tab=Map[If[MatrixQ[#],MatrixForm[#],#]&, tab, {2}];
  tab=Table[{i,Sequence@@label[[i]],type[[i]],Sequence@@tab[[i]]},{i,iridx}];
  
  spl:=Sequence@@Table[SpanFromLeft,3];
  tmp=Column[{Row[{"MPG: ", mpgcor["number"][[2]],", ",MSGSymStd@mpgcor["symbol"]}],
        Row[{"USubG: ", MSGSymStd@mpgcor["USubG"][[3]], " (",str2Mulliken@mpgcor["USubG"][[2]],")"}]
      }];
  (*\:6ce8\:610f\:ff1a\:5982\:679c\:4e0d\:5bf9\:7fa4\:5143\:52a0\:4e2abox\:5c01\:88c5\:4e00\:4e0b\:ff0c\:5e26bar\:7684\:7fa4\:5143\:4f1a\:5bfc\:81f4\:4e0a\:4e00\:884c\:7684\:7f16\:53f7\:884c\:7684\:884c\:9ad8\:5f02\:5e38\:53d8\:5927*)
  box=Grid[{{#}},Spacings->{0,0},Alignment->{Center,Center}]&;
  row1={{tmp,spl,Sequence@@box/@elmidx},
        {SpanFromAbove,Sequence@@Table[SpanFromBoth,3],Sequence@@(box@showMagRot[#]&/@elems)}};
  If[OptionValue["rotmat"]=!=False,
    brav=If[16<=mpgno[[1]]<=27, "HexaPrim", "CubiPrim"];
    If[OptionValue["cartesian"]===True,
      rots1=RotMatCart[StringReplace[#,"bar"->""]]&/@elems[[All,1]];  tmp="(cart.)",  
      rots1=getRotMat[brav,StringReplace[#,"bar"->""]]&/@elems[[All,1]];  tmp=Nothing
    ];
    rots1={Column[{"Rotation","matrix",tmp}], spl, Sequence@@MatrixForm/@rots1};
    rots2=ComplexExpand@First@getSpinRotOp[#]&/@elems[[All,1]];
    tmp="(\[DownArrow]\[UpArrow])";
    If[OptionValue["spin"]==="updown", 
       tmp="(\[UpArrow]\[DownArrow])"; rots2={{0,1},{1,0}}.#.{{0,1},{1,0}}&/@rots2
    ];
    rots2={Column[{"Spin"<>tmp,"rotation","matrix"}], spl, Sequence@@MatrixForm/@rots2};
    tab=Prepend[tab,rots1];  nstart=4;
    If[dbl=!=False, tab=Insert[tab,rots2,2];  nstart=5]; 
    , (*----else----*)
    nstart=3
  ];
  tab=Join[row1,tab];

  sty1=Directive[Black,Thickness[OptionValue["linewidth"]]];
  sty2=Directive[Thin,GrayLevel[0.8]];
  bg0={1,1}->Lighter[Red,0.9];
  bg1a={{2,nstart-1},{1,2}}->Lighter[Yellow,0.85];
  bg1={{1,nstart-1},{2,-1}}->Lighter[Yellow,0.9];
  sidx=Position[iridx,#][[1,1]]&/@Intersection[sidx,iridx];
  didx=Complement[Range@Length[iridx],sidx];
  bg2={{#,#}+nstart-1,{1,-1}}->Lighter[Green,0.95]&/@sidx;
  bg3={{#,#}+nstart-1,{1,-1}}->Lighter[Blue,0.95]&/@didx;
  bg4={{#,#}+nstart-1,{1,4}}->Lighter[Green,0.90]&/@sidx;
  bg5={{#,#}+nstart-1,{1,4}}->Lighter[Blue,0.90]&/@didx;

  grid=Grid[tab, Frame->All, Alignment->{Center,Center}, ItemSize->{{{},{2->3.1,3->2.9}},{}}, 
                 Dividers->{{{{sty2}},Join[#->sty1&/@{1,5,-1},#->sty2&/@{2}]},
                            {{{sty2}},#->sty1&/@{1,3,-1}}},
                 Background->{None,None,{bg0,bg1a,bg1,Sequence@@Join[bg2,bg3,bg4,bg5]}}
           ]
]


(* ::Subsection::Closed:: *)
(*MPGCorepDirectProduct*)


Options[MPGCorepDirectProduct]={"output"->1};
MPGCorepDirectProduct[mpg_, coreps1_, OptionsPattern[]]/;!VectorQ[Flatten[{coreps1}],MemberQ[{Rule,RuleDelayed},Head[#]]&]:=
  MPGCorepDirectProduct[mpg, coreps1, coreps1, "output"->OptionValue["output"]]
MPGCorepDirectProduct[mpg_, OptionsPattern[]]:=MPGCorepDirectProduct[mpg, All, All, "output"->OptionValue["output"]]
MPGCorepDirectProduct[mpg_,coreps1_,coreps2_,OptionsPattern[]]/;
 !Or@@(VectorQ[Flatten[{#}],MemberQ[{Rule,RuleDelayed},Head[#]]&]&/@{coreps1,coreps2}):=Module[{n,
  mpgno,mpgcor,d,cijk,sub,type,subidx,subidx1,err,cr1idx,cr2idx,label,ncr,outopt,out1,out2,out3,dim},
  
  n=checkMPGinput[mpg,"MPGCorepDirectProduct"];  mpgno=MPGinfo[[n,2]];
  mpgcor=getMPGCorep[mpgno,"double"->True,"trace"->True];
  sub=mpgcor["USubG"];   type=mpgcor["type"];   subidx=mpgcor["subidx"];
  label=mpgcor["label"];   ncr=Length[label];   dim=mpgcor["corep"][[All,1]];
  (*-------check input coreps1 and coreps2--------*)
  err:=Print["MPGCorepDirectProduct: Inputs coreps1 and coreps2 can be:\n",
           "An integer OR a list of integers for the corep sequence numbers. OR\n",
           "A span such as 1;;5.  OR  All.  Refer to\n",
           Grid[Prepend[Transpose@label,Range[ncr]], Frame->All,FrameStyle->Gray] ];
  If[coreps1===0||coreps2===0, err; Abort[]];
  cr1idx=If[IntegerQ[coreps1], {coreps1}, coreps1];
  If[!(VectorQ[cr1idx,IntegerQ]||Head[cr1idx]===Span||cr1idx===All), err; Abort[]]
  Check[cr1idx=Range[ncr][[DeleteCases[cr1idx,0]]],
    Print["MPGCorepDirectProduct: out of range! There are ",ncr," coreps in total."]; Abort[], {Part::partw,Part::take}];
  cr2idx=If[IntegerQ[coreps2], {coreps2}, coreps2];
  If[!(VectorQ[cr2idx,IntegerQ]||Head[cr2idx]===Span||cr2idx===All), err; Abort[]]
  Check[cr2idx=Range[ncr][[DeleteCases[cr2idx,0]]],
    Print["MPGCorepDirectProduct: out of range! There are ",ncr," coreps in total."]; Abort[], {Part::partw,Part::take}];
  (*--------check option "output"---------------*)
  outopt=OptionValue["output"];
  If[!MemberQ[{1,2,3,4},outopt],
    Print["MPGCorepDirectProduct: option \"output\" can be one of {1,2,3,4}:\n",
      "1. Output the Mulliken label.\n2. Output the Gamma label.\n",
      "3. Output the occurrence numbers of all coreps.\n4. Output all the above 1-3 results."
    ]; Abort[]
  ];
  (*--------------------------------------------*)

  subidx1=If[IntegerQ[#],#,#[[1]]]&/@subidx;
  cijk=PGIrepDirectProduct[sub[[1]],"output"->3];
  
  (* BC-Tab 7.8 *)
  d[cri_Integer,crj_Integer]:=Module[{typei,typej,typek,facij,fack,si,sj,sij,re},
    typei=type[[cri]];  typej=type[[crj]];  
    facij=1; If[typei=="b",facij*=2];  If[typej=="b",facij*=2];
    si=subidx[[cri]]; If[typei!="c", si={si}];
    sj=subidx[[crj]]; If[typej!="c", sj={sj}];
    sij=Flatten[Table[{i,j},{i,si},{j,sj}],1];
    fack=If[#!="b",1,1/2]&/@type;
    Total[cijk/@sij][[subidx1]]*facij*fack
  ];
  
  out3=Flatten[Table[{i,j}->d[i,j],{i,cr1idx},{j,cr2idx}],1]//Association;
  out1=label[[#,1]]->(If[#[[1]]==1,#[[2]],#]&/@Select[Transpose@{out3[#],label[[All,1]]},#[[1]]!=0&])&/@Keys[out3]//Association;
  out2=label[[#,2]]->(If[#[[1]]==1,#[[2]],#]&/@Select[Transpose@{out3[#],label[[All,2]]},#[[1]]!=0&])&/@Keys[out3]//Association;

  (* (*Check dimensions*)
  If[!And@@(Times@@dim[[#]]\[Equal]Total[out3[#]*dim])&/@Keys[out3], Print["MPGCorepDirectProduct: Corep dimensions error!"]];
  *)
  Switch[outopt, 1, out1, 2, out2, 3, out3, 4, {out1,out2,out3}]
]


(* ::Subsection::Closed:: *)
(*showMPGCorepDirectProduct*)


Options[showMPGCorepDirectProduct]={"label"->1, "double"->True, "linewidth"->2, "emph"->None};
showMPGCorepDirectProduct[mpg_, coreps1_, coreps2_, OptionsPattern[]]/;
 !Or@@(VectorQ[Flatten[{#}],MemberQ[{Rule,RuleDelayed},Head[#]]&]&/@{coreps1,coreps2}):=Module[{n,
  mpgno,mpgcor,ncor,dp,type,cor1,cor2,tmp,label,lopt,tab,scor1,dcor1,scor2,dcor2,sty1,sty2,bg0,bg1,
  bg2,bg3,bg4,bg5,s1pos,d1pos,s2pos,d2pos,emopt,emidx,emerr,head,rowh,box},
  lopt=OptionValue["label"];
  If[!MemberQ[{1,2},lopt],
    Print["showMPGCorepDirectProduct: option \"label\" can be 1 or 2:\n",
          "1. Output the Mulliken label.\n2. Output the Gamma label."]; Abort[]
  ];

  n=checkMPGinput[mpg,"showMPGCorepDirectProduct"];  mpgno=MPGinfo[[n,2]];
  mpgcor=getMPGCorep[mpg, "double"->True];
  label=mpgcor["label"];  ncor=Length[label];   type=mpgcor["type"];
  (*-------check option "emph"--------*)
  emopt=OptionValue["emph"];    If[emopt===None,emopt={}];
  emerr:=Print["showMPGCorepDirectProduct: option \"emph\" specifies the corep(s) to be emphasized and can be:\n",
           "An integer OR a list of integers for the corep sequence numbers. OR\n",
           "A span such as 1;;5.  OR  All.  Refer to\n",
           Grid[Prepend[Transpose@label,Range[ncor]], Frame->All,FrameStyle->Gray] ];
  If[emopt===0, emerr; Abort[]];
  emidx=If[StringQ[emopt]||IntegerQ[emopt], {emopt}, emopt];
  If[!(VectorQ[emidx,IntegerQ]||Head[emidx]===Span||emidx===All), emerr; Abort[]]
  Check[emidx=Range[ncor][[DeleteCases[emidx,0]]], 
    Print["showMPGCorepDirectProduct: out of range! There are ",ncor," coreps in total."]; Abort[], {Part::partw,Part::take}];

  dp=MPGCorepDirectProduct[mpg,coreps1,coreps2,"output"->4];
  cor1=Keys[dp[[3]]][[All,1]]//DeleteDuplicates;
  cor2=Keys[dp[[3]]][[All,2]]//DeleteDuplicates;
  scor1=Intersection[cor1,mpgcor["sindex"]];
  scor2=Intersection[cor2,mpgcor["sindex"]];
  dcor1=Complement[cor1,scor1];
  dcor2=Complement[cor2,scor2];
  If[OptionValue["double"]===False, cor1=scor1; cor2=scor2];
  tmp=Join@@(Position[dp,#]&/@label[[emidx,lopt]]);
  If[emidx!={}, dp=MapAt[Style[#,{Red,Bold}]&,dp,tmp]];
  dp=Row[#,"+"]&/@Map[If[ListQ[#],Row[#],#]&, dp[[lopt]], {2}];
  If[emidx!={}, dp=Style[#,Gray]&/@dp];

  tab=Table[dp[{label[[i,lopt]],label[[j,lopt]]}], {i,cor1}, {j,cor2}];
  tab=Transpose@Join[{cor1}, Transpose@label[[cor1]], {type[[cor1]]}, Transpose@tab];
  head=Column[{Row[{"MPG: ", mpgcor["number"][[2]],", ",MSGSymStd@mpgcor["symbol"]}],
        Row[{"USubG: ", MSGSymStd@mpgcor["USubG"][[3]], " (",str2Mulliken@mpgcor["USubG"][[2]],")"}]
      }];
  box=Grid[{{#}},Spacings->{0,0},Alignment->{Center,Center}]&;
  rowh={{head,SpanFromLeft,SpanFromLeft,SpanFromLeft,Sequence@@cor2},
        {SpanFromAbove,SpanFromBoth,SpanFromBoth,SpanFromBoth,Sequence@@box/@label[[cor2,lopt]]}};
  tab=Join[rowh,tab];

  tmp=Association@Table[cor1[[i]]->i+2,{i,Length[cor1]}];
  s1pos=tmp/@scor1;  d1pos=tmp/@dcor1;
  tmp=Association@Table[cor2[[i]]->i+4,{i,Length[cor2]}];
  s2pos=tmp/@scor2;  d2pos=tmp/@dcor2;
 
  sty1=Directive[Black,Thickness[OptionValue["linewidth"]]];
  sty2=Directive[Thin,GrayLevel[0.8]];
  bg0={1,1}->Lighter[Red,0.9];
  bg1=With[{c=Lighter[Green,0.85]}, Flatten@Join[Table[{#,i}->c,{i,4}]&/@s1pos,Table[{i,#}->c,{i,2}]&/@s2pos]];
  bg2=With[{c=Lighter[Blue,0.85]}, Flatten@Join[Table[{#,i}->c,{i,4}]&/@d1pos,Table[{i,#}->c,{i,2}]&/@d2pos]];
  bg3=Table[{i,j}->Lighter[Green,0.9],{i,s1pos},{j,s2pos}]//Flatten[#,1]&;
  bg4=Table[{i,j}->Lighter[Yellow,0.9],{i,d1pos},{j,d2pos}]//Flatten[#,1]&;
  bg5=Join[Table[{i,j}->Lighter[Blue,0.9],{i,s1pos},{j,d2pos}],
           Table[{i,j}->Lighter[Blue,0.9],{i,d1pos},{j,s2pos}]]//Flatten[#,1]&;

  Grid[tab, Frame->All, Alignment->{Center,Center}, ItemSize->{{{},{2->3.1,3->2.9}},{}}, 
            Dividers->{{{{sty2}},Join[#->sty1&/@{1,5,-1},#->sty2&/@{2}]},
                       {{{sty2}},#->sty1&/@{1,3,-1}}},
            Background->{None,None,{bg0,Sequence@@Join[bg1,bg2,bg3,bg4,bg5]}}
      ]
]

showMPGCorepDirectProduct[mpg_, coreps1_, OptionsPattern[]]/;!VectorQ[Flatten[{coreps1}],MemberQ[{Rule,RuleDelayed},Head[#]]&]:=
  showMPGCorepDirectProduct[mpg, coreps1, coreps1, (#->OptionValue[#])&/@{"label","double","linewidth","emph"}]
showMPGCorepDirectProduct[mpg_, OptionsPattern[]]:=
  showMPGCorepDirectProduct[mpg, All, All, (#->OptionValue[#])&/@{"label","double","linewidth","emph"}]


(* ::Section:: *)
(*MLG Corep (small corep)*)


(* ::Subsection::Closed:: *)
(*findMatrixN*)


(* find a unitary matrix N which satisfies \[CapitalDelta](R)=N*bar\[CapitalDelta](R)*N^-1, where bar\[CapitalDelta](R)=\[CapitalDelta](A^-1RA)^* *)
findMatrixN[Delta_, barDelta_]:=Module[{tryN,len,d,i,tmp,q,U1q},
  (*----------method 1, only for 1D rep---------------*)
  If[!MatrixQ[Delta[[1]]], Return[1]];
  (*----------method 2, maybe not work in some cases--------------*)
  len=Length[Delta];  d=Dimensions[Delta[[1]]]//First;
  tryN=Total@Table[Delta[[i]].barDelta[[i]]\[ConjugateTranspose],{i,len}]/len//Simplify[#,u\[Element]Reals]&; 
  If[Total@Flatten@Abs[tryN.tryN\[ConjugateTranspose]-IdentityMatrix[d]]<1*^-8, Return[tryN]]; 
  (*----------method 3, work in any case--------------*)
  For[i=1,i<=d,i++, 
    tmp=Total[Delta[[All,1,1]]\[Conjugate]*barDelta[[All,i,i]]]*d/len//Simplify[#,u\[Element]Reals]&//Chop;  
      If[Abs[tmp]>1*^-8,  q=i; U1q=Sqrt[tmp]; Break[]]
  ];
  tryN=Table[Total[Delta[[All,1,i]]\[Conjugate]*barDelta[[All,q,j]]],{i,d},{j,d}]*d/(len*U1q)//Simplify[#,u\[Element]Reals]&//Chop;
  If[Total@Flatten@Abs[tryN.tryN\[ConjugateTranspose]-IdentityMatrix[d]]>1*^-8, Print["findMatrixN: N is not unitary"]];
  tryN
]


(* ::Subsection:: *)
(*For type-3 MSG*)


(* ::Subsubsection::Closed:: *)
(*SeitzConvert*)


(*Convert the SG Seitz symbol from cell1 (with lattice brav1) to cell2 (with lattice brav2).
  Suppose that cell1=(t1,t2,t3) where t1,t2,t3 are column vectors of the basic vectors of cell1,
  then cell2=R.cell1.T and os is the components of the origin shift O1-O2 under the bases of cell2.
  T is the transformation matrix and R is the rotation matrix. 
  The returned value contains two Seitz symbols: the first for single SG, and the second for double SG.
  NOTE: If Rspin is not give, it will be calculated in SeitzConvert, however this will slow down
  this function. In practice, Rspin had better be calculated out of the function.  *)
SeitzConvert[brav1_String, brav2_String, T_, os_, R_]:=SeitzConvert[brav1, brav2, T, os, R, None]
SeitzConvert[brav1_String, brav2_String, T_, os_, R_, Rspin_]:=Function[{Rv},
 Module[{rot,rot2,v2,RSpin,Rang,Raxis,sx,srot,srot2,o3det,Rn2,sRn2,Rname,v,iT},
  sx={{0,1},{1,0}};   Rname=Rv[[1]];  v=Rv[[2]];
  rot=getRotMat[brav1,StringReplace[Rname,"bar"->""]];
  (* Making the matrics numeric will speed up the calculations! *)
  iT=Inverse[T//N];    rot2=iT.rot.T;    v2= iT.v-rot2.os+os;
  If[Rspin=!=None,  RSpin=Rspin,
    {Raxis,Rang}=rotAxisAngle[R//N][[{2,3}]];       
    RSpin=MatrixExp[-I*Rang*Raxis.(PauliMatrix/@{1,2,3})/2]//Simplify
  ];
  {srot,o3det}=getSpinRotOp[Rname];
  srot2=sx.RSpin.sx.srot.sx.RSpin\[ConjugateTranspose].sx//Simplify;
  Rn2=getRotName[brav2,rot2];
  sRn2=getSpinRotName[brav2,{srot2,o3det}];
  If[StringReplace[sRn2,"bar"->""]!=Rn2, Print["SeitzConvert: error, ",Rn2,"!=",sRn2]];
  {{Rn2,v2}, {sRn2,v2}}//Chop//Rationalize
 ]
]


(* ::Subsubsection:: *)
(*getType3MLGCorep*)


Options[getType3MLGCorep]={"format"->True, "abcOrBasVec"->None, "trace"->False};
getType3MLGCorep[{sgno_,num_}, k_, OptionsPattern[]]:=
  getType3MLGCorep[{sgno,num}, k, "a", (#->OptionValue[#])&/@{"format","abcOrBasVec","trace"}]
getType3MLGCorep[{sgno_,num_}, k_, BZtype_String, OptionsPattern[]]:=
 Module[{brav,MSG,MLG,Gno,antiU,reptab,Gk,getrep,square,format=OptionValue["format"],BZtypeOrBasVec,BZs,
  stypes,dGk,dsquare,dantiU,dtypes,brav2,T,os,R,Rang,Raxis,Rspin,sx,k2,tmp,conv,kinfo,kname,Sw,kco,usub,
  bztype,ckbv,fullBZtype,kcoBZs,tmp2,bvec=None,mytr,sct, dct, srep, drep, info, A,R2Rv, slb,slb2,pos,repi,
  dlb,dlb2,sidx,didx,i,time,inv,scorep,dcorep,stypes2,dtypes2,MLG0,MLG0c,dMLG0c,sct2,dct2,iARA,barDelta,
  Delta,BA,iAB,BiA,matN,DeltaBiA,paired,k2name,slabel,dlabel,realSimplify,k2info,deltau,fixrep,sNs,dNs,u2},
  
  (* Here use FullSimplify will cause problem, e.g. Root appears for S4x+ of {230,147},"P"*)
  realSimplify[x_]:=Chop@Simplify[x,u\[Element]Reals]; 
  MSG=getType3MSGElem[{sgno,num}];    
  info=<||>;   info["symbol"]={MSGSymStd[{sgno,num}],{sgno,num},MSGSymBC[{sgno,num}]};
  
  (*-----------Proess BZ type or basic vectors-------------*)
  brav=getSGLatt[sgno];
  bztype=If[MemberQ[{"OrthBase","OrthBody","OrthFace","TetrBody","TrigPrim"},brav],BZtype,""];
  BZtypeOrBasVec=bztype;
  If[(tmp=OptionValue["abcOrBasVec"])=!=None,
    bvec=If[Position[tmp,Rule]!={}, BasicVectors[brav]/.tmp, tmp];
    ckbv=checkBasVec[brav, bvec];  
    If[ckbv[[1]]=!=True, Print["getType3MLGCorep: basic vectors ",bvec," do not belong to ",
                          brav, "."];   Abort[]];
    bztype=ckbv[[2]];  BZtypeOrBasVec=bvec;
  ];
  fullBZtype=brav<>If[bztype!="", "("<>bztype<>")", ""]; 

  usub=u->1/10;   (* the default value, may be updated if k is a numeric vector *)
  (*-----------------Process k name and k coordinates---------------*)
  If[VectorQ[k],
    If[!VectorQ[k,NumericQ], 
        Print["getType3MLGCorep: the k coordinates ",k," should be numeric."]; Abort[]];
    kinfo=identifyBCHSKptBySG[sgno,BZtypeOrBasVec,k];    
    (*------------------for debug---------------*)
    (*(*\:68c0\:67e5{S|w}\:662f\:5426\:4f9d\:7136\:80fd\:4f7f\:78c1\:7fa4\:81ea\:5171\:8f6d\:ff0c\:53ea\:9700\:9a8c\:8bc1\:5176\:5e7a\:6b63\:5b50\:7fa4\:5373\:53ef\:ff0c\:7ecftestklist\:6d4b\:8bd5\:672a\:53d1\:73b0\:95ee\:9898*)
    If[kinfo[[2]]!="UN"&&kinfo[[2]]!="GP"&&(Sw=kinfo[[6]])!={"E",{0,0,0}},  
       tmp=Select[MSG,#[[3]]==0&][[All,{1,2}]];
       time=SeitzTimes[brav]; inv=invSeitz[brav];
       tmp2=time[inv[Sw],time[#,Sw]]&/@tmp;  
       If[!seteq[modone[tmp],modone[tmp2]], Print["MSG ",sgno,".",num,
              " inv(Sw)*MSG*Sw != MSG: k=",k,", kname=",kinfo[[2]],"{S|w}=",Sw]]
    ];*)
    (*-----------------------------------------*)
    If[Position[kinfo,u]!={}, usub=kinfo[[9]]];  
    kname=kinfo[[2]];  kco=k;
    If[kname!="GP"&&kname!="UN",
      If[brav=="TrigPrim"&&kname=="F", BZs={fullBZtype}, 
        kcoBZs=kBCcoord[sgno,kname];   pos=Position[kcoBZs,fullBZtype][[1,1]]; BZs=kcoBZs[[pos,2]]
      ],
      BZs=Union@@kBCcoord[sgno,"\[CapitalGamma]"][[All,2]]
    ],
    (*------else: k is name-------*)
    kname=k;
    kcoBZs=kBCcoord[sgno,k];   pos=Position[kcoBZs,fullBZtype];
    If[pos!={}, 
      kco=kcoBZs[[pos[[1,1]],1]]; BZs=kcoBZs[[All,2]]//Flatten,
      kco=kcoBZs[[1,1]];  BZs=kcoBZs[[All,2]]//Flatten;
      bztype=If[bztype=="", bztype, StringReplace[BZs[[1]],{__~~"("->"",")"->""}]];
      getType3MLGCorep::BZchange="Warning, k-point "<>k<>" exists only in `1`, not in `2` for "<>ToString[{sgno,num}];
      Message[getType3MLGCorep::BZchange,BZs,fullBZtype];
    ];
    kinfo=identifyBCHSKptBySG[sgno,bztype,kco/.usub];  
  ];
  If[kname=="aF"||kname=="bF",kname="F"];
  If[Position[kinfo,u]!={}, kco=kinfo[[7]]+kinfo[[8]]];  (* update kco with u *)
  info["k"]=If[VectorQ[k]&&Position[kco,u]!={}, {kco,kname,BZs,usub}, {kco,kname,BZs}];
  info["kinfo"]=kinfo; 
  
  MLG=getMLGElem[brav,MSG,kco];  
  {Gno,T,os,R}=type3USubInfo[{sgno,num}];  brav2=getSGLatt[Gno]; 
  MLG0=Select[MLG,#[[3]]==0&][[All,{1,2}]];  
  antiU=Select[MLG,#[[3]]==1&][[All,{1,2}]];  
  k2=T\[Transpose].kco;   (*convert k-point coordinates from MSG to its unitary subgroup.*) 
  If[bvec=!=None, bvec=(R.bvec\[Transpose].T)\[Transpose]//Simplify];  (*convert the basic vectors to the subgroup's ones*)
  If[MemberQ[{"TricPrim","MonoPrim","MonoBase"},brav2], bvec=None]; (*avoid checkBasVec for Tric and Mono*)
  reptab=getLGIrepTab[Gno,k2/.usub,"abcOrBasVec"->bvec,"format"->False]; 
  tmp=type3kpairDict[{{sgno,num},kname}];
  If[MissingQ[tmp], repi=1,
    (*If[AssociationQ[tmp],tmp=tmp[kinfo[[7]]]];*)
    (*\:4e0a\:9762\:4e00\:53e5\:6539\:6210\:5982\:4e0b\:ff0ckinfo[[7]]\:53ef\:80fd\:4e0d\:5728tmp\:7684Keys\:4e2d\:ff0c\:4f46\:662f\:5374\:4e0e\:5176\:4e2d\:4e00\:4e2aKey\:7b49\:4ef7\:ff0c\:6bd4\:5982 {93,123},{9/10,9/10,1/2}*)
    tmp2=If[AssociationQ[tmp], Values@KeySelect[tmp,keqmod[#,kinfo[[7]]]&]//First, tmp];
    (*\:5bf9\:4e8e\:6307\:5b9a\:4e86\:6676\:683c\:5e38\:6570\:4e14u\[Equal]umax\:7684\:4e34\:754c\:60c5\:51b5\:ff0c\:8bc6\:522bk\:70b9\:65f6\:4ece\:4e24\:4e2a\:540d\:79f0\:91cc\:9009\:4e86\:4e00\:4e2a\:8fdb\:884c\:8f93\:51fa\:ff08\:5373reptab\:91cc\:7684\:ff09\:ff0c
      \:5982\:679c\:5b83\:4e0e\:5e94\:8be5\:914d\:5bf9\:7684tmp2\:4e0d\:540c\:ff0c\:5219\:4f1a\:89e6\:53d1\:4e0b\:9762\:8bed\:53e5\:4e2d\:9519\:8bef\:ff0c\:6b63\:5e38\:60c5\:51b5\:4e0b\:4e0d\:5e94\:51fa\:73b0\:6b64\:60c5\:5f62*)
    repi=Check[Select[Range[Length[reptab]], reptab[[#]]["kBZs"][[1,1]]==tmp2&]//First, 
          Print["getType3MLGCorep: MSG ",{sgno,num},", MLG ",kinfo[[{1,2}]]," expects ",tmp2//InputForm," out of ",
                tmp, ". But the reptab for unitary subgroup only has ", #["kBZs"][[1,1]]&/@reptab]; Abort[],
          {First::nofirst}];
  ]; (* Select the k-point name of the subgroup, i.e. kname2, according to type3kpairDict *)
  reptab=reptab[[repi]];
  Gk=reptab["Gkin"];    dGk=Join[Gk,{"bar"<>#[[1]],#[[2]]}&/@Gk];  
  k2name=reptab["kBZs"][[1,1]];
  k2info=reptab["kinfo"];  
  If[Position[kinfo,u]!={}&&Position[k2info,u]!={}&&Position[k2,u]=={}, k2=k2info[[7]]+k2info[[8]]]; 
  info["ksub"]={k2,k2name,reptab["kBZs"][[1,3]]};
  info["ksubinfo"]=k2info;  
  info["USubSG"]=Gno;
  info["transformation_matrix"]=T;  info["origin_shift"]=os; info["rotation_matrix"]=R;
  MLG=Join[Append[#,0]&/@MLG0,Append[#,1]&/@antiU];    
  info["MLG"]=MLG;
    
  (* getrep works for both characters and representation matrices*)  
  getrep[rep_][{R_,v_}]:=Module[{ch, trans},
    trans=Association[Rule@@@Gk];
    ch=Association[Rule@@@Transpose[{Gk[[All,1]],rep}]];
    (trans["bar"<>#]=trans[#])&/@Gk[[All,1]];
    (ch["bar"<>#]=-ch[#])&/@Gk[[All,1]]; 
    ch[R]*Exp[-I*k2.(v-trans[R])*2Pi]//realSimplify
  ];
  
  sx={{0,1},{1,0}};
  {Raxis,Rang}=rotAxisAngle[R//N][[{2,3}]];    
  Rspin=MatrixExp[-I*Rang*Raxis.(PauliMatrix/@{1,2,3})/2]//Simplify; 

  conv=SeitzConvert[brav,brav2,T,os,R,Rspin];     
  tmp=DSGSeitzTimes[brav][#,#]&/@antiU;    tmp=conv/@tmp;  
  square={StringReplace[#1,"bar"->""],#2}&@@@tmp[[All,1]]; 
  dsquare=tmp[[All,2]];   
  {MLG0c,dMLG0c}=(conv/@MLG0)\[Transpose];  
  
  mytr[m_]:=If[MatrixQ[m],Tr[m],m];
  srep=reptab["sirep"];   sct=Map[mytr,srep,{2}];
  drep=reptab["direp"];   dct=Map[mytr,drep,{2}];
  slb=reptab["slabel"][[All,2]];   dlb=reptab["dlabel"][[All,2]];
  
  (* If k is HS k-point (no u in kinfo) and k2 is on HS line (k2info has u), u should be
   substituted by its value in k2info. This occurs mainly at k={1/4,1/4,1/4}, such as in
   45.238, 46.243, 98.161, 110.247, 120.323, ... *)
  If[Position[kinfo,u]=={}&&Position[k2info,u]!={},
    srep=srep/.k2info[[9]]//Simplify;    sct=sct/.k2info[[9]]//Simplify;
    drep=drep/.k2info[[9]]//Simplify;    dct=dct/.k2info[[9]]//Simplify;
  ];
                                            
  (* If kco has u (not numeric) the IRs of k2 should also keep u alive. But the IRs in reptab
     (for k2) is numeric for "UN" and "GP" cases. This should be fixed. *)
  If[Position[kco,u]!={}&&(k2name=="UN"||k2name=="GP"),
     deltau=1/10;
     fixrep[repu1_, repu2_]:=Module[{c1,c2},
       If[ListQ[repu1], Return[fixrep[#1,#2]&@@@Transpose[{repu1,repu2}]]];
       If[Abs[repu1-repu2]<1*^-12, Return[(repu1+repu2)/2//Chop]];
       c1=Log[repu2/repu1]*I/(2Pi*deltau)//FullSimplify;
       c2=repu1*Exp[I*2Pi*c1*1/10]//FullSimplify//Chop;   (* u1=1/10 *)
       (*FullSimplify is needed, or else \[ExponentialE]^(-\[ImaginaryI]\[Pi]u) will become (-(-1)^(9/10))^(10u) for {40,205}, "H" *)
       c2*Exp[-I*2Pi*c1*u]//Chop
     ];
     tmp=getLGIrepTab[Gno,k2/.u->1/10,"abcOrBasVec"->bvec,"format"->False][[repi]]; 
     tmp2=getLGIrepTab[Gno,k2/.u->(1/10+deltau),"abcOrBasVec"->bvec,"format"->False][[repi]];  
     srep=fixrep[tmp["sirep"], tmp2["sirep"]];
     drep=fixrep[tmp["direp"], tmp2["direp"]];
  ];
  
  (*When u existes in both kinfo and k2info but has different meanings, the u in kinfo
  should be used in the final results. So some data should be updated in this case. *)
  If[Position[k2info,u]!={}&&Position[kco,u]!={},
     tmp=k2info[[7]]+k2info[[8]];   
     If[k2!=tmp/.u->0.05, (* has to be numeric, or else no result for the comparison *)
        tmp2=Solve[(tmp/.u->u2)==k2,u2][[1,1]]/.u2->u;   
        k2info[[9]]=tmp2;   k2info[[1]]=k2;   
         (* update k2info, srep, drep, sct and dct *)
        k2info=MapAt[#/.u->\[Alpha]&, k2info, {{5},{7},{9,1}}];
        info["ksubinfo"]=k2info;  
        srep=srep/.tmp2;    sct=sct/.tmp2;  
        drep=drep/.tmp2;    dct=dct/.tmp2;
     ];
  ];
  
  stypes=Table[getrep[ir]/@square//Total,{ir,sct}]/Length[Gk]//Simplify;   (*BC-Eq. 7.6.41*)
  dtypes=Table[-getrep[ir]/@dsquare//Total,{ir,dct}]/Length[Gk]//Simplify;  
  (* The above Simplify is needed, e.g. for {187,211},"\[CapitalDelta]" *)
  stypes=Rationalize[stypes,0.1]/.{1->"a",-1->"b",0->"c"};
  dtypes=Rationalize[dtypes,0.1]/.{1->"a",-1->"b",0->"c"}; 
  
  If[antiU=={}, (*----------for type x------------*)
    stypes=stypes/.{"c"->"x"}; dtypes=dtypes/.{"c"->"x"};  
    stypes2=stypes;   dtypes2=dtypes;
    slb2=slb;   dlb2=dlb;   
    sidx=Range@Length[slb];  didx=Range@Length[dlb];
    scorep=Table[getrep[ir]/@MLG0c, {ir,srep}];
    dcorep=Table[getrep[ir]/@dMLG0c, {ir,drep}];
    info["A"]=None;    sNs=Table[None,Length[slb]];  dNs=Table[None,Length[dlb]];
    Goto["end"]  (* Goto ========================> end *)
  ];
  
  R2Rv[_]=None;   (R2Rv[#[[1]]]=#)&/@antiU;
  tmp=DeleteCases[R2Rv/@{"I","\[Sigma]z","C2z","\[Sigma]x","\[Sigma]y","\[Sigma]h"},None];  (*the priority of the rotation of A*)
  A=If[tmp!={}, tmp[[1]], antiU[[1]]];  
  info["A"]=Append[A,1];
  
  (*------------------------for single-valued corepresentations---------------------*)
  time=SeitzTimes[brav];  inv=invSeitz[brav];  
  tmp=time[inv[A],time[#,A]]&/@MLG0;    
  iARA=(conv/@tmp)[[All,1]];        
  BA=conv[time[#,A]][[1]]&/@antiU;      
  iAB=conv[time[inv[A],#]][[1]]&/@antiU;   
  BiA=conv[time[#,inv[A]]][[1]]&/@antiU;   
  sct2=Table[getrep[ir]/@MLG0c,{ir,sct}];   
  
  slb2=sidx=scorep=stypes2=sNs=paired={}; 
  For[i=1,i<=Length[slb],i++,  
    If[MemberQ[paired,i], Continue[]];
    Delta=getrep[srep[[i]]]/@MLG0c;
    barDelta=getrep[srep[[i]]][#]\[Conjugate]&/@iARA//realSimplify;
    If[MatchQ[stypes[[i]],"a"|"b"],
      matN=findMatrixN[Delta,barDelta];   AppendTo[sNs,matN];
      DeltaBiA=getrep[srep[[i]]]/@BiA;
      (* (*------ check matN, for debug-------*)
      Print["SV type ",stypes[[i]],":  NN^*=", If[MatrixQ[matN],matN.matN\[Conjugate],matN*matN\[Conjugate]],
            ", Delta(A^2)=",getrep[srep[[i]]][conv[time[A,A]][[1]]]];  *)
    ];
    Switch[stypes[[i]],
      "a", AppendTo[slb2,slb[[i]]]; AppendTo[sidx,i]; AppendTo[stypes2,"a"];
           tmp=If[MatrixQ[matN], #.matN&/@DeltaBiA, DeltaBiA*matN]//realSimplify;
           AppendTo[scorep, Join[Delta,tmp]]; 
           ,
      "b", AppendTo[slb2,slb[[i]]]; AppendTo[sidx,i]; AppendTo[stypes2,"b"];
           tmp=If[MatrixQ[matN], #.matN&/@DeltaBiA, DeltaBiA*matN]//realSimplify;
           tmp=ArrayFlatten[{{0,-#},{#,0}}]&/@tmp;
           AppendTo[scorep, Join[ArrayFlatten[{{#,0},{0,#}}]&/@Delta,tmp]]; 
           ,
      "c", If[Length[slb]==2, slb2={slb}; sidx={{1,2}}; paired={2},
             tmp=If[MatrixQ[barDelta//First], Tr/@barDelta, barDelta]//realSimplify;
             tmp2=Position[Simplify@Total@Abs[tmp-#]<1*^-5&/@sct2,True][[1,1]];  
             AppendTo[slb2,{slb[[i]],slb[[tmp2]]}];
             AppendTo[sidx,{i,tmp2}];   AppendTo[paired,tmp2];
           ];
           AppendTo[stypes2,"c"];   AppendTo[sNs,None];
           tmp=getrep[srep[[i]]][#]&/@BA;    (* Delta(BA) *)
           tmp2=getrep[srep[[i]]][#]\[Conjugate]&/@iAB//realSimplify;   (* Delta(A^-1*B)^* *)
           tmp2=ArrayFlatten[{{0,#1},{#2,0}}]&@@@Transpose[{tmp,tmp2}];
           tmp=ArrayFlatten[{{#1,0},{0,#2}}]&@@@Transpose[{Delta,barDelta}];
           AppendTo[scorep, Join[tmp, tmp2]];   
    ];
  ];
  
  (*------------------------for double-valued corepresentations---------------------*)
  time=DSGSeitzTimes[brav];  inv=DSGinvSeitz[brav];  
  tmp=time[inv[A],time[#,A]]&/@MLG0;    
  iARA=(conv/@tmp)[[All,2]];      
  BA=conv[time[#,A]][[2]]&/@antiU;     
  iAB=conv[time[inv[A],#]][[2]]&/@antiU;
  BiA=conv[time[#,inv[A]]][[2]]&/@antiU;
  dct2=Table[getrep[ir]/@dMLG0c,{ir,dct}];  
  (*Note that here we use the multiplication of DSG, but in fact the multiplication of DMSG 
  is needed. So a fix has to be done for BA. Note that for double MSG, theta^2\[Equal]barE in which
  theta is the time reversal.*)
  BA=If[StringTake[#1,1]=="b", {StringTake[#1,{4,-1}],#2}, {"bar"<>#1,#2}]&@@@BA;
  
  dlb2=didx=dcorep=dtypes2=dNs=paired={}; 
  For[i=1,i<=Length[dlb],i++,  
    If[MemberQ[paired,i], Continue[]];
    Delta=getrep[drep[[i]]]/@dMLG0c;
    barDelta=getrep[drep[[i]]][#]\[Conjugate]&/@iARA//realSimplify;
    If[MatchQ[dtypes[[i]],"a"|"b"],
      matN=findMatrixN[Delta,barDelta];    AppendTo[dNs,matN];
      DeltaBiA=getrep[drep[[i]]]/@BiA;
      (*(*------ check matN, for debug -------*)
      Print["DV type ",dtypes[[i]],":  NN^*=", If[MatrixQ[matN],matN.matN\[Conjugate],matN*matN\[Conjugate]],
            ", Delta(A^2)=",getrep[drep[[i]]][conv[time[A,A]][[2]]]];  *)
    ];
    Switch[dtypes[[i]],
      "a", AppendTo[dlb2,dlb[[i]]]; AppendTo[didx,i]; AppendTo[dtypes2,"a"];
           tmp=If[MatrixQ[matN], #.matN&/@DeltaBiA, DeltaBiA*matN]//realSimplify;
           AppendTo[dcorep, Join[Delta,tmp]]; 
           ,
      "b", AppendTo[dlb2,dlb[[i]]]; AppendTo[didx,i]; AppendTo[dtypes2,"b"];
           tmp=If[MatrixQ[matN], #.matN&/@DeltaBiA, DeltaBiA*matN]//realSimplify;
           tmp=ArrayFlatten[{{0,-#},{#,0}}]&/@tmp;
           AppendTo[dcorep, Join[ArrayFlatten[{{#,0},{0,#}}]&/@Delta,tmp]]; 
           , 
      "c", If[Length[dlb]==2, dlb2={dlb}; didx={{1,2}}; paired={2},
             tmp=If[MatrixQ[barDelta//First], Tr/@barDelta, barDelta]//realSimplify;
             tmp2=Position[Simplify@Total@Abs[(tmp-#)]<1*^-5&/@dct2,True][[1,1]];
             AppendTo[dlb2,{dlb[[i]],dlb[[tmp2]]}];
             AppendTo[didx,{i,tmp2}];   AppendTo[paired,tmp2];
           ];
           AppendTo[dtypes2,"c"];    AppendTo[dNs,None];
           tmp=getrep[drep[[i]]][#]&/@BA;    (* Delta(BA) *)
           tmp2=getrep[drep[[i]]][#]\[Conjugate]&/@iAB//realSimplify;   (* Delta(A^-1*B)^* *)
           tmp2=ArrayFlatten[{{0,#1},{#2,0}}]&@@@Transpose[{tmp,tmp2}];
           tmp=ArrayFlatten[{{#1,0},{0,#2}}]&@@@Transpose[{Delta,barDelta}];
           AppendTo[dcorep, Join[tmp, tmp2]];   
    ];
  ];
  
  Label["end"];
  
  slabel=("("<>kname<>")"<>Switch[#1,"a"|"x",#2,"b",#2<>#2,"c",#2[[1]]<>#2[[2]]])&
          @@@Transpose[{stypes2,slb2}];
  dlabel=("("<>kname<>")"<>Switch[#1,"a"|"x",#2,"b",#2<>#2,"c",#2[[1]]<>#2[[2]]])&
          @@@Transpose[{dtypes2,dlb2}];
  
  If[OptionValue["trace"]===True, 
    scorep=Map[If[MatrixQ[#],Tr[#],#]&, scorep, {2}]//Simplify;
    dcorep=Map[If[MatrixQ[#],Tr[#],#]&, dcorep, {2}]//Simplify;
  ];
  info["sidx"]=sidx;        info["stype"]=stypes2;    
  info["slabel"]=slabel;    info["smatN"]=sNs;           
  info["scorep"]=If[format===True, Map[formatRepMat,scorep,{2}], scorep];
  info["didx"]=didx;        info["dtype"]=dtypes2;    
  info["dlabel"]=dlabel;    info["dmatN"]=dNs;           
  info["dcorep"]=If[format===True, Map[formatRepMat,dcorep,{2}], dcorep];
  
  (*(*for debug*)
  Print["scorep:\n", (MatrixForm/@#)&/@scorep//Chop//TableForm[#,TableHeadings->{slabel,showMSGSeitz/@MLG}]&];
  Print["dcorep:\n", (MatrixForm/@#)&/@dcorep//Chop//TableForm[#,TableHeadings->{dlabel,showMSGSeitz/@MLG}]&];
  *)
  info
]


(* ::Subsubsection::Closed:: *)
(*showType3MLGCorep*)


Options[showType3MLGCorep]={"uNumeric"->False,"corep"->All,"elem"->All,"rotmat"->True,"trace"->False,
                        "spin"->"downup","abcOrBasVec"->None,"linewidth"->2,"maxDim"->4};
showType3MLGCorep[{sgno_,mno_}, kNameOrCoord_, OptionsPattern[]]:=showType3MLGCorep[{sgno,mno},
    kNameOrCoord, "a", (#->OptionValue[#])&/@{"uNumeric","corep","elem","rotmat","trace","spin",
    "abcOrBasVec","linewidth","maxDim"}]
showType3MLGCorep[{sgno_,mno_}, kNameOrCoord_, BZtype_, OptionsPattern[]]:=Block[{u,crinfo,brav,
  showmat,idxsir,idxdir,idxelm,showrot,sx,kname, k2name, kBZ, k2BZ, kinfo, k2info, slbl,stype,scorep,
  dlbl,dtype,dcorep, MLG,subno,R,T,os, Rang,Raxis,small,bold,dmax,rmRe0,rot,trans,srot,table,nelem,
  nsir,ndir,sfl,sfa,nc,nr,nfrom,head,h1,h2,h3,h4,h5,grid,sty1,sty2,thickHLines,tmp,nsir1,ndir1,
  bg1,bg2,bg3,bg4,bg5,bg6,nstart,emph,strkBC,symstd,symBC},
  crinfo=getType3MLGCorep[{sgno,mno},kNameOrCoord,BZtype,(#->OptionValue[#])&/@{"abcOrBasVec","trace"}];
  brav=getSGLatt[sgno];
  dmax=OptionValue["maxDim"];
  rmRe0=Map[If[MachineNumberQ[#],If[Re[#]==0,Which[Im[#]==1,I,Im[#]==-1,-I,True,Im[#]"\[ImaginaryI]"],#],#]&, #, -1]&;
  showmat[m_]:=If[!MatrixQ[m], rmRe0[m], If[Length[m]<=dmax, MatrixForm[rmRe0[m]], 
      tmp=Row[{"("<>ToString[#[[1,1]]]<>","<>ToString[#[[1,2]]]<>")",#[[2]]},":"]&;
      Partition[tmp/@ArrayRules[rmRe0[m]][[;;-2]],UpTo[2]]//Grid[#,Alignment->Left,ItemSize->Full]&
    ]];  
  showrot=OptionValue["rotmat"];
  sx={{0,1},{1,0}};

  kBZ=crinfo["k"];        kname=kBZ[[2]];      kinfo=crinfo["kinfo"];     
  k2BZ=crinfo["ksub"];    k2name=k2BZ[[2]];    k2info=crinfo["ksubinfo"];     
  slbl=crinfo["slabel"];     stype=crinfo["stype"];     scorep=crinfo["scorep"];
  dlbl=crinfo["dlabel"];     dtype=crinfo["dtype"];     dcorep=crinfo["dcorep"];
  MLG=crinfo["MLG"];  subno=crinfo["USubSG"];
  
  If[OptionValue["uNumeric"]&&VectorQ[kNameOrCoord]&&Position[kinfo,Rule]!={}, 
     scorep=scorep/.kinfo[[9]];  dcorep=dcorep/.kinfo[[9]]];
  nelem=Length[MLG];   nsir=Length[scorep];   ndir=Length[dcorep];
  tmp=Check[Range[nsir+ndir][[If[IntegerQ[tmp=OptionValue["corep"]], {tmp}, tmp]]],
             Print["showType3MLGCorep: index of corep out of range [1,",nsir+ndir,"]"]; Abort[],
             {Part::partw, Part::pkspec1}];
  idxsir=Select[tmp,#<=nsir&];    idxdir=Complement[tmp,idxsir];
  nsir1=Length[idxsir];    ndir1=Length[idxdir];
  idxelm=Check[Range[nelem][[If[IntegerQ[tmp=OptionValue["elem"]], {tmp}, tmp]]],
             Print["showType3MLGCorep: index of element out of range [1,",nelem,"]"]; Abort[],
             {Part::partw, Part::pkspec1}];

  small=Style[#,Smaller]&;  bold=Style[#,Bold]&;  emph=Style[#,Bold,Blue]&;
  strkBC="\!\(\*SubscriptBox[\(k\), \(BC\)]\)";
  {symstd,symBC}=crinfo["symbol"][[{1,3}]];
  tmp=If[symBC=!=symstd,Row[{Row[{emph@symstd,","}],"","BC:",emph@symBC}," "], emph@symstd];
  h1=Row[{"MSG ",emph@Row[{sgno,".",mno}],"(",tmp,"):  k-point name is ",
          emph[kname],", for ",Row[kBZ[[3]],","]}," "];
  h2=Row[{"\[FilledRightTriangle]  k=(",Row[kBZ[[1]],","],")"}];
  If[Position[kinfo,Rule]!={}&&VectorQ[kNameOrCoord], h2=Row[{h2,"  (u=",kinfo[[9,2]],")"}]];
  If[kname!="UN"&&kname!="GP"&&kinfo[[6,1]]!="E", 
     h2=Row[{h2," non-standard, ",strkBC,"=(",Row[kinfo[[5]],","],")  [",
             showSeitz[kinfo[[6]]][[1,2]],"]",strkBC," \[DoubleLeftRightArrow] k"}],
     If[kname!="UN"&&kname!="GP", 
       tmp=If[kinfo[[8]]=={0,0,0}, Nothing, Row[{",  ",strkBC,"=(",Row[kinfo[[5]],","],")"}]];
       h2=Row[{h2," standard",tmp}]]
  ];
  R=crinfo["rotation_matrix"];  T=crinfo["transformation_matrix"]; os=crinfo["origin_shift"];
  tmp=rotAxisAngle[R];  Rang=tmp[[3]];  Raxis=tmp[[2]]//Round;
  Rang=Switch[Rang, Pi,"\[Pi]", Pi/2,"\[Pi]/2", Pi/4,"\[Pi]/4", 2Pi/3,"2\[Pi]/3", _,Rang];
  h3=Row[{"The unitary subgroup is SG ",emph[subno],"(",emph@SGSymStd[subno],
       "), whose standard BC basic vectors are\n(",
       Row[Subsuperscript[bold["t"],#,"\[Prime]"]&/@{1,2,3}],")=R(",
       Row[Subscript[bold["t"],#]&/@{1,2,3}],")T with origin shift Os=(", Row[os,","],") and\n", 
       Column[{"transformation","matrix"},Alignment->Center],  "  T=", MatrixForm[T], "  and  ",       
       Column[{"rotation","matrix"},Alignment->Center], "  R=", MatrixForm[R], " ",
       If[Rang!=0, small@Column[{small["axis"],Row[If[#<0,OverBar[-#],#]&/@Raxis],small["angle"],Rang},Alignment->Center,Spacings->0.2], Nothing]       
       }];
  tmp=If[Position[k2info,\[Alpha]]=={},"", Row[{"  (", k2info[[9,1]],"=",k2info[[9,2]],")"}]];
  (* To check the above line, use {75,27}, {0.1,0.1,0.4} and -{0.1,0.1,0.4} *)
  h4=Row[{"k-point of the subgroup is ",emph[k2name]," for ",If[Length[#]>1,#,#[[1]]]&@k2BZ[[3]],
       "\n\[FilledRightTriangle]  ", Superscript["k","\[Prime]"],"=T\[Transpose].k=(",Row[k2BZ[[1]],","],")",
       If[k2name!="UN"&&k2name!="GP"&&k2info[[6,1]]!="E", 
         Row[{" non-standard, "<>strkBC<>"=(",Row[k2info[[5]],","],")",tmp,"  [",
               showSeitz[k2info[[6]]][[1,2]],"]"<>strkBC<>" \[DoubleLeftRightArrow] ",Superscript["k","\[Prime]"]}],
         If[k2name=="UN"||k2name=="GP", "", 
           If[Position[k2info,\[Alpha]]=={}, " standard", 
             Row[{" standard, "<>strkBC<>"=(",Row[k2info[[5]],","],")",tmp}] ]
         ]
       ]
     }];
  head={h1,h2,h3,h4}//Flatten//Column;
    
  rot=MatrixForm[getRotMat[brav,#]]&/@MLG[[idxelm,1]];
  trans=MatrixForm[{InputForm/@#}\[Transpose]]&/@MLG[[idxelm,2]];
  srot=getSpinRotOp[#][[1]]&/@MLG[[idxelm,1]];
  If[OptionValue["spin"]=="updown", srot=sx.#.sx&/@srot];
  srot=MatrixForm[Expand[#]]&/@srot;
  sfl=SpanFromLeft;   sfa=SpanFromAbove;
  table={idxelm, showMSGSeitz/@MLG[[idxelm]], 
         Sequence@@If[showrot,{rot, srot},{}], 
         Sequence@@Map[showmat,scorep[[idxsir,idxelm]],{2}],
         Sequence@@Map[showmat,dcorep[[idxdir-nsir,idxelm]],{2}]};
  {nr,nc}=Dimensions[table];   
  table=Join[Table[sfl,3,nr],table\[Transpose]]\[Transpose];
  table[[1,1]]="Index";     table[[2,1]]="Element";   
  If[showrot,
    table[[3,1]]=Column[{"Rotation","matrix"},ItemSize->Full];
    table[[4,1]]=Column[{If[OptionValue["spin"]=="updown","Spin(\[UpArrow]\[DownArrow])","Spin(\[DownArrow]\[UpArrow])"],"rotation","matrix"},ItemSize->Full]; 
    nstart=5,
    nstart=3
  ];   
  nfrom=nstart;
  table[[nfrom;;nfrom+nsir1-1,1]]=idxsir;
  table[[nfrom;;nfrom+nsir1-1,2]]=slbl[[idxsir]];
  table[[nfrom;;nfrom+nsir1-1,3]]=stype[[idxsir]];  
  If[ndir1>0,
    nfrom=nfrom+nsir1; 
    table[[nfrom;;nfrom+ndir1-1,1]]=idxdir;    
    table[[nfrom;;nfrom+ndir1-1,2]]=dlbl[[idxdir-nsir]];
    table[[nfrom;;nfrom+ndir1-1,3]]=dtype[[idxdir-nsir]]
  ];     
    
  sty1=Directive[Black,Thickness[OptionValue["linewidth"]]];   
  sty2=Directive[Thin,GrayLevel[0.8]];
  thickHLines={1,nstart,nstart+nsir1,-1};
  bg1={{1,nstart-1},{1,-1}}->Lighter[Yellow,0.9];
  bg2={{2,2},{1,-1}}->Lighter[Yellow,0.9];
  bg3={};  nfrom=nstart;
  bg4={{nfrom,nfrom+nsir1-1},{1,-1}}->Lighter[Green,0.95];
  nfrom+=nsir1;  
  bg5={};
  bg6={{nfrom,-1},{1,-1}}->Lighter[Blue,0.95];
  grid=Grid[table,Frame->All,Alignment->{Center,Center},ItemSize->Full,
                  Dividers->{{{{sty2}},Join[#->sty1&/@{1,4,-1},#->sty2&/@{2,3}]}, 
                            {{{sty2}},Join[#->sty1&/@thickHLines, {2->sty2}]}},
                  Background->{None,None,{bg1,bg2,bg3,bg4,bg5,bg6}}
           ];
  Column[{head,grid}]
]


(* ::Subsection:: *)
(*For type-2 and type-4 MSG*)


(* ::Subsubsection::Closed:: *)
(*common for getType2MLGCorep  and  getType4MLGCorep*)


Options[getType24MLGCorep]={"format"->True, "abcOrBasVec"->None, "trace"->False};
getType24MLGCorep[{sgno_,num_}, k_, OptionsPattern[]]:=
  getType24MLGCorep[{sgno,num}, k, "a", (#->OptionValue[#])&/@{"format","abcOrBasVec","trace"}]
getType24MLGCorep[{sgno_,num_}, k_, BZtype_String, OptionsPattern[]]:=
 Module[{brav,MSG,MLG, antiU,reptab,Gk,getrep,square,format=OptionValue["format"],BZtypeOrBasVec,
  stypes,dGk,dsquare,dantiU,dtypes,tmp,kinfo,kname,Sw,kco,usub,i,j,
  bztypes, BZs, fullBZtypes, tmp2, mytr, sct, dct, srep, drep, info, A, R2Rv, slb,slb2,
  dlb,dlb2,sidx,didx,time,inv,scorep,dcorep,stypes2,dtypes2,MLG0,sct2,dct2,iARA,barDelta,
  Delta,BA,iAB,BiA,matN,DeltaBiA,paired,slabel,dlabel,realSimplify,sNs,dNs},
  
  (* Here use FullSimplify will cause problem, e.g. Root appears for S4x+ of {230,147},"P"*)
  realSimplify[x_]:=Chop@Simplify[x,u\[Element]Reals]; 
  MSG=getMSGElem[{sgno,num}];    MSG=MapAt[modone,MSG,{All,2}];
  brav=getSGLatt[sgno]; 
  info=<||>;   info["symbol"]={MSGSymStd[{sgno,num}],{sgno,num},MSGSymBC[{sgno,num}]};

  reptab=getLGIrepTab[sgno,k,"abcOrBasVec"->OptionValue["abcOrBasVec"],"format"->False];  
  BZs=#["kBZs"][[All,3]]&/@reptab;
  bztypes=Map[StringReplace[#,{__~~"("->"",")"->""}]&, BZs, {-1}];
  tmp=Position[bztypes,BZtype];
  If[tmp=={},  i=j=1, i=tmp[[1,1]]; j=tmp[[1,2]] ];
  If[StringQ[k]&&OptionValue["abcOrBasVec"]===None&&tmp=={}&&StringPosition[First@Flatten[BZs],"("]!={},
    getType24MLGCorep::BZchange="Warning, k-point "<>k<>" exists only in `1`, not in `2` for "<>ToString[{sgno,num}];
    Message[getType24MLGCorep::BZchange,Flatten[BZs],brav<>"("<>BZtype<>")"];
  ];
  reptab=reptab[[i]];
  kinfo=reptab["kinfo"];
  fullBZtypes=If[StringQ[k], BZs[[i,j]], Flatten[BZs[[i]]]];
  kco=If[StringQ[k], reptab["kBZs"][[j,2]], 
        If[Position[kinfo,Rule]!={}, kco=kinfo[[7]]+kinfo[[8]], k]];
  usub=If[!StringQ[k]&&Position[kinfo,Rule]!={}, kinfo[[9]], Nothing];
  kname=kinfo[[2]];
  info["k"]={kco,kname,fullBZtypes,usub};
  info["kinfo"]=kinfo;
      
  MLG=getMLGElem[brav,MSG,kco];  
  MLG0=Select[MLG,#[[3]]==0&][[All,{1,2}]];  
  antiU=Select[MLG,#[[3]]==1&][[All,{1,2}]];  
  Gk=reptab["Gkin"];    dGk=Join[Gk,{"bar"<>#[[1]],#[[2]]}&/@Gk];  
  info["MLG"]=MLG;
    
  (* getrep works for both characters and representation matrices*)  
  getrep[rep_][{R_,v_}]:=Module[{ch, trans},
    trans=Association[Rule@@@Gk];
    ch=Association[Rule@@@Transpose[{Gk[[All,1]],rep}]];
    (trans["bar"<>#]=trans[#])&/@Gk[[All,1]];
    (ch["bar"<>#]=-ch[#])&/@Gk[[All,1]]; 
    ch[R]*Exp[-I*kco.(v-trans[R])*2Pi]//realSimplify
  ];
  
  dsquare=DSGSeitzTimes[brav][#,#]&/@antiU;   
  square=If[dsquare!={}, MapAt[StringReplace[#1,"bar"->""]&, dsquare, {All,1}], {}]; 

  mytr[m_]:=If[MatrixQ[m],Tr[m],m];
  srep=reptab["sirep"];   sct=Map[mytr,srep,{2}];
  drep=reptab["direp"];   dct=Map[mytr,drep,{2}];
  slb=reptab["slabel"][[All,2]];   dlb=reptab["dlabel"][[All,2]];
                                        
  stypes=Table[getrep[ir]/@square//Total,{ir,sct}]/Length[Gk]//Simplify;   (*BC-Eq. 7.6.41*)
  dtypes=Table[-getrep[ir]/@dsquare//Total,{ir,dct}]/Length[Gk]//Simplify;  
  (* The above Simplify is needed, e.g. for {143,2},"\[CapitalGamma]" *)
  stypes=Rationalize[stypes,0.1]/.{1->"a",-1->"b",0->"c"};
  dtypes=Rationalize[dtypes,0.1]/.{1->"a",-1->"b",0->"c"}; 
  If[antiU=={}, (*----------for type x------------*)
    stypes=stypes/.{"c"->"x"}; dtypes=dtypes/.{"c"->"x"};  
    stypes2=stypes;   dtypes2=dtypes;
    slb2=slb;   dlb2=dlb;   
    sidx=Range@Length[slb];  didx=Range@Length[dlb];
    scorep=Table[getrep[ir]/@MLG0, {ir,srep}];
    dcorep=Table[getrep[ir]/@MLG0, {ir,drep}];
    info["A"]=None;    sNs=Table[None,Length[slb]];  dNs=Table[None,Length[dlb]];
    Goto["end"]  (* Goto ========================> end *)
  ];
  
  R2Rv[_]=None;   (R2Rv[#[[1]]]=#)&/@antiU;
  tmp=DeleteCases[R2Rv/@{"E","I","\[Sigma]z","C2z","\[Sigma]x","\[Sigma]y","\[Sigma]h"},None];  (*the priority of the rotation of A*)
  A=If[tmp!={}, tmp[[1]], antiU[[1]]];  
  info["A"]=Append[A,1];

  (*------------------------for single-valued corepresentations---------------------*)
  time=SeitzTimes[brav];  inv=invSeitz[brav];  
  iARA=time[inv[A],time[#,A]]&/@MLG0;    
  BA=time[#,A]&/@antiU;      
  iAB=time[inv[A],#]&/@antiU;   
  BiA=time[#,inv[A]]&/@antiU;   
  sct2=Table[getrep[ir]/@MLG0,{ir,sct}];   
  
  slb2=sidx=scorep=stypes2=sNs=paired={}; 
  For[i=1,i<=Length[slb],i++,  
    If[MemberQ[paired,i], Continue[]];
    Delta=getrep[srep[[i]]]/@MLG0;
    barDelta=getrep[srep[[i]]][#]\[Conjugate]&/@iARA//realSimplify;
    If[MatchQ[stypes[[i]],"a"|"b"],
      matN=findMatrixN[Delta,barDelta];   AppendTo[sNs,matN];
      DeltaBiA=getrep[srep[[i]]]/@BiA;
      (* (*------ check matN, for debug-------*)
      Print["SV type ",stypes[[i]],":  NN^*=", If[MatrixQ[matN],matN.matN\[Conjugate],matN*matN\[Conjugate]],
            ", Delta(A^2)=",getrep[srep[[i]]][conv[time[A,A]][[1]]]];  *)
    ];
    Switch[stypes[[i]],
      "a", AppendTo[slb2,slb[[i]]]; AppendTo[sidx,i]; AppendTo[stypes2,"a"];
           tmp=If[MatrixQ[matN], #.matN&/@DeltaBiA, DeltaBiA*matN]//realSimplify;
           AppendTo[scorep, Join[Delta,tmp]]; 
           ,
      "b", AppendTo[slb2,slb[[i]]]; AppendTo[sidx,i]; AppendTo[stypes2,"b"];
           tmp=If[MatrixQ[matN], #.matN&/@DeltaBiA, DeltaBiA*matN]//realSimplify;
           tmp=ArrayFlatten[{{0,-#},{#,0}}]&/@tmp;
           AppendTo[scorep, Join[ArrayFlatten[{{#,0},{0,#}}]&/@Delta,tmp]]; 
           ,
      "c", If[Length[slb]==2, slb2={slb}; sidx={{1,2}}; paired={2},
             tmp=If[MatrixQ[barDelta//First], Tr/@barDelta, barDelta]//realSimplify;
             tmp2=Position[Simplify@Total@Abs[tmp-#]<1*^-5&/@sct2,True][[1,1]];  
             AppendTo[slb2,{slb[[i]],slb[[tmp2]]}];
             AppendTo[sidx,{i,tmp2}];   AppendTo[paired,tmp2];
           ];
           AppendTo[stypes2,"c"];   AppendTo[sNs,None];
           tmp=getrep[srep[[i]]][#]&/@BA;    (* Delta(BA) *)
           tmp2=getrep[srep[[i]]][#]\[Conjugate]&/@iAB//realSimplify;   (* Delta(A^-1*B)^* *)
           tmp2=ArrayFlatten[{{0,#1},{#2,0}}]&@@@Transpose[{tmp,tmp2}];
           tmp=ArrayFlatten[{{#1,0},{0,#2}}]&@@@Transpose[{Delta,barDelta}];
           AppendTo[scorep, Join[tmp, tmp2]];   
    ];
  ];
  
  (*------------------------for double-valued corepresentations---------------------*)
  time=DSGSeitzTimes[brav];  inv=DSGinvSeitz[brav];  
  iARA=time[inv[A],time[#,A]]&/@MLG0;    
  BA=time[#,A]&/@antiU;     
  iAB=time[inv[A],#]&/@antiU;
  BiA=time[#,inv[A]]&/@antiU;
  dct2=Table[getrep[ir]/@MLG0,{ir,dct}];  
  (*Note that here we use the multiplication of DSG, but in fact the multiplication of DMSG 
  is needed. So a fix has to be done for BA. Note that for double MSG, theta^2\[Equal]barE in which
  theta is the time reversal.*)
  BA=If[StringTake[#1,1]=="b", {StringTake[#1,{4,-1}],#2}, {"bar"<>#1,#2}]&@@@BA;
  
  dlb2=didx=dcorep=dtypes2=dNs=paired={}; 
  For[i=1,i<=Length[dlb],i++,  
    If[MemberQ[paired,i], Continue[]];
    Delta=getrep[drep[[i]]]/@MLG0;
    barDelta=getrep[drep[[i]]][#]\[Conjugate]&/@iARA//realSimplify;
    If[MatchQ[dtypes[[i]],"a"|"b"],
      matN=findMatrixN[Delta,barDelta];    AppendTo[dNs,matN];
      DeltaBiA=getrep[drep[[i]]]/@BiA;
      (*(*------ check matN, for debug -------*)
      Print["DV type ",dtypes[[i]],":  NN^*=", If[MatrixQ[matN],matN.matN\[Conjugate],matN*matN\[Conjugate]],
            ", Delta(A^2)=",getrep[drep[[i]]][conv[time[A,A]][[2]]]];  *)
    ];
    Switch[dtypes[[i]],
      "a", AppendTo[dlb2,dlb[[i]]]; AppendTo[didx,i]; AppendTo[dtypes2,"a"];
           tmp=If[MatrixQ[matN], #.matN&/@DeltaBiA, DeltaBiA*matN]//realSimplify;
           AppendTo[dcorep, Join[Delta,tmp]]; 
           ,
      "b", AppendTo[dlb2,dlb[[i]]]; AppendTo[didx,i]; AppendTo[dtypes2,"b"];
           tmp=If[MatrixQ[matN], #.matN&/@DeltaBiA, DeltaBiA*matN]//realSimplify;
           tmp=ArrayFlatten[{{0,-#},{#,0}}]&/@tmp;
           AppendTo[dcorep, Join[ArrayFlatten[{{#,0},{0,#}}]&/@Delta,tmp]]; 
           , 
      "c", If[Length[dlb]==2, dlb2={dlb}; didx={{1,2}}; paired={2},
             tmp=If[MatrixQ[barDelta//First], Tr/@barDelta, barDelta]//realSimplify;
             tmp2=Position[Simplify@Total@Abs[(tmp-#)]<1*^-5&/@dct2,True][[1,1]];
             AppendTo[dlb2,{dlb[[i]],dlb[[tmp2]]}];
             AppendTo[didx,{i,tmp2}];   AppendTo[paired,tmp2];
           ];
           AppendTo[dtypes2,"c"];    AppendTo[dNs,None];
           tmp=getrep[drep[[i]]][#]&/@BA;    (* Delta(BA) *)
           tmp2=getrep[drep[[i]]][#]\[Conjugate]&/@iAB//realSimplify;   (* Delta(A^-1*B)^* *)
           tmp2=ArrayFlatten[{{0,#1},{#2,0}}]&@@@Transpose[{tmp,tmp2}];
           tmp=ArrayFlatten[{{#1,0},{0,#2}}]&@@@Transpose[{Delta,barDelta}];
           AppendTo[dcorep, Join[tmp, tmp2]];   
    ];
  ];
  
  Label["end"];
  
  slabel=Switch[#1,"a"|"x",#2,"b",#2<>#2,"c",#2[[1]]<>#2[[2]]]&@@@Transpose[{stypes2,slb2}];
  dlabel=Switch[#1,"a"|"x",#2,"b",#2<>#2,"c",#2[[1]]<>#2[[2]]]&@@@Transpose[{dtypes2,dlb2}];
  
  If[OptionValue["trace"]===True, 
    scorep=Map[If[MatrixQ[#],Tr[#],#]&, scorep, {2}]//Simplify;
    dcorep=Map[If[MatrixQ[#],Tr[#],#]&, dcorep, {2}]//Simplify;
  ];
  info["sidx"]=sidx;        info["stype"]=stypes2;    
  info["slabel"]=slabel;    info["smatN"]=sNs;           
  info["scorep"]=If[format===True, Map[formatRepMat,scorep,{2}], scorep];
  info["didx"]=didx;        info["dtype"]=dtypes2;    
  info["dlabel"]=dlabel;    info["dmatN"]=dNs;           
  info["dcorep"]=If[format===True, Map[formatRepMat,dcorep,{2}], dcorep];
  
  (*(*for debug*)
  Print["scorep:\n", (MatrixForm/@#)&/@scorep//Chop//TableForm[#,TableHeadings->{slabel,showMSGSeitz/@MLG}]&];
  Print["dcorep:\n", (MatrixForm/@#)&/@dcorep//Chop//TableForm[#,TableHeadings->{dlabel,showMSGSeitz/@MLG}]&];
  *)
  info
]


(* ::Subsubsection::Closed:: *)
(*getType4MLGCorep*)


Options[getType4MLGCorep]={"format"->True, "abcOrBasVec"->None, "trace"->False};
getType4MLGCorep[{sgno_,num_}, k_, OptionsPattern[]]:=
  getType4MLGCorep[{sgno,num}, k, "a", (#->OptionValue[#])&/@{"format","abcOrBasVec","trace"}]
getType4MLGCorep[{sgno_,num_}, k_, BZtype_String, OptionsPattern[]]:=
 Module[{tmp},
  tmp=type4nums[sgno];
  If[tmp=={}, Print["getType4MLGCorep: There is no type-4 MSG begin with ",sgno]; Abort[],
     If[!MemberQ[tmp,num], 
         Print["getType4MLGCorep: for type-4 MSG ",sgno,".num, num should be in ",
                Row[{#,"(",MSGSymStd[{sgno,#}],")"}]&/@tmp];
         Abort[]]
  ];
  getType24MLGCorep[{sgno,num},k,BZtype,(#->OptionValue[#])&/@{"format","abcOrBasVec","trace"}]
]


(* ::Subsubsection::Closed:: *)
(*getType2MLGCorep*)


Options[getType2MLGCorep]={"format"->True, "abcOrBasVec"->None, "trace"->False};
getType2MLGCorep[{sgno_,num_}, k_, OptionsPattern[]]:=
  getType2MLGCorep[{sgno,num}, k, "a", (#->OptionValue[#])&/@{"format","abcOrBasVec","trace"}]
getType2MLGCorep[{sgno_,num_}, k_, BZtype_String, OptionsPattern[]]:=
 Module[{tmp},
  tmp=type2num[sgno];
  If[num!=tmp, Print["getType2MLGCorep: for type-2 MSG ",sgno,".num, num should be ",tmp]; Abort[]];
  getType24MLGCorep[{sgno,num},k,BZtype,(#->OptionValue[#])&/@{"format","abcOrBasVec","trace"}]
]
getType2MLGCorep[sgno_Integer, k_, BZtype_String, OptionsPattern[]]:=
  getType2MLGCorep[{sgno,type2num[sgno]}, k, BZtype, (#->OptionValue[#])&/@{"format","abcOrBasVec","trace"}]
getType2MLGCorep[sgno_Integer, k_, OptionsPattern[]]:=
  getType2MLGCorep[sgno, k, "a", (#->OptionValue[#])&/@{"format","abcOrBasVec","trace"}]


(* ::Subsubsection::Closed:: *)
(*common for showType2MLGCorep and showType4MLGCorep*)


Options[showType24MLGCorep]={"uNumeric"->False,"corep"->All,"elem"->All,"rotmat"->True,"trace"->False,
                        "spin"->"downup","abcOrBasVec"->None,"linewidth"->2,"maxDim"->4};
showType24MLGCorep[{sgno_,mno_}, kNameOrCoord_, OptionsPattern[]]:=showType24MLGCorep[{sgno,mno},
    kNameOrCoord, "a", (#->OptionValue[#])&/@{"uNumeric","corep","elem","rotmat","trace","spin",
    "abcOrBasVec","linewidth","maxDim"}]
showType24MLGCorep[{sgno_,mno_}, kNameOrCoord_, BZtype_, OptionsPattern[]]:=Block[{u,crinfo,brav,
  showmat,idxsir,idxdir,idxelm,showrot,kname, kBZ, kinfo, slbl,stype,scorep,symstd,symBC,
  dlbl,dtype,dcorep, MLG, small,bold,dmax,rmRe0,rot,trans,srot,table,nelem,strkBC,
  nsir,ndir,sfl,sfa,nc,nr,nfrom,head,h1,h2,h3,h4,h5,grid,sty1,sty2,thickHLines,tmp,nsir1,ndir1,
  bg1,bg2,bg3,bg4,bg5,bg6,nstart,emph},
  crinfo=getType24MLGCorep[{sgno,mno},kNameOrCoord,BZtype,(#->OptionValue[#])&/@{"abcOrBasVec","trace"}];
  brav=getSGLatt[sgno];
  dmax=OptionValue["maxDim"];
  rmRe0=Map[If[MachineNumberQ[#],If[Re[#]==0,Which[Im[#]==1,I,Im[#]==-1,-I,True,Im[#]"\[ImaginaryI]"],#],#]&, #, -1]&;
  showmat[m_]:=If[!MatrixQ[m], rmRe0[m], If[Length[m]<=dmax, MatrixForm[rmRe0[m]], 
      tmp=Row[{"("<>ToString[#[[1,1]]]<>","<>ToString[#[[1,2]]]<>")",#[[2]]},":"]&;
      Partition[tmp/@ArrayRules[rmRe0[m]][[;;-2]],UpTo[2]]//Grid[#,Alignment->Left,ItemSize->Full]&
    ]];  
  showrot=OptionValue["rotmat"];

  kBZ=crinfo["k"];        kname=kBZ[[2]];      kinfo=crinfo["kinfo"];     
  slbl=crinfo["slabel"];     stype=crinfo["stype"];     scorep=crinfo["scorep"];
  dlbl=crinfo["dlabel"];     dtype=crinfo["dtype"];     dcorep=crinfo["dcorep"];
  MLG=crinfo["MLG"];   
  
  If[OptionValue["uNumeric"]&&VectorQ[kNameOrCoord]&&Position[kinfo,Rule]!={}, 
     scorep=scorep/.kinfo[[9]];  dcorep=dcorep/.kinfo[[9]]];
  nelem=Length[MLG];   nsir=Length[scorep];   ndir=Length[dcorep];
  tmp=Check[Range[nsir+ndir][[If[IntegerQ[tmp=OptionValue["corep"]], {tmp}, tmp]]],
             Print["showType4MLGCorep: index of corep out of range [1,",nsir+ndir,"]"]; Abort[],
             {Part::partw, Part::pkspec1}];
  idxsir=Select[tmp,#<=nsir&];    idxdir=Complement[tmp,idxsir];
  nsir1=Length[idxsir];    ndir1=Length[idxdir];
  idxelm=Check[Range[nelem][[If[IntegerQ[tmp=OptionValue["elem"]], {tmp}, tmp]]],
             Print["showType4MLGCorep: index of element out of range [1,",nelem,"]"]; Abort[],
             {Part::partw, Part::pkspec1}];

  small=Style[#,Smaller]&;  bold=Style[#,Bold]&;  emph=Style[#,Bold,Blue]&;
  strkBC="\!\(\*SubscriptBox[\(k\), \(BC\)]\)";
  {symstd,symBC}=crinfo["symbol"][[{1,3}]];
  tmp=If[symBC=!=symstd,Row[{Row[{emph@symstd,","}],"","BC:",emph@symBC}," "], emph@symstd];
  h1=Row[{"MSG ",emph@Row[{sgno,".",mno}],"(",tmp,"):  k-point name is ",
          emph[kname],", for ",Row[kBZ[[3]],","]}," "];
  h2=Row[{"\[FilledRightTriangle]  k=(",Row[kBZ[[1]],","],")"}];
  If[Position[kinfo,Rule]!={}&&VectorQ[kNameOrCoord], h2=Row[{h2,"  (u=",kinfo[[9,2]],")"}]];
  If[kname!="UN"&&kname!="GP"&&kinfo[[6,1]]!="E", 
     h2=Row[{h2," non-standard, ",strkBC,"=(",Row[kinfo[[5]],","],")  [",
             showSeitz[kinfo[[6]]][[1,2]],"]",strkBC," \[DoubleLeftRightArrow] k"}],
     If[kname!="UN"&&kname!="GP", 
       tmp=If[kinfo[[8]]=={0,0,0}, Nothing, Row[{",  ",strkBC,"=(",Row[kinfo[[5]],","],")"}]];
       h2=Row[{h2," standard",tmp}]]
  ];
  h3=h4={};
  head={h1,h2,h3,h4}//Flatten//Column;
    
  rot=MatrixForm[getRotMat[brav,#]]&/@MLG[[idxelm,1]];
  trans=MatrixForm[{InputForm/@#}\[Transpose]]&/@MLG[[idxelm,2]];
  srot=getSpinRotOp[#][[1]]&/@MLG[[idxelm,1]];
  If[OptionValue["spin"]=="updown", srot=sx.#.sx&/@srot];
  srot=MatrixForm[Expand[#]]&/@srot;
  sfl=SpanFromLeft;   sfa=SpanFromAbove;
  table={idxelm, showMSGSeitz/@MLG[[idxelm]], 
         Sequence@@If[showrot,{rot, srot},{}], 
         Sequence@@Map[showmat,scorep[[idxsir,idxelm]],{2}],
         Sequence@@Map[showmat,dcorep[[idxdir-nsir,idxelm]],{2}]};
  {nr,nc}=Dimensions[table];   
  table=Join[Table[sfl,3,nr],table\[Transpose]]\[Transpose];
  table[[1,1]]="Index";     table[[2,1]]="Element";   
  If[showrot,
    table[[3,1]]=Column[{"Rotation","matrix"}, ItemSize->Full];
    table[[4,1]]=Column[{If[OptionValue["spin"]=="updown","Spin(\[UpArrow]\[DownArrow])","Spin(\[DownArrow]\[UpArrow])"],"rotation","matrix"}, ItemSize->Full]; 
    nstart=5,
    nstart=3
  ];   
  nfrom=nstart;
  tmp=Association[Rule@@@Transpose[{slbl[[idxsir]],stype[[idxsir]]}]];
  table[[nfrom;;nfrom+nsir1-1,1]]=idxsir;
  table[[nfrom;;nfrom+nsir1-1,2]]=If[tmp[#]=="a"||tmp[#]=="x"," "<>#<>" ",#]&/@slbl[[idxsir]];
  table[[nfrom;;nfrom+nsir1-1,3]]=stype[[idxsir]];  
  If[ndir1>0,
    nfrom=nfrom+nsir1; 
    tmp=Association[Rule@@@Transpose[{dlbl[[idxdir-nsir]],dtype[[idxdir-nsir]]}]];
    table[[nfrom;;nfrom+ndir1-1,1]]=idxdir;    
    table[[nfrom;;nfrom+ndir1-1,2]]=If[tmp[#]=="a"||tmp[#]=="x"," "<>#<>" ",#]&/@dlbl[[idxdir-nsir]];
    table[[nfrom;;nfrom+ndir1-1,3]]=dtype[[idxdir-nsir]]
  ];     
    
  sty1=Directive[Black,Thickness[OptionValue["linewidth"]]];   
  sty2=Directive[Thin,GrayLevel[0.8]];
  thickHLines={1,nstart,nstart+nsir1,-1};
  bg1={{1,nstart-1},{1,-1}}->Lighter[Yellow,0.9];
  bg2={{2,2},{1,-1}}->Lighter[Yellow,0.9];
  bg3={};  nfrom=nstart;
  bg4={{nfrom,nfrom+nsir1-1},{1,-1}}->Lighter[Green,0.95];
  nfrom+=nsir1;  
  bg5={};
  bg6={{nfrom,-1},{1,-1}}->Lighter[Blue,0.95];
  grid=Grid[table,Frame->All,Alignment->{Center,Center},ItemSize->Full,
                  Dividers->{{{{sty2}},Join[#->sty1&/@{1,4,-1},#->sty2&/@{2,3}]}, 
                            {{{sty2}},Join[#->sty1&/@thickHLines, {2->sty2}]}},
                  Background->{None,None,{bg1,bg2,bg3,bg4,bg5,bg6}}
           ];
  Column[{head,grid}]
]


(* ::Subsubsection::Closed:: *)
(*showType4MLGCorep*)


Options[showType4MLGCorep]={"uNumeric"->False,"corep"->All,"elem"->All,"rotmat"->True,"trace"->False,
                        "spin"->"downup","abcOrBasVec"->None,"linewidth"->2,"maxDim"->4};
showType4MLGCorep[{sgno_,num_}, kNameOrCoord_, OptionsPattern[]]:=showType4MLGCorep[{sgno,num},
    kNameOrCoord, "a", (#->OptionValue[#])&/@{"uNumeric","corep","elem","rotmat","trace","spin",
    "abcOrBasVec","linewidth","maxDim"}]
showType4MLGCorep[{sgno_,num_}, kNameOrCoord_, BZtype_, OptionsPattern[]]:=Module[{tmp},
  tmp=type4nums[sgno];
  If[tmp=={}, Print["showType4MLGCorep: There is no type-4 MSG begin with ",sgno]; Abort[],
     If[!MemberQ[tmp,num], 
         Print["showType4MLGCorep: for type-4 MSG ",sgno,".num, num should be in ",
                Row[{#,"(",MSGSymStd[{sgno,#}],")"}]&/@tmp];
         Abort[]]
  ];
  showType24MLGCorep[{sgno,num},kNameOrCoord,BZtype,(#->OptionValue[#])&/@{"uNumeric","corep",
                      "elem", "rotmat","trace","spin","abcOrBasVec","linewidth","maxDim"}]
]


(* ::Subsubsection::Closed:: *)
(*showType2MLGCorep*)


Options[showType2MLGCorep]={"uNumeric"->False,"corep"->All,"elem"->All,"rotmat"->True,"trace"->False,
                        "spin"->"downup","abcOrBasVec"->None,"linewidth"->2,"maxDim"->4};
showType2MLGCorep[{sgno_,num_}, kNameOrCoord_, OptionsPattern[]]:=showType2MLGCorep[{sgno,num},
    kNameOrCoord, "a", (#->OptionValue[#])&/@{"uNumeric","corep","elem","rotmat","trace","spin",
    "abcOrBasVec","linewidth","maxDim"}]
showType2MLGCorep[{sgno_,num_}, kNameOrCoord_, BZtype_, OptionsPattern[]]:=Module[{tmp},
  tmp=type2num[sgno];
  If[num!=tmp, Print["showType2MLGCorep: for type-2 MSG ",sgno,".num, num should be ",tmp]; Abort[]];
  showType24MLGCorep[{sgno,num},kNameOrCoord,BZtype,(#->OptionValue[#])&/@{"uNumeric","corep",
                      "elem", "rotmat","trace","spin","abcOrBasVec","linewidth","maxDim"}]
]
showType2MLGCorep[sgno_Integer, kNameOrCoord_, BZtype_, OptionsPattern[]]:=
  showType2MLGCorep[{sgno,type2num[sgno]},kNameOrCoord, BZtype, (#->OptionValue[#])&/@{"uNumeric",
                     "corep","elem","rotmat","trace","spin","abcOrBasVec","linewidth","maxDim"}]
showType2MLGCorep[sgno_Integer, kNameOrCoord_, OptionsPattern[]]:=
  showType2MLGCorep[{sgno,type2num[sgno]},kNameOrCoord, "a", (#->OptionValue[#])&/@{"uNumeric",
                     "corep","elem","rotmat","trace","spin","abcOrBasVec","linewidth","maxDim"}]


(* ::Subsection::Closed:: *)
(*getMLGCorep*)


Options[getMLGCorep]={"format"->True, "abcOrBasVec"->None, "trace"->False};
getMLGCorep[{sgno_,num_}, k_, OptionsPattern[]]:=
  getMLGCorep[{sgno,num}, k, "a", (#->OptionValue[#])&/@{"format","abcOrBasVec","trace"}]
getMLGCorep[{sgno_,num_}, k_, BZtype_String, OptionsPattern[]]:=Module[{nums,pos,fun,str},
  {nums,pos}=checkMSGinput[{sgno,num},"getMLGCorep"];
  str=If[pos[[1,1]]==1, "24", ToString[pos[[1,1]]]];
  fun=ToExpression[cont<>"getType"<>str<>"MLGCorep"];
  fun[{sgno,num},k,BZtype,(#->OptionValue[#])&/@{"format","abcOrBasVec","trace"}]
]


(* ::Subsection::Closed:: *)
(*checkMLGCorep*)


(* Check whether the corep from getMLGCorep satisfies correct multiplications. 
 If the returned numbers are all zeros, the corep goes right. 
 To get accurate multiplications, use "format"\[Rule]False option in getMLGCorep. *)
checkMLGCorep[corepinfo_]:=Module[{MLG,mtab,scorep,dcorep, time, brav, fBZ, rot2elem,n,tmp,sub,k,
  itab,dvtab,ftab,rot2idx, repmtab, reptime, tr, check, dtime,mtab2, bartab, empty},
  MLG=corepinfo["MLG"];   n=Length[MLG];
  If[Position[corepinfo["kinfo"],u]!={}, sub=corepinfo["kinfo"][[9]], sub={}];
  rot2elem=#[[{1,3}]]->#&/@MLG//Association;
  rot2idx=Flatten[{MLG[[#,{1,3}]]->#,{"bar"<>MLG[[#,1]],MLG[[#,3]]}->#}&/@Range[n]]//Association; 
  scorep=corepinfo["scorep"];  dcorep=corepinfo["dcorep"];
  fBZ=corepinfo["k"][[3]];  If[ListQ[fBZ], fBZ=fBZ[[1]]];
  k=corepinfo["k"][[1]];
  brav=If[StringTake[fBZ,-1]==")",StringTake[fBZ,1;;-4],fBZ];
  time=MSGSeitzTimes[brav];
  mtab=Table[time[i,j],{i,MLG},{j,MLG}];
  itab=Table[rot2idx[mtab[[i,j,{1,3}]]],{i,n},{j,n}];  
  dvtab=Table[tmp=mtab[[i,j]];tmp=tmp[[2]]-rot2elem[tmp[[{1,3}]]][[2]],{i,n},{j,n}];
  ftab=Table[Exp[-I*k.dvtab[[i,j]]*2Pi]//Chop,{i,n},{j,n}]; 
  tr=MLG[[All,3]];
  reptime[rep_,i_,j_]:=Module[{m1,m2,tr1,tr2},
     m1=rep[[i]]; m2=rep[[j]];  tr1=tr[[i]];  tr2=tr[[j]];
     If[MatrixQ[m1], If[tr1==1,m1.m2\[Conjugate], m1.m2], If[tr1==1,m1*m2\[Conjugate], m1*m2]]//Simplify[#,u\[Element]Reals]&
  ];
  repmtab[rep_]:=Table[reptime[rep,i,j],{i,n},{j,n}];

  dtime=DMSGSeitzTimes[brav];
  mtab2=Table[dtime[i,j],{i,MLG},{j,MLG}];  
  bartab=Table[If[mtab2[[i,j,1]]==MLG[[itab[[i,j]],1]],1,-1],{i,n},{j,n}]; 

  check[rep_,d_]:=Module[{diff, tab},
    tab= Table[rep[[itab[[i,j]]]],{i,n},{j,n}]*ftab;
    If[d=="d",tab=tab*bartab];
    diff=Flatten[(repmtab[rep]-tab)/.sub];
    Total@Abs[diff//N//Chop//Simplify]
  ];
   
  empty=If[scorep=={}||dcorep=={}, 1, 0];
  {empty, check[#,"s"]&/@scorep, check[#,"d"]&/@dcorep}//Simplify
]


(* ::Subsection::Closed:: *)
(*showMLGCorep*)


Options[showMLGCorep]={"uNumeric"->False,"corep"->All,"elem"->All,"rotmat"->True,"trace"->False,
                        "spin"->"downup","abcOrBasVec"->None,"linewidth"->2,"maxDim"->4};
showMLGCorep[{sgno_,num_}, k_, OptionsPattern[]]:=showMLGCorep[{sgno,num}, k, "a", 
        (#->OptionValue[#])&/@{"uNumeric","corep","elem","rotmat","trace","spin",
        "abcOrBasVec","linewidth","maxDim"}]
showMLGCorep[{sgno_,num_}, k_, BZtype_String, OptionsPattern[]]:=Module[{nums,pos,fun,str},
  {nums,pos}=checkMSGinput[{sgno,num},"showMLGCorep"];
  str=If[pos[[1,1]]==1, "24", ToString[pos[[1,1]]]];
  fun=ToExpression[cont<>"showType"<>str<>"MLGCorep"];
  fun[{sgno,num},k,BZtype,(#->OptionValue[#])&/@{"uNumeric","corep","elem","rotmat","trace",
                           "spin","abcOrBasVec","linewidth","maxDim"}]
]


(* ::Subsection::Closed:: *)
(*getMLGCorepMat*)


(*Get corep matrices for any MLG elements according to the result of getMLGCorep*)
Options[getMLGCorepMat]={"uNumeric"->False, "trace"->False};
getMLGCorepMat[crinfo_Association, OptionsPattern[]][elmOrList_]:=
  getMLGCorepMat[crinfo,All,"uNumeric"->OptionValue["uNumeric"],"trace"->OptionValue["trace"]][elmOrList]
getMLGCorepMat[crinfo_Association, cridx_, OptionsPattern[]][elmOrList_]:=Module[{k,Mk,corep,labels,nscr,ndcr,usub={},idx,ckelm,
  types,dims,tab,idxerr=False,re},
  k=crinfo["k"][[1]];    Mk=crinfo["MLG"];
  If[MissingQ[Mk], Print["getMLGCorepMat:  the argument crinfo should be the result of getMLGCorep."]; Abort[]];
  If[Length[crinfo["k"]]==4, usub=crinfo["k"]//Last];
  nscr=Length[crinfo["slabel"]];  ndcr=Length[crinfo["dlabel"]];  
  corep=Join[crinfo["scorep"], crinfo["dcorep"]];
  If[OptionValue["trace"]===True, corep=Map[If[MatrixQ[#],Tr[#],#]&, corep, {2}]//FullSimplify];
  idx=Map[If[#===0,"0",#]&,cridx,{-1}];
  idx=Check[Range[nscr+ndcr][[idx]], idxerr=True];
  If[idxerr,
    labels=Join[crinfo["slabel"], crinfo["dlabel"]];
    types=Join[crinfo["stype"], crinfo["dtype"]];
    dims=If[MatrixQ[#],Length[#],#]&/@corep[[All,1]];  
    tab={Join[{"corep"},{"single-valued"},Table[SpanFromLeft,nscr-1],{"double-valued"},Table[SpanFromLeft,ndcr-1]],
         Prepend[Range[nscr+ndcr],"index"],  Prepend[labels, "label"], Prepend[types,"type"], Prepend[dims,"dim"] };
    tab=Grid[tab,Frame->All,ItemSize->7,FrameStyle->Directive[Thin,Gray],Alignment->{Center,Center}];
    Print["getMLGCorepMat: index of corep out of range [1,",nscr,"]\[Union][",nscr+1,",",nscr+ndcr,"]. Refer to:\n", tab]; 
    Abort[]
  ];
  corep=corep[[idx]];
  ckelm=ListQ[#]&&Length[#]==3&&StringQ[#[[1]]]&&VectorQ[#[[2]]]&&MemberQ[{0,1},#[[3]]]&;
  If[!(ckelm[elmOrList]||And@@(ckelm/@elmOrList)),
    Print["getMLGCorepMat:  the arugment elmOrList should be one MLG element such as ",InputForm@Last@Mk,
      " or a list of MLG elements."]; Abort[];
  ];
  re=getRepMat[k,Mk,corep][elmOrList];
  If[OptionValue["uNumeric"],re=re/.usub];
  re
]


(* ::Section:: *)
(*MSG Corep (full corep)*)


(* ::Subsection::Closed:: *)
(*getMSGCorep*)


Options[getMSGCorep]={"format"->True, "abcOrBasVec"->None, "trace"->False};
getMSGCorep[{sgno_,num_}, k_, OptionsPattern[]]:=
  getMSGCorep[{sgno,num}, k, "a", (#->OptionValue[#])&/@{"format","abcOrBasVec","trace"}]
(* Note that here the subscripts of t use special characters, ie. \: plus 2081, 2082, 2083. *)
getMSGCorep[{sgno_, num_}, kNameOrCoord_, BZtype_String, OptionsPattern[]]:=Block[{u,t\:2081,t\:2082,t\:2083,
  MLGkininfo,MSGinfo,kinfo,kname,kin,usub,k1,MLGk1,kstar,k1scorep,k1dcorep,MSG,MSG0,MSG1,kBZ2a,kBZ2b,
  typex,tmp,tmp2,brav,cosetrep,invgiggj,inMLG,notinMLG,repdict,scorep,dcorep,sLbl,dLbl,invcsr,getstar,
  dinvcsr,zeros,nir,nelem,dims,ncsr,ggj,i,j,ir,ig,invgi,Ettt,Efac,tm,inv,dtm,dinv,repEttt,sstar,sidx,didx,
  bztypeORbvec,typeIII,subno,TM,kstar2a,kstar2b,kname2a,kname2b,k2a,k2b,k2b0,underbar,kinfo2b,srep2a,
  drep2a,srep2b,drep2b,slb2b,dlb2b,MLGinfo2b,MLGk1info,kinfo2a,srep2atob,srep2btob,drep2atob,drep2btob},
  
  MLGkininfo=getMLGCorep[{sgno,num},kNameOrCoord,BZtype,
                       (#->OptionValue[#])&["abcOrBasVec"],"format"->False];
  typex=MLGkininfo["stype"][[1]]=="x";

  brav=getSGLatt[sgno];
  {kin,kname}=MLGkininfo["k"][[1;;2]];
  usub=If[Length[MLGkininfo["k"]]==4, MLGkininfo["k"][[4]], {}];
  kinfo=MLGkininfo["kinfo"];     
  MSG=getMSGElem[{sgno,num}]; 
  MSG0=Select[MSG,#[[3]]==0&];   MSG1=Select[MSG,#[[3]]==1&];
  MSG=Join[MSG0,MSG1];
  tmp=OptionValue["abcOrBasVec"];
  bztypeORbvec=If[tmp===None, BZtype, If[Position[tmp,Rule]=={}, tmp, BasicVectors[brav]/.tmp]];
  
  getstar[kco_]:=Module[{s1,s2},  (* set kstar, kstar2a, kstar2b, cosetrep *)
    s1={#,getRotMatOfK[brav,#[[1]]].kco}&/@MSG0;
    s1=Gather[s1,keqmod[#1[[2]],#2[[2]]]&];
    s2={#,-getRotMatOfK[brav,#[[1]]].kco}&/@MSG1;
    s2=Gather[s2,keqmod[#1[[2]],#2[[2]]]&];
    cosetrep=Join[s1[[All,1,1]], If[typex, s2[[All,1,1]], {}]];
    kstar2a=s1[[All,1,2]];   kstar2b=If[typex, s2[[All,1,2]],{}];
    kstar=Join[kstar2a, kstar2b];
  ];
  getstar[kin];
  nelem=Length[MSG];       ncsr=Length[cosetrep];
  
  tm=MSGSeitzTimes[brav];        inv=MSGinvSeitz[brav];
  dtm=DMSGSeitzTimes[brav];    dinv=DMSGinvSeitz[brav];
  
  (*\:661f\:4e2d\:82e5\:6709\:6807\:51c6k\:70b9\:5c06\:5176\:8bbe\:4e3ak1\:ff0c\:5426\:5219\:8f93\:5165k\:70b9\:5c31\:662fk1*)      
  If[Length[kinfo]>4&&(tmp=Select[kstar,keqmod[#,kinfo[[5]]]&])!={}&&!keqmod[kin,tmp[[1]]],
    k1=tmp[[1]];     getstar[k1];
    MLGk1info=getMLGCorep[{sgno,num},k1/.usub,BZtype,
                       (#->OptionValue[#])&["abcOrBasVec"],"format"->False];
    kname=MLGk1info["k"][[2]];
    usub=If[Length[MLGk1info["k"]]==4, MLGk1info["k"][[4]], {}];
    ,(*--------else---------*)
    k1=kin;  MLGk1info=MLGkininfo
  ];
  If[Select[kstar2b,keqmod[#,-k1]&]!={},
    tmp=Association[kstar[[#]]->#&/@Range[ncsr]];
    kstar2b=Table[Select[kstar2b,keqmod[#,-k]&][[1]],{k,kstar2a}];
    kstar=Join[kstar2a,kstar2b];
    cosetrep=cosetrep[[tmp/@kstar]]
  ];
  subno=MLGkininfo["USubSG"];  typeIII=False;
  If[!MissingQ[subno], typeIII=True; TM=MLGkininfo["transformation_matrix"], subno=sgno];
  If[typeIII, kstar2a=TM\[Transpose].#&/@kstar2a;  kstar2b=TM\[Transpose].#&/@kstar2b];
  
  MLGk1=MLGk1info["MLG"];
  sstar="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\), \(*\)]\)";
  sLbl=sstar<>#&/@MLGk1info["slabel"];  (* corrections are made for type-x in the follow *)
  dLbl=sstar<>#&/@MLGk1info["dlabel"];
  sidx=MLGk1info["sidx"];    didx=MLGk1info["didx"];
  k1scorep=MLGk1info["scorep"];  k1dcorep=MLGk1info["dcorep"];
     
  Ettt={"E",{t\:2081,t\:2082,t\:2083},0};
  (* Efac=Exp[-I*2Pi*k1.tm[tm[inv[#],Ettt],#][[2]]]&/@cosetrep//Simplify; *)
  (* To check this, use {77,15},"W" *)
  Efac=With[{tmp=Exp[-I*2Pi*k1.tm[tm[inv[#],Ettt],#][[2]]]//FullSimplify},
             If[#[[3]]==0,tmp,tmp\[Conjugate]//FullSimplify[#,_Symbol\[Element]Reals]&]]&/@cosetrep; 
  
  invcsr=inv/@cosetrep;
  ggj=Table[tm[g,gj],{g,MSG}, {gj,cosetrep}];
  nir=Length[k1scorep];    zeros=0*k1scorep[[All,1]];
  scorep=Table[0, nir, nelem, ncsr, ncsr];   
  invgiggj=Table[tm[invcsr[[i]],ggj[[ig,j]]],{ig,nelem},{i,ncsr},{j,ncsr}];
  tmp=Flatten[invgiggj,2]//DeleteDuplicates;
  inMLG=Select[tmp, MemberQ[MLGk1[[All,{1,3}]],#[[{1,3}]]]&];
  notinMLG=Complement[tmp,inMLG];
  tmp=getRepMat[k1,MLGk1,k1scorep][inMLG]\[Transpose];
  repdict=Association[Rule@@@Transpose[{inMLG,tmp}]];
  If[notinMLG!={}, AssociateTo[repdict, #->zeros&/@notinMLG]];
  For[i=1,i<=ncsr,i++, invgi=invcsr[[i]];
    For[ig=1,ig<=nelem,ig++,
      For[j=1,j<=ncsr,j++,
        If[invgi[[3]]==0,
          scorep[[All,ig,i,j]]=repdict[invgiggj[[ig,i,j]]],
          scorep[[All,ig,i,j]]=repdict[invgiggj[[ig,i,j]]]\[Conjugate]//Simplify[#,u\[Element]Reals]&//Chop
          ];
  ]]];  
  scorep=Map[ArrayFlatten,scorep,{2}];
  dims=(Length/@zeros)/.{0->1};
  repEttt=DiagonalMatrix@Flatten[Outer[Times,Efac,Table[1,#]]]&/@dims;
  scorep[[All,1]]=repEttt;
  scorep=Map[If[Dimensions[#]=={1,1},#[[1,1]],#]&, scorep, {2}]//Simplify;  
  
  dinvcsr=dinv/@cosetrep;
  ggj=Table[dtm[g,gj],{g,MSG}, {gj,cosetrep}];
  nir=Length[k1dcorep];    zeros=0*k1dcorep[[All,1]];
  dcorep=Table[0, nir, nelem, ncsr, ncsr];   
  invgiggj=Table[dtm[dinvcsr[[i]],ggj[[ig,j]]],{ig,nelem},{i,ncsr},{j,ncsr}];
  tmp=Flatten[invgiggj,2]//DeleteDuplicates;
  inMLG=Select[tmp, MemberQ[MLGk1[[All,{1,3}]],{StringReplace[#[[1]],"bar"->""],#[[3]]}]&];
  notinMLG=Complement[tmp,inMLG];
  tmp=getRepMat[k1,MLGk1,k1dcorep][inMLG]\[Transpose];
  repdict=Association[Rule@@@Transpose[{inMLG,tmp}]];
  If[notinMLG!={}, AssociateTo[repdict, #->zeros&/@notinMLG]];
  For[i=1,i<=ncsr,i++, invgi=dinvcsr[[i]];
    For[ig=1,ig<=nelem,ig++,
      For[j=1,j<=ncsr,j++,
        If[invgi[[3]]==0,
          dcorep[[All,ig,i,j]]=repdict[invgiggj[[ig,i,j]]],
          dcorep[[All,ig,i,j]]=repdict[invgiggj[[ig,i,j]]]\[Conjugate]//Simplify[#,u\[Element]Reals]&//Chop
          ];
  ]]];  
  dcorep=Map[ArrayFlatten,dcorep,{2}];
  dims=(Length/@zeros)/.{0->1};
  repEttt=DiagonalMatrix@Flatten[Outer[Times,Efac,Table[1,#]]]&/@dims;
  dcorep[[All,1]]=repEttt;
  dcorep=Map[If[Dimensions[#]=={1,1},#[[1,1]],#]&, dcorep, {2}]//Simplify;  
  
  {k2a,kname2a,kBZ2a}=If[typeIII, MLGk1info["ksub"], MLGk1info["k"]][[1;;3]];      
  kinfo2a=If[typeIII, MLGk1info["ksubinfo"], MLGk1info["kinfo"]];
  kBZ2b=kBZ2a;   kinfo2b={};
  (*=======================for labels of type x=============================*)
  (* Examples: {79,27},"X" : M\:548cA\:914d\:5bf9\:ff1b{80,32},"\[CapitalLambda]" : \[CapitalLambda]\:548c-\[CapitalLambda]\:914d\:5bf9\:ff1b{80,32},"V" : V\:548c-V\:914d\:5bf9\:ff1b
               {79,27},"W" : U\:548cUA\:914d\:5bf9\:ff1b{154,43},"K" : K\:548c-K\:914d\:5bf9\:ff0c\:8be5\:4f8b\:4e2d\:5176\:5b9e\:662f\:4e0e-K\:7b49\:4ef7\:7684k\:70b9
               \:56e0\:800c\:5224\:65ad\:65f6\:4e0d\:80fd\:7b80\:5355\:7684 k2b\[Equal]-k2a\:ff0c\:5426\:5219\:5c06\:4f1a\:7ed9\:51fa K\:548cKA\:914d\:5bf9\:3002*)
  (* type x \:91cc\:5e76\:975e\:603b\:662fk_n\:548c(-k)_n\:914d\:5bf9\:ff0c\:6bd4\:5982 {81,37),"W" \:548c {77,15},"\[CapitalLambda]"\:3002\:6ce8\:610f\:ff0c\:82e5\:8f93\:5165\:7684\:4e3a-W\:70b9\:7684\:5750\:6807\:ff0c\:5219
     \:5f97\:5230\:7684\:5171\:8868\:793a\:5176\:7b26\:53f7\:4e0e\:8f93\:5165\:4e3aW\:65f6\:7684\:7ed3\:679c\:95f4\:5b58\:5728\:6b67\:4e49\:ff08\:8fd9\:91ccW\:4e0e-W\:5c5e\:540c\:4e00\:661f\:ff0c\:4f46\:5b58\:5728\:5171\:8868\:793a\:7b26\:53f7\:76f8\:540c
     \:800c\:5171\:8868\:793a\:4e0d\:540c\:7684\:60c5\:51b5\:ff09\:ff0c\:4e3a\:907f\:514d\:6b64\:79cd\:60c5\:51b5\:ff0c\:5e94\:5c06\:6ce2\:77e2\:661f\:6807\:51c6\:5316\:ff0c\:5373\:8f93\:5165k\:70b9\:4e0d\:8bba\:662f\:6ce2\:77e2\:661f\:91cc\:54ea\:4e00\:4e2a\:ff0c
     \:5f97\:5230\:7684\:6ce2\:77e2\:661f\:4e2dk\:70b9\:987a\:5e8f\:90fd\:5f04\:6210\:4e00\:6837\:3002 *)
  underbar=StringTemplate["\!\(\*UnderscriptBox[\(``\), \(_\)]\)"];
  If[typex&&MSG1!={},
    If[typeIII==False, 
      k2a=k1;  k2b=-k2a;   kname2a=kname;  kname2b=underbar[kname]; 
      ,(*---------------else: type-III MSG--------------------*)
      {k2a,kname2a,kBZ2a}=MLGk1info["ksub"];
      k2b=kstar2b[[1]];
      If[keqmod[k2a,-k2b],
        kname2b=underbar[kname2a];
        ,(*------else: k2b is not equivalent to -k2a ---------*)
        tmp=If[typeIII&&MatrixQ[bztypeORbvec], (MLGkininfo["rotation_matrix"].bztypeORbvec\[Transpose].TM)\[Transpose]//Simplify, bztypeORbvec]; 
        kinfo2b=identifyBCHSKptBySG[subno,tmp,(k2b/.usub)/.u->0.1];
        kname2b=kinfo2b[[2]];
        If[kname2b==kname2a, kname2b=kname2b<>"A"];
      ];
    ];
    srep2a=Map[If[MatrixQ[#],Tr[#],#]&, k1scorep, {2}]//Simplify;
    drep2a=Map[If[MatrixQ[#],Tr[#],#]&, k1dcorep, {2}]//Simplify;
    srep2a=(srep2a/.usub)/.u->0.1;    drep2a=(drep2a/.usub)/.u->0.1;
    i=Length[kstar2a]+1;
    srep2atob=srep2a;   drep2atob=drep2a;
    If[cosetrep[[i,3]]==1, srep2atob=srep2a\[Conjugate]; drep2atob=drep2a\[Conjugate]];
    If[tmp=kstar[[i]];keqmod[tmp,kin],
      MLGinfo2b=MLGkininfo;  (*\:8fd9\:91cc\:9700\:8981trace\:ff0c\:800cMLGkininfo\:91cc\:7684\:662f\:8868\:793a\:77e9\:9635\:ff0c\:9700\:8981\:5904\:7406\:ff0c\:5426\:5219\:51fa\:9519\:ff0c\:6bd4\:5982 36.173 {0,0,-0.1} *)
      MLGinfo2b["scorep"]=Map[If[MatrixQ[#],Tr[#],#]&, MLGinfo2b["scorep"], {2}]//Simplify;
      MLGinfo2b["dcorep"]=Map[If[MatrixQ[#],Tr[#],#]&, MLGinfo2b["dcorep"], {2}]//Simplify;
      ,  (* else: *)
      MLGinfo2b=getMLGCorep[{sgno,num}, (tmp/.usub)/.u->0.1, BZtype,
                    (#->OptionValue[#])&@"abcOrBasVec","trace"->True,"format"->False];
    ];
    kinfo2b=If[typeIII, MLGinfo2b["ksubinfo"], MLGinfo2b["kinfo"]];
    slb2b=StringReplace[#,"("~~__~~")"->""]&/@MLGinfo2b["slabel"];
    dlb2b=StringReplace[#,"("~~__~~")"->""]&/@MLGinfo2b["dlabel"];
    tmp=If[typeIII, MLGinfo2b["ksub"], MLGinfo2b["k"]];
    If[!keqmod[k2b,-k2a]&&kname2a!=tmp[[2]], kBZ2b=tmp[[3]]];
    If[keqmod[k2b,-k2a]&&kname2a!=tmp[[2]],
      tmp=getLGIrepTab[subno,(k2b/.usub)/.u->0.1];
      tmp2=LGIRtwokRelation[tmp];
      slb2b=tmp2/@slb2b;    dlb2b=tmp2/@dlb2b;
      tmp=Select[tmp,#["kinfo"][[2]]==kname2a&];
      kinfo2b=tmp[[1]]["kinfo"];  (* to check this use {80,32},"V" *)
    ];  
    (*\:6700\:521d\:8fd9\:4e00\:90e8\:5206\:7684\:903b\:8f91\:5f04\:9519\:4e86\:ff0c\:5bfc\:81f4 {20,33},"D"; {63,462},"D"; {85,61},"\[CapitalDelta]","U"; {152,35},"P";
    {154,43},"P" \:51fa\:9519\:ff0c\:65e0\:6cd5\:914d\:5bf9\:ff0c\:4f46\:6240\:6709\:5176\:4ed6k\:70b9\:7adf\:7136\:90fd\:80fd\:914d\:4e0a*)
    srep2b=MLGinfo2b["scorep"];       drep2b=MLGinfo2b["dcorep"];
    tmp=tm[cosetrep[[i]],tm[#,inv[cosetrep[[i]]]]]&/@MLGk1;
    (*getRepMat\:7684\:7b2c\:4e00\:4e2a\:53c2\:6570\:4e00\:5f00\:59cb\:7528kstar[[i]]\:ff0c\:4f46\:5728{88,83}\:548c{98,159}\:7684"F"\:65f6\:51fa\:95ee\:9898\:ff08"a"\:51fa\:9519\:ff0c"b"\:6b63\:5e38\:ff0c\:56e0u\:4e0d\:540c\:ff09*)
    srep2btob=getRepMat[MLGinfo2b["k"][[1]], MLGinfo2b["MLG"], srep2b][tmp];
    srep2btob=srep2btob/.If[Length[MLGinfo2b["k"]]==4,MLGinfo2b["k"][[4]],{}];
    tmp2=Table[Position[Total@Abs[((j-#)/.usub)/.u->0.1]<1*^-5&/@srep2btob,True][[1,1]],{j,srep2atob}];  
    (*\:5bf9\:4e8etypex\:ff0csidx/didx \:7ed9\:51fa\:7684\:4e00\:5bf9\:5e8f\:53f7\:5206\:522b\:662fk2a\:548c\:4e0e\:4e4b\:914d\:5bf9\:7684k2b\:7684\:5c0f\:8868\:793a\:5e8f\:53f7*)
    sidx=Transpose[{Sort[tmp2],tmp2}];
    sLbl=If[keqmod[k2b,-k2a], #1<>underbar[#2], If[kname2b==kname2a<>"A",
            #1<>StringReplace[#2,"\("<>kname2a<>"\)"->"\("<>kname2b<>"\)"], 
            #1<>#2]]&@@@Transpose[{sLbl,slb2b[[tmp2]]}];          
    tmp=dtm[cosetrep[[i]],dtm[#,dinv[cosetrep[[i]]]]]&/@MLGk1;
    drep2btob=getRepMat[MLGinfo2b["k"][[1]], MLGinfo2b["MLG"], drep2b][tmp];
    drep2btob=drep2btob/.If[Length[MLGinfo2b["k"]]==4,MLGinfo2b["k"][[4]],{}];
    tmp2=Table[Position[Total@Abs[((j-#)/.usub)/.u->0.1]<1*^-5&/@drep2btob,True][[1,1]],{j,drep2atob}];
    didx=Transpose[{Sort[tmp2],tmp2}];
    dLbl=If[keqmod[k2b,-k2a], #1<>underbar[#2], If[kname2b==kname2a<>"A",
            #1<>StringReplace[#2,"\("<>kname2a<>"\)"->"\("<>kname2b<>"\)"], 
            #1<>#2]]&@@@Transpose[{dLbl,dlb2b[[tmp2]]}];
  ];
  (*==================================================================\[Equal]*)
   
  If[OptionValue["trace"]===True, 
    scorep=Map[If[MatrixQ[#],Tr[#],#]&, scorep, {2}]//Simplify;
    dcorep=Map[If[MatrixQ[#],Tr[#],#]&, dcorep, {2}]//Simplify;
  ];
  If[OptionValue["format"]===True, 
    scorep=Map[formatRepMat,scorep,{2}];
    dcorep=Map[formatRepMat,dcorep,{2}];
    k1scorep=Map[formatRepMat,k1scorep,{2}];
    k1dcorep=Map[formatRepMat,k1dcorep,{2}];
  ];
      
  MSGinfo=<||>;    
  MSGinfo["symbol"]=MLGk1info["symbol"];
  MSGinfo["kin"]=MLGkininfo["k"];
  MSGinfo["k1"]=MLGk1info["k"];   MSGinfo["k1info"]=MLGk1info["kinfo"];     
  MSG[[1]]=Ettt;                  MSGinfo["elements"]=MSG; 
  MSGinfo["MLGk1"]=MLGk1;         MSGinfo["USubSG"]=subno;
  If[typeIII, (MSGinfo[#]=MLGk1info[#])&/@{"transformation_matrix","origin_shift","rotation_matrix"}];
  MSGinfo["cosetrep"]=cosetrep;   MSGinfo["kstar"]=kstar;
  If[typex&&MSG1!={},
    MSGinfo["ksub"]={{k2a,kname2a,kBZ2a},{k2b,kname2b,kBZ2b}};
    MSGinfo["ksubinfo"]={kinfo2a,kinfo2b};
    MSGinfo["ksubstar"]={kstar2a,kstar2b}, (* else *)
    MSGinfo["ksub"]={{k2a,kname2a,kBZ2a},{}};
    MSGinfo["ksubinfo"]={kinfo2a,{}};
    MSGinfo["ksubstar"]={kstar2a,{}}    
  ];
  MSGinfo["slabel"]=sLbl;         MSGinfo["sidx"]=sidx;     MSGinfo["stype"]=MLGk1info["stype"];
  MSGinfo["k1scorep"]=k1scorep;   MSGinfo["scorep"]=scorep;
  MSGinfo["dlabel"]=dLbl;         MSGinfo["didx"]=didx;     MSGinfo["dtype"]=MLGk1info["dtype"];
  MSGinfo["k1dcorep"]=k1dcorep;   MSGinfo["dcorep"]=dcorep;
  
  (*(*for debug*)
  Print["scorep:\n", (MatrixForm/@#)&/@scorep//Chop//TableForm[#,TableHeadings->{sLbl,showMSGSeitz/@MSG}]&];
  Print["dcorep:\n", (MatrixForm/@#)&/@dcorep//Chop//TableForm[#,TableHeadings->{dLbl,showMSGSeitz/@MSG}]&];    
  *)
  MSGinfo
]


(* ::Subsection::Closed:: *)
(*checkMSGCorep*)


(* Check whether the corep from getMSGCorep satisfies correct multiplications. 
 If the returned numbers are all zeros, the corep goes right. 
 To get accurate multiplications, use "format"\[Rule]False option in getMSGCorep. *)
checkMSGCorep[corepinfo_]:=Module[{MSG,mtab,scorep,dcorep, time, brav, fBZ, rot2elem,n,tmp,sub,
  itab,dvtab,rot2idx, repmtab, reptime, tr, check, dtime,mtab2, bartab, tm, empty},
  MSG=corepinfo["elements"]/.Thread[{t\:2081,t\:2082,t\:2083}->{0,0,0}];   n=Length[MSG];
  (*If[Position[corepinfo["kinfo"],u]!={}, sub=corepinfo["kinfo"][[9]], sub={}];*)
  sub=u->0.1;
  rot2elem=#[[{1,3}]]->#&/@MSG//Association;
  rot2idx=Flatten[{MSG[[#,{1,3}]]->#,{"bar"<>MSG[[#,1]],MSG[[#,3]]}->#}&/@Range[n]]//Association; 
  scorep=corepinfo["scorep"];  dcorep=corepinfo["dcorep"];
  fBZ=corepinfo["k1"][[3]];  If[ListQ[fBZ], fBZ=fBZ[[1]]];
  brav=If[StringTake[fBZ,-1]==")",StringTake[fBZ,1;;-4],fBZ];
  time=MSGSeitzTimes[brav];
  mtab=Table[time[i,j],{i,MSG},{j,MSG}];
  itab=Table[rot2idx[mtab[[i,j,{1,3}]]],{i,n},{j,n}];  
  dvtab=Table[tmp=mtab[[i,j]];tmp=tmp[[2]]-rot2elem[tmp[[{1,3}]]][[2]],{i,n},{j,n}];
  tr=MSG[[All,3]];
  reptime[rep_,i_,j_]:=Module[{m1,m2,tr1,tr2},
     m1=rep[[i]]; m2=rep[[j]];  tr1=tr[[i]];  tr2=tr[[j]];
     If[MatrixQ[m1], If[tr1==1,m1.m2\[Conjugate], m1.m2], If[tr1==1,m1*m2\[Conjugate], m1*m2]]//Simplify[#,u\[Element]Reals]&
  ];
  tm[m1_,m2_]:=If[MatrixQ[m1],m1.m2,m1*m2]//Simplify;
  repmtab[rep_]:=Table[reptime[rep,i,j],{i,n},{j,n}];

  dtime=DMSGSeitzTimes[brav];
  mtab2=Table[dtime[i,j],{i,MSG},{j,MSG}];  
  bartab=Table[If[mtab2[[i,j,1]]==MSG[[itab[[i,j]],1]],1,-1],{i,n},{j,n}]; 

  check[rep_,d_]:=Module[{diff, tab, Ettt, rep1},
    Ettt=rep[[1]]/.Thread[{t\:2081,t\:2082,t\:2083}->#]&;
    rep1=rep;  rep1[[1]]=Ettt[{0,0,0}];
    tab= Table[tm[Ettt[dvtab[[i,j]]],rep1[[itab[[i,j]]]]],{i,n},{j,n}];
    If[d=="d",tab=tab*bartab];
    diff=Flatten[(repmtab[rep1]-tab)/.sub];
    Total@Abs[diff//N//Chop//Simplify]
  ];
   
  empty=If[scorep=={}||dcorep=={}, 1, 0];
  {empty, check[#,"s"]&/@scorep, check[#,"d"]&/@dcorep}//Simplify
]


(* ::Subsection::Closed:: *)
(*showMSGCorep*)


Options[showMSGCorep]={"uNumeric"->False,"corep"->All,"elem"->All,"rotmat"->True,"trace"->False,
                        "spin"->"downup","abcOrBasVec"->None,"linewidth"->2,"maxDim"->4};
showMSGCorep[{sgno_,mno_}, kNameOrCoord_, OptionsPattern[]]:=showMSGCorep[{sgno,mno},
    kNameOrCoord, "a", (#->OptionValue[#])&/@{"uNumeric","corep","elem","rotmat","trace","spin",
    "abcOrBasVec","linewidth","maxDim"}]
showMSGCorep[{sgno_,mno_}, kNameOrCoord_, BZtype_, OptionsPattern[]]:=Block[{u,crinfo,brav,kin,
  showmat,idxsir,idxdir,idxelm,showrot,sx,kname,kname2a,kBZ,kBZ2a,kinfo,kinfo2a,slbl,stype,scorep,
  dlbl,dtype,dcorep, MSG,subno,R,T,os, Rang,Raxis,small,bold,dmax,rmRe0,rot,trans,srot,table,nelem,
  nsir,ndir,sfl,sfa,nc,nr,nfrom,head,h1,h2,h3,h4,h5,grid,sty1,sty2,thickHLines,tmp,nsir1,ndir1,
  bg1,bg2,bg3,bg4,bg5,bg6,nstart,emph,strkBC,kBZ2b,kinfo2b,kname2b,typeIII,sstar,kn,kstar,kstar2a,
  kstar2b,tmp2,knp,h6,h7,nka,symstd,symBC},
  crinfo=getMSGCorep[{sgno,mno},kNameOrCoord,BZtype,(#->OptionValue[#])&/@{"abcOrBasVec","trace"}];
  brav=getSGLatt[sgno];
  dmax=OptionValue["maxDim"];
  rmRe0=Map[If[MachineNumberQ[#],If[Re[#]==0,Which[Im[#]==1,I,Im[#]==-1,-I,True,Im[#]"\[ImaginaryI]"],#],#]&, #, -1]&;
  showmat[m_]:=If[!MatrixQ[m], rmRe0[m], If[Length[m]<=dmax, MatrixForm[rmRe0[m]], 
      tmp=Row[{"("<>ToString[#[[1,1]]]<>","<>ToString[#[[1,2]]]<>")",#[[2]]},":"]&;
      Partition[tmp/@ArrayRules[rmRe0[m]][[;;-2]],UpTo[2]]//Grid[#,Alignment->Left,ItemSize->Full]&
    ]];  
  showrot=OptionValue["rotmat"];
  sx={{0,1},{1,0}};

  kin=crinfo["kin"][[1]];
  kBZ=crinfo["k1"];             kname=kBZ[[2]];       kinfo=crinfo["k1info"];
  kBZ2a=crinfo["ksub"][[1]];    kname2a=kBZ2a[[2]];   {kinfo2a,kinfo2b}=crinfo["ksubinfo"];    
  kBZ2b=crinfo["ksub"][[2]]; 
  kstar=crinfo["kstar"]//Chop;        {kstar2a,kstar2b}=crinfo["ksubstar"]//Chop;
  If[kstar2b!={}, kname2b=kBZ2b[[2]]];
  slbl=crinfo["slabel"];     stype=crinfo["stype"];     scorep=crinfo["scorep"];
  dlbl=crinfo["dlabel"];     dtype=crinfo["dtype"];     dcorep=crinfo["dcorep"];
  MSG=crinfo["elements"];  subno=crinfo["USubSG"];
  nka=Length[kstar2a];
  
  If[OptionValue["uNumeric"]&&VectorQ[kNameOrCoord]&&Position[kinfo,Rule]!={}, 
     scorep=scorep/.kinfo[[9]]//Chop;  dcorep=dcorep/.kinfo[[9]]//Chop];
  nelem=Length[MSG];   nsir=Length[scorep];   ndir=Length[dcorep];
  tmp=Check[Range[nsir+ndir][[If[IntegerQ[tmp=OptionValue["corep"]], {tmp}, tmp]]],
             Print["showType3MLGCorep: index of corep out of range [1,",nsir+ndir,"]"]; Abort[],
             {Part::partw, Part::pkspec1}];
  idxsir=Select[tmp,#<=nsir&];    idxdir=Complement[tmp,idxsir];
  nsir1=Length[idxsir];    ndir1=Length[idxdir];
  idxelm=Check[Range[nelem][[If[IntegerQ[tmp=OptionValue["elem"]], {tmp}, tmp]]],
             Print["showType3MLGCorep: index of element out of range [1,",nelem,"]"]; Abort[],
             {Part::partw, Part::pkspec1}];

  sstar="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\), \(*\)]\)";
  small=Style[#,Smaller]&;  bold=Style[#,Bold]&;  emph=Style[#,Bold,Blue]&;
  strkBC="\!\(\*SubscriptBox[\(k\), \(BC\)]\)";
  kn=StringTemplate["\!\(\*SubscriptBox[\(k\), \(``\)]\)"];
  knp=StringTemplate["\!\(\*SubsuperscriptBox[\(k\), \(``\), \(\[Prime]\)]\)"];
  {symstd,symBC}=crinfo["symbol"][[{1,3}]];
  tmp=If[symBC=!=symstd,Row[{Row[{emph@symstd,","}],"","BC:",emph@symBC}," "], emph@symstd];
  h1=Row[{"MSG ",emph@Row[{sgno,".",mno}],"(",tmp,"):  k-point name of "<>
          kn[1]<>" is ", emph[kname],", for ",Row[kBZ[[3]],","]}," "];
  h2=Row[{kn[1]<>"=(",Row[kBZ[[1]],","],")"}];
  If[Position[kinfo,Rule]!={}&&VectorQ[kNameOrCoord], h2=Row[{h2,"  (u=",kinfo[[9,2]],")"}]];
  If[kname!="UN"&&kname!="GP"&&kinfo[[6,1]]!="E", 
     h2=Row[{h2," non-standard, ",strkBC,"=(",Row[kinfo[[5]],","],")  [",
             showSeitz[kinfo[[6]]][[1,2]],"]",strkBC," \[DoubleLeftRightArrow] k"}],
     If[kname!="UN"&&kname!="GP", 
       tmp=If[kinfo[[8]]=={0,0,0}, Nothing, Row[{",  ",strkBC,"=(",Row[kinfo[[5]],","],")"}]];
       h2=Row[{h2," standard",tmp}]]
  ];
  tmp=Select[Range[Length[kstar]],keqmod[kin,kstar[[#]]]&][[1]];
  tmp=If[(tmp2=Rationalize[kin-kstar[[tmp]],0.01])=={0,0,0}, {kn["in"]<>"="<>kn[tmp]},
         {kn["in"]<>"="<>kn[tmp]<>"+(",Row[If[#<0,OverBar[-#],#]&/@tmp2],")"}];
  h3=Row[{"\[FilledRightTriangle] The magnetic k-star ",emph[sstar<>kname]," (input k-point ",Sequence@@tmp,") contains:\n",
           Style[Row[Row[{Subscript[If[#==1," k","k"], #],"=(",Row[kstar[[#]],","],")"}]&/@Range@Length[kstar],";  "],Darker[Red]]}];
  R=crinfo["rotation_matrix"];  T=crinfo["transformation_matrix"]; os=crinfo["origin_shift"];
  typeIII=!MissingQ[R];
  If[typeIII,
    tmp=rotAxisAngle[R];  Rang=tmp[[3]];  Raxis=tmp[[2]]//Round;
    Rang=Switch[Rang, Pi,"\[Pi]", Pi/2,"\[Pi]/2", Pi/4,"\[Pi]/4", 2Pi/3,"2\[Pi]/3", _,Rang];
    h4=Row[{"The unitary subgroup is SG ",emph[subno],"(",emph@SGSymStd[subno],
       "), whose standard BC basic vectors are\n(",
       Row[Subsuperscript[bold["t"],#,"\[Prime]"]&/@{1,2,3}],")=R(",
       Row[Subscript[bold["t"],#]&/@{1,2,3}],")T with origin shift Os=(", Row[os,","],") and\n", 
       Column[{"transformation","matrix"},Alignment->Center],  "  T=", MatrixForm[T], "  and  ",       
       Column[{"rotation","matrix"},Alignment->Center], "  R=", MatrixForm[R], " ",
       If[Rang!=0, small@Column[{small["axis"],Row[If[#<0,OverBar[-#],#]&/@Raxis],small["angle"],Rang},Alignment->Center,Spacings->0.2], Nothing]       
       }];
    , (* else: *) 
    h4=Row[{"The unitary subgroup is SG ",emph[subno],"(",emph@SGSymStd[subno],"), k-point coordinates unchanged"}]
  ];
  h5=If[typeIII,
    Row[{"\[FilledRightTriangle] The k-star of the unitary subgroup: ",emph[sstar<>kname2a],"(",knp[1], If[nka>1,"~"<>knp[nka],Nothing],")", 
         If[kstar2b!={}, Row[{" and ",emph[sstar<>kname2b],"(",knp[nka+1], If[nka>1,"~"<>knp[2nka],Nothing],")"}], Nothing],
         " ("<>knp["i"]<>"=T\[Transpose]."<>kn["i"],If[kBZ2b!={}&&keqmod[kBZ2a[[1]],-kBZ2b[[1]]],", "<>sstar<>kname2b<>"\[DoubleLeftRightArrow]-"<>sstar<>kname2a,Nothing],")\n",
         sstar<>kname2a<>": ",Style[Row[Row[{knp[#],"=(",Row[kstar2a[[#]],","],")"}]&/@Range@Length[kstar2a],";  "],Darker[Red]],
         If[kstar2b!={},Row[{"\n"<>sstar<>kname2b<>": ",Style[Row[Row[{knp[#+nka],"=(",Row[kstar2b[[#]],","],")"}]&/@Range@Length[kstar2a],";  "],Darker[Red]]}],Nothing]}]
    ,(* ----- else ----- *) 
    Row[{"\[FilledRightTriangle] The k-star of the unitary subgroup: ",emph[sstar<>kname2a],"(",kn[1], If[nka>1,"~"<>kn[nka],Nothing],")", 
         If[kstar2b!={}, Row[{" and ",emph[sstar<>kname2b],"(",kn[nka+1], If[nka>1,"~"<>kn[2nka],Nothing],")"}], Nothing],
         If[kBZ2b!={}&&keqmod[kBZ2a[[1]],-kBZ2b[[1]]],", ("<>sstar<>kname2b<>"\[DoubleLeftRightArrow]-"<>sstar<>kname2a,Nothing],")"
        }]
  ];
  If[!typeIII, Goto["non-typeIII"]];     (*------for typeIII------*)
  tmp=If[Position[kinfo2a,\[Alpha]]=={},"", Row[{"  (", kinfo2a[[9,1]],"=",kinfo2a[[9,2]],")"}]];
  (* To check the above line, use {79,27}, {0.1,0.1,0.4} and -{0.1,0.1,0.4} *)
  h6=Row[{knp[1]<>"=(",Row[kBZ2a[[1]],","],")",
       If[kname2a!="UN"&&kname2a!="GP"&&kinfo2a[[6,1]]!="E", 
         Row[{" non-standard, "<>strkBC<>"=(",Row[kinfo2a[[5]],","],")",tmp,"  [",showSeitz[kinfo2a[[6]]][[1,2]],"]"<>strkBC<>" \[DoubleLeftRightArrow] ",knp[1]}],
         (*---else-----*)
         If[kname2a=="UN"||kname2a=="GP", "", 
           If[Position[kinfo2a,\[Alpha]]=={}, " standard", Row[{" standard, "<>strkBC<>"=(",Row[kinfo2a[[5]],","],")",tmp}]]  ]
       ], 
       " for ",If[Length[#]>1,#,#[[1]]]&@kBZ2a[[3]]
     }];
  If[kinfo2b=={}, h7=Nothing; Goto["head"]];
  tmp=If[Position[kinfo2b,\[Alpha]]=={},"", Row[{"  (", kinfo2b[[9,1]],"=",kinfo2b[[9,2]],")"}]];
  h7=Row[{knp[nka+1]<>"=(",Row[kBZ2b[[1]],","],")",
       If[kinfo2b[[2]]!="UN"&&kinfo2b[[2]]!="GP"&&kinfo2b[[6,1]]!="E", 
         Row[{" non-standard, "<>strkBC<>"=(",Row[kinfo2b[[5]],","],")",tmp,"  [",showSeitz[kinfo2b[[6]]][[1,2]],"]"<>strkBC<>" \[DoubleLeftRightArrow] ",knp[nka+1]}],
         (*---else-----*)
         If[kinfo2b[[2]]=="UN"||kinfo2b[[2]]=="GP", "", 
           If[Position[kinfo2b,\[Alpha]]=={}, " standard", Row[{" standard, "<>strkBC<>"=(",Row[kinfo2b[[5]],","],")",tmp}]]  ]
       ], 
       " for ",If[Length[#]>1,#,#[[1]]]&@kBZ2b[[3]]
     }];  
  Label["non-typeIII"]; If[typeIII, Goto["head"]];  (*------for non-typeIII-------*)
  h6=Nothing;
  If[kinfo2b=={}||kinfo2b[[2]]=="GP"||kinfo2b[[2]]=="UN", h7=Nothing; Goto["head"]];
  tmp=If[Position[kinfo2b,\[Alpha]]=={},"", Row[{"  (", kinfo2b[[9,1]],"=",kinfo2b[[9,2]],")"}]];
  h7=Row[{kn[nka+1]<>"=(",Row[kBZ2b[[1]],","],")",
       If[kinfo2b[[2]]!="UN"&&kinfo2b[[2]]!="GP"&&kinfo2b[[6,1]]!="E", 
         Row[{" non-standard, "<>strkBC<>"=(",Row[kinfo2b[[5]],","],")",tmp,"  [",showSeitz[kinfo2b[[6]]][[1,2]],"]"<>strkBC<>" \[DoubleLeftRightArrow] ",kn[nka+1]}],
         (*---else-----*)
         If[kinfo2b[[2]]=="UN"||kinfo2b[[2]]=="GP", "", 
           If[Position[kinfo2b,\[Alpha]]=={}, " standard", Row[{" standard, "<>strkBC<>"=(",Row[kinfo2b[[5]],","],")",tmp}]]  ]
       ]}];    
  Label["head"]; (*------for non-typeIII end-------*)
  head={h1,h2,h3,h4,h5,h6,h7}//Flatten//Column;
    
  rot=MatrixForm[getRotMat[brav,#]]&/@MSG[[idxelm,1]];
  trans=MatrixForm[{InputForm/@#}\[Transpose]]&/@MSG[[idxelm,2]];
  srot=getSpinRotOp[#][[1]]&/@MSG[[idxelm,1]];
  If[OptionValue["spin"]=="updown", srot=sx.#.sx&/@srot];
  srot=MatrixForm[Expand[#]]&/@srot;
  sfl=SpanFromLeft;   sfa=SpanFromAbove;
  table={idxelm, showMSGSeitz/@MSG[[idxelm]], 
         Sequence@@If[showrot,{rot, srot},{}], 
         Sequence@@Map[showmat,scorep[[idxsir,idxelm]],{2}],
         Sequence@@Map[showmat,dcorep[[idxdir-nsir,idxelm]],{2}]};
  {nr,nc}=Dimensions[table];   
  table=Join[Table[sfl,3,nr],table\[Transpose]]\[Transpose];
  table[[1,1]]="Index";     table[[2,1]]="Element";   
  If[showrot,
    table[[3,1]]=Column[{"Rotation","matrix"},ItemSize->Full];
    table[[4,1]]=Column[{If[OptionValue["spin"]=="updown","Spin(\[UpArrow]\[DownArrow])","Spin(\[DownArrow]\[UpArrow])"],"rotation","matrix"},ItemSize->Full]; 
    nstart=5,
    nstart=3
  ];   
  nfrom=nstart;
  table[[nfrom;;nfrom+nsir1-1,1]]=idxsir;
  table[[nfrom;;nfrom+nsir1-1,2]]=slbl[[idxsir]];
  table[[nfrom;;nfrom+nsir1-1,3]]=stype[[idxsir]];  
  If[ndir1>0,
    nfrom=nfrom+nsir1; 
    table[[nfrom;;nfrom+ndir1-1,1]]=idxdir;    
    table[[nfrom;;nfrom+ndir1-1,2]]=dlbl[[idxdir-nsir]];
    table[[nfrom;;nfrom+ndir1-1,3]]=dtype[[idxdir-nsir]]
  ];     
    
  sty1=Directive[Black,Thickness[OptionValue["linewidth"]]];   
  sty2=Directive[Thin,GrayLevel[0.8]];
  thickHLines={1,nstart,nstart+nsir1,-1};
  bg1={{1,nstart-1},{1,-1}}->Lighter[Yellow,0.9];
  bg2={{2,2},{1,-1}}->Lighter[Yellow,0.9];
  bg3={};  nfrom=nstart;
  bg4={{nfrom,nfrom+nsir1-1},{1,-1}}->Lighter[Green,0.95];
  nfrom+=nsir1;  
  bg5={};
  bg6={{nfrom,-1},{1,-1}}->Lighter[Blue,0.95];
  grid=Grid[table,Frame->All,Alignment->{Center,Center},ItemSize->Full,
                  Dividers->{{{{sty2}},Join[#->sty1&/@{1,4,-1},#->sty2&/@{2,3}]}, 
                            {{{sty2}},Join[#->sty1&/@thickHLines, {2->sty2}]}},
                  Background->{None,None,{bg1,bg2,bg3,bg4,bg5,bg6}}
           ];
  Column[{head,grid}]
]


(* ::Subsection::Closed:: *)
(*getMSGCorepMat*)


(*Get corep matrices for any MSG elements according to the result of getMSGCorep*)
Options[getMSGCorepMat]={"uNumeric"->False, "trace"->False};
getMSGCorepMat[crinfo_Association, OptionsPattern[]][elmOrList_]:=
  getMSGCorepMat[crinfo,All,"uNumeric"->OptionValue["uNumeric"],"trace"->OptionValue["trace"]][elmOrList]
getMSGCorepMat[crinfo_Association, cridx_, OptionsPattern[]][elmOrList_]:=Module[{M,corep,labels,nscr,ndcr,usub={},idx,ckelm,
  types,dims,tab,idxerr=False,re},
  M=crinfo["elements"];
  If[MissingQ[M], Print["getMSGCorepMat:  the argument crinfo should be the result of getMSGCorep."]; Abort[]];
  If[Length[crinfo["k1"]]==4, usub=crinfo["k1"]//Last];
  nscr=Length[crinfo["slabel"]];  ndcr=Length[crinfo["dlabel"]];  
  corep=Join[crinfo["scorep"], crinfo["dcorep"]];
(*  If[OptionValue["trace"]===True, corep=Map[If[MatrixQ[#],Tr[#],#]&, corep, {2}]//FullSimplify]; *)
  idx=Map[If[#===0,"0",#]&,cridx,{-1}];
  idx=Check[Range[nscr+ndcr][[idx]], idxerr=True];
  If[idxerr,
    labels=Join[crinfo["slabel"], crinfo["dlabel"]];
    types=Join[crinfo["stype"], crinfo["dtype"]];
    dims=getFullRepMat[M,corep,"trace"->True][{"E",{0,0,0},0}]//Rationalize;  
    tab={Join[{"corep"},{"single-valued"},Table[SpanFromLeft,nscr-1],{"double-valued"},Table[SpanFromLeft,ndcr-1]],
         Prepend[Range[nscr+ndcr],"index"],  Prepend[labels, "label"], Prepend[types,"type"], Prepend[dims,"dim"] };
    tab=Grid[tab,Frame->All,ItemSize->7,FrameStyle->Directive[Thin,Gray],Alignment->{Center,Center}];
    Print["getMSGCorepMat: index of corep out of range [1,",nscr,"]\[Union][",nscr+1,",",nscr+ndcr,"]. Refer to:\n", tab]; 
    Abort[]
  ];
  corep=corep[[idx]];
  ckelm=ListQ[#]&&Length[#]==3&&StringQ[#[[1]]]&&VectorQ[#[[2]]]&&MemberQ[{0,1},#[[3]]]&;
  If[!(ckelm[elmOrList]||And@@(ckelm/@elmOrList)),
    Print["getMSGCorepMat:  the arugment elmOrList should be one MSG element such as ",InputForm@Last@M,
      " or a list of MSG elements."]; Abort[];
  ];
  re=getFullRepMat[M,corep,"trace"->OptionValue["trace"]][elmOrList];
  If[OptionValue["trace"], re=FullSimplify[re]];
  If[OptionValue["uNumeric"],re=re/.usub];
  re
]


(* ::Section:: *)
(*MSGCorepDirectProduct*)


(* ::Subsection::Closed:: *)
(*MSGCorepDirectProduct*)


Options[MSGCorepDirectProduct]={"abcOrBasVec"->None};
MSGCorepDirectProduct[{sgno_Integer, mno_},kin1_,kin2_,OptionsPattern[]]:=Module[{checktypex,k1typex,
  k2typex,k1corep,k2corep,TM,k1coord,k2coord,k1name,k2name,k1BZs,k2BZs,typeIII,subno,sstar,nk1sir,nk1dir,k1idx,
  k1type,k1label,nk2sir,nk2dir,k2idx,k2type,k2label,tmp,subk1,subk1b,subk2,subk2b,subk1name,subk1bname,
  subk2name,subk2bname,k1usub,k2usub,HSKpt,subDP,opts,subk3s,nsubk3,k3s,k3keys,nk3,i,j,mapkidxup,k3types,
  mapkidxdown,k3coreps,tmpx,k3labels,k3idxs,tmpusub,k3substars,k3names,subk3names,xstar,i1,j1,
  RM,subopts,instar,ij,mapkdown,tmpsubk3,tmpnames,tmp1,tmp2,c,d,reDP,kn,calcDP,k3irep2corep,k3corepdim,
  samestar=False,MSG,subSG,brav,subbrav,subk3stars,ijnsubk3,subk3partner,getcrs,typeI},
  
  HSKpt=Select[LGIrep[sgno], VectorQ[#[[2,1,2]]]&]//Keys;
  If[StringQ[kin1]&&!MemberQ[HSKpt,kin1]||StringQ[kin2]&&!MemberQ[HSKpt,kin2], 
     Print["MSGCorepDirectProduct: k-name is only supported for high-symmetry k-points, i.e. one of ",HSKpt,
           ", not k-lines. Or use numeric coordinates for k-points."];
     Abort[]
  ];
  
  MSG=getMSGElem[{sgno,mno}];   brav=getSGLatt[sgno];
  typeI=Length@Union@MSG[[All,3]]==1;
  If[typeI,
    k1coord=If[StringQ[kin1],kBCcoord[sgno,kin1][[1,1]],kin1];
    k2coord=If[StringQ[kin2],kBCcoord[sgno,kin2][[1,1]],kin2];
    subDP=SGIrepDirectProduct[sgno,kin1,kin2,"abcOrBasVec"->OptionValue["abcOrBasVec"]][[1]];
    reDP=<||>;
    reDP["BZs"]=subDP[[1]];
    reDP["kin"]={subDP[[2]],{k1coord,k2coord}}//Transpose;
    reDP["samestar"]="determined later";
    reDP["ncorep"]=subDP[[3]];
    reDP["kout"]=subDP[[4]];
    reDP["result"]=<|"label"->#["label"][[1]],"dim"->#["dim"], "corep"->#["irep"][[All,{1,2,4}]]|>&/@subDP[[5]];
    reDP["samestar"]=Length[subDP[[5]]]<Times@@(Total/@subDP[[3]]);
    Return[reDP]
  ];
  
  checktypex[k_]:=!MemberQ[getMLGElem[brav,MSG,If[StringQ[k],kBCcoord[sgno,k][[1,1]],k]][[All,3]],1];
  k1typex=checktypex[kin1];     k2typex=checktypex[kin2]; 
  opts=Sequence["format"->False,"abcOrBasVec"->OptionValue["abcOrBasVec"]];
  If[kin1===kin2||VectorQ[kin1]&&VectorQ[kin2]&&keqmod[kin1,kin2], samestar=True];
  (*\:53ea\:6709typex\:624d\:9700\:8981\:8ba1\:7b97getMSGCorep\:7528\:4ee5\:786e\:5b9a\:914d\:5bf9\:7684k\:661f\:ff0c\:5426\:5219\:53ea\:7b97getMLGCorep\:4ee5\:63d0\:9ad8\:6548\:7387*)
  k1corep=If[k1typex, getMSGCorep, getMLGCorep][{sgno,mno},kin1,opts];
  k2corep=If[samestar, k1corep, If[k2typex, getMSGCorep, getMLGCorep][{sgno,mno},kin2,opts]];
  TM=k1corep["transformation_matrix"];   subno=k1corep["USubSG"];  RM=k1corep["rotation_matrix"];
  typeIII=!MissingQ[TM];     
  If[!typeIII, TM=RM=IdentityMatrix[3]; subno=sgno];
  tmp=If[k1typex, k1corep["kin"], k1corep["k"]];   (* tmp\:957f\:4e3a4\:65f6\:6700\:540e\:4e00\:9879\:662fu\[Rule]xxx *)
  k1usub=If[Length[tmp]==4,tmp[[4]],{}];   {k1coord,k1name,k1BZs}=tmp[[;;3]]/.k1usub; 
  tmp=If[k2typex, k2corep["kin"], k2corep["k"]];  
  k2usub=If[Length[tmp]==4,tmp[[4]],{}];   {k2coord,k2name,k2BZs}=tmp[[;;3]]/.k2usub;
  subSG=getSGElem[subno];    subbrav=getSGLatt[subno];

  instar[star_,kpt_]:=Module[{i,nk}, nk=Length[star];
    For[i=1,i<=nk,i++,If[keqmod[kpt,star[[i]]], Return[True]]];
    Return[False]
  ];
    
  If[!samestar,
    If[k1typex, 
      tmp=k1corep["kstar"]/.k1usub; tmp1=k2coord,
      If[k2typex, tmp=k2corep["kstar"]/.k2usub; tmp1=k1coord]
    ];
    If[k1typex||k2typex,
      samestar=instar[tmp,tmp1],  (*else*)
      For[i=1,i<=Length[MSG],i++, 
        tmp=getRotMatOfK[brav,MSG[[i,1]]].k1coord*If[MSG[[i,3]]==0,1,-1];
        If[keqmod[tmp,k2coord], samestar=True; Break[]]
      ];
    ];
  ];
  
  sstar="\!\(\*SuperscriptBox[\(\[InvisiblePrefixScriptBase]\), \(*\)]\)";
  nk1sir=Length[k1corep["stype"]];  nk1dir=Length[k1corep["dtype"]];
  k1type=Join[k1corep["stype"],k1corep["dtype"]];
  k1label=Join[k1corep["slabel"],k1corep["dlabel"]];  If[!k1typex,k1label=sstar<>#&/@k1label];
  k1idx=Join[tmp=k1corep["sidx"],k1corep["didx"]+Max@Flatten[tmp]];
  nk2sir=Length[k2corep["stype"]];  nk2dir=Length[k2corep["dtype"]];
  k2type=Join[k2corep["stype"],k2corep["dtype"]];
  k2label=Join[k2corep["slabel"],k2corep["dlabel"]];  If[!k2typex,k2label=sstar<>#&/@k2label];
  k2idx=Join[tmp=k2corep["sidx"],k2corep["didx"]+Max@Flatten[tmp]];
  
  {subk1b,subk1bname,subk2b,subk2bname}={None,None,None,None};
  If[k1typex,
    tmp=k1corep["ksubinfo"];  
    {subk1,subk1name}=tmp[[1,{1,2}]];  
    If[tmp[[2]]!={}, {subk1b,subk1bname}=tmp[[2,{1,2}]]], (* --- else --- *)
    subk1=k1coord; subk1name=k1name;
    If[typeIII, {subk1,subk1name}=k1corep["ksubinfo"][[{1,2}]]]
  ];
  If[k2typex,
    tmp=k2corep["ksubinfo"];  
    {subk2,subk2name}=tmp[[1,{1,2}]];  
    If[tmp[[2]]!={}, {subk2b,subk2bname}=tmp[[2,{1,2}]]], (* --- else --- *)
    subk2=k2coord; subk2name=k2name;
    If[typeIII, {subk2,subk2name}=k2corep["ksubinfo"][[{1,2}]]]
  ];    
  {subk1,subk1b}={subk1,subk1b}/.k1usub;
  {subk2,subk2b}={subk2,subk2b}/.k2usub;
  
  (* (* ------- for debug --------- *)
  Print["{k1coord,k1name,k1BZs}=",{k1coord,k1name,k1BZs}];
  Print["{nk1sir,nk1dir,k1type}=",{nk1sir,nk1dir,k1type}];
  Print["{k1label,k1idx}=",{k1label,k1idx}];
  Print["{k2coord,k2name,k2BZs}=",{k2coord,k2name,k2BZs}];
  Print["{nk2sir,nk2dir,k2type}=",{nk2sir,nk2dir,k2type}];
  Print["{k2label,k2idx}=",{k2label,k2idx}];
  Print["{subk1,subk1name,subk1b,subk1bname}=",{subk1,subk1name,subk1b,subk1bname}];
  Print["{subk2,subk2name,subk2b,subk2bname}=",{subk2,subk2name,subk2b,subk2bname}];    *)

  tmp=OptionValue["abcOrBasVec"];
  If[Position[tmp,Rule]!={}, tmp=BasicVectors[brav]/.tmp];
  If[tmp=!=None, tmp=(RM.tmp\[Transpose].TM)\[Transpose]//Simplify];  (*convert the basic vectors to the subgroup's ones*)
  If[MemberQ[{"TricPrim","MonoPrim","MonoBase"},subbrav], tmp=None]; (*avoid checkBasVec for Tric and Mono*)
  subopts="abcOrBasVec"->tmp;
  subDP=<||>;
  subDP["ij"]=SGIrepDirectProduct[subno,subk1,subk2,subopts];
  If[k1typex, subDP["i'j"]=SGIrepDirectProduct[subno,subk1b,subk2,subopts]];
  If[k2typex, subDP["ij'"]=SGIrepDirectProduct[subno,subk1,subk2b,subopts]];
  If[k1typex&&k2typex, subDP["i'j'"]=SGIrepDirectProduct[subno,subk1b,subk2b,subopts]];
    
  (*\:6536\:96c6subDP\:6240\:6709\:5206\:91cf\:91cc\:7684k\:ff0c\:5148\:521d\:6b65\:53bb\:9664\:91cd\:590d\:7684\:ff0c\:7136\:540e\:518d\:53bb\:9664\:5904\:4e8e\:76f8\:540c\:661f\:7684\:3002\:4f8b\:5982 79.27,"X","X" \:7684\:7ed3\:679c\:661f\:5c31\:5728\:4e0d\:540c\:5206\:91cf\:91cc*)
  subk3s=DeleteDuplicates[Join@@(Values/@Values@subDP[[All,1,4]]),keqmod[#1[[2]],#2[[2]]]&];
  nsubk3=Length[subk3s];  
  tmp=getRotMatOfK[subbrav,#]&/@subSG[[All,1]];
  subk3stars=Table[Gather[#.subk3s[[i,2]]&/@tmp, keqmod][[All,1]], {i,nsubk3}];
  ijnsubk3=subDP["ij"][[1,4]]//Length;
  For[tmp1=<||>;i=ijnsubk3+1,i<=nsubk3,i++,
    For[j=1,j<i,j++,
      If[KeyExistsQ[tmp1,j], Continue[]];
      If[instar[subk3stars[[j]],subk3s[[i,2]]], tmp1[i]=1; Break[]]
    ];
  ];
  tmp=Complement[Range[nsubk3],Keys[tmp1]];
  subk3s=subk3s[[tmp]];   subk3stars=subk3stars[[tmp]];  nsubk3=Length[tmp];
  
  subk3partner=Association@Table[i->False,{i,nsubk3}];
  mapkidxup=mapkidxdown=k3coreps=k3labels=k3types=k3idxs=<||>;
  k3s=If[!typeIII,subk3s,{#1,Inverse[TM\[Transpose]].#2}&@@@subk3s]//Chop;   (*this is only initial value*)
  k3s={#1,modone[#2+(0.5-1*^-14)]-(0.5-1*^-14)}&@@@k3s;
  For[j=0; i=1,i<=nsubk3,i++, 
    If[KeyExistsQ[mapkidxup,i], Continue[]];
    tmpx=checktypex[k3s[[i,2]]];
    k3coreps[++j]=If[tmpx, getMSGCorep, getMLGCorep][{sgno,mno},k3s[[i,2]],opts];
    tmp=k3coreps[j][If[tmpx,"k1info","kinfo"]];
    k3s[[i,1]]={tmp[[2]], If[tmp[[-1]]==="not in G",1,0], If[tmpx,2,1]*subk3s[[i,1,3]]};
    mapkidxup[i]=j;   mapkidxdown[j]=i;  
    k3types[j]=Join[k3coreps[j]["stype"],k3coreps[j]["dtype"]];
    k3labels[j]=Join[k3coreps[j]["slabel"],k3coreps[j]["dlabel"]];
    k3idxs[j]=Join[tmp=k3coreps[j]["sidx"],k3coreps[j]["didx"]+Max@Flatten[tmp]];
    If[!tmpx, k3labels[j]=sstar<>#&/@k3labels[j]];
    tmpusub=k3coreps[j][If[tmpx,"kin","k"]]//Last;  If[Position[tmpusub,Rule]=={}, tmpusub={}];
    If[tmpx, 
      xstar=k3coreps[j]["ksubstar"]/.tmpusub; (* \:9700\:5224\:65adsubk3s[[i]]\:5bf9\:5e94\:7684\:5230\:5e95\:662f\:7b2c\:4e00\:4e2a\:8fd8\:662f\:7b2c\:4e8c\:4e2astar*)
      If[instar[xstar[[1]],subk3s[[i,2]]], 
        (*------- \:5bf9\:5e94\:7b2c\:4e00\:4e2a\:661f\:ff0c\:7ee7\:7eed\:7b2c\:4e8c\:4e2a\:661f\:91cc\:7684 -------*)
        For[i1=i+1,i1<=nsubk3,i1++, 
          If[KeyExistsQ[mapkidxup,i1], Continue[]];
          If[instar[xstar[[2]],subk3s[[i1,2]]], mapkidxup[i1]=j; subk3partner[i1]=True; Break[]]
        ],
        (*-------- else: \:5bf9\:5e94\:7b2c\:4e8c\:4e2a\:661f\:ff0c\:7ee7\:7eed\:7b2c\:4e00\:4e2a\:661f\:91cc\:7684 -------*)
        For[i1=i+1,i1<=nsubk3,i1++,  (* 79.27,Z,X *)
          If[KeyExistsQ[mapkidxup,i1], Continue[]];
          If[instar[xstar[[1]],subk3s[[i1,2]]], 
            mapkidxup[i1]=j;  mapkidxdown[j]=i1; 
            k3s[[i1,1]]=k3s[[i,1]];  (*\:5982\:65e0\:8fd9\:53e5\:4f1a\:5bfc\:81f4k3s\:540d\:79f0\:51fa\:9519\:ff0c\:4f8b\:5982\:ff1a 79.27,X,N *)
            subk3partner[i]=True; Break[]
          ]
        ];
      ];
      If[i1>nsubk3, Print["MSGCorepDirectProduct: Error, cannot find the k partner of subk3s[[",i,"]]"]];        
    ];
  ];  
  k3substars=Association@Table[i->subk3stars[[mapkidxdown[i]]],{i,Length[mapkidxdown]}];
  
  subk3names=<||>;
  For[i=1,i<=nsubk3,i++, j=mapkidxup[i];
    If[k3types[j][[1]]!="x", 
      subk3names[i]=k3coreps[j][If[typeIII,"ksubinfo","kinfo"]][[2]], (*else*)
      tmp=If[subk3partner[i],2,1];
      subk3names[i]=k3coreps[j]["ksubinfo"][[tmp,2]]
    ];
  ];
  If[Values[subk3names]!=subk3s[[All,1,1]], 
    (*\:6309\:8bf4subk3names\:5e94\:8be5\:7b49\:4e8esubk3s[[All,1,1]]\:ff0c\:52a0\:6b64\:5224\:65ad\:7528\:4e8e\:8c03\:8bd5*)
    Print["MSGCorepDirectProduct: Warning! subk3names ",subk3names," != subk3s[[All,1,1]] ",subk3s[[All,1,1]]];
  ];
  
  k3s=k3s[[Values[mapkidxdown]]];   nk3=Length[k3s];  (*\:6b63\:5f0f\:7684k3s\:4ece\:8fd9\:91cc\:5f00\:59cb*)
  k3irep2corep=k3corepdim=<||>; 
  For[j=1,j<=nk3,j++, 
    tmp=If[k3types[j][[1]]=="x", k3idxs[j][[All,1]], k3idxs[j]];  
    tmp=Table[If[ListQ[tmp[[i]]],#->i&/@tmp[[i]],tmp[[i]]->i],{i,Length[tmp]}];
    k3irep2corep[j]=Association@Flatten[tmp];
    tmp=Join[k3coreps[j,"scorep"],k3coreps[j,"dcorep"]][[All,1]]; 
    tmp=If[MatrixQ[#],Length[#],1]&/@tmp;
    k3corepdim[j]=If[k3types[j][[1]]=="x", 1, Length[MSG]/Length[k3coreps[j,"MLG"]]]*tmp;
  ];

  (*\:4ecesubDP["ij"]\:91cc\:9009\:51fa\:5177\:6709\:6240\:9700k\:540d\:79f0\:7684\:90a3\:7ec4\:6570\:636e*)
  tmp={#[[2,All,1]],Values[#[[4]]][[All,1,1]]}&/@subDP["ij"];  
  i=If[Length[tmp]==1, 1,
    First@Select[Range[Length[tmp]],tmp[[#]]=={{subk1name,subk2name},Values[subk3names][[;;ijnsubk3]]}&]
    ]; 
  subDP["ij"]=subDP["ij"][[i]];
  
  (* (*---- for debug ----*)
  Echo[subk3stars,"subk3stars"];
  Echo[subk3s,"subk3s"];  
  Echo[k3s,"k3s"];
  Echo[subk3names,"subk3names"];
  Echo[k3substars,"k3substars"];
  Echo[mapkidxup,"mapkidxup"];
  Echo[mapkidxdown,"mapkidxdown"];
  Echo[k3labels,"k3labels"];
  Echo[k3types,"k3types"];
  Echo[k3idxs,"k3idxs"];
  Echo[k3irep2corep,"k3irep2corep"];
  Echo[k3corepdim,"k3corepdim"];
  Echo[subk3partner,"subk3partner"];  
  Print["ij: ",tmp,",  i=",i];
  If[KeyExistsQ[subDP,"i'j"],{#[[2,All,1]],Values[#[[4]]][[All,1,1]]}&/@subDP["i'j"]//Print["i'j: ",#]&];
  If[KeyExistsQ[subDP,"ij'"],{#[[2,All,1]],Values[#[[4]]][[All,1,1]]}&/@subDP["ij'"]//Print["ij': ",#]&];
  If[KeyExistsQ[subDP,"i'j'"],{#[[2,All,1]],Values[#[[4]]][[All,1,1]]}&/@subDP["i'j'"]//Print["i'j': ",#]&]; *)
 
  (* "i'j","ij'","i'j'" \:4e09\:79cd\:79cd\:60c5\:51b5\:4e0b k3s \:4e2d\:7684k\:70b9\:5206\:522b\:5bf9\:5e94\:5176\:5b50\:7fa4\:4e2d\:7684\:7b2c\:51e0\:4e2ak\:70b9\:ff0c\:5b58\:4e8emapkdown\:4e2d\:3002
     \:540c\:65f6\:ff0c\:7b5b\:9009\:51fa\:4e09\:79cd\:60c5\:51b5\:4e0b\:5177\:6709\:4e0e "ij" \:5339\:914d\:7684k\:70b9\:540d\:79f0\:7684\:90a3\:7ec4\:6570\:636e *) 
  mapkdown=<||>;
  Do[If[!KeyExistsQ[subDP,ij], Continue[]]; mapkdown[ij]=<||>;
    tmpsubk3=Values[subDP[ij][[1,4]]][[All,2]];  (* subk3s for ij orther than "ij" *)
    tmpnames={#[[2,All,1]],Values[#[[4]]][[All,1,1]]}&/@subDP[ij];
    tmp=<||>;
    For[j=1,j<=nk3,j++,
      For[i=1,i<=Length[tmpsubk3],i++, If[KeyExistsQ[tmp,i], Continue[]];
        If[instar[k3substars[[j]],tmpsubk3[[i]]], mapkdown[ij,j]=i; tmp[i]=j; Break[]];
      ];
      If[i>Length[tmpsubk3],   mapkdown[ij,j]=0];   (* 0 for invalid *)
    ];
    If[Length[tmpnames]==1, subDP[ij]=subDP[ij][[1]]; Continue[]]; 
    tmp=DeleteCases[Values[mapkdown[ij]],0];
    tmp1=Switch[ij, "i'j",{subk1bname,subk2name}, "ij'",{subk1name,subk2bname}, "i'j'",{subk1bname,subk2bname}];
    tmp1={tmp1, subk3names[mapkidxdown[#]]&/@Keys@Select[mapkdown[ij],#!=0&]};
    For[i=1,i<=Length[tmpnames],i++,
      tmp2={tmpnames[[i,1]],tmpnames[[i,2,tmp]]};
      If[tmp2==tmp1, Break[]];
    ];
    If[i>Length[tmpnames], Print["MSGCorepDirectProduct: Error! i>",Length[tmpnames]," for ij=",InputForm[ij]]; Abort[]];
    subDP[ij]=subDP[ij][[i]];
  ,{ij,{"i'j","ij'","i'j'"}}];
  
  mapkdown["ij"]=Append[mapkidxdown,#->0&/@Keys@Select[mapkidxdown,#>ijnsubk3&]];  (*"ij"\:7684\:4e5f\:52a0\:5230mapkdown\:91cc*)
  
  (* (*---- for debug ----*)
  Echo[mapkdown,"mapkdown"];
  Echo[subDP,"subDP"];  *)
  
  (* cri, crj, crk are the indexes of the MSG coreps *)
  kn=\!\(\*
TagBox[
StyleBox[
RowBox[{"\"\<\\!\\(\\*SubscriptBox[\\(k\\), \\(\>\"", "<>", 
RowBox[{"ToString", "[", "#", "]"}], "<>", "\"\<\\)]\\)\>\""}],
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\) &;
  c[ij_String][cri_Integer,crj_Integer,{ik_Integer,crk_Integer}]:=Module[{typei,typej,typek,DP,
    scri,scrj,scrk,sik,idxi,idxj,idxk,coeff,ij2},
    typei=k1type[[cri]];  typej=k2type[[crj]];  typek=k3types[ik][[crk]];
    idxi=If[ListQ[#],#,{#}]&/@k1idx;
    idxj=If[ListQ[#],#,{#}]&/@k2idx;
    idxk=If[ListQ[#],#,{#}]&/@k3idxs[ik];
    Which[ij=="ij", (*-----------ij-------------*)
      DP=subDP["ij"][[5]];  sik=mapkdown[ij,ik];  If[MissingQ[sik]||sik==0,Return[0]];
      scri=idxi[[cri,1]]; scrj=idxj[[crj,1]]; scrk=idxk[[crk,1]],
      ij=="i'j",    (*-----------i'j-------------*)
      If[typei=="a"||typei=="b", Return[0]];
      ij2=If[typei=="c", "ij", ij];
      DP=subDP[ij2][[5]];  sik=mapkdown[ij2,ik];  If[MissingQ[sik]||sik==0,Return[0]];
      scri=idxi[[cri,2]]; scrj=idxj[[crj,1]]; scrk=idxk[[crk,1]],
      ij=="ij'",    (*-----------ij'-------------*)
      If[typej=="a"||typej=="b", Return[0]];
      ij2=If[typej=="c", "ij", ij];
      DP=subDP[ij2][[5]];  sik=mapkdown[ij2,ik];  If[MissingQ[sik]||sik==0,Return[0]];
      scri=idxi[[cri,1]]; scrj=idxj[[crj,2]]; scrk=idxk[[crk,1]],
      ij=="i'j'",    (*-----------i'j'-------------*)
      If[typei=="a"||typei=="b"||typej=="a"||typej=="b", Return[0]];
      ij2=Switch[{typei,typej}, {"c","c"}, "ij", {"c","x"},"ij'",{"x","c"},"i'j",{"x","x"},"i'j'"];
      DP=subDP[ij2][[5]];  sik=mapkdown[ij2,ik];  If[MissingQ[sik]||sik==0,Return[0]];
      scri=idxi[[cri,2]]; scrj=idxj[[crj,2]]; scrk=idxk[[crk,1]]
    ]; 
    coeff=DP[If[samestar&&scri>scrj,{scrj,scri},{scri,scrj}],"irep",kn[sik]][[1]];
    coeff=Association[#2->#1&@@@coeff];
    If[KeyExistsQ[coeff,scrk], coeff[scrk], 0]
  ];
     
  (* BC-Tab 7.8 *)
  d[cri_Integer,crj_Integer,{ik_Integer,crk_Integer}]:=Module[{typei,typej,typek,factor,ijs,re},
    typei=k1type[[cri]]/."x"->"c";  typej=k2type[[crj]]/."x"->"c";  typek=k3types[ik][[crk]];
    Switch[{typei,typej},
      {"a","a"}, factor=1; ijs={"ij"},
      {"a","b"}|{"b","a"}, factor=2; ijs={"ij"},
      {"a","c"}, factor=1; ijs={"ij","ij'"},
      {"c","a"}, factor=1; ijs={"ij","i'j"},
      {"b","b"}, factor=4; ijs={"ij"},
      {"b","c"}, factor=2; ijs={"ij","ij'"},
      {"c","b"}, factor=2; ijs={"ij","i'j"},
      {"c","c"}, factor=1; ijs={"ij","i'j","ij'","i'j'"}
    ];
    re=factor*Total[c[#][cri,crj,{ik,crk}]&/@ijs];
    If[typek=="b", re/2, re]
  ];

  (*\:5224\:65ad\:5171\:8868\:793acri\:4e0ecrj\:76f4\:79ef\:540e\:7684ik\:661f\:91cc\:5b58\:5728\:54ea\:4e9b\:5171\:8868\:793a*)
  (*test: 79.27, \[CapitalGamma],X; X,X; \[CapitalGamma],\[CapitalGamma] *)
  getcrs[cri_,crj_,ik_]:=Module[{typei,typej,idxi,idxj,ij,ij2,crs={},subi,subj,irep,sik,i12,j12},
    typei=k1type[[cri]];  typej=k2type[[crj]];
    If[ik>nk3, Return[{}]];
    idxi=If[ListQ[#],#,{#}]&/@k1idx;  
    idxj=If[ListQ[#],#,{#}]&/@k2idx; 
    Do[Switch[ij,
      "ij", ij2=ij; i12=j12=1,
      "i'j", If[typei=="a"||typei=="b", Continue[]]; 
        ij2=If[typei=="c","ij",ij]; {i12,j12}={2,1},
      "ij'", If[typej=="a"||typej=="b", Continue[]]; 
        ij2=If[typej=="c","ij",ij]; {i12,j12}={1,2},
      "i'j'", If[typei=="a"||typei=="b"||typej=="a"||typej=="b", Continue[]];
        ij2=Switch[{typei,typej}, {"c","c"}, "ij", {"c","x"},"ij'",{"x","c"},"i'j",{"x","x"},"i'j'"];
        {i12,j12}={2,2}
      ];
      subi=If[ListQ[#],#,{#}]&@k1idx[[cri]];  subi=subi[[i12]];
      subj=If[ListQ[#],#,{#}]&@k2idx[[crj]];  subj=subj[[j12]];
      sik=mapkdown[ij2,ik]; If[sik==0, Continue[]];  
      irep=subDP[ij2][[5]][If[samestar,Sort[#],#]&@{subi,subj},"irep",kn[sik]]; 
      crs=Append[crs,irep[[1,All,2]]];
    ,{ij, {"ij","i'j","ij'","i'j'"}}];
    k3irep2corep[ik]/@(crs//Flatten//Union)//Union
  ];


  calcDP[cri_Integer,crj_Integer]:=Module[{label,dim,corep,subi,subj,subdat,crs,tmp},
    label={k1label[[cri]],k2label[[crj]]};  
    subi=k1idx[[cri]]; If[ListQ[subi],subi=First[subi]];
    subj=k2idx[[crj]]; If[ListQ[subj],subj=First[subj]];
    subdat=subDP["ij"][[5]][{subi,subj}];
    dim={If[k1type[[cri]]=="a",1,2],If[k2type[[crj]]=="a",1,2]}*subdat["dim"];
    corep=Table[crs=getcrs[cri,crj,i];
      kn[i]->{tmp=({d[cri,crj,{i,#}],#}&/@crs), 
       tmp=tmp[[All,1]]; If[#1>1,ToString[#1],""]<>#2&@@@Transpose[{tmp,k3labels[i][[crs]]}], 
       k3corepdim[i][[crs]]}, {i,nk3}]//Association;
    (*------- for debug: check dimension ---------*)
    If[dim[[1]]*dim[[2]]!=Total@Flatten[#1[[All,1]]*#3&@@@Values[corep]],
      Print["DP error: dimensions not equal for coreps ",{cri,crj}];
    ];
    (*-----------------------------------------*)
    <|"label"->label,"dim"->dim,"corep"->corep|>
  ];

  
  reDP=<||>;
  reDP["BZs"]=Intersection[k1BZs,k2BZs];
  tmp1=If[k1corep[If[k1typex,"k1info","kinfo"]][[-1]]==="not in G",1,0];
  tmp2=If[k1typex,2,1]*subDP["ij"][[2,1,3]];
  tmp={{k1name,tmp1,tmp2},k1coord};  
  tmp1=If[k2corep[If[k2typex,"k1info","kinfo"]][[-1]]==="not in G",1,0];
  tmp2=If[k2typex,2,1]*subDP["ij"][[2,2,3]];
  reDP["kin"]={tmp,{{k2name,tmp1,tmp2},k2coord}};
  reDP["samestar"]=samestar;
  reDP["ncorep"]={{nk1sir,nk1dir},{nk2sir,nk2dir}};
  reDP["kout"]=Association@Table[kn[i]->k3s[[i]],{i,nk3}];
  tmp=Flatten[Table[{i,j},{i,nk1sir+nk1dir},{j,nk2sir+nk2dir}],1];
  If[samestar, tmp=Select[tmp,#[[1]]<=#[[2]]&]];
  reDP["result"]=Association@(#->calcDP@@#&/@tmp);
  reDP
    
]


(* ::Subsection::Closed:: *)
(*showMSGCorepDirectProduct*)


Options[showMSGCorepDirectProduct]={"abcOrBasVec"->None,"linewidth"->2,"numPerRow"->4};
showMSGCorepDirectProduct[{sgno_Integer, mno_}, kin1_, kin2_, OptionsPattern[]]:=Module[{DPdat,showk,bold,
  kcoord1,kcoord2,BZ,nsir1,ndir1,nsir2,ndir2,DP,k3dict,h0,h1,h2,kname1,kname2,kt1,kt2,skp,sstar,dag,
  h3,h4,k3label,tmp,narm1,narm2,head,ir1ir2,table,key1,key2,key3,n1,n2,n3,show1,show2,sty1,sty2,bg0,bg1,
  bg2,bg3,tab,maxNitem,npr=OptionValue["numPerRow"]},
  DPdat=MSGCorepDirectProduct[{sgno,mno},kin1,kin2,"abcOrBasVec"->OptionValue["abcOrBasVec"]];
  showk[k_]:="("<>StringRiffle[ToString[Chop@Round[#,1.*^-5]]&/@k,","]<>")";
  bold=Style[#,Bold]&;
  
  BZ=DPdat["BZs"];
  {{{kname1,kt1,narm1},kcoord1},{{kname2,kt2,narm2},kcoord2}}=DPdat["kin"];
  {{nsir1,ndir1},{nsir2,ndir2}}=DPdat["ncorep"];
  k3dict=DPdat["kout"];
  DP=DPdat["result"];

  skp="\!\(\*SuperscriptBox[\(k\), \(\[Prime]\)]\)";
  sstar=\!\(\*
TagBox[
StyleBox["\"\<\\!\\(\\*SuperscriptBox[\\(\\), \\(*\\)]\\)\>\"",
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\);
  dag="\!\(\*SuperscriptBox[\(\), \(\[Dagger]\)]\)"; 
  h0=Row[{"For Brillouin zone type", If[Length[BZ]>1,"s: ",": "], bold[Row[BZ,","]]}];
  h1=Row[{"Direct products of coreps of magnetic space group ",bold[MSGSymStd[{sgno,mno}]]," (No. ",bold[Row[{sgno,".",mno}]],"):"}];
  h2=Row[{sstar<>"k \[CircleTimes] "<>sstar<>skp<>":  k=",showk[kcoord1],"[",kname1,If[kt1==1,dag,""],",",narm1,
         "],  "<>skp<>"=",showk[kcoord2],"[",kname2,If[kt2==1,dag,""],",",narm2,"]"}];
  If[Length[DP]<(nsir1+ndir1)(nsir2+ndir2),   h2=Row[{h2," (in the same star)"}]];
  k3label=Keys[k3dict];
  h3="The results contain magnetic stars "<>sstar<>First[k3label]<>"~"<>sstar<>Last[k3label]<>":";
  tmp=StringJoin[{#, "=", showk[k3dict[#][[2]]], "[", k3dict[#][[1,1]], 
                  If[k3dict[#][[1,2]]==1,dag,""],",",ToString[k3dict[#][[1,3]]], "]"}]&/@k3label;
  h4=TableForm[Partition[tmp,UpTo[3]]];
  head=Column[{h1,h0,h2,"",h3,h4,""}];  
  ir1ir2=Keys[DP];
        
  table=Table["", Length[ir1ir2]+1, 4];
  table[[1]]={"Direct Product",SpanFromLeft,SpanFromLeft,"Results"};
  key1=Select[ir1ir2, #[[1]]<=nsir1&&#[[2]]<=nsir2&];
  key2=Select[ir1ir2, #[[1]]<=nsir1&&#[[2]]>nsir2||#[[1]]>nsir1&&#[[2]]<=nsir2&];
  key3=Select[ir1ir2, #[[1]]>nsir1&&#[[2]]>nsir2&];
  {n1,n2,n3}=Length/@{key1,key2,key3};
    
  maxNitem=Max[Length/@(Join@@@(Values[#["corep"]][[All,1]]&/@Values[DP]))];
    
  show1[key_]:=Module[{dp,L1,L2,d1,d2}, dp=DP[key];
    {L1,L2}=dp["label"];   {d1,d2}=ToString/@dp["dim"];
    {L1<>"(k,"<>d1<>")", "\[CircleTimes]", L2<>"("<>skp<>","<>d2<>")"}
  ];
    
  show2[key_]:=Module[{rep,kis,reps,g},
    rep=DP[key,"corep"];      kis=Keys[rep];
    reps=Table[Row[{#1,Style[Row[{"(",ki,",",#2,")"}],Gray]}]&@@@Transpose[rep[ki][[{2,3}]]], {ki,kis}];
    reps=Flatten[reps];
    If[maxNitem<=npr, g={Riffle[reps,"+"]},
      reps=Partition[reps,UpTo[npr]];
      If[Length[reps]==1, g={Riffle[reps[[1]],"+"]},
        g=Riffle[#,"+",{2,-1,2}]&/@reps[[;;-2]];
        AppendTo[g,Riffle[reps[[-1]],"+"]]
      ]
    ];
    Grid[g, ItemSize->Full,Spacings->{0.2, 0.2}]
  ];
    
  table[[2;;1+n1,1;;3]]=show1/@key1;     
  table[[2;;1+n1,4]]=show2/@key1;
  table[[2+n1;;1+n1+n2,1;;3]]=show1/@key2;     
  table[[2+n1;;1+n1+n2,4]]=show2/@key2;
  table[[2+n1+n2;;-1,1;;3]]=show1/@key3;     
  table[[2+n1+n2;;-1,4]]=show2/@key3;
    
  sty1=Directive[Black,Thickness[OptionValue["linewidth"]]];   
  sty2=Directive[Thin,GrayLevel[0.8]];
  bg0={{1,2},{1,-1}}->Lighter[Gray,0.9];
  bg1={{2,1+n1},{1,-1}}->Lighter[Green,0.95];
  bg2={{2+n1,1+n1+n2},{1,-1}}->Lighter[Yellow,0.95];
  bg3={{2+n1+n2,-1},{1,-1}}->Lighter[Blue,0.95];
  tab=Grid[table,Alignment->{Left,Center,{{1,1}->Center,{1,4}->Center}},
             Dividers->{{{None},{1->sty1,4->sty1,-1->sty1}},
                       {{{sty2}},#->sty1&/@{1,2,2+n1,2+n1+n2,-1}}},
             Background->{None,None,{bg0,bg1,bg2,bg3}},
             ItemSize->{{{},{4->Full}},{}}, Spacings->{{}, 0.75}
   ];
  Column[{head,tab}]
]


(* ::Section:: *)
(*Determining the coreps of energy bands*)


(* ::Subsection::Closed:: *)
(*Generate libMLGCorep.mx file containing MLG corep data for all named k*)


(*Note that this function will take a long time to regenerate the libMLGCorep.mx file. 
  e.g. On a 4-core PC, it will take near an hour to do the calculation.
  To make the file work, put it under any of the path in $Path.
  By default, the file will be output to the directory Directory[], and this can be changed
  to a user-defined directory using the "path" option.
*)
Options[generateLibMLGCorep]={"path"->None};
generateLibMLGCorep[OptionsPattern[]]:=Module[{allmsgno,msgno,sgno,brav,knames,kcoords,lib,kstars,fullsymSG,fullsymPG,fsPG,i,k,
  kco,kcos,nukcos,tmp,usub=u->1/10,kBZs,tryabc,cr,j,k2,dictkco,dictk,dictMLG,dictLabel,dictType,dictCorep},
  tryabc=<|"OrthBase(a)"->{a->3,b->2,c->4}, "OrthBase(b)"->{a->2,b->3,c->4},
      "OrthBody(a)"->{a->4,b->2,c->3}, "OrthBody(b)"->{a->3,b->4,c->2},
      "OrthBody(c)"->{a->2,b->3,c->4}, "OrthFace(a)"->{a->3,b->3.5`,c->4},
      "OrthFace(b)"->{a->3,b->4,c->1.8}, "OrthFace(c)"->{a->4,b->1.8,c->3},
      "OrthFace(d)"->{a->1.8,b->3,c->4}, "TetrBody(a)"->{a->3,c->2},
      "TetrBody(b)"->{a->2,c->4}, "TrigPrim(a)"->{a->5,c->2},
      "TrigPrim(b)"->{a->2,c->3}|>;
  allmsgno=MSGSymText//Keys//Sort;
  fullsymSG=<|"Tric"->2,"Mono"->10,"Orth"->47,"Tetr"->123,"Trig"->166,"Hexa"->191,"Cubi"->221|>;
  fullsymPG[sg_]:=getSGElem[fullsymSG[StringTake[getSGLatt[sg],4]]][[All,1]];
  lib=<||>;   dictMLG=dictLabel=dictType=dictCorep=<||>;
  ParallelNeeds["MSGCorep`"];
  SetSharedVariable[lib,tryabc,fullsymSG,usub];
  SetSharedFunction[fullsymPG];
  ParallelDo[
    sgno=msgno[[1]]; brav=getSGLatt[sgno];
    knames=LGIrep[sgno]//Keys;   
    kcoords=kBCcoord[sgno,#][[1,1]]&/@knames;
    kBZs=kBCcoord[sgno,#][[1,2,1]]&/@knames;
    fsPG=fullsymPG[sgno];
    dictk=<||>;
    For[i=1,i<=Length[knames],i++, k=knames[[i]]; kco=kcoords[[i]];
      kcos=Gather[getRotMatOfK[brav,#].kco&/@fsPG,keqmod][[All,1]];
      nukcos=modone[kcos/.usub];
      dictkco=<||>;
      For[j=1,j<=Length[nukcos],j++,
        tmp=tryabc[kBZs[[i]]];  
        cr=getMLGCorep[msgno,nukcos[[j]],"abcOrBasVec"->If[MissingQ[tmp],None,tmp]];
        k2=If[MSGtype[msgno]==3,cr["ksub"][[2]],None];
        tmp={cr["scorep"],cr["dcorep"]};
        dictkco[nukcos[[j]]]=<|"k"->kcos[[j]], "ksub"->k2, "MLG"->cr["MLG"], 
            "label"->{cr["slabel"],cr["dlabel"]}, "type"->{cr["stype"],cr["dtype"]},
            "trace"->Map[If[MatrixQ[#],Tr[#],#]&,tmp,{3}]//Simplify,  "corep"->tmp|>;
      ];
      dictk[k]=dictkco;
    ];
   (*lib[msgno]=dictk;*)
   AppendTo[lib,msgno->dictk];
  ,{msgno,allmsgno}];  
  tmp=Union@Flatten[Values@Values@Values@lib[[All,All,All,"MLG"]],2];
  dictMLG=tmp[[#]]->#&/@Range[Length[tmp]]//Association;
  tmp=Union@Flatten[Values@Values@Values@lib[[All,All,All,"label"]],2];
  dictLabel=tmp[[#]]->#&/@Range[Length[tmp]]//Association;
  tmp=Union@Flatten[Values@Values@Values@lib[[All,All,All,"type"]],2];
  dictType=tmp[[#]]->#&/@Range[Length[tmp]]//Association;
  tmp=Join[Union@Flatten[Values@Values@Values@lib[[All,All,All,"trace"]],2],
           Union@Flatten[Values@Values@Values@lib[[All,All,All,"corep"]],2]]//Union;
  dictCorep=tmp[[#]]->#&/@Range[Length[tmp]]//Association;
  (*SetSharedVariable[dictMLG, dictLabel, dictType, dictCorep];*)
  Do[ 
    knames=Keys@lib[msgno];
    Do[nukcos=Keys@lib[msgno,k];
      Do[tmp=lib[msgno,k,kco];
        tmp["MLG"]=dictMLG[tmp["MLG"]];
        tmp["label"]=dictLabel[tmp["label"]];
        tmp["type"]=dictType[tmp["type"]];
        tmp["trace"]=dictCorep[tmp["trace"]];
        tmp["corep"]=dictCorep[tmp["corep"]];
        lib[msgno,k,kco]=tmp;       
      ,{kco,nukcos}]
    ,{k,knames}];
  ,{msgno,Keys[lib]}];
  lib=KeySort[lib];
  lib["MLG"]=Reverse/@(dictMLG/.Association->List)//Association;
  lib["label"]=Reverse/@(dictLabel/.Association->List)//Association;
  lib["type"]=Reverse/@(dictType/.Association->List)//Association;
  lib["corep"]=Reverse/@(dictCorep/.Association->List)//Association;
  libMLGCorep=lib;
  tmp=If[OptionValue["path"]===None, "", OptionValue["path"]<>$PathnameSeparator];
  DumpSave[tmp<>"libMLGCorep.mx",libMLGCorep];
]


<<"libMLGCorep.mx";
(* lookup MLGCorep in libMLGCorep.mx file *)
lookupMLGCorep[{sgno_,num_},kname_,kco_]:=Module[{nums,ks,kcos,MLG,label,type,corep,re},
  {nums,pos}=checkMSGinput[{sgno,num},"lookupMLGCorep"];
  ks=libMLGCorep[{sgno,num}]//Keys;
  If[!MemberQ[ks,kname], Print["lookupMLGCorep: kname ",kname," should be in ",ks]; Abort[]];
  kcos=libMLGCorep[{sgno,num},kname]//Keys;
  If[!MemberQ[kcos,kco], Print["lookupMLGCorep: kco should be in ",kcos]; Abort[]];
  re=libMLGCorep[{sgno,num},kname,kco];
  MLG=libMLGCorep["MLG"];       label=libMLGCorep["label"];
  type=libMLGCorep["type"];     corep=libMLGCorep["corep"];
  re["MLG"]=MLG[re["MLG"]];     re["label"]=label[re["label"]];
  re["type"]=type[re["type"]];  re["corep"]=corep[re["corep"]];
  re["trace"]=corep[re["trace"]];
  re
]


(* ::Subsection::Closed:: *)
(*readMagTrace*)


readMagTrace[filename_String]:=Module[{dat,nelec,soc,nsym,i,j,rot,trans,srot,nk,kpt,nband,
  trace,deg,ene,knsym,kisym,ik,ib,unitary},
  
  If[!FileExistsQ[filename],Print["readMagTrace: Cannot read file ",filename,"!"]; Abort[]];
  dat=Import[filename,"Table"];  
  {nelec,soc,nsym}=dat[[1;;3,1]];
  
  rot=trans=srot=unitary=Table[0,{nsym}];
  For[j=4,j<=3+nsym,j++,i=j-3;
    rot[[i]]=Partition[dat[[j,1;;9]],3];
    trans[[i]]=dat[[j,10;;12]];
    srot[[i]]=Partition[(#[[1]]+I #[[2]])&/@Partition[dat[[j,13;;20]],2],2];
    unitary[[i]]=dat[[j,21]];
  ];
  
  nk=dat[[4+nsym]]//First;        (* the number of k points *)
  kpt=dat[[5+nsym;;4+nsym+nk]];   (* kpoints *) 
  nband=Module[{},i=7+nsym+nk;    (* the number of bands *)
    While[i<=Length[dat]&&Length[dat[[i]]]>1, i++];
    i--;  dat[[i,1]]+dat[[i,2]]-1 
   ];
  
  (*ene[[ik,ib]] : the energy of the k-point ik and band ib
    deg[[ik,ib]] : the degeneracy of the state
    trace[[ik,ib]] : character of the little group for the states
    knsym[[ik]] : the order of the little co-group of k-point ik
    kisym[[ik]]: the indexes of the operations in the little co-group   *)
  trace=deg=ene=Table[0,{nk},{nband}]; 
  knsym=kisym=Table[0,{nk}];
  j=5+nsym+nk;
  For[ik=1,ik<=nk,ik++,
    knsym[[ik]]=dat[[j++,1]];
    kisym[[ik]]=dat[[j++]];
    For[ib=1,ib<=nband,ib++,
      deg[[ik,ib]]=dat[[j,2]];
      ene[[ik,ib]]=dat[[j,3]];
      trace[[ik,ib]]=(#[[1]]+I #[[2]])&/@Partition[dat[[j,4;;]],2];
      If[ib>=dat[[j,1]]+deg[[ik,ib]]-1,j++];
    ];
  ];

  trace=Chop[trace,1*^-5];
  <|"nelec"->nelec, "soc"->soc, "nsym"->nsym, "rot"->rot, "trans"->trans, "srot"->srot,
    "unitary"->unitary, "nk"->nk, "kpt"->kpt, "nband"->nband, "ene"->ene, "deg"->deg, 
    "knsym"->knsym, "kisym"->kisym, "trace"->trace|>
]


(* ::Subsection::Closed:: *)
(*getBandCorep*)


Options[getBandCorep]={"CompressDegeneracy"->True, "showdim"->True, "abcOrBasVec"->None};
(* Note that the parameter traceData here corresponds to primitive cell of BC setting. *)
getBandCorep[{sgno_Integer, mno_Integer}, traceData_, ikOrListOrSpan_, ibOrListOrSpan_, OptionsPattern[]]/;
 And@@(Position[#,Rule]=={}&&(IntegerQ[#]||ListQ[#]||Head[#]==Span||#==All)&/@{ikOrListOrSpan,ibOrListOrSpan}):=
 Module[{bandCorepOneK,kinfoList,bv,brav,rotName,iks,iik,ibs0,iball,kpathstr,re,dsg,sx,reduceCorep},
  iks=Check[Range[traceData["nk"]][[ikOrListOrSpan]],
        Print["getBandCorep: ik ",ikOrListOrSpan," out of range [1,",traceData["nk"],"]."]; Abort[],
        {Part::partw,Part::pkspec1,Part::take}];
  If[IntegerQ[ikOrListOrSpan], iks={iks}];
  iball=Range[traceData["nband"]];
  ibs0=Check[iball[[ibOrListOrSpan]],
        Print["getBandCorep: ib ",ibOrListOrSpan," out of range [1,",traceData["nband"],"]."]; Abort[],
        {Part::partw,Part::pkspec1,Part::take}];
  If[IntegerQ[ibOrListOrSpan], ibs0={ibs0}]; 
  iik=Association[Rule@@@Transpose[{iks,Range[Length[iks]]}]]; 
  brav=getSGLatt[sgno];
  bv=OptionValue["abcOrBasVec"];
  If[bv=!=None&&Position[bv,Rule]!={}, bv=BasicVectors[brav]/.bv];
  kinfoList=identifyBCHSKptBySG[sgno,If[bv=!=None,bv,"a"],traceData["kpt"][[iks]]];
  dsg=If[traceData["soc"]==1, True, False];
  rotName=If[!dsg, 
    getRotName[brav,#]&/@traceData["rot"],
    (*-------else: dsg--------*)
    (*Note that the bases of spin rotation matrices of BC are {down,up}, different from the
      usual {up,down} people used. So, a transformation by sx is needed. *)
    sx={{0,1},{1,0}};
    getSpinRotName[brav,{sx.#1.sx,Det[#2]}]&@@@Transpose[{traceData["srot"],traceData["rot"]}]
  ];
          
  kpathstr=StringRiffle[If[#1[[2]]!=""||#1[[1]]=="UN"||#1[[1]]=="GP", #1[[1]],
                          "["<>#1[[1]]<>"("<>ToString[#2]<>")]"]&
             @@@Transpose[{kinfoList[[All,{2,3}]],iks}],"-"];

  reduceCorep[chartab_,Itypes_,chars_,ne_]:=chars\[Conjugate].#1/(#2*ne)&@@@Transpose[{chartab,Itypes}];
    
  (* Format of kinfo is like:
  {{-0.2,0.6,0.1},"GP","","C1"}
  {{-0.2,0.6,0.1},"UN","",{"E","\[Sigma]z"}}
  {{0,0.1,0.5},"Z","XM","C2v",{u,0.5,0},{"C31+",{0,0,0}},{0,u,0.5},{0,0,0},u\[Rule]0.1`,0.5,"in G"} *)
  bandCorepOneK[ik_]:=Module[{kinfo,k,ktip,kname,kname2,MLG,MLG0,Mk,Mk0,N0,kisym,times,inv,chars,
    idx,factor,dt,tmp,tmp1,deg,ibs2,rep,i,abc,iball2,deg2,ibidx,ibs,facbar,rots,chartab,label,type,keyk,
    reducoef,dim,ib,iR,as},
    kinfo=kinfoList[[iik[ik]]];
    {k,kname,ktip}=kinfo[[1;;3]];  
    kisym=traceData["kisym"][[ik]];
    Mk={rotName[[kisym]],traceData["trans"][[kisym]],(1-traceData["unitary"][[kisym]])/2}\[Transpose];
    Mk0={StringReplace[#1,"bar"->""],#2,#3}&@@@Mk;
    deg=traceData["deg"][[ik]];
    ibs=ibs0;
    If[OptionValue["CompressDegeneracy"],
      {ibs2,deg2}=Transpose@
           ((Reap@For[i=1,i<=Last[iball],i++,Sow[{i,deg[[i]]}];i+=deg[[i]]-1;])[[2,1]]);
      iball2=Range[#1,#1+#2-1]&@@@Transpose[{ibs2,deg2}];   
      ibidx=DeleteDuplicates[Position[iball2,#][[1,1]]&/@ibs0]; 
      ibs=ibs2[[ibidx]];
    ];
    
    If[kname=="GP"||kname=="UN",
      MLG=getMLGElem[{sgno,mno},k];
      tmp=getMLGCorep[{sgno,mno},k,"trace"->True];
      chartab=tmp[If[dsg,"dcorep","scorep"]];
      label=tmp[If[dsg,"dlabel","slabel"]];
      type=tmp[If[dsg,"dtype","stype"]],
      (*---------else---------*)
      kname2=If[brav=="TrigPrim"&&kname=="F", "aF", kname]; (*the result of bF is the same with aF*)
      keyk=modone[kinfo[[7]]/.u->1/10];
      tmp=lookupMLGCorep[{sgno,mno},kname2,keyk];
      MLG=tmp["MLG"];     
      chartab=tmp["trace"][[If[dsg,2,1]]];
      If[Position[kinfo,Rule]!={}, chartab=chartab/.kinfo[[9]]];  (*if has u, substitute it*)
      label=tmp["label"][[If[dsg,2,1]]];
      type=tmp["type"][[If[dsg,2,1]]];
    ];
    
    MLG0=Select[MLG,#[[3]]==0&];    N0=Length[MLG0];
    idx=Position[Mk0[[All,{1,3}]],#[[{1,3}]]][[1,1]]&/@MLG0;
    Mk0=Mk0[[idx]];
    If[StringReplace[#,"bar"->""]&/@Mk0[[All,1]]!=MLG0[[All,1]] ||
       Total@modone@Flatten[Mk0[[All,2]]-MLG0[[All,2]]]>1*^-6,
      Print["getBandCorep: ik=",ik," (",k,"), Mk0=",showMSGSeitz/@Rationalize[Mk0,0.01]," not compatible with MLG0=",
        showMSGSeitz/@MLG0,". ","Check whether the MSG number is right or whether the unit cell conforms "
        "to the BC convention."];
      Abort[];
    ]; 
    facbar=If[dsg, If[StringPosition[#[[1]],"bar"]!={},-1,1]&/@Mk0, Table[1,N0]];
    factor=If[dt=MLG0[[#,2]]-Mk0[[#,2]];dt!={0,0,0},Exp[-I*k.dt*2Pi],1]&/@Range[N0]//Chop;
    factor=factor*facbar;    
                
    chars=traceData["trace"][[ik]];
    tmp=If[Position[kinfo,Rule]!={}, kinfo[[9]], {}];
    chars=(chars[[#,idx]]*factor/.tmp)&/@ibs;
    reducoef=reduceCorep[chartab[[All,;;N0]],type/.{"a"->1,"b"->4,"c"->2,"x"->1},#,N0]&/@chars;       
    reducoef=reducoef//Chop//Rationalize[#,0.1]&;  
        
    dim=chartab[[All,1]]//Round;
    rep=Table[0,Length[reducoef]];
    For[i=1,i<=Length[ibs],i++, ib=ibs[[i]];
      iR=Select[Range[Length[type]],reducoef[[i,#]]!=0&]; 
      tmp1=If[OptionValue["CompressDegeneracy"],iball2[[ibidx[[i]],{1,-1}]],ib]; 
      as=If[#==1,"",ToString[#,InputForm]]&/@reducoef[[i,iR]];
      If[OptionValue["showdim"],
        tmp=StringRiffle[#1<>#2<>"("<>ToString[#3]<>")"&@@@Transpose[{as,label[[iR]],dim[[iR]]}],"\[CirclePlus]"],
        tmp=StringRiffle[#1<>#2&@@@Transpose[{as,label[[iR]]}],"\[CirclePlus]"]
      ];
      If[VectorQ[reducoef[[i]],IntegerQ], (* The multiplicities of reps should be integers. *)
        rep[[i]]={tmp1,traceData["ene"][[ik,ib]],deg[[ib]],tmp},
        (* rep[[i]]={tmp1,traceData["ene"][[ik,ib]],deg[[ib]],"??"<>#<>"??"&/@tmp}; (*for debug*);
        Print["Warning: getBandCorep: ik=",ik,", ib=",ib," multiplicities of reps are not integers ",
               repR[[i,2]]];  (*for debug*)   *)
        rep[[i]]={tmp1,traceData["ene"][[ik,ib]],deg[[ib]],"??"};
      ]
    ];    
    
    If[IntegerQ[ibOrListOrSpan],rep[[1]],rep]
  ];
  
  re=<||>;  
  re["kpath"]=kpathstr;
  re["corep"]=If[IntegerQ[ikOrListOrSpan], bandCorepOneK[iks[[1]]], bandCorepOneK/@iks];
  re["kinfo"]=kinfoList;  
  re
]

getBandCorep[{sgno_Integer, mno_Integer}, traceData_, ikOrListOrSpan_, OptionsPattern[]]/;
 And@@(Position[#,Rule]=={}&&(IntegerQ[#]||ListQ[#]||Head[#]==Span||#==All)&@ikOrListOrSpan):=
  getBandCorep[{sgno,mno},traceData,ikOrListOrSpan,All,#->OptionValue[#]&/@{"CompressDegeneracy","showdim","abcOrBasVec"}]
getBandCorep[{sgno_Integer, mno_Integer}, traceData_, OptionsPattern[]]:=
  getBandCorep[{sgno,mno},traceData,All,All,#->OptionValue[#]&/@{"CompressDegeneracy","showdim","abcOrBasVec"}]


(*corep should be the returned value of getBandCorep[{n,m}, tr] without specifying ik and ib. 
  Although result will be given when corep=getBandCorep[{n,m}, tr, ik], a warning will be issued. *)
SetAttributes[showBandCorep,HoldFirst];
Options[showBandCorep]={"bottomUp"->True};
showBandCorep[corep_, ik_Integer, OptionsPattern[]]:=showBandCorep[corep,ik,All,"bottomUp"->OptionValue["bottomUp"]]
showBandCorep[corep_, ik_Integer, ibOrListOrSpan_, OptionsPattern[]]:=Module[{cr,crik,ibmax,iball,ibs,ibs0,ibs1,crout,tab},
  cr=corep["corep"];
  If[IntegerQ[cr[[1]]]||VectorQ[cr[[1]],IntegerQ],
    Print["showBandCorep: the corep should be the returned value of getBandCorep without specifying band index."]; Abort[]
  ];
  If[Length[cr[[1]]]==4 && (IntegerQ[cr[[1,1]]]||VectorQ[cr[[1,1]],IntegerQ]), 
    Print["Warning: there is only one k-point in the data of "<>StringTake[ToString@Hold[corep],{6,-2}]<>"."];
    crik=cr,  (* for the case in which ik has been specified in getBandCorep, i.e. corep=getBandCorep[{n,m}, tr, ik]*)
    (*---- else: for the case corep=getBandCorep[{n,m}, tr] ------*)
    Check[crik=cr[[ik]], Print["showBandCorep: ik out of range [1,",Length[cr],"]!"]; Abort[],
      {Part::partw,Part::take}];
  ];
  ibs0=crik[[All,1]];  ibmax=Max@Flatten@ibs0;  iball=Range[ibmax];
  ibs=Check[iball[[ibOrListOrSpan]],
        Print["showBandCorep: ib ",ibOrListOrSpan," out of range [1,",ibmax,"]."]; Abort[],
        {Part::partw,Part::pkspec1,Part::take}];
  If[IntegerQ[ibOrListOrSpan], ibs={ibs}]; 
  If[ibOrListOrSpan===0, ibs=iball];
  ibs1=(Position[Range@@@ibs0,#][[1,1]]&/@ibs)//Union;
  crout=If[OptionValue["bottomUp"]===True, crik[[ibs1]]//Reverse, crik[[ibs1]]];
  tab=MapAt[If[IntegerQ[#], #, If[#[[1]]==#[[2]],#[[1]],Row[#,"-"]]]&, crout, {All,1}];
  tab=Prepend[tab,{"bands","energy","degeneracy","small corep"}];
  Grid[tab, Alignment->{{Left,Left,Center,Left}}, Spacings -> {{Default, 3, 1, 1}}, 
            Dividers->{{},{2->Directive[Thickness[0.4],Gray]}}, Frame->Thickness[0.4]]
]


(* ::Section:: *)
(*End*)


End[] 
EndPackage[]
