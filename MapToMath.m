(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: MapToMath														*)

(*
	Copyright (C) 2020-2021 Vladyslav Shtabovenko
	Copyright (C) 1998-2020 Wolfram Research, Inc
*)

(* :Summary:	Converter from Maple to Mathematica. At the current stage
				much of the code is copied from MapleConverter 0.95
				http://library.wolfram.com/infocenter/Demos/188/			*)

(* :License:	MapToMath is distributed under the MIT license				*)

(* ------------------------------------------------------------------------ *)

BeginPackage["MapToMath`"];

MTMConvert::usage=
"MTMConvert[string] converts the Maple input string to its Mathematica equivalent."

StandardDictionary::usage=
"StandardDictionary is a list of the Maple functions whose equivalent is built in function in Mathematica."

$MTMVerbose::usage =
"$MTMVerbosee is a global variable with default setting 0. \
If set to 1, 2, ..., less and more intermediate comments and informations \
are displayed during calculations.";

MTMVerbose::usage =
"MTMVerbosee is an option with default setting 0. \
If set to 1, 2, ..., less and more intermediate comments and informations \
are displayed during calculations.";

MapToMath::failmsg = "Error! MapToMath has encountered a fatal problem and must abort the evaluation. \
The problem reads: `1`";

Begin["`Private`"]

SetAttributes[MTMPrint, HoldRest];

Options[MTMPrint] = {
		MTMDoControl :> $MTMVerbose,
		WriteString -> False,
		MTMWriteStringOutput ->"stdout"
}

MTMPrint[level_, mtmprintx__ /;!OptionQ[{mtmprintx}] , OptionsPattern[]] :=
	Block[{flowcontrol=OptionValue[MTMDoControl]},
		If[ flowcontrol >= level,
			If[ OptionValue[WriteString],
				WriteString[OptionValue[MTMWriteStringOutput],mtmprintx],
				Print[mtmprintx]
			]
		]
	];

StandardDictionary={
	"sin","cos","tan","diff","evalf","evalhf", "int","abs",
	"argument","binomial","ceil","conjugate","erf","erfc","exp","factorial",
	"floor","ln","max","min","round","signum","sqrt","trunc","cot","sec",
	"csc","sinh","cosh","tanh","sech","csch","coth","arcsin","arccos",
	"arctan","arccot","arcsec","arccsc","arcsinh","arccosh","arctanh",
	"arccoth","arccsch","arcsech","expand","factor","factors","coeffs","gcd",
	"lcm","rem","quo","plot","plot3d","\""->"%","`"->"\"","resultant",
	"collect", "sqrfree","bernoulli","cyclotomic","euler","fibonacci","numer",

	"denom","product","sum","dsolve","Ci","Chi","Li","Si","limit","series",
	"solve","root","eliminate","EigenVals","isprime","ithprime","factorset",
	"divisors","iquo","irem","igcd","ilcm","Gamma","GAMMA","Ai","Bi","Psi",
	"map","seq","simplify","subs","fsolve","numpart","stirling1","stirling2",
	"BesselI","BesselJ","BesselK","BesselY","Beta","coeff","compoly",
	"fortran","latex","RETURN","taylor","frac","whattype","Zeta","substring",
	"member","log","FresnelC","FresnelS","ihermite","lprint"
};



ZeroRule = {
	"delta[" ~~ x : WordCharacter .. ~~ "]" :>
		"delta(" <> ToString[x] <> ")",

	"zeta[" ~~ x : DigitCharacter .. ~~ "]" :>
		"zeta(" <> ToString[x] <> ")",
	"zeta[" ~~ x1 : DigitCharacter .. ~~ "," ~~ x2 : DigitCharacter .. ~~ "]" :>
		"mzv(" <> ToString[x1] <> "," <> ToString[x2] <> ")",

	"zeta[" ~~ x1 : DigitCharacter .. ~~ "," ~~ x2 : DigitCharacter .. ~~ "," ~~ x3 : DigitCharacter .. ~~ "]" :>
		"mzv(" <> ToString[x1] <> "," <> ToString[x2] <> "," <> ToString[x3] <> ")",

	"zeta[" ~~ x1 : DigitCharacter .. ~~ "," ~~ x2 : DigitCharacter .. ~~ "," ~~ x3 : DigitCharacter .. ~~ "," ~~ x4 : DigitCharacter .. ~~ "]" :>
		"mzv(" <> ToString[x1] <> "," <> ToString[x2] <> "," <> ToString[x3] <> "," <> ToString[x4] <> ")",

	"zeta[" ~~ x1 : DigitCharacter .. ~~ "," ~~ x2 : DigitCharacter .. ~~ "," ~~ x3 : DigitCharacter .. ~~ "," ~~
	x4 : DigitCharacter .. ~~ "," ~~ x5 : DigitCharacter .. ~~ "]" :>
		"mzv(" <> ToString[x1] <> "," <> ToString[x2] <> "," <> ToString[x3] <> "," <> ToString[x4] <> "," <> ToString[x5] <> ")",

	"zeta[" ~~ x1 : DigitCharacter .. ~~ "," ~~ x2 : DigitCharacter .. ~~ "," ~~ x3 : DigitCharacter .. ~~ "," ~~
	x4 : DigitCharacter .. ~~ "," ~~ x5 : DigitCharacter .. ~~ "," ~~ x6 : DigitCharacter .. ~~ "]" :>
		"mzv(" <> ToString[x1] <> "," <> ToString[x2] <> "," <> ToString[x3] <> "," <> ToString[x4] <> "," <> ToString[x5] <> "," <> ToString[x6] <> ")"
};

FirstRule ={"[["->"[[","]]"->"]]","["->"{","]"->"}"};

SecondRule = {
	"delta("->"DiracDelta[",
	"sin("->"Sin[",
	"cos("->"Cos[",
	"tan("->"Tan[",
	"diff("->"D[",
	"evalf("->"N[",
	"evalhf("->"N[",
	"int("->"Integrate[",
	"abs("->"Abs[",
	"argument("->"Arg[",
	"binomial("->"Binomial[",
	"ceil("->"Ceiling[",
	"conjugate("->"Conjugate[",
	"erf("->"Erf[",
	"erfc("->"Erfc[",
	"exp("->"Exp[",
	"factorial("->"Factorial[",
	"floor("->"Floor[","ln("->"Log[","max("->"Max[","min("->"Min[",
	"round("->"Round[","signum("->"Sign[","sqrt("->"Sqrt[",
	"trunc("->"IntegerPart[","cot("->"Cot[","sec("->"Sec[","csc("->"Csc[",
	"sinh("->"Sinh[","cosh("->"Cosh[","tanh("->"Tanh[","sech("->"Sech[",
	"csch("->"Csch[","coth("->"Coth[","arcsin("->"ArcSin[",
	"arccos("->"ArcCos[","arctan("->"ArcTan[","arccot("->"ArcCot[",
	"arcsec("->"ArcSec[","arccsc("->"ArcCsc[","arcsinh("->"ArcSinh[",
	"arccosh("->"ArcCosh[","arctanh("->"ArcTanh[","arccoth("->"ArcCoth[",
	"arccsch("->"ArcCsch[","arcsech("->"ArcSech[","expand("->"Expand[",
	"factor("->"Factor[","factors("->"FactorList[",
	"coeffs("->"CoefficientList[","gcd("->"PolynomialGCD[",
	"lcm("->"PolynomialLCM[","rem("->"PolynomialRemainder[",
	"quo("->"PolynomialQuotient[","plot("->"Plot[","plot3d("->"Plot3DHOL[",
	"\""->"%","`"->"\"","resultant("->"Resultant[","collect("->"Collect[",
	"sqrfree("->"FactorSquareFree[","bernoulli("->"BernoulliB[",
	"cyclotomic("->"Cyclotomic[","euler("->"EulerE[",
	"fibonacci("->"Fibonacci[","numer("->"Numerator[",
	"denom("->"Denominator[","product("->"Product[","sum("->"Sum[",
	"dsolve("->"DSolve[","Ci("->"CosIntegral[","Chi("->"CoshIntegral[",
	"Li("->"LogIntegral[","Si("->"SinIntegral[","limit("->"Limit[",
	"series("->"Series[","solve("->"Solve[","root("->"Root[",
	"eliminate("->"Eliminate[","EigenVals("->"Eignevalues[",
	"isprime("->"PrimeQ[","ithprime("->"Prime[",
	"factorset("->"FactorInteger[","divisors("->"Divisors[",
	"iquo("->"Quotient[","irem("->"Mod[","igcd("->"GCD[","ilcm("->"LCM[",
	"Gamma("->"EulerGamma[","GAMMA("->"Gamma[","AiryAi("->"AiryAi[",
	"AiryBi("->"AiryBi[","Psi("->"PolyGamma[","map("->"Map[","seq("->"Table[",

	"simplify("->"Simplify[","subs("->"RepeatedReplace[","fsolve("->"NSolve[",

	"numpart("->"Partitions[","stirling1("->"StirlingS1[",
	"stirling2("->"StirlingS2[","BesselI("->"BesselI[","BesselJ("->"BesselJ[",

	"BesselK("->"BesselK[","BesselY("->"BesselY[","Beta("->"Beta[",
	"coeff("->"Coefficient[","compoly("->"Decompose[",
	"fortran("->"FortranForm[","latex("->"TexForm[","RETURN("->"Return[",
	"taylor("->"Series[","frac("->"FractionalPart[","whattype("->"Head[",
	"Zeta("->"Zeta[","substring("->"StringTake[","member("->"MemberQ[",
	"laplace("->"LaplaceTransform[","log("->"Log[","log["->"Log[",
	"FresnelC("->"FresnelC[","FresnelS("->"FresnelS[",
	"ihermite("->"HermiteNormalForm[","lprint("->"InputForm[",

	"zeta"->"MapToMath`Private`zeta",
	"mzv"->"MapToMath`Private`mzv"
};


LimitRules = {
	"="->"->",
	"right"->"Direction->-1",
	"left"->"Direction->1"
};

ThirdRule = {
	"="->"==",
	":="->"=",
	"HOL["->"["
};

FinalRule = {
	"HLog"->"Hlog"

};

StatisticsFunctions={
	"mean("->"Mean[",
	"coefficientofvariation("->"CoefficientOfVariation[",
	"covariance("->"CovarianceMLE[","geometricmean("->"GeometricMean[",
	"harmonicmean("->"HarmonicMean[","kurtosis("->"Kurtosis[",
	"linearcorrelation("->"Correlation[","meandeviation("->"MeanDeviation[",
	"median("->"Median[","mode("->"Mode[","quadraticmean("->"RootMeanSquare[",

	"range("->"SampleRange[","skewness("->"Skewness[",
	"standarddeviation("->"StandardDeviationMLE[",
	"variance("->"VarianceMLE["
};

StatisticalDistributions={
	"binomiald("->"BinomialDistribution[",
	"hypergeometric("->"HypergeometricDistribution[",
	"negativebinomial("->"NegativeBinomialDistribution[",
	"poisson("->"PoissonDistribution[","chisquare("->"ChiSquareDistribution[",

	"beta("->"BetaDistribution[","cauchy("->"CauchyDistribution[",
	"fratio("->"FRatioDistribution[","gamma("->"GammaDistribution[",
	"laplaced("->"LaplaceDistribution[","logistic("->"LogisticDistribution[",
	"lognormal("->"LogNormalDistribution[","normald("->"NormalDistribution[",
	"studentst("->"StudentTDistribution[","uniform("->"UniformDistribution[",
	"weibull("->"WeibullDistribution["
};

BadFunctionalArguments={"kurtosis[","coefficientofvariation[","skewness["};

travdata={{0,"[",1},{0,"]",-1},{1,",",0}};
ubdata={{0,"[",1},{1,"]",-1},{1,",",0}};
fbdata={{0,"[",1},{0,"]",-1},{0,"(",1},{1,")",-1}};
gidata={{0,"[",1},{1,"]",-1}};

frmVerbose::usage="";

Options[MTMConvert] = {
	ToExpression->True,
	MTMVerbose -> False,
	Zeta -> True
}

MTMConvert[expression_String, OptionsPattern[]]:=
	Block[{newstring,i,res},

		If [OptionValue[MTMVerbose]===False,
			frmVerbose=$MTMVerbose,
			If[MatchQ[OptionValue[MTMVerbose], _Integer],
				frmVerbose=OptionValue[MTMVerbose]
			];
		];

		MTMPrint[1,"MapToMath: Entering. ", MTMDoControl->frmVerbose];
		MTMPrint[3,"MapToMath: Entering with: ", expression , MTMDoControl->frmVerbose];

		newstring =	expression;

		newstring = StringReplace[newstring,ZeroRule];

		MTMPrint[3,"MapToMath: After ZeroRule: ", newstring , MTMDoControl->frmVerbose];

		newstring = FixBadArguments[newstring];

		MTMPrint[3,"MapToMath: After FixBadArguments: ", newstring , MTMDoControl->frmVerbose];

		newstring = FixExtract[newstring];

		MTMPrint[3,"MapToMath: After FixExtract: ", newstring , MTMDoControl->frmVerbose];

		newstring = StringReplace[newstring,FirstRule];

		MTMPrint[3,"MapToMath: After FirstRule: ", newstring , MTMDoControl->frmVerbose];


		newstring = StringReplace[newstring,SecondRule];

		MTMPrint[3,"MapToMath: After SecondRule: ", newstring , MTMDoControl->frmVerbose];


		(*newstring = StringReplace[newstring,StatisticsFunctions];
		newstring = StringReplace[newstring,StatisticalDistributions];*)

		While[	StringMatchQ[newstring,"*Log[[*"],
				newstring=FixLog[newstring]
		];

		newstring = FixLeftBrackets[newstring];

		MTMPrint[3,"MapToMath: After FixLeftBrackets: ", newstring , MTMDoControl->frmVerbose];

		newstring = FixRightBrackets[newstring];

		MTMPrint[3,"MapToMath: After FixRightBrackets: ", newstring , MTMDoControl->frmVerbose];

		newstring = FixUserDefinedFunctions[newstring];

		MTMPrint[3,"MapToMath: After FixUserDefinedFunctions: ", newstring , MTMDoControl->frmVerbose];

		(*newstring =StringReplace[newstring,ColorRule];*)

		For[	i=1, i<=Length[StringPosition[newstring,"StringTake"]], i++,
				newstring=FixDots[newstring,i]
		];

		(*If[	StringMatchQ[newstring,"*Plot*"],
			newstring=FixParametricPlot[newstring,1]
		];*)

		While[	StringMatchQ[newstring,"*..*"],
				newstring = FixRange[newstring]
		];

		While[	StringMatchQ[newstring,"*RepeatedReplace*"],
				newstring = FixSubstitution[newstring]
		];

		For[	i=1, i<=Length[StringPosition[newstring,"MemberQ"]],i++,
				newstring=FixMember[newstring,i]
		];

		newstring=FixDerivative[newstring];
		(*newstring=FixDSolve[newstring];*)
		(*newstring=FixHypergeometricDistribution[newstring];
		newstring = StringReplace[newstring,PlotOptionsRule];*)
		newstring=FixLimit[newstring];
		newstring = StringReplace[newstring,ThirdRule];

		If[	StringMatchQ[newstring,"*proc[*"],
			newstring=FixProc[newstring]
		];

		newstring = StringReplace[newstring,FinalRule];

		If[	OptionValue[ToExpression],
			res = ToExpression[newstring],
			res = newstring
		];

		If[	OptionValue[Zeta] && Head[res]=!=String,
			(*	HPL uses the definition of MZVs from math/9910045 (Eq 1.4, n_1 > n_2 > ... > n_k > 0) , while HyperInt sticks to   (n_k > ... > n_2 > n_1 > 0)
			Here we convert to the HPL convention. *)
			res = res /. {MapToMath`Private`mzv[j__Integer] :> HPL`MZV[Reverse[{j}]], MapToMath`Private`zeta[j_] :> Zeta[j]};
		];

		MTMPrint[1,"MapToMath: Leaving. ", MTMDoControl->frmVerbose];
		MTMPrint[3,"MapToMath: Leaving with: ", res , MTMDoControl->frmVerbose];

		res
	];

Traversal[data_,match_,terminate_,openclose_]:=
	Block[{clist,locs,i,j,k,counter,ending,locslist,len},

		locs	= StringPosition[data,match];
		len		= StringLength[data];
		clist	= Characters[data];
		locslist={};

		For[	k=1,k<=Length[locs],k++,
				i=locs[[k,1]];
				counter=0;
				ending=0;
				While[	ending==0,
						While[	LetterQ[clist[[i]]] || DigitQ[clist[[i]]],
								If[i==len,Break[],i++]
						];

						If[	i==len,
							ending=i,
							j=1;
							While[	openclose[[j,2]]!=clist[[i]],
									If[j==Length[openclose],
									j++;
									Break[],j++]
							];
						If[	j==Length[openclose]+1,counter+=0,
							Switch[
									openclose[[j,1]],
									0,
										counter+=openclose[[j,3]],
									_,
									If[	counter==terminate,
										ending=i,
										counter+=openclose[[j,3]]]
							]
						]
						];
					i++;
				];

			AppendTo[locslist,ending]
		];
		locslist
	];


Rebuild[argslist_]:=
	Block[{newstring,i},

		newstring=argslist[[1]]<>"[";
		For[	i=2,i<Length[argslist],i++,
				newstring=newstring<>argslist[[i]]<>","
		];
		newstring=newstring<>Last[argslist]<>"]";

		newstring
	];


SwapArgs[data_,pos1_,pos2_]:=
	Block[{newargs},

		newargs=ReplacePart[data,data[[pos2]],pos1];
		newargs=ReplacePart[newargs,data[[pos1]],pos2];
		newargs
	];


GetLocalVariables[data_]:=
	Block[{varlist,templist,tempstring,locs,tlocs,i,j},
		locs=StringPosition[data,"local"];
		varlist={};
		For[	i=1, i<=Length[locs], i++,
				tempstring	= StringDrop[data,locs[[i,2]]+1];
				tlocs		= StringPosition[tempstring,";"][[1,1]];
				tempstring	= StringTake[tempstring,tlocs-1];
				templist=IsolateArguments["flag["<>tempstring<>"]"];
				For[	j=2,j<=Length[templist],j++,
						AppendTo[varlist,{templist[[j]],locs[[i,1]],tlocs}]];
				];

		varlist
	];


BuildParameterString[paramlist_]:=
	Block[{i,paramstring},
		paramstring="";
		For[	i=1,i<Length[paramlist],i++,
				paramstring=paramstring<>paramlist[[i]]<>"_,"
		];
		If[	Length[paramlist]!=0,
			paramstring=paramstring<>Last[paramlist]<>"_"
		];

		paramstring
	];


BuildVariableString[varlist_]:=
	Block[{varstring,i},

		varstring="{";
		For[	i=1,i<Length[varlist],i++,
				varstring = varstring<>varlist[[i,1]]<>","
		];
		If[	Length[varlist]!=0,
			varstring = varstring<>Last[varlist][[1]]
		];
		varstring = varstring<>"}";

		varstring
	];

BuildBodyString[data_,varlist_]:=
	Block[{i,body,loc,droplist},

		loc = First[Traversal[data,"proc",1,{{0,"[",1},{1,"]",-1}}]];
		droplist={};
		For[	i=1,i<=Length[varlist],i++,
				AppendTo[droplist,{varlist[[i,2]],varlist[[i,2]]+varlist[[i,3]]+6}]
		];
		body	= StringReplacePart[data,"",Union[droplist]];
		body	= StringDrop[body,loc];
		body	= StringDrop[body,-4];

		i=1;
		While[StringTake[body,{-i}]===" " || StringTake[body,{-i}]==="\n",i++];

		If[	StringTake[body,{-i}]===";",
			body=StringDrop[body,{-i}]
		];

		body
	];


GetParameters[data_]:=
	Block[{loc, i, tempstring, paramlist, templist},

		tempstring=StringTake[data,IsolateExpression[data,"proc",1]];
		templist=IsolateArguments[tempstring];
		paramlist={};

		For[	i=2, i<=Length[templist], i++,
				tempstring = templist[[i]];
				loc = StringPosition[tempstring,"::"];
				If[	Length[loc]!=0,
					AppendTo[paramlist,StringTake[tempstring,loc[[1,1]]-1]],
					AppendTo[paramlist,tempstring]
				];
		];

		paramlist
	];

IsolateExpression[data_,match_,num_]:=
	Block[{start,end,i,clist,counter},
		start=StringPosition[data,match][[num,1]];
		clist=Characters[data];
		i=start;
		end=0;
		counter=0;
		While[	end==0,
				While[	LetterQ[clist[[i]]] || DigitQ[clist[[i]]],
						i++
				];
				Switch[
					clist[[i]],
					"[",
						counter++,
					"]",
						If[	counter==1,
							end=i,
							counter--]
						];
				i++;
		];
		{start,end}
	];


IsolateArguments[data_]:=
	Block[{start,end,i,clist,counter,arglist,tempstring,j,len},
		start = First[StringPosition[data,"["]][[1]]+1;
		len = StringLength[data];
		i = start;
		clist = Characters[data];
		arglist = {StringTake[data,start-2]};
		While[	i<len,
				counter=0;
				end=0;
				j=i;
				While[	end==0,
						While[	LetterQ[clist[[i]]] || DigitQ[clist[[i]]] && i<len,
								i++
						];
					Switch[
							clist[[i]],
							"[",
								counter++,
							"]",
								If[	i == StringLength[data],
									end=i,
									counter--
								],
							"{",
								counter++,
							"}",
								counter--,
							",",
							If[	counter==0,
								end=i
							]
					];
					i++;
				];
				tempstring = StringTake[data,{j,end-1}];
				AppendTo[arglist,tempstring];
		];
		arglist
	];


FixDerivative[data_]:=
	Block[{i,locs,newstring,returnstring,endpoints},
		returnstring = data;
		locs = StringPosition[returnstring,"D["];
		For[ 	i=1, i<=Length[locs], i++,
				endpoints = IsolateExpression[returnstring,"D[",i];
				newstring = StringTake[returnstring,endpoints];
				newstring = FD[newstring];
				returnstring = StringReplacePart[returnstring,newstring,endpoints]
		];
		returnstring
	];

(*
FixDSolve[data_]:=
	Block[{locs,i,endpoints,returnstring,newstring,arglist},
		returnstring = data;
		locs = StringPosition[returnstring,"DSolve"];
		For[	i=1,i<=Length[locs],i++,
				endpoints = IsolateExpression[returnstring,"DSolve",i];
				newstring = StringTake[returnstring,endpoints];
				arglist = IsolateArguments[newstring];
				newstring= Rebuild[{arglist[[1]],arglist[[2]],arglist[[3]],"Insert I.V Here"}];
				returnstring = StringReplacePart[returnstring,newstring,endpoints]
		];
		returnstring
	];*)

FixLog[data_]:=
	Block[	{locs,tempstring},
		tempstring=data;
		locs = IsolateExpression[tempstring,"Log[[",1];
		tempstring = StringReplacePart[tempstring,",",{Last[locs]-1,Last[locs]+1}];
		tempstring = StringDrop[tempstring,{First[locs]+3}];
		tempstring
	];


FixBadArguments[data_]:=
Block[{endpoints,i,j,tempstring1,tempstring2,match,locs},
		tempstring1=data;
		For[	i=1,i<=Length[BadFunctionalArguments],i++,
				match = BadFunctionalArguments[[i]];
				locs = StringPosition[tempstring1,match];

				For[	j=1,j<=Length[locs],j++,
						endpoints=IsolateExpression[tempstring1,match,j];
						tempstring2=StringTake[tempstring1,endpoints];
						tempstring2= StringTake[tempstring2,StringPosition[tempstring2,"["][[1,1]]-1];
						tempstring1=StringReplacePart[tempstring1,tempstring2,endpoints];
				];
		];
		Return[tempstring1]
	];


FD[data_]:=
	Block[{i,newstring,arglist,loc,tempstring,leftparts,rightparts,newlist},

		leftparts={};
		rightparts={};
		tempstring="";
		arglist=IsolateArguments[data];
		For[	i=3,i<=Length[arglist],i++,
				newstring=StringReplace[arglist[[i]],{"{"->"","}"->""}];
				arglist=ReplacePart[arglist,newstring,i];
		];
		newstring=Rebuild[arglist];
		arglist=IsolateArguments[newstring];
		For[	i=3,i<=Length[arglist],i++,
				loc=StringPosition[arglist[[i]],"$"];
				If[	Length[loc]==0,
					AppendTo[leftparts,arglist[[i]]];
					AppendTo[rightparts,"1"],
					AppendTo[leftparts,StringTake[arglist[[i]],{1,loc[[1,1]]-1}]];
					AppendTo[rightparts, StringTake[arglist[[i]],{loc[[1,1]]+1,StringLength[arglist[[i]]]}]]
				];
		];
		For[	i=1,i<=Length[leftparts],i++,
				If[	rightparts[[i]]=="1",
					tempstring=tempstring<>leftparts[[i]]<>",",
					tempstring= tempstring<>"{"<>leftparts[[i]]<>","<>rightparts[[i]]<>"}"<>","
				]
		];
		tempstring=StringDrop[tempstring,-1];
		newlist={arglist[[1]],arglist[[2]],tempstring};
		newstring=Rebuild[newlist];
		newstring
	];

FS[data_]:=
	Block[{newstring,arglist,k,tempstring},

		tempstring	= StringTake[data,IsolateExpression[data,"RepeatedReplace",1]];
		arglist		= IsolateArguments[tempstring];
		tempstring="{";
		For[	k=2,k<Length[arglist],k++,
				tempstring=tempstring<>arglist[[k]]<>","
		];
		tempstring	= StringDrop[tempstring,{StringLength[tempstring]}];
		tempstring	= tempstring<>"}";
		tempstring	= StringReplace[tempstring,"="->"->"];
		newstring	= "ReplaceRepeated"<>"["<>Last[arglist]<>","<>tempstring<>"]";

		newstring
	];

FindLowerBound[data_]:=
	Block[	{i,clist,pos},

		pos=StringPosition[data,".."][[1,1]];
		i=pos;
		clist=Characters[data];
		While[	clist[[i]]!="=",
				i--
		];

		i
	];


FixExtract[data_]:=
	Block[{blocs,i,flag,replist,endlist,newstring,replist2},
		blocs = StringPosition[data,"["];
		replist={};
		For[	i=1, i<=Length[blocs], i++,
				If[	blocs[[i,1]]!=1,
					flag=StringTake[data,{blocs[[i,1]]-1}];

					If[	DigitQ[flag] || LetterQ[flag],
						AppendTo[replist,{blocs[[i,1]],blocs[[i,1]]}]
					]
				]
		];
		newstring = StringReplacePart[data,"[[",replist];
		endlist = Traversal[newstring,"[[",2,gidata];
		replist2={};
		For[	i=1, i<=Length[endlist], i++,
				AppendTo[replist2,{endlist[[i]],endlist[[i]]}]
		];
		newstring = StringReplacePart[newstring,"]]",replist2];
		newstring
	];

FixLeftBrackets[data_]:=
	Block[{locs,clist,i,spot,tempstring},
		tempstring 	= data;
		locs		= StringPosition[data,"("];
		clist		= Characters[data];
		For[	i=1,i<=Length[locs],i++,
				spot= locs[[i,1]]-1;
				If[	spot!=0,
					If[	LetterQ[clist[[spot]]] || DigitQ[clist[[spot]]],
						tempstring=StringReplacePart[tempstring,"[",{spot+1,spot+1}]
					]
				]
		];

		tempstring
	];


FixRightBrackets[data_]:=
	Block[{tempstring,locs,test,i},

		tempstring 	= data;
		locs 		= Sort[Select[Traversal[tempstring,"[",1,fbdata],Positive]];
		If[	Length[locs]>0,
			test = Last[locs];
			If[	StringTake[tempstring,{test}]!=")",
				locs = Delete[locs,-1]
			]
		];
		For[	i=1,i<=Length[locs],i++,
				tempstring = StringReplacePart[tempstring,"]",{locs[[i]],locs[[i]]}]
		];

		tempstring
	];

FixUserDefinedFunctions[data_]:=
	Block[{loc,place,varstring,rulestring,addin,returnstring},

		place=StringPosition[data,"->"];
		If[	Length[place]==0,

			Return[data],

			loc				= place[[1,1]];
			varstring		= GetVariables[StringTake[data,loc-1]];
			rulestring		= StringTake[data,{loc+2,StringLength[data]}];
			addin			= "Function["<>First[varstring]<>","<>rulestring<>"]";
			returnstring	= StringReplacePart[data,addin,{Last[varstring],StringLength[data]}];
			Return[returnstring]
		]
	];


FixDots[data_,num_]:=
	Block[{endpoints,tempstring,arglist,bounds,newarglist,newstring},

		endpoints	= IsolateExpression[data,"StringTake",num];
		tempstring	= StringTake[data,endpoints];
		arglist		= IsolateArguments[tempstring];
		tempstring	= data;
		bounds		= arglist[[3]];
		bounds		= StringReplace[bounds,".."->","];
		bounds		= "{"<>bounds<>"}";
		newarglist	= Drop[arglist,-1];
		AppendTo[newarglist,bounds];
		newstring=StringReplacePart[tempstring,Rebuild[newarglist],endpoints];

		newstring
	];

FixRange[data_]:=
	Block[{flagpos,namepos,name,lbpos,lb,ubpos,ub,newstring,addin,tempstring},

		tempstring=data;
		If[	StringMatchQ[data,"*Plot*"],
			tempstring=FixPlotRange[data]
		];

		flagpos	= StringPosition[tempstring,".."][[1,1]];
		namepos	= FindName[tempstring];
		lbpos	= FindLowerBound[tempstring];
		name 	= StringTake[tempstring,{namepos+1,lbpos-1}];
		lb		= StringTake[tempstring,{lbpos+1,flagpos-1}];
		ubpos	= Traversal[tempstring,"..",0,ubdata][[1]];
		ub		= StringTake[tempstring,{flagpos+2,ubpos-1}];
		addin="{"<>name<>","<>lb<>","<>ub<>"}";
		newstring	= StringInsert[StringDrop[tempstring,{namepos+1,ubpos-1}],addin, namepos+1];

		newstring
	];

FixSubstitution[data_]:=
	Block[{endpoints,tempstring,newstring},

		endpoints	= IsolateExpression[data,"RepeatedReplace",1];
		tempstring	= FS[data];
		newstring	= StringReplacePart[data,tempstring,{endpoints[[1]],endpoints[[2]]}];

		newstring
	];


FixMember[data_,num_]:=
	Block[{tempstring,newstring,addin,endpoints,arglist},

		endpoints	= IsolateExpression[data,"MemberQ",num];
		tempstring	= StringTake[data,endpoints];
		arglist		= IsolateArguments[tempstring];
		arglist		= SwapArgs[arglist,2,3];
		addin		= Rebuild[arglist];
		newstring	= StringReplacePart[data,addin,endpoints];

		newstring
	];


FixLimit[data_]:=
	Block[{tempstring,endpoints,i,locs,returnstring},
		locs=StringPosition[data,"Limit["];
		returnstring=data;
		For[	i=1,i<=Length[locs],i++,
				endpoints		= IsolateExpression[returnstring,"Limit",i];
				tempstring		= StringTake[returnstring,endpoints];
				tempstring		= StringReplace[tempstring,LimitRules];
				returnstring	= StringReplacePart[returnstring,tempstring,endpoints];
		];
		returnstring
	];


FixProc[data_]:=
	Block[{funcname,paramstring,loc,tempstring,headerstring,varlist, varstring,body},

		loc = StringPosition[data,"proc"];

		If[	(loc[[1,1]]-2)>=0,
			tempstring=StringTake[data,loc[[1,1]]-2],
			tempstring="SetFunctionName"
		];

		funcname 	= GetVariables[tempstring][[1]];
		paramstring = BuildParameterString[GetParameters[data]];
		varlist		= GetLocalVariables[data];
		varstring	= BuildVariableString[varlist];
		body		= BuildBodyString[data,varlist];

		headerstring= funcname<>"["<>paramstring<>"]:=Block["<>varstring<>","<>body<>"];";
		headerstring
	];

GetVariables[data_]:=
	Block[{ending,clist,len,i,tempstring},
		clist	= Characters[data];
		len		= Length[clist];
		ending=0;
		i=len;
		While[
			ending==0,
			While[	DigitQ[clist[[i]]] || LetterQ[clist[[i]]],
					If[	i==1,
						Break[],i--
					]
			];
			If[	i==1,
				ending=1,
				Switch[
					clist[[i]],
					",",
						i--,
					")",
						i--,
					"(",
						i--,
					"_",
						i--,
					" ",
						i--,
					_,ending=i]
			];
		];

		If[	ending==1,
			ending=0
		];
		tempstring = StringTake[data,{ending+1,len}];
		tempstring = StringReplace[tempstring,{"("->"{",")"->"}"}];

		{tempstring,ending+1}
	];



End[]

EndPackage[]
