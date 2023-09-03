RhoSeries::usage = "RhoSeries[massPattern, coefficientName, RhoS, RhoT, RhoU, maxN] - generate usual amplitude ansatz"

Begin["`Private`"]


RhoSeries[
	particleContent_ParticleContent,
	coefficient_,
	RhoS:RhoVariable[s, OptionsPattern[] ],
	RhoT:RhoVariable[t, OptionsPattern[] ],
	RhoU:RhoVariable[u, OptionsPattern[] ],
	maxN_Integer,OptionsPattern[]
]:=
	Ansatz[
		<| 
		"vector"->RhoSeries`Vector[particleContent, coefficient, RhoS, RhoT, RhoU, maxN],
		"particleContent"->particleContent
		|>
	];


RhoSeries`Vector[
	particleContent_ParticleContent,
	coefficient_,
	RhoS:RhoVariable[s,OptionsPattern[] ],
	RhoT:RhoVariable[t,OptionsPattern[] ],
	RhoU:RhoVariable[u,OptionsPattern[] ],
	maxN_Integer,OptionsPattern[]
]:=<|Table[{coefficient[a,b,c]->RhoS^a RhoT^b RhoU^c},{a,0, maxN}, {b,0, maxN-a}, {c,0, maxN-a-b}]//Flatten, 1->0|>\
	//RhoSeries`MandelstramRedundance//RhoSeries`ImposeSymmetry[particleContent @ "MandelstamSymmetry"]


RhoSeries`ImposeSymmetry[symmetry_][assoc_Association] := assoc//KeyValueMap[{coeff, variable}
  |->{RhoSeries`ImposeSymmetryOnCoefficient[coeff,symmetry], variable}]//GroupBy[#,First->Last,Total]&;

RhoSeries`MandelstramRedundance[vector_Association] := vector//KeySelect[key |-> key/.{_[abc__]:>Times[abc]==0, Except[_[abc__] ]->True}]


RhoSeries`ImposeSymmetryOnCoefficient[coefficient_[a_,b_,c_], symmetry_] := Switch[symmetry,
	"ST", coefficient @@ Insert[Sort[{a,b}],c,3],
	"SU", coefficient @@ Insert[Sort[{a,c}],b,2],
	"TU", coefficient @@ Insert[Sort[{b,c}],a,1],
	"STU", coefficient @@ Sort[{a,b,c}],
	_, coefficient @ {a,b,c}]

RhoSeries`ImposeSymmetryOnCoefficient[coefficient:Except[ _[_,_,_] ], symmetry_]:=coefficient

End[]