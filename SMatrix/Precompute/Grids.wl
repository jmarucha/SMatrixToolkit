

ChebyshevGrid::usage = "ChebyshevGrid[N, \
\!\(\*SubscriptBox[\(\[Rho]\), \(s\)]\)] - grid which is denser near \
end points (threshold and infinity)"

RegularGrid::usage = "RegularGrid[N, \!\(\*SubscriptBox[\(\[Rho]\), \(s\)]\)] - points \
evenly spaced in Arg[\!\(\*SubscriptBox[\(\[Rho]\), \(s\)]\)]"

Options[PhiGrid]={WorkingPrecision:>$WorkingPrecision, ExtraObjects->{}}
Options[ChebyshevGrid]=Options[PhiGrid]
Options[ChebyshevGrid]=Options[PhiGrid]

BeginPrivateSection[];

InverseRhoS[RhoS:RhoVariable[s,___] ] := rho|->Evaluate[ s/.Quiet[Solve[rho == RhoExpand[RhoS], s],{Solve::nongen}]//First ] 


RegularPhiList[count_Integer]:=
	Table[(i \[Pi])/(count+1),{i, count}]

ChebyshevPhiList[count_Integer]:=
	With[{
		rangeTrans = t |->(1-t)/2*\[Pi],
		node = i |-> Cos[(2i -1)/(2 count) \[Pi]]
	}, Table[rangeTrans[ node[i] ],{i, count}] ]


ChebyshevGrid[count_Integer, RhoS:RhoVariable[s,___], opt:OptionsPattern[] ] :=
	PhiGrid[RegularPhiList[count], RhoS, opt]

RegularGrid[count_Integer, RhoS:RhoVariable[s,___], opt:OptionsPattern[] ] :=
	PhiGrid[ChebyshevPhiList[count], RhoS, opt]



PhiGrid[phiList_, RhoS:RhoVariable[s,___], OptionsPattern[] ]:=
Module[
	{
		RhoInv = InverseRhoS[RhoS],
		objectList = Union[{RhoS}, OptionValue[ExtraObjects] ] ~ Join ~ {s},
		sOfAngle
	},
	sOfAngle = phi |-> Evaluate[RhoInv[Exp[I phi] ]//ComplexExpand//Simplify];
	
	phiList // Map[
			phi |-> objectList // AssociationMap[object|->N[object/.{RhoS->Exp[I phi], s->sOfAngle[phi] }, OptionValue[WorkingPrecision] ] ]
	]
]
EndPrivateSection[];