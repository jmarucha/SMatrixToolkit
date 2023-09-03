(*
this section of code is designed to speed up computation of partial integrals by use of fixed order Gauss-Kronrod quadratre along with taking advantage of fast-powering the elements in Rho series. The initial trials shown absurdly promising speedup of about 30x vs ordinary approach with NIntegrate, however I could not get past memory management issues and on 32GB system the program crashes for attemped practical comptations.
*)


BatchIntegrate::usage = "BatchIntegrate[integralObjects, grid, spin]";
Options[BatchIntegrate] = {PolynomialBasis -> LegendreP}
Begin["`BatchIntegrate`"]

GetAtoms::usage;
GaussKronrodData::usage;
EvaluateAtomsForPoint::usage;
EvaluateIntegral::usage
EvaluatePolynomialBasis::usage;
EvaluatePrefactors::usage;

BeginPrivateSection[]

BatchIntegrate[integralObjects_, sGrid_, jGrid_, gkData_, prec_, OptionsPattern[] ] :=
    Module[{
        integralNames = Keys[integralObjects],
        splitIntegrals = Values[integralObjects] /. 
            prefactor_ Inactive[Integrate][ OptionValue[PolynomialBasis][_, _] integrand_, {var_, -1, 1}] :> {prefactor, integrand, var},
        vars, variable, (* extracting integration variable *)
        atoms,
        atomicGrid,
        polynomialWeightGrid = EvaluatePolynomialBasis[jGrid, gkData, prec, OptionValue[PolynomialBasis] ],
        numericalPrefactors, simplifiedIntegrals,
        integralsWithPrefactors,
        evaluatedIntegralGrid
    },
        vars = splitIntegrals[[All,3]]//Union;
        If[Length[vars]>1, Abort[] ]; (* todo *)
        variable = First[vars];
        atoms = splitIntegrals[[All,2]]//GetAtoms;
        simplifiedIntegrals = PrepareIntegrals[atoms, splitIntegrals[[All,2]] ];
        atomicGrid = ParallelMap[
            EvaluateAtomsForPoint[variable, atoms, #, gkData, prec+1]&, sGrid
        ];
        numericalPrefactors = EvaluatePrefactors[splitIntegrals[[All,1]], sGrid, prec];
        evaluatedIntegralGrid = ParallelMap[
            EvaluateIntegralPage[simplifiedIntegrals, #, polynomialWeightGrid]&, atomicGrid
        ] ~ Transpose ~ {2, 3, 1};
        integralsWithPrefactors = ParallelMap[numericalPrefactors*#&, evaluatedIntegralGrid];
        Return[
            <|
                "integralNames"->integralNames,
                "sGrid"->sGrid,
                "jGrid"->jGrid,
                "data"->integralsWithPrefactors
            |>
        ]
    ]


GetAtoms[expr_] := Cases[expr, RhoVariable[__], Infinity]//Union//Map[{#, Unique[atom]}&];


(* memoization *)
GaussKronrodData[n_, prec_] := GaussKronrodData[n, prec] = Module[
    {data = NIntegrate`GaussKronrodRuleData[n, prec]},
    {
       2*data[[1]]-1, (* sample points *)
       2*data[[2]], (* weights *)
       2*data[[3]] (* error estimate *)
    }
]

PrepareIntegrals[atoms_, integrals_] :=
    Module[
        {replacementRules = MapApply[Rule,atoms]},
        Map[ReplaceAll[replacementRules], integrals]
    ]
    


EvaluateAtomsForPoint[variable_, atoms_, gridPoint_, gkData_, prec_] :=
    Module[{
        vsubs = {variable->#}&/@gkData[[1]], (* values of cosTh *)
        subNameList = atoms[[All, 2]]~Join~{variable},
        evalList = atoms[[All, 1]]~Join~{variable}/.gridPoint
    },
    N[evalList/.vsubs, prec]//Map[AssociationThread[subNameList, #] ~ Join ~ gridPoint &]
]


EvaluatePolynomialBasis[spins_, gkData_, prec_,  basis_:LegendreP] :=
    Outer[
        {spin, samplePoint}
            |-> basis[spin, samplePoint[[1]] ]*samplePoint[[2]] ,
        spins,
        gkData//Transpose,
        1 (* otherwise it decoposes sampling points *)
    ]


(* shape: Length[simplifiedIntegrals], Length[gkRule[[]] ]*)
EvaluateSamplePoints[simplifiedIntegrals_, atomicGrid_] :=
    Map[simplifiedIntegrals/.#&,atomicGrid]//Transpose


EvaluateIntegralPage[simplifiedIntegrals_, atomicGrid_, basisWeights_]:=
    Outer[Dot, EvaluateSamplePoints[simplifiedIntegrals, atomicGrid], basisWeights, 1]

(* lots of prefactors reappear so counting the uniques is a good idea *)
EvaluatePrefactors[prefactors_, grid_, prec_] :=
    ReplaceAll[prefactors, Union[prefactors]//AssociationMap[ReplaceAll[grid] ] ]//Transpose

EndPrivateSection[]

End[]

