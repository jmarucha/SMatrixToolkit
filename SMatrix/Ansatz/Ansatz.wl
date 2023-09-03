
(* basically a wrapped assoc *)
Ansatz::usage = "Amplitude anzatz.";

Reshape::usage = "Reshape[ansatz == expr, {s->s0,t->t0,u->u0}] redefines coefficients such that amplitude has value expr at a given point]";
Reshape::nocoeffs = "Cannot impose relation with given restrictions.";
Reshape::ambigous = "Multiple ways of reshaping ansatz. Removing coefficient `1` while keeping `2`";


IntegralObjects::usage = "IntegralObjects[ansatz] generates functions to be decoposed in partial waves for precomputation";
CreateAnsatz::usage = "CreateAnsatz[{inParticle1,inParticle2}, {outParticle1, outParticle2}]";
Crossing::usage = "Crossing[ansatz, crossing] creates amplitude ansatz with particles exchanged as given by crossing. Crossings are ginven as is two-way rules, for example \"I1\"<->\"O1\" swaps first in-particle with first out-particle.";

Options[Reshape] = {KeepCoefficients -> {}};

Begin["`Private`"]

Ansatz[data_]["Fields"]:={"Coefficients", "Expression", "Integrals", "MassPattern", "Length", "Symmetry", "_Data"}
Ansatz[data_]["_Data"]:=data;

Ansatz[data_]["Coefficients"]:=Keys[data[["vector"]]];
Ansatz[data_]["Vector"]:=data[["vector"]];
Ansatz[data_]["Expression"]:=KeyValueMap[Times][ data[["vector"]] ]//Total;

Ansatz[data_]["ParticleContent"]:=data[["particleContent"]];

Ansatz[data_]["IntegralObjects"]:=IntegralObjects[ Ansatz[data] ];
Ansatz[data_]["PartialIntegrals"]:=Module[
  {
    integralObjects = Ansatz[data] @ "IntegralObjects",
    particleContent = data["particleContent"]
  },
  AssociationMap[
    PreparePartialIntegral[particleContent],
    integralObjects
  ]//KeyMap[MarkMasses[#,particleContent]& ]
];

(ansatz:Ansatz[data_])["ApplyBindings"][bindings_, explicitBindings_]:=
Module[
  {
    vector = ansatz @ "Vector" // KeyDrop[Keys[explicitBindings] ],
    particleContent = ansatz @ "ParticleContent"
  },
  (vector//Map[ApplyBindings[bindings, particleContent] ]) ~ Join ~ Association[explicitBindings]
]

ApplyBindings[bindings_, particleContent_][expr_]:=
  expr//
    GroupByTUs//
    MarkMasses[#,particleContent]&//
    Replace[#, bindings, {1}]&//
    Replace[#, bindings, {2}]&//
    Replace[#, {1->
      2*NormalizationOfPartialAmplitude[particleContent @ "MassPattern"]
      *KroneckerDelta[J,0]}, {1}]&//
    #*SymmetryFactor[particleContent @ "PartialWaveSymmetry"] &//
    KeyValueMap[Times]//
    Total


IntegralObjects[a_Ansatz]:=a["Expression"]//GroupByTUs//Values//Union//DeleteCases[1]

GroupByTUs[expr_] :=
  Module[
    {
    normalizedExpression = With[{expandedExpression = expr//Expand}, If[Head[expandedExpression] === Plus, List @@ expandedExpression, {expandedExpression} ] ]
    },
    normalizedExpression // Map[
      Function[term,
        If[FreeQ[term, t|u], {term, 1},
          Replace[
          {
            Longest[prefactor_?(FreeQ[t | u])]*rest_. -> {prefactor, rest},
            rest_ -> {1, rest}
          }][term]
        ]
      ]
    ] // GroupBy[#, First -> Last, Total] &
  ];

MarkMasses[expr_, particleContent_]:=expr/.{
  t->t[particleContent["UniversalMassPattern"] ],
  u->u[particleContent["UniversalMassPattern"] ]
}

NormalizePolynomialByTUs[poly_] :=
  poly//GroupByTUs//KeyValueMap[Times]//Total;

Ansatz/:Plus[Ansatz[data1_], Ansatz[data2_] ]:=
Ansatz@<|
  "vector"->data1["vector"]~Join~data2["vector"],
  "particleContent"->data1["particleContent"]
|>;

Reshape[ansatz_Ansatz == Longest[prefactor_?NumericQ] newCoefficient_, 
rules : List[__Rule ...], OptionsPattern[] ] :=
Block[{
  lhs = ((ansatz["Expression"]) /. rules) // RhoExpand,

  zeroedKeys,
  coeffsToReshape,
  vectorToReshape,
  reshapedVals,
  restrictableCoefficients,
  removedCoeff,
  reshapedVector,
  newData = ansatz["_Data"]
  },
coeffsToReshape = 
  Select[ansatz["Coefficients"], Not[FreeQ[lhs, #] ] &];
vectorToReshape = ansatz["Vector"] // KeyTake[coeffsToReshape];
reshapedVals = vectorToReshape /. rules // RhoExpand;

zeroedKeys = Keys[Select[reshapedVals, EqualTo[0] ] ];
coeffsToReshape = Complement[coeffsToReshape, zeroedKeys];
vectorToReshape = KeyDrop[vectorToReshape, zeroedKeys];
reshapedVals = KeyDrop[reshapedVals, zeroedKeys];

restrictableCoefficients = 
  Select[coeffsToReshape, 
  FreeQ[OptionValue[KeepCoefficients], #] &];
Assert[Length[restrictableCoefficients] != 0, Reshape::nocoeffs];
If[Length[restrictableCoefficients] > 1, 
  Message[Reshape::ambigous, restrictableCoefficients // First, 
  restrictableCoefficients // Rest] ];
removedCoeff = restrictableCoefficients // First;
reshapedVector =
  (Map[ (* reshaped old coefficients *)
      key |-> key -> 
        vectorToReshape[key] - 
        reshapedVals[key]*
          vectorToReshape[removedCoeff]/reshapedVals[removedCoeff], 
      coeffsToReshape] // KeyDrop[removedCoeff])~Join~
  <|newCoefficient -> (* newCoefficient *)
    prefactor/( Length[coeffsToReshape])
      Total @ Map[vectorToReshape[#]/reshapedVals[#] &, 
        coeffsToReshape]|>;
newData["vector"] = 
  Expand/@(Join[KeyDrop[newData["vector"], coeffsToReshape], reshapedVector]);
Return[Ansatz @ newData]
];

Crossing[Ansatz[data_], crossing:TwoWayRule[_,_] ]:=
  Ansatz @ <|
    "vector" -> data["vector"]/.(SMatrix`ParticleContent`ReplacementRule[crossing]),
    "particleContent" -> 
      SMatrix`ParticleContent`Crossing[data["particleContent"], crossing]
  |>;


FixCoefficient[ansatz_Ansatz, coeff_ == value_?NumericQ] :=
  Ansatz @ <|
    "vector" -> With[{oldVector = ansatz@"Vector"},
        Join[
          oldVector//KeyDrop[coeff],
          <|1 -> value*oldVector[[ Key[coeff] ]]+oldVector[[ Key[1] ]] |>
    ] ],
    "particleContent" -> ansatz@"ParticleContent"
  |>/;KeyExistsQ[ansatz@"Vector", coeff];
FixCoefficient[ansatz_Ansatz, coeff_ == value_] := FixCoefficient[ansatz, value == coeff]/;KeyExistsQ[ansatz@"Vector", value];

End[]