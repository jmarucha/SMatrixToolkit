BuildSMatrix::usage = "BuildSMatrix[problemTemplate, goal, data] creates SDPB input from problemTemplate and precomputed data, with goal of Maximal[variableName],Minimal[variableName] or Manual[association], being the vector to optimize.";
ProblemTemplate::usage = "An object of unitarity matrices with bindings to integral objects already applied.";
DecomposeLinearFunction::usage = "DecomposeLinearFunction[function, nargs] decomposes linear function into {constant_part,{function_of_argument1,...,function_of_argumentN}}";
LimitKeys::usage = "LimitKeys[keys, maxN] removes coefficients of form coeff[a,b,c] if a+b+c>maxN";
CreateProblemTemplate::usage = "CreateProblemTemplate[matrices, data] transforms a fromula for a set of UnitarityMatrix and the precomputed data into ProblemTemplate objecty which is an expression to translate matrices and data to SDPB input."

Options[BuildSMatrix] = {MaxN -> Infinity, MaxJ -> Infinity, AdditionalConstraints->{}}
Options[CreateProblemTemplate] = {MaxN -> Infinity, MaxJ -> Infinity, ExplicitBindings -> {}}

Begin["`Private`"]

ProblemTemplate[data_]["Templates"] := data["templates"];

ProblemTemplate[data_]["Keys"] := data["keys"];
ProblemTemplate[data_]["Keys", maxN_] := LimitKeys[data["keys"], maxN];

CreateProblemTemplate[matrices:{_UnitarityMatrix..}, PrecomputedData[bindings_, data_], OptionsPattern[] ]:=
  Module[{
    keys = (Union @@ Through[matrices["Coefficients"] ]) ~ LimitKeys ~ OptionValue[MaxN]
  },
  ProblemTemplate @ <|
    "keys" -> keys,
    "templates" -> Map[#["GenerateTemplate"][keys, bindings, Association[OptionValue[ExplicitBindings] ]  ]&,matrices],
    "data" -> data
  |>
]


BuildSMatrix[problemTemplate_ProblemTemplate, goal:Minimal[_]|Maximal[_]|Manual[_], data:PrecomputedData[bindings_, pages_], OptionsPattern[] ]:=
	Module[{
    goalVec,
    normalizationVec,
    keys = LimitKeys[problemTemplate[[1]][["keys"]],Echo[OptionValue[MaxN] ] ]
	},

  goalVec = If[goal[[0]] === Manual,
    Replace[keys,goal[[1]], {1}],
    SparseArray[
      { FirstPosition[keys, goal[[1]], _, {1}]->
      Switch[goal[[0]], Minimal, -1, Maximal, +1] }, {Length[keys]}]//Normal
  ];
  normalizationVec = SparseArray[{FirstPosition[keys, 1, _, {1}] -> 1}, {Length[keys]}]//Normal;


	Global`SDP[
		goalVec,
		normalizationVec,
    {Outer[
      LoadDataPage,
      problemTemplate["Templates"] ,
      pages//Select[#[[1]]<=OptionValue[MaxJ]&]
    ], Global`PositiveMatrixWithPrefactor[1,GenerateMatrixConstraint[#,keys] ]&/@OptionValue[AdditionalConstraints]}//Flatten
	]
]

LimitKeys[keys_List, maxN_]:=
  DeleteCases[keys, _[a_,b_,c_]/;(a+b+c)>maxN]

LoadDataPage[unitarityMatrixTemplate_UnitarityMatrixTemplate, DataPage[spin_, filename_] ]:=
  If[unitarityMatrixTemplate["JTest"][spin],
	  Module[{loadedData = Import[filename]//Select[unitarityMatrixTemplate["STest"][#[s] ]& ]},
        Transpose[(unitarityMatrixTemplate["matrixTemplate"]/.loadedData),{1, 4, 2, 3}] // Map[Global`PositiveMatrixWithPrefactor[1, #]& ] // Apply[Sequence]
	  ], Nothing
]

DecomposeLinearFunction[function_Function, nargs_Integer]:=
  Module[{const, separates},
  const = function @@ Table[0, nargs];
  separates = Table[
    Module[{tempVar, tempRes},
      tempRes = function @@ SparseArray[{i->tempVar}, {nargs}]-const;
      Evaluate[tempRes/.tempVar->Slot[1] ]//Function]
    ,{i, nargs}
  ];
  {const, separates}
]

GenerateMatrixConstraint[matrix_, keys_] := matrix/.(MapIndexed[#1 -> SparseArray[#2+1 -> 1, Length[keys] ] &, 
    keys // Rest]~
   Join~{"one" -> 
     SparseArray[{FirstPosition[keys, 1, _, {1}] -> 
        1}, {Length[keys]}]}) // Normal

End[]