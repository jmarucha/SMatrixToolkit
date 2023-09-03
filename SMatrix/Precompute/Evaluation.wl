
PrecomputedData; DataPage;

PrepareAnsatz::usage = "PrepareAnsatz[ansatz, precomputedData] creates an substitution-optimized vector from ansatz";
LoadData::usage = "LoadData[directory] gets an array of percomputed integrals from the folder";

SplitStages::usage = "SplitStages[integralObjects] generates symbol mappings for quick load of integral tables";
PrecomputeGrid::usage = "PrecomputeGrid[directory, integralObjects, grid, spins] evaluates and saves integralObjects at given grid for provided set of spins";
EvaluateGrid::usage = "EvaluateGrid[integralObjects, grid, spin] evaluates integrals from integralObjects at points in given grid with given spin";
EvaluatePoint::usage = "EvaluatePoint[integralObjects, gridPoints] evaluates integrals from integralObjects at given gridPoints";

Begin["`Private`"]


EvaluateGrid[integralObjects_,grid_, spin_, nintegrateOptions:OptionsPattern[NIntegrate] ]:=
  Map[gridPoint|->EvaluatePoint[integralObjects, gridPoint~Join~<|J->spin|>, nintegrateOptions], grid]

EvaluatePoint[integralObjects_, gridPoint_, nintegrateOptions:OptionsPattern[NIntegrate] ]:=Map[
		obj|-> (obj/.(Inactive[Integrate][expr_,args__] :> NIntegrate[RhoExpand[Evaluate[expr/.gridPoint] ], args, nintegrateOptions] ))/.gridPoint,
		integralObjects
	]~Join~gridPoint


(* assign each of composite integral object a simple name to massively speed up loading*)
SplitStages[integralObjects_]:=
  KeyValueMap[{key, value}|->
    With[{integralObject = Unique["integralObject"]},
    {
		key -> integralObject,
		integralObject->value,
		integralObject->If[key === (key/.{t->u, u->t}), 0, value]
	}],
    integralObjects]//
      Transpose//
      (
			x|->{x[[1]],
			Association[x[[2]]],
			Association[x[[3]]]
			})


PrecomputeGrid[directory_?DirectoryQ, integralObjects_, grid_, spinGrid_, nintegrateOptions:OptionsPattern[NIntegrate] ]:=
	Module[
		{
			absPath = ExpandFileName[directory],
			stages = SplitStages[integralObjects],
			bindings, evenTasks, oddTasks
		},
		{bindings, evenTasks, oddTasks} = stages;
		(* abs path because parallel contexts are anything but predictable *)
		Export[FileNameJoin[{directory, "integralObjectsMap.m"}], bindings];
    (* food for thought - shall we split by spin? *)
		ParallelMap[Function[spin, 
			Export[FileNameJoin[{absPath,"spin="~~ToString[spin]~~".mx"}],
			EvaluateGrid[
				If[EvenQ[spin], evenTasks, oddTasks],
				grid,
				spin,
				nintegrateOptions]
			] ], spinGrid, DistributedContexts->Automatic];
	]


LoadData[directory_?DirectoryQ]:=
	Module[{bindings, datasetPaths, dataPages},
	bindings = Import[FileNames["integralObjectsMap.m*", directory]//First];
	datasetPaths = FileNames["spin*.mx", directory];
	dataPages = Map[
		filename|->DataPage[
			StringCases[__~~"="~~spinString:NumberString~~".mx":>FromDigits[spinString] ][filename][[1]],
			filename
		],datasetPaths
	];
	PrecomputedData[bindings,dataPages] 
]

End[]