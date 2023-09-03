
UnitarityMatrix::usage = "UnitarityMatrix[f, ansatze] declares unitarity matrices as a function f of amplitude anstatze ansatze.";


Options[UnitarityMatrix] = {JConstraints -> Automatic, SConstraints -> Automatic};


UnitarityMatrix[function_Function, ansatz_Ansatz, opt:OptionsPattern[] ] :=
    UnitarityMatrix[function, {ansatz}, opt];

UnitarityMatrix[function_Function, ansatze_List, OptionsPattern[] ]["Coefficients"] :=
    Union @@ Through[ansatze["Coefficients"] ];

UnitarityMatrix[function_Function, ansatze_List, OptionsPattern[] ]["JTest"] :=
    Switch[OptionValue[JConstraints],
        Automatic, If[
            ContainsNone[{"None"}] @ UnitarityMatrix[function, ansatze]["PartialWaveSymmetry"],
            EvenQ, True&
        ],
        None, True&,
        f_, f
]

UnitarityMatrix[function_Function, ansatze_List, OptionsPattern[] ]["STest"] :=
    Switch[OptionValue[SConstraints],
        Automatic, GreaterThan[FirstCut[ansatze] ]/.GreaterThan[Infinity]->(True&),
        None, True&,
        f_, f
    ]

(this:UnitarityMatrix[function_Function, ansatze_List, opt:OptionsPattern[] ])["GenerateTemplate"][keys_, bindings_, explicitBindings_] :=
Module[{
    mappedAnsatze = MapIndexed[#1["ApplyBindings"][bindings, Map[Extract[#2],explicitBindings] ]&][ansatze],
    functionTemplate = DecomposeLinearFunction[function, Length[ansatze] ],
    matrixTemplate
},
    matrixTemplate = Map[
        key |-> Inner[
            {linerarMap, ansatz} |-> linerarMap[key;Lookup[ansatz, key, 0] ],
            functionTemplate[[2]],
            mappedAnsatze
        ],
        keys
    ];
    (* constant part needs some extra love *)
    matrixTemplate[[FirstPosition[keys, 1, _, 1] ]] += {functionTemplate[[1]]};
    UnitarityMatrixTemplate @ <|
        "keys" -> keys,
        "matrixTemplate" -> matrixTemplate,
        "JTest" -> this["JTest"],
        "STest" -> this["STest"]
        |>
]

UnitarityMatrix[_Function, ansatze_List, OptionsPattern[] ]["PartialWaveSymmetry"]:=
    Map[#["ParticleContent"]["PartialWaveSymmetry"]&, ansatze];


FirstCut[ansatze_List]:=Cases[Map[#["Vector"]&,ansatze],
 RhoVariable[s, rhoOpt : OptionsPattern[] ] :> {OptionValue[Cut]}, Infinity]//Min

 UnitarityMatrixTemplate[data_]["JTest"] := data[["JTest"]]
 UnitarityMatrixTemplate[data_]["STest"] := data[["STest"]]
 UnitarityMatrixTemplate[data_]["matrixTemplate"] := data[["matrixTemplate"]]