LoadDatapoint::usage = "LoadDatapoint[folder, variableNames]"


Begin["`Private`"]

LoadDatapoint[folder_, variableNames_] := Module[{
    keys = Import[variableNames],
    yTxt = Import[FileNameJoin[{folder, "y.txt"}] ],
    outTxt = Import[FileNameJoin[{folder, "out.txt"}] ],
    outAssoc, yAssoc
},
outAssoc = StringSplit[outTxt, "\n"]//
    Map[StringCases[#,StartOfString~~Shortest[key__]~~Whitespace~~"="~~Whitespace~~value__~~";":> (key -> Interpreter["Number" | "Expression"][value])] &]// Flatten;
yAssoc = AssociationThread[keys//Rest, StringSplit[yTxt, "\n"]//Rest//Interpreter["Number"] ];
Return[Association[outAssoc]~Join~<|"vector"->yAssoc|> ]
]


ParseNumber[str_]

End[]