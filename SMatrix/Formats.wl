Begin["`Private`"]
(* Format[RhoVariable[z_, OptionsPattern[] ] ]:=Subscript[\[Rho], z] *)

Format[Ansatz[data_] ]:=Grid[
  {
    {"Ansatz", SpanFromLeft, SpanFromLeft},
    {data[["particleContent"]], SpanFromLeft, SpanFromLeft},
    {"Free coefficients", Length[data[["vector"]]], Short[data[["vector"]]//Keys]},
    {Short[KeyValueMap[Times][data[["vector"]]]//Total, 4], SpanFromLeft, SpanFromLeft}
  }, 	Alignment->{Left, Baseline}, Frame->All
]

Format[Global`SDP[goal_, normaliation_, positiveMatrices_] ]:=Grid[
  {
    {"SDP", SpanFromLeft},
    {"Goal", goal},
    {"Normalization", normaliation},
    {positiveMatrices, SpanFromLeft}
  }, 	Alignment->{Left, Baseline}, Frame->All
]

Format[Global`PositiveMatrixWithPrefactor[_, matrixVector_] ]:= NumberForm[MatrixForm[matrixVector, TableDepth -> 2],3]

Format[UnitarityMatrixTemplate[data_] ] :=
  Grid[{
    data["keys"],
    data["matrixTemplate"]//Map @ MatrixForm
  }//Transpose, Frame->All]

End[]