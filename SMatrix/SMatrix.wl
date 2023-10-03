
BeginPackage["SMatrix`"];


Minimal::usage = "Minimize coefficient";
Maximal::usage = "Maximize coefficient";

BeginPrivateSection[]:=(
  PrependTo[$ContextPath, Context[] ];
  $Context = $Context <> "Private`";
)

EndPrivateSection[]:=(
  $Context = First[$ContextPath];
  $ContextPath = Rest[$ContextPath];
)

With[
  {ApplicationDirectory = DirectoryName[$InputFileName]},
  
  Get[FileNameJoin[{ApplicationDirectory, "Headers.wl"}] ]
  Get[FileNameJoin[{ApplicationDirectory, "RhoVariable.wl"}] ]

  Get[FileNameJoin[{ApplicationDirectory, "Ansatz", "Ansatz.wl"}] ]
  Get[FileNameJoin[{ApplicationDirectory, "Ansatz", "ExtraParam.wl"}] ]
  Get[FileNameJoin[{ApplicationDirectory, "Ansatz", "LowEnergyMatching.wl"}] ]
  Get[FileNameJoin[{ApplicationDirectory, "Ansatz", "RhoSeries.wl"}] ]
  Get[FileNameJoin[{ApplicationDirectory, "Ansatz", "Particle.wl"}] ]

  Get[FileNameJoin[{ApplicationDirectory, "Precompute", "Evaluation.wl"}] ]
  Get[FileNameJoin[{ApplicationDirectory, "Precompute", "Grids.wl"}] ]
  Get[FileNameJoin[{ApplicationDirectory, "Precompute", "BatchIntegrate.wl"}] ]

  Get[FileNameJoin[{ApplicationDirectory, "PartialWaves.wl"}] ]
  Get[FileNameJoin[{ApplicationDirectory, "ProblemBuilder.wl"}] ]
  Get[FileNameJoin[{ApplicationDirectory, "UnitarityMatrix.wl"}] ]
  Get[FileNameJoin[{ApplicationDirectory, "Formats.wl"}] ]

  Get[FileNameJoin[{ApplicationDirectory, "ResultsParser.wl"}] ]
]

EndPackage[];