RhoVariable[z_,opt:OptionsPattern[] ]["Expression"] := 
  (Sqrt[Cut-Center] - Sqrt[Cut-z])/
  (Sqrt[Cut-Center] + Sqrt[Cut-z]) /.
    Merge[First][{opt, Options[RhoVariable]}]

RhoExpand[expr_] := (expr /. rho_RhoVariable :> rho @ "Expression")
Begin["`Private`"];

N[rho:RhoVariable[z_Real, opt:OptionsPattern[] ], pa__]:=
  N[rho @ "Expression", pa]//Conjugate;

N[rho:RhoVariable[z_Complex, opt:OptionsPattern[] ], pa__]:=
  N[rho @ "Expression", pa];
  
N[RhoVariable[z_?NumericQ,c:OptionsPattern[] ], pa__] := N[ RhoVariable[N[z, pa], c], pa];

End[];