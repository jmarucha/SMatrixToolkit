

(* globals *)
Protect[{s,t,u}]
Protect[J]
If[Not[ValueQ[$WorkingPrecision] ], $WorkingPrecision = $MachinePrecision]

(* RhoVariable.wl *)
RhoVarible::usage="RhoVariable[z, Center->s0, Cut->w] symbolically defines \[Rho] building block for amplitude ansatz.";
RhoExpand::usage="RhoExpand[expr] replaces each occurence of RhoVariable[...] with the explicit expression.";
SetAttributes[RhoVariable, {NHoldRest, NumericFunction}]
Options[RhoVariable] = {Cut -> 4, Center -> 0};

(* Ansatz/Ansatz.wl *)

FixCoefficient::usage="";
LowEnergyMatching::usage="";
(* Ansatz/Particle.wl *)
Particle::usage = "Patricle[name,m] symbolizes the particle of mass m";
ParticleContent::usage = "ParticleContent[{I1, I2}, {O1, O2}] represents input particles I1,I2 and output particles O1,O2";

Begin["`ParticleContent`"]

MandelstamSymmetry::usage = "MandelstamSymmetry[particleContent] infers the crossing symmetry related to given set of particles";
CrossingToMandel::usage = "CrossingToMandel[twoWayRule] transforms two-way rule into crossing in terms of s,t,u";
Crossing::usage = "Crossing[particleContent, twoWayRule] generates particlecontent with describet particle swapped";
ReplacementRule::usage = "";
End[]

UnitarityMatrix::usage = "UnitarityMatrix[f, ansatze] declares unitarity matrices as a function f of amplitude anstatze ansatze.";
UnitarityMatrixTemplate::usage = ""
ProblemTemplate::usage = ""
CreateProblemTemplate::usage = ""

LoadDataPage::usage = ""

PreparePartialIntegral::usage;