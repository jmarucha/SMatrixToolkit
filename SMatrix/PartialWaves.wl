

(* Begin["`PartialWaves`"] *)

PreparePartialIntegral::usage = "PreparePartialIntegral[particleContent][expr] transforms building block expr to an integral giving partial amplitude associated with expr";
MandelstamToAngle::usage = "MandelstamToAngle[{{imass1, imass2},{omass1, omass2}}] generate suubstitution rules to replace t and u with functions of cos\[CapitalTheta] and s";
NormalizationOfPartialAmplitude::usage = "XD";
SymmetryFactor::usage = "XD";

cosTh::usage = "Cosine with a fancy display";

BeginPrivateSection[];

Format[cosTh]:="cos\[CapitalTheta]";

PreparePartialIntegral[particleContent_ParticleContent][expr_] :=
Module[
	{
		integrand,
		massPattern = particleContent @ "MassPattern",
		symmetry = particleContent @ "PartialWaveSymmetry"
	},
	integrand = expr/.MandelstamToAngle[massPattern];
	NormalizationOfPartialAmplitude[massPattern]*
		Inactive[Integrate][LegendreP[J, cosTh]integrand,{cosTh, -1, 1}]
];

(* 2011.11708 D.22 *)
MandelstamL[s_,{m1_, m2_}]:=Sqrt[s^2-2s(m1^2+m2^2)+(m1^2-m2^2)^2]

(* following 2011.11708 D.24 *)
MandelstamToAngle[{{imass1_, imass2_},{omass1_, omass2_}}]:=Evaluate[{
	t->1/(2s) (SumM2 s - s^2 + cosTh MandelstamL[s, {imass1, imass2}] MandelstamL[s, {omass1, omass2}] - DiffM2),
	u->1/(2s) (SumM2 s - s^2 - cosTh MandelstamL[s, {imass1, imass2}] MandelstamL[s, {omass1, omass2}] + DiffM2)}/.
		{SumM2 -> imass1^2+imass2^2+omass1^2+omass2^2, DiffM2 -> (imass1^2-imass2^2)(omass1^2-omass2^2)
}]

 (* 2011.11708 2.32 *)
`ClebschGordan[s_,{m1_, m2_}]:=Sqrt[8\[Pi] s/MandelstamL[s,{m1, m2}] ]

(* 2011.11708 2.106 *)
`NormalizationOfPartialAmplitude[{{imass1_, imass2_},{omass1_, omass2_}}] :=
	1/(2*
		`ClebschGordan[s,{imass1, imass2}]
		`ClebschGordan[s,{omass1, omass2}])

NormalizationOfPartialAmplitude[{{imass1_, imass2_},{omass1_, omass2_}}] := `NormalizationOfPartialAmplitude[{{imass1, imass2},{omass1, omass2}}]

`SymmetryFactor[symmetry_String]:= Switch[symmetry,
	"Both", 1/2,
	"In", Sqrt[2]/2,
	"Out", Sqrt[2]/2,
	"None", 1,
	_, Message[smatrix::bad, symmetry];0]

SymmetryFactor[symmetry_String] := `SymmetryFactor[symmetry]

EndPrivateSection[];
(* End[] *)
