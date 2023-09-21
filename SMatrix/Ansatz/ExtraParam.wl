Pole::usage = "Pole[
	particleContent,
	coefficient, channel, location] creates amplitude ansatz with with free coefficient coefficient associated with term 1/(channel-location). Channels shall be s, t or u, and location should be a numerical valuue."
ExtraParam::usage = "ExtraParam[particleContent,coefficient, value] creates amplitude ansatz with coefficient coefficient and associated term value."
ExtraParam::badsymmetry = "beep boop unknown symmetry '``'"

Options[Pole] = {Symmetry->Automatic};
Options[ExtraParam] = {Symmetry->Automatic};
Begin["`Private`"]

Pole[
	particleContent_ParticleContent,
	coefficient_, channel_, location_,
	opt:OptionsPattern[]
]:=ExtraParam[particleContent, coefficient, -1/(channel-location), opt]

ExtraParam[
	particleContent_ParticleContent,
	coefficient_, value_,
	OptionsPattern[]
]:=With[
	{symmetrizedValue=ExtraParam`ImposeSymmetryOnValue[value,If[OptionValue[Symmetry]===Automatic, particleContent @ "MandelstamSymmetry", OptionValue[Symmetry] ] ]},
Ansatz@<|
	"vector"-><|1->0, coefficient->symmetrizedValue|>,
	"particleContent"->particleContent
|> ]

ExtraParam`ImposeSymmetryOnValue[value_, symmetry_] := Switch[symmetry,
	"ST", (value + (value/.{s->t, t->s})),
	"SU", (value + (value/.{s->u, u->s})),
	"TU", (value + (value/.{t->u, u->t})),
	"STU", 1/2*Total[
    value/.Thread[{s, t, u} -> #] & /@ Permutations[{s, t, u}]
  ],
  "none", value,
	_, (Message[ExtraParam::badsymmetry, symmetry];0)]//Expand

End[]
