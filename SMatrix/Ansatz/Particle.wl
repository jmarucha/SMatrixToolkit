



Begin["`ParticleContent`"]

BeginPrivateSection[]

`Path = <|
    "I1"->{1,1},
    "I2"->{1,2},
    "O1"->{2,1},
    "O2"->{2,2}
|>

Particle[_,m_]["m"] := m;


particleRefs = ("I1"|"I2"|"O1"|"O2");

ParticleContent[p__]["MandelstamSymmetry"] := MandelstamSymmetry @ ParticleContent[p];
ParticleContent[p__]["PartialWaveSymmetry"] := PartialWaveSymmetry @ ParticleContent[p];

ParticleContent[p__]["MassPattern"] := Map[#@"m"&,{p},{2}]
ParticleContent[p__]["UniversalMassPattern"] :=
    Module[{massPattern = Map[#@"m"&,{p},{2}]},
        Sort[
            {massPattern[[{1, 2}, {1, 2}]],
            massPattern[[{2, 1}, {1, 2}]],
            massPattern[[{2, 1}, {2, 1}]],
            massPattern[[{1, 2}, {2, 1}]]}
        ]//Last (* i1 becomes the largest mass *)
    ]


ParticleContent[p__]["TotalMass"] := Total[ParticleContent[p]["MassPattern"]^2, 2];
ParticleContent[p__]["Fields"]:={"MandelstamSymmetry", "PartialWaveSymmetry", "MassPattern"}


ParticleContent[p__][ref:particleRefs] := Extract[{p}, `Path[ref] ];




MandelstamSymmetry[
    ParticleContent[{i1_Particle, i2_Particle},{o1_Particle, o2_Particle}] 
] :=
    {
        If[i1===i2 || o1===o2, "TU", Nothing],
        If[i1===o1 || i2===o2, "SU", Nothing],
        If[i1===o2 || i2===o1, "ST", Nothing]
    }//Switch[Length[#],
        0, "None",
        1, First[#],
        2, "STU",
        3, "STU"
    ]&;

PartialWaveSymmetry[
    ParticleContent[{i1_Particle, i2_Particle},{o1_Particle, o2_Particle}]
]:=If[i1===i2 && o1===o2, "Both",
    If[i1===i2, "In",
        If[o1===o2, "Out",
            "None"
        ]
    ]
];


CrossingToMandel[TwoWayRule[OrderlessPatternSequence["I1","I2"] ] ]="TU";
CrossingToMandel[TwoWayRule[OrderlessPatternSequence["O1","O2"] ] ]="TU";

CrossingToMandel[TwoWayRule[OrderlessPatternSequence["I1","O1"] ] ]="SU";
CrossingToMandel[TwoWayRule[OrderlessPatternSequence["I2","O2"] ] ]="SU";

CrossingToMandel[TwoWayRule[OrderlessPatternSequence["I1","O2"] ] ]="ST";
CrossingToMandel[TwoWayRule[OrderlessPatternSequence["I2","O1"] ] ]="ST";

ReplacementRule["TU"]={t->u, u->t};
ReplacementRule["ST"]={t->s, s->t};
ReplacementRule["SU"]={s->u, u->s};

ReplacementRule[p:TwoWayRule[OrderlessPatternSequence[_, _] ] ] := p//CrossingToMandel//ReplacementRule;

Crossing[p_ParticleContent, TwoWayRule[partName1_,partName2_] ] := ReplacePart[p,
    {
        `Path[partName1] -> p[partName2],
        `Path[partName2] -> p[partName1]
    }
]

EndPrivateSection[];

End[];