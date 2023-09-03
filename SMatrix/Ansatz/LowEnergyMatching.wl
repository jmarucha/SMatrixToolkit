(* these functions are designed with matching amplitude ansatz with low energy expansion given by poly. however they are far from operational*)


BeginPrivateSection[]
LowEnergyMatching[ansatz_Ansatz == poly_] :=
    LowEnergyMatching[
        ansatz == poly,
        Exponent[poly/.MandelstamToOrder[ansatz["ParticleContent"]["TotalMass"] ], s] ]

LowEnergyMatching[ansatz_Ansatz == poly_, order_Integer] :=
    Module[
        {
            expandedVector = ansatz["Vector"]//
                Map[
                    Normal[Series[
                        RhoExpand[#]/.MandelstamToOrder[ansatz["ParticleContent"]["TotalMass"] ],
                        {s, 0, order}
                    ] ] &],
            expandedPoly = Series[
                poly/.MandelstamToOrder[ansatz["ParticleContent"]["TotalMass"] ], {s, 0, order}
            ],
            nonZeroTerms,
            logic
        },
        nonZeroTerms = Select[expandedVector, #=!=0&]//Echo;
        logic = Equal[nonZeroTerms//KeyValueMap[Times]//Total, expandedPoly];
        SolveAlways[logic, {x, s}]
        
    ]

OrderGuess[poly_]:= 3

MandelstamToOrder[totalMass_]:= {t -> ((-1-x)(s-totalMass))/2, u -> ((-1+x)(s-totalMass))/2}
EndPrivateSection[];
