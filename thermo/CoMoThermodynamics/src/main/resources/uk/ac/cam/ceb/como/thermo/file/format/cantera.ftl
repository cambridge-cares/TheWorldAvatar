species(name = "${SpeciesName}",
    atoms = " ${ElementalComposition}",
    thermo = (
       NASA( [ ${TLow}, ${TMid} ], [ ${Poly.LoTemp1},
               ${Poly.LoTemp2}, ${Poly.LoTemp3}, ${Poly.LoTemp4},
               ${Poly.LoTemp5}, ${Poly.LoTemp6}, ${Poly.LoTemp7} ] ),
       NASA( [ ${TMid}, ${THigh} ], [ ${Poly.HiTemp1},
               ${Poly.HiTemp2}, ${Poly.HiTemp3}, ${Poly.HiTemp4},
               ${Poly.HiTemp5}, ${Poly.HiTemp6}, ${Poly.HiTemp7} ] )
    ),
    note = "JChem ${Note}"
)
