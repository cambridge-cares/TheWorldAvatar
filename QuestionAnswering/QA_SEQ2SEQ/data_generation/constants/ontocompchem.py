OCC_RESULT_LABELS = {
    "OptimizedGeometry": ["optimized geometry", "geometry", "molecular structure"],
    "SCFEnergy": ["SCF energy", "energy", "electronic energy"],
    "TotalGibbsFreeEnergy": ["total Gibbs free energy"],
    "ZeroPointEnergy": ["zero point energy", "ground state energy"],
    "HOMOEnergy": ["HOMO energy", "energy of highest occupied molecular orbital"],
    "HOMOMinus1Energy": [
        "HOMO-1 energy",
        "NHOMO energy",
        "energy of next-to-highest occupied molecular orbital",
    ],
    "HOMOMinus2Energy": ["HOMO-2 energy"],
    "LUMOEnergy": ["LUMO energy", "energy of lowest unoccupied molecular orbital"],
    "LUMOPlus1Energy": [
        "LUMO+1 energy",
        "SLUMO energy",
        "energy of second lowest unoccupied molecular orbital",
    ],
    "LUMOPlus2Energy": ["LUMO+2 energy"],
    "TotalEnergy": ["total energy"],
    "TotalEnthalpy": ["total enthalpy"],
    "Frequencies": ["frequencies"],
    "RotationalConstants": ["rotational constants"],
}

OCC_RESULT_KEYS = list(OCC_RESULT_LABELS.keys())