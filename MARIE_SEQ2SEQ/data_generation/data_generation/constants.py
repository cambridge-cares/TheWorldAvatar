from dataclasses import dataclass
from typing import List


def _add_space_and_lower(string: str):
    chars = []
    for i, char in enumerate(string):
        if char.isupper():
            if i > 0:
                chars.extend([" ", char.lower()])
            else:
                chars.append(char.lower())
        else:
            chars.append(char)
    return "".join(chars)


@dataclass
class ValueLabelsEntry:
    value: str
    labels: List[str]


PROPERTIES = [
    ValueLabelsEntry(value=p, labels=[_add_space_and_lower(p)])
    if not isinstance(p, ValueLabelsEntry)
    else p
    for p in [
        "AtomChiralCount",
        "AtomChiralDefCount",
        "AtomChiralUndefCount",
        "BondChiralCount",
        "BondChiralDefCount",
        "BondChiralUndefCount",
        "CanonicalizedCompound",
        "Charge",
        "CompoundComplexity",
        "CovalentUnitCount",
        "ExactMass",
        "HeavyAtomCount",
        "HydrogenBondAcceptorCount",
        "HydrogenBondDonorCount",
        "IsotopeAtomCount",
        "MolecularWeight",
        "MonoIsotopicWeight",
        "RotatableBondCount",
        "SubStructureKeysFingerprint",
        "TautomerCount",
        ValueLabelsEntry(value="XLogP3", labels=["XLogP3", "XLOGP3"]),
        "AutoignitionTemperature",
        ValueLabelsEntry(value="Caco2Permeability", labels=["CaCO2 permeability", "caco2 permeability"]),
        "CollisionCrossSection",
        "Hydrophobicity",
        "IonizationPotential",
        "IsoelectricPoint",
        ValueLabelsEntry(value="LogP", labels=["LogP", "logP", "Log P", "log P"]),
        ValueLabelsEntry(value="LogS", labels=["LogS", "logS", "Log S", "log S"]),
        "PolarSurfaceArea",
        "BoilingPoint",
        "Density",
        "DissociationConstants",
        "EnthalpyOfSublimation",
        "FlashPoint",
        "StandardEnthalpyOfFormation",
        "HeatOfCombustion",
        "HeatOfVaporization",
        "HenrysLawConstant",
        "MeltingPoint",
        "OpticalRotation",
        "Solubility",
        "SurfaceTension",
        "VaporDensity",
        "VaporPressure",
        "Viscosity",
    ]
]

IDENTIFIERS = [
    ValueLabelsEntry(
        value="ChebiID",
        labels=["ChEBI ID", "Chemical Entities of Biological Interest ID"],
    ),
    ValueLabelsEntry(
        value="CID", labels=["CID", "PubChem CID", "PubChem ID", "PubChem Compound ID"]
    ),
    ValueLabelsEntry(value="EmpiricalFormula", labels=["empirical formula"]),
    ValueLabelsEntry(
        value="InChI", labels=["InChI", "International Chemical Identifier"]
    ),
    ValueLabelsEntry(value="InChIKey", labels=["InChIKey", "hashed InChI"]),
    ValueLabelsEntry(value="IUPACName", labels=["IUPAC name"]),
    ValueLabelsEntry(value="MolecularFormula", labels=["molecular formula"]),
    ValueLabelsEntry(
        value="SMILES",
        labels=[
            "SMILES",
            "SMILES string",
            "simplified molecular-input line-entry system",
        ],
    ),
]
