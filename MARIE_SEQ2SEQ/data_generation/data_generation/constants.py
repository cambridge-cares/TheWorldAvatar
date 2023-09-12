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
        ValueLabelsEntry(
            value="AtomChiralCount",
            labels=["atom chiral count", "atom stereocenter count"],
        ),
        ValueLabelsEntry(
            value="AtomChiralDefCount",
            labels=["atom chiral def count", "defined atom stereocenter count"],
        ),
        ValueLabelsEntry(
            value="AtomChiralUndefCount",
            labels=["atom chiral undef count", "undefined atom stereocenter count"],
        ),
        ValueLabelsEntry(
            value="BondChiralCount",
            labels=["bond chiral count", "bond stereocenter count"],
        ),
        ValueLabelsEntry(
            value="BondChiralDefCount",
            labels=["bond chiral def count", "defined bond stereocenter count"],
        ),
        ValueLabelsEntry(
            value="BondChiralUndefCount",
            labels=["bond chiral undef count", "undefined bond stereocenter count"],
        ),
        "CanonicalizedCompound",
        "Charge",
        "CompoundComplexity",
        ValueLabelsEntry(
            value="CovalentUnitCount",
            labels=["covalent unit count", "number of covalent units"],
        ),
        "ExactMass",
        ValueLabelsEntry(
            value="HeavyAtomCount",
            labels=["heavy atom count", "number of non-hydrogen atoms"],
        ),
        ValueLabelsEntry(
            value="HydrogenBondAcceptorCount",
            labels=[
                "hydrogen bond acceptor count",
                "number of hydrogen bond acceptors",
            ],
        ),
        ValueLabelsEntry(
            value="HydrogenBondDonorCount",
            labels=["hydrogen bond donor count", "number of hydrogen bond donors"],
        ),
        "IsotopeAtomCount",
        ValueLabelsEntry(
            value="MolecularWeight", labels=["molecular weight", "molecular mass"]
        ),
        ValueLabelsEntry(
            value="MonoIsotopicWeight",
            labels=["mono isotopic weight", "monoisotopic weight", "monoisotopic mass"],
        ),
        "RotatableBondCount",
        "SubStructureKeysFingerprint",
        ValueLabelsEntry(
            value="TautomerCount", labels=["tautomer count", "number of tautomers"]
        ),
        ValueLabelsEntry(value="XLogP3", labels=["XLogP3", "XLOGP3"]),
        "AutoignitionTemperature",
        ValueLabelsEntry(value="Caco2Permeability", labels=["Caco-2 permeability"]),
        "CollisionCrossSection",
        "Hydrophobicity",
        "IonizationPotential",
        ValueLabelsEntry(value="IsoelectricPoint", labels=["isoelectric point", "IEP"]),
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
        ValueLabelsEntry(value="HenrysLawConstant", labels=["Henry's law constant"]),
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
        value="InChI",
        labels=[
            "InChI",
            "International Chemical Identifier",
            "IUPAC International Chemical Identifier",
        ],
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
