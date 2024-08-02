import os
import pathlib


PKG_ROOT_DIRPATH = pathlib.Path(__file__).parent.resolve()
RESOURCE_DIRPATH = os.path.join(PKG_ROOT_DIRPATH, "resources")

PROPERTY_NAMES = [
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
    "XLogP3",
    "AutoignitionTemperature",
    "Caco2Permeability",
    "CollisionCrossSection",
    "Hydrophobicity",
    "IonizationPotential",
    "IsoelectricPoint",
    "LogP",
    "LogS",
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

IDENTIFIER_NAMES = [
    "ChebiID",
    "CID",
    "EmpiricalFormula",
    "InChI",
    "InChIKey",
    "IUPACName",
    "MolecularFormula",
    "SMILES",
]

with open(os.path.join(RESOURCE_DIRPATH, "species.txt")) as f:
    SPECIES = [line.strip() for line in f.readlines()]
with open(os.path.join(RESOURCE_DIRPATH, "chemical_classes.txt")) as f:
    CHEMICALCLASSES = [line.strip() for line in f.readlines()]
with open(os.path.join(RESOURCE_DIRPATH, "uses.txt")) as f:
    USES = [line.strip() for line in f.readlines()]


PROPERTY_LABELS = {
    "AtomChiralCount": ["atom chiral count", "atom stereocenter count"],
    "AtomChiralDefCount": [
        "atom chiral def count",
        "defined atom stereocenter count",
    ],
    "AtomChiralUndefCount": [
        "atom chiral undef count",
        "undefined atom stereocenter count",
    ],
    "BondChiralCount": ["bond chiral count", "bond stereocenter count"],
    "BondChiralDefCount": [
        "bond chiral def count",
        "defined bond stereocenter count",
    ],
    "BondChiralUndefCount": [
        "bond chiral undef count",
        "undefined bond stereocenter count",
    ],
    "CanonicalizedCompound": ["canonicalized compound"],
    "Charge": ["charge"],
    "CompoundComplexity": ["compound complexity"],
    "CovalentUnitCount": ["covalent unit count", "number of covalent units"],
    "ExactMass": ["exact mass"],
    "HeavyAtomCount": ["heavy atom count", "number of non-hydrogen atoms"],
    "HydrogenBondAcceptorCount": [
        "hydrogen bond acceptor count",
        "number of hydrogen bond acceptors",
    ],
    "HydrogenBondDonorCount": [
        "hydrogen bond donor count",
        "number of hydrogen bond donors",
    ],
    "IsotopeAtomCount": ["isotope atom count"],
    "MolecularWeight": ["molecular weight", "molecular mass"],
    "MonoIsotopicWeight": [
        "mono isotopic weight",
        "monoisotopic weight",
        "monoisotopic mass",
    ],
    "RotatableBondCount": ["rotatable bond count"],
    "SubStructureKeysFingerprint": ["substructure key-based fingerprint"],
    "TautomerCount": ["tautomer count", "number of tautomers"],
    "XLogP3": ["XLogP3", "XLOGP3"],
    "AutoignitionTemperature": ["autoignition temperature"],
    "Caco2Permeability": ["Caco-2 permeability"],
    "CollisionCrossSection": ["collision cross section"],
    "Hydrophobicity": ["hydrophobicity"],
    "IonizationPotential": ["ionization potential"],
    "IsoelectricPoint": ["isoelectric point"],
    "LogP": ["LogP", "logP", "Log P", "log P"],
    "LogS": ["LogS", "logS", "Log S", "log S"],
    "PolarSurfaceArea": ["polar surface area"],
    "BoilingPoint": ["boiling point"],
    "Density": ["density"],
    "DissociationConstants": ["dissociation constants"],
    "EnthalpyOfSublimation": ["enthalpy of sublimation"],
    "FlashPoint": ["flash point"],
    "StandardEnthalpyOfFormation": ["standard enthalpy of formation"],
    "HeatOfCombustion": ["heat of combustion"],
    "HeatOfVaporization": ["heat of vaporization"],
    "HenrysLawConstant": ["Henry's law constant"],
    "MeltingPoint": ["melting point"],
    "OpticalRotation": ["optical rotation"],
    "Solubility": ["solubility"],
    "SurfaceTension": ["surface tension"],
    "VaporDensity": ["vapor density"],
    "VaporPressure": ["vapor pressure"],
    "Viscosity": ["viscosity"],
}

IDENTIFIER_LABELS = {
    "ChebiID": ["ChEBI ID", "Chemical Entities of Biological Interest ID"],
    "CID": ["CID", "PubChem CID", "PubChem ID", "PubChem Compound ID"],
    "EmpiricalFormula": ["empirical formula"],
    "InChI": [
        "InChI",
        "International Chemical Identifier",
        "IUPAC International Chemical Identifier",
    ],
    "InChIKey": ["InChIKey", "hashed InChI"],
    "IUPACName": ["IUPAC name"],
    "MolecularFormula": ["molecular formula"],
    "SMILES": [
        "SMILES",
        "SMILES string",
    ],
}
