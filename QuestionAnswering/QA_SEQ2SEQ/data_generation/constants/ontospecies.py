from enum import Enum
import itertools

from constants.nl import PLURAL_ADJS


class OSSpeciesAttrKey(Enum):
    PROPERTY = "Property"
    IDENTIFIER = "Identifier"
    CHEMCLASS = "ChemicalClass"
    USE = "Use"


class OSPropertyKey(Enum):
    ATOM_CHIRAL_COUNT = "AtomChiralCount"
    ATOM_CHIRAL_DEF_COUNT = "AtomChiralDefCount"
    ATOM_CHIRAL_UNDEF_COUNT = "AtomChiralUndefCount"
    AUTOIGNITION_TEMPERATURE = "AutoignitionTemperature"
    BOILING_POINT = "BoilingPoint"
    BOND_CHIRAL_COUNT = "BondChiralCount"
    BOND_CHIRAL_DEF_COUNT = "BondChiralDefCount"
    BOND_CHIRAL_UNDEF_COUNT = "BondChiralUndefCount"
    CACO2_PERMEABILITY = "Caco2Permeability"
    # CANONICALIZED_COMPOUND = "CanonicalizedCompound"
    CHARGE = "Charge"
    COLLISION_CROSSSECTION = "CollisionCrossSection"
    COMPOUND_COMPLEXITY = "CompoundComplexity"
    COVALENT_UNIT_COUNT = "CovalentUnitCount"
    DENSITY = "Density"
    DISSOCIATION_CONSTANTS = "DissociationConstants"
    ENTHALPY_OF_SUBLIMATION = "EnthalpyofSublimation"
    EXACT_MASS = "ExactMass"
    FLASH_POINT = "FlashPoint"
    HEAT_CAPACITY = "HeatCapacity"
    HEAT_OF_COMBUSTION = "HeatofCombustion"
    HEAT_OF_VAPORIZATION = "HeatofVaporization"
    HEAVY_ATOM_COUNT = "HeavyAtomCount"
    HENRYS_LAW_CONSTANT = "HenrysLawConstant"
    # HIGHER_HEATING_VALUE = "HigherHeatingValue"
    HYDROGEN_BOND_ACCEPTOR_COUNT = "HydrogenBondAcceptorCount"
    HYDROGEN_BOND_DONOR_COUNT = "HydrogenBondDonorCount"
    HYDROPHOBICITY = "Hydrophobicity"
    IONIZATION_POTENTIAL = "IonizationPotential"
    ISOELECTRIC_POINT = "IsoelectricPoint"
    ISOTOPE_ATOM_COUNT = "IsotopeAtomCount"
    LOG_P = "LogP"
    LOG_S = "LogS"
    # LOWER_HEATING_VALUE = "LowerHeatingValue"
    MELTING_POINT = "MeltingPoint"
    MOLECULAR_WEIGHT = "MolecularWeight"
    MONOISOTOPIC_WEIGHT = "MonoIsotopicWeight"
    OPTICAL_ROTATION = "OpticalRotation"
    POLAR_SURFACE_AREA = "PolarSurfaceArea"
    ROTATABLE_BOND_COUNT = "RotatableBondCount"
    SOLUBILITY = "Solubility"
    # SUBSTRUCTURE_KEYS_FINGERPRINT = "SubStructureKeysFingerprint"
    SURFACE_TENSION = "SurfaceTension"
    TAUTOMERS_COUNT = "TautomersCount"
    VAPOR_DENSITY = "VaporDensity"
    VAPOR_PRESSURE = "VaporPressure"
    VICOSITY = "Viscosity"
    XLOGP3 = "XLogP3"


class OSIdentifierKey(Enum):
    CID = "CID"
    CHEBI_ID = "ChebiId"
    # EMPIRICAL_FORMULA = "EmpiricalFormula"
    IUPAC_NAME = "IUPACName"
    INCHI = "InChI"
    INCHI_KEY = "InChIKey"
    MOLECULAR_FORMULA = "MolecularFormula"
    SMILES = "SMILES"


ABSTRACT_PROPERTY_KEY = "PropertyName"
ABSTRACT_IDENTIFIER_KEY = "IdentifierName"

SPECIES_ABSTRACT_ATTRIBUTE_KEYS = [ABSTRACT_PROPERTY_KEY, ABSTRACT_IDENTIFIER_KEY]

OS_PROPERTY_LABELS = {
    OSPropertyKey.ATOM_CHIRAL_COUNT: ["atom chiral count", "atom stereocenter count"],
    OSPropertyKey.ATOM_CHIRAL_DEF_COUNT: [
        "atom chiral def count",
        "defined atom stereocenter count",
    ],
    OSPropertyKey.ATOM_CHIRAL_UNDEF_COUNT: [
        "atom chiral undef count",
        "undefined atom stereocenter count",
    ],
    OSPropertyKey.AUTOIGNITION_TEMPERATURE: ["autoignition temperature"],
    OSPropertyKey.BOILING_POINT: ["boiling point"],
    OSPropertyKey.BOND_CHIRAL_COUNT: ["bond chiral count", "bond stereocenter count"],
    OSPropertyKey.BOND_CHIRAL_DEF_COUNT: [
        "bond chiral def count",
        "defined bond stereocenter count",
    ],
    OSPropertyKey.BOND_CHIRAL_UNDEF_COUNT: [
        "bond chiral undef count",
        "undefined bond stereocenter count",
    ],
    OSPropertyKey.CACO2_PERMEABILITY: ["Caco-2 permeability"],
    # OSPropertyKey.CANONICALIZED_COMPOUND: ["is_canonicalized"],
    OSPropertyKey.CHARGE: ["charge"],
    OSPropertyKey.COMPOUND_COMPLEXITY: ["compound complexity"],
    OSPropertyKey.COLLISION_CROSSSECTION: ["collision cross-section"],
    OSPropertyKey.COVALENT_UNIT_COUNT: [
        "covalent unit count",
        "number of covalent units",
    ],
    OSPropertyKey.DENSITY: ["density"],
    OSPropertyKey.DISSOCIATION_CONSTANTS: ["dissociation constants"],
    OSPropertyKey.ENTHALPY_OF_SUBLIMATION: ["enthalpy of sublimation"],
    OSPropertyKey.EXACT_MASS: ["exact mass"],
    OSPropertyKey.FLASH_POINT: ["flash point"],
    OSPropertyKey.HEAT_CAPACITY: ["heat capacity"],
    OSPropertyKey.HEAT_OF_COMBUSTION: ["heat of combustion"],
    OSPropertyKey.HEAT_OF_VAPORIZATION: ["heat of vaporization"],
    OSPropertyKey.HEAVY_ATOM_COUNT: [
        "heavy atom count",
        "number of non-hydrogen atoms",
    ],
    OSPropertyKey.HENRYS_LAW_CONSTANT: ["Henry's law constant"],
    OSPropertyKey.HYDROGEN_BOND_ACCEPTOR_COUNT: [
        "hydrogen bond acceptor count",
        "number of hydrogen bond acceptors",
    ],
    OSPropertyKey.HYDROGEN_BOND_DONOR_COUNT: [
        "hydrogen bond donor count",
        "number of hydrogen bond donors",
    ],
    OSPropertyKey.HYDROPHOBICITY: ["hydrophobicity"],
    OSPropertyKey.IONIZATION_POTENTIAL: ["ionization potential"],
    OSPropertyKey.ISOELECTRIC_POINT: ["isoelectric point"],
    OSPropertyKey.ISOTOPE_ATOM_COUNT: ["isotope atom count"],
    OSPropertyKey.LOG_P: ["LogP", "logP", "Log P", "log P"],
    OSPropertyKey.LOG_S: ["LogS", "logS", "Log S", "log S"],
    OSPropertyKey.MELTING_POINT: ["melting point"],
    OSPropertyKey.MOLECULAR_WEIGHT: ["molecular weight", "molecular mass"],
    OSPropertyKey.MONOISOTOPIC_WEIGHT: [
        "mono isotopic weight",
        "monoisotopic weight",
        "monoisotopic mass",
    ],
    OSPropertyKey.OPTICAL_ROTATION: ["optical rotation"],
    OSPropertyKey.POLAR_SURFACE_AREA: ["polar surface area"],
    OSPropertyKey.ROTATABLE_BOND_COUNT: ["rotatable bond count"],
    OSPropertyKey.SOLUBILITY: ["solubility"],
    # OSPropertyKey.SUBSTRUCTURE_KEYS_FINGERPRINT: ["substructure key-based fingerprint"],
    OSPropertyKey.SURFACE_TENSION: ["surface tension"],
    OSPropertyKey.TAUTOMERS_COUNT: ["tautomer count", "number of tautomers"],
    OSPropertyKey.VAPOR_DENSITY: ["vapor density"],
    OSPropertyKey.VAPOR_PRESSURE: ["vapor pressure"],
    OSPropertyKey.VICOSITY: ["viscosity"],
    OSPropertyKey.XLOGP3: ["XLogP3", "XLOGP3"],
}

IDENTIFIER_LABELS = {
    OSIdentifierKey.CHEBI_ID: [
        "ChEBI ID",
        "Chemical Entities of Biological Interest ID",
    ],
    OSIdentifierKey.CID: ["CID", "PubChem CID", "PubChem ID", "PubChem Compound ID"],
    # OSIdentifierKey.EMPIRICAL_FORMULA: ["empirical formula"],
    OSIdentifierKey.INCHI: [
        "InChI",
        "International Chemical Identifier",
        "IUPAC International Chemical Identifier",
    ],
    OSIdentifierKey.INCHI_KEY: ["InChIKey", "hashed InChI"],
    OSIdentifierKey.IUPAC_NAME: ["IUPAC name"],
    OSIdentifierKey.MOLECULAR_FORMULA: ["molecular formula"],
    OSIdentifierKey.SMILES: [
        "SMILES",
        "SMILES string",
    ],
}

USE_LABELS = ["use", "application", "role"]
CHEMCLASS_LABELS = ["chemical class", "chemical classification"]

ABSTRACT_PROPERTY_LABELS = [
    " ".join(x) for x in itertools.product(PLURAL_ADJS, ["properties"])
]
ABSTRACT_IDENTIFIER_LABELS = [
    " ".join(x)
    for x in itertools.product(PLURAL_ADJS, ["identifiers", "labels", "names"])
]

KEY2LABELS = {
    **OS_PROPERTY_LABELS,
    **IDENTIFIER_LABELS,
    **{
        OSSpeciesAttrKey.USE: USE_LABELS,
        OSSpeciesAttrKey.CHEMCLASS: CHEMCLASS_LABELS,
        ABSTRACT_PROPERTY_KEY: ABSTRACT_PROPERTY_LABELS,
        ABSTRACT_IDENTIFIER_KEY: ABSTRACT_IDENTIFIER_LABELS,
    },
}
