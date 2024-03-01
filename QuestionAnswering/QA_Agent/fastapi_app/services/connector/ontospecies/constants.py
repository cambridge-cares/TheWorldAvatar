from enum import Enum
from typing import Union


class SpeciesIdentifierAttrKey(Enum):
    CID = "CID"
    CHEBI_ID = "ChebiId"
    IUPAC_NAME = "IUPACName"
    INCHI = "InChI"
    INCHI_KEY = "InChIKey"
    MOLECULAR_FORMULA = "MolecularFormula"
    SMILES = "SMILES"


class SpeciesChemicalClassAttrKey(Enum):
    CHEMICAL_CLASS = "ChemicalClass"


class SpeciesUseAttrKey(Enum):
    USE = "Use"


class SpeciesPropertyAttrKey(Enum):
    ATOM_CHIRAL_COUNT = "AtomChiralCount"
    ATOM_CHIRAL_DEF_COUNT = "AtomChiralDefCount"
    ATOM_CHIRAL_UNDEF_COUNT = "AtomChiralUndefCount"
    ATOMIC_BOND = "AtomicBond"
    BOILING_POINT = "BoilingPoint"
    BOND_CHIRAL_COUNT = "BondChiralCount"
    BOND_CHIRAL_DEF_COUNT = "BondChiralDefCount"
    BOND_CHIRAL_UNDEF_COUNT = "BondChiralUndefCount"
    CANONICALIZED_COMPOUND = "CanonicalizedCompound"
    CHARGE = "Charge"
    COMPOUND_COMPLEXITY = "CompoundComplexity"
    COVALENT_UNIT_COUNT = "CovalentUnitCount"
    DENSITY = "Density"
    EXACT_MASS = "ExactMass"
    HEAVY_ATOM_COUNT = "HeavyAtomCount"
    HYDROGEN_BOND_ACCEPTOR_COUNT = "HydrogenBondAcceptorCount"
    HYDROGEN_BOND_DONOR_COUNT = "HydrogenBondDonorCount"
    ISOTOPE_ATOM_COUNT = "IsotopeAtomCount"
    LOG_P = "LogP"
    MELTING_POINT = "MeltingPoint"
    MOLECULAR_WEIGHT = "MolecularWeight"
    MONO_ISOTOPIC_WEIGHT = "MonoIsotopicWeight"
    POLAR_SURFACE_AREA = "PolarSurfaceArea"
    ROTATABLE_BOND_COUNT = "RotatableBondCount"
    SOLUBILITY = "Solubility"
    SUBSTRUCTURE_KEYS_FINGERPRINT = "SubStructureKeysFingerprint"
    TAUTOMERS_COUNT = "TautomersCount"
    XLOGP3 = "XLogP3"


SpeciesAttrKey = Union[
    SpeciesIdentifierAttrKey,
    SpeciesChemicalClassAttrKey,
    SpeciesUseAttrKey,
    SpeciesPropertyAttrKey,
]
