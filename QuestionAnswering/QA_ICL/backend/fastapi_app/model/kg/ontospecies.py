from enum import Enum

from rdflib import SKOS
from rdflib.namespace import RDFS

from constants.namespace import GC, ONTOSPECIES
from model.rdf_orm import RDFEntity, RDFField


class PeriodictableElement(RDFEntity):
    symbol: str = RDFField(path=ONTOSPECIES.hasElementSymbol / ONTOSPECIES.value)
    name: str = RDFField(path=ONTOSPECIES.hasElementName / ONTOSPECIES.value)


class OntospeciesHasValueHasUnit(RDFEntity):
    value: str = RDFField(path=ONTOSPECIES.value)
    unit: str | None = RDFField(default=None, path=ONTOSPECIES.unit / RDFS.label)


class OntospeciesProperty(OntospeciesHasValueHasUnit):
    reference_state: OntospeciesHasValueHasUnit | None = RDFField(
        default=None, path=ONTOSPECIES.hasReferenceState
    )
    provenance: str | None = RDFField(
        default=None, path=ONTOSPECIES.hasProvenance / RDFS.label
    )


class GcAtom(RDFEntity):
    element: PeriodictableElement = RDFField(path=GC.isElement)
    x: OntospeciesHasValueHasUnit = RDFField(path=ONTOSPECIES.hasXCoordinate)
    y: OntospeciesHasValueHasUnit = RDFField(path=ONTOSPECIES.hasYCoordinate)
    z: OntospeciesHasValueHasUnit = RDFField(path=ONTOSPECIES.hasZCoordinate)


class OntospeciesIdentifier(RDFEntity):
    value: str = RDFField(path=ONTOSPECIES.value)


class OntospeciesHasLabel(RDFEntity):
    label: str = RDFField(path=RDFS.label)


class OntospeciesChemicalClass(RDFEntity):
    label: str = RDFField(path=RDFS.label)


class OntospeciesUse(RDFEntity):
    label: str = RDFField(path=RDFS.label)


class OntospeciesSpeciesBase(RDFEntity):
    label: str = RDFField(path=RDFS.label)
    IUPAC_name: str | None = RDFField(
        default=None, path=ONTOSPECIES.hasIUPACName / ONTOSPECIES.value
    )
    InChI: str = RDFField(path=ONTOSPECIES.hasInChI / ONTOSPECIES.value)


class SpeciesPropertyKey(str, Enum):
    ATOM_CHIRAL_COUNT = "AtomChiralCount"
    ATOM_CHIRAL_DEF_COUNT = "AtomChiralDefCount"
    ATOM_CHIRAL_UNDEF_COUNT = "AtomChiralUndefCount"
    AUTOIGNITION_TEMPERATURE = "AutoignitionTemperature"
    BOILING_POINT = "BoilingPoint"
    BOND_CHIRAL_COUNT = "BondChiralCount"
    BOND_CHIRAL_DEF_COUNT = "BondChiralDefCount"
    BOND_CHIRAL_UNDEF_COUNT = "BondChiralUndefCount"
    CACO2_PERMEABILITY = "Caco2Permeability"
    CHARGE = "Charge"
    COLLISION_CROSS_SECTION = "CollisionCrossSection"
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
    HIGHER_HEATING_VALUE = "HigherHeatingValue"
    HYDROGEN_BOND_ACCEPTOR_COUNT = "HydrogenBondAcceptorCount"
    HYDROGEN_BOND_DONOR_COUNT = "HydrogenBondDonorCount"
    HYDROPHOBICITY = "Hydrophobicity"
    IONIZATION_POTENTIAL = "IonizationPotential"
    ISOELECTRIC_POINT = "IsoelectricPoint"
    ISOTOPE_ATOM_COUNT = "IsotopeAtomCount"
    LOG_P = "LogP"
    LOG_S = "LogS"
    LOWER_HEATING_VALUE = "LowerHeatingValue"
    MELTING_POINT = "MeltingPoint"
    MOLECULAR_WEIGHT = "MolecularWeight"
    MONO_ISOTOPIC_WEIGHT = "MonoIsotopicWeight"
    OPTICAL_ROTATION = "OpticalRotation"
    POLAR_SURFACE_AREA = "PolarSurfaceArea"
    ROTATABLE_BOND_COUNT = "RotatableBondCount"
    SOLUBILITY = "Solubility"
    SURFACE_TENSION = "SurfaceTension"
    TAUTOMERS_COUNT = "TautomersCount"
    VAPOR_DENSITY = "VaporDensity"
    VAPOR_PRESSURE = "VaporPressure"
    VISCOSITY = "Viscosity"
    XLOGP3 = "XLogP3"


class SpeciesIdentifierKey(str, Enum):
    CID = "CID"
    CHEBI_ID = "ChebiId"
    IUPAC_NAME = "IUPACName"
    INCHI = "InChI"
    INCHI_KEY = "InChIKey"
    MOLECULAR_FORMULA = "MolecularFormula"
    SMILES = "SMILES"


class OntospeciesSpecies(OntospeciesSpeciesBase):
    alt_label: list[str] = RDFField(path=SKOS.altLabel)

    chemical_classes: list[OntospeciesChemicalClass] = RDFField(
        path=ONTOSPECIES.hasChemicalClass
    )
    uses: list[OntospeciesUse] = RDFField(path=ONTOSPECIES.hasUse)

    identifiers: dict[SpeciesIdentifierKey, list[OntospeciesIdentifier]]
    properties: dict[SpeciesPropertyKey, list[OntospeciesProperty]]
