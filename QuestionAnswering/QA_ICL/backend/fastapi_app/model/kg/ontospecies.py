from datetime import date
from enum import Enum

from pydantic import Field, create_model
from rdflib import SKOS
from rdflib.namespace import RDFS, RDF

from constants.namespace import GC, ONTOSPECIES
from model.rdf_ogm import RDFEntity, RDFField


class PeriodictableElement(RDFEntity):
    symbol: str = RDFField(path=ONTOSPECIES.hasElementSymbol / ONTOSPECIES.value)
    name: str = RDFField(path=ONTOSPECIES.hasElementName / ONTOSPECIES.value)


class OntospeciesHasValueHasUnit(RDFEntity):
    value: str = RDFField(path=ONTOSPECIES.value)
    unit: str | None = RDFField(path=(ONTOSPECIES.unit / RDFS.label) | ONTOSPECIES.unit)


class OntospeciesProperty(OntospeciesHasValueHasUnit):
    referenceState: OntospeciesHasValueHasUnit | None = RDFField(
        path=ONTOSPECIES.hasReferenceState
    )
    provenance: list[str] | None = RDFField(path=ONTOSPECIES.hasProvenance / RDFS.label)
    dateOfAccess: str | None = RDFField(path=ONTOSPECIES.dateOfAccess)
    originalData: str | None = RDFField(path=ONTOSPECIES.originalDataString)
    isRecomended: bool | None = RDFField(path=ONTOSPECIES.isRecomended)
    

class OntospeciesDissociationConstant(OntospeciesProperty):
    comment: str | None = RDFField(path=RDFS.comment)
    method: str | None = RDFField(path=ONTOSPECIES.hasMethod / RDFS.label)
    assessment: str | None = RDFField(path=ONTOSPECIES.hasAssessment)
    relatedAcidityLabel: str | None = RDFField(path=ONTOSPECIES.relatedAcidityLabel)
    dissociationConstantType: str = RDFField(path=RDF.type)
    index: str | None = RDFField(path=ONTOSPECIES.hasIndex / ONTOSPECIES.value)


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
    label: str | None = RDFField(path=RDFS.label | (ONTOSPECIES.hasIdentifier / ONTOSPECIES.value))
    IUPACName: str | None = RDFField(path=ONTOSPECIES.hasIUPACName / ONTOSPECIES.value)
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
    DISSOCIATION_CONSTANT = "DissociationConstant"
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


class SpeciesAttrKey(str, Enum):
    ALT_LABEL = "altLabel"
    CHEMICAL_CLASS = "ChemicalClass"
    USE = "Use"
    IDENTIFIER = "Identifier"
    PROPERTY = "Property"


class OntospeciesSpecies(OntospeciesSpeciesBase):
    altLabel: list[str] = RDFField(path=SKOS.altLabel, alias=SpeciesAttrKey.ALT_LABEL)

    ChemicalClass: list[OntospeciesChemicalClass] = RDFField(
        path=ONTOSPECIES.hasChemicalClass, alias=SpeciesAttrKey.CHEMICAL_CLASS
    )
    Use: list[OntospeciesUse] = RDFField(
        path=ONTOSPECIES.hasUse, alias=SpeciesAttrKey.USE
    )

    Identifier: dict[SpeciesIdentifierKey, list[OntospeciesIdentifier]]
    Property: dict[SpeciesPropertyKey, list[OntospeciesProperty]]


OntospeciesSpeciesPartial = create_model(
    "OntospeciesSpeciesPartial",
    **{
        field: (info.annotation | None, Field(..., alias=info.alias))
        for field, info in OntospeciesSpecies.model_fields.items()
    }
)
