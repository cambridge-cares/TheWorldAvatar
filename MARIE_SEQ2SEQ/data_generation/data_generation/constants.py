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
with open(os.path.join(RESOURCE_DIRPATH, 'chemical_classes.txt')) as f:
    CHEMICALCLASSES = [line.strip() for line in f.readlines()]
with open(os.path.join(RESOURCE_DIRPATH, 'uses.txt')) as f:
    USES = [line.strip() for line in f.readlines()]
