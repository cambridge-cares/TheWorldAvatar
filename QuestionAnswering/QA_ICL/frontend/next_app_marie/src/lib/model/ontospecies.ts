export const CHEMICAL_CLASS_KEY = 'ChemicalClass'
export const USE_KEY = 'Use'
export const RETURN_FIELD_KEY = 'ReturnField'

export interface ChemicalClass extends RDFEntity {
  label: string
}

export interface Use extends RDFEntity {
  label: string
}

export interface OntospeciesIdentifier extends RDFEntity {
  value: string
}

export interface HasValueHasUnit extends RDFEntity {
  value: string
  unit?: string
}

export interface OntospeciesProperty extends HasValueHasUnit {
  reference_state?: HasValueHasUnit
  provenance?: string
}

export const OSpeciesPropertyKey = {
  ATOM_CHIRAL_COUNT: 'AtomChiralCount',
  ATOM_CHIRAL_DEF_COUNT: 'AtomChiralDefCount',
  ATOM_CHIRAL_UNDEF_COUNT: 'AtomChiralUndefCount',
  AUTOIGNITION_TEMPERATURE: 'AutoignitionTemperature',
  BOILING_POINT: 'BoilingPoint',
  BOND_CHIRAL_COUNT: 'BondChiralCount',
  BOND_CHIRAL_DEF_COUNT: 'BondChiralDefCount',
  BOND_CHIRAL_UNDEF_COUNT: 'BondChiralUndefCount',
  CACO2_PERMEABILITY: 'Caco2Permeability',
  CHARGE: 'Charge',
  COLLISION_CROSS_SECTION: 'CollisionCrossSection',
  COMPOUND_COMPLEXITY: 'CompoundComplexity',
  COVALENT_UNIT_COUNT: 'CovalentUnitCount',
  DENSITY: 'Density',
  DISSOCIATION_CONSTANTS: 'DissociationConstants',
  ENTHALPY_OF_SUBLIMATION: 'EnthalpyofSublimation',
  EXACT_MASS: 'ExactMass',
  FLASH_POINT: 'FlashPoint',
  HEAT_CAPACITY: 'HeatCapacity',
  HEAT_OF_COMBUSTION: 'HeatofCombustion',
  HEAT_OF_VAPORIZATION: 'HeatofVaporization',
  HEAVY_ATOM_COUNT: 'HeavyAtomCount',
  HENRYS_LAW_CONSTANT: 'HenrysLawConstant',
  HIGHER_HEATING_VALUE: 'HigherHeatingValue',
  HYDROGEN_BOND_ACCEPTOR_COUNT: 'HydrogenBondAcceptorCount',
  HYDROGEN_BOND_DONOR_COUNT: 'HydrogenBondDonorCount',
  HYDROPHOBICITY: 'Hydrophobicity',
  IONIZATION_POTENTIAL: 'IonizationPotential',
  ISOELECTRIC_POINT: 'IsoelectricPoint',
  ISOTOPE_ATOM_COUNT: 'IsotopeAtomCount',
  LOG_P: 'LogP',
  LOG_S: 'LogS',
  LOWER_HEATING_VALUE: 'LowerHeatingValue',
  MELTING_POINT: 'MeltingPoint',
  MOLECULAR_WEIGHT: 'MolecularWeight',
  MONO_ISOTOPIC_WEIGHT: 'MonoIsotopicWeight',
  OPTICAL_ROTATION: 'OpticalRotation',
  POLAR_SURFACE_AREA: 'PolarSurfaceArea',
  ROTATABLE_BOND_COUNT: 'RotatableBondCount',
  SOLUBILITY: 'Solubility',
  SURFACE_TENSION: 'SurfaceTension',
  TAUTOMERS_COUNT: 'TautomersCount',
  VAPOR_DENSITY: 'VaporDensity',
  VAPOR_PRESSURE: 'VaporPressure',
  VISCOSITY: 'Viscosity',
  XLOGP3: 'XLogP3',
} as const
export type SpeciesPropertyKey =
  (typeof OSpeciesPropertyKey)[keyof typeof OSpeciesPropertyKey]

export const OSpeciesIdentifierKey = {
  CID: 'CID',
  CHEBI_ID: 'ChebiId',
  IUPAC_NAME: 'IUPACName',
  INCHI: 'InChI',
  INCHI_KEY: 'InChIKey',
  MOLECULAR_FORMULA: 'MolecularFormula',
  SMILES: 'SMILES',
} as const
export type SpeciesIdentifierKey =
  (typeof OSpeciesIdentifierKey)[keyof typeof OSpeciesIdentifierKey]

export const SPECIES_IDENTIFIER_KEY_LABELS = {
  [OSpeciesIdentifierKey.CID]: 'CID',
  [OSpeciesIdentifierKey.CHEBI_ID]: 'ChEBI ID',
  [OSpeciesIdentifierKey.IUPAC_NAME]: 'IUPAC name',
  [OSpeciesIdentifierKey.INCHI]: 'InChI',
  [OSpeciesIdentifierKey.INCHI_KEY]: 'InChIKey',
  [OSpeciesIdentifierKey.MOLECULAR_FORMULA]: 'molecular formula',
  [OSpeciesIdentifierKey.SMILES]: 'SMILES string',
}

export interface SpeciesBase extends RDFEntity {
  label?: string
  IUPAC_name?: string
  InChI: string
}

export interface Species extends SpeciesBase {
  alt_labels: string[]
  chemical_classes: ChemicalClass[]
  uses: Use[]
  identifiers: { [key in SpeciesIdentifierKey]: OntospeciesIdentifier[] }
  properties: { [key in SpeciesPropertyKey]: OntospeciesProperty[] }
}
