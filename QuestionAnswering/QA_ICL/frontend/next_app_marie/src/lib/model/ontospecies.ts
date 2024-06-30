import { ComparisonOperator } from './comp-op'

export interface ChemicalClass {
  IRI: string
  label: string
}

export interface Use {
  IRI: string
  label: string
}

export const OSpeciesPropertyKey = {
  ATOM_CHIRAL_COUNT: 'atom-chiral-count',
  ATOM_CHIRAL_DEF_COUNT: 'atom-chiral-def-count',
  ATOM_CHIRAL_UNDEF_COUNT: 'atom-chiral-undef-count',
  AUTOIGNITION_TEMPERATURE: 'autoignition-temperature',
  BOILING_POINT: 'boiling-point',
  BOND_CHIRAL_COUNT: 'bond-chiral-count',
  BOND_CHIRAL_DEF_COUNT: 'bond-chiral-def-count',
  BOND_CHIRAL_UNDEF_COUNT: 'bond-chiral-undef-count',
  CACO2_PERMEABILITY: 'caco2-permeability',
  CHARGE: 'charge',
  COLLISION_CROSS_SECTION: 'collision-cross-section',
  COMPOUND_COMPLEXITY: 'compound-complexity',
  COVALENT_UNIT_COUNT: 'covalent-unit-count',
  DENSITY: 'density',
  DISSOCIATION_CONSTANTS: 'dissociation-constants',
  ENTHALPYOF_SUBLIMATION: 'enthalpyof-sublimation',
  EXACT_MASS: 'exact-mass',
  FLASH_POINT: 'flash-point',
  HEAT_CAPACITY: 'heat-capacity',
  HEATOF_COMBUSTION: 'heatof-combustion',
  HEATOF_VAPORIZATION: 'heatof-vaporization',
  HEAVY_ATOM_COUNT: 'heavy-atom-count',
  HENRYS_LAW_CONSTANT: 'henrys-law-constant',
  HIGHER_HEATING_VALUE: 'higher-heating-value',
  HYDROGEN_BOND_ACCEPTOR_COUNT: 'hydrogen-bond-acceptor-count',
  HYDROGEN_BOND_DONOR_COUNT: 'hydrogen-bond-donor-count',
  HYDROPHOBICITY: 'hydrophobicity',
  IONIZATION_POTENTIAL: 'ionization-potential',
  ISOELECTRIC_POINT: 'isoelectric-point',
  ISOTOPE_ATOM_COUNT: 'isotope-atom-count',
  LOG_P: 'log-p',
  LOG_S: 'log-s',
  LOWER_HEATING_VALUE: 'lower-heating-value',
  MELTING_POINT: 'melting-point',
  MOLECULAR_WEIGHT: 'molecular-weight',
  MONO_ISOTOPIC_WEIGHT: 'mono-isotopic-weight',
  OPTICAL_ROTATION: 'optical-rotation',
  POLAR_SURFACE_AREA: 'polar-surface-area',
  ROTATABLE_BOND_COUNT: 'rotatable-bond-count',
  SOLUBILITY: 'solubility',
  SURFACE_TENSION: 'surface-tension',
  TAUTOMERS_COUNT: 'tautomers-count',
  VAPOR_DENSITY: 'vapor-density',
  VAPOR_PRESSURE: 'vapor-pressure',
  VISCOSITY: 'viscosity',
  X_LOG_P3: 'x-log-p3',
} as const
export type SpeciesPropertyKey =
  (typeof OSpeciesPropertyKey)[keyof typeof OSpeciesPropertyKey]

export interface SpeciesRequest {
  chemicalClass: string[]
  use: string[]
  property: {
    [Property in keyof SpeciesPropertyKey]: [ComparisonOperator, number][]
  }
}
