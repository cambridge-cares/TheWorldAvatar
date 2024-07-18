import internal from 'stream'

export const OUnitCellLengthKey = {
  A: 'a',
  B: 'b',
  C: 'c',
} as const
export type UnitCellLengthKey =
  (typeof OUnitCellLengthKey)[keyof typeof OUnitCellLengthKey]

export const OUnitCellAngleKey = {
  ALPHA: 'alpha',
  BETA: 'beta',
  GAMMA: 'gamma',
}
export type UnitCellAngleKey =
  (typeof OUnitCellAngleKey)[keyof typeof OUnitCellAngleKey]

export const OScalarTopoPropKey = {
  ACCESSIBLE_AREA_PER_CELL: 'accessible-area-per-cell',
  ACCESSIBLE_AREA_PER_GRAM: 'accessible-area-per-gram',
  ACCESSIBLE_VOLUME: 'accessible-volume',
  ACCESSIBLE_VOLUME_PER_CELL: 'accessible-volume-per-cell',
  DENSITY: 'density',
  FRAMEWORK_DENSITY: 'framework-density',
  OCCUPIABLE_AREA_PER_CELL: 'occupiable-area-per-cell',
  OCCUPIABLE_AREA_PER_GRAM: 'occupiable-area-per-gram',
  OCCUPIABLE_VOLUME: 'occupiable-volume',
  OCCUPIABLE_VOLUME_PER_CELL: 'occupiable-volume-per-cell',
  SPECIFIC_ACCESSIBLE_AREA: 'specific-accessible-area',
  SPECIFIC_OCCUPIABLE_AREA: 'specific-accupiable-area',
  TOPOLOGICAL_DENSITY: 'topological-density',
}
export type ScalarTopoPropKey =
  (typeof OScalarTopoPropKey)[keyof typeof OScalarTopoPropKey]

export const SCALAR_TOPO_PROP_UNITS = {
  [OScalarTopoPropKey.ACCESSIBLE_AREA_PER_CELL]: 'Å²',
  [OScalarTopoPropKey.ACCESSIBLE_AREA_PER_GRAM]: 'm²/g',
  [OScalarTopoPropKey.ACCESSIBLE_VOLUME]: '%',
  [OScalarTopoPropKey.ACCESSIBLE_VOLUME_PER_CELL]: 'Å³',
  [OScalarTopoPropKey.DENSITY]: 'g/cm³',
  [OScalarTopoPropKey.FRAMEWORK_DENSITY]: 'nm⁻³',
  [OScalarTopoPropKey.OCCUPIABLE_AREA_PER_CELL]: 'Å²',
  [OScalarTopoPropKey.OCCUPIABLE_AREA_PER_GRAM]: 'm²/g',
  [OScalarTopoPropKey.OCCUPIABLE_VOLUME]: '%',
  [OScalarTopoPropKey.OCCUPIABLE_VOLUME_PER_CELL]: 'Å³',
  [OScalarTopoPropKey.SPECIFIC_ACCESSIBLE_AREA]: 'm²/cm³',
  [OScalarTopoPropKey.SPECIFIC_OCCUPIABLE_AREA]: 'm²/cm³',
}

export interface ZeoliteFrameworkBase extends RDFEntity {
  code: string
}

export interface ZeoliticMaterialBase extends RDFEntity {
  chemical_formula: string
}

export interface Quantity {
  unit: string
  value: number
}

export interface VectorComponent {
  index?: number
  label?: string
  value: number
}

export interface MeasureVector {
  unit: string
  vector_component: VectorComponent[]
}

export interface MatrixComponent {
  col_index: number
  row_index: number
  value: number
}

export interface MeasureMatrix {
  unit: string
  matrix_component: MatrixComponent[]
}

export interface AtomSite {
  label: string
  cart_pos: MeasureVector
  fract_pos: MeasureVector
}

export interface TileFaceNum {
  value: number
  tile_face: {
    face_code?: string
    edge_num: number
  }
}

export interface TileNum {
  value: number
  tile: {
    signature: string
    tile_code: string
    edge_num: number
    face_num: number
    vertex_num: number
    tile_face_number: TileFaceNum[]
  }
}

export interface XRDPeak {
  relative_intensity: number
  two_theta_position: number
  is_simulated: boolean
  miller_indices: MeasureVector
}

export interface CrystalInfo {
  atomic_structure: { atom_site: AtomSite[] }
  coord_transform: {
    transform_matrix_to_cart: MeasureMatrix
    transform_matrix_to_frac: MeasureMatrix
    transform_vector_to_cart: MeasureVector
    transform_vector_to_frac: MeasureVector
  }
  unit_cell: {
    lattice_system?: string
    space_group_symbol?: string
    symmetry_number?: string

    angles: MeasureVector
    lengths: MeasureVector
    vector_set: MeasureVector[]

    reciprocal_angles: MeasureVector
    reciprocal_lengths: MeasureVector
    reciprocal_vector_set: MeasureVector[]

    volumne: Quantity
  }
  tiled_structure?: {
    signature: string
    tile_num: TileNum[]
  }
  xrd_spectrum?: {
    peak: XRDPeak[]
  }
}

export interface TopologicalProperties {
  accessible_area_per_cell: Quantity
  accessible_area_per_gram: Quantity
  accessible_volume: Quantity
  accessible_volume_per_cell: Quantity

  occupiable_area_per_cell: Quantity
  occupiable_area_per_gram: Quantity
  occupiable_volume: Quantity
  occupiable_volume_per_cell: Quantity

  specific_accessible_area: Quantity
  specific_occupiable_area: Quantity

  density: Quantity
  framework_density: Quantity
  topo_density: {
    TD: number
    TD10: number
  }

  RDLS?: number
  ring_sizes: MeasureVector
  T_atom: {
    index: number
    name: string
    coord_seq?: MeasureVector
    vertex_symbol: {
      ring_size: string
      symbol_position: number
    }[]
  }[]
  secondary_bu: string[]
  composite_bu?: {
    cage: string[]
    T_cage: string[]
  }
  abc_seq?: string
}

export interface ZeoliteFramework extends ZeoliteFrameworkBase {
  crystal_information: CrystalInfo
  topo_props: TopologicalProperties
  material: ZeoliticMaterialBase[]
}
