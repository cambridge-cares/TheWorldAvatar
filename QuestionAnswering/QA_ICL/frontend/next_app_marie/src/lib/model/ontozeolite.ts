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
} as const
export type UnitCellAngleKey =
  (typeof OUnitCellAngleKey)[keyof typeof OUnitCellAngleKey]

export const OTopoPropKey = {
  ACCESSIBLE_AREA_PER_CELL: 'AccessibleAreaPerCell',
  ACCESSIBLE_AREA_PER_GRAM: 'AccessibleAreaPerGram',
  ACCESSIBLE_VOLUME: 'AccessibleVolume',
  ACCESSIBLE_VOLUME_PER_CELL: 'AccessibleVolumePerCell',

  OCCUPIABLE_AREA_PER_CELL: 'OccupiableAreaPerCell',
  OCCUPIABLE_AREA_PER_GRAM: 'OccupiableAreaPerGram',
  OCCUPIABLE_VOLUME: 'OccupiableVolume',
  OCCUPIABLE_VOLUME_PER_CELL: 'OccupiableVolumePerCell',

  SPECIFIC_ACCESSIBLE_AREA: 'SpecificAccessibleArea',
  SPECIFIC_OCCUPIABLE_AREA: 'SpecificOccupiableArea',

  DENSITY: 'Density',
  FRAMEWORK_DENSITY: 'FrameworkDensity',
  TOPOLOGICAL_DENSITY: 'TopologicalDensity',

  RDLS: 'RDLS',
  RING_SIZES: 'RingSizes',
  SECONDARY_BU: 'SecondaryBU',
  COMPOSITE_BU: 'CompositeBU',
  SPHERE_DIAMETER: 'SphereDiameter',
  T_ATOM: 'TAtom',
  ABC_SEQUENCE: 'ABCSequence',
} as const
export type TopoPropKey = (typeof OTopoPropKey)[keyof typeof OTopoPropKey]

export const SCALAR_TOPO_PROP_KEYS = [
  OTopoPropKey.ACCESSIBLE_AREA_PER_CELL,
  OTopoPropKey.ACCESSIBLE_AREA_PER_GRAM,
  OTopoPropKey.ACCESSIBLE_VOLUME,
  OTopoPropKey.ACCESSIBLE_VOLUME_PER_CELL,
  OTopoPropKey.OCCUPIABLE_AREA_PER_CELL,
  OTopoPropKey.OCCUPIABLE_AREA_PER_GRAM,
  OTopoPropKey.OCCUPIABLE_VOLUME,
  OTopoPropKey.OCCUPIABLE_VOLUME_PER_CELL,
  OTopoPropKey.SPECIFIC_ACCESSIBLE_AREA,
  OTopoPropKey.SPECIFIC_OCCUPIABLE_AREA,
  OTopoPropKey.DENSITY,
  OTopoPropKey.FRAMEWORK_DENSITY,
] as const
export type ScalarTopoPropKey = (typeof SCALAR_TOPO_PROP_KEYS)[number]

export const TOPO_PROP_UNITS = {
  [OTopoPropKey.ACCESSIBLE_AREA_PER_CELL]: 'Å²',
  [OTopoPropKey.ACCESSIBLE_AREA_PER_GRAM]: 'm²/g',
  [OTopoPropKey.ACCESSIBLE_VOLUME]: '%',
  [OTopoPropKey.ACCESSIBLE_VOLUME_PER_CELL]: 'Å³',
  [OTopoPropKey.DENSITY]: 'g/cm³',
  [OTopoPropKey.FRAMEWORK_DENSITY]: 'nm⁻³',
  [OTopoPropKey.OCCUPIABLE_AREA_PER_CELL]: 'Å²',
  [OTopoPropKey.OCCUPIABLE_AREA_PER_GRAM]: 'm²/g',
  [OTopoPropKey.OCCUPIABLE_VOLUME]: '%',
  [OTopoPropKey.OCCUPIABLE_VOLUME_PER_CELL]: 'Å³',
  [OTopoPropKey.SPECIFIC_ACCESSIBLE_AREA]: 'm²/cm³',
  [OTopoPropKey.SPECIFIC_OCCUPIABLE_AREA]: 'm²/cm³',
}

export interface ZeoliteFrameworkBase extends RDFEntity {
  code: string
}

export interface ZeoliticMaterialBase extends RDFEntity {
  ChemicalFormula: string
}

export interface Quantity {
  unit?: string
  value: number
}

export interface VectorComponent {
  index?: number
  label?: string
  value: number
}

export interface MeasureVector {
  unit?: string
  component: VectorComponent[]
}

export interface MatrixComponent {
  col: number
  row: number
  value: number
}

export interface MeasureMatrix {
  unit: string
  component: MatrixComponent[]
}

export interface AtomSite {
  label: string
  CartesianPosition: MeasureVector
  FractionalPosition: MeasureVector
}

export interface TileFaceNum {
  value: number
  TileFace: {
    FaceCode?: string
    EdgeNum: number
  }
}

export interface TileNum {
  value: number
  Tile: {
    Signature: string
    TileCode: string
    EdgeNum: number
    FaceNum: number
    VertexNum: number
    TileFaceNumber: TileFaceNum[]
  }
}

export const XRD_PEAK_KEY = 'XRDPeak'
export const UNIT_CELL_KEY_PREFIX = 'UnitCell-'

export interface XRDPeak {
  RelativeIntensity: number
  TwoThetaPosition: number
  isSimulated: boolean
  MillerIndices: MeasureVector
}

export interface TiledStructure {
  Signature: string
  TileNumber: TileNum[]
}

export interface CoordinateTransformation {
  TransformationMatrixToCartesian?: MeasureMatrix
  TransformationMatrixToFractional?: MeasureMatrix
  TransformationVectorToCartesian?: MeasureVector
  TransformationVectorToFractional?: MeasureVector
}

export interface CrystalInfo {
  AtomicStructure: { AtomSite: AtomSite[] }
  CoordinateTransformation: CoordinateTransformation
  UnitCell: {
    LatticeSystem?: string
    SpaceGroupSymbol?: string
    SymmetryNumber?: string

    Lengths: MeasureVector
    Angles: MeasureVector
    VectorSet: MeasureVector[]

    ReciprocalLengths: MeasureVector
    ReciprocalAngles: MeasureVector
    ReciprocalVectorSet: MeasureVector[]

    Volume: Quantity
  }
  TiledStructure?: TiledStructure
  XRDSpectrum?: {
    Peak: XRDPeak[]
  }
}

export interface TAtom {
  index: number
  name: string
  CooridnateSequence?: MeasureVector
  VertexSymbol: {
    RingCount?: number
    RingSize: number
    SymbolPosition: number
  }[]
}

export interface TopologicalProperties {
  AccessibleAreaPerCell: Quantity
  AccessibleAreaPerGram: Quantity
  AccessibleVolume: Quantity
  AccessibleVolumePerCell: Quantity

  OccupiableAreaPerCell: Quantity
  OccupiableAreaPerGram: Quantity
  OccupiableVolume: Quantity
  OccupiableVolumePerCell: Quantity

  SpecificAccessibleArea: Quantity
  SpecificOccupiableArea: Quantity

  Density: Quantity
  FrameworkDensity: Quantity
  TopologicalDensity: {
    TD?: number
    TD10: number
  }

  SphereDiameter: MeasureVector
  RingSizes: MeasureVector

  RDLS?: number
  TAtom: TAtom[]
  SecondaryBU: string[]
  CompositeBU?: {
    Cage: string[]
    TCage: string[]
  }
  ABCSequence?: string
}

export interface ZeoliteFramework extends ZeoliteFrameworkBase {
  CrystalInformation: CrystalInfo
  TopologicalProperties: TopologicalProperties
  ZeoliticMaterial: ZeoliticMaterialBase[]
}

export interface ZeoliteFrameworkPartial extends ZeoliteFrameworkBase {
  CrystalInformation?: Partial<CrystalInfo>
  TopologicalProperties: Partial<TopologicalProperties>
  ZeoliticMaterial?: ZeoliteFrameworkBase[]
}

export interface Journal extends RDFEntity {
  title: string
}

export interface Person extends RDFEntity {
  family_name: string
  firstName: string
}

export interface AcademicArticle extends RDFEntity {
  AuthorIndex: {
    value: number
    author: Person
  }[]
  title?: string
  doi?: string
  Url?: string
}

export interface ZeoliticMaterial extends ZeoliticMaterialBase {
  name: string
  framework: ZeoliteFrameworkBase
  CrystalInformation?: CrystalInfo
  Citation?: AcademicArticle
}
