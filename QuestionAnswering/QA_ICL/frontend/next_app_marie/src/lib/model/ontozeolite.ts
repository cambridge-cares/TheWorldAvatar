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

export interface ZeoliteFrameworkBase {
  IRI: string
  code: string
}
