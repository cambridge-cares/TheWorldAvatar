import {
  TiledStructure,
  ZeoliteFrameworkPartial,
} from '@/lib/model/ontozeolite'
import { ZeoliteFrameworkPlotAttrKey } from './model'

const extractTileFaceEdgeNums = (tiledStructure: TiledStructure) =>
  tiledStructure.TileNumber.flatMap(x =>
    x.Tile.TileFaceNumber.map(y => y.TileFace.EdgeNum)
  )

export function getZeoliteFrameworkAttrValue(
  { CrystalInformation, TopologicalProperties }: ZeoliteFrameworkPartial,
  key: ZeoliteFrameworkPlotAttrKey
) {
  switch (key) {
    case ZeoliteFrameworkPlotAttrKey.TD:
      return (
        TopologicalProperties && TopologicalProperties.TopologicalDensity?.TD
      )
    case ZeoliteFrameworkPlotAttrKey.TD10:
      return (
        TopologicalProperties && TopologicalProperties.TopologicalDensity?.TD10
      )
    case ZeoliteFrameworkPlotAttrKey.FRAMEWORK_DENSITY:
      return (
        TopologicalProperties && TopologicalProperties.FrameworkDensity?.value
      )
    case ZeoliteFrameworkPlotAttrKey.DENSITY:
      return TopologicalProperties && TopologicalProperties.Density?.value
    case ZeoliteFrameworkPlotAttrKey.OCCUPIABLE_VOLUME_PER_CELL:
      return (
        TopologicalProperties &&
        TopologicalProperties.OccupiableVolumePerCell?.value
      )
    case ZeoliteFrameworkPlotAttrKey.OCCUPIABLE_VOLUME:
      return (
        TopologicalProperties && TopologicalProperties.OccupiableVolume?.value
      )
    case ZeoliteFrameworkPlotAttrKey.ACCESSIBLE_VOLUME_PER_CELL:
      return (
        TopologicalProperties &&
        TopologicalProperties.AccessibleVolumePerCell?.value
      )
    case ZeoliteFrameworkPlotAttrKey.ACCESSIBLE_VOLUME:
      return (
        TopologicalProperties && TopologicalProperties.AccessibleVolume?.value
      )
    case ZeoliteFrameworkPlotAttrKey.OCCUPIABLE_AREA_PER_CELL:
      return (
        TopologicalProperties &&
        TopologicalProperties.OccupiableAreaPerCell?.value
      )
    case ZeoliteFrameworkPlotAttrKey.OCCUPIABLE_AREA_PER_GRAM:
      return (
        TopologicalProperties &&
        TopologicalProperties.OccupiableAreaPerGram?.value
      )
    case ZeoliteFrameworkPlotAttrKey.ACCESSIBLE_AREA_PER_CELL:
      return (
        TopologicalProperties &&
        TopologicalProperties.AccessibleAreaPerCell?.value
      )
    case ZeoliteFrameworkPlotAttrKey.ACCESSIBLE_AREA_PER_GRAM:
      return (
        TopologicalProperties &&
        TopologicalProperties.AccessibleAreaPerGram?.value
      )
    case ZeoliteFrameworkPlotAttrKey.SPECIFIC_OCCUPIABLE_AREA:
      return (
        TopologicalProperties &&
        TopologicalProperties.SpecificOccupiableArea?.value
      )
    case ZeoliteFrameworkPlotAttrKey.SPECIFIC_ACCESSIBLE_AREA:
      return (
        TopologicalProperties &&
        TopologicalProperties.SpecificAccessibleArea?.value
      )
    case ZeoliteFrameworkPlotAttrKey.INCLUDED_SPHERE_DIAMETER:
      return (
        TopologicalProperties &&
        TopologicalProperties.SphereDiameter?.component.find(
          x => x.label === 'included'
        )?.value
      )
    case ZeoliteFrameworkPlotAttrKey.A_PROPAGATING_SPHERE_DIAMETER:
      return (
        TopologicalProperties &&
        TopologicalProperties.SphereDiameter?.component.find(
          x => x.label === 'a'
        )?.value
      )
    case ZeoliteFrameworkPlotAttrKey.B_PROPAGATING_SPHERE_DIAMETER:
      return (
        TopologicalProperties &&
        TopologicalProperties.SphereDiameter?.component.find(
          x => x.label === 'b'
        )?.value
      )
    case ZeoliteFrameworkPlotAttrKey.C_PROPAGATING_SPHERE_DIAMETER:
      return (
        TopologicalProperties &&
        TopologicalProperties.SphereDiameter?.component.find(
          x => x.label === 'c'
        )?.value
      )
    case ZeoliteFrameworkPlotAttrKey.SMALLEST_RING_SIZE:
      return (
        TopologicalProperties &&
        TopologicalProperties.RingSizes?.component
          .map(x => x.value)
          .reduce(reduceMin, undefined)
      )
    case ZeoliteFrameworkPlotAttrKey.LARGEST_RING_SIZE:
      return (
        TopologicalProperties &&
        TopologicalProperties.RingSizes?.component
          .map(x => x.value)
          .reduce(reduceMax, undefined)
      )
    case ZeoliteFrameworkPlotAttrKey.SMALLEST_TILE_FACE_EDGE_NUM:
      return (
        CrystalInformation &&
        CrystalInformation.TiledStructure &&
        extractTileFaceEdgeNums(CrystalInformation.TiledStructure)?.reduce(
          reduceMin,
          undefined
        )
      )
    case ZeoliteFrameworkPlotAttrKey.LARGEST_TILE_FACE_EDGE_NUM:
      CrystalInformation &&
        CrystalInformation.TiledStructure &&
        extractTileFaceEdgeNums(CrystalInformation.TiledStructure)?.reduce(
          reduceMax,
          undefined
        )
  }
}
