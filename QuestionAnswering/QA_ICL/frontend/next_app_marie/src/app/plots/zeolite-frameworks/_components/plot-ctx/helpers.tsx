import {
  TiledStructure,
  ZeoliteFrameworkPartial,
} from '@/lib/model/ontozeolite'
import { ZeoFrameworkPlotAttrKey } from './model'

const extractTileFaceEdgeNums = (tiledStructure: TiledStructure) =>
  tiledStructure.TileNumber.flatMap(x =>
    x.Tile.TileFaceNumber.map(y => y.TileFace.EdgeNum)
  )

export function getZeoliteFrameworkAttrValue(
  { CrystalInformation, TopologicalProperties }: ZeoliteFrameworkPartial,
  key: ZeoFrameworkPlotAttrKey
) {
  switch (key) {
    case ZeoFrameworkPlotAttrKey.TD:
      return (
        TopologicalProperties && TopologicalProperties.TopologicalDensity?.TD
      )
    case ZeoFrameworkPlotAttrKey.TD10:
      return (
        TopologicalProperties && TopologicalProperties.TopologicalDensity?.TD10
      )
    case ZeoFrameworkPlotAttrKey.FRAMEWORK_DENSITY:
      return (
        TopologicalProperties && TopologicalProperties.FrameworkDensity?.value
      )
    case ZeoFrameworkPlotAttrKey.DENSITY:
      return TopologicalProperties && TopologicalProperties.Density?.value
    case ZeoFrameworkPlotAttrKey.OCCUPIABLE_VOLUME_PER_CELL:
      return (
        TopologicalProperties &&
        TopologicalProperties.OccupiableVolumePerCell?.value
      )
    case ZeoFrameworkPlotAttrKey.OCCUPIABLE_VOLUME:
      return (
        TopologicalProperties && TopologicalProperties.OccupiableVolume?.value
      )
    case ZeoFrameworkPlotAttrKey.ACCESSIBLE_VOLUME_PER_CELL:
      return (
        TopologicalProperties &&
        TopologicalProperties.AccessibleVolumePerCell?.value
      )
    case ZeoFrameworkPlotAttrKey.ACCESSIBLE_VOLUME:
      return (
        TopologicalProperties && TopologicalProperties.AccessibleVolume?.value
      )
    case ZeoFrameworkPlotAttrKey.OCCUPIABLE_AREA_PER_CELL:
      return (
        TopologicalProperties &&
        TopologicalProperties.OccupiableAreaPerCell?.value
      )
    case ZeoFrameworkPlotAttrKey.OCCUPIABLE_AREA_PER_GRAM:
      return (
        TopologicalProperties &&
        TopologicalProperties.OccupiableAreaPerGram?.value
      )
    case ZeoFrameworkPlotAttrKey.ACCESSIBLE_AREA_PER_CELL:
      return (
        TopologicalProperties &&
        TopologicalProperties.AccessibleAreaPerCell?.value
      )
    case ZeoFrameworkPlotAttrKey.ACCESSIBLE_AREA_PER_GRAM:
      return (
        TopologicalProperties &&
        TopologicalProperties.AccessibleAreaPerGram?.value
      )
    case ZeoFrameworkPlotAttrKey.SPECIFIC_OCCUPIABLE_AREA:
      return (
        TopologicalProperties &&
        TopologicalProperties.SpecificOccupiableArea?.value
      )
    case ZeoFrameworkPlotAttrKey.SPECIFIC_ACCESSIBLE_AREA:
      return (
        TopologicalProperties &&
        TopologicalProperties.SpecificAccessibleArea?.value
      )
    case ZeoFrameworkPlotAttrKey.INCLUDED_SPHERE_DIAMETER:
      return (
        TopologicalProperties &&
        TopologicalProperties.SphereDiameter?.component.find(
          x => x.label === 'included'
        )?.value
      )
    case ZeoFrameworkPlotAttrKey.A_PROPAGATING_SPHERE_DIAMETER:
      return (
        TopologicalProperties &&
        TopologicalProperties.SphereDiameter?.component.find(
          x => x.label === 'a'
        )?.value
      )
    case ZeoFrameworkPlotAttrKey.B_PROPAGATING_SPHERE_DIAMETER:
      return (
        TopologicalProperties &&
        TopologicalProperties.SphereDiameter?.component.find(
          x => x.label === 'b'
        )?.value
      )
    case ZeoFrameworkPlotAttrKey.C_PROPAGATING_SPHERE_DIAMETER:
      return (
        TopologicalProperties &&
        TopologicalProperties.SphereDiameter?.component.find(
          x => x.label === 'c'
        )?.value
      )
    case ZeoFrameworkPlotAttrKey.SMALLEST_RING_SIZE:
      return (
        TopologicalProperties &&
        TopologicalProperties.RingSizes?.component
          .map(x => x.value)
          .reduce(reduceMin, undefined)
      )
    case ZeoFrameworkPlotAttrKey.LARGEST_RING_SIZE:
      return (
        TopologicalProperties &&
        TopologicalProperties.RingSizes?.component
          .map(x => x.value)
          .reduce(reduceMax, undefined)
      )
    case ZeoFrameworkPlotAttrKey.SMALLEST_TILE_FACE_EDGE_NUM:
      return (
        CrystalInformation &&
        CrystalInformation.TiledStructure &&
        extractTileFaceEdgeNums(CrystalInformation.TiledStructure)?.reduce(
          reduceMin,
          undefined
        )
      )
    case ZeoFrameworkPlotAttrKey.LARGEST_TILE_FACE_EDGE_NUM:
      CrystalInformation &&
        CrystalInformation.TiledStructure &&
        extractTileFaceEdgeNums(CrystalInformation.TiledStructure)?.reduce(
          reduceMax,
          undefined
        )
  }
}
