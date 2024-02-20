from enum import Enum


class OZZeoFwAttrKey(Enum):
    # Crystal
    ATOMIC_STRUCTURE = "AtomicStructure"
    UNIT_CELL = "UnitCell"
    TILED_STRUCTURE = "TiledStructure"
    COORD_TRANSFORM = "CoordinateTransformation"
    # Zeolite
    ACCESSIBLE_AREA_PER_CELL = "AccessibleAreaPerCell"
    ACCESSIBLE_AREA_PER_GRAM = "AccessibleAreaPerGram"
    ACCESSIBLE_VOLUME = "AccessibleVolume"
    ACCESSIBLE_VOLUME_PER_CELL = "AccessibleVolumePerCell"
    OCCUPIABLE_AREA_PER_CELL = "OccupiableAreaPerCell"
    OCCUPIABLE_AREA_PER_GRAM = "OccupiableAreaPerGram"
    OCCUPIABLE_VOLUME = "OccupiableVolume"
    OCCUPIABLE_VOLUME_PER_CELL = "OccupiableVolumePerCell"
    SPECIFIC_ACCESSIBLE_AREA = "SpecificAccessibleArea"
    SPECIFIC_OCCUPIABLE_AREA = "SpecificOccupiableArea"
    DENSITY = "Density"
    FRAMEWORK_DENSITY = "FrameworkDensity"
    TOPOLOGICAL_DENSITY = "TopologicalDensity"
    RING_SIZES = "RingSizes"
    SECONDARY_BU = "SecondaryBU"
    COMPOSITE_BU = "CompositeBU"
    SPHERE_DIAMETER = "SphereDiameter"
    T_ATOM = "TAtom"


CRYSTAL_ATTRS = set(
    [
        OZZeoFwAttrKey.ATOMIC_STRUCTURE,
        OZZeoFwAttrKey.UNIT_CELL,
        OZZeoFwAttrKey.TILED_STRUCTURE,
        OZZeoFwAttrKey.COORD_TRANSFORM,
    ]
)

ZEOLITE_ATTR_LABELS = {
    OZZeoFwAttrKey.ATOMIC_STRUCTURE: ["atomic structure"],
    OZZeoFwAttrKey.UNIT_CELL: ["unit cell", "unit cell information", "unit cell dimensions"],
    OZZeoFwAttrKey.TILED_STRUCTURE: ["tiled structure", "tiling information"],
    OZZeoFwAttrKey.ACCESSIBLE_AREA_PER_CELL: ["accessible area per cell"],
    OZZeoFwAttrKey.ACCESSIBLE_AREA_PER_GRAM: ["accessible area per gram"],
    OZZeoFwAttrKey.ACCESSIBLE_VOLUME: ["accessible volume"],
    OZZeoFwAttrKey.ACCESSIBLE_VOLUME_PER_CELL: ["accessible volume per cell"],
    OZZeoFwAttrKey.OCCUPIABLE_AREA_PER_CELL: ["occupiable area per cell"],
    OZZeoFwAttrKey.OCCUPIABLE_AREA_PER_GRAM: ["occupiable area per gram"],
    OZZeoFwAttrKey.OCCUPIABLE_VOLUME: ["occupiable volume"],
    OZZeoFwAttrKey.OCCUPIABLE_VOLUME_PER_CELL: ["occupiable volume per cell"],
    OZZeoFwAttrKey.SPECIFIC_ACCESSIBLE_AREA: ["specific accessible area"],
    OZZeoFwAttrKey.SPECIFIC_OCCUPIABLE_AREA: ["specific occupiable area"],
    OZZeoFwAttrKey.DENSITY: ["density"],
    OZZeoFwAttrKey.FRAMEWORK_DENSITY: ["framework density"],
    OZZeoFwAttrKey.TOPOLOGICAL_DENSITY: ["topological density"],
    OZZeoFwAttrKey.RING_SIZES: ["ring sizes"],
    OZZeoFwAttrKey.SECONDARY_BU: ["secondary building block"],
    OZZeoFwAttrKey.COMPOSITE_BU: ["composite building block"],
    OZZeoFwAttrKey.SPHERE_DIAMETER: ["sphere diameter"],
    OZZeoFwAttrKey.T_ATOM: ["T atom"]
}