from enum import Enum


class OZCrystalInfoAttrKey(Enum):
    ATOMIC_STRUCTURE = "AtomicStructure"
    UNIT_CELL = "UnitCell"
    TILED_STRUCTURE = "TiledStructure"
    COORD_TRANSFORM = "CoordinateTransformation"


class OZZeoTopoAttrKey(Enum):
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


ZEOTOPO_SCALAR_KEYS = [
    OZZeoTopoAttrKey.ACCESSIBLE_AREA_PER_CELL,
    OZZeoTopoAttrKey.ACCESSIBLE_AREA_PER_GRAM,
    OZZeoTopoAttrKey.ACCESSIBLE_VOLUME,
    OZZeoTopoAttrKey.ACCESSIBLE_VOLUME_PER_CELL,
    OZZeoTopoAttrKey.OCCUPIABLE_AREA_PER_CELL,
    OZZeoTopoAttrKey.OCCUPIABLE_AREA_PER_GRAM,
    OZZeoTopoAttrKey.OCCUPIABLE_VOLUME,
    OZZeoTopoAttrKey.OCCUPIABLE_VOLUME_PER_CELL,
    OZZeoTopoAttrKey.SPECIFIC_ACCESSIBLE_AREA,
    OZZeoTopoAttrKey.SPECIFIC_OCCUPIABLE_AREA,
    OZZeoTopoAttrKey.DENSITY,
    OZZeoTopoAttrKey.FRAMEWORK_DENSITY,
]


class OZFrameworkAttrKey(Enum):
    CRYSTAL_INFO = 0
    TOPO_ATTR = 1


class OZMaterialAttrKey(Enum):
    FRAMEWORK = 0
    CRYSTAL_INFO = 1
    GUEST_COMPOUND = 2


CRYSTAL_ATTR_LABELS = {
    OZCrystalInfoAttrKey.ATOMIC_STRUCTURE: ["atomic structure"],
    OZCrystalInfoAttrKey.UNIT_CELL: [
        "unit cell",
        "unit cell information",
        "unit cell dimensions",
    ],
    OZCrystalInfoAttrKey.TILED_STRUCTURE: ["tiled structure", "tiling information"],
}

ZEOTOPO_ATTR_LABELS = {
    OZZeoTopoAttrKey.ACCESSIBLE_AREA_PER_CELL: ["accessible area per cell"],
    OZZeoTopoAttrKey.ACCESSIBLE_AREA_PER_GRAM: ["accessible area per gram"],
    OZZeoTopoAttrKey.ACCESSIBLE_VOLUME: ["accessible volume"],
    OZZeoTopoAttrKey.ACCESSIBLE_VOLUME_PER_CELL: ["accessible volume per cell"],
    OZZeoTopoAttrKey.OCCUPIABLE_AREA_PER_CELL: ["occupiable area per cell"],
    OZZeoTopoAttrKey.OCCUPIABLE_AREA_PER_GRAM: ["occupiable area per gram"],
    OZZeoTopoAttrKey.OCCUPIABLE_VOLUME: ["occupiable volume"],
    OZZeoTopoAttrKey.OCCUPIABLE_VOLUME_PER_CELL: ["occupiable volume per cell"],
    OZZeoTopoAttrKey.SPECIFIC_ACCESSIBLE_AREA: ["specific accessible area"],
    OZZeoTopoAttrKey.SPECIFIC_OCCUPIABLE_AREA: ["specific occupiable area"],
    OZZeoTopoAttrKey.DENSITY: ["density"],
    OZZeoTopoAttrKey.FRAMEWORK_DENSITY: ["framework density"],
    OZZeoTopoAttrKey.TOPOLOGICAL_DENSITY: ["topological density"],
    OZZeoTopoAttrKey.RING_SIZES: ["ring sizes"],
    OZZeoTopoAttrKey.SECONDARY_BU: ["secondary building block"],
    OZZeoTopoAttrKey.COMPOSITE_BU: ["composite building block"],
    OZZeoTopoAttrKey.SPHERE_DIAMETER: ["sphere diameter"],
    OZZeoTopoAttrKey.T_ATOM: ["T atom"],
}

ZEOMATERIAL_ATTR_LABELS = {
    OZMaterialAttrKey.GUEST_COMPOUND: ["guest compound", "guest species", "OSDA"]
}