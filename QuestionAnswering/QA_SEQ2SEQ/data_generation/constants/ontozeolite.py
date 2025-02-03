from enum import Enum


class OZCrystalInfoAttrKey(str, Enum):
    ATOMIC_STRUCTURE = "AtomicStructure"
    UNIT_CELL = "UnitCell"
    TILED_STRUCTURE = "TiledStructure"
    COORD_TRANSFORM = "CoordinateTransformation"


CRYSTAL_SCALAR_KEYS = [
    OZCrystalInfoAttrKey.UNIT_CELL,
    OZCrystalInfoAttrKey.TILED_STRUCTURE,
]


class OZZeoTopoAttrKey(str, Enum):
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
    FRAMEWORK_COMPONENTS = 0
    CRYSTAL_INFO = 1
    TOPO_ATTR = 2
    MATERIALS = 3
    GUEST_SPECIES = 4


class OZMaterialAttrKey(Enum):
    FRAMEWORK = 0
    CRYSTAL_INFO = 1
    FRAMEWORK_COMPONENTS = 2
    GUEST_SPECIES = 3


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
    OZMaterialAttrKey.FRAMEWORK_COMPONENTS: [
        "building elements",
        "framework building elements",
        "framework components",
    ],
    OZMaterialAttrKey.GUEST_SPECIES: [
        "guest",
        "guest compound",
        "guest species",
        "incorporated species",
        "OSDA",
    ],
}

ZEOMATERIAL_PRED_LABELS = {
    OZMaterialAttrKey.FRAMEWORK_COMPONENTS: [
        "is built by",
        "is built by elements being",
        "is made up of",
        "is made up of elements being",
        "has building elements being",
        "has framework building elements being",
        "has framework components being",
    ],
    OZMaterialAttrKey.GUEST_SPECIES: ["incorporates", "has guest species", "has guest"],
}
