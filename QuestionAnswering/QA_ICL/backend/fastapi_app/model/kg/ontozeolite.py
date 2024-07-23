from enum import Enum
from constants.namespace import OM2, ONTOCRYSTAL, ONTOZEOLITE
from model.rdf_orm import RDFEntity, RDFField


class Om2Quantity(RDFEntity):
    unit: str = RDFField(path=OM2.hasUnit)
    value: float = RDFField(path=OM2.hasNumericalValue)


class OntocrystalQuantity(Om2Quantity):
    pass


class OntocrystalVectorComponent(RDFEntity):
    index: int | None = RDFField(path=ONTOCRYSTAL.hasComponentIndex)
    label: str | None = RDFField(path=ONTOCRYSTAL.hasComponentLabel)
    value: float = RDFField(path=ONTOCRYSTAL.hasComponentValue)


class OntocrystalMeasureVector(RDFEntity):
    unit: str = RDFField(path=OM2.hasUnit)
    component: list[OntocrystalVectorComponent] = RDFField(
        path=ONTOCRYSTAL.hasVectorComponent
    )


class OntocrystalMatrixComponent(RDFEntity):
    col: int = RDFField(path=ONTOCRYSTAL.hasColumnIndex)
    row: int = RDFField(path=ONTOCRYSTAL.hasRowIndex)
    value: float = RDFField(path=ONTOCRYSTAL.hasComponentValue)


class OntocrystalMeasureMatrix(RDFEntity):
    unit: str = RDFField(path=OM2.hasUnit)
    component: list[OntocrystalMatrixComponent] = RDFField(
        path=ONTOCRYSTAL.hasMatrixComponent
    )


class OntozeoliteZeoliteFrameworkBase(RDFEntity):
    code: str = RDFField(path=ONTOZEOLITE.hasFrameworkCode)


class OntozeoliteZeoliticMaterialBase(RDFEntity):
    ChemicalFormula: str = RDFField(path=ONTOZEOLITE.hasChemicalFormula)


class OntocrystalAtomSite(RDFEntity):
    label: str = RDFField(path=ONTOCRYSTAL.hasAtomSiteLabel)
    CartesianPosition: OntocrystalMeasureVector = RDFField(
        path=ONTOCRYSTAL.hasCartesianPosition
    )
    FractionalPosition: OntocrystalMeasureVector = RDFField(
        path=ONTOCRYSTAL.hasFractionalPosition
    )


class OntocrystalAtomicStructure(RDFEntity):
    AtomSite: list[OntocrystalAtomSite] = RDFField(path=ONTOCRYSTAL.hasAtomSite)


class OntocrystalCoordinateTransformation(RDFEntity):
    TransformationMatrixToCartesian: OntocrystalMeasureMatrix = RDFField(
        path=ONTOCRYSTAL.hasTransformationMatrixToCartesian
    )
    TransformationMatrixToFractional: OntocrystalMeasureMatrix = RDFField(
        path=ONTOCRYSTAL.hasTransformationMatrixToFractional
    )
    TransformationVectorToCartesian: OntocrystalMeasureVector = RDFField(
        path=ONTOCRYSTAL.hasTransformationVectorToCartesian
    )
    TransformationVectorToFractional: OntocrystalMeasureVector = RDFField(
        path=ONTOCRYSTAL.hasTransformationVectorToFractional
    )


class OntocrystalUnitCell(RDFEntity):
    LatticeSystem: str | None = RDFField(path=ONTOCRYSTAL.hasLatticeSystem)
    SpaceGroupSymbol: str | None = RDFField(path=ONTOCRYSTAL.hasSpaceGroupSymbol)
    SymmetryNumber: int | None = RDFField(path=ONTOCRYSTAL.hasSymmetryNumber)

    Lengths: OntocrystalMeasureVector = RDFField(path=ONTOCRYSTAL.hasUnitCellLengths)
    Angles: OntocrystalMeasureVector = RDFField(path=ONTOCRYSTAL.hasUnitCellAngles)
    VectorSet: list[OntocrystalMeasureVector] = RDFField(
        path=ONTOCRYSTAL.hasUnitCellVectorSet
    )

    ReciprocalLengths: OntocrystalMeasureVector = RDFField(
        path=ONTOCRYSTAL.hasReciprocalUnitCellLengths
    )
    ReciprocalAngles: OntocrystalMeasureVector = RDFField(
        path=ONTOCRYSTAL.hasReciprocalUnitCellAngles
    )
    ReciprocalVectorSet: list[OntocrystalMeasureVector] = RDFField(
        path=ONTOCRYSTAL.hasUnitCellReciprocalVectorSet
    )
    
    Volume: Om2Quantity = RDFField(path=ONTOCRYSTAL.hasUnitCellVolume)


class OntocrystalTileFace(RDFEntity):
    FaceCode: str | None = RDFField(path=ONTOCRYSTAL.hasFaceCode)
    EdgeNum: int = RDFField(path=ONTOCRYSTAL.hasNumberOfEdges)


class OntocrystalTileFaceNumber(RDFEntity):
    value: int = RDFField(path=ONTOCRYSTAL.hasValue)
    TileFace: OntocrystalTileFace = RDFField(path=ONTOCRYSTAL.isNumberOfTileFaces)


class OntocrystalTile(RDFEntity):
    Signature: str = RDFField(path=ONTOCRYSTAL.hasTileSignature)
    TileCode: str = RDFField(path=ONTOCRYSTAL.hasTileCode)
    EdgeNum: int = RDFField(path=ONTOCRYSTAL.hasNumberOfEdges)
    FaceNum: int = RDFField(path=ONTOCRYSTAL.hasNumberOfFaces)
    VertexNum: int = RDFField(path=ONTOCRYSTAL.hasNumberOfVertices)
    TileFaceNumber: list[OntocrystalTileFaceNumber] = RDFField(
        path=ONTOCRYSTAL.hasTileFaceNumber
    )


class OntocrystalTileNumber(RDFEntity):
    value: int = RDFField(path=ONTOCRYSTAL.hasValue)
    Tile: OntocrystalTile = RDFField(path=ONTOCRYSTAL.isNumberOfTiles)


class OntocrystalTiledStructure(RDFEntity):
    Signature: str = RDFField(path=ONTOCRYSTAL.hasTileSignature)
    TileNumber: list[OntocrystalTileNumber] = RDFField(path=ONTOCRYSTAL.hasTileNumber)


class OntozeoliteZeoliticMaterial(OntozeoliteZeoliticMaterialBase):
    Framework: OntozeoliteZeoliteFrameworkBase = RDFField(
        path=~ONTOZEOLITE.hasZeoliticMaterial
    )


class OntocrystalXRDPeak(RDFEntity):
    RelativeIntensity: float = RDFField(path=ONTOCRYSTAL.hasRelativeIntensity)
    TwoThetaPosition: float = RDFField(path=ONTOCRYSTAL.hasTwoThetaPosition)
    isSimulated: bool = RDFField(path=ONTOCRYSTAL.isSimulated)
    MillerIndices: OntocrystalMeasureVector = RDFField(
        path=ONTOCRYSTAL.hasMillerIndices
    )


class OntocrystalXRDSpectrum(RDFEntity):
    Peak: list[OntocrystalXRDPeak] = RDFField(path=ONTOCRYSTAL.hasCharacteristicPeak)


class OntocrystalCrystalInfo(RDFEntity):
    AtomicStructure: OntocrystalAtomicStructure = RDFField(
        path=ONTOCRYSTAL.hasAtomicStructure
    )
    CoordinateTransformation: OntocrystalCoordinateTransformation = RDFField(
        path=ONTOCRYSTAL.hasCoordinateTransformation
    )
    UnitCell: OntocrystalUnitCell = RDFField(path=ONTOCRYSTAL.hasUnitCell)
    TiledStructure: OntocrystalTiledStructure | None = RDFField(
        path=ONTOCRYSTAL.hasTiledStructure
    )
    XRDSpectrum: OntocrystalXRDSpectrum | None = RDFField(
        path=ONTOCRYSTAL.hasXRDSpectrum
    )


class OntozeoliteVertexSymbol(RDFEntity):
    RingSize: str = RDFField(path=ONTOZEOLITE.hasRingSize)
    SymbolPosition: int = RDFField(path=ONTOZEOLITE.hasSymbolPosition)


class OntozeoliteTAtom(RDFEntity):
    index: int = RDFField(path=ONTOZEOLITE.hasTAtomIndex)
    name: str = RDFField(path=ONTOZEOLITE.hasTAtomName)

    CooridnateSequence: OntocrystalMeasureVector | None = RDFField(
        path=ONTOZEOLITE.hasCooridnateSequence
    )
    VertexSymbol: list[OntozeoliteVertexSymbol] = RDFField(
        path=ONTOZEOLITE.hasVertexSymbol
    )


class OntozeoliteTopoDensity(RDFEntity):
    TD: float | None = RDFField(path=ONTOZEOLITE.hasValueTD)
    TD10: float = RDFField(path=ONTOZEOLITE.hasValueTD10)


class OntozeoliteCompositeBU(RDFEntity):
    Cage: list[str] = RDFField(path=ONTOZEOLITE.hasCage)
    TCage: list[str] = RDFField(path=ONTOZEOLITE.hasTCage)


class TopologicalPropertyKey(str, Enum):
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

    RDLS = "RDLS"
    RING_SIZES = "RingSizes"
    SECONDARY_BU = "SecondaryBU"
    COMPOSITE_BU = "CompositeBU"
    SPHERE_DIAMETER = "SphereDiameter"
    T_ATOM = "TAtom"
    ABC_SEQUENCE = "ABCSequence"


class OntozeoliteTopoProps(RDFEntity):
    AccessibleAreaPerCell: OntocrystalQuantity = RDFField(
        path=ONTOZEOLITE.hasAccessibleAreaPerCell,
        alias=TopologicalPropertyKey.ACCESSIBLE_AREA_PER_CELL,
    )
    AccessibleAreaPerGram: OntocrystalQuantity = RDFField(
        path=ONTOZEOLITE.hasAccessibleAreaPerGram,
        alias=TopologicalPropertyKey.ACCESSIBLE_AREA_PER_GRAM,
    )
    AccessibleVolume: OntocrystalQuantity = RDFField(
        path=ONTOZEOLITE.hasAccessibleVolume,
        alias=TopologicalPropertyKey.ACCESSIBLE_VOLUME,
    )
    AccessibleVolumePerCell: OntocrystalQuantity = RDFField(
        path=ONTOZEOLITE.hasAccessibleVolumePerCell,
        alias=TopologicalPropertyKey.ACCESSIBLE_VOLUME_PER_CELL,
    )

    OccupiableAreaPerCell: OntocrystalQuantity = RDFField(
        path=ONTOZEOLITE.hasOccupiableAreaPerCell,
        alias=TopologicalPropertyKey.OCCUPIABLE_AREA_PER_CELL,
    )
    OccupiableAreaPerGram: OntocrystalQuantity = RDFField(
        path=ONTOZEOLITE.hasOccupiableAreaPerGram,
        alias=TopologicalPropertyKey.OCCUPIABLE_AREA_PER_GRAM,
    )
    OccupiableVolume: OntocrystalQuantity = RDFField(
        path=ONTOZEOLITE.hasOccupiableVolume,
        alias=TopologicalPropertyKey.OCCUPIABLE_VOLUME,
    )
    OccupiableVolumePerCell: OntocrystalQuantity = RDFField(
        path=ONTOZEOLITE.hasOccupiableVolumePerCell,
        alias=TopologicalPropertyKey.OCCUPIABLE_VOLUME_PER_CELL,
    )

    SpecificAccessibleArea: OntocrystalQuantity = RDFField(
        path=ONTOZEOLITE.hasSpecificAccessibleArea,
        alias=TopologicalPropertyKey.SPECIFIC_ACCESSIBLE_AREA,
    )
    SpecificOccupiableArea: OntocrystalQuantity = RDFField(
        path=ONTOZEOLITE.hasSpecificOccupiableArea,
        alias=TopologicalPropertyKey.SPECIFIC_OCCUPIABLE_AREA,
    )

    Density: OntocrystalQuantity = RDFField(path=ONTOZEOLITE.hasDensity)
    FrameworkDensity: OntocrystalQuantity = RDFField(
        path=ONTOZEOLITE.hasFrameworkDensity,
        alias=TopologicalPropertyKey.FRAMEWORK_DENSITY,
    )
    TopologicalDensity: OntozeoliteTopoDensity = RDFField(
        path=ONTOZEOLITE.hasTopologicalDensity,
        alias=TopologicalPropertyKey.TOPOLOGICAL_DENSITY,
    )

    SphereDiameter: OntocrystalMeasureVector = RDFField(
        path=ONTOZEOLITE.hasSphereDiameter, alias=TopologicalPropertyKey.SPHERE_DIAMETER
    )
    RingSizes: OntocrystalMeasureVector = RDFField(
        path=ONTOZEOLITE.hasRingSizes, alias=TopologicalPropertyKey.RING_SIZES
    )

    RDLS: float | None = RDFField(
        path=ONTOZEOLITE.hasRDLS, alias=TopologicalPropertyKey.RDLS
    )
    TAtom: list[OntozeoliteTAtom] = RDFField(
        path=ONTOZEOLITE.hasTAtom, alias=TopologicalPropertyKey.T_ATOM
    )
    SecondaryBU: list[str] = RDFField(
        path=ONTOZEOLITE.hasSecondaryBU, alias=TopologicalPropertyKey.SECONDARY_BU
    )
    CompositeBU: OntozeoliteCompositeBU | None = RDFField(
        path=ONTOZEOLITE.hasCompositeBU, alias=TopologicalPropertyKey.COMPOSITE_BU
    )
    ABCSequence: str | None = RDFField(
        path=ONTOZEOLITE.hasABCSequence, alias=TopologicalPropertyKey.ABC_SEQUENCE
    )


class OntozeoliteZeoliteFramework(OntozeoliteZeoliteFrameworkBase):
    CrystalInformation: OntocrystalCrystalInfo = RDFField(
        path=ONTOCRYSTAL.hasCrystalInformation
    )
    TopologicalProperties: OntozeoliteTopoProps = RDFField(
        path=ONTOZEOLITE.hasTopologicalProperties
    )
    ZeoliticMaterial: list[OntozeoliteZeoliticMaterialBase] = RDFField(
        path=ONTOZEOLITE.hasZeoliticMaterial
    )
