from constants.namespace import OM2, ONTOCRYSTAL, ONTOZEOLITE
from model.rdf_orm import RDFEntity, RDFField


class Om2Quantity(RDFEntity):
    unit: str = RDFField(path=OM2.hasUnit)
    value: float = RDFField(path=OM2.hasNumericalValue)


class OntocrystalQuantity(Om2Quantity):
    pass


class OntocrystalVectorComponent(RDFEntity):
    index: int | None = RDFField(default=None, path=ONTOCRYSTAL.hasComponentIndex)
    label: str | None = RDFField(default=None, path=ONTOCRYSTAL.hasComponentLabel)
    value: float = RDFField(path=ONTOCRYSTAL.hasComponentValue)


class OntocrystalMeasureVector(RDFEntity):
    unit: str = RDFField(path=OM2.hasUnit)
    vector_component: list[OntocrystalVectorComponent] = RDFField(
        path=ONTOCRYSTAL.hasVectorComponent
    )


class OntocrystalMatrixComponent(RDFEntity):
    column_index: int = RDFField(path=ONTOCRYSTAL.hasColumnIndex)
    row_index: int = RDFField(path=ONTOCRYSTAL.hasRowIndex)
    value: float = RDFField(path=ONTOCRYSTAL.hasComponentValue)


class OntocrystalMeasureMatrix(RDFEntity):
    unit: str = RDFField(path=OM2.hasUnit)
    matrix_component: list[OntocrystalMatrixComponent] = RDFField(
        path=ONTOCRYSTAL.hasMatrixComponent
    )


class OntozeoliteZeoliteFrameworkBase(RDFEntity):
    code: str = RDFField(path=ONTOZEOLITE.hasFrameworkCode)


class OntozeoliteZeoliticMaterialBase(RDFEntity):
    chemical_formula: str = RDFField(path=ONTOZEOLITE.hasChemicalFormula)


class OntocrystalAtomSite(RDFEntity):
    label: str = RDFField(path=ONTOCRYSTAL.hasAtomSiteLabel)
    cart_pos: OntocrystalMeasureVector = RDFField(path=ONTOCRYSTAL.hasCartesianPosition)
    fract_post: OntocrystalMeasureVector = RDFField(
        path=ONTOCRYSTAL.hasFractionalPosition
    )


class OntocrystalAtomicStructure(RDFEntity):
    atom_site: list[OntocrystalAtomSite] = RDFField(path=ONTOCRYSTAL.hasAtomSite)


class OntocrystalCoordinateTransformation(RDFEntity):
    transform_matrix_to_cart: OntocrystalMeasureMatrix = RDFField(
        path=ONTOCRYSTAL.hasTransformationMatrixToCartesian
    )
    transform_matrix_to_frac: OntocrystalMeasureMatrix = RDFField(
        path=ONTOCRYSTAL.hasTransformationMatrixToFractional
    )
    transform_vector_to_cart: OntocrystalMeasureVector = RDFField(
        path=ONTOCRYSTAL.hasTransformationVectorToCartesian
    )
    transform_vector_to_frac: OntocrystalMeasureVector = RDFField(
        path=ONTOCRYSTAL.hasTransformationVectorToCartesian
    )


class OntocrystalUnitCell(RDFEntity):
    lattice_system: str | None = RDFField(
        default=None, path=ONTOCRYSTAL.hasLatticeSystem
    )
    space_group_symbol: str | None = RDFField(
        default=None, path=ONTOCRYSTAL.hasSpaceGroupSymbol
    )
    symmetry_number: int | None = RDFField(
        default=None, path=ONTOCRYSTAL.hasSymmetryNumber
    )
    angles: OntocrystalMeasureVector = RDFField(path=ONTOCRYSTAL.hasUnitCellAngles)
    reciprocal_angles: OntocrystalMeasureVector = RDFField(
        path=ONTOCRYSTAL.hasReciprocalUnitCellAngles
    )
    lengths: OntocrystalMeasureVector = RDFField(path=ONTOCRYSTAL.hasUnitCellLengths)
    reciprocal_lengths: OntocrystalMeasureVector = RDFField(
        path=ONTOCRYSTAL.hasReciprocalUnitCellLengths
    )
    vector_set: list[OntocrystalMeasureVector] = RDFField(
        path=ONTOCRYSTAL.hasUnitCellVectorSet
    )
    volume: Om2Quantity = RDFField(path=ONTOCRYSTAL.hasUnitCellVolume)


class OntocrystalTileFace(RDFEntity):
    face_code: str | None = RDFField(default=None, path=ONTOCRYSTAL.hasFaceCode)
    edge_num: int = RDFField(path=ONTOCRYSTAL.hasNumberOfEdges)


class OntocrystalTileFaceNumber(RDFEntity):
    value: int = RDFField(path=ONTOCRYSTAL.hasValue)
    tile_face: OntocrystalTileFace = RDFField(path=ONTOCRYSTAL.isNumberOfTileFaces)


class OntocrystalTile(RDFEntity):
    edge_num: int = RDFField(path=ONTOCRYSTAL.hasNumberOfEdges)
    face_num: int = RDFField(path=ONTOCRYSTAL.hasNumberOfFaces)
    vertex_num: int = RDFField(path=ONTOCRYSTAL.hasNumberOfVertices)
    tile_code: str = RDFField(path=ONTOCRYSTAL.hasTileCode)
    signature: str = RDFField(path=ONTOCRYSTAL.hasTileSignature)
    tile_face_number: list[OntocrystalTileFaceNumber] = RDFField(
        path=ONTOCRYSTAL.hasTileFaceNumber
    )


class OntocrystalTileNumber(RDFEntity):
    value: int = RDFField(path=ONTOCRYSTAL.hasValue)
    tile: OntocrystalTile = RDFField(path=ONTOCRYSTAL.isNumberOfTiles)


class OntocrystalTiledStructure(RDFEntity):
    signature: str = RDFField(path=ONTOCRYSTAL.hasTileSignature)
    tile_num: list[OntocrystalTileNumber] = RDFField(path=ONTOCRYSTAL.hasTileNumber)


class OntozeoliteZeoliteFramework(OntozeoliteZeoliteFrameworkBase):
    zeolitic_material: list[OntozeoliteZeoliticMaterialBase] = RDFField(
        path=ONTOZEOLITE.hasZeoliticMaterial
    )


class OntozeoliteZeoliticMaterial(OntozeoliteZeoliticMaterialBase):
    framework: OntozeoliteZeoliteFrameworkBase = RDFField(
        path=~ONTOZEOLITE.hasZeoliticMaterial
    )


class OntocrystalUnitCell(RDFEntity):
    lattice_system: str = RDFField(path=ONTOCRYSTAL.hasLatticeSystem)
    space_group_symbol: str = RDFField(path=ONTOCRYSTAL.hasSpaceGroupSymbol)
    symmetry_number: str = RDFField(path=ONTOCRYSTAL.hasSymmetryNumber)

    reciprocal_angles: OntocrystalMeasureVector = RDFField(
        path=ONTOCRYSTAL.hasReciprocalUnitCellAngles
    )
    reciprocal_lengths: OntocrystalMeasureVector = RDFField(
        path=ONTOCRYSTAL.hasReciprocalUnitCellLengths
    )
    reciprocal_vector_set: list[OntocrystalMeasureVector] = RDFField(
        path=ONTOCRYSTAL.hasReciprocalUnitCellVectorSet / ONTOCRYSTAL.hasUnitCellVector
    )

    angles: OntocrystalMeasureVector = RDFField(path=ONTOCRYSTAL.hasUnitCellAngles)
    lengths: OntocrystalMeasureVector = RDFField(path=ONTOCRYSTAL.hasUnitCellLengths)
    vector_set: list[OntocrystalMeasureVector] = RDFField(
        path=ONTOCRYSTAL.hasUnitCellVectorSet / ONTOCRYSTAL.hasUnitCellVector
    )

    volume: OntocrystalQuantity = RDFField(path=ONTOCRYSTAL.hasUnitCellVolume)


class OntocrystalXRDPeak(RDFEntity):
    relative_intensity: float = RDFField(path=ONTOCRYSTAL.hasRelativeIntensity)
    two_theta_position: float = RDFField(path=ONTOCRYSTAL.hasTwoThetaPosition)
    is_simulated: bool = RDFField(path=ONTOCRYSTAL.isSimulated)
    miller_indices: OntocrystalMeasureVector = RDFField(
        path=ONTOCRYSTAL.hasMillerIndices
    )


class OntocrystalXRDSpectrum(RDFEntity):
    peak: list[OntocrystalXRDPeak] = RDFField(path=ONTOCRYSTAL.hasCharacteristicPeak)


class OntocrystalCrystalInfo(RDFEntity):
    atomic_structure: OntocrystalAtomicStructure = RDFField(
        path=ONTOCRYSTAL.hasAtomicStructure
    )
    coordinate_transform: OntocrystalCoordinateTransformation = RDFField(
        path=ONTOCRYSTAL.hasCoordinateTransformation
    )
    unit_cell: OntocrystalUnitCell = RDFField(path=ONTOCRYSTAL.hasUnitCell)
    tiled_structure: OntocrystalTiledStructure | None = RDFField(
        default=None, path=ONTOCRYSTAL.hasTiledStructure
    )
    xrd_spectrum: OntocrystalXRDSpectrum | None = RDFField(
        default=None, path=ONTOCRYSTAL.hasXRDSpectrum
    )


class OntozeoliteVertexSymbol(RDFEntity):
    ring_size: str = RDFField(path=ONTOZEOLITE.hasRingSize)
    symbol_position: int = RDFField(path=ONTOZEOLITE.hasSymbolPosition)


class OntozeoliteTAtom(RDFEntity):
    index: int = RDFField(path=ONTOZEOLITE.hasTAtomIndex)
    name: str = RDFField(path=ONTOZEOLITE.hasTAtomName)

    coord_seq: OntocrystalMeasureVector | None = RDFField(
        default=None, path=ONTOZEOLITE.hasCooridnateSequence
    )
    vertex_symbol: list[OntozeoliteVertexSymbol] = RDFField(
        path=ONTOZEOLITE.hasVertexSymbol
    )


class OntozeoliteTopoDensity(RDFEntity):
    TD: float = RDFField(path=ONTOZEOLITE.hasValueTD)
    TD10: float = RDFField(path=ONTOZEOLITE.hasValueTD10)


class OntozeoliteCompositeBU(RDFEntity):
    cage: list[str] = RDFField(path=ONTOZEOLITE.hasCage)
    T_cage: list[str] = RDFField(path=ONTOZEOLITE.hasTCage)


class OntozeoliteTopoProps(RDFEntity):
    accessible_area_per_cell: OntocrystalQuantity = RDFField(
        path=ONTOZEOLITE.hasAccessibleAreaPerCell
    )
    accessible_area_per_gram: OntocrystalQuantity = RDFField(
        path=ONTOZEOLITE.hasAccessibleAreaPerGram
    )
    accessible_volume: OntocrystalQuantity = RDFField(
        path=ONTOZEOLITE.hasAccessibleVolume
    )
    accessible_volume_per_cell: OntocrystalQuantity = RDFField(
        path=ONTOZEOLITE.hasAccessibleVolumePerCell
    )

    occupiable_area_per_cell: OntocrystalQuantity = RDFField(
        path=ONTOZEOLITE.hasOccupiableAreaPerCell
    )
    occupiable_area_per_gram: OntocrystalQuantity = RDFField(
        path=ONTOZEOLITE.hasOccupiableAreaPerGram
    )
    occupiable_volume: OntocrystalQuantity = RDFField(
        path=ONTOZEOLITE.hasOccupiableVolume
    )
    occupiable_volume_per_cell: OntocrystalQuantity = RDFField(
        path=ONTOZEOLITE.hasOccupiableVolumePerCell
    )

    specific_accessible_area: OntocrystalQuantity = RDFField(
        path=ONTOZEOLITE.hasSpecificAccessibleArea
    )
    specific_occupiable_area: OntocrystalQuantity = RDFField(
        path=ONTOZEOLITE.hasSpecificOccupiableArea
    )

    density: OntocrystalQuantity = RDFField(path=ONTOZEOLITE.hasDensity)
    framework_density: OntocrystalQuantity = RDFField(
        path=ONTOZEOLITE.hasFrameworkDensity
    )
    topo_density: OntozeoliteTopoDensity = RDFField(
        path=ONTOZEOLITE.hasTopologicalDensity
    )

    RDLS: float | None = RDFField(default=None, path=ONTOZEOLITE.hasRDLS)
    ring_sizes: OntocrystalMeasureVector = RDFField(path=ONTOZEOLITE.hasRingSizes)
    t_atom: list[OntozeoliteTAtom] = RDFField(path=ONTOZEOLITE.hasTAtom)
    secondary_bu: list[str] = RDFField(path=ONTOZEOLITE.hasSecondaryBU)
    composite_bu: OntozeoliteCompositeBU | None = RDFField(
        default=None, path=ONTOZEOLITE.hasCompositeBU
    )
    abc_seq: str | None = RDFField(default=None, path=ONTOZEOLITE.hasABCSequence)


class OntozeoliteZeoliteFramework(OntozeoliteZeoliteFrameworkBase):
    crystal_information: OntocrystalCrystalInfo = RDFField(
        path=ONTOCRYSTAL.hasCrystalInformation
    )
    topo_props: OntozeoliteTopoProps = RDFField(
        path=ONTOZEOLITE.hasTopologicalProperties
    )
    material: list[OntozeoliteZeoliticMaterialBase] = RDFField(
        path=ONTOZEOLITE.hasZeoliticMaterial
    )
