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
    label: str
    cart_pos: OntocrystalMeasureVector
    fract_post: OntocrystalMeasureVector


class OntocrystalAtomicStructure(RDFEntity):
    atom_site: list[OntocrystalAtomSite]


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
