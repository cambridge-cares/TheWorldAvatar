"""
# Author: picas9dan #

This file provides the 3D Tiles Next schema as TypedDict.
"""

# Standard library imports
from typing import TypedDict, List, Literal, Dict


# For python>=3.11, NotRequired can be used to denote non-compulsory keys of a TypedDict
# Alternatively, as per https://peps.python.org/pep-0655/#interaction-with-total-false, we can define compulsory keys
# in a base TypedDict, and non-compulsory keys in the derived class declared with total=False.


# https://github.com/CesiumGS/3d-tiles/blob/main/specification/schema/asset.schema.json
class Asset(TypedDict):
    version: str


# https://github.com/CesiumGS/3d-tiles/blob/main/specification/schema/boundingVolume.schema.json
class BoundingVolume(TypedDict, total=False):
    box: List[float]
    region: List[float]
    sphere: List[float]


# https://github.com/CesiumGS/3d-tiles/blob/main/specification/schema/content.schema.json
class _ContentBase(TypedDict):
    uri: str


class Content(_ContentBase, total=False):
    boundingVolume: BoundingVolume


# https://github.com/CesiumGS/3d-tiles/blob/main/specification/schema/tile.schema.json
class _TileBase(TypedDict):
    boundingVolume: BoundingVolume
    geometricError: float


class Tile(_TileBase, total=False):
    viewerRequestVolume: dict
    refine: Literal["ADD", "REPLACE", "string"]
    transform: list
    content: Content
    # https://github.com/CesiumGS/3d-tiles/blob/main/extensions/3DTILES_multiple_contents/schema/tile.3DTILES_multiple_contents.schema.json
    contents: List[Content]
    children: List["Tile"]


# https://github.com/CesiumGS/3d-tiles/blob/main/extensions/3DTILES_metadata/schema/class.schema.json
class Class(TypedDict):
    name: str
    description: str
    properties: dict


# https://github.com/CesiumGS/3d-tiles/blob/main/extensions/3DTILES_metadata/schema/schema.schema.json
class Schema(TypedDict, total=False):
    id: str
    name: str
    description: str
    version: str
    classes: Dict[str, Class]
    enums: dict


# https://github.com/CesiumGS/3d-tiles/blob/main/extensions/3DTILES_metadata/schema/metadataEntity.schema.json
_MetadataEntityBase = TypedDict("_MetadataEntityBase", {"class": str})


class MetadataEntity(_MetadataEntityBase, total=False):
    properties: dict


# https://github.com/CesiumGS/3d-tiles/blob/main/specification/schema/tileset.schema.json
class _TilesetBase(TypedDict):
    asset: Asset
    geometricError: float
    root: Tile


class Tileset(_TilesetBase, total=False):
    properties: dict
    extensionUsed: list
    extensionRequired: list
    # https://github.com/CesiumGS/3d-tiles/blob/main/extensions/3DTILES_metadata/schema/tileset.3DTILES_metadata.schema.json
    schema: Schema
    # https://github.com/CesiumGS/3d-tiles/blob/main/extensions/3DTILES_metadata/schema/tileset.schema.json
    metadata: MetadataEntity
