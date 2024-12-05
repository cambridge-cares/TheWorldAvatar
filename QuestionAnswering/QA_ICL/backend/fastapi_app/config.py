from functools import cache
import logging
import os
from typing import Annotated, Literal, Union

from openai import BaseModel
from pydantic import ConfigDict, Field, TypeAdapter
import yaml

from utils.collections import deep_update


logger = logging.getLogger()


class RedisSettings(BaseModel):
    model_config = ConfigDict(frozen=True)

    host: str
    port:str = 6379


class TextEmbeddingSettingsTriton(BaseModel):
    model_config = ConfigDict(frozen=True)

    backend: Literal["triton"]
    url: str
    model: str


class OpenaiSettings(BaseModel):
    model_config = ConfigDict(frozen=True)

    base_url: str
    api_key: str
    model: str


class TextEmbeddingSettingsOpenai(OpenaiSettings):
    model_config = ConfigDict(frozen=True)

    backend: Literal["openai"]


TextEmbeddingSettings = Annotated[
    Union[TextEmbeddingSettingsTriton, TextEmbeddingSettingsOpenai],
    Field(discriminator="backend"),
]


class LocationIqSettings(BaseModel):
    model_config = ConfigDict(frozen=True)

    api_key: str


class ChemistryEndpointsSettings(BaseModel):
    model_config = ConfigDict(frozen=True)

    ontospecies: str
    ontokin: str
    ontocompchem: str
    ontozeolite: str
    ontomops: str


class SingaporeEndpointsSettings(BaseModel):
    model_config = ConfigDict(frozen=True)

    ontop: str
    ontop_internal: str
    dispersion: str
    plot: str
    company: str
    carpark: str
    carpark_internal: str
    feature_info_agent: str
    pollutant_concentration: str


class EntityLinkingConfigEntry(BaseModel):
    model_config = ConfigDict(frozen=True)

    cls: str | None = None
    strategy: Literal["fuzzy", "semantic"] = "fuzzy"
    k: int = 3


class SemanticSearchSettings(BaseModel):
    model_config = ConfigDict(frozen=True)

    threshold: float


class EntityLinkingSettings(BaseModel):
    model_config = ConfigDict(frozen=True)

    semantic: SemanticSearchSettings
    entries: tuple[EntityLinkingConfigEntry, ...]


class ContextAugmentationSettings(BaseModel):
    model_config = ConfigDict(frozen=True)

    example_num: int
    relation_num: int


class SemanticParsingSettings(BaseModel):
    model_config = ConfigDict(frozen=True)

    context_augmentation: ContextAugmentationSettings


class OntomopsFileserverSettings(BaseModel):
    model_config = ConfigDict(frozen=True)

    username: str
    password: str


class AppSettings(BaseModel):
    model_config = ConfigDict(frozen=True)

    redis: RedisSettings
    text_embedding: TextEmbeddingSettings
    translator: OpenaiSettings
    chat: OpenaiSettings
    qt_recog: OpenaiSettings
    location_iq: LocationIqSettings
    chemistry_endpoints: ChemistryEndpointsSettings
    singapore_endpoints: SingaporeEndpointsSettings
    entity_linking: EntityLinkingSettings
    semantic_parsing: SemanticParsingSettings
    ontomops_fileserver: OntomopsFileserverSettings


def _load_yaml(filepath: str):
    with open(filepath, "r") as f:
        config = yaml.safe_load(f)
    if not isinstance(config, dict):
        raise Exception("Expects content of .yaml to be a dict, found:\n" + str(config))
    return config


def _overwrite_base_config(base_config: dict, filepath: str):
    if os.path.exists(filepath):
        overwriting_config = _load_yaml(filepath)
        deep_update(source=overwriting_config, destination=base_config)


@cache
def get_app_settings():
    dirpath = os.path.dirname(os.path.realpath(__file__))

    config = _load_yaml(os.path.join(dirpath, "app.yaml"))
    _overwrite_base_config(
        base_config=config,
        filepath=os.path.join(
            dirpath, "app.{env}.yaml".format(env=os.getenv("APP_ENV", "dev"))
        ),
    )
    _overwrite_base_config(
        base_config=config, filepath=os.path.join(dirpath, "app.local.yaml")
    )

    adapter = TypeAdapter(AppSettings)
    settings = adapter.validate_python(config)

    logger.info("Loaded settings:\n" + settings.model_dump_json(indent=2))
    return settings


@cache
def get_frontend_name():
    return "zaha"
