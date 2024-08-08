from functools import cache
from typing import Annotated

from fastapi import Depends

from config import AppSettings, ContextAugmentationSettings, get_app_settings
from model.structured_answer import TranslationContext
from services.stores.nlq2datareq_example_store import (
    Nlq2DataReqExampleStore,
    get_nlq2datareq_exampleStore,
)
from services.stores.schema_store import SchemaStore, get_schema_store


class Nlq2DataReqContextRetriever:
    def __init__(
        self,
        nlq2datareq_example_store: Nlq2DataReqExampleStore,
        schema_store: SchemaStore,
        example_num: int = 10,
        relation_num: int = 10,
    ):
        self.nlq2datareq_example_store = nlq2datareq_example_store
        self.schema_store = schema_store
        self.example_num = example_num
        self.relation_num = relation_num

    def retrieve(self, nlq: str):
        property_score_pairs = self.schema_store.retrieve_properties(nlq=nlq, k=self.relation_num)
        example_score_pairs = self.nlq2datareq_example_store.retrieve_examples(nlq=nlq, k=self.example_num)
        return TranslationContext(examples=example_score_pairs, properties=property_score_pairs)


def get_contextAugmentation_settings(
    app_settings: Annotated[AppSettings, Depends(get_app_settings)]
):
    return app_settings.semantic_parsing.context_augmentation


@cache
def get_nlq2datareq_contextRetriever(
    schema_store: Annotated[SchemaStore, Depends(get_schema_store)],
    example_store: Annotated[
        Nlq2DataReqExampleStore, Depends(get_nlq2datareq_exampleStore)
    ],
    context_augment_settings: Annotated[
        ContextAugmentationSettings, Depends(get_contextAugmentation_settings)
    ],
):
    return Nlq2DataReqContextRetriever(
        nlq2datareq_example_store=example_store,
        schema_store=schema_store,
        example_num=context_augment_settings.example_num,
        relation_num=context_augment_settings.relation_num,
    )
