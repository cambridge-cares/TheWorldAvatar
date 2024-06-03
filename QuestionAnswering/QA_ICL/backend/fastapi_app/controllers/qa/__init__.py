import logging
from typing import Annotated

from fastapi import Depends

from services.entity_store import EntityStore, get_entity_store
from services.example_store import (
    ExampleStore,
    get_example_store,
)
from services.schema_store import SchemaStore, get_schema_store
from .execute_data_req import DataReqExecutor, get_dataReq_executor
from .translate import Nlq2DataReqTranslator, get_nlq2datareq_translator


logger = logging.getLogger(__name__)


class DataSupporter:
    def __init__(
        self,
        example_store: ExampleStore,
        schema_store: SchemaStore,
        translator: Nlq2DataReqTranslator,
        entity_store: EntityStore,
        executor: DataReqExecutor,
    ):
        self.example_store = example_store
        self.schema_store = schema_store
        self.translator = translator
        self.entity_store = entity_store
        self.executor = executor

    def query(self, query: str):
        logger.info("Input query: " + query)

        logger.info("Retrieving schema items...")
        schema_items = self.schema_store.retrieve_relations(nlq=query, k=10)
        logger.info("Retrieved schema items: " + str(schema_items))

        logger.info("Retrieving examples...")
        examples = self.example_store.retrieve_examples(nlq=query, k=10)
        logger.info("Retrieved examples: " + str(examples))

        logger.info("Translating input question into data request...")
        # KIV: example permutation
        data_req = self.translator.translate(
            nlq=query, schema_items=schema_items, examples=examples
        )
        logger.info("Predicted data request: " + str(data_req))

        logger.info(
            "Performing entity linking for bindings: " + str(data_req.entity_bindings)
        )
        var2iris = {
            var: [
                iri
                for val in binding.values
                for iri in self.entity_store.link(
                    cls=binding.cls, text=val.text, identifier=val.identifier
                )
            ]
            for var, binding in data_req.entity_bindings.items()
        }
        logger.info("Linked IRIs: " + str(var2iris))

        return self.executor.exec(
            entity_bindings=var2iris,
            const_bindings=data_req.const_bindings,
            req_form=data_req.req_form,
        )


def get_data_supporter(
    schema_store: Annotated[SchemaStore, Depends(get_schema_store)],
    example_store: Annotated[ExampleStore, Depends(get_example_store)],
    translator: Annotated[Nlq2DataReqTranslator, Depends(get_nlq2datareq_translator)],
    entity_store: Annotated[EntityStore, Depends(get_entity_store)],
    executor: Annotated[DataReqExecutor, Depends(get_dataReq_executor)],
):
    return DataSupporter(
        schema_store=schema_store,
        example_store=example_store,
        translator=translator,
        entity_store=entity_store,
        executor=executor,
    )
