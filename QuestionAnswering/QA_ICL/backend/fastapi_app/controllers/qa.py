import logging
from typing import Annotated

from fastapi import Depends

from model.qa import (
    QARequestArtifact,
    QAResponse,
    QAResponseMetadata,
    TranslationContext,
)
from services.stores.entity_store import EntityStore, get_entity_store
from services.stores.nlq2datareq_example_store import (
    Nlq2DataReqExampleStore,
    get_example_store,
)
from services.stores.qa_artifact_store import QARequestArtifactStore, get_qaReq_artifactStore
from services.stores.schema_store import SchemaStore, get_schema_store
from services.execute_data_req import DataReqExecutor, get_dataReq_executor
from services.translate_nlq import Nlq2DataReqTranslator, get_nlq2datareq_translator


logger = logging.getLogger(__name__)


class DataSupporter:
    def __init__(
        self,
        example_store: Nlq2DataReqExampleStore,
        schema_store: SchemaStore,
        translator: Nlq2DataReqTranslator,
        entity_store: EntityStore,
        executor: DataReqExecutor,
        artifact_store: QARequestArtifactStore,
    ):
        self.example_store = example_store
        self.schema_store = schema_store
        self.translator = translator
        self.entity_store = entity_store
        self.executor = executor
        self.artifact_store = artifact_store

    def query(self, query: str):
        logger.info("Input query: " + query)

        logger.info("Retrieving schema items...")
        schema = self.schema_store.retrieve_relations(nlq=query, k=10)
        logger.info("Retrieved schema items: " + str(schema))

        logger.info("Retrieving examples...")
        examples = self.example_store.retrieve_examples(nlq=query, k=10)
        logger.info("Retrieved examples: " + str(examples))

        translation_context = TranslationContext(
            examples=examples, schema_relations=schema
        )
        logger.info("Translating input question into data request...")
        # KIV: example permutation
        data_req = self.translator.translate(
            nlq=query, translation_context=translation_context
        )
        logger.info("Predicted data request: " + str(data_req))

        logger.info(
            "Performing entity linking for bindings: " + str(data_req.entity_bindings)
        )
        var2iris = {
            var: list(
                set(
                    [
                        iri
                        for val in binding.values
                        for iri in self.entity_store.link(
                            cls=binding.cls, text=val.text, identifier=val.identifier
                        )
                    ]
                )
            )
            for var, binding in data_req.entity_bindings.items()
        }
        logger.info("Linked IRIs: " + str(var2iris))

        logger.info("Executing data request...")
        data, data_artifact = self.executor.exec(
            entity_bindings=var2iris,
            const_bindings=data_req.const_bindings,
            req_form=data_req.req_form,
        )
        logger.info("Done")

        logger.info("Saving QA request artifact...")
        id = self.artifact_store.save(
            QARequestArtifact(nlq=query, data_req=data_req, data=data_artifact)
        )

        return QAResponse(
            request_id=id,
            metadata=QAResponseMetadata(
                translation_context=translation_context,
                data_request=data_req,
                linked_variables=var2iris,
            ),
            data=data,
        )


def get_data_supporter(
    schema_store: Annotated[SchemaStore, Depends(get_schema_store)],
    example_store: Annotated[Nlq2DataReqExampleStore, Depends(get_example_store)],
    translator: Annotated[Nlq2DataReqTranslator, Depends(get_nlq2datareq_translator)],
    entity_store: Annotated[EntityStore, Depends(get_entity_store)],
    executor: Annotated[DataReqExecutor, Depends(get_dataReq_executor)],
    artifact_store: Annotated[QARequestArtifactStore, Depends(get_qaReq_artifactStore)],
):
    return DataSupporter(
        schema_store=schema_store,
        example_store=example_store,
        translator=translator,
        entity_store=entity_store,
        executor=executor,
        artifact_store=artifact_store,
    )
