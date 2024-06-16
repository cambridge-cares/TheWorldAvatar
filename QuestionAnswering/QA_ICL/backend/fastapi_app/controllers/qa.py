from functools import cache
import logging
from typing import Annotated

from fastapi import Depends

from model.qa import (
    QARequestArtifact,
    QAResponse,
    QAResponseMetadata,
)
from services.rewrite_nlq import NlqRewriter, get_nlq_rewriter
from services.sem_parse.retrieve_context import (
    Nlq2DataReqContextRetriever,
    get_nlq2datareq_contextRetriever,
)
from services.stores.entity_store import EntityStore, get_entity_store
from services.stores.qa_artifact_store import (
    QARequestArtifactStore,
    get_qaReq_artifactStore,
)
from services.execute_data_req import DataReqExecutor, get_dataReq_executor
from services.sem_parse.translate_nlq import (
    Nlq2DataReqTranslator,
    get_nlq2datareq_translator,
)
from services.mol_vis.vis_data_store import VisualisationDataStore, get_visData_store


logger = logging.getLogger(__name__)


class DataSupporter:
    def __init__(
        self,
        nlq_rewriter: NlqRewriter,
        context_retriever: Nlq2DataReqContextRetriever,
        translator: Nlq2DataReqTranslator,
        entity_store: EntityStore,
        executor: DataReqExecutor,
        artifact_store: QARequestArtifactStore,
        vis_data_store: VisualisationDataStore,
    ):
        self.nlq_rewriter = nlq_rewriter
        self.context_retriever = context_retriever
        self.translator = translator
        self.entity_store = entity_store
        self.executor = executor
        self.artifact_store = artifact_store
        self.vis_data_store = vis_data_store

    def query(self, query: str):
        logger.info("Input query: " + query)

        logger.info("Rewriting input query...")
        rewritten_query = self.nlq_rewriter.rewrite(question=query)
        logger.info("Rewritten query: " + rewritten_query)

        logger.info("Retrieving translation context...")
        translation_context = self.context_retriever.retrieve(nlq=rewritten_query)
        logger.info("Translation context: " + str(translation_context))

        logger.info("Translating input question into data request...")
        data_req = self.translator.translate(
            nlq=rewritten_query, translation_context=translation_context
        )
        logger.info("Predicted data request: " + str(data_req))

        logger.info(
            "Performing entity linking for bindings: " + str(data_req.entity_bindings)
        )
        var2cls = data_req.var2cls or None
        var2iris = {
            var: list(
                set(
                    [
                        iri
                        for val in values
                        for iri in self.entity_store.link(
                            cls=var2cls and var2cls.get(var),
                            text=val if isinstance(val, str) else None,
                            identifier=val if isinstance(val, dict) else dict(),
                        )  # TODO: handle when no IRIs are returned
                    ]
                )
            )
            for var, values in data_req.entity_bindings.items()
        }
        logger.info("Linked IRIs: " + str(var2iris))

        logger.info("Executing data request...")
        data, data_artifact = self.executor.exec(
            var2cls=data_req.var2cls,
            entity_bindings=var2iris,
            const_bindings=data_req.const_bindings,
            req_form=data_req.req_form,
        )
        logger.info("Done")

        logger.info("Saving QA request artifact...")
        id = self.artifact_store.save(
            QARequestArtifact(
                nlq=query,
                nlq_rewritten=rewritten_query if rewritten_query != query else None,
                data_req=data_req,
                data=data_artifact,
            )
        )
        logger.info("Done")

        logger.info("Retrieving visualisation data...")
        clses = [
            data_req.var2cls.get(var)
            for var in data_req.visualise
            for _ in var2iris.get(var, [])
        ]
        iris = [iri for var in data_req.visualise for iri in var2iris.get(var, [])]
        chem_struct_data = self.vis_data_store.get(cls=clses, iris=iris)
        logger.info("Done")

        return QAResponse(
            request_id=id,
            metadata=QAResponseMetadata(
                rewritten_question=(
                    rewritten_query if rewritten_query != query else None
                ),
                translation_context=translation_context,
                data_request=data_req,
                linked_variables=var2iris,
            ),
            data=data,
            visualisation=chem_struct_data,
        )


@cache
def get_data_supporter(
    nlq_rewriter: Annotated[NlqRewriter, Depends(get_nlq_rewriter)],
    context_retriever: Annotated[
        Nlq2DataReqContextRetriever, Depends(get_nlq2datareq_contextRetriever)
    ],
    translator: Annotated[Nlq2DataReqTranslator, Depends(get_nlq2datareq_translator)],
    entity_store: Annotated[EntityStore, Depends(get_entity_store)],
    executor: Annotated[DataReqExecutor, Depends(get_dataReq_executor)],
    artifact_store: Annotated[QARequestArtifactStore, Depends(get_qaReq_artifactStore)],
    vis_data_store: Annotated[VisualisationDataStore, Depends(get_visData_store)],
):
    return DataSupporter(
        nlq_rewriter=nlq_rewriter,
        context_retriever=context_retriever,
        translator=translator,
        entity_store=entity_store,
        executor=executor,
        artifact_store=artifact_store,
        vis_data_store=vis_data_store,
    )
