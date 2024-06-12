from functools import cache
import logging
from typing import Annotated

from fastapi import Depends

from model.qa import (
    ChemicalStructureData,
    QARequestArtifact,
    QAResponse,
    QAResponseMetadata,
    TranslationContext,
)
from services.mol_vis.cif import CIFManager, get_cif_manager
from services.mol_vis.xyz import XYZManager, get_xyz_manager
from services.processs_response.augment_node import NodeDataRetriever
from services.processs_response.ontospecies import get_ontospecies_nodeDataRetriever
from services.processs_response.ontozeolite import get_ontozeolite_nodeDataRetriever
from services.rewrite_nlq import NlqRewriter, get_nlq_rewriter
from services.stores.entity_store import EntityStore, get_entity_store
from services.stores.nlq2datareq_example_store import (
    Nlq2DataReqExampleStore,
    get_nlq2datareq_exampleStore,
)
from services.stores.qa_artifact_store import (
    QARequestArtifactStore,
    get_qaReq_artifactStore,
)
from services.stores.schema_store import SchemaStore, get_schema_store
from services.execute_data_req import DataReqExecutor, get_dataReq_executor
from services.translate_nlq import Nlq2DataReqTranslator, get_nlq2datareq_translator


logger = logging.getLogger(__name__)


class DataSupporter:
    def __init__(
        self,
        nlq_rewriter: NlqRewriter,
        nlq2datareq_example_store: Nlq2DataReqExampleStore,
        schema_store: SchemaStore,
        translator: Nlq2DataReqTranslator,
        entity_store: EntityStore,
        executor: DataReqExecutor,
        artifact_store: QARequestArtifactStore,
        xyz_manager: XYZManager,
        cif_manager: CIFManager,
        ontospecies_node_data_retriever: NodeDataRetriever,
        ontozeolite_node_data_retriever: NodeDataRetriever,
    ):
        self.nlq_rewriter = nlq_rewriter
        self.nlq2datareq_example_store = nlq2datareq_example_store
        self.schema_store = schema_store
        self.translator = translator
        self.entity_store = entity_store
        self.executor = executor
        self.artifact_store = artifact_store
        self.xyz_manager = xyz_manager
        self.cif_manager = cif_manager
        self.ontospecies_node_data_retriever = ontospecies_node_data_retriever
        self.ontozeolite_node_data_retriever = ontozeolite_node_data_retriever

    def query(self, query: str):
        logger.info("Input query: " + query)

        logger.info("Rewriting input query...")
        rewritten_query = self.nlq_rewriter.rewrite(question=query)
        logger.info("Rewritten query: " + rewritten_query)

        logger.info("Retrieving schema items...")
        schema = self.schema_store.retrieve_relations(nlq=rewritten_query, k=10)
        logger.info("Retrieved schema items: " + str(schema))

        logger.info("Retrieving examples...")
        examples = self.nlq2datareq_example_store.retrieve_examples(
            nlq=rewritten_query, k=10
        )
        logger.info("Retrieved examples: " + str(examples))

        translation_context = TranslationContext(
            examples=examples, schema_relations=schema
        )
        logger.info("Translating input question into data request...")
        # KIV: example permutation
        data_req = self.translator.translate(
            nlq=rewritten_query, translation_context=translation_context
        )
        logger.info("Predicted data request: " + str(data_req))

        logger.info(
            "Performing entity linking for bindings: " + str(data_req.entity_bindings)
        )
        var2cls = data_req.var2cls or dict()
        var2iris = {
            var: list(
                set(
                    [
                        iri
                        for val in values
                        for iri in self.entity_store.link(
                            cls=data_req.var2cls and data_req.var2cls.get(var),
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

        logger.info("Retrieving visualisation data")
        species_vars = [
            var
            for var in data_req.visualise
            if data_req.var2cls.get(var) == "os:Species"
        ]
        species_iris = [iri for var in species_vars for iri in var2iris.get(var, [])]
        species_iris = list(set(species_iris))
        xyzs = [self.xyz_manager.get(iri) for iri in species_iris]
        # TODO: Use ORM to access species label
        species_node_data = self.ontospecies_node_data_retriever.retrieve(
            "os:Species", iris=species_iris
        )
        chem_struct_data = [
            ChemicalStructureData(
                type="xyz", label=node_datum.get("Label", ""), iri=iri, data=xyz
            )
            for iri, xyz, node_datum in zip(species_iris, xyzs, species_node_data)
            if xyz
        ]

        zeoframework_vars = [
            var
            for var in data_req.visualise
            if data_req.var2cls.get(var) == "zeo:ZeoliteFramework"
        ]
        zeoframework_iris = [
            iri for var in zeoframework_vars for iri in var2iris.get(var, [])
        ]
        zeoframework_iris = list(set(zeoframework_iris))
        zeoframework_cifs = [self.cif_manager.get(iri) for iri in zeoframework_iris]
        # TODO: Use ORM to access zeolite codes and formulae
        zeoframework_node_data = self.ontozeolite_node_data_retriever.retrieve(
            "zeo:ZeoliteFramework", iris=zeoframework_iris
        )
        chem_struct_data.extend(
            ChemicalStructureData(
                type="cif",
                iri=iri,
                label=node_datum.get("FrameworkCode"),
                data=cif,
            )
            for iri, cif, node_datum in zip(
                zeoframework_iris, zeoframework_cifs, zeoframework_node_data
            )
            if cif
        )

        zeomaterial_vars = [
            var
            for var in data_req.visualise
            if data_req.var2cls.get(var) == "zeo:ZeoliticMaterial"
        ]
        zeomaterial_iris = [
            iri for var in zeomaterial_vars for iri in var2iris.get(var, [])
        ]
        zeomaterial_iris = list(set(zeomaterial_iris))
        zeomaterial_cifs = [self.cif_manager.get(iri) for iri in zeomaterial_iris]
        # TODO: Use ORM to access zeolite codes and formulae
        zeomaterial_node_data = self.ontozeolite_node_data_retriever.retrieve(
            "zeo:ZeoliticMaterial", iris=zeomaterial_iris
        )
        chem_struct_data.extend(
            ChemicalStructureData(
                type="cif",
                iri=iri,
                label=node_datum.get("ChemicalFormula"),
                data=cif,
            )
            for iri, cif, node_datum in zip(
                zeomaterial_iris, zeomaterial_cifs, zeomaterial_node_data
            )
            if cif
        )
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
    schema_store: Annotated[SchemaStore, Depends(get_schema_store)],
    nlq2datareq_example_store: Annotated[
        Nlq2DataReqExampleStore, Depends(get_nlq2datareq_exampleStore)
    ],
    translator: Annotated[Nlq2DataReqTranslator, Depends(get_nlq2datareq_translator)],
    entity_store: Annotated[EntityStore, Depends(get_entity_store)],
    executor: Annotated[DataReqExecutor, Depends(get_dataReq_executor)],
    artifact_store: Annotated[QARequestArtifactStore, Depends(get_qaReq_artifactStore)],
    xyz_manager: Annotated[XYZManager, Depends(get_xyz_manager)],
    cif_manager: Annotated[CIFManager, Depends(get_cif_manager)],
    ontospecies_node_data_retriever: Annotated[
        NodeDataRetriever, Depends(get_ontospecies_nodeDataRetriever)
    ],
    ontozeolite_node_data_retriever: Annotated[
        NodeDataRetriever, Depends(get_ontozeolite_nodeDataRetriever)
    ],
):
    return DataSupporter(
        nlq_rewriter=nlq_rewriter,
        schema_store=schema_store,
        nlq2datareq_example_store=nlq2datareq_example_store,
        translator=translator,
        entity_store=entity_store,
        executor=executor,
        artifact_store=artifact_store,
        xyz_manager=xyz_manager,
        cif_manager=cif_manager,
        ontospecies_node_data_retriever=ontospecies_node_data_retriever,
        ontozeolite_node_data_retriever=ontozeolite_node_data_retriever,
    )
