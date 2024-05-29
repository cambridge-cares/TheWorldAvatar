import logging
import time
from typing import Annotated, List

from fastapi import Depends

from controllers.qa.model import QAStep
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
        executor: DataReqExecutor,
    ):
        self.example_store = example_store
        self.schema_store = schema_store
        self.translator = translator
        self.executor = executor

    def query(self, query: str):
        steps: List[QAStep] = []
        logger.info("Input query: " + query)

        logger.info("Retrieving schema items...")
        timestamp = time.time()
        schema_items = self.schema_store.retrieve_relations(nlq=query, k=10)
        latency = time.time() - timestamp
        logger.info("Retrieved schema items: " + str(schema_items))
        steps.append(
            QAStep(
                action="retrieve_schema",
                arguments=dict(nlq=query, k=10),
                results=schema_items[:3] + ["..."],
                latency=latency,
            )
        )

        logger.info("Retrieving examples...")
        timestamp = time.time()
        examples = self.example_store.retrieve_examples(nlq=query, k=10)
        latency = time.time() - timestamp
        logger.info("Retrieved examples: " + str(examples))
        steps.append(
            QAStep(
                action="retrieve_examples",
                arguments=dict(nlq=query, k=10),
                results=examples[:3] + ["..."],
                latency=latency,
            )
        )

        timestamp = time.time()
        # KIV: example permutation
        data_req = self.translator.translate(
            nlq=query, schema_items=schema_items, examples=examples
        )
        latency = time.time() - timestamp
        logger.info("Predicted data request: " + str(data_req))
        steps.append(
            QAStep(
                action="nlq2datareq", arguments=query, results=data_req, latency=latency
            )
        )

        _steps, data = self.executor.exec(data_req)
        steps.extend(_steps)

        return steps, data


def get_data_supporter(
    schema_store: Annotated[SchemaStore, Depends(get_schema_store)],
    example_store: Annotated[ExampleStore, Depends(get_example_store)],
    translator: Annotated[Nlq2DataReqTranslator, Depends(get_nlq2datareq_translator)],
    executor: Annotated[DataReqExecutor, Depends(get_dataReq_executor)],
):
    return DataSupporter(
        schema_store=schema_store,
        example_store=example_store,
        translator=translator,
        executor=executor,
    )
