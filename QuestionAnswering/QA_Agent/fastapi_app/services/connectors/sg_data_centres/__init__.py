from functools import cached_property
import logging
import time
from typing import List, Optional

from services.core.parse import KeyAggregateParser
from model.qa import QAStep
from services.connectors.sg_data_centres.model import DataCentreAttrKey
from services.core.align_enum import EnumAligner
from services.connectors.sg_data_centres.agent import SGDataCentresAgent
from services.connectors.agent_connector import AgentConnectorBase
from .parse import DataCentreConstraintsParser


logger = logging.getLogger(__name__)


class SGDataCentresAgentConnector(AgentConnectorBase):
    def __init__(
        self,
        agent: SGDataCentresAgent,
        data_centre_attr_key_aligner: EnumAligner[DataCentreAttrKey],
        data_centre_constraints_parser: DataCentreConstraintsParser,
        attr_agg_parser: KeyAggregateParser[DataCentreAttrKey],
    ):
        self.agent = agent
        self.data_centre_attr_key_aligner = data_centre_attr_key_aligner
        self.data_centre_constraints_parser = data_centre_constraints_parser
        self.attr_agg_parser = attr_agg_parser

    @cached_property
    def funcs(self):
        return [{"name": ""}]

    @cached_property
    def name2method(
        self,
    ):
        pass

    def lookup_dataCentre_attribute(self, name: str, attribute: str):
        steps: List[QAStep] = []

        logger.info("Aligning attribute: " + attribute)
        timestamp = time.time()
        attr_key = self.data_centre_attr_key_aligner.align(attribute)
        latency = time.time() - timestamp
        logger.info("Aligned attribute: " + str(attr_key))
        steps.append(
            QAStep(
                action="align_attribute_key",
                arguments=attribute,
                results=attr_key.value,
                latency=latency,
            )
        )

        timestamp = time.time()
        data = self.agent.lookup_dataCentre_attribute(name=name, attr_key=attr_key)
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="lookup_dataCentre_attribute",
                arguments=dict(name=name, attr_key=attr_key.value),
                latency=latency,
            )
        )

        return steps, data

    def find_dataCentres(self, constraints: Optional[str] = None):
        steps = List[QAStep] = []

        if constraints:
            logger.info("Parsing data centre constraints: " + constraints)
            timestamp = time.time()
            parsed_constraints = self.data_centre_constraints_parser.parse(constraints)
            latency = time.time() - timestamp
            unpacked_constraints = {
                k.value: v.value for k, v in parsed_constraints.items()
            }
            steps.append(
                action="parse_constraints",
                arguments=constraints,
                results=unpacked_constraints,
                latency=latency,
            )
        else:
            parsed_constraints = None
            unpacked_constraints = None

        timestamp = time.time()
        data = self.agent.find_dataCentres(parsed_constraints)
        latency = time.time()
        steps.append(
            QAStep(
                action="find_dataCentres",
                arguments=unpacked_constraints,
                latency=latency,
            )
        )

        return steps, data

    def count_dataCentres(self):
        steps = []

        timestamp = time.time()
        data = self.agent.count_dataCentres()
        latency = time.time() - timestamp
        steps.append(QAStep(action="count_dataCentres", latency=latency))

        return steps, data

    def compute_aggregate_dataCentre_attibute(self, attribute_aggregate: str):
        steps: List[QAStep] = []

        logger.info("Aligning attribute aggregate: " + attribute_aggregate)
        timestamp = time.time()
        attr_agg = self.attr_agg_parser.parse(attribute_aggregate)
        latency = time.time() - timestamp
        logger.info("Aligned attribute aggregate: " + str(attr_agg))
        unpacked_attr_agg = tuple(x.value for x in attr_agg)
        steps.append(
            QAStep(
                action="align_attr_agg",
                arguments=attribute_aggregate,
                results=unpacked_attr_agg,
                latency=latency,
            )
        )

        timestamp = time.time()
        data = self.agent.compute_aggregate_dataCentres_attribute(attr_agg)
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="compute_agg_attr", arguments=unpacked_attr_agg, latency=latency
            )
        )

        return steps, data
