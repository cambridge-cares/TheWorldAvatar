from functools import cached_property
import logging
import time
from typing import Annotated, List, Optional

from fastapi import Depends

from services.core.parse import KeyAggregateParser
from model.qa import QAStep
from services.connectors.sg_data_centres.model import DataCentreAttrKey
from services.core.align_enum import EnumAligner
from services.connectors.sg_data_centres.agent import (
    SGDataCentresAgent,
    get_sgDataCentres_agent,
)
from services.connectors.agent_connector import AgentConnectorBase
from .align import get_dataCentreAttrkey_aligner
from .parse import (
    DataCentreConstraintsParser,
    get_dataCentreAttr_aggParser,
    get_dataCentreConstraints_parser,
)


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
        return [
            {
                "name": "lookup_dataCentre_attribute",
                "description": "Look up the attribute of a data centre e.g. heat emission, utilization rate",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "company": {
                            "type": "string",
                            "description": "Owner of the data centre",
                        },
                        "name": {"type": "string", "description": "Data centre name"},
                        "attribute": {
                            "type": "string",
                            "description": "Attribute to look up e.g. floor area, maximum IT capacity",
                        },
                    },
                },
                "required": ["attribute"],
            },
            {
                "name": "find_dataCentres",
                "description": "Find ICT companies and data centres that satisfy some constraints e.g. lowest heat emission",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "constraints": {
                            "type": "string",
                            "description": "Constraints e.g. largest floor area",
                        },
                        "limit": {
                            "type": "number",
                            "description": "Number of entries to return",
                        },
                    },
                },
            },
            {
                "name": "count_dataCentres",
                "description": "Tally number of data centres",
            },
            {
                "name": "compute_aggregate_dataCentre_attribute",
                "description": "Aggregate attribute data of data centres",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "attribute_aggregate": {
                            "type": "string",
                            "description": "An attribute name and an aggregate operation acted upon it e.g. total heat emission, smallest floor area",
                        }
                    },
                },
                "required": ["attribute_aggregate"],
            },
        ]

    @cached_property
    def name2method(self):
        return {
            "lookup_dataCentre_attribute": self.lookup_dataCentre_attribute,
            "find_dataCentres": self.find_dataCentres,
            "count_dataCentres": self.count_dataCentres,
            "compute_aggregate_dataCentre_attribute": self.compute_aggregate_dataCentre_attribute,
        }

    def lookup_dataCentre_attribute(
        self, attribute: str, company: Optional[str] = None, name: Optional[str] = None
    ):
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
        data = self.agent.lookup_dataCentre_attribute(
            company=company, name=name, attr_key=attr_key
        )
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="lookup_dataCentre_attribute",
                arguments=dict(name=name, attr_key=attr_key.value),
                latency=latency,
            )
        )

        return steps, data

    def find_dataCentres(
        self, constraints: Optional[str] = None, limit: Optional[int] = None
    ):
        steps: List[QAStep] = []

        if constraints:
            logger.info("Parsing data centre constraints: " + constraints)
            timestamp = time.time()
            parsed_constraints = self.data_centre_constraints_parser.parse(constraints)
            latency = time.time() - timestamp
            unpacked_constraints = {
                k.value: v.value for k, v in parsed_constraints.items()
            }
            steps.append(
                QAStep(
                    action="parse_constraints",
                    arguments=constraints,
                    results=unpacked_constraints,
                    latency=latency,
                )
            )
        else:
            parsed_constraints = None
            unpacked_constraints = None

        timestamp = time.time()
        data = self.agent.find_dataCentres(parsed_constraints, limit)
        latency = time.time() - timestamp
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

    def compute_aggregate_dataCentre_attribute(self, attribute_aggregate: str):
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


def get_sgDataCentres_agentConnector(
    agent: Annotated[SGDataCentresAgent, Depends(get_sgDataCentres_agent)],
    data_centre_attr_key_aligner: Annotated[
        EnumAligner[DataCentreAttrKey], Depends(get_dataCentreAttrkey_aligner)
    ],
    data_centre_constraints_parser: Annotated[
        DataCentreConstraintsParser, Depends(get_dataCentreConstraints_parser)
    ],
    attr_agg_parser: Annotated[
        KeyAggregateParser[DataCentreAttrKey], Depends(get_dataCentreAttr_aggParser)
    ],
):
    return SGDataCentresAgentConnector(
        agent=agent,
        data_centre_attr_key_aligner=data_centre_attr_key_aligner,
        data_centre_constraints_parser=data_centre_constraints_parser,
        attr_agg_parser=attr_agg_parser,
    )
