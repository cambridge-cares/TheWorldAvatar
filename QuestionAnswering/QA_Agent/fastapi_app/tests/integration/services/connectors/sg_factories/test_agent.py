import os
import pytest

from redis import Redis

from model.aggregate import AggregateOperator
from services.connectors.sg_factories.constants import FactoryAttrKey, FactoryConcept
from services.connectors.sg_factories.agent.labels_store import (
    get_sg_factories_bindings,
)
from services.labels_store import LabelsStore
from services.kg_client import KgClient
from services.connectors.sg_factories.agent import SGFactoriesAgent


# TODO: Use a single dedicated blazegraph instance to run tests
# TODO: Insert sample data within this scope
@pytest.fixture(scope="module")
def sg_factories_ontop_client():
    yield KgClient(os.getenv("KG_ENDPOINT_SG_FACTORIES_ONTOP"))


@pytest.fixture(scope="module")
def sg_factories_labels_store(
    redis_client: Redis,
    sg_factories_ontop_client: KgClient,
):
    yield LabelsStore(
        redis_client=redis_client,
        key_prefix="sg_factories:factories:",
        index_name="idx:sg_factories:factories",
        bindings=get_sg_factories_bindings(ontop_client=sg_factories_ontop_client),
    )


@pytest.fixture(scope="module")
def sg_factories_agent(
    sg_factories_ontop_client: KgClient,
    sg_factories_labels_store: LabelsStore,
):
    yield SGFactoriesAgent(
        ontop_client=sg_factories_ontop_client,
        labels_store=sg_factories_labels_store,
    )


class TestSGFactoriesAgent:
    def test_lookupFactoryAttribute(self, sg_factories_agent: SGFactoriesAgent):
        # Arrange
        plant_name = "KENSETSU INTERNATIONAL (S) PTE. LTD."

        # Act
        data = sg_factories_agent.lookup_factory_attribute(
            plant_name=plant_name, attr_key=FactoryAttrKey.GENERATED_HEAT
        )

        # Assert
        assert data.vars == ["IRI", "GeneratedHeatNumericalValue", "GeneratedHeatUnit"]
        assert len(data.bindings) == 1
        assert (
            data.bindings[0].items()
            >= {
                "GeneratedHeatNumericalValue": "3.625656925",
                "GeneratedHeatUnit": "MW",
            }.items()
        )

    def test_countFactories_all(self, sg_factories_agent: SGFactoriesAgent):
        # Act
        data = sg_factories_agent.count_factories(
            factory_type=FactoryConcept.CHEMICAL_PLANT
        )

        # Assert
        assert data.vars == ["Count"]
        assert data.bindings == [{"Count": "96"}]

    def test_countFactories_groupby(self, sg_factories_agent: SGFactoriesAgent):
        # Act
        data = sg_factories_agent.count_factories(factory_type=None, group_bytype=True)

        # Assert
        assert data.vars == ["Count", "Type"]
        # TODO: compare by set
        assert data.bindings == [
            {
                "Count": "96",
                "Type": "http://www.theworldavatar.com/kg/ontochemplant/ChemicalPlant",
            },
            {
                "Count": "201",
                "Type": "http://www.theworldavatar.com/kg/ontocompany/FoodPlant",
            },
            {
                "Count": "11",
                "Type": "http://www.theworldavatar.com/kg/ontocompany/SemiconductorPlant",
            },
        ]

    def test_computeAggregateFactoryAttribute_all(
        self, sg_factories_agent: SGFactoriesAgent
    ):
        # Act
        data = sg_factories_agent.compute_aggregate_factory_attribute(
            factory_type=FactoryConcept.FOOD_PLANT,
            attr_agg=(FactoryAttrKey.DESIGN_CAPACITY, AggregateOperator.AVG),
        )

        # Assert
        assert data.vars == ["DesignCapacityNumericalValueAVG"]
        assert data.bindings == [
            {"DesignCapacityNumericalValueAVG": "0.07682154649751231"}
        ]

    def test_computeAggregateFactoryAttribute_groupby(
        self, sg_factories_agent: SGFactoriesAgent
    ):
        # Act
        data = sg_factories_agent.compute_aggregate_factory_attribute(
            factory_type=None,
            attr_agg=(FactoryAttrKey.GENERATED_HEAT, AggregateOperator.SUM),
            group_bytype=True,
        )

        # Assert
        assert data.vars == ["Type", "GeneratedHeatNumericalValueSUM"]
        assert data.bindings == [
            {
                "Type": "http://www.theworldavatar.com/kg/ontochemplant/ChemicalPlant",
                "GeneratedHeatNumericalValueSUM": "648.8399716939999",
            },
            {
                "GeneratedHeatNumericalValueSUM": "149.17109734199994",
                "Type": "http://www.theworldavatar.com/kg/ontocompany/FoodPlant",
            },
            {
                "GeneratedHeatNumericalValueSUM": "124.29142511900002",
                "Type": "http://www.theworldavatar.com/kg/ontocompany/SemiconductorPlant",
            },
        ]
