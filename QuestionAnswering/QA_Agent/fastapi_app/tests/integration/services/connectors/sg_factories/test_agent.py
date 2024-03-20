import os
from urllib.parse import urljoin
import pytest

from redis import Redis
from yarl import URL

from tests.integration.services.utils import TriplesManager
from model.aggregate import AggregateOperator
from services.connectors.sg_factories.constants import FactoryAttrKey, FactoryConcept
from services.connectors.sg_factories.agent.labels_store import (
    get_sg_factories_bindings,
)
from services.labels_store import LabelsStore
from services.kg_client import KgClient
from services.connectors.sg_factories.agent import SGFactoriesAgent


@pytest.fixture(scope="module")
def sg_factories_ontop_client(blazegraph_base_url: URL):
    triples_manager = TriplesManager(
        base_url=blazegraph_base_url,
        namespace="sg_factories_ontop",
        prefixes="""PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany/>
PREFIX ontochemplant: <http://www.theworldavatar.com/kg/ontochemplant/>""",
    )

    triples_manager.insert(
        """<http://test.com/1> 
    rdf:type ontochemplant:ChemicalPlant ;
    rdfs:label "NORMET SINGAPORE PTE. LTD." ;
    ontocompany:hasGeneratedHeat [ om:hasValue [ om:hasNumericalValue "5.610722679"^^xsd:float ; om:hasUnit [ skos:notation "MW" ] ]  ] ;
    ontocompany:hasThermalEfficiency [ om:hasValue [ om:hasNumericalValue "0.52"^^xsd:float ] ] .
<http://test.com/2>
    rdf:type ontochemplant:ChemicalPlant ;
    rdfs:label "AICA SINGAPORE PTE. LTD." ;
    ontocompany:hasGeneratedHeat [ om:hasValue [ om:hasNumericalValue "12.97975266"^^xsd:float ; om:hasUnit [ skos:notation "MW" ] ] ] ;
    ontocompany:hasThermalEfficiency [ om:hasValue [ om:hasNumericalValue "0.52"^^xsd:float ] ] .
<http://test.com/3>
    rdf:type ontocompany:FoodPlant ;
    rdfs:label "NORTHERN LUCK PTE LTD" ;
    ontocompany:hasGeneratedHeat [ om:hasValue [ om:hasNumericalValue "6.427321157"^^xsd:float ; om:hasUnit [ skos:notation "MW" ] ] ]  ;
    ontocompany:hasThermalEfficiency [ om:hasValue [ om:hasNumericalValue "0.43"^^xsd:float ] ] ."""
    )

    endpoint = str(
        blazegraph_base_url / "blazegraph/namespace/sg_factories_ontop/sparql"
    )
    yield KgClient(endpoint)

    triples_manager.delete_all()


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
        # Act
        data = sg_factories_agent.lookup_factory_attribute(
            plant_name="Normet Singapore", attr_key=FactoryAttrKey.THERMAL_EFFICIENCY
        )

        # Assert
        assert data.vars == ["IRI", "ThermalEfficiencyNumericalValue"]
        assert len(data.bindings) == 1
        assert (
            data.bindings[0].items()
            >= {
                "ThermalEfficiencyNumericalValue": "0.52",
            }.items()
        )

    def test_countFactories_all(self, sg_factories_agent: SGFactoriesAgent):
        # Act
        data = sg_factories_agent.count_factories(
            factory_type=FactoryConcept.CHEMICAL_PLANT
        )

        # Assert
        assert data.vars == ["Count"]
        assert data.bindings == [{"Count": "2"}]

    def test_countFactories_groupby(self, sg_factories_agent: SGFactoriesAgent):
        # Act
        data = sg_factories_agent.count_factories(factory_type=None, groupby_type=True)

        # Assert
        assert data.vars == ["Count", "Type"]
        # TODO: compare by set
        assert data.bindings == [
            {
                "Count": "2",
                "Type": "http://www.theworldavatar.com/kg/ontochemplant/ChemicalPlant",
            },
            {
                "Count": "1",
                "Type": "http://www.theworldavatar.com/kg/ontocompany/FoodPlant",
            },
        ]

    def test_computeAggregateFactoryAttribute_all(
        self, sg_factories_agent: SGFactoriesAgent
    ):
        # Act
        data = sg_factories_agent.compute_aggregate_factory_attribute(
            factory_type=FactoryConcept.CHEMICAL_PLANT,
            attr_agg=(FactoryAttrKey.GENERATED_HEAT, AggregateOperator.AVG),
        )

        # Assert
        assert data.vars == ["GeneratedHeatNumericalValueAVG", "GeneratedHeatUnit"]
        assert data.bindings == [
            {
                "GeneratedHeatNumericalValueAVG": "9.295238",
                "GeneratedHeatUnit": "MW",
            }
        ]

    def test_computeAggregateFactoryAttribute_groupby(
        self, sg_factories_agent: SGFactoriesAgent
    ):
        # Act
        data = sg_factories_agent.compute_aggregate_factory_attribute(
            factory_type=None,
            attr_agg=(FactoryAttrKey.GENERATED_HEAT, AggregateOperator.SUM),
            groupby_type=True,
        )

        # Assert
        assert data.vars == [
            "Type",
            "GeneratedHeatNumericalValueSUM",
            "GeneratedHeatUnit",
        ]
        assert data.bindings == [
            {
                "Type": "http://www.theworldavatar.com/kg/ontochemplant/ChemicalPlant",
                "GeneratedHeatNumericalValueSUM": "18.590475",
                "GeneratedHeatUnit": "MW",
            },
            {
                "GeneratedHeatNumericalValueSUM": "6.427321",
                "Type": "http://www.theworldavatar.com/kg/ontocompany/FoodPlant",
                "GeneratedHeatUnit": "MW",
            },
        ]
