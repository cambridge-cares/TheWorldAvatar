import pytest

from redis import Redis
from yarl import URL

from model.constraint import ExtremeValueConstraint
from model.qa import QAData
from model.aggregate import AggregateOperator
from services.core.labels_store import LabelsStore
from services.core.kg import KgClient
from services.connectors.sg_factories.model import (
    FactoryAttrKey,
    FactoryConstraints,
    Industry,
)
from services.connectors.sg_factories.agent import SGFactoriesAgent
from services.connectors.sg_factories.agent.labels_store import (
    get_sg_factories_bindings,
)
from services.connectors.sg_factories.agent.make_sparql import (
    SGFactoriesSPARQLMaker,
)
from tests.integration.services.utils import TriplesManager


@pytest.fixture(scope="class")
def sg_factories_bg_client(blazegraph_base_url: URL):
    triples_manager = TriplesManager(
        base_url=blazegraph_base_url,
        namespace="sg_factories_bg",
        prefixes="""PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany#>
PREFIX ontochemplant: <http://www.theworldavatar.com/kg/ontochemplant#>""",
    )

    triples_manager.insert(
        """ontochemplant:ChemicalPlant rdfs:subClassOf ontocompany:Factory .
ontocompany:FoodPlant rdfs:subClassOf ontocompany:Factory .
ontocompany:SemiconductorPlant rdfs:subClassOf ontocompany:Factory ."""
    )

    endpoint = str(blazegraph_base_url / "blazegraph/namespace/sg_factories_bg/sparql")
    yield KgClient(endpoint)

    triples_manager.delete_all()


@pytest.fixture(scope="class")
def sg_factories_ontop_client(blazegraph_base_url: URL):
    triples_manager = TriplesManager(
        base_url=blazegraph_base_url,
        namespace="sg_factories_ontop",
        prefixes="""PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany#>
PREFIX ontochemplant: <http://www.theworldavatar.com/kg/ontochemplant#>""",
    )

    triples_manager.insert(
        """<http://test.com/1> 
    rdf:type ontochemplant:ChemicalPlant ;
    ontocompany:belongsToIndustry [ rdf:type ontocompany:ChemicalIndustry ] ;
    rdfs:label "NORMET SINGAPORE PTE. LTD." ;
    ontocompany:hasGeneratedHeat [ om:hasValue [ om:hasNumericalValue "5.610722679"^^xsd:float ; om:hasUnit [ skos:notation "MW" ] ]  ] ;
    ontocompany:hasThermalEfficiency [ om:hasValue [ om:hasNumericalValue "0.52"^^xsd:float ] ] .
<http://test.com/2>
    rdf:type ontochemplant:ChemicalPlant ;
    ontocompany:belongsToIndustry [ rdf:type ontocompany:ChemicalIndustry ] ;
    rdfs:label "AICA SINGAPORE PTE. LTD." ;
    ontocompany:hasGeneratedHeat [ om:hasValue [ om:hasNumericalValue "12.97975266"^^xsd:float ; om:hasUnit [ skos:notation "MW" ] ] ] ;
    ontocompany:hasThermalEfficiency [ om:hasValue [ om:hasNumericalValue "0.52"^^xsd:float ] ] .
<http://test.com/3>
    rdf:type ontocompany:FoodPlant ;
    ontocompany:belongsToIndustry [ rdf:type ontocompany:FoodIndustry ] ;
    rdfs:label "NORTHERN LUCK PTE LTD" ;
    ontocompany:hasGeneratedHeat [ om:hasValue [ om:hasNumericalValue "6.427321157"^^xsd:float ; om:hasUnit [ skos:notation "MW" ] ] ]  ;
    ontocompany:hasThermalEfficiency [ om:hasValue [ om:hasNumericalValue "0.43"^^xsd:float ] ] ."""
    )

    endpoint = str(
        blazegraph_base_url / "blazegraph/namespace/sg_factories_ontop/sparql"
    )
    yield KgClient(endpoint)

    triples_manager.delete_all()


@pytest.fixture(scope="class")
def sg_factories_labels_store(
    redis_client: Redis,
    sg_factories_ontop_client: KgClient,
    sg_factories_bg_client: KgClient,
):
    yield LabelsStore(
        redis_client=redis_client,
        key_prefix="sg_factories:factories:",
        index_name="idx:sg_factories:factories",
        bindings=get_sg_factories_bindings(
            ontop_client=sg_factories_ontop_client, bg_client=sg_factories_bg_client
        ),
    )


@pytest.fixture(scope="class")
def sg_factories_sparql_maker():
    yield SGFactoriesSPARQLMaker(
        [
            "http://www.theworldavatar.com/kg/ontochemplant#ChemicalPlant",
            "http://www.theworldavatar.com/kg/ontocompany#FoodPlant",
            "http://www.theworldavatar.com/kg/ontocompany#SemiconductorPlant",
        ]
    )


@pytest.fixture(scope="class")
def sg_factories_agent(
    sg_factories_ontop_client: KgClient,
    sg_factories_labels_store: LabelsStore,
    sg_factories_sparql_maker: SGFactoriesSPARQLMaker,
):
    yield SGFactoriesAgent(
        ontop_client=sg_factories_ontop_client,
        labels_store=sg_factories_labels_store,
        sparql_maker=sg_factories_sparql_maker,
    )


class TestSGFactoriesAgent:
    def test_lookupFactoryAttribute(self, sg_factories_agent: SGFactoriesAgent):
        # Act
        actual = sg_factories_agent.lookup_factory_attribute(
            plant_name="Normet Singapore", attr_key=FactoryAttrKey.THERMAL_EFFICIENCY
        )

        # Assert
        assert actual == QAData(
            vars=["IRI", "ThermalEfficiencyNumericalValue"],
            bindings=[
                {"IRI": "http://test.com/1", "ThermalEfficiencyNumericalValue": "0.52"}
            ],
        )

    @pytest.mark.parametrize(
        "industry,groupby_industry,expected",
        [
            (None, False, QAData(vars=["Count"], bindings=[{"Count": "3"}])),
            (
                Industry.CHEMICAL,
                False,
                QAData(vars=["Count"], bindings=[{"Count": "2"}]),
            ),
            (
                None,
                True,
                QAData(
                    vars=["Count", "Industry"],
                    bindings=[
                        {
                            "Count": "2",
                            "Industry": "http://www.theworldavatar.com/kg/ontocompany#ChemicalIndustry",
                        },
                        {
                            "Count": "1",
                            "Industry": "http://www.theworldavatar.com/kg/ontocompany#FoodIndustry",
                        },
                    ],
                ),
            ),
        ],
    )
    def test_countFactories(
        self,
        sg_factories_agent: SGFactoriesAgent,
        industry,
        groupby_industry,
        expected,
    ):
        # Act
        actual = sg_factories_agent.count_factories(
            industry=industry, groupby_industry=groupby_industry
        )

        # Assert
        assert actual == expected

    # def test_computeAggregateFactoryAttribute_all(
    #     self, sg_factories_agent: SGFactoriesAgent
    # ):
    #     # Act
    #     data = sg_factories_agent.compute_aggregate_factory_attribute(
    #         factory_type=Industry.CHEMICAL,
    #         attr_agg=(FactoryAttrKey.GENERATED_HEAT, AggregateOperator.AVG),
    #     )

    #     # Assert
    #     assert data.vars == ["GeneratedHeatNumericalValueAVG", "GeneratedHeatUnit"]
    #     assert data.bindings == [
    #         {
    #             "GeneratedHeatNumericalValueAVG": "9.295238",
    #             "GeneratedHeatUnit": "MW",
    #         }
    #     ]

    # def test_computeAggregateFactoryAttribute_groupby(
    #     self, sg_factories_agent: SGFactoriesAgent
    # ):
    #     # Act
    #     data = sg_factories_agent.compute_aggregate_factory_attribute(
    #         factory_type=None,
    #         attr_agg=(FactoryAttrKey.GENERATED_HEAT, AggregateOperator.SUM),
    #         groupby_type=True,
    #     )

    #     # Assert
    #     assert data.vars == [
    #         "Type",
    #         "GeneratedHeatNumericalValueSUM",
    #         "GeneratedHeatUnit",
    #     ]
    #     assert data.bindings == [
    #         {
    #             "Type": "http://www.theworldavatar.com/kg/ontochemplant#ChemicalPlant",
    #             "GeneratedHeatNumericalValueSUM": "18.590475",
    #             "GeneratedHeatUnit": "MW",
    #         },
    #         {
    #             "GeneratedHeatNumericalValueSUM": "6.427321",
    #             "Type": "http://www.theworldavatar.com/kg/ontocompany#FoodPlant",
    #             "GeneratedHeatUnit": "MW",
    #         },
    #     ]
