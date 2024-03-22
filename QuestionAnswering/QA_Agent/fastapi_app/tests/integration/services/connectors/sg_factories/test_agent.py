import pytest

from yarl import URL

from model.constraint import ExtremeValueConstraint
from model.qa import QAData
from model.aggregate import AggregateOperator
from services.core.labels_store import LabelsStore
from services.core.kg import KgClient
from services.connectors.sg_factories.model import (
    FactoryNumAttrKey,
    Industry,
)
from services.connectors.sg_factories.agent import SGFactoriesAgent
from services.connectors.sg_factories.agent.labels_store import (
    sgFactories_bindings_gen,
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
    ontocompany:hasDesignCapacity [ om:hasValue [ om:hasNumericalValue "2.219685"^^xsd:float ; om:hasUnit [ skos:notation "kg/s" ] ]  ] ;
    ontocompany:hasThermalEfficiency [ om:hasValue [ om:hasNumericalValue "0.52"^^xsd:float ] ] .
<http://test.com/2>
    rdf:type ontochemplant:ChemicalPlant ;
    ontocompany:belongsToIndustry [ rdf:type ontocompany:ChemicalIndustry ] ;
    rdfs:label "AICA SINGAPORE PTE. LTD." ;
    ontocompany:hasDesignCapacity [ om:hasValue [ om:hasNumericalValue "2.219685"^^xsd:float ; om:hasUnit [ skos:notation "kg/s" ] ] ] ;
    ontocompany:hasThermalEfficiency [ om:hasValue [ om:hasNumericalValue "0.61"^^xsd:float ] ] .
<http://test.com/3>
    rdf:type ontocompany:SemiconductorPlant ;
    ontocompany:belongsToIndustry [ rdf:type ontocompany:SemiconductorIndustry ] ;
    rdfs:label "NORTHERN LUCK PTE LTD" ;
    ontocompany:hasDesignCapacity [ om:hasValue [ om:hasNumericalValue "0.001121092"^^xsd:float ; om:hasUnit [ skos:notation "m^2/s" ] ] ]  ;
    ontocompany:hasThermalEfficiency [ om:hasValue [ om:hasNumericalValue "0.43"^^xsd:float ] ] ."""
    )

    endpoint = str(
        blazegraph_base_url / "blazegraph/namespace/sg_factories_ontop/sparql"
    )
    yield KgClient(endpoint)

    triples_manager.delete_all()


@pytest.fixture(scope="class")
def factory_subclasses():
    yield [
        "http://www.theworldavatar.com/kg/ontochemplant#ChemicalPlant",
        "http://www.theworldavatar.com/kg/ontocompany#FoodPlant",
        "http://www.theworldavatar.com/kg/ontocompany#SemiconductorPlant",
    ]


@pytest.fixture(scope="class")
def sg_factories_labels_store(
    redis_client, sg_factories_ontop_client, factory_subclasses
):
    yield LabelsStore(
        redis_client=redis_client,
        key_prefix="sg_factories:factories:",
        index_name="idx:sg_factories:factories",
        bindings=sgFactories_bindings_gen(
            factory_subclasses=factory_subclasses,
            ontop_client=sg_factories_ontop_client,
        ),
    )


@pytest.fixture(scope="class")
def sg_factories_sparql_maker(factory_subclasses):
    yield SGFactoriesSPARQLMaker(factory_subclasses)


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
        # Arrange
        plant_name = "Normet Singapore"
        attr_key = FactoryNumAttrKey.THERMAL_EFFICIENCY

        expected = QAData(
            vars=["IRI", "label", "ThermalEfficiencyValue", "ThermalEfficiencyUnit"],
            bindings=[
                {
                    "IRI": "http://test.com/1",
                    "label": "NORMET SINGAPORE PTE. LTD.",
                    "ThermalEfficiencyValue": "0.52",
                }
            ],
        )

        # Act
        actual = sg_factories_agent.lookup_factory_attribute(
            plant_name=plant_name, attr_key=attr_key
        )

        # Assert
        assert actual == expected

    def test_findFactories(self, sg_factories_agent: SGFactoriesAgent):
        # Arrange
        industry = Industry.CHEMICAL
        constraints = {FactoryNumAttrKey.THERMAL_EFFICIENCY: ExtremeValueConstraint.MAX}
        limit = 1

        expected = QAData(
            vars=[
                "Industry",
                "IRI",
                "label",
                "ThermalEfficiencyValue",
                "ThermalEfficiencyUnit",
            ],
            bindings=[
                {
                    "Industry": "ChemicalIndustry",
                    "IRI": "http://test.com/2",
                    "label": "AICA SINGAPORE PTE. LTD.",
                    "ThermalEfficiencyValue": "0.61",
                }
            ],
        )

        # Act
        actual = sg_factories_agent.find_factories(
            industry=industry,
            numattr_constraints=constraints,
            limit=limit,
        )

        # Assert
        assert actual == expected

    @pytest.mark.parametrize(
        "industry, expected",
        [
            (
                Industry.CHEMICAL,
                QAData(
                    vars=["Industry", "Count"],
                    bindings=[
                        {
                            "Industry": "ChemicalIndustry",
                            "Count": "2",
                        }
                    ],
                ),
            ),
            (
                None,
                QAData(
                    vars=["Industry", "Count"],
                    bindings=[
                        {
                            "Industry": "ChemicalIndustry",
                            "Count": "2",
                        },
                        {"Industry": "FoodIndustry", "Count": "0"},
                        {
                            "Industry": "SemiconductorIndustry",
                            "Count": "1",
                        },
                        {"Industry": "SUM", "Count": "3"},
                    ],
                ),
            ),
        ],
    )
    def test_countFactories(
        self,
        sg_factories_agent: SGFactoriesAgent,
        industry,
        expected,
    ):
        # Act
        actual = sg_factories_agent.count_factories(industry=industry)

        # Assert
        assert actual == expected

    def test_computeAggregateFactoryAttribute_chemicalIndustry(
        self, sg_factories_agent: SGFactoriesAgent
    ):
        # Arrange
        industry = Industry.CHEMICAL
        attr_agg = (FactoryNumAttrKey.DESIGN_CAPACITY, AggregateOperator.AVG)

        expected = QAData(
            vars=["Industry", "DesignCapacityValueAVG", "DesignCapacityUnit"],
            bindings=[
                {
                    "Industry": "ChemicalIndustry",
                    "DesignCapacityValueAVG": "2.219685",
                    "DesignCapacityUnit": "kg/s",
                }
            ],
        )

        # Act
        actual = sg_factories_agent.compute_aggregate_factory_attribute(
            attr_agg=attr_agg, industry=industry
        )

        # Assert
        assert actual == expected

    def test_computeAggregateFactoryAttribute_noIndustry(
        self, sg_factories_agent: SGFactoriesAgent
    ):
        # Arrange
        attr_agg = (FactoryNumAttrKey.DESIGN_CAPACITY, AggregateOperator.SUM)
        industry = None

        expected = QAData(
            vars=["Industry", "DesignCapacityValueSUM", "DesignCapacityUnit"],
            bindings=[
                {
                    "Industry": "ChemicalIndustry",
                    "DesignCapacityValueSUM": "4.43937",
                    "DesignCapacityUnit": "kg/s",
                },
                {
                    "Industry": "SemiconductorIndustry",
                    "DesignCapacityValueSUM": "0.001121092",
                    "DesignCapacityUnit": "m^2/s",
                },
            ],
        )

        # Act
        actual = sg_factories_agent.compute_aggregate_factory_attribute(
            attr_agg=attr_agg,
            industry=industry,
        )

        # Assert
        assert actual == expected
