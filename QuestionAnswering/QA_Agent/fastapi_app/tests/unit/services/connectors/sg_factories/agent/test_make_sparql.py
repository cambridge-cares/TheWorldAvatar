import pytest

from model.aggregate import AggregateOperator
from model.constraint import ExtremeValueConstraint
from services.connectors.sg_factories.model import (
    FactoryIndustryKey,
    FactoryNumAttrKey,
    Industry,
)
from services.connectors.sg_factories.agent.make_sparql import SGFactoriesSPARQLMaker


@pytest.fixture(scope="class")
def sg_factories_sparql_maker():
    yield SGFactoriesSPARQLMaker(
        [
            "http://www.theworldavatar.com/kg/ontochemplant#ChemicalPlant",
            "http://www.theworldavatar.com/kg/ontocompany#FoodPlant",
            "http://www.theworldavatar.com/kg/ontocompany#SemiconductorPlant",
        ]
    )


class TestSGFactoriesSPARQLMaker:
    def test_lookupFactoryAttribute_industry(
        self, sg_factories_sparql_maker: SGFactoriesSPARQLMaker
    ):
        # Act
        actual = sg_factories_sparql_maker.lookup_factory_attribute(
            iris=["http://test.com/1", "http://test.com/2"],
            attr_key=FactoryIndustryKey.INDUSTRY,
        )

        # Assert
        assert (
            actual
            == """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany#>

SELECT DISTINCT ?IRI ?Industry WHERE {
VALUES ?IRI { <http://test.com/1> <http://test.com/2> }
?IRI ontocompany:belongsToIndustry/rdf:type ?Industry .
}"""
        )

    def test_lookupFactoryAttribute_numAttr(
        self, sg_factories_sparql_maker: SGFactoriesSPARQLMaker
    ):
        # Act
        actual = sg_factories_sparql_maker.lookup_factory_attribute(
            iris=["http://test.com/1", "http://test.com/2"],
            attr_key=FactoryNumAttrKey.DESIGN_CAPACITY,
        )

        # Assert
        assert (
            actual
            == """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany#>

SELECT DISTINCT ?IRI ?DesignCapacityValue ?DesignCapacityUnit WHERE {
VALUES ?IRI { <http://test.com/1> <http://test.com/2> }
?IRI ontocompany:hasDesignCapacity/om:hasValue ?DesignCapacity .
?DesignCapacity om:hasNumericalValue ?DesignCapacityValue .
OPTIONAL { ?DesignCapacity om:hasUnit/skos:notation ?DesignCapacityUnit . }
}"""
        )

    def test_findFactories(self, sg_factories_sparql_maker: SGFactoriesSPARQLMaker):
        # Arrange
        industry = Industry.FOOD
        constraints = {
            FactoryNumAttrKey.GENERATED_HEAT: ExtremeValueConstraint.MIN,
            FactoryNumAttrKey.THERMAL_EFFICIENCY: ExtremeValueConstraint.MAX,
        }
        limit = 3

        expected = """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany#>

SELECT DISTINCT ?IRI ?GeneratedHeatValue ?GeneratedHeatUnit ?ThermalEfficiencyValue ?ThermalEfficiencyUnit WHERE {
VALUES ?Type { <http://www.theworldavatar.com/kg/ontochemplant#ChemicalPlant> <http://www.theworldavatar.com/kg/ontocompany#FoodPlant> <http://www.theworldavatar.com/kg/ontocompany#SemiconductorPlant> }
?IRI rdf:type ?Type .
?IRI ontocompany:belongsToIndustry/rdf:type ontocompany:FoodIndustry .
?IRI ontocompany:hasGeneratedHeat/om:hasValue ?GeneratedHeat .
?GeneratedHeat om:hasNumericalValue ?GeneratedHeatValue .
OPTIONAL { ?GeneratedHeat om:hasUnit/skos:notation ?GeneratedHeatUnit . }
?IRI ontocompany:hasThermalEfficiency/om:hasValue ?ThermalEfficiency .
?ThermalEfficiency om:hasNumericalValue ?ThermalEfficiencyValue .
OPTIONAL { ?ThermalEfficiency om:hasUnit/skos:notation ?ThermalEfficiencyUnit . }
}
ORDER BY ?GeneratedHeatValue DESC(?ThermalEfficiencyValue)
LIMIT 3"""

        # Act
        actual = sg_factories_sparql_maker.find_factories(
            industry=industry,
            numattr_constraints=constraints,
            limit=limit,
        )

        # Assert
        assert actual == expected

    def test_countFactories(
        self, sg_factories_sparql_maker: SGFactoriesSPARQLMaker
    ):
        # Act
        actual = sg_factories_sparql_maker.count_factories(industry=Industry.CHEMICAL)

        # Assert
        assert (
            actual
            == """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany#>

SELECT (COUNT(?IRI) AS ?Count) WHERE {
VALUES ?Type { <http://www.theworldavatar.com/kg/ontochemplant#ChemicalPlant> <http://www.theworldavatar.com/kg/ontocompany#FoodPlant> <http://www.theworldavatar.com/kg/ontocompany#SemiconductorPlant> }
?IRI rdf:type ?Type .
?IRI ontocompany:belongsToIndustry/rdf:type ontocompany:ChemicalIndustry .
}"""
        )

    def test_computeAggregateFactoryAttribute(
        self, sg_factories_sparql_maker: SGFactoriesSPARQLMaker
    ):
        # Act
        actual = sg_factories_sparql_maker.compute_aggregate_factory_attribute(
            industry=Industry.SEMICONDUCTOR,
            attr_agg=(FactoryNumAttrKey.DESIGN_CAPACITY, AggregateOperator.AVG),
        )

        # Assert
        assert (
            actual
            == """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany#>
PREFIX ontochemplant: <http://www.theworldavatar.com/kg/ontochemplant#>

SELECT (AVG(?DesignCapacityValue) AS ?DesignCapacityValueAVG) ?DesignCapacityUnit WHERE {
VALUES ?Type { <http://www.theworldavatar.com/kg/ontochemplant#ChemicalPlant> <http://www.theworldavatar.com/kg/ontocompany#FoodPlant> <http://www.theworldavatar.com/kg/ontocompany#SemiconductorPlant> }
?IRI rdf:type ?Type .
?IRI ontocompany:belongsToIndustry/rdf:type ontocompany:SemiconductorIndustry .
?IRI ontocompany:hasDesignCapacity/om:hasValue ?DesignCapacity .
?DesignCapacity om:hasNumericalValue ?DesignCapacityValue .
OPTIONAL { ?DesignCapacity om:hasUnit/skos:notation ?DesignCapacityUnit . }
}
GROUP BY ?DesignCapacityUnit"""
        )
