import pytest
from model.constraint import ExtremeValueConstraint
from services.connectors.sg_factories.model import FactoryAttrKey, FactoryConstraints, Industry
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
    def test_lookupFactoryAttribute(self, sg_factories_sparql_maker: SGFactoriesSPARQLMaker):
        # Act
        actual = sg_factories_sparql_maker.lookup_factory_attribute(iris=["http://test.com/1", "http://test.com/2"], attr_key=FactoryAttrKey.INDUSTRY)

        # Assert
        assert actual == """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany#>

SELECT DISTINCT ?IRI ?Industry WHERE {
VALUES ?IRI { <http://test.com/1> <http://test.com/2> }
?IRI ontocompany:belongsToIndustry/rdf:type ?Industry .
}"""

    def test_findFactories(self, sg_factories_sparql_maker: SGFactoriesSPARQLMaker):
        # Arrange
        constraints = FactoryConstraints(
            industry=Industry.FOOD,
            generated_heat=ExtremeValueConstraint.MIN,
            thermal_efficiency=ExtremeValueConstraint.MAX,
        )

        # Act
        actual = sg_factories_sparql_maker.find_factories(constraints=constraints, limit=3)

        # Assert
        assert (
            actual
            == """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany#>

SELECT DISTINCT ?IRI ?GeneratedHeatNumericalValue ?ThermalEfficiencyNumericalValue WHERE {
VALUES ?Type { <http://www.theworldavatar.com/kg/ontochemplant#ChemicalPlant> <http://www.theworldavatar.com/kg/ontocompany#FoodPlant> <http://www.theworldavatar.com/kg/ontocompany#SemiconductorPlant> }
?IRI rdf:type ?Type .
?IRI ontocompany:belongsToIndustry/rdf:type ontocompany:FoodIndustry .
?IRI ontocompany:hasGeneratedHeat/om:hasValue [ om:hasNumericalValue ?GeneratedHeatNumericalValue ; om:hasUnit/skos:notation "MW" ] .
?IRI ontocompany:hasThermalEfficiency/om:hasValue/om:hasNumericalValue ?ThermalEfficiencyNumericalValue .
}
ORDER BY ?GeneratedHeatNumericalValue DESC(?ThermalEfficiencyNumericalValue)
LIMIT 3"""
        )

    def test_countFactories(self, sg_factories_sparql_maker: SGFactoriesSPARQLMaker):
        # Act
        actual = sg_factories_sparql_maker.count_factories(
            industry=Industry.CHEMICAL, groupby_industry=True
        )

        # Assert
        assert (
            actual
            == """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany#>

SELECT (COUNT(?IRI) AS ?Count) ?Industry WHERE {
VALUES ?Type { <http://www.theworldavatar.com/kg/ontochemplant#ChemicalPlant> <http://www.theworldavatar.com/kg/ontocompany#FoodPlant> <http://www.theworldavatar.com/kg/ontocompany#SemiconductorPlant> }
?IRI rdf:type ?Type .
?IRI ontocompany:belongsToIndustry/rdf:type ontocompany:ChemicalIndustry .
?IRI ontocompany:belongsToIndustry/rdf:type ?Industry .
}
GROUP BY ?Industry"""
        )
