import pytest

from controllers.qa.retrieve import Nlq2ActionExample, Nlq2ActionRetriever


@pytest.fixture(scope="class")
def nlq2action_retriever(redis_client, embedder):
    examples = [
        Nlq2ActionExample(
            nlq="Find the plots with a maximum permitted GFA greater than 500 square meters.",
            action={
                "sparql": {
                    "namespace": "ontop",
                    "query": "SELECT ?Plot ?GFAValue ?GFAUnit WHERE {\n  ?Plot rdf:type ontoplot:Plot .\n  ?Plot ontoplot:hasMaximumPermittedGFA/om:hasValue [ om:hasNumericalValue  ?GFAValue ; om:hasUnit ?GFAUnit ] .\n  FILTER (?GFAValue > 500)\n}",
                }
            },
        ),
        Nlq2ActionExample(
            nlq="Identify residential plots with a maximum permitted GFA less than 1000 square meters.",
            action={
                "sparql": {
                    "namespace": "ontop",
                    "query": 'SELECT ?Plot ?GFAValue ?GFAUnit WHERE {\n  ?Plot rdf:type ontoplot:Plot .\n  VALUES ?LandUseType { <LandUseType:"residential"> }\n  ?Plot ontozoning:hasLandUseType ?LandUseType .\n  ?Plot ontoplot:hasMaximumPermittedGFA/om:hasValue [ om:hasNumericalValue  ?GFAValue ; om:hasUnit ?GFAUnit ] .\n  FILTER (?GFAValue > 500)\n}',
                }
            },
        ),
        Nlq2ActionExample(
            nlq="Retrieve the top 5 plots with the largest plot area.",
            action={
                "sparql": {
                    "namespace": "ontop",
                    "query": "SELECT ?Plot ?PlotAreaValue ?PlotAreaUnit WHERE {\n    ?Plot rdf:type ontoplot:Plot .\n    ?Plot ontoplot:hasPlotArea/om:value [ om:hasNumericalValue ?PlotAreaValue ; om:hasUnit ?PlotAreaUnit ] .\n}\nORDER BY DESC(?PlotAreaValue)\nLIMIT 5",
                }
            },
        ),
    ]

    yield Nlq2ActionRetriever(
        redis_client=redis_client, embedder=embedder, examples=iter(examples)
    )


class TestNlq2ActionRetriever:
    def test_retrieveExamples(self, nlq2action_retriever: Nlq2ActionRetriever):
        # Arrange
        nlq = "Identify residential plots with a maximum permitted GFA more than 700 square meters."

        # Act
        actual = nlq2action_retriever.retrieve_examples(nlq, k=1)

        # Assert
        actual == [
            Nlq2ActionExample(
                nlq="Identify residential plots with a maximum permitted GFA less than 1000 square meters.",
                action={
                    "sparql": {
                        "namespace": "ontop",
                        "query": 'SELECT ?Plot ?GFAValue ?GFAUnit WHERE {\n  ?Plot rdf:type ontoplot:Plot .\n  VALUES ?LandUseType { <LandUseType:"residential"> }\n  ?Plot ontozoning:hasLandUseType ?LandUseType .\n  ?Plot ontoplot:hasMaximumPermittedGFA/om:hasValue [ om:hasNumericalValue  ?GFAValue ; om:hasUnit ?GFAUnit ] .\n  FILTER (?GFAValue > 500)\n}',
                    }
                },
            )
        ]
