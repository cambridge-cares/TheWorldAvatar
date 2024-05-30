import pytest

from services.example_store import Nlq2DataReqExample, ExampleStore


@pytest.fixture(scope="class")
def nlq2action_retriever(redis_client, embedder):
    examples = [
        Nlq2DataReqExample(
            nlq="Find the plots with a maximum permitted GFA greater than 500 square meters.",
            data_req={
                "sparql": {
                    "namespace": "ontop",
                    "query": "SELECT ?Plot ?GFAValue ?GFAUnit WHERE {\n  ?Plot rdf:type ontoplot:Plot .\n  ?Plot ontoplot:hasMaximumPermittedGFA/om:hasValue [ om:hasNumericalValue  ?GFAValue ; om:hasUnit ?GFAUnit ] .\n  FILTER (?GFAValue > 500)\n}",
                }
            },
        ),
        Nlq2DataReqExample(
            nlq="Identify residential plots with a maximum permitted GFA less than 1000 square meters.",
            data_req={
                "sparql": {
                    "namespace": "ontop",
                    "query": 'SELECT ?Plot ?GFAValue ?GFAUnit WHERE {\n  ?Plot rdf:type ontoplot:Plot .\n  VALUES ?LandUseType { <LandUseType:"residential"> }\n  ?Plot ontozoning:hasLandUseType ?LandUseType .\n  ?Plot ontoplot:hasMaximumPermittedGFA/om:hasValue [ om:hasNumericalValue  ?GFAValue ; om:hasUnit ?GFAUnit ] .\n  FILTER (?GFAValue > 500)\n}',
                }
            },
        ),
        Nlq2DataReqExample(
            nlq="Retrieve the top 5 plots with the largest plot area.",
            data_req={
                "sparql": {
                    "namespace": "ontop",
                    "query": "SELECT ?Plot ?PlotAreaValue ?PlotAreaUnit WHERE {\n    ?Plot rdf:type ontoplot:Plot .\n    ?Plot ontoplot:hasPlotArea/om:value [ om:hasNumericalValue ?PlotAreaValue ; om:hasUnit ?PlotAreaUnit ] .\n}\nORDER BY DESC(?PlotAreaValue)\nLIMIT 5",
                }
            },
        ),
    ]

    yield ExampleStore(
        redis_client=redis_client, embedder=embedder, examples=iter(examples)
    )


class TestNlq2ActionRetriever:
    def test_retrieveExamples(self, nlq2action_retriever: ExampleStore):
        # Arrange
        nlq = "Identify residential plots with a maximum permitted GFA more than 700 square meters."

        # Act
        actual = nlq2action_retriever.retrieve_examples(nlq, k=1)

        # Assert
        actual == [
            Nlq2DataReqExample(
                nlq="Identify residential plots with a maximum permitted GFA less than 1000 square meters.",
                data_req={
                    "sparql": {
                        "namespace": "ontop",
                        "query": 'SELECT ?Plot ?GFAValue ?GFAUnit WHERE {\n  ?Plot rdf:type ontoplot:Plot .\n  VALUES ?LandUseType { <LandUseType:"residential"> }\n  ?Plot ontozoning:hasLandUseType ?LandUseType .\n  ?Plot ontoplot:hasMaximumPermittedGFA/om:hasValue [ om:hasNumericalValue  ?GFAValue ; om:hasUnit ?GFAUnit ] .\n  FILTER (?GFAValue > 500)\n}',
                    }
                },
            )
        ]
