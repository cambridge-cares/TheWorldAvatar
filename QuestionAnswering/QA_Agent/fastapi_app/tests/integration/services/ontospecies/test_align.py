from services.retrieve_docs import DocsRetriever
from services.embed import TritonMPNetEmbedder
from services.kg_client import KgClient
from services.connector.ontospecies.align import OntoSpeciesAligner


class MockKgClient(KgClient):
    def __init__(self, returned_data: dict):
        self.returned_data = returned_data

    def query(self, query: str):
        return dict(self.returned_data)


class TestOntoSpeciesAligner:
    def test_(self):
        # Arrange
        kg_client = MockKgClient(
            returned_data={
                "results": {
                    "bindings": [
                        {"Label": {"value": x}}
                        for x in ["dithiane", "dithianes", "alcohol"]
                    ]
                }
            }
        )
        embedder = TritonMPNetEmbedder(url="localhost:8001")
        docs_retriever = DocsRetriever(embedder)
        aligner = OntoSpeciesAligner(kg_client=kg_client, docs_retriever=docs_retriever)

        chemical_classes = ["Dithiane", "alcohol compound"]

        # Act
        aligned_chemical_classes = aligner.align_chemical_classes(chemical_classes)

        # Assert
        assert aligned_chemical_classes == [["dithiane", "dithianes"], ["alcohol"]]