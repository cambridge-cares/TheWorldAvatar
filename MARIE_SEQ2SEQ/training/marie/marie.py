from marie.kg_client import KgClient
from marie.translation import TranslationModel


class Marie:
    def __init__(
        self,
        model_path: str = "google/flan-t5-base",
        kg_endpoint: str = "http://178.128.105.213:3838/blazegraph/namespace/ontospecies/sparql",
    ):
        self.nl2sparql_model = TranslationModel(model_path)
        self.kg_client = KgClient(kg_endpoint)

    def get_answer(self, question: str):
        query = self.nl2sparql_model(question)
        return self.kg_client.query(query)
