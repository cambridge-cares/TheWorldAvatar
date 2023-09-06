import numpy as np
import tritonclient.http as httpclient
from SPARQLWrapper import POST, SPARQLWrapper, JSON


class TranslationClient:
    def __init__(self, triton_endpoint: str = "0.0.0.0:8000"):
        self.client = httpclient.InferenceServerClient(url=triton_endpoint)

    def translate(self, text: str):
        text = np.array(
            [text],
            dtype=object,
        )

        # Set Inputs
        input_tensors = [httpclient.InferInput("text", text.shape, datatype="BYTES")]
        input_tensors[0].set_data_from_numpy(text)

        # Query
        query_response = self.client.infer(
            model_name="translation", inputs=input_tensors
        )

        # Output
        output_compact = query_response.as_numpy("output_compact").astype(str)[0]
        output_verbose = query_response.as_numpy("output_verbose").astype(str)[0]

        return dict(sparql_query=output_verbose, sparql_query_compact=output_compact)


class KgClient:
    QUERY_PREFIXES = (
        "PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>\n"
        "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n"
        "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n"
    )

    def __init__(
        self,
        kg_endpoint: str = "http://178.128.105.213:3838/blazegraph/namespace/ontospecies/sparql",
    ):
        sparql = SPARQLWrapper(kg_endpoint)
        sparql.setReturnFormat(JSON)
        sparql.setMethod(POST)
        self.sparql = sparql

    def query(self, query: str):
        """Queries the KG and returns the response with the following format
        {
            "head": {
                "vars": List[str]
            },
            "results": {
                "bindings": [
                    Dict[
                        str,
                        {
                            "datatype": NotRequired[str],
                            "type": str
                            "value" str
                        }
                    ]
                ]
            }
        }
        """
        query = query.strip()
        if not query.startswith("PREFIX"):
            query = self.QUERY_PREFIXES + query

        self.sparql.setQuery(query)
        return self.sparql.queryAndConvert()
