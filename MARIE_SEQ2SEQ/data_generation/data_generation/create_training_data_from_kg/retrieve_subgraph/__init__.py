import random
from typing import Dict, Optional

from SPARQLWrapper import SPARQLWrapper, JSON, POST

from .construct_query import RetrieveQueryConstructor
from .parse_kg_response import KgResponseParser
from .timeout import timeout


class SubgraphRetriever:
    def __init__(self, kg_endpoint: str):
        sparql_client = SPARQLWrapper(kg_endpoint)
        sparql_client.setReturnFormat(JSON)
        sparql_client.setMethod(POST)
        self.sparql_client = sparql_client
        self.query_constructor = RetrieveQueryConstructor()
        self.kg_response_parser = KgResponseParser()

    @timeout(5, default_value=None)
    def get_subgraph(
        self,
        tail_nums: Optional[Dict[str, int]] = None,
        bindings: Optional[Dict[str, str]] = None,
    ):
        results = []

        try_num = 0
        try_limit = 5 if not bindings else 1
        while len(results) == 0 and try_num < try_limit:
            if tail_nums is None:
                tail_nums = self.get_rand_tail_nums()
            sparql_query = self.query_constructor.construct(
                tail_nums=tail_nums, bindings=bindings
            )

            self.sparql_client.setQuery(sparql_query)
            results = self.sparql_client.queryAndConvert()["results"]["bindings"]
            try_num += 1

        if len(results) == 0:
            return None

        return self.kg_response_parser.to_subgraph(results[0])

    def get_rand_tail_nums(self):
        property_num = random.randint(0, 3)
        identifier_num = random.randint(property_num, 3) - property_num

        id_prop_num = identifier_num + property_num
        if id_prop_num == 0:
            use_num, chemicalclass_num = random.choice([(0, 1), (1, 0)])
        elif id_prop_num == 1:
            use_num, chemicalclass_num = random.choice([(0, 0), (0, 1), (1, 0), (1, 1)])
        elif id_prop_num == 2:
            use_num, chemicalclass_num = random.choice([(0, 0), (0, 1), (1, 0)])
        else:
            use_num, chemicalclass_num = 0, 0

        return dict(
            property_num=property_num,
            identifier_num=identifier_num,
            use_num=use_num,
            chemicalclass_num=chemicalclass_num,
        )

    def get_rand_tail_nums_non_identifier(self):
        property_num = random.randint(0, 3)
        use_num = random.randint(property_num, 3) - property_num
        chemicalclass_num = (
            random.randint(property_num + use_num, 3) - property_num - use_num
        )
        return dict(
            property_num=property_num,
            use_num=use_num,
            chemicalclass_num=chemicalclass_num,
        )
