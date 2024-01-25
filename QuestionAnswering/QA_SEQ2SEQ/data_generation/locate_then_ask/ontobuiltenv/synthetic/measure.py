from decimal import Decimal
import os

import pandas as pd
from constants.fs import ROOTDIR

from locate_then_ask.ontobuiltenv.model import OmMeasure


class OmMeasureSynthesizer:
    DIRPATH = os.path.join(ROOTDIR, "data/ontobuiltenv/")

    def __init__(self, key: str):
        filepath = os.path.join(self.DIRPATH, key + ".csv")
        if not os.path.exists(filepath):
            from locate_then_ask.kg_client import KgClient

            kg_client = KgClient(
                "http://165.232.172.16:3838/blazegraph/namespace/kingslynn/sparql"
            )
            template = """PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX obe: <https://www.theworldavatar.com/kg/ontobuiltenv/>
        
SELECT DISTINCT ?NumericalValue ?Unit WHERE {{
    ?s obe:has{Prop}/om:hasValue [ om:hasNumericalValue ?NumericalValue ; om:hasUnit ?Unit ]
}}
ORDER BY RAND()
LIMIT 100"""

            query = template.format(Prop=key)
            bindings = kg_client.query(query)["results"]["bindings"]
            data = [
                {key: binding[key]["value"] for key in ["NumericalValue", "Unit"]}
                for binding in bindings
            ]
            df = pd.DataFrame(data)
            df.to_csv(filepath)
        else:
            df = pd.read_csv(filepath)
        self.df = df

    def make(self):
        sample = self.df.sample(1).iloc[0]
        return OmMeasure(
            numerical_value=Decimal(str(sample["NumericalValue"])), unit_iri=sample["Unit"]
        )
