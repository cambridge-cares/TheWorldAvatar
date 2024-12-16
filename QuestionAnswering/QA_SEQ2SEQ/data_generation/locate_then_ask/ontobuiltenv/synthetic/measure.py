from decimal import Decimal
import os
from typing import Optional

import pandas as pd
from constants.fs import ROOTDIR

from locate_then_ask.ontobuiltenv.model import OmMeasure


class OmMeasureSynthesizer:
    DIRPATH = os.path.join(ROOTDIR, "data/ontobuiltenv/")

    def __init__(self, key: str, kg_endpoint: Optional[str] = None):
        filepath = os.path.join(self.DIRPATH, key + ".csv")
        if not os.path.exists(filepath):
            if not kg_endpoint:
                raise ValueError(
                    "No cache for addresses found, `kg_endpoint` must be provided."
                )

            from locate_then_ask.kg_client import KgClient

            kg_client = KgClient(kg_endpoint)
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
            numerical_value=Decimal(str(sample["NumericalValue"])),
            unit_iri=sample["Unit"],
        )
