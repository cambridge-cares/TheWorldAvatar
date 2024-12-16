import os
from typing import Optional
import numpy as np

import pandas as pd
from constants.fs import ROOTDIR
from locate_then_ask.ontobuiltenv.model import IctAddress


class IctAddressSynthesizer:
    FILEPATH = os.path.join(ROOTDIR, "data/ontobuiltenv/Addresses.csv")

    def __init__(self, kg_endpoint: Optional[str] = None):
        if not os.path.exists(self.FILEPATH):
            if not kg_endpoint:
                raise ValueError(
                    "No cache for addresses found, `kg_endpoint` must be provided."
                )

            from locate_then_ask.kg_client import KgClient

            kg_client = KgClient(kg_endpoint)
            query = """PREFIX ict: 	  <http://ontology.eil.utoronto.ca/icontact.owl#>
PREFIX obe:       <https://www.theworldavatar.com/kg/ontobuiltenv/>

SELECT * WHERE {
  ?Address a ict:Address
  OPTIONAL {
    ?Address ict:hasStreet ?Street .
  }
  OPTIONAL {
    ?Address ict:hasStreetNumber ?StreetNumber .
  }
  OPTIONAL {
    ?Address ict:hasUnitName ?UnitName .
  }
  OPTIONAL {
    ?Address obe:hasPostalCode/rdfs:label ?PostalCodeLabel .
  }
}
ORDER BY RAND()
LIMIT 100"""
            bindings = kg_client.query(query)["results"]["bindings"]
            data = [
                {
                    key: binding.get(key, {}).get("value", None)
                    for key in [
                        "Address",
                        "Street",
                        "StreetNumber",
                        "UnitName",
                        "PostalCodeLabel",
                    ]
                }
                for binding in bindings
            ]
            df = pd.DataFrame(data)
            df.to_csv(self.FILEPATH, index=False)
        else:
            df = pd.read_csv(self.FILEPATH)
        df.replace({np.nan: None}, inplace=True)
        self.df = df

    def make(self) -> IctAddress:
        row = self.df.sample(n=1).iloc[0]

        return IctAddress(
            street=row["Street"],
            street_number=row["StreetNumber"],
            unit_name=row["UnitName"],
            postal_code=row["PostalCodeLabel"],
        )
