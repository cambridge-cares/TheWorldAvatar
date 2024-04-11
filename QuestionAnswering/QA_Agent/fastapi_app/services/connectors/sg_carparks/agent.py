from typing import Annotated
from fastapi import Depends

from model.qa import QAData
from services.utils.rdf import flatten_sparql_response
from services.core.kg import KgClient
from services.connectors.feature_info_client import (
    FeatureInfoClient,
    get_featureInfoClient,
)
from .kg import get_sgCarparks_bgClient


class SGCarparksAgent:
    def __init__(self, bg_client: KgClient, feature_info_client: FeatureInfoClient):
        self.bg_client = bg_client
        self.feature_info_client = feature_info_client

    def find_nearest_carpark(self, lat: str, lon: str, limit: int = 1):
        """ """
        query = """PREFIX carpark:	<https://www.theworldavatar.com/kg/ontocarpark/>
PREFIX geo: <http://www.opengis.net/ont/geosparql#>
PREFIX geof: <http://www.opengis.net/def/function/geosparql/>
PREFIX unit: <http://qudt.org/vocab/unit/>
        
SELECT * WHERE {{
  ?Carpark a carpark:Carpark; rdfs:label ?Label .
  SERVICE <http://sg-ontop:8080/sparql> {{
    SELECT (geof:distance(?Coords, "<http://www.opengis.net/def/crs/OGC/1.3/CRS84> POINT({lon} {lat})"^^geo:wktLiteral, <http://www.opengis.net/def/uom/OGC/1.0/metre>) as ?Distance) ?Carpark ?Coords
  	WHERE {{
      ?Carpark geo:hasGeometry/geo:asWKT ?Coords
    }}
  }}
}}
ORDER BY ASC(?Distance)
LIMIT {limit}""".format(
            lat=lat, lon=lon, limit=limit
        )

        res = self.bg_client.query(query)
        vars, bindings = flatten_sparql_response(res)

        vars.extend(["LotAvailability", "Time"])
        for binding in bindings:
            res = self.feature_info_client.query(
                iri=binding["Carpark"],
                endpoint="http://sg-blazegraph:8080/blazegraph/namespace/carpark/sparql",
            )
            binding["LotAvailability"] = res["time"][0]["values"][0][-1]
            binding["Time"] = res["time"][0]["time"][-1]

        return QAData(vars=vars, bindings=bindings)


def get_sgCarpark_agent(
    bg_client: Annotated[KgClient, Depends(get_sgCarparks_bgClient)],
    feature_info_client: Annotated[FeatureInfoClient, Depends(get_featureInfoClient)],
):
    return SGCarparksAgent(bg_client=bg_client, feature_info_client=feature_info_client)
