import os
from typing import Annotated

from fastapi import Depends

from utils.rdf import flatten_sparql_select_response
from services.kg import KgClient, get_sgCarpark_bgClient


class NearestCarparkLocator:
    def __init__(self, bg_client: KgClient, sg_stack_internal_ontop_endpoint: str):
        self.bg_client = bg_client
        self.sg_stack_internal_ontop_endpoint = sg_stack_internal_ontop_endpoint

    def locate(self, lat: str, lon: str):
        query = """PREFIX carpark:	<https://www.theworldavatar.com/kg/ontocarpark/>
PREFIX geo: <http://www.opengis.net/ont/geosparql#>
PREFIX geof: <http://www.opengis.net/def/function/geosparql/>
PREFIX unit: <http://qudt.org/vocab/unit/>
        
SELECT * WHERE {{
    ?Carpark a carpark:Carpark; rdfs:label ?Label .
    SERVICE <{sg_stack_internal_ontop_endpoint}> {{
        SELECT (geof:distance(?Coords, "<http://www.opengis.net/def/crs/OGC/1.3/CRS84> POINT({lon} {lat})"^^geo:wktLiteral, <http://www.opengis.net/def/uom/OGC/1.0/metre>) AS ?Distance) ?Carpark ?Coords
        WHERE {{
            ?Carpark geo:hasGeometry/geo:asWKT ?Coords
        }}
    }}
}}
ORDER BY ASC(?Distance)
LIMIT 1""".format(
            sg_stack_internal_ontop_endpoint=self.sg_stack_internal_ontop_endpoint,
            lat=lat,
            lon=lon,
        )

        bg_res = self.bg_client.querySelect(query)
        return flatten_sparql_select_response(bg_res)


def get_nearestCarpark_locator(
    bg_client: Annotated[KgClient, Depends(get_sgCarpark_bgClient)],
):
    return NearestCarparkLocator(
        bg_client=bg_client,
        sg_stack_internal_ontop_endpoint=os.environ["SG_STACK_INTERNAL_ONTOP_ENDPOINT"],
    )
