from typing import Annotated

from fastapi import Depends

from config import AppSettings, get_app_settings
from services.sparql import SparqlClient, get_sgCarpark_endpoint


class NearestCarparkLocator:
    def __init__(self, carpark_endpoint: str, endpoint_singapore_ontop_internal: str):
        self.sparql_client = SparqlClient(carpark_endpoint)
        self.endpoint_singapore_ontop_internal = endpoint_singapore_ontop_internal

    def locate(self, lat: str, lon: str):
        query = """PREFIX carpark:	<https://www.theworldavatar.com/kg/ontocarpark/>
PREFIX geo: <http://www.opengis.net/ont/geosparql#>
PREFIX geof: <http://www.opengis.net/def/function/geosparql/>
PREFIX unit: <http://qudt.org/vocab/unit/>
        
SELECT * WHERE {{
    ?Carpark a carpark:Carpark; rdfs:label ?Label .
    SERVICE <{endpoint_sparql_sg_ontop_internal}> {{
        SELECT (geof:distance(?Coords, "<http://www.opengis.net/def/crs/OGC/1.3/CRS84> POINT({lon} {lat})"^^geo:wktLiteral, <http://www.opengis.net/def/uom/OGC/1.0/metre>) AS ?Distance) ?Carpark ?Coords
        WHERE {{
            ?Carpark geo:hasGeometry/geo:asWKT ?Coords
        }}
    }}
}}
ORDER BY ASC(?Distance)
LIMIT 1""".format(
            endpoint_sparql_sg_ontop_internal=self.endpoint_singapore_ontop_internal,
            lat=lat,
            lon=lon,
        )

        return self.sparql_client.querySelectThenFlatten(query)


def get_nearestCarpark_locator(
    endpoint: Annotated[str, Depends(get_sgCarpark_endpoint)],
    settings: Annotated[AppSettings, Depends(get_app_settings)],
):
    return NearestCarparkLocator(
        carpark_endpoint=endpoint,
        endpoint_singapore_ontop_internal=settings.singapore_endpoints.ontop_internal,
    )
