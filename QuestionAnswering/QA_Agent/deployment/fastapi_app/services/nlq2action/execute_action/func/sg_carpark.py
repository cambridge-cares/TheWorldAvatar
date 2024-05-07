import logging
import time
from typing import Annotated, List

from fastapi import Depends

from model.qa import QAStep
from core.kg import KgClient
from utils.rdf import flatten_sparql_response
from services.kg import get_sgCarpark_bgClient
from services.feature_info_client import FeatureInfoClient, get_featureInfoClient
from services.geocoding import IGeocoder, get_geocoder
from services.support_data import TableDataItem
from services.nlq2action.execute_action.func.base import Name2Func

logger = logging.getLogger(__name__)


class SGCarParkFuncExecutor(Name2Func):
    def __init__(
        self,
        bg_client: KgClient,
        feature_info_client: FeatureInfoClient,
        geocoder: IGeocoder,
    ):
        self.bg_client = bg_client
        self.feature_info_client = feature_info_client
        self.geocoder = geocoder

    def get_name2func(self):
        return {"find_nearest_carpark": self.find_nearest_carpark}

    def find_nearest_carpark(self, location: str):
        steps: List[QAStep] = []

        logger.info("Get coordinates for the location: " + location)
        timestamp = time.time()
        place = self.geocoder.search(location)
        latency = time.time() - timestamp
        logger.info("Geo-decoded data: " + str(place))
        steps.append(
            QAStep(
                action="geodecode",
                arguments=location,
                results=str(place),
                latency=latency,
            )
        )

        timestamp = time.time()
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
    LIMIT 1""".format(
            lat=place.lat, lon=place.lon
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

        data = [TableDataItem(vars=vars, bindings=bindings)]

        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="get_nearest_carpark",
                arguments=str(place),
                latency=latency,
            )
        )

        return steps, data


def get_sgCarpark_funcExec(
    bg_client: Annotated[KgClient, Depends(get_sgCarpark_bgClient)],
    feature_info_client: Annotated[FeatureInfoClient, Depends(get_featureInfoClient)],
    geocoder: Annotated[IGeocoder, Depends(get_geocoder)],
):
    return SGCarParkFuncExecutor(
        bg_client=bg_client, feature_info_client=feature_info_client, geocoder=geocoder
    )
