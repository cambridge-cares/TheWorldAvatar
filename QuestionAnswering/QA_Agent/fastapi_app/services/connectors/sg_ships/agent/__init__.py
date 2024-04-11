from dataclasses import asdict
from functools import cache
from typing import Annotated, Optional

from fastapi import Depends
from redis import Redis

from model.qa import QAData
from services.core.kg import KgClient
from services.core.redis import get_redis_client
from services.connectors.sg import get_sgDispersion_bgClient
from services.connectors.feature_info_client import (
    FeatureInfoClient,
    get_featureInfoClient,
)
from .link_entity import ShipIdentifier, ShipLinker


class SGShipsAgent:
    def __init__(self, ship_linker: ShipLinker, feature_info_client: FeatureInfoClient):
        self.linker = ship_linker
        self.feature_info_client = feature_info_client

    def _link(self, name: Optional[str], mmsi: Optional[str]):
        # mmsi takes precedence and is matched exactly
        if mmsi:
            ship = self.linker.lookup_by_mmsi(mmsi)
            if not ship:
                ships = []
            else:
                ships = [ship]
        elif name:
            ships = self.linker.lookup_by_name(name)
        else:
            raise ValueError("Either `name` or `mmsi` must not be None")

        return ships

    def _sanitise(self, text: str):
        text = text.split("^^", maxsplit=1)[0]
        if text.startswith('"') and text.endswith('"'):
            text = text[1:-1]
        return text

    def lookup_ship_attributes(
        self, name: Optional[str] = None, mmsi: Optional[str] = None
    ):
        """
        Given ship name or MMSI, returns MMSI, maximum static draught, dimension, IMO number, ship type, call sign
        """
        ships = self._link(name, mmsi)

        vars = ["IRI", "MMSI", "name"]
        vars_set = set(vars)
        bindings = []

        for ship in ships:
            ship_data: dict = self.feature_info_client.query(ship.IRI)["meta"]
            ship_data = {k: self._sanitise(v) for k, v in ship_data.items()}
            bindings.append({**asdict(ship), **ship_data})

            for key in ship_data.keys():
                if key not in vars_set:
                    vars_set.add(key)
                    vars.append(key)

        return QAData(vars=vars, bindings=bindings)

    def lookup_ship_timeseries(self, name: Optional[str], mmsi: Optional[str] = None):
        """
        Given ship name or MMSI, returns speed over ground, course over ground, longitude, latitude
        """
        ships = self._link(name, mmsi)

        vars = ["IRI", "MMSI", "name", "key", "timeseries"]
        bindings = []

        for ship in ships:
            ship_data = self.feature_info_client.query(ship.IRI)["time"][0]
            """
            {
                "data": [string, ...],
                "values": [[number, ...], ...],
                "units": [string, ...],
                "time": [string, ...]
            }
            """

            for i, key in enumerate(ship_data["data"]):
                bindings.append(
                    {
                        **asdict(ship),
                        **dict(
                            key="{key} ({unit})".format(key=key, unit=ship_data["units"][i]),
                            timeseries=list(
                                zip(ship_data["time"], ship_data["values"][i])
                            ),
                        ),
                    }
                )

        title_tokens = ["{key}", "of"]
        if name:
            title_tokens.append(name)
        if mmsi:
            title_tokens.append("(MMSI: {mmsi})".format(mmsi=mmsi))

        return QAData(
            title_template=" ".join(title_tokens), vars=vars, bindings=bindings
        )


def find_all_ships(kg_client: KgClient):
    query = """PREFIX disp: <https://www.theworldavatar.com/kg/ontodispersion/>
PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>

SELECT ?IRI ?MMSI ?name WHERE {
    ?IRI a disp:Ship ; 
         rdfs:label ?name ; 
         disp:hasProperty [ 
            a disp:MMSI; 
            om:hasValue/om:hasNumericalValue ?MMSI 
         ]
}"""

    for binding in kg_client.query(query)["results"]["bindings"]:
        yield ShipIdentifier(
            IRI=binding["IRI"]["value"],
            MMSI=binding["MMSI"]["value"],
            name=binding["name"]["value"],
        )


@cache
def get_sgShips_agent(
    redis_client: Annotated[Redis, Depends(get_redis_client)],
    bg_client: Annotated[KgClient, Depends(get_sgDispersion_bgClient)],
    feature_info_client: Annotated[FeatureInfoClient, Depends(get_featureInfoClient)],
):
    return SGShipsAgent(
        ship_linker=ShipLinker(redis_client, ships=find_all_ships(bg_client)),
        feature_info_client=feature_info_client,
    )
