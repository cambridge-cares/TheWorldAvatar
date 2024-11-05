from functools import cache
import logging
from typing import Annotated

from fastapi import Depends, HTTPException
import requests

from services.geocoding.base import IGeocoder
from services.geocoding.serial import get_serial_geocoder
from services.feature_info_client import FeatureInfoClient

from model.structured_answer import (
    ScatterPlotData,
    ScatterPlotTrace,
    TableData,
    TypedSeries,
)
from services.funcs.base import Name2Func
from .pollutant_conc import (
    PollutantConcClient,
    get_pollutantConc_client,
)
from .ship import ShipMeta, get_ship_featureInfo_client


logger = logging.getLogger(__name__)


class SGDispersionFuncExecutor(Name2Func):
    def __init__(
        self,
        geocoder: IGeocoder,
        pollutant_conc_client: PollutantConcClient,
        ship_feature_info_client: FeatureInfoClient[ShipMeta],
    ):
        self.geocoder = geocoder
        self.pollutant_conc_client = pollutant_conc_client
        self.ship_feature_info_client = ship_feature_info_client

    def get_name2func(self):
        return {
            "get_pollutant_conc": self.get_pollutant_conc,
            "lookup_ship_attributes": self.lookup_ship_attributes,
            "lookup_ship_timeseries": self.lookup_ship_timeseries,
        }

    def get_pollutant_conc(self, location: str, **kwargs):
        logger.info("Get coordinates for the location: " + location)
        place = self.geocoder.search(location)
        logger.info("Geo-decoded data: " + str(place))

        try:
            res = self.pollutant_conc_client.get(lat=place.lat, lon=place.lon)
        except requests.HTTPError as e:
            if e.response.status_code == 404:
                raise HTTPException(
                    404,
                    detail="Unable to retrieve pollutant concentrations. Please ensure that the provided location is either in the Jurong Island area or NUS Kent Ridge campus.",
                )
            else:
                raise e

        data = [
            ScatterPlotData(
                title="{key} ({unit}) in {location}".format(
                    key=key, unit="µg/m³", location=place.name
                ),
                traces=[
                    ScatterPlotTrace(
                        x=TypedSeries(data=res.time, type="date"),
                        y=TypedSeries(data=readings, type="number"),
                    )
                ],
            )
            for key, readings in res.model_dump().items()
            if key != "time"
        ]

        return data

    def _sanitise(self, text: str):
        text = text.split("^^", maxsplit=1)[0]
        if text.startswith('"') and text.endswith('"'):
            text = text[1:-1]
        return text

    def lookup_ship_attributes(self, ship: list[str], **kwargs):
        """
        Given ship name or MMSI, returns MMSI, maximum static draught, dimension, IMO number, ship type, call sign
        """
        vars = ["Ship", "ShipName"]
        vars_set = set(vars)
        bindings = []

        for iri in ship:
            binding = {"Ship": iri, "ShipName": ""}  # TODO: retrieve ship name

            ship_feature_info = self.ship_feature_info_client.query(iri=iri)
            ship_meta = ship_feature_info.meta.model_dump()
            ship_meta = {k: self._sanitise(v) if isinstance(v, str) else v for k, v in ship_meta.items()}

            for k, v in ship_meta.items():
                if k not in vars_set:
                    vars_set.add(k)
                    vars.append(k)
                binding[k] = v

            bindings.append(binding)

        data = TableData(columns=vars, data=bindings)

        return [data]

    def lookup_ship_timeseries(self, ship: list[str], **kwargs):
        """
        Given ship name or MMSI, returns speed over ground, course over ground, longitude, latitude
        """
        key2plot: dict[str, ScatterPlotData] = dict()
        for iri in ship:
            ship_feature_info = self.ship_feature_info_client.query(iri=iri)

            if not ship_feature_info.time:
                continue

            timeseries_data = ship_feature_info.time[0]
            for key, values, unit in zip(
                timeseries_data.data, timeseries_data.values, timeseries_data.units
            ):
                if key in key2plot:
                    plot = key2plot[key]
                else:
                    plot = ScatterPlotData(
                        title="{key} ({unit})".format(key=key, unit=unit)
                    )
                    key2plot[key] = plot

                plot.traces.append(
                    ScatterPlotTrace(
                        name="MMSI: {mmsi}".format(mmsi=self._sanitise(ship_feature_info.meta.mmsi)),
                        x=TypedSeries(data=timeseries_data.time, type="date"),
                        y=TypedSeries(data=values, type="number"),
                    )
                )

        return list(key2plot.values())


@cache
def get_sgDispersion_funcExec(
    geocoder: Annotated[IGeocoder, Depends(get_serial_geocoder)],
    pollutant_conc_client: Annotated[
        PollutantConcClient, Depends(get_pollutantConc_client)
    ],
    ship_feature_info_client: Annotated[
        FeatureInfoClient[ShipMeta], Depends(get_ship_featureInfo_client)
    ],
):
    return SGDispersionFuncExecutor(
        geocoder=geocoder,
        pollutant_conc_client=pollutant_conc_client,
        ship_feature_info_client=ship_feature_info_client,
    )
