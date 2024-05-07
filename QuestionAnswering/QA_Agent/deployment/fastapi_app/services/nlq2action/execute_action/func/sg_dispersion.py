from functools import cache
import logging
import os
import time
from typing import Annotated, Dict, List

from fastapi import Depends, HTTPException
import requests
from model.qa import QAStep
from services.entity_store import EntityStore, get_entity_store
from services.feature_info_client import FeatureInfoClient, get_featureInfoClient
from services.geocoding import IGeocoder, get_geocoder
from services.support_data import ScatterPlotDataItem, ScatterPlotTrace, TableDataItem
from .base import Name2Func


logger = logging.getLogger(__name__)


class SGDispersionFuncExecutor(Name2Func):
    def __init__(
        self,
        pollutant_conc_endpoint: str,
        geocoder: IGeocoder,
        entity_store: EntityStore,
        feature_info_client: FeatureInfoClient,
    ):
        self.pollutant_conc_endpoint = pollutant_conc_endpoint
        self.geocoder = geocoder
        self.entity_store = entity_store
        self.feature_info_client = feature_info_client

    def get_name2func(self):
        return {
            "get_pollutant_conc": self.get_pollutant_conc,
            "lookup_ship_attributes": self.lookup_ship_attributes,
            "lookup_ship_timeseries": self.lookup_ship_timeseries,
        }

    def get_pollutant_conc(self, location: str, **kwargs):
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
        try:
            query_params = {"lat": place.lat, "lon": place.lon}
            res = requests.get(self.pollutant_conc_endpoint, params=query_params)
            res.raise_for_status()
            res: dict = res.json()

            timestamps = res["time"]
            data = [
                ScatterPlotDataItem(
                    title=f"{key} in {place.name}",
                    traces=[ScatterPlotTrace(x=timestamps, y=readings)],
                )
                for key, readings in res.items()
                if key != "time"
            ]

        except requests.HTTPError as e:
            if e.response.status_code == 404:
                raise HTTPException(
                    404,
                    detail="Unable to retrieve pollutant concentrations. Please ensure that the provided location is either in the Jurong Island area or NUS Kent Ridge campus.",
                )
            else:
                raise e

        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="get_pollutant_concentrations",
                arguments=str(place),
                latency=latency,
            )
        )

        return steps, data

    def _sanitise(self, text: str):
        text = text.split("^^", maxsplit=1)[0]
        if text.startswith('"') and text.endswith('"'):
            text = text[1:-1]
        return text

    def lookup_ship_attributes(self, surface_form: str, **kwargs):
        """
        Given ship name or MMSI, returns MMSI, maximum static draught, dimension, IMO number, ship type, call sign
        """
        steps: List[QAStep] = []

        logger.info("Perform entity linking for: " + surface_form)
        timestamp = time.time()
        iris = self.entity_store.link(surface_form, clsname="Ship", k=1)
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="link_entity",
                arguments=surface_form,
                results=iris,
                latency=latency,
            )
        )

        timestamp = time.time()

        vars = ["Ship", "ShipName"]
        vars_set = set(vars)
        bindings = []

        for iri in iris:
            binding = {"Ship": iri, "ShipName": self.entity_store.lookup_label(iri)}

            ship_data: dict = self.feature_info_client.query(iri=iri)["meta"]
            ship_data = {k: self._sanitise(v) for k, v in ship_data.items()}

            for k, v in ship_data.items():
                if k not in vars_set:
                    vars_set.add(k)
                    vars.append(k)
                binding[k] = v

            bindings.append(binding)

        data = TableDataItem(vars=vars, bindings=bindings)

        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="lookup_ship_attributes",
                arguments=dict(iris=iris),
                latency=latency,
            )
        )

        return steps, [data]

    def lookup_ship_timeseries(self, surface_form: str, **kwargs):
        """
        Given ship name or MMSI, returns speed over ground, course over ground, longitude, latitude
        """
        steps: List[QAStep] = []

        logger.info("Perform entity linking for: " + surface_form)
        timestamp = time.time()
        iris = self.entity_store.link(surface_form, clsname="Ship", k=1)
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="link_entity",
                arguments=surface_form,
                results=iris,
                latency=latency,
            )
        )

        timestamp = time.time()

        key2plot: Dict[str, ScatterPlotDataItem] = dict()

        for iri in iris:
            retrieved_info = self.feature_info_client.query(iri=iri)
            meta_data = retrieved_info["meta"]
            timeseries_data = retrieved_info["time"][0]
            """
            {
                "data": [string, ...],
                "values": [[number, ...], ...],
                "units": [string, ...],
                "time": [string, ...]
            }
            """

            for i, key in enumerate(timeseries_data["data"]):
                if key not in key2plot:
                    plot = ScatterPlotDataItem(title=key)
                else:
                    plot = key2plot[plot]

                label = self.entity_store.lookup_label(iri)
                mmsi = (
                    "(MMSI: {mmsi})".format(mmsi=meta_data["MMSI"])
                    if "MMSI" in meta_data
                    else None
                )

                plot.traces.append(
                    ScatterPlotTrace(
                        name=" ".join(x for x in [label, mmsi] if x),
                        x=timeseries_data["time"],
                        y=timeseries_data["values"][i],
                    )
                )

        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="lookup_ship_timeseries",
                arguments=dict(iris=iris),
                latency=latency,
            )
        )

        return steps, list(key2plot.values())


@cache
def get_pollutantConc_endpoint():
    return os.getenv("ENDPOINT_POLLUTANT_CONCENTRATIONS")


@cache
def get_sgDispersion_funcExec(
    pollutant_conc_endpoint: Annotated[str, Depends(get_pollutantConc_endpoint)],
    geocoder: Annotated[IGeocoder, Depends(get_geocoder)],
    entity_store: Annotated[EntityStore, Depends(get_entity_store)],
    feature_info_client: Annotated[FeatureInfoClient, Depends(get_featureInfoClient)],
):
    return SGDispersionFuncExecutor(
        pollutant_conc_endpoint=pollutant_conc_endpoint,
        geocoder=geocoder,
        entity_store=entity_store,
        feature_info_client=feature_info_client,
    )
