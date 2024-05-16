from functools import cache
import logging
import time
from typing import Annotated, Dict, List

from fastapi import Depends, HTTPException
import requests

from services.geocoding.base import IGeocoder
from services.geocoding.serial import get_serial_geocoder
from services.entity_store import EntityStore, get_entity_store
from services.feature_info_client import FeatureInfoClient
from controllers.qa.support_data import (
    ScatterPlotDataItem,
    ScatterPlotTrace,
    TableDataItem,
    TypedSeries,
)
from controllers.qa.support_data import QAStep
from controllers.qa.execute_action.func.base import Name2Func
from .pollutant_conc import (
    PollutantConcClient,
    get_pollutantConc_client,
)
from .ship import (
    ShipMeta,
    get_ship_featureInfo_client,
)


logger = logging.getLogger(__name__)


class SGDispersionFuncExecutor(Name2Func):
    def __init__(
        self,
        geocoder: IGeocoder,
        entity_store: EntityStore,
        pollutant_conc_client: PollutantConcClient,
        ship_feature_info_client: FeatureInfoClient[ShipMeta],
    ):
        self.geocoder = geocoder
        self.entity_store = entity_store
        self.pollutant_conc_client = pollutant_conc_client
        self.ship_feature_info_client = ship_feature_info_client

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
            ScatterPlotDataItem(
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
        logger.info("Linked IRIs: " + str(iris))
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

            ship_feature_info = self.ship_feature_info_client.query(iri=iri)
            ship_meta = ship_feature_info.meta.model_dump()
            ship_meta = {k: self._sanitise(v) for k, v in ship_meta.items()}

            for k, v in ship_meta.items():
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
        logger.info("Linked IRIs: " + str(iris))
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
                    plot = ScatterPlotDataItem(
                        title="{key} ({unit})".format(key=key, unit=unit)
                    )
                    key2plot[key] = plot

                label = self.entity_store.lookup_label(iri)
                mmsi = "(MMSI: {mmsi})".format(mmsi=ship_feature_info.meta.mmsi)

                plot.traces.append(
                    ScatterPlotTrace(
                        name=" ".join(x for x in [label, mmsi] if x),
                        x=TypedSeries(data=timeseries_data.time, type="date"),
                        y=TypedSeries(data=values, type="number"),
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
def get_sgDispersion_funcExec(
    geocoder: Annotated[IGeocoder, Depends(get_serial_geocoder)],
    entity_store: Annotated[EntityStore, Depends(get_entity_store)],
    pollutant_conc_client: Annotated[
        PollutantConcClient, Depends(get_pollutantConc_client)
    ],
    ship_feature_info_client: Annotated[
        FeatureInfoClient[ShipMeta], Depends(get_ship_featureInfo_client)
    ],
):
    return SGDispersionFuncExecutor(
        geocoder=geocoder,
        entity_store=entity_store,
        pollutant_conc_client=pollutant_conc_client,
        ship_feature_info_client=ship_feature_info_client,
    )
