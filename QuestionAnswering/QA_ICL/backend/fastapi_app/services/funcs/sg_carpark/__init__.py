from functools import cache
import logging
import time
from typing import Annotated

from fastapi import Depends

from model.structured_answer import TableData
from services.geocoding.base import IGeocoder
from services.geocoding.serial import get_serial_geocoder
from services.funcs.base import Name2Func
from .nearest import NearestCarparkLocator, get_nearestCarpark_locator
from .feature_info import CarparkFeatureInfoClient, get_carpark_featureInfo_client

logger = logging.getLogger(__name__)


class SGCarparkFuncExecutor(Name2Func):
    def __init__(
        self,
        geocoder: IGeocoder,
        nearest_carpark_locator: NearestCarparkLocator,
        carpark_feature_info_client: CarparkFeatureInfoClient,
    ):
        self.geocoder = geocoder
        self.nearest_carpark_locator = nearest_carpark_locator
        self.carpark_feature_info_client = carpark_feature_info_client

    def get_name2func(self):
        return {"find_nearest_carpark": self.find_nearest_carpark}

    def find_nearest_carpark(self, location: str):
        logger.info("Get coordinates for the location: " + location)
        place = self.geocoder.search(location)
        logger.info("Geo-decoded data: " + str(place))

        timestamp = time.time()

        _, bindings = self.nearest_carpark_locator.locate(
            lat=place.lat, lon=place.lon
        )

        for binding in bindings:
            carpark_feature_info = self.carpark_feature_info_client.query(
                iri=binding["Carpark"]
            )
            if not carpark_feature_info.time:
                continue

            binding["LotAvailability"] = carpark_feature_info.time[0].values[0][-1]
            binding["Time"] = carpark_feature_info.time[0].time[-1]

        data = [TableData.from_data(bindings)]

        return data


@cache
def get_sgCarpark_funcExec(
    geocoder: Annotated[IGeocoder, Depends(get_serial_geocoder)],
    nearest_carpark_locator: Annotated[
        NearestCarparkLocator, Depends(get_nearestCarpark_locator)
    ],
    carpark_client: Annotated[
        CarparkFeatureInfoClient, Depends(get_carpark_featureInfo_client)
    ],
):
    return SGCarparkFuncExecutor(
        geocoder=geocoder,
        nearest_carpark_locator=nearest_carpark_locator,
        carpark_feature_info_client=carpark_client,
    )
