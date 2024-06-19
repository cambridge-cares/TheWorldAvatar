from functools import cache
import logging
from typing import Annotated

from fastapi import Depends
from .base import IGeocoder
from .location_iq import LocationIQGeocoder, get_locationIq_geocoder
from .nominatim import NominatimGeocoder, get_nominatim_geocoder


logger = logging.getLogger(__name__)


class SerialGeocoder(IGeocoder):
    def __init__(self, geocoders: tuple[IGeocoder, ...]):
        self.geocoders = geocoders

    def search(self, location: str):
        exceptions = []

        for i, geocoder in enumerate(self.geocoders):
            try:
                return geocoder.search(location)
            except Exception as e:
                logger.info("Fail to geocode with the geocoder number " + str(i))
                logger.info("Error message: " + str(e))

                exceptions.append(e)

        raise Exception(exceptions)


@cache
def get_serial_geocoder(
    nominatim_geocoder: Annotated[NominatimGeocoder, Depends(get_nominatim_geocoder)],
    location_iq_geocoder: Annotated[
        LocationIQGeocoder, Depends(get_locationIq_geocoder)
    ],
):
    return SerialGeocoder(geocoders=(nominatim_geocoder, location_iq_geocoder))
