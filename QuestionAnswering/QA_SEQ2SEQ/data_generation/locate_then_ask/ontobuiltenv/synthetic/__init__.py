from decimal import Decimal
import random
from typing import Optional
from constants.namespaces import DABGEO, OBE

from locate_then_ask.ontobuiltenv.model import OBEProperty
from .address import IctAddressSynthesizer
from .measure import OmMeasureSynthesizer
from .property_usage import OBEPropertyUsageSynthesizer


class OBEPropertySynthesizer:
    def __init__(self, kg_endpoint: Optional[str] = None):
        self.addr_synth = IctAddressSynthesizer(kg_endpoint)
        self.propuse_synth = OBEPropertyUsageSynthesizer()
        self.totalfloorarea_synth = OmMeasureSynthesizer("TotalFloorArea", kg_endpoint)
        self.marketvalue_synth = OmMeasureSynthesizer("MarketValue", kg_endpoint)
        self.groundelevation_synth = OmMeasureSynthesizer("GroundElevation", kg_endpoint)

    def make(self):
        return OBEProperty(
            iri="placeholder",
            concept=random.choice(
                [OBE + "Property", DABGEO + "Building", OBE + "Flat"]
            ),
            address=self.addr_synth.make(),
            built_form=random.choice(
                [OBE + "Terraced", OBE + "Detached", OBE + "Semi-Detached"]
            ),
            energy_rating=random.choice("ABCDEFG"),
            latest_epc=random.choice([None, "placeholder"]),
            number_of_habitable_rooms=Decimal(str(random.randint(1, 69))),
            property_type=random.choice(
                [OBE + "ParkHome", OBE + "Maisonette", OBE + "House", OBE + "Bungalow"]
            ),
            property_usage=self.propuse_synth.make(),
            total_floor_area=self.totalfloorarea_synth.make(),
            market_value=self.marketvalue_synth.make(),
            latest_transaction_record=random.choice([None, "placeholder"]),
            ground_elevation=self.groundelevation_synth.make(),
        )
