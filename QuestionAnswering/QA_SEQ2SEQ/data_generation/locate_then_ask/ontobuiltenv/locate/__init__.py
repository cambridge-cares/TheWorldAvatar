import random
from typing import Dict, Optional

import numpy as np

from constants.namespaces import DABGEO, OBE
from constants.ontobuiltenv import OBEAttrKey
from locate_then_ask.query_graph import QueryGraph
from locate_then_ask.ontobuiltenv.entity_store import OBEEntityStore
from utils.numerical import normalize_1d
from .attr import OBEAttrLocator
from .address import OBEAddressLocator
from .built_form import OBEBuiltFormLocator
from .energy_rating import OBEEnergyRatingLocator
from .ground_elevation import OBEGroundElevationLocator
from .market_value import OBEMarketValueLocator
from .number_of_habitable_rooms import OBENumOfHabitableRoomsLocator
from .property_type import OBEPropertyTypeLocator
from .property_usage import OBEPropertyUsageLocator
from .total_floor_area import OBETotalFloorAreaLocator
from .latest_epc import OBELatestEPCLocator
from .latest_transaction_record import OBELatestTransactionRecordLocator


class OBELocator:
    ATTR_KEY_WEIGHTS = {
        OBEAttrKey.ADDRESS: 4,
        OBEAttrKey.BUILT_FORM: 2,
        OBEAttrKey.ENERGY_RATING: 1,
        OBEAttrKey.LATEST_EPC: 1.4,
        OBEAttrKey.NUMBER_OF_HABITABLE_ROOMS: 1,
        OBEAttrKey.PROPERTY_TYPE: 2,
        OBEAttrKey.PROPERTY_USAGE: 25,
        OBEAttrKey.TOTAL_FLOOR_AREA: 1,
        OBEAttrKey.MARKET_VALUE: 1,
        OBEAttrKey.LATEST_TRANSACTION_RECORD: 1.4,
        OBEAttrKey.GROUND_ELEVATION: 1,
    }

    def __init__(self, store: Optional[OBEEntityStore]):
        if store is None:
            self.store = OBEEntityStore()
        else:
            self.store = store

        self.attr_locators: Dict[OBEAttrKey, OBEAttrLocator] = {
            OBEAttrKey.ADDRESS: OBEAddressLocator(),
            OBEAttrKey.BUILT_FORM: OBEBuiltFormLocator(),
            OBEAttrKey.ENERGY_RATING: OBEEnergyRatingLocator(),
            OBEAttrKey.LATEST_EPC: OBELatestEPCLocator(),
            OBEAttrKey.NUMBER_OF_HABITABLE_ROOMS: OBENumOfHabitableRoomsLocator(),
            OBEAttrKey.PROPERTY_TYPE: OBEPropertyTypeLocator(),
            OBEAttrKey.PROPERTY_USAGE: OBEPropertyUsageLocator(),
            OBEAttrKey.TOTAL_FLOOR_AREA: OBETotalFloorAreaLocator(),
            OBEAttrKey.MARKET_VALUE: OBEMarketValueLocator(),
            OBEAttrKey.LATEST_TRANSACTION_RECORD: OBELatestTransactionRecordLocator(),
            OBEAttrKey.GROUND_ELEVATION: OBEGroundElevationLocator(),
        }

    def locate_concept_name(self, entity_iri: str):
        query_graph = QueryGraph()
        query_graph.add_topic_node("Property", iri=entity_iri)

        entity = self.store.get(entity_iri)
        if entity.concept == DABGEO + "Building":
            query_graph.add_iri_node("dabgeo:Building", prefixed=True)
            query_graph.add_triple("Property", "a", "dabgeo:Building")

            verbalization = "building"
        elif entity.concept == OBE + "Flat":
            query_graph.add_iri_node("obe:Flat", prefixed=True)
            query_graph.add_triple("Property", "a", "obe:Flat")

            verbalization = "flat"
        elif entity.concept == OBE + "Property":
            query_graph.add_iri_node("obe:Property", prefixed=True)
            query_graph.add_triple("Property", "a/rdfs:subClassOf*", "obe:Property")

            verbalization = "property"
        else:
            raise ValueError(
                "Unexpeced type {type} for entity {iri}".format(
                    type=entity.concept, iri=entity_iri
                )
            )

        return query_graph, verbalization

    def locate_concept_and_literal_multi(self, entity_iri: str, cond_num: int = 2):
        query_graph, concept = self.locate_concept_name(entity_iri)
        entity = self.store.get(entity_iri)

        cond_verbns = []
        keys = entity.get_nonnone_keys()
        weights = [self.ATTR_KEY_WEIGHTS[k] for k in keys]
        for k in np.random.choice(
            keys, size=min(cond_num, len(keys)), p=normalize_1d(weights), replace=False
        ):
            attr_locator = self.attr_locators[k]
            cond_verbn = attr_locator.locate(query_graph, entity=entity)
            cond_verbns.append(cond_verbn)

        verbn = "{concept} whose {conds}".format(
            concept=concept, conds=" and ".join(cond_verbns)
        )

        return query_graph, verbn
