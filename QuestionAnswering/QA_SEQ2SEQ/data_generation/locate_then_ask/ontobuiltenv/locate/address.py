import random

from constants.ontobuiltenv import OBEAttrKey
from locate_then_ask.ontobuiltenv.locate.attr import OBEAttrLocator
from locate_then_ask.ontobuiltenv.model import OBEProperty
from locate_then_ask.query_graph import QueryGraph


class OBEAddressLocator(OBEAttrLocator):
    KEY2PROP = {
        "unit_name": "obe:hasUnitName",
        "street_number": "ict:hasStreetNumber",
        "street": "ict:hasStreet",
        "postal_code": "obe:hasPostalCode/rdfs:label",
    }

    def locate(self, query_graph: QueryGraph, entity: OBEProperty):
        """Locates the topic entity in the query_graph by its address.
        The query_graph is modified in-place."""
        assert entity.address is not None

        address_node = query_graph.make_blank_node(key=OBEAttrKey.ADDRESS)
        query_graph.add_triple("Property", "obe:hasAddress", address_node)

        candidates = entity.address.get_nonnone_keys()
        assert len(candidates) > 0

        if "postal_code" in candidates and (len(candidates) == 1 or random.random() < 2/3):
            if len(candidates) == 1 or random.getrandbits(1):
                # locate by postal code
                postalcode_node = query_graph.make_literal_node(
                    entity.address.postal_code
                )
                query_graph.add_triple(
                    address_node, "obe:hasPostalCode/rdfs:label", postalcode_node
                )

                verbn = "the postal code [{code}]".format(
                    code=entity.address.postal_code
                )
            else:
                # locate by everything
                for key in candidates:
                    node = query_graph.make_literal_node(getattr(entity.address, key))
                    query_graph.add_triple(address_node, self.KEY2PROP[key], node)
                verbn = "[{addr}]".format(
                    addr=" ".join(getattr(entity.address, key) for key in candidates)
                )
        else:
            # locate by everything but postal code
            for key in candidates:
                if key == "postal_code":
                    continue
                node = query_graph.make_literal_node(getattr(entity.address, key))
                query_graph.add_triple(address_node, self.KEY2PROP[key], node)

            verbn = "[{addr}]".format(
                addr=" ".join(
                    getattr(entity.address, key)
                    for key in candidates
                    if key != "postal_code"
                )
            )

        verbn = "addresss is at " + verbn

        return verbn
