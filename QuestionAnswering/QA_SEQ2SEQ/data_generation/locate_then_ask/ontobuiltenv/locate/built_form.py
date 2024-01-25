from constants.namespaces import OBE
from constants.ontobuiltenv import OBEAttrKey
from locate_then_ask.ontobuiltenv.locate.attr import OBEAttrLocator
from locate_then_ask.ontobuiltenv.model import OBEProperty
from locate_then_ask.query_graph import QueryGraph


class OBEBuiltFormLocator(OBEAttrLocator):
    def locate(self, query_graph: QueryGraph, entity: OBEProperty):
        assert entity.built_form is not None

        assert entity.built_form.startswith(OBE), entity.built_form
        clsname = entity.built_form[len(OBE) :]
        clsname_node = "obe:" + clsname
        query_graph.add_iri_node(clsname_node, prefixed=True, key=OBEAttrKey.BUILT_FORM)
        query_graph.add_triple("Property", "obe:hasBuiltForm/a", clsname_node)

        verbn = "built form is " + clsname

        return verbn