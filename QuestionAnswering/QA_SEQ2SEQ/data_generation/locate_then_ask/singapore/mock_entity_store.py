from .synthetic import OPltPlotSynthesizer
from .entity_store import SgEntityStore


class MockSgEntityStore(SgEntityStore):
    def __init__(self, bg_endpoint: str, ontop_endpoint: str):
        self.synth = OPltPlotSynthesizer(bg_endpoint, ontop_endpoint)

    def get(self, entity_iri: str):
        return self.synth.make()
