from .synthetic import OBEPropertySynthesizer
from .entity_store import OBEEntityStore


class MockOBEEntityStore(OBEEntityStore):
    def __init__(self):
        self.synth = OBEPropertySynthesizer()

    def get(self, entity_iri: str):
        return self.synth.make()
