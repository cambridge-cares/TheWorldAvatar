from .synthetic import OSSpeciesSynthesizer
from .entity_store import OSEntityStore


class MockOSEntityStore(OSEntityStore):
    def __init__(self):
        self.synth = OSSpeciesSynthesizer()

    def get(self, entity_iri: str):
        return self.synth.make()
