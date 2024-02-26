from typing import Optional

from .synthetic.framework import OZFrameworkSynthesizer
from .synthetic.material import OZMaterialSynthesizer
from .entity_store import OZEntityStore


class MockOZEntityStore(OZEntityStore):
    def __init__(self, kg_endpoint: Optional[str] = None):
        self.framework_synth = OZFrameworkSynthesizer(kg_endpoint)
        self.material_synth = OZMaterialSynthesizer(kg_endpoint)

    def get_framework(self, entity_iri: str):
        return self.framework_synth.make()

    def get_material(self, entity_iri: str):
        return self.material_synth.make()
