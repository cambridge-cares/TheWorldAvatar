from typing import Optional
from locate_then_ask.ontospecies.model import OSSpecies
from .use import OSUseSynthesizer
from .chemclass import OSChemClassSynthesizer
from .property import OSPropertySynthesizer
from .identifier import OSIdentifierSynthesizer


class OSSpeciesSynthesizer:
    def __init__(self, kg_endpoint: Optional[str] = None):
        self.prop_synth = OSPropertySynthesizer(kg_endpoint)
        self.ident_synth = OSIdentifierSynthesizer(kg_endpoint)
        self.chemclass_synth = OSChemClassSynthesizer(kg_endpoint)
        self.use_synth = OSUseSynthesizer(kg_endpoint)

    def make(self):
        return OSSpecies(
            iri="synthetic_entity",
            key2identifier=self.ident_synth.make(),
            key2property=self.prop_synth.make(),
            chemclasses=self.chemclass_synth.make(),
            uses=self.use_synth.make(),
        )
