from locate_then_ask.ontospecies.model import OSSpecies
from .use import OSUseSynthesizer
from .chemclass import OSChemClassSynthesizer
from .property import OSPropertySynthesizer
from .identifier import OSIdentifierSynthesizer


class OSSpeciesSynthesizer:
    def __init__(self):
        self.prop_synth = OSPropertySynthesizer()
        self.ident_synth = OSIdentifierSynthesizer()
        self.chemclass_synth = OSChemClassSynthesizer()
        self.use_synth = OSUseSynthesizer()

    def make(self):
        return OSSpecies(
            iri="synthetic_entity",
            key2identifier=self.ident_synth.make(),
            key2property=self.prop_synth.make(),
            chemclasses=self.chemclass_synth.make(),
            uses=self.use_synth.make(),
        )
