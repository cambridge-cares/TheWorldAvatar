from locate_then_ask.ontocompchem.entity_store import OCCEntityStore
from locate_then_ask.ontocompchem.synthetic.molcomp import (
    OCCMolecularComputationSynthesizer,
)
from locate_then_ask.ontocompchem.synthetic.species import OCCSpeciesSynthesizer


class MockOCCEntityStore(OCCEntityStore):
    def __init__(self):
        self.molcomp_synth = OCCMolecularComputationSynthesizer()
        self.species_synth = OCCSpeciesSynthesizer()

    def get_molcomp(self, entity_iri: str):
        return self.molcomp_synth.make()

    def get_species(self, entity_iri: str):
        return self.species_synth.make()
