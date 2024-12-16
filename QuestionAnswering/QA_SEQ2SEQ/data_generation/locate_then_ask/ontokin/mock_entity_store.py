from locate_then_ask.ontokin.synthetic.mechanism import OKMechanismSynthesizer
from locate_then_ask.ontokin.synthetic.rxn import OKRxnSynthesizer
from locate_then_ask.ontokin.synthetic.species import OKSpeciesSynthesizer
from .entity_store import OKEntityStore


class MockOKEntityStore(OKEntityStore):
    def __init__(self):
        self.mechm_synth = OKMechanismSynthesizer()
        self.rxn_synth = OKRxnSynthesizer()
        self.species_synth = OKSpeciesSynthesizer()

    def get_mechanism(self, entity_iri: str):
        return self.mechm_synth.make()
    
    def get_rxn(self, entity_iri: str):
        return self.rxn_synth.make()
    
    def get_species(self, entity_iri: str):
        return self.species_synth.make()
