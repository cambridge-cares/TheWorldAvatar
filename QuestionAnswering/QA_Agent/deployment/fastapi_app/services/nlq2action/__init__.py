from services.nlq2action.link_entity import EntityLinkingMediator
from .translate import Nlq2ActionTranslator


class SupportingDataRetriever:
    def __init__(self, translator: Nlq2ActionTranslator, entity_linker: EntityLinkingMediator):
        self.translator = translator
        self.entity_linker = entity_linker

    def retrieve(self, nlq: str):
        action = self.translator.translate(nlq)
        