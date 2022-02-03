# The purpose of this module is to provide utility functions
# to interact with the knowledge graph
#============================================================
from urllib import response
from rdflib import Graph, URIRef, Namespace, Literal, BNode
from rdflib.namespace import RDF
import pandas as pd
import collections
import json
import time
import uuid
import os

from pyasyncagent.kg_operations import PySparqlClient
from pyasyncagent.data_model import *

from expsetupagent.data_model import *
from expsetupagent.kg_operations import *

from doeagent.data_model import *


import logging
logger = logging.getLogger('sparql_client')

class ExpSetupSparqlClient(PySparqlClient):
    # def getNewExperimentInfo(self, newexp_iri: str) -> NewExperiment:
    #     query = """SELECT ?rxnexp WHERE { <%s> <%s> ?rxnexp .}""" % (trimIRI(newexp_iri), ONTODOE_REFERSTO)
    #     response = self.performQuery(query)
    #     rxnexp_iris = [res['rxnexp'] for res in response]
    #     newexp = NewExperiment(
    #         instance_iri=newexp_iri,
    #         refersTo=self.getReactionExperiment(rxnexp_iris)
    #     )
    #     return newexp

    # def createNewEquipSettingsFromRxnExp(self, newexp: NewExperiment) -> List[NewEquipSettings]:
    #     list_equip_setting = []
    #     for rxnexp in newexp.refersTo:
    #         reactor_setting = ReactorSettings(
    #             instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
    #             hasResidenceTimeSetting=ResidenceTimeSetting(
    #                 instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
    #                 hasQuantity=self.getSpecificRxnConOrPerfInd(rxnexp.hasReactionCondition, ONTORXN_RESIDENCETIME).instance_iri,
    #                 namespace_for_init=getNameSpace(rxnexp.instance_iri)
    #             ),
    #             hasReactorTemperatureSetting=ReactorTemperatureSetting(
    #                 instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
    #                 hasQuantity=self.getSpecificRxnConOrPerfInd(rxnexp.hasReactionCondition, ONTORXN_REACTIONTEMPERATURE).instance_iri,
    #                 namespace_for_init=getNameSpace(rxnexp.instance_iri)
    #             ),
    #             namespace_for_init=getNameSpace(rxnexp.instance_iri)
    #         )

    #         # TODO add support for PumpSettings
    #         # pump_setting = PumpSettings()

    #         equip = NewEquipSettings(
    #             instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
    #             isGeneratedFor=rxnexp,
    #             isGeneratedAt=int(time.time()),
    #             hasEquipmentSettings=[reactor_setting], # [reactor_setting, pump_setting],
    #             namespace_for_init=getNameSpace(rxnexp.instance_iri)
    #         )

    #         list_equip_setting.append(equip)

    #     return list_equip_setting

    # def writeDigitalTwinConfigToKG(self, dt_confs: List[NewEquipSettings]):
    #     filePath = f'{str(uuid.uuid4())}.xml'

    #     g = Graph()
    #     for dt_ in dt_confs:
    #         g = dt_.create_instance_for_kg(g)
    #     g.serialize(filePath, format='xml')
    #     self.uploadOntology(filePath)
    #     # Delete generated XML file
    #     os.remove(filePath)

    def createEquipmentSettingsFromReactionExperiment(self, rxnexp: ReactionExperiment) -> List[EquipmentSettings]:
        list_equip_setting = []
        reactor_setting = ReactorSettings(
            instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
            hasResidenceTimeSetting=ResidenceTimeSetting(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                hasQuantity=self.getSpecificRxnConOrPerfInd(rxnexp.hasReactionCondition, ONTORXN_RESIDENCETIME).instance_iri,
                namespace_for_init=getNameSpace(rxnexp.instance_iri)
            ),
            hasReactorTemperatureSetting=ReactorTemperatureSetting(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                hasQuantity=self.getSpecificRxnConOrPerfInd(rxnexp.hasReactionCondition, ONTORXN_REACTIONTEMPERATURE).instance_iri,
                namespace_for_init=getNameSpace(rxnexp.instance_iri)
            ),
            namespace_for_init=getNameSpace(rxnexp.instance_iri)
        )

        list_equip_setting.append(reactor_setting)

        # TODO add support for PumpSettings
        # pump_setting = PumpSettings()
        # list_equip_setting.append(pump_setting)

        return list_equip_setting

    def writeEquipmentSettingsToKG(self, equip_settings: List[EquipmentSettings]):
        filePath = f'{str(uuid.uuid4())}.xml'

        g = Graph()
        for es in equip_settings:
            g = es.create_instance_for_kg(g)
        g.serialize(filePath, format='xml')
        self.uploadOntology(filePath)
        # Delete generated XML file
        # os.remove(filePath)

    def getSpecificRxnConOrPerfInd(self, list_: List[ReactionCondition] or List[PerformanceIndicator], clz, positionalID=None):
        var = []
        for l in list_:
            # TODO what if unknown positionalID when calling this function? in fact, does positionalID matter?
            logger.error(l.__dict__)
            if tuple((l.clz, l.positionalID)) == tuple((clz, positionalID)):
                var.append(l)
        if len(var) > 1:
            raise Exception("Only one appearance should be allowed for a ReactionCondition/PerformanceIndicator to be set as a ParameterSetting, found: <%s>." % '>, <'.join(var))
        elif len(var) < 1:
            raise Exception("Class <%s> was not found." % clz)
            # raise Exception(len(list_))
        else:
            logger.error("Identified quantity")
            logger.error(var[0])
            return var[0]
