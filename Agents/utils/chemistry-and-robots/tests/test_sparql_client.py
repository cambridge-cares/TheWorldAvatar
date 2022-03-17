from chemistry_and_robots.hardware import hplc
from testcontainers.core.container import DockerContainer
from rdflib import Graph
from pathlib import Path
from enum import Enum
import logging
import pkgutil
import pytest
import time
import uuid
import os

logging.getLogger("py4j").setLevel(logging.INFO)
logger = logging.getLogger('test_sparql_client')

from chemistry_and_robots.kg_operations.sparql_client import ChemistryAndRobotsSparqlClient
import chemistry_and_robots.data_model as onto

class TargetIRIs(Enum):
    DUMMY_LAB_BASE_IRI = 'http://example.com/blazegraph/namespace/testlab/dummy_lab/'
    AUTOMATEDRXNPLATFORM_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'AutomatedRxnPlatform_Dummy'
    VAPOURTECRS400_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'VapourtecRS400_Dummy'
    HPLC_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'HPLC_Dummy'
    BPR_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'BPR_Dummy'
    VAPOURTECR4REACTOR_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'VapourtecR4_Dummy'
    VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'VapourtecR4_Another_Dummy'
    VAPOURTECR2PUMP_1_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'VapourtecR2_1_Dummy'
    VAPOURTECR2PUMP_2_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'VapourtecR2_2_Dummy'
    VAPOURTECR2PUMP_3_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'VapourtecR2_3_Dummy'
    VAPOURTECR2PUMP_4_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'VapourtecR2_4_Dummy'
    AUTOSAMPLER_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'AutoSampler_Dummy'
    EXP_1_BASE_IRI = 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/'
    EXP_2_BASE_IRI = 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_2/'
    EXP_3_BASE_IRI = 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_3/'
    EXP_4_BASE_IRI = 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_4/'
    EXP_5_BASE_IRI = 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_5/'
    EXAMPLE_RXN_EXP_1_IRI = EXP_1_BASE_IRI + 'RxnExp_1'
    EXAMPLE_RXN_EXP_2_IRI = EXP_2_BASE_IRI + 'RxnExp_1'
    EXAMPLE_RXN_EXP_3_IRI = EXP_3_BASE_IRI + 'RxnExp_1'
    EXAMPLE_RXN_EXP_4_IRI = EXP_4_BASE_IRI + 'RxnExp_1'
    EXAMPLE_RXN_EXP_5_IRI = EXP_5_BASE_IRI + 'RxnExp_1'
    NEW_RXN_EXP_1_IRI = EXP_1_BASE_IRI + 'ReactionVariation_fac53bb1-3ae0-4941-9f5b-38738b07ab70'
    NEW_RXN_EXP_2_IRI = EXP_1_BASE_IRI + 'ReactionVariation_3bd3166d-f782-4cdc-a6a8-75336afd71a8'
    NEW_RXN_EXP_3_IRI = EXP_1_BASE_IRI + 'ReactionVariation_c4b175d9-e53c-4d7e-b053-3a81f7ca0ddf'
    EXAMPLE_RXN_EXP_1_REACTION_CONDITION_IRI_LIST = [EXP_1_BASE_IRI+'ResidenceTime_1',EXP_1_BASE_IRI+'RxnTemperature_1',EXP_1_BASE_IRI+'RxnPressure_1',
        EXP_1_BASE_IRI+'StoiRatio_1',EXP_1_BASE_IRI+'StoiRatio_2',EXP_1_BASE_IRI+'StoiRatio_3',EXP_1_BASE_IRI+'RxnScale_1']
    EXAMPLE_RXN_EXP_1_PERFORMANCE_INDICATOR_IRI_LIST = [EXP_1_BASE_IRI+'Yield_1',EXP_1_BASE_IRI+'RunMaterialCost_1']
    EXAMPLE_RXN_EXP_2_REACTION_CONDITION_IRI_LIST = [EXP_2_BASE_IRI+'ResidenceTime_1',EXP_2_BASE_IRI+'RxnTemperature_1',EXP_2_BASE_IRI+'RxnPressure_1',
        EXP_2_BASE_IRI+'StoiRatio_1',EXP_2_BASE_IRI+'StoiRatio_2',EXP_2_BASE_IRI+'StoiRatio_3',EXP_2_BASE_IRI+'RxnScale_1']
    EXAMPLE_RXN_EXP_2_PERFORMANCE_INDICATOR_IRI_LIST = [EXP_2_BASE_IRI+'Yield_1',EXP_2_BASE_IRI+'RunMaterialCost_1']
    EXAMPLE_RXN_EXP_3_REACTION_CONDITION_IRI_LIST = [EXP_3_BASE_IRI+'ResidenceTime_1',EXP_3_BASE_IRI+'RxnTemperature_1',EXP_3_BASE_IRI+'RxnPressure_1',
        EXP_3_BASE_IRI+'StoiRatio_1',EXP_3_BASE_IRI+'StoiRatio_2',EXP_3_BASE_IRI+'StoiRatio_3',EXP_3_BASE_IRI+'RxnScale_1',]
    EXAMPLE_RXN_EXP_3_PERFORMANCE_INDICATOR_IRI_LIST = [EXP_3_BASE_IRI+'Yield_1',EXP_3_BASE_IRI+'RunMaterialCost_1']
    EXAMPLE_RXN_EXP_4_REACTION_CONDITION_IRI_LIST = [EXP_4_BASE_IRI+'ResidenceTime_1',EXP_4_BASE_IRI+'RxnTemperature_1',EXP_4_BASE_IRI+'RxnPressure_1',
        EXP_4_BASE_IRI+'StoiRatio_1',EXP_4_BASE_IRI+'StoiRatio_2',EXP_4_BASE_IRI+'StoiRatio_3',EXP_4_BASE_IRI+'RxnScale_1',]
    EXAMPLE_RXN_EXP_4_PERFORMANCE_INDICATOR_IRI_LIST = [EXP_4_BASE_IRI+'Yield_1',EXP_4_BASE_IRI+'RunMaterialCost_1']
    EXAMPLE_RXN_EXP_5_REACTION_CONDITION_IRI_LIST = [EXP_5_BASE_IRI+'ResidenceTime_1',EXP_5_BASE_IRI+'RxnTemperature_1',EXP_5_BASE_IRI+'RxnPressure_1',
        EXP_5_BASE_IRI+'StoiRatio_1',EXP_5_BASE_IRI+'StoiRatio_2',EXP_5_BASE_IRI+'StoiRatio_3',EXP_5_BASE_IRI+'RxnScale_1']
    EXAMPLE_RXN_EXP_5_PERFORMANCE_INDICATOR_IRI_LIST = [EXP_5_BASE_IRI+'Yield_1',EXP_5_BASE_IRI+'RunMaterialCost_1']
    NEW_RXN_EXP_1_REACTION_CONDITION_IRI_LIST = [EXP_1_BASE_IRI+'ResidenceTime_f228988f-7b01-4776-84c0-8cf41bb57176',
        EXP_1_BASE_IRI+'ReactionPressure_a75c05d9-e6c5-4710-8618-cfada4d27ad3',
        EXP_1_BASE_IRI+'ReactionScale_12f103bd-5831-4538-80f1-e9f1b67ccf3b',
        EXP_1_BASE_IRI+'ReactionTemperature_ccb93a20-fea9-4179-a3c2-067e31989adf',
        EXP_1_BASE_IRI+'StoichiometryRatio_866bfdf2-0d32-40da-8fcb-f89669cf1d31',
        EXP_1_BASE_IRI+'StoichiometryRatio_8ae63d49-d3e5-469c-a85a-e7799ddf1807',
        EXP_1_BASE_IRI+'StoichiometryRatio_c242fa46-e60c-481c-8dc2-741f69386f25']
    NEW_RXN_EXP_2_REACTION_CONDITION_IRI_LIST = [EXP_1_BASE_IRI+'ResidenceTime_e8c453af-4dca-4e0b-8137-a073e1ab0d82',
        EXP_1_BASE_IRI+'ReactionPressure_06f24390-c46f-4a82-b998-d52f6b03701d',
        EXP_1_BASE_IRI+'ReactionScale_340aac69-106c-4894-94b9-a7c489b5c597',
        EXP_1_BASE_IRI+'ReactionTemperature_9809328d-1e34-4b5f-87aa-67e4accb75bd',
        EXP_1_BASE_IRI+'StoichiometryRatio_4f55a3ff-3a06-4749-8775-52d7673497c7',
        EXP_1_BASE_IRI+'StoichiometryRatio_58691e29-9d69-41ad-ab5f-8fd9b35f5e0c',
        EXP_1_BASE_IRI+'StoichiometryRatio_6a845185-032d-41d8-86ff-6a9952418063']
    NEW_RXN_EXP_3_REACTION_CONDITION_IRI_LIST = [EXP_1_BASE_IRI+'ResidenceTime_6e59352a-5296-4d13-8df9-2839b1f66eef',
        EXP_1_BASE_IRI+'ReactionPressure_b57d5d10-70bc-43c5-9424-f493aeda7967',
        EXP_1_BASE_IRI+'ReactionScale_2845e69e-1441-4758-9803-f038744cea8a',
        EXP_1_BASE_IRI+'ReactionTemperature_867f1bd9-0c7b-41d1-adc6-44738631a568',
        EXP_1_BASE_IRI+'StoichiometryRatio_3712c062-8b67-4a82-82d9-166ff34909ba',
        EXP_1_BASE_IRI+'StoichiometryRatio_9968472e-74b9-4925-b5cc-63220f4be1a9',
        EXP_1_BASE_IRI+'StoichiometryRatio_fb5fecbb-3de2-4d98-b96c-38a3d033ca2d']
    NEW_RXN_EXP_1_PERFORMANCE_INDICATOR_IRI_LIST = [EXP_1_BASE_IRI+'Conversion_967deb19-b7bc-4b5b-a236-13df15a0d2ea',
        EXP_1_BASE_IRI+'EcoScore_ed7d5c04-3388-4b34-9795-0685e2ece916',
        EXP_1_BASE_IRI+'EnvironmentalFactor_a03c52ca-9404-4b71-9ef3-ba19e78c23a0',
        EXP_1_BASE_IRI+'SpaceTimeYield_0d5b724a-d31a-4200-af31-ae574d14be50',
        EXP_1_BASE_IRI+'RunMaterialCost_9e97205c-7b59-497c-b5bc-187c212e53c4',
        EXP_1_BASE_IRI+'Yield_7d236fe9-89ce-4fc1-b12e-e427b73b9bba']
    NEW_RXN_EXP_2_PERFORMANCE_INDICATOR_IRI_LIST = [EXP_1_BASE_IRI+'Conversion_6d64895f-3600-444b-972a-c5e8a0683f05',
        EXP_1_BASE_IRI+'EcoScore_967aca85-cfbc-40d5-8443-9a5d7d38dd81',
        EXP_1_BASE_IRI+'EnvironmentalFactor_8d611396-9126-4415-97b0-5f7f869fab38',
        EXP_1_BASE_IRI+'SpaceTimeYield_501ee5ab-8d6e-402f-86d7-3082bcff606e',
        EXP_1_BASE_IRI+'RunMaterialCost_f075631a-5721-4224-b786-7ce00761f743',
        EXP_1_BASE_IRI+'Yield_38c71d2a-4497-4d6e-bb5e-e219b268a834']
    NEW_RXN_EXP_3_PERFORMANCE_INDICATOR_IRI_LIST = [EXP_1_BASE_IRI+'Conversion_aaa4ac45-4cc8-4f03-b63e-7f416c2008ae',
        EXP_1_BASE_IRI+'EcoScore_c6b4f897-ea3e-4aa7-b46d-89dbdd1288f5',
        EXP_1_BASE_IRI+'EnvironmentalFactor_73c41c3c-3b59-4ea8-85ea-6d7e3ae76f12',
        EXP_1_BASE_IRI+'SpaceTimeYield_4021bb12-fd78-4c97-a065-2ad6693f71a8',
        EXP_1_BASE_IRI+'RunMaterialCost_967051c0-422e-48cd-baae-afa74f8f0557',
        EXP_1_BASE_IRI+'Yield_d1a24199-40af-4ade-ab39-99e324f12ceb']

    CHEMICAL_REACTION_BASE_IRI = 'https://www.example.com/triplestore/ontorxn/ChemRxn_1/'
    CHEMICAL_REACTION_IRI = CHEMICAL_REACTION_BASE_IRI + 'ChemRxn_1'
    REACTANT_SPECIES_DICTIONARY = {CHEMICAL_REACTION_BASE_IRI + 'Species_1': 'http://www.theworldavatar.com/kb/ontospecies/Species_54d8b46b-17bc-4bbd-a3cc-3b3a16d6ae4b',
    CHEMICAL_REACTION_BASE_IRI + 'Species_2': 'http://www.theworldavatar.com/kb/ontospecies/Species_353d4667-e25d-476a-bd74-5c34723c8ea3'}
    PRODUCT_SPECIES_DICTIONARY = {CHEMICAL_REACTION_BASE_IRI + 'Species_3': CHEMICAL_REACTION_BASE_IRI + 'Species_placeholder_pubchemcid_637759',
    CHEMICAL_REACTION_BASE_IRI + 'Species_4': CHEMICAL_REACTION_BASE_IRI + 'Species_placeholder_pubchemcid_640180'}
    CATALYST_SPECIES_DICTIONARY = {CHEMICAL_REACTION_BASE_IRI + 'Species_5': 'http://www.theworldavatar.com/kb/ontospecies/Species_cb3b0560-0df7-4deb-891e-bbb11e7c2b3d'}
    SOLVENT_SPECIES_DICTIONARY = {CHEMICAL_REACTION_BASE_IRI + 'Species_6': 'http://www.theworldavatar.com/kb/ontospecies/Species_0401f93b-b62d-488e-ba1f-7d5c37e365cb',
    CHEMICAL_REACTION_BASE_IRI + 'Species_7': 'http://www.theworldavatar.com/kb/ontospecies/Species_63fefc5a-d49d-4841-a946-2cdb5f356983'}
    LIST_RXN_EXP_1_INPUT_CHEMICAL_IRI = [EXP_1_BASE_IRI + 'InputChemical_1', EXP_1_BASE_IRI + 'InputChemical_2', EXP_1_BASE_IRI + 'InputChemical_3']
    LIST_RXN_EXP_2_INPUT_CHEMICAL_IRI = [EXP_2_BASE_IRI + 'InputChemical_1', EXP_2_BASE_IRI + 'InputChemical_2', EXP_2_BASE_IRI + 'InputChemical_3']
    LIST_RXN_EXP_3_INPUT_CHEMICAL_IRI = [EXP_3_BASE_IRI + 'InputChemical_1', EXP_3_BASE_IRI + 'InputChemical_2', EXP_3_BASE_IRI + 'InputChemical_3']
    LIST_RXN_EXP_4_INPUT_CHEMICAL_IRI = [EXP_4_BASE_IRI + 'InputChemical_1', EXP_4_BASE_IRI + 'InputChemical_2', EXP_4_BASE_IRI + 'InputChemical_3']
    LIST_RXN_EXP_5_INPUT_CHEMICAL_IRI = [EXP_5_BASE_IRI + 'InputChemical_1', EXP_5_BASE_IRI + 'InputChemical_2', EXP_5_BASE_IRI + 'InputChemical_3']
    LIST_RXN_EXP_1_OUTPUT_CHEMICAL_IRI = [EXP_1_BASE_IRI + 'OutputChemical_4']
    LIST_RXN_EXP_2_OUTPUT_CHEMICAL_IRI = [EXP_2_BASE_IRI + 'OutputChemical_4']
    LIST_RXN_EXP_3_OUTPUT_CHEMICAL_IRI = [EXP_3_BASE_IRI + 'OutputChemical_4']
    LIST_RXN_EXP_4_OUTPUT_CHEMICAL_IRI = [EXP_4_BASE_IRI + 'OutputChemical_4']
    LIST_RXN_EXP_5_OUTPUT_CHEMICAL_IRI = [EXP_5_BASE_IRI + 'OutputChemical_4']

    RXNEXP_TYPE_DICT = {EXAMPLE_RXN_EXP_1_IRI:onto.ONTORXN_REACTIONEXPERIMENT,EXAMPLE_RXN_EXP_2_IRI:onto.ONTORXN_REACTIONEXPERIMENT,
        EXAMPLE_RXN_EXP_3_IRI:onto.ONTORXN_REACTIONEXPERIMENT,EXAMPLE_RXN_EXP_4_IRI:onto.ONTORXN_REACTIONEXPERIMENT,
        EXAMPLE_RXN_EXP_5_IRI:onto.ONTORXN_REACTIONEXPERIMENT,NEW_RXN_EXP_1_IRI:onto.ONTORXN_REACTIONVARIATION,
        NEW_RXN_EXP_2_IRI:onto.ONTORXN_REACTIONVARIATION,NEW_RXN_EXP_3_IRI:onto.ONTORXN_REACTIONVARIATION}
    RXNEXP_REACTION_CONDITION_DICT = {EXAMPLE_RXN_EXP_1_IRI:EXAMPLE_RXN_EXP_1_REACTION_CONDITION_IRI_LIST,EXAMPLE_RXN_EXP_2_IRI:EXAMPLE_RXN_EXP_2_REACTION_CONDITION_IRI_LIST,
        EXAMPLE_RXN_EXP_3_IRI:EXAMPLE_RXN_EXP_3_REACTION_CONDITION_IRI_LIST,EXAMPLE_RXN_EXP_4_IRI:EXAMPLE_RXN_EXP_4_REACTION_CONDITION_IRI_LIST,
        EXAMPLE_RXN_EXP_5_IRI:EXAMPLE_RXN_EXP_5_REACTION_CONDITION_IRI_LIST,NEW_RXN_EXP_1_IRI:NEW_RXN_EXP_1_REACTION_CONDITION_IRI_LIST,
        NEW_RXN_EXP_2_IRI:NEW_RXN_EXP_2_REACTION_CONDITION_IRI_LIST,NEW_RXN_EXP_3_IRI:NEW_RXN_EXP_3_REACTION_CONDITION_IRI_LIST}
    RXNEXP_PERFORMANCE_INDICATOR_DICT = {EXAMPLE_RXN_EXP_1_IRI:EXAMPLE_RXN_EXP_1_PERFORMANCE_INDICATOR_IRI_LIST,EXAMPLE_RXN_EXP_2_IRI:EXAMPLE_RXN_EXP_2_PERFORMANCE_INDICATOR_IRI_LIST,
        EXAMPLE_RXN_EXP_3_IRI:EXAMPLE_RXN_EXP_3_PERFORMANCE_INDICATOR_IRI_LIST,EXAMPLE_RXN_EXP_4_IRI:EXAMPLE_RXN_EXP_4_PERFORMANCE_INDICATOR_IRI_LIST,
        EXAMPLE_RXN_EXP_5_IRI:EXAMPLE_RXN_EXP_5_PERFORMANCE_INDICATOR_IRI_LIST}
    RXNEXP_INPUT_CHEMICAL_DICT = {EXAMPLE_RXN_EXP_1_IRI:LIST_RXN_EXP_1_INPUT_CHEMICAL_IRI,EXAMPLE_RXN_EXP_2_IRI:LIST_RXN_EXP_2_INPUT_CHEMICAL_IRI,
        EXAMPLE_RXN_EXP_3_IRI:LIST_RXN_EXP_3_INPUT_CHEMICAL_IRI,EXAMPLE_RXN_EXP_4_IRI:LIST_RXN_EXP_4_INPUT_CHEMICAL_IRI,
        EXAMPLE_RXN_EXP_5_IRI:LIST_RXN_EXP_5_INPUT_CHEMICAL_IRI}
    RXNEXP_OUTPUT_CHEMICAL_DICT = {EXAMPLE_RXN_EXP_1_IRI:LIST_RXN_EXP_1_OUTPUT_CHEMICAL_IRI,EXAMPLE_RXN_EXP_2_IRI:LIST_RXN_EXP_2_OUTPUT_CHEMICAL_IRI,
        EXAMPLE_RXN_EXP_3_IRI:LIST_RXN_EXP_3_OUTPUT_CHEMICAL_IRI,EXAMPLE_RXN_EXP_4_IRI:LIST_RXN_EXP_4_OUTPUT_CHEMICAL_IRI,
        EXAMPLE_RXN_EXP_5_IRI:LIST_RXN_EXP_5_OUTPUT_CHEMICAL_IRI}
    RXNEXP_REACTOR_ASSIGNED_DICT = {EXAMPLE_RXN_EXP_1_IRI:VAPOURTECR4REACTOR_DUMMY_IRI,EXAMPLE_RXN_EXP_2_IRI:VAPOURTECR4REACTOR_DUMMY_IRI,
        EXAMPLE_RXN_EXP_3_IRI:VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI,EXAMPLE_RXN_EXP_4_IRI:VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI,
        EXAMPLE_RXN_EXP_5_IRI:VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI}
    RXNEXP_CHEMICAL_REACTION_IRI_DICT = {EXAMPLE_RXN_EXP_1_IRI:CHEMICAL_REACTION_IRI,EXAMPLE_RXN_EXP_2_IRI:CHEMICAL_REACTION_IRI,
        EXAMPLE_RXN_EXP_3_IRI:CHEMICAL_REACTION_IRI,EXAMPLE_RXN_EXP_4_IRI:CHEMICAL_REACTION_IRI,
        EXAMPLE_RXN_EXP_5_IRI:CHEMICAL_REACTION_IRI}

    LIST_EXAMPLE_RXN_EXP = [EXAMPLE_RXN_EXP_1_IRI, EXAMPLE_RXN_EXP_2_IRI, EXAMPLE_RXN_EXP_3_IRI, EXAMPLE_RXN_EXP_4_IRI, EXAMPLE_RXN_EXP_5_IRI]
    LIST_RXN_EXP_ASSIGNEDTO_VAPR4_DUMMY = [EXAMPLE_RXN_EXP_1_IRI, EXAMPLE_RXN_EXP_2_IRI]
    # LIST_VAPR4_DUMMY_CONDUCTED_RXN_EXP = [EXAMPLE_RXN_EXP_1_IRI, EXAMPLE_RXN_EXP_2_IRI]
    LIST_RXN_EXP_ASSIGNEDTO_VAPR4_ANOTHER_DUMMY = [EXAMPLE_RXN_EXP_3_IRI, EXAMPLE_RXN_EXP_4_IRI, EXAMPLE_RXN_EXP_5_IRI]
    # LIST_VAPR4_ANOTHER_DUMMY_CONDUCTED_RXN_EXP = [EXAMPLE_RXN_EXP_3_IRI, EXAMPLE_RXN_EXP_4_IRI, EXAMPLE_RXN_EXP_5_IRI]
    LIST_DUMMY_R2PUMPS = [VAPOURTECR2PUMP_1_DUMMY_IRI, VAPOURTECR2PUMP_2_DUMMY_IRI, VAPOURTECR2PUMP_3_DUMMY_IRI, VAPOURTECR2PUMP_4_DUMMY_IRI]
    LIST_DUMMY_R4REACTORS = [VAPOURTECR4REACTOR_DUMMY_IRI, VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI]
    RXN_EXP_QUEUE_1 = 'http://example.com/blazegraph/namespace/test_temp/exp_1'
    RXN_EXP_QUEUE_2 = 'http://example.com/blazegraph/namespace/test_temp/exp_2'
    RXN_EXP_QUEUE_3 = 'http://example.com/blazegraph/namespace/test_temp/exp_3'
    RXN_EXP_QUEUE_4 = 'http://example.com/blazegraph/namespace/test_temp/exp_4'
    RXN_EXP_QUEUE_5 = 'http://example.com/blazegraph/namespace/test_temp/exp_5'
    RXN_EXP_QUEUE_6 = 'http://example.com/blazegraph/namespace/test_temp/exp_6'
    RXN_EXP_QUEUE_7 = 'http://example.com/blazegraph/namespace/test_temp/exp_7'
    RXN_EXP_1_PRIOR = []
    RXN_EXP_2_PRIOR = [RXN_EXP_QUEUE_1]
    RXN_EXP_3_PRIOR = [RXN_EXP_QUEUE_1, RXN_EXP_QUEUE_2]
    RXN_EXP_4_PRIOR = [RXN_EXP_QUEUE_1, RXN_EXP_QUEUE_2, RXN_EXP_QUEUE_3]
    RXN_EXP_5_PRIOR = [RXN_EXP_QUEUE_1, RXN_EXP_QUEUE_2, RXN_EXP_QUEUE_3, RXN_EXP_QUEUE_4]
    RXN_EXP_6_PRIOR = [RXN_EXP_QUEUE_1, RXN_EXP_QUEUE_2, RXN_EXP_QUEUE_3, RXN_EXP_QUEUE_4]
    RXN_EXP_7_PRIOR = [RXN_EXP_QUEUE_1, RXN_EXP_QUEUE_2, RXN_EXP_QUEUE_3, RXN_EXP_QUEUE_4]
    HPLCMETHOD_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'HPLCMethod_Dummy'
    PHASECOMPONENT_INTERNAL_STANDARD_IRI = 'https://www.example.com/triplestore/ontorxn/SinglePhase/PhaseComponent_InternalStandard'
    ONTOSPECIES_INTERNAL_STANDARD_IRI = 'http://www.theworldavatar.com/kb/ontospecies/Species_4fa4fdea-ed3d-4b0a-aee5-1f4e97dd2340'
    MOLARITY_INTERNAL_STANDARD = 0.02
    HPLCREPORT_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'HPLCReport_Dummy'
    HPLCJOB_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'HPLCJob_Dummy'
    CHEMICAL_SOLUTION_FOR_OUTPUTCHEMICAL_4_IRI = DUMMY_LAB_BASE_IRI + 'ChemicalSolution_For_OutputChemical_4'
    CHROMATOGRAMPOINT_1_IRI = DUMMY_LAB_BASE_IRI + 'ChromatogramPoint_Dummy_1'
    CHROMATOGRAMPOINT_2_IRI = DUMMY_LAB_BASE_IRI + 'ChromatogramPoint_Dummy_2'
    CHROMATOGRAMPOINT_3_IRI = DUMMY_LAB_BASE_IRI + 'ChromatogramPoint_Dummy_3'
    CHROMATOGRAMPOINT_4_IRI = DUMMY_LAB_BASE_IRI + 'ChromatogramPoint_Dummy_4'
    CHROMATOGRAMPOINT_5_IRI = DUMMY_LAB_BASE_IRI + 'ChromatogramPoint_Dummy_5'
    CHROMATOGRAMPOINT_6_IRI = DUMMY_LAB_BASE_IRI + 'ChromatogramPoint_Dummy_6'
    CHROMATOGRAMPOINT_7_IRI = DUMMY_LAB_BASE_IRI + 'ChromatogramPoint_Dummy_7'
    CHROMATOGRAMPOINT_8_IRI = DUMMY_LAB_BASE_IRI + 'ChromatogramPoint_Dummy_8'
    LIST_CHROMATOGRAMPOINT_IRI = [CHROMATOGRAMPOINT_1_IRI, CHROMATOGRAMPOINT_2_IRI, CHROMATOGRAMPOINT_3_IRI, CHROMATOGRAMPOINT_4_IRI,
    CHROMATOGRAMPOINT_5_IRI, CHROMATOGRAMPOINT_6_IRI, CHROMATOGRAMPOINT_7_IRI, CHROMATOGRAMPOINT_8_IRI]
    DOE_BASE_IRI = 'https://www.example.com/triplestore/ontodoe/DoE_1/'
    DOE_IRI = DOE_BASE_IRI + 'DoE_1'
    DOE_STRATEGY_IRI = DOE_BASE_IRI + 'Strategy_1'
    DOE_TSEMO_STRATEGY_IRI = DOE_STRATEGY_IRI
    DOE_DOMAIN_IRI = DOE_BASE_IRI + 'Domain_1'
    DOE_SYS_RES_1_IRI = DOE_BASE_IRI + 'SystemResponse_1'
    DOE_SYS_RES_2_IRI = DOE_BASE_IRI + 'SystemResponse_2'
    DOE_SYS_RES_IRI_LIST = [DOE_SYS_RES_1_IRI, DOE_SYS_RES_2_IRI]
    DOE_HIST_DATA_IRI = DOE_BASE_IRI + 'HistoricalData_1'
    DOE_HIST_DATE_REFERTO_IRI = LIST_EXAMPLE_RXN_EXP
    DOE_NEW_EXP_IRI = DOE_BASE_IRI + 'ReactionExperiment_new'
    DOE_CONT_VAR_1_IRI = DOE_BASE_IRI + 'ContinuousVariable_1'
    DOE_CONT_VAR_2_IRI = DOE_BASE_IRI + 'ContinuousVariable_2'
    DOE_CONT_VAR_3_IRI = DOE_BASE_IRI + 'ContinuousVariable_3'
    DOE_CONT_VAR_4_IRI = DOE_BASE_IRI + 'ContinuousVariable_4'
    DOE_CONT_VAR_IRI_LIST = [DOE_CONT_VAR_1_IRI, DOE_CONT_VAR_2_IRI, DOE_CONT_VAR_3_IRI, DOE_CONT_VAR_4_IRI]
    DOE_SYS_RES_MAXIMISE_DICT = {DOE_SYS_RES_1_IRI:True, DOE_SYS_RES_2_IRI:False}

# The (scope="module") is added to make the initialisation only run once for the whole python module so it saves time
@pytest.fixture(scope="module")
def initialise_triple_store():
    # NOTE: requires access to the docker.cmclinnovations.com registry from the machine the test is run on.
    # For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
    blazegraph = DockerContainer('docker.cmclinnovations.com/blazegraph_for_tests:1.0.0')
    blazegraph.with_exposed_ports(9999) # the port is set as 9999 to match with the value set in the docker image
    yield blazegraph

# The (scope="module") is added to make the initialisation only run once for the whole python module so it saves time
@pytest.fixture(scope="module")
def initialise_triples(initialise_triple_store):
    with initialise_triple_store as container:
        # Wait some arbitrary time until container is reachable
        time.sleep(3)

        # Retrieve SPARQL endpoint
        endpoint = get_endpoint(container)
        print(endpoint)

        # Create SparqlClient for testing
        sparql_client = ChemistryAndRobotsSparqlClient(endpoint, endpoint)

        # Upload the example triples for testing
        pathlist = Path(str(Path(__file__).absolute().parent.parent)+'/chemistry_and_robots/resources/').glob('**/*.ttl')
        for path in pathlist:
            sparql_client.uploadOntology(str(path))

        yield sparql_client

        # Clear logger at the end of the test
        clear_loggers()

def test_amount_of_triples_none_zero(initialise_triples):
    sparql_client = initialise_triples
    assert sparql_client.getAmountOfTriples() != 0

def test_getDesignVariables(initialise_triples):
    sparql_client = initialise_triples
    design_var_list = sparql_client.getDesignVariables(TargetIRIs.DOE_DOMAIN_IRI.value)
    design_var_iri_list = [var.instance_iri for var in design_var_list]
    assert len(design_var_iri_list) == len(TargetIRIs.DOE_CONT_VAR_IRI_LIST.value)
    assert all(iri in design_var_iri_list for iri in TargetIRIs.DOE_CONT_VAR_IRI_LIST.value)
    assert all(all([isinstance(var, onto.ContinuousVariable), var.refersTo is not None, var.upperLimit > var.lowerLimit]) for var in design_var_list)

@pytest.mark.parametrize(
    "sys_res_iri,maximise",
    [# here we are testing that the function should work wether the passed in iri is already a list or not
        (TargetIRIs.DOE_SYS_RES_1_IRI.value, [True]),
        ([TargetIRIs.DOE_SYS_RES_1_IRI.value], [True]),
        (TargetIRIs.DOE_SYS_RES_2_IRI.value, [False]),
        ([TargetIRIs.DOE_SYS_RES_2_IRI.value], [False]),
        (TargetIRIs.DOE_SYS_RES_IRI_LIST.value, [True, False]),
    ],
)
def test_getSystemResponses(initialise_triples, sys_res_iri, maximise):
    sparql_client = initialise_triples
    sys_res_list = sparql_client.getSystemResponses(sys_res_iri)
    length = len(sys_res_iri) if isinstance(sys_res_iri, list) else 1
    assert length == len(sys_res_list)
    assert all(all([isinstance(r, onto.SystemResponse), r.refersTo is not None]) for r in sys_res_list)
    dct_m = {s.instance_iri:s.maximise for s in sys_res_list}
    if isinstance(sys_res_iri, list):
        assert all(m == dct_m.get(s) for s, m in zip(sys_res_iri, maximise))

def test_getTSEMOSettings(initialise_triples):
    sparql_client = initialise_triples
    tsemo = sparql_client.getTSEMOSettings(TargetIRIs.DOE_TSEMO_STRATEGY_IRI.value)
    assert tsemo.instance_iri == TargetIRIs.DOE_TSEMO_STRATEGY_IRI.value
    assert isinstance(tsemo, onto.TSEMO)
    assert tsemo.nRetries is not None
    assert tsemo.nSpectralPoints is not None
    assert tsemo.nGenerations is not None
    assert tsemo.populationSize is not None

def test_getDoEStrategy(initialise_triples):
    sparql_client = initialise_triples
    strategy = sparql_client.getDoEStrategy(TargetIRIs.DOE_TSEMO_STRATEGY_IRI.value)
    assert strategy.instance_iri == TargetIRIs.DOE_TSEMO_STRATEGY_IRI.value
    assert isinstance(strategy, onto.TSEMO)
    assert strategy.nRetries is not None
    assert strategy.nSpectralPoints is not None
    assert strategy.nGenerations is not None
    assert strategy.populationSize is not None

def test_getDoEDomain(initialise_triples):
    sparql_client = initialise_triples
    doe_domain_instance = sparql_client.getDoEDomain(TargetIRIs.DOE_DOMAIN_IRI.value)
    assert doe_domain_instance.instance_iri == TargetIRIs.DOE_DOMAIN_IRI.value
    lst_var = [v.instance_iri for v in doe_domain_instance.hasDesignVariable]
    assert len(lst_var) == len(TargetIRIs.DOE_CONT_VAR_IRI_LIST.value)
    assert all(var in lst_var for var in TargetIRIs.DOE_CONT_VAR_IRI_LIST.value)
    assert all(all([isinstance(var, onto.ContinuousVariable), var.refersTo is not None, var.upperLimit > var.lowerLimit]) for var in doe_domain_instance.hasDesignVariable)

@pytest.mark.parametrize(
    "rxnexp_iri,rxnexp_condition_iri",
    [# here we are testing that the function should work wether the passed in iri is already a list or not
        (TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_1_REACTION_CONDITION_IRI_LIST.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_2_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_2_REACTION_CONDITION_IRI_LIST.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_3_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_3_REACTION_CONDITION_IRI_LIST.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_4_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_4_REACTION_CONDITION_IRI_LIST.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_5_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_5_REACTION_CONDITION_IRI_LIST.value),
        (TargetIRIs.NEW_RXN_EXP_1_IRI.value, TargetIRIs.NEW_RXN_EXP_1_REACTION_CONDITION_IRI_LIST.value),
        (TargetIRIs.NEW_RXN_EXP_2_IRI.value, TargetIRIs.NEW_RXN_EXP_2_REACTION_CONDITION_IRI_LIST.value),
        (TargetIRIs.NEW_RXN_EXP_3_IRI.value, TargetIRIs.NEW_RXN_EXP_3_REACTION_CONDITION_IRI_LIST.value),
    ],
)
def test_getExpReactionCondition(initialise_triples, rxnexp_iri, rxnexp_condition_iri):
    sparql_client = initialise_triples
    rxn_condition_list = sparql_client.getExpReactionCondition(rxnexp_iri)
    assert len(rxnexp_condition_iri) == len(rxn_condition_list)
    for con in rxn_condition_list:
        assert all([isinstance(con, onto.ReactionCondition), con.clz is not None, isinstance(con.objPropWithExp, list),
            con.hasValue.hasUnit is not None, con.hasValue.hasNumericalValue is not None, con.instance_iri in rxnexp_condition_iri])
        if con.clz == onto.ONTORXN_STOICHIOMETRYRATIO:
            assert con.indicatesMultiplicityOf is not None
        elif con.clz == onto.ONTORXN_REACTIONSCALE:
            assert con.indicateUsageOf is not None

@pytest.mark.parametrize(
    "rxnexp_iri,rxnexp_pref_indicator_iri,rxn_type",
    [# here we are testing that the function should work wether the passed in iri is already a list or not
        (TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_1_PERFORMANCE_INDICATOR_IRI_LIST.value, onto.ONTORXN_REACTIONEXPERIMENT),
        (TargetIRIs.EXAMPLE_RXN_EXP_2_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_2_PERFORMANCE_INDICATOR_IRI_LIST.value, onto.ONTORXN_REACTIONEXPERIMENT),
        (TargetIRIs.EXAMPLE_RXN_EXP_3_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_3_PERFORMANCE_INDICATOR_IRI_LIST.value, onto.ONTORXN_REACTIONEXPERIMENT),
        (TargetIRIs.EXAMPLE_RXN_EXP_4_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_4_PERFORMANCE_INDICATOR_IRI_LIST.value, onto.ONTORXN_REACTIONEXPERIMENT),
        (TargetIRIs.EXAMPLE_RXN_EXP_5_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_5_PERFORMANCE_INDICATOR_IRI_LIST.value, onto.ONTORXN_REACTIONEXPERIMENT),
        (TargetIRIs.NEW_RXN_EXP_1_IRI.value, TargetIRIs.NEW_RXN_EXP_1_PERFORMANCE_INDICATOR_IRI_LIST.value, onto.ONTORXN_REACTIONVARIATION),
        (TargetIRIs.NEW_RXN_EXP_2_IRI.value, TargetIRIs.NEW_RXN_EXP_2_PERFORMANCE_INDICATOR_IRI_LIST.value, onto.ONTORXN_REACTIONVARIATION),
        (TargetIRIs.NEW_RXN_EXP_3_IRI.value, TargetIRIs.NEW_RXN_EXP_3_PERFORMANCE_INDICATOR_IRI_LIST.value, onto.ONTORXN_REACTIONVARIATION),
    ],
)
def test_getExpPerformanceIndicator(initialise_triples, rxnexp_iri, rxnexp_pref_indicator_iri, rxn_type):
    sparql_client = initialise_triples
    perf_ind_list = sparql_client.getExpPerformanceIndicator(rxnexp_iri)
    assert len(rxnexp_pref_indicator_iri) == len(perf_ind_list)
    for ind in perf_ind_list:
        assert all([isinstance(ind, onto.PerformanceIndicator), ind.clz is not None, isinstance(ind.objPropWithExp, list), ind.instance_iri in rxnexp_pref_indicator_iri])
    if rxn_type == onto.ONTORXN_REACTIONEXPERIMENT:
        for ind in perf_ind_list:
            assert all([ind.hasValue.hasUnit is not None, ind.hasValue.hasNumericalValue is not None])
    elif rxn_type == onto.ONTORXN_REACTIONVARIATION:
        for ind in perf_ind_list:
            assert all([ind.hasValue is None])
    else:
        assert False

@pytest.mark.parametrize(
    "rxnexp_iri,expected_rxn_type",
    [# here we are testing that the function should work wether the passed in iri is already a list or not
        (TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value, onto.ONTORXN_REACTIONEXPERIMENT),
        (TargetIRIs.EXAMPLE_RXN_EXP_2_IRI.value, onto.ONTORXN_REACTIONEXPERIMENT),
        (TargetIRIs.EXAMPLE_RXN_EXP_3_IRI.value, onto.ONTORXN_REACTIONEXPERIMENT),
        (TargetIRIs.EXAMPLE_RXN_EXP_4_IRI.value, onto.ONTORXN_REACTIONEXPERIMENT),
        (TargetIRIs.EXAMPLE_RXN_EXP_5_IRI.value, onto.ONTORXN_REACTIONEXPERIMENT),
        (TargetIRIs.NEW_RXN_EXP_1_IRI.value, onto.ONTORXN_REACTIONVARIATION),
        (TargetIRIs.NEW_RXN_EXP_2_IRI.value, onto.ONTORXN_REACTIONVARIATION),
        (TargetIRIs.NEW_RXN_EXP_3_IRI.value, onto.ONTORXN_REACTIONVARIATION),
    ],
)
def test_get_rdf_type_of_rxn_exp(initialise_triples, rxnexp_iri, expected_rxn_type):
    sparql_client = initialise_triples
    rxn_type = sparql_client.get_rdf_type_of_rxn_exp(rxnexp_iri)
    assert rxn_type == expected_rxn_type

@pytest.mark.parametrize(
    "rxnexp_iris,rxn_type,rxnexp_condition,rxnexp_perfind,input_chem,output_chem,reactor_assigned,chem_rxn",
    [# here we are testing that the function should work wether the passed in iri is already a list or not
        (TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value, TargetIRIs.RXNEXP_TYPE_DICT.value, TargetIRIs.RXNEXP_REACTION_CONDITION_DICT.value,
            TargetIRIs.RXNEXP_PERFORMANCE_INDICATOR_DICT.value, TargetIRIs.RXNEXP_INPUT_CHEMICAL_DICT.value, TargetIRIs.RXNEXP_OUTPUT_CHEMICAL_DICT.value,
            TargetIRIs.RXNEXP_REACTOR_ASSIGNED_DICT.value, TargetIRIs.RXNEXP_CHEMICAL_REACTION_IRI_DICT.value),
        ([TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value], TargetIRIs.RXNEXP_TYPE_DICT.value, TargetIRIs.RXNEXP_REACTION_CONDITION_DICT.value,
            TargetIRIs.RXNEXP_PERFORMANCE_INDICATOR_DICT.value, TargetIRIs.RXNEXP_INPUT_CHEMICAL_DICT.value, TargetIRIs.RXNEXP_OUTPUT_CHEMICAL_DICT.value,
            TargetIRIs.RXNEXP_REACTOR_ASSIGNED_DICT.value, TargetIRIs.RXNEXP_CHEMICAL_REACTION_IRI_DICT.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_2_IRI.value, TargetIRIs.RXNEXP_TYPE_DICT.value, TargetIRIs.RXNEXP_REACTION_CONDITION_DICT.value,
            TargetIRIs.RXNEXP_PERFORMANCE_INDICATOR_DICT.value, TargetIRIs.RXNEXP_INPUT_CHEMICAL_DICT.value, TargetIRIs.RXNEXP_OUTPUT_CHEMICAL_DICT.value,
            TargetIRIs.RXNEXP_REACTOR_ASSIGNED_DICT.value, TargetIRIs.RXNEXP_CHEMICAL_REACTION_IRI_DICT.value),
        ([TargetIRIs.EXAMPLE_RXN_EXP_2_IRI.value], TargetIRIs.RXNEXP_TYPE_DICT.value, TargetIRIs.RXNEXP_REACTION_CONDITION_DICT.value,
            TargetIRIs.RXNEXP_PERFORMANCE_INDICATOR_DICT.value, TargetIRIs.RXNEXP_INPUT_CHEMICAL_DICT.value, TargetIRIs.RXNEXP_OUTPUT_CHEMICAL_DICT.value,
            TargetIRIs.RXNEXP_REACTOR_ASSIGNED_DICT.value, TargetIRIs.RXNEXP_CHEMICAL_REACTION_IRI_DICT.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_3_IRI.value, TargetIRIs.RXNEXP_TYPE_DICT.value, TargetIRIs.RXNEXP_REACTION_CONDITION_DICT.value,
            TargetIRIs.RXNEXP_PERFORMANCE_INDICATOR_DICT.value, TargetIRIs.RXNEXP_INPUT_CHEMICAL_DICT.value, TargetIRIs.RXNEXP_OUTPUT_CHEMICAL_DICT.value,
            TargetIRIs.RXNEXP_REACTOR_ASSIGNED_DICT.value, TargetIRIs.RXNEXP_CHEMICAL_REACTION_IRI_DICT.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_4_IRI.value, TargetIRIs.RXNEXP_TYPE_DICT.value, TargetIRIs.RXNEXP_REACTION_CONDITION_DICT.value,
            TargetIRIs.RXNEXP_PERFORMANCE_INDICATOR_DICT.value, TargetIRIs.RXNEXP_INPUT_CHEMICAL_DICT.value, TargetIRIs.RXNEXP_OUTPUT_CHEMICAL_DICT.value,
            TargetIRIs.RXNEXP_REACTOR_ASSIGNED_DICT.value, TargetIRIs.RXNEXP_CHEMICAL_REACTION_IRI_DICT.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_5_IRI.value, TargetIRIs.RXNEXP_TYPE_DICT.value, TargetIRIs.RXNEXP_REACTION_CONDITION_DICT.value,
            TargetIRIs.RXNEXP_PERFORMANCE_INDICATOR_DICT.value, TargetIRIs.RXNEXP_INPUT_CHEMICAL_DICT.value, TargetIRIs.RXNEXP_OUTPUT_CHEMICAL_DICT.value,
            TargetIRIs.RXNEXP_REACTOR_ASSIGNED_DICT.value, TargetIRIs.RXNEXP_CHEMICAL_REACTION_IRI_DICT.value),
        (TargetIRIs.LIST_EXAMPLE_RXN_EXP.value, TargetIRIs.RXNEXP_TYPE_DICT.value, TargetIRIs.RXNEXP_REACTION_CONDITION_DICT.value,
            TargetIRIs.RXNEXP_PERFORMANCE_INDICATOR_DICT.value, TargetIRIs.RXNEXP_INPUT_CHEMICAL_DICT.value, TargetIRIs.RXNEXP_OUTPUT_CHEMICAL_DICT.value,
            TargetIRIs.RXNEXP_REACTOR_ASSIGNED_DICT.value, TargetIRIs.RXNEXP_CHEMICAL_REACTION_IRI_DICT.value),
        # TODO (TargetIRIs.NEW_RXN_EXP_1_IRI.value, []),isVariationOf
        # TODO (TargetIRIs.NEW_RXN_EXP_2_IRI.value, []),isVariationOf
        # TODO (TargetIRIs.NEW_RXN_EXP_3_IRI.value, []),isVariationOf
    ],
)
def test_getReactionExperiment(initialise_triples, rxnexp_iris, rxn_type, rxnexp_condition, rxnexp_perfind, input_chem, output_chem, reactor_assigned, chem_rxn):
    sparql_client = initialise_triples
    rxn_exp_list = sparql_client.getReactionExperiment(rxnexp_iris)
    length = len(rxnexp_iris) if isinstance(rxnexp_iris, list) else 1
    assert len(rxn_exp_list) == length
    for exp in rxn_exp_list:
        assert exp.clz == rxn_type.get(exp.instance_iri)
        assert len(exp.hasReactionCondition) == len(rxnexp_condition.get(exp.instance_iri))
        assert len(exp.hasPerformanceIndicator) == len(rxnexp_perfind.get(exp.instance_iri))
        assert len(exp.hasInputChemical) == len(input_chem.get(exp.instance_iri))
        assert len(exp.hasOutputChemical) == len(output_chem.get(exp.instance_iri))
        assert all(con in [c.instance_iri for c in exp.hasReactionCondition] for con in rxnexp_condition.get(exp.instance_iri))
        assert all(pre in [pi.instance_iri for pi in exp.hasPerformanceIndicator] for pre in rxnexp_perfind.get(exp.instance_iri))
        assert all(inp in [ic.instance_iri for ic in exp.hasInputChemical] for inp in input_chem.get(exp.instance_iri))
        assert all(out in [oc.instance_iri for oc in exp.hasOutputChemical] for out in output_chem.get(exp.instance_iri))
        assert exp.isAssignedTo == reactor_assigned.get(exp.instance_iri)
        assert exp.isOccurenceOf.instance_iri == chem_rxn.get(exp.instance_iri)

def test_getNewExperimentFromDoE(initialise_triples):
    sparql_client = initialise_triples
    new_exp_iri = sparql_client.getNewExperimentFromDoE(TargetIRIs.DOE_IRI.value)
    assert new_exp_iri == TargetIRIs.DOE_NEW_EXP_IRI.value

def test_getDoEHistoricalData(initialise_triples):
    sparql_client = initialise_triples
    # sparql_client = ChemistryAndRobotsSparqlClient()
    hist_data_instance = sparql_client.getDoEHistoricalData(TargetIRIs.DOE_HIST_DATA_IRI.value)
    assert isinstance(hist_data_instance, onto.HistoricalData)
    assert len(hist_data_instance.refersTo) == len(TargetIRIs.DOE_HIST_DATE_REFERTO_IRI.value)
    assert all(iri in [h.instance_iri for h in hist_data_instance.refersTo] for iri in TargetIRIs.DOE_HIST_DATE_REFERTO_IRI.value)

def test_get_doe_instance(initialise_triples):
    sparql_client = initialise_triples
    doe_instance = sparql_client.get_doe_instance(TargetIRIs.DOE_IRI.value)
    # Check Domain
    assert doe_instance.hasDomain.instance_iri == TargetIRIs.DOE_DOMAIN_IRI.value
    lst_var = [v.instance_iri for v in doe_instance.hasDomain.hasDesignVariable]
    assert len(lst_var) == len(TargetIRIs.DOE_CONT_VAR_IRI_LIST.value)
    assert all(var in lst_var for var in TargetIRIs.DOE_CONT_VAR_IRI_LIST.value)
    assert all(all([isinstance(var, onto.ContinuousVariable), var.refersTo is not None, var.upperLimit > var.lowerLimit]) for var in doe_instance.hasDomain.hasDesignVariable)
    # Check SystemResponse
    assert all(all([isinstance(r, onto.SystemResponse), r.refersTo is not None]) for r in doe_instance.hasSystemResponse)
    dct_m = {s.instance_iri:s.maximise for s in doe_instance.hasSystemResponse}
    assert all([int(TargetIRIs.DOE_SYS_RES_MAXIMISE_DICT.value[s]) == int(dct_m.get(s)) for s in TargetIRIs.DOE_SYS_RES_MAXIMISE_DICT.value])
    # Check Strategy
    strategy = doe_instance.usesStrategy
    assert strategy.instance_iri == TargetIRIs.DOE_STRATEGY_IRI.value
    assert isinstance(strategy, onto.TSEMO)
    assert strategy.nRetries is not None
    assert strategy.nSpectralPoints is not None
    assert strategy.nGenerations is not None
    assert strategy.populationSize is not None

@pytest.mark.parametrize(
    "rxn_variation_iri,expected_rxn_rxp_iri",
    [# here we are testing that the function should work wether the passed in iri is already a list or not
        (TargetIRIs.NEW_RXN_EXP_1_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value),
        (TargetIRIs.NEW_RXN_EXP_2_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value),
        (TargetIRIs.NEW_RXN_EXP_3_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value),
    ],
)
def test_get_rxn_exp_iri_given_rxn_variation(initialise_triples, rxn_variation_iri, expected_rxn_rxp_iri):
    sparql_client = initialise_triples
    # sparql_client = ChemistryAndRobotsSparqlClient()
    rxn_exp_iri = sparql_client.get_rxn_exp_iri_given_rxn_variation(rxn_variation_iri)
    assert rxn_exp_iri == expected_rxn_rxp_iri

#############################################
## sparql_client.py functions to be tested ##
#############################################
# def test_(initialise_triples):
#     sparql_client = initialise_triples
#     sparql_client = ChemistryAndRobotsSparqlClient()
#     pass

# get_output_chemical_of_rxn_exp
# get_ontocape_material
# get_autosampler_from_vapourtec_rs400
# sort_r2_pumps_in_vapourtec_rs400
# get_rxn_con_or_perf_ind

# get_r4_reactor_rxn_exp_assigned_to

# updateNewExperimentInKG
# create_equip_settings_for_rs400_from_rxn_exp
# get_autosampler_site_given_input_chemical
# write_equip_settings_to_kg
# write_hplc_report_path_to_kg

# get_hplc_local_report_folder_path
# get_matching_species_from_hplc_results
# get_species_molar_mass_kilogrampermole
# get_species_density
# get_species_material_cost
# get_species_eco_score
# get_reactor_volume_given_reactor
# get_rxn_exp_associated_with_hplc_report
# get_internal_standard_associated_with_hplc_report

def test_get_all_autosampler_with_fill(initialise_triples):
    sparql_client = initialise_triples
    response = sparql_client.get_all_autosampler_with_fill()
    assert len(response) == 1
    autosampler = response[0]
    assert isinstance(autosampler, onto.AutoSampler)
    assert autosampler.instance_iri == TargetIRIs.AUTOSAMPLER_DUMMY_IRI.value

def test_get_r4_reactor_given_vapourtec_rs400(initialise_triples):
    sparql_client = initialise_triples
    response = sparql_client.get_r4_reactor_given_vapourtec_rs400(TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value)
    assert len(response) == len(TargetIRIs.LIST_DUMMY_R4REACTORS.value)
    list_reactor_iri = [res.instance_iri for res in response]
    assert len(set(list_reactor_iri).difference(set(TargetIRIs.LIST_DUMMY_R4REACTORS.value))) == 0

def test_get_r2_pump_given_vapourtec_rs400(initialise_triples):
    sparql_client = initialise_triples
    response = sparql_client.get_r2_pump_given_vapourtec_rs400(TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value)
    assert len(response) == len(TargetIRIs.LIST_DUMMY_R2PUMPS.value)
    list_pumps_iri = [res.instance_iri for res in response]
    assert len(set(list_pumps_iri).difference(set(TargetIRIs.LIST_DUMMY_R2PUMPS.value))) == 0

# TODO commented out for now, decide whether to keep it before merging back to develop based on the function in sparql_client.py
# @pytest.mark.parametrize(
#     "r4_reactor_iri,rxn_exp_conducted",
#     [
#         (TargetIRIs.VAPOURTECR4REACTOR_DUMMY_IRI.value, TargetIRIs.LIST_VAPR4_DUMMY_CONDUCTED_RXN_EXP.value),
#         (TargetIRIs.VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI.value, TargetIRIs.LIST_RXN_EXP_ASSIGNEDTO_VAPR4_ANOTHER_DUMMY.value),
#     ],
# )
# def test_get_rxn_exp_conducted_in_r4_reactor(initialise_triples, r4_reactor_iri, rxn_exp_conducted):
#     sparql_client = initialise_triples
#     response = sparql_client.get_rxn_exp_conducted_in_r4_reactor(r4_reactor_iri)
#     assert len(response) == len(rxn_exp_conducted)
#     assert len(set(response).difference(set(rxn_exp_conducted))) == 0

@pytest.mark.parametrize(
    "r4_reactor_iri,rxn_exp_assigned",
    [
        (TargetIRIs.VAPOURTECR4REACTOR_DUMMY_IRI.value, TargetIRIs.LIST_RXN_EXP_ASSIGNEDTO_VAPR4_DUMMY.value),
        (TargetIRIs.VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI.value, TargetIRIs.LIST_RXN_EXP_ASSIGNEDTO_VAPR4_ANOTHER_DUMMY.value),
    ],
)
def test_get_rxn_exp_assigned_to_r4_reactor(initialise_triples, r4_reactor_iri, rxn_exp_assigned):
    sparql_client = initialise_triples
    response = sparql_client.get_rxn_exp_assigned_to_r4_reactor(r4_reactor_iri)
    assert len(response) == len(rxn_exp_assigned)
    assert len(set(response).difference(set(rxn_exp_assigned))) == 0

# TODO commented out for now, decide whether to keep it before merging back to develop based on the function in sparql_client.py
# @pytest.mark.parametrize(
#     "r4_reactor_iri,rxn_exp_pending",
#     [
#         (TargetIRIs.VAPOURTECR4REACTOR_DUMMY_IRI.value, []),
#         (TargetIRIs.VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI.value, []),
#     ],
# )
# def test_get_rxn_exp_pending_for_r4_reactor(initialise_triples, r4_reactor_iri, rxn_exp_pending):
#     sparql_client = initialise_triples
#     response = sparql_client.get_rxn_exp_pending_for_r4_reactor(r4_reactor_iri)
#     assert len(response) == len(rxn_exp_pending)
#     assert len(set(response).difference(set(rxn_exp_pending))) == 0

@pytest.mark.parametrize(
    "rxnexp_iri,input_chemical_iri",
    [
        (TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value, TargetIRIs.LIST_RXN_EXP_1_INPUT_CHEMICAL_IRI.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_2_IRI.value, TargetIRIs.LIST_RXN_EXP_2_INPUT_CHEMICAL_IRI.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_3_IRI.value, TargetIRIs.LIST_RXN_EXP_3_INPUT_CHEMICAL_IRI.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_4_IRI.value, TargetIRIs.LIST_RXN_EXP_4_INPUT_CHEMICAL_IRI.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_5_IRI.value, TargetIRIs.LIST_RXN_EXP_5_INPUT_CHEMICAL_IRI.value),
    ],
)
def test_get_input_chemical_of_rxn_exp(initialise_triples, rxnexp_iri, input_chemical_iri):
    sparql_client = initialise_triples
    response = sparql_client.get_input_chemical_of_rxn_exp(rxnexp_iri)
    list_input_chemical = [res.instance_iri for res in response]
    assert len(list_input_chemical) == len(input_chemical_iri)
    assert len(set(list_input_chemical).difference(set(input_chemical_iri))) == 0

def test_get_vapourtec_rs400_given_autosampler(initialise_triples):
    sparql_client = initialise_triples
    autosampler = sparql_client.get_autosampler(TargetIRIs.AUTOSAMPLER_DUMMY_IRI.value)
    assert autosampler.instance_iri == TargetIRIs.AUTOSAMPLER_DUMMY_IRI.value
    response = sparql_client.get_vapourtec_rs400_given_autosampler(autosampler)
    assert  response.instance_iri == TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value

@pytest.mark.parametrize(
    "new_rxn_exp_iri,list_r4_reactor_iri,vapourtec_rs400",
    [
        (TargetIRIs.NEW_RXN_EXP_1_IRI.value, TargetIRIs.LIST_DUMMY_R4REACTORS.value, TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value),
        (TargetIRIs.NEW_RXN_EXP_2_IRI.value, TargetIRIs.LIST_DUMMY_R4REACTORS.value, TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value),
        (TargetIRIs.NEW_RXN_EXP_3_IRI.value, TargetIRIs.LIST_DUMMY_R4REACTORS.value, TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value),
    ],
)
def test_get_preferred_vapourtec_rs400(initialise_triples, new_rxn_exp_iri, list_r4_reactor_iri, vapourtec_rs400):
    sparql_client = initialise_triples
    response = sparql_client.getReactionExperiment(new_rxn_exp_iri)
    assert len(response) == 1
    assert response[0].instance_iri == new_rxn_exp_iri
    preferred_rs400, preferred_r4_reactor = sparql_client.get_preferred_vapourtec_rs400(response[0])
    assert preferred_r4_reactor.instance_iri in list_r4_reactor_iri
    assert preferred_rs400.instance_iri == vapourtec_rs400

    # Change the status to Null
    sparql_client.update_vapourtec_rs400_state(vapourtec_rs400, onto.ONTOVAPOURTEC_NULL)
    # Now perform the same checking
    new_rs400, new_r4_reactor = sparql_client.get_preferred_vapourtec_rs400(response[0])
    # Change back the status to Idle
    sparql_client.update_vapourtec_rs400_state(vapourtec_rs400, onto.ONTOVAPOURTEC_IDLE)
    # Now perform the same checking, the returned values should be None, None
    assert None == new_rs400
    assert None == new_r4_reactor

@pytest.mark.parametrize(
    "new_rxn_exp_iri,r4_reactor_iri",
    [
        (TargetIRIs.NEW_RXN_EXP_1_IRI.value, TargetIRIs.VAPOURTECR4REACTOR_DUMMY_IRI.value),
        (TargetIRIs.NEW_RXN_EXP_2_IRI.value, TargetIRIs.VAPOURTECR4REACTOR_DUMMY_IRI.value),
        (TargetIRIs.NEW_RXN_EXP_3_IRI.value, TargetIRIs.VAPOURTECR4REACTOR_DUMMY_IRI.value),
        (TargetIRIs.NEW_RXN_EXP_1_IRI.value, TargetIRIs.VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI.value),
        (TargetIRIs.NEW_RXN_EXP_2_IRI.value, TargetIRIs.VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI.value),
        (TargetIRIs.NEW_RXN_EXP_3_IRI.value, TargetIRIs.VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI.value),
    ],
)
def test_assign_and_remove_rxn_exp_to_r4_reactor(initialise_triples, new_rxn_exp_iri, r4_reactor_iri):
    sparql_client = initialise_triples
    response1 = sparql_client.get_rxn_exp_assigned_to_r4_reactor(r4_reactor_iri)
    assert new_rxn_exp_iri not in response1

    sparql_client.assign_rxn_exp_to_r4_reactor(new_rxn_exp_iri, r4_reactor_iri)
    response2 = sparql_client.get_rxn_exp_assigned_to_r4_reactor(r4_reactor_iri)
    assert new_rxn_exp_iri in response2

    sparql_client.remove_rxn_exp_from_r4_reactor(new_rxn_exp_iri, r4_reactor_iri)
    response3 = sparql_client.get_rxn_exp_assigned_to_r4_reactor(r4_reactor_iri)
    assert new_rxn_exp_iri not in response3

@pytest.mark.parametrize(
    "rxn_exp_iri,prior_rxn_exp",
    [
        (TargetIRIs.RXN_EXP_QUEUE_1.value, TargetIRIs.RXN_EXP_1_PRIOR.value),
        (TargetIRIs.RXN_EXP_QUEUE_2.value, TargetIRIs.RXN_EXP_2_PRIOR.value),
        (TargetIRIs.RXN_EXP_QUEUE_3.value, TargetIRIs.RXN_EXP_3_PRIOR.value),
        (TargetIRIs.RXN_EXP_QUEUE_4.value, TargetIRIs.RXN_EXP_4_PRIOR.value),
        (TargetIRIs.RXN_EXP_QUEUE_5.value, TargetIRIs.RXN_EXP_5_PRIOR.value),
        (TargetIRIs.RXN_EXP_QUEUE_6.value, TargetIRIs.RXN_EXP_6_PRIOR.value),
        (TargetIRIs.RXN_EXP_QUEUE_7.value, TargetIRIs.RXN_EXP_7_PRIOR.value),
    ],
)
def test_get_prior_rxn_exp_in_queue(initialise_triples, rxn_exp_iri, prior_rxn_exp):
    sparql_client = initialise_triples
    rxn_exp_queue = sparql_client.get_prior_rxn_exp_in_queue(rxn_exp_iri)
    assert all(item in prior_rxn_exp for item in [*rxn_exp_queue])

@pytest.mark.parametrize(
    "rxn_exp_iri,rxn_type,chem_rxn_iri,reactant,product,catalyst,solvent",
    [
        (TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value, onto.ONTORXN_REACTIONEXPERIMENT, TargetIRIs.CHEMICAL_REACTION_IRI.value,
        TargetIRIs.REACTANT_SPECIES_DICTIONARY.value, TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
        TargetIRIs.CATALYST_SPECIES_DICTIONARY.value, TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_2_IRI.value, onto.ONTORXN_REACTIONEXPERIMENT, TargetIRIs.CHEMICAL_REACTION_IRI.value,
        TargetIRIs.REACTANT_SPECIES_DICTIONARY.value, TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
        TargetIRIs.CATALYST_SPECIES_DICTIONARY.value, TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_3_IRI.value, onto.ONTORXN_REACTIONEXPERIMENT, TargetIRIs.CHEMICAL_REACTION_IRI.value,
        TargetIRIs.REACTANT_SPECIES_DICTIONARY.value, TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
        TargetIRIs.CATALYST_SPECIES_DICTIONARY.value, TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_4_IRI.value, onto.ONTORXN_REACTIONEXPERIMENT, TargetIRIs.CHEMICAL_REACTION_IRI.value,
        TargetIRIs.REACTANT_SPECIES_DICTIONARY.value, TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
        TargetIRIs.CATALYST_SPECIES_DICTIONARY.value, TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_5_IRI.value, onto.ONTORXN_REACTIONEXPERIMENT, TargetIRIs.CHEMICAL_REACTION_IRI.value,
        TargetIRIs.REACTANT_SPECIES_DICTIONARY.value, TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
        TargetIRIs.CATALYST_SPECIES_DICTIONARY.value, TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value),
        (TargetIRIs.NEW_RXN_EXP_1_IRI.value, onto.ONTORXN_REACTIONVARIATION, TargetIRIs.CHEMICAL_REACTION_IRI.value,
        TargetIRIs.REACTANT_SPECIES_DICTIONARY.value, TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
        TargetIRIs.CATALYST_SPECIES_DICTIONARY.value, TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value),
        (TargetIRIs.NEW_RXN_EXP_2_IRI.value, onto.ONTORXN_REACTIONVARIATION, TargetIRIs.CHEMICAL_REACTION_IRI.value,
        TargetIRIs.REACTANT_SPECIES_DICTIONARY.value, TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
        TargetIRIs.CATALYST_SPECIES_DICTIONARY.value, TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value),
        (TargetIRIs.NEW_RXN_EXP_3_IRI.value, onto.ONTORXN_REACTIONVARIATION, TargetIRIs.CHEMICAL_REACTION_IRI.value,
        TargetIRIs.REACTANT_SPECIES_DICTIONARY.value, TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
        TargetIRIs.CATALYST_SPECIES_DICTIONARY.value, TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value),
    ],
)
def test_get_chemical_reaction(initialise_triples, rxn_exp_iri, rxn_type, chem_rxn_iri, reactant, product, catalyst, solvent):
    sparql_client = initialise_triples
    if rxn_type == onto.ONTORXN_REACTIONEXPERIMENT:
        chem_rxn = sparql_client.get_chemical_reaction(rxn_exp_iri)
    elif rxn_type == onto.ONTORXN_REACTIONVARIATION:
        chem_rxn = sparql_client.get_chemical_reaction_of_rxn_variation(rxn_exp_iri)
    else:
        assert False
    assert chem_rxn.instance_iri == chem_rxn_iri
    dict_reactant = {reactant.instance_iri:reactant.hasUniqueSpecies for reactant in chem_rxn.hasReactant}
    assert dict_reactant == reactant
    dict_product = {product.instance_iri:product.hasUniqueSpecies for product in chem_rxn.hasProduct}
    assert dict_product == product
    dict_catalyst = {catalyst.instance_iri:catalyst.hasUniqueSpecies for catalyst in chem_rxn.hasCatalyst}
    assert dict_catalyst == catalyst
    dict_solvent = {solvent.instance_iri:solvent.hasUniqueSpecies for solvent in chem_rxn.hasSolvent}
    assert dict_solvent == solvent

def test_get_raw_hplc_report_path_and_extension():
    # TODO implement this test case once the file server is working
    pass

def test_get_internal_standard(initialise_triples):
    sparql_client = initialise_triples
    internal_standard = sparql_client.get_internal_standard(TargetIRIs.HPLCMETHOD_DUMMY_IRI.value)
    assert internal_standard.instance_iri == TargetIRIs.PHASECOMPONENT_INTERNAL_STANDARD_IRI.value
    assert internal_standard.representsOccurenceOf == TargetIRIs.ONTOSPECIES_INTERNAL_STANDARD_IRI.value
    assert internal_standard.hasProperty.hasValue.numericalValue == TargetIRIs.MOLARITY_INTERNAL_STANDARD.value

def test_get_hplc_method(initialise_triples):
    sparql_client = initialise_triples
    hplc_method = sparql_client.get_hplc_method(TargetIRIs.HPLCMETHOD_DUMMY_IRI.value)
    internal_standard = sparql_client.get_internal_standard(TargetIRIs.HPLCMETHOD_DUMMY_IRI.value)
    assert hplc_method.instance_iri == TargetIRIs.HPLCMETHOD_DUMMY_IRI.value
    assert hplc_method.usesInternalStandard == internal_standard
    assert all(rf.refersToSpecies is not None for rf in hplc_method.hasResponseFactor)
    assert all(rt.refersToSpecies is not None for rt in hplc_method.hasRetentionTime)

def test_get_hplc_method_given_hplc_report(initialise_triples):
    sparql_client = initialise_triples
    hplc_method = sparql_client.get_hplc_method_given_hplc_report(TargetIRIs.HPLCREPORT_DUMMY_IRI.value)
    internal_standard = sparql_client.get_internal_standard(TargetIRIs.HPLCMETHOD_DUMMY_IRI.value)
    assert hplc_method.instance_iri == TargetIRIs.HPLCMETHOD_DUMMY_IRI.value
    assert hplc_method.usesInternalStandard == internal_standard
    assert all(rf.refersToSpecies is not None for rf in hplc_method.hasResponseFactor)
    assert all(rt.refersToSpecies is not None for rt in hplc_method.hasRetentionTime)

def test_get_chromatogram_point_of_hplc_report(initialise_triples):
    sparql_client = initialise_triples
    list_chrom_pts = sparql_client.get_chromatogram_point_of_hplc_report(TargetIRIs.HPLCREPORT_DUMMY_IRI.value)
    assert len(list_chrom_pts) == len(TargetIRIs.LIST_CHROMATOGRAMPOINT_IRI.value)
    for pt in list_chrom_pts:
        assert pt.instance_iri in TargetIRIs.LIST_CHROMATOGRAMPOINT_IRI.value
        assert pt.indicatesComponent.instance_iri is not None
        assert pt.indicatesComponent.representsOccurenceOf is not None
        assert isinstance(pt.indicatesComponent.hasProperty, onto.OntoCAPE_PhaseComponentConcentration)
        assert pt.indicatesComponent.hasProperty.hasValue.numericalValue is not None
        assert pt.atRetentionTime.hasValue.hasNumericalValue > 0
        assert pt.atRetentionTime.hasValue.hasUnit is not None
        assert pt.hasPeakArea.hasValue.hasNumericalValue > 0
        assert pt.hasPeakArea.hasValue.hasUnit is not None

def test_get_existing_hplc_report(initialise_triples):
    sparql_client = initialise_triples
    list_chrom_pts = sparql_client.get_chromatogram_point_of_hplc_report(TargetIRIs.HPLCREPORT_DUMMY_IRI.value)
    assert len(list_chrom_pts) == len(TargetIRIs.LIST_CHROMATOGRAMPOINT_IRI.value)
    assert all(pt.instance_iri in TargetIRIs.LIST_CHROMATOGRAMPOINT_IRI.value for pt in list_chrom_pts)
    hplc_report = sparql_client.get_existing_hplc_report(TargetIRIs.HPLCREPORT_DUMMY_IRI.value)
    assert all(pt in list_chrom_pts for pt in hplc_report.records)
    assert hplc_report.generatedFor.instance_iri == TargetIRIs.CHEMICAL_SOLUTION_FOR_OUTPUTCHEMICAL_4_IRI.value
    assert hplc_report.hasReportPath is not None
    assert hplc_report.localReportFile is not None
    assert hplc_report.lastLocalModifiedAt > 0
    assert hplc_report.lastUploadedAt > 0

def test_process_raw_hplc_report(initialise_triples):
    # TODO implement this test case once the file server is sorted
    pass

def test_get_hplc_job_given_hplc_report_instance(initialise_triples):
    sparql_client = initialise_triples
    hplc_report_instance = sparql_client.get_existing_hplc_report(TargetIRIs.HPLCREPORT_DUMMY_IRI.value)
    hplc_job_instance = sparql_client.get_hplc_job_given_hplc_report_instance(hplc_report_instance)
    assert hplc_job_instance.instance_iri == TargetIRIs.HPLCJOB_DUMMY_IRI.value
    assert hplc_job_instance.characterises.instance_iri == TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value
    assert hplc_job_instance.usesMethod.instance_iri == TargetIRIs.HPLCMETHOD_DUMMY_IRI.value
    assert hplc_job_instance.hasReport == hplc_report_instance

def test_get_hplc_job_given_hplc_report_iri(initialise_triples):
    sparql_client = initialise_triples
    hplc_job_instance = sparql_client.get_hplc_job_given_hplc_report_iri(TargetIRIs.HPLCREPORT_DUMMY_IRI.value)
    assert hplc_job_instance.instance_iri == TargetIRIs.HPLCJOB_DUMMY_IRI.value
    assert hplc_job_instance.characterises.instance_iri == TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value
    assert hplc_job_instance.usesMethod.instance_iri == TargetIRIs.HPLCMETHOD_DUMMY_IRI.value
    assert hplc_job_instance.hasReport == sparql_client.get_existing_hplc_report(TargetIRIs.HPLCREPORT_DUMMY_IRI.value)

def test_get_hplc_job(initialise_triples):
    sparql_client = initialise_triples
    hplc_job_instance = sparql_client.get_hplc_job(TargetIRIs.HPLCJOB_DUMMY_IRI.value)
    assert hplc_job_instance.instance_iri == TargetIRIs.HPLCJOB_DUMMY_IRI.value
    assert hplc_job_instance.characterises.instance_iri == TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value
    assert hplc_job_instance.usesMethod.instance_iri == TargetIRIs.HPLCMETHOD_DUMMY_IRI.value
    assert hplc_job_instance.hasReport.instance_iri == TargetIRIs.HPLCREPORT_DUMMY_IRI.value

def get_endpoint(docker_container):
    # Retrieve SPARQL endpoint for temporary testcontainer
    # endpoint acts as both Query and Update endpoint
    endpoint = 'http://' + docker_container.get_container_host_ip().replace('localnpipe', 'localhost') + ':' \
               + docker_container.get_exposed_port(9999)
    # 'kb' is default namespace in Blazegraph
    endpoint += '/blazegraph/namespace/kb/sparql'
    return endpoint

# method adopted from https://github.com/pytest-dev/pytest/issues/5502#issuecomment-647157873
def clear_loggers():
    """Remove handlers from all loggers"""
    import logging
    loggers = [logging.getLogger()] + list(logging.Logger.manager.loggerDict.values())
    for logger in loggers:
        handlers = getattr(logger, 'handlers', [])
        for handler in handlers:
            logger.removeHandler(handler)
