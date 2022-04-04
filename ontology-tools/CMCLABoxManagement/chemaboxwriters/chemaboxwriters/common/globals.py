from enum import Enum

#json keys
ENTRY_IRI='EntryIRI'
SPECIES_IRI='SpeciesIRI'
ENTRY_UUID='EntryUUID'

#default cc log extensions
CC_LOG_EXT= ".log,.g03,.g09,.g16"

#stage/pipeline tags
QUANTUM_CALC_TAG = 'QC'
ONTO_COMP_CHEM_TAG = 'OC'
ONTO_SPECIES_TAG = 'OS'
ONTO_PESSCAN_TAG = 'OPS'
ONTO_MOPS_INP_TAG = 'OMINP'
ONTO_MOPS_TAG = 'OM'

SUPPORTED_PIPELINES = [ONTO_COMP_CHEM_TAG, ONTO_SPECIES_TAG, ONTO_PESSCAN_TAG, ONTO_MOPS_TAG]

_aboxStagesMap = {
    QUANTUM_CALC_TAG: ['LOG', 'JSON'],
    ONTO_COMP_CHEM_TAG: ['JSON', 'CSV', 'OWL'],
    ONTO_SPECIES_TAG: ['JSON', 'CSV', 'OWL'],
    ONTO_PESSCAN_TAG: ['JSON', 'CSV', 'OWL'],
    ONTO_MOPS_TAG: ['JSON', 'CSV', 'OWL'],
    ONTO_MOPS_INP_TAG: ['JSON']
}

_aboxStages = []
for tag, stages in _aboxStagesMap.items():
    for stage in stages:
        _aboxStages.append(f"{tag}_{stage}")

aboxStages = Enum('aboxStages', _aboxStages)