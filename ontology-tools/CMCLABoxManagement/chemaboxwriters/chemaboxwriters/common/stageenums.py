from chemaboxwriters.app_exceptions.app_exceptions import UnsupportedStage
from enum import Enum

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

def stage_name_to_enum(
    inpFileType: str
    )->Enum:
    try:
        inStage = aboxStages[inpFileType.upper()]
    except KeyError:
        raise UnsupportedStage
    return inStage