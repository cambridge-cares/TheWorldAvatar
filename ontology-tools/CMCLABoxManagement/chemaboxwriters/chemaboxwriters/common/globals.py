from enum import Enum

# json keys
ENTRY_IRI = "EntryIRI"
SPECIES_IRI = "SpeciesIRI"
ENTRY_UUID = "EntryUUID"

# default cc log extensions
CC_LOG_EXT = "qc_log,log,out,g03,g09,g16"

# stage/pipeline tags
QUANTUM_CALC_TAG = "QC"
ONTO_COMP_CHEM_TAG = "OC"
ONTO_SPECIES_TAG = "OS"
ONTO_PESSCAN_TAG = "OPS"
ONTO_MOPS_INP_TAG = "OMINP"
ONTO_MOPS_TAG = "OM"

SUPPORTED_PIPELINES = [
    ONTO_COMP_CHEM_TAG,
    ONTO_SPECIES_TAG,
    ONTO_PESSCAN_TAG,
    ONTO_MOPS_TAG,
]

_aboxStagesMap = {
    QUANTUM_CALC_TAG: ["LOG", "JSON"],
    ONTO_COMP_CHEM_TAG: ["JSON", "CSV", "OWL"],
    ONTO_SPECIES_TAG: ["JSON", "CSV", "OWL"],
    ONTO_PESSCAN_TAG: ["JSON", "CSV", "OWL"],
    ONTO_MOPS_TAG: ["JSON", "CSV", "OWL"],
    ONTO_MOPS_INP_TAG: ["JSON", "XYZ"],
}

_aboxStages = []
for tag, stages in _aboxStagesMap.items():
    for stage in stages:
        _aboxStages.append(f"{tag}_{stage}")
_aboxStages.append("NOT_DEFINED")

aboxStages = Enum("aboxStages", _aboxStages)


CONFIG_FILE_ENV_VAR = "ABOXWRITERS_CONFIG_FILE"
TRIPLE_STORE_SPARQL_ENDPOINT_KEY = "triple_store_sparql_endpoint"
TRIPLE_STORE_SECRETS_FILE_KEY = "triple_store_secrets_file"
FILE_SERVER_UPLOAD_ENDPOINT_KEY = "file_server_upload_endpoint"
FILE_SERVER_SECRETS_FILE_KEY = "file_server_secrets_file"
FILE_SERVER_SUBDIR_KEY = "file_server_subdir"
UPLOAD_TO_FILE_SERVER = "upload_to_file_server"
UPLOAD_TO_TRIPLE_STORE = "upload_to_triple_store"

FILE_SERVER_KEYS = [
    FILE_SERVER_UPLOAD_ENDPOINT_KEY,
    FILE_SERVER_SECRETS_FILE_KEY,
    FILE_SERVER_SUBDIR_KEY,
]

TRIPLE_STORE_KEYS = [TRIPLE_STORE_SPARQL_ENDPOINT_KEY, TRIPLE_STORE_SECRETS_FILE_KEY]

CONFIG_FILE_KEYS = [
    UPLOAD_TO_FILE_SERVER,
    UPLOAD_TO_TRIPLE_STORE,
]
CONFIG_FILE_KEYS.extend(FILE_SERVER_KEYS)
CONFIG_FILE_KEYS.extend(TRIPLE_STORE_KEYS)
