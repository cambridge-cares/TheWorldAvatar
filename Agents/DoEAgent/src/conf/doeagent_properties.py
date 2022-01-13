from pathlib import Path
import json

###--- Properties for Python script ---###
# This python module reads properties from doeagent_properties.json
with open(str(Path(__file__).absolute().parent) + '/doeagent_properties.json', "r") as f:
    doeagent_properties = json.load(f)

# Server and repository name of the triple store where the created OntoRxn:ReactionExperiment/OntoRxn:ReactionVariation instances should be uploaded to
# Note: TRIPLE_STORE_UPLOAD_SERVER should NOT end with '/', an example: 'http://kg.cmclinnovations.com:81/blazegraph'
triple_store_upload_repository = doeagent_properties['KNOWLEDGE_GRAPH']['TRIPLE_STORE_UPLOAD_SERVER']
TRIPLE_STORE_UPLOAD_SERVER = triple_store_upload_repository if not triple_store_upload_repository.endswith('/') else triple_store_upload_repository[:-1]
TRIPLE_STORE_UPLOAD_REPOSITORY = doeagent_properties['KNOWLEDGE_GRAPH']['TRIPLE_STORE_UPLOAD_REPOSITORY'] # an example: 'testontorxn'

# Full SPARQL Query and Update endpoints of OntoRxn triple store, this should be in line with the triple upload server and repository name provided above
SPARQL_QUERY_ENDPOINT = f'{TRIPLE_STORE_UPLOAD_SERVER}/namespace/{TRIPLE_STORE_UPLOAD_REPOSITORY}/sparql'
SPARQL_UPDATE_ENDPOINT = f'{TRIPLE_STORE_UPLOAD_SERVER}/namespace/{TRIPLE_STORE_UPLOAD_REPOSITORY}/sparql'

# Periodic timescale for monitoring Derivation (seconds)
PERIODIC_TIMESCALE = doeagent_properties['DERIVATION_CLIENT']['PERIODIC_TIMESCALE']

# Common NAMESPACE for creating OntoRxn ABoxes
# Note: (1) NAMESPACE_KB_ONTORXN removes the triple store name in TRIPLE_STORE_UPLOAD_SERVER and append 'ontorxn/' after it
# (2) TODO currently, this script only deals with 'blazegraph'
# an example if TRIPLE_STORE_UPLOAD_SERVER is 'http://kg.cmclinnovations.com:81/blazegraph': 'http://kg.cmclinnovations.com:81/ontorxn/'
namespace_kb_ontorxn = ''.join(TRIPLE_STORE_UPLOAD_SERVER.rsplit('blazegraph', 1)) if TRIPLE_STORE_UPLOAD_SERVER.endswith('blazegraph') else TRIPLE_STORE_UPLOAD_SERVER
NAMESPACE_KB_ONTORXN = namespace_kb_ontorxn + 'ontorxn/'

# OntoAgent:Service IRI of DoEAgent
DOEAGENT_ONTOAGENT_SERVICE = doeagent_properties['DOEAGENT_ONTOAGENT']['DOEAGENT_ONTOAGENT_SERVICE'] # an example: 'http://www.theworldavatar.com/kb/agents/Service__DoE#Service'

# DoEAgent input JSON key for evaluating the inputs
DOEAGENT_INPUT_JSON_KAY = doeagent_properties['DERIVATION_CLIENT']['DOEAGENT_INPUT_JSON_KAY']

# Derivation instances base URL
if "DERIVATION_INSTANCE_BASE_URL" in doeagent_properties['DERIVATION_CLIENT']:
    DERIVATION_INSTANCE_BASE_URL = doeagent_properties['DERIVATION_CLIENT']['DERIVATION_INSTANCE_BASE_URL'] 
else:
    DERIVATION_INSTANCE_BASE_URL = TRIPLE_STORE_UPLOAD_SERVER + '/' + TRIPLE_STORE_UPLOAD_REPOSITORY + '/'
