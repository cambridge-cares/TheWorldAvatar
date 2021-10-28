###--- Properties for Python script ---###

# Server and repository name of the triple store where the created OntoRxn:ReactionExperiment/OntoRxn:ReactionVariation instances should be uploaded to
TRIPLE_STORE_UPLOAD_SERVER = 'http://kg.cmclinnovations.com:81/blazegraph' # Note: this address should NOT end with '/'
TRIPLE_STORE_UPLOAD_REPOSITORY = 'testontorxn'

# Full SPARQL Query and Update endpoints of OntoRxn triple store, this should be in line with the triple upload server and repository name provided above
SPARQL_QUERY_ENDPOINT = f'{TRIPLE_STORE_UPLOAD_SERVER}/namespace/{TRIPLE_STORE_UPLOAD_REPOSITORY}/sparql'
SPARQL_UPDATE_ENDPOINT = f'{TRIPLE_STORE_UPLOAD_SERVER}/namespace/{TRIPLE_STORE_UPLOAD_REPOSITORY}/sparql'

# Periodic timescale for monitoring Derivation (seconds)
PERIODIC_TIMESCALE = 120

# Common NAMESPACE for creating OntoRxn ABoxes
NAMESPACE_KB_ONTORXN = 'https://theworldavatar.com/kb/ontorxn/'

# OntoAgent:Service IRI of DoEAgent
DOEAGENT_ONTOAGENT_SERVICE = 'http://www.theworldavatar.com/kb/agents/Service__DoE#Service'

# DoEAgent input JSON key for evaluating the inputs
DOEAGENT_INPUT_JSON_KAY = 'agent_input'
