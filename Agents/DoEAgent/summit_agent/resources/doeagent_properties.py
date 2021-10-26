###--- Properties for Python script ---###

# Full SPARQL Query and Update endpoints of OntoRxn triple store
SPARQL_QUERY_ENDPOINT = 'http://kg.cmclinnovations.com:81/blazegraph/namespace/testontorxn/sparql'
SPARQL_UPDATE_ENDPOINT = 'http://kg.cmclinnovations.com:81/blazegraph/namespace/testontorxn/sparql'
TRIPLE_STORE_UPLOAD_SERVER = 'http://kg.cmclinnovations.com:81/blazegraph'
TRIPLE_STORE_UPLOAD_REPOSITORY = 'testontorxn'

# Periodic timescale for monitoring Derivation (seconds)
PERIODIC_TIMESCALE = 120

# Common NAMESPACE for creating OntoRxn ABoxes
NAMESPACE_KB_ONTORXN = 'https://theworldavatar.com/kb/ontorxn/'

# OntoAgent:Service IRI of DoEAgent
DOEAGENT_ONTOAGENT_SERVICE = 'http://www.theworldavatar.com/kb/agents/Service__DoE#Service'

# DoEAgent input JSON key for evaluating the inputs
DOEAGENT_INPUT_JSON_KAY = 'agent_input'