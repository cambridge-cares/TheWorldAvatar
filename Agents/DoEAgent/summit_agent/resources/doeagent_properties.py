###--- Properties for Python script ---###

# Full SPARQL Query and Update endpoints of OntoRxn triple store
SPARQL_QUERY_ENDPOINT = 'http://theworldavatar.com/blazegraph/namespace/textontorxn/sparql'
SPARQL_UPDATE_ENDPOINT = 'http://theworldavatar.com/blazegraph/namespace/textontorxn/sparql'
TRIPLE_STORE_UPLOAD_SERVER = 'http://theworldavatar.com/blazegraph'
TRIPLE_STORE_UPLOAD_REPOSITORY = 'textontorxn'

# Periodic timescale for monitoring Derivation (mins)
PERIODIC_TIMESCALE = 5

# Common NAMESPACE for creating OntoRxn ABoxes
NAMESPACE_KB_ONTORXN = 'https://theworldavatar.com/kb/ontorxn/'

# OntoAgent:Service IRI of DoEAgent
DOEAGENT_ONTOAGENT_SERVICE = 'http://www.theworldavatar.com/kb/agents/Service__DoE#Service'

# DoEAgent input JSON key for evaluating the inputs
DOEAGENT_INPUT_JSON_KAY = 'agent_input'