from pyderivationagent.data_model.iris import ONTODERIVATION_DERIVATIONWITHTIMESERIES
from pyderivationagent.kg_operations import PyDerivationClient
deriv_client = PyDerivationClient('http://localhost:5001/ForecastingAgent', 'http://localhost:9999/blazegraph/namespace/country/sparql', 'http://localhost:9999/blazegraph/namespace/country/sparql')

# Create new forecast derivation
agent_iri = 'https://www.theworldavatar.com/resource/agents/Service__ForecastingAgent/Test'
derivation_input_set = [
                       'https://www.theworldavatar.com/kg/country/Number_773521f9-528e-40fc-9bcb-4218e49f40e8',
                        'https://www.theworldavatar.com/test/ForecastingModel_1',
                           'https://www.theworldavatar.com/test/Frequency_1',
                           'https://www.theworldavatar.com/test/OptimisationInterval_1',
    'https://www.theworldavatar.com/test/Duration_1'
]
derivation_type = ONTODERIVATION_DERIVATIONWITHTIMESERIES
# NOTE: It is recommended to import the derivation type from pyderivation agent directly;
#       however, string reference of the full IRI works as well
#derivation_type = "https://www.theworldavatar.com/kg/ontoderivation/DerivationWithTimeSeries"

# Markup derivation inputs and immediately compute and instantiate forecast
#derivation = deriv_client.createSyncDerivationForNewInfo(agent_iri, derivation_input_set, derivation_type)
#derivation_iri = derivation.getIri()

# Update existing forecast derivation
#deriv_client.unifiedUpdateDerivation(derivation_iri)
quantityIRI = 'https://www.theworldavatar.com/kg/country/Number_773521f9-528e-40fc-9bcb-4218e49f40e8'
a = deriv_client.getDerivationsOf([quantityIRI])
print(a)