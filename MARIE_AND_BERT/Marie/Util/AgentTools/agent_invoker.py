import json
import requests
import json
import re


class AgentInvoker():

    def __init__(self):
        pass
        # Parameters for agent invokation. Qualifiers is an optional parameter required by Thermo Agent

    def run(self, agent, output, species_iri, qualifier={}):
        output = self.camel_to_sentence(output)

        if agent == 'ontopceagent':
            result = self.invoke_pce_agent(species_iri, output)
        elif agent == 'ontothermoagent':
            result = self.invoke_thermo_agent(species_iri, output, qualifier)
        else:
            result = None
        return result

    def invoke_thermo_agent(self, species_iri, output, qualifier={}):
        base_url = 'http://159.223.42.53:5001/api/thermoagent/calculate?'
        params = {'ontospecies_IRI': species_iri}
        if len(qualifier) > 0:
            params.update(qualifier)

        response = requests.get(base_url, params=params)
        print("full request:", response.request.url)
        try:
            data = json.loads(response.content)

            result = {}
            if len(qualifier) == 0:
                if output == 'Thermo property':
                    result['Enthalpy'] = \
                        data['result']['Thermodynamic data over a selected T range at a single P point']['Enthalpy']
                    result['Entropy'] = \
                        data['result']['Thermodynamic data over a selected T range at a single P point']['Entropy']
                    result['Internal Energy'] = \
                        data['result']['Thermodynamic data over a selected T range at a single P point'][
                            'Internal energy']
                    result['Gibbs Energy'] = \
                        data['result']['Thermodynamic data over a selected T range at a single P point']['Gibbs energy']
                    result['Heat capacity at constant pressure'] = \
                        data['result']['Thermodynamic data over a selected T range at a single P point'][
                            'Heat capacity at constant pressure']
                    result['Heat capacity at constant volume'] = \
                        data['result']['Thermodynamic data over a selected T range at a single P point'][
                            'Heat capacity at constant volume']
                elif output == 'Heat capacity':
                    result['Heat capacity at constant pressure'] = \
                        data['result']['Thermodynamic data over a selected T range at a single P point'][
                            'Heat capacity at constant pressure']
                    result['Heat capacity at constant volume'] = \
                        data['result']['Thermodynamic data over a selected T range at a single P point'][
                            'Heat capacity at constant volume']
                else:
                    result[output] = \
                        data['result']['Thermodynamic data over a selected T range at a single P point'][output]
            else:
                if output == 'Thermo property':
                    result['Enthalpy'] = data['result']['Thermodynamic data for a single T, P point']['Enthalpy']
                    result['Entropy'] = data['result']['Thermodynamic data for a single T, P point']['Entropy']
                    result['Internal Energy'] = data['result']['Thermodynamic data for a single T, P point'][
                        'Internal energy']
                    result['Gibbs Energy'] = data['result']['Thermodynamic data for a single T, P point'][
                        'Gibbs energy']
                    result['Heat capacity at constant pressure'] = \
                        data['result']['Thermodynamic data for a single T, P point'][
                            'Heat capacity at constant pressure']
                    result['Heat capacity at constant volume'] = \
                        data['result']['Thermodynamic data for a single T, P point']['Heat capacity at constant volume']
                elif output == 'Heat capacity':
                    result['Heat capacity at constant pressure'] = \
                        data['result']['Thermodynamic data for a single T, P point'][
                            'Heat capacity at constant pressure']
                    result['Heat capacity at constant volume'] = \
                        data['result']['Thermodynamic data for a single T, P point']['Heat capacity at constant volume']
                else:
                    result[output] = data['result']['Thermodynamic data for a single T, P point'][output]
        except ValueError:
            return None

        return result

    def invoke_pce_agent(self, species_iri, output):
        base_url = 'http://159.223.42.53:5000/api/model/predict?'
        params = {'spec_iris': species_iri}
        result = {}
        response = requests.get(base_url, params=params)
        try:
            data = json.loads(response.content)
            result[output] = data['result']['solarCellPowerConversionEfficiency']
        except ValueError:
            return None

        return result

    def camel_to_sentence(self, s):
        # insert space before capital letters
        s = re.sub(r'(?<!^)(?=[A-Z])', ' ', s)
        # convert to lower case
        s = s.lower()
        # capitalize first letter
        s = s.capitalize()
        return s

    if __name__ == '__main__':
        agent = 'ontothermoagent'
