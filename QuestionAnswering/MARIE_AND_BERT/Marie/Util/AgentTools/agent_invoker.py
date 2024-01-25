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
                value_node = data['result']['Thermodynamic data over a selected T range at a single P point']
                if output == 'Thermo property':
                    result['Enthalpy'] = value_node['Enthalpy']
                    result['Entropy'] = value_node['Entropy']
                    result['Internal Energy'] = value_node['Internal energy']
                    result['Gibbs Energy'] = value_node['Gibbs energy']
                    result['Heat Capacity at Constant Pressure'] = value_node['Heat capacity at constant pressure']
                    result['Heat capacity at Constant Volume'] = value_node['Heat capacity at constant volume']
                elif output == 'Heat capacity':
                    result['Heat capacity at Constant Pressure'] = \
                        {"y": value_node['Heat capacity at constant pressure'],
                         "x": value_node["Temperature"]}
                    result['Heat capacity at Constant Volume'] = \
                        {"y": value_node['Heat capacity at constant volume'],
                         "x": value_node["Temperature"]}
                else:
                    result[output] = value_node[output]
            else:
                value_node = data['result']['Thermodynamic data for a single T, P point']
                if output == 'Thermo property':
                    result['Enthalpy'] = value_node['Enthalpy']
                    result['Entropy'] = value_node['Entropy']
                    result['Internal Energy'] = value_node['Internal energy']
                    result['Gibbs Energy'] = value_node['Gibbs energy']
                    result['Heat capacity at Constant Pressure'] = value_node['Heat capacity at constant pressure']
                    result['Heat capacity at Constant Volume'] = value_node['Heat capacity at constant volume']
                elif output == 'Heat capacity':
                    result['Heat capacity at Constant Pressure'] = value_node['Heat capacity at constant pressure']
                    result['Heat capacity at Constant Volume'] = value_node['Heat capacity at constant volume']
                else:
                    result[output] = value_node[output]
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
