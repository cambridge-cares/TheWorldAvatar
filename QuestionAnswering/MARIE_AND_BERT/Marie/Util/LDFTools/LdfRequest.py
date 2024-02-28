import requests
import json

class LdfRequest:

    def __init__(self, reactants, products):    
        self.base_url = 'http://159.223.42.53:3000/ldfserver/ontokin'
        self.params = {'ontology': 'ontokin', 'predicate': 'predicate=http://www.theworldavatar.com/kb/ontokin/ontokin.owl%23hasEquation'}
        self.reactants = reactants
        self.products = products

        # append each reactant to the reactants parameter
        for reactant in self.reactants:
            self.params.setdefault('reactants', []).append(reactant)
        # append each product to the products parameter
        for product in self.products:
            self.params.setdefault('products', []).append(product)

    def get_equations(self):
        if(len(self.reactants)==0 and len(self.products)==0):
            return set()
        
        # send the HTTP GET request

        response = requests.get(self.base_url, params=self.params)
        print("full request", response.request.url)
        # print the response content
        data = json.loads(response.content)

        equations = set()
        # Extract the value of hasEquation
        for item in data['@graph']:
            if 'hasEquation' in item:
                equations.add(item['hasEquation'])

        return list(equations)
    
