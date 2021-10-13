import os
from pprint import pprint
from openbabel import pybel
from rasa.nlu.model import Interpreter
from SPARQLWrapper import SPARQLWrapper, POST, DIGEST, JSON

class OtherInterpreter:

    def __init__(self):
        self.nlu_model_directory = os.path.join('C:/TWA/TheWorldAvatar/JPS_Chatbot/jps-chatbot/UI/source/PCE_query'
                                                '/training/models', 'nlu')
        self.interpreter = Interpreter.load(self.nlu_model_directory)
        self.template = '''
        
        PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX OntoChemExp: <http://www.theworldavatar.com/ontology/ontochemexp/OntoChemExp.owl#>
        PREFIX OntoSpecies:<http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        SELECT ?DonorinChI ?PowerConversionEfficiencyValue
        WHERE {
              ?exp OntoChemExp:hasCommonProperties ?commonProperties ;
                   OntoChemExp:hasDataGroup ?dataGroup .
              ?commonProperties OntoChemExp:hasDimensionalQuantity ?DimQuantity1 .
              ?DimQuantity1 a OntoChemExp:JunctionArchitecture .
              ?DimQuantity1 OntoChemExp:hasValue ?JunctionArchitectureValue .
              FILTER regex(str(?JunctionArchitectureValue), 'bulk heterojunction')
              ?commonProperties OntoChemExp:hasDimensionalQuantity ?DimQuantity2 .
              ?DimQuantity2 a OntoChemExp:Acceptor .
              ?DimQuantity2 OntoChemExp:hasComponent ?AcceptorCompontent .
              ?AcceptorCompontent OntoChemExp:hasSpeciesLink ?AcceptorCompontentLink .
              ?AcceptorCompontentLink OntoChemExp:hasDatPreferredKey ?AcceptorType .
              FILTER (?AcceptorType != 'TiO2'^^xsd:string)
              ?commonProperties OntoChemExp:hasDimensionalQuantity ?DimQuantity3 .
              ?DimQuantity3 a OntoChemExp:Donor .
              ?DimQuantity3 OntoChemExp:hasComponent ?DonorCompontent .
              ?DonorCompontent OntoChemExp:hasSpeciesLink ?DonorCompontentLink .
              ?DonorCompontentLink OntoSpecies:inChI ?DonorinChI .
              FILTER (?DonorinChI = '%s'^^xsd:string)
              ?dataGroup OntoChemExp:hasDataPoint ?DataPoint .
              ?DataPoint OntoChemExp:hasDataPointX ?DataPointX .
              ?DataPointX OntoChemExp:refersTo ?Measurement ;
                          OntoChemExp:hasValue ?PowerConversionEfficiencyValue .
              ?Measurement a OntoChemExp:PowerConversionEfficiency .
          }
        '''

    def parse(self, question):
        stop_words = ['what', 'is', 'the']
        question_tokens = question.split(' ')
        tmp = [t for t in question_tokens if t not in stop_words]
        question = ' '.join(tmp).strip()
        result = self.interpreter.parse(question)
        return result

    def get_entities(self, result):
        species = None
        entities = result['entities']
        for e in entities:
            v = e['value']
            t = e['entity']
            if t == 'species':
                species = v
        return species

    def convert_to_inchi(self, SMILES):
        mol = pybel.readstring("smi", SMILES)
        inchi = mol.write("inchi")
        return str(inchi)

    def run(self, inchi):
        sparql = self.template % inchi.strip().replace('\n','')

        print('========================')
        print(sparql)
        print('========================')
        return self.fire_query(sparql)

    def ask_others(self, question):
        rst = self.parse(question)
        pprint(rst)
        species = self.get_entities(rst)
        # print(species)
        inchi = self.convert_to_inchi(species)
        result = self.run(inchi)
        return result



    def fire_query(self, query):
        namespace = "opvhopv15"
        sparql = SPARQLWrapper("http://www.theworldavatar.com/blazegraph/namespace/" + namespace + "/sparql")

        sparql.setQuery(query)
        sparql.setReturnFormat(JSON)
        results = sparql.query().convert()
        inchi = None
        pce = None
        for result in results["results"]["bindings"]:
            inchi = (result["DonorinChI"]["value"])
            pce = result['PowerConversionEfficiencyValue']['value']

        return pce



if __name__ == '__main__':
    oi = OtherInterpreter()
    rst = oi.parse('what is the pce of COc1ccc2c(c1)-c1cc(OC)c(-c3ccc(-c4ccc(-c5cccs5)c5nsnc54)s3)cc1[Si]2(C)C')
    pprint(rst)
    species = oi.get_entities(rst)
    # print(species)
    inchi = oi.convert_to_inchi(species)
    result = oi.run(inchi)
    print(result)
