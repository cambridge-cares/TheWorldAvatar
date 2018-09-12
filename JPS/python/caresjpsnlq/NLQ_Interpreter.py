from NLP_Toolbox import toolbox
from NLQ_PatternProcessor import PatternProcessor


class Interpreter:
    # We will skip the skeleton stage for NLQ, since in our case, before the skeleton is formed, lookup actions are already taken.

    """Interpreter will take the fully processed tree and give semantic interpretation for the tree
    and return a skeleton of a SPARQL query e.g. []"""

    #  ________________________________________________________________________________
    #  ||| What is the number of power plants versus the gdp total of all countries |||
    #  ________________________________________________________________________________
    #
    # PREFIX: rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>
    #
    # SELECT DISTINCT ?country ?gdpTotal ?numberOfPowerPlants WHERE{
    #   SERVICE <http://dbpedia.org/sparql/>
    #       {
    #           ?country rdf:type <http://dbpedia.org/ontology/country>
    #           ?country <http://dbpedia.org/property/gdpPpp> ?gdpTotal .
    #
    #       }
    #
    #   SELECT DISTINCT ?country (COUNT(?powerPlants) as ?numberOfPowerPlants)
    #       {
    #           ?powerPlants rdf:type <http://www.theworldavatar.com/OntoEIP/OntoEN/power_plant.owl#PowerPlant> .
    #           ?powerPlants <http://dbpeida.org/ontology/country> ?country .
    #       } GROUP BY ?country
    # }
    # General strategy as following:
    #   1.  Country as a sub is for sure. Due to the 'all' component, which looks like NP-all FUNC_ALL. it indicates it is a type. Therefore, stem it first,
    #       then apply method 'get class uri'. Such method searches both jps and dbpedia. For most of the time, only one knowledge base contain the instance/class
    #   2.  If the uri is from dbpedia, flag. If SERVICE indicated in the future, put ?country type <country> in SERVICE
    #   3.  - GDP and number of power plants. First go to #JPS# to search such term. If there is no result, presume it is a property within dbpedia.
    #   4.  After that, gdp is recognized as property in dbpedia, power plants is recognized as class in jps
    #   5.
    ####################################################################################################################
    #
    #  ________________________________________________________________________________
    #  ||| What are the values of all sensors in public area in cares lab |||
    #  ________________________________________________________________________________
    #   SELECT DISTINCT * WHERE {
    #       <CARES_LAB>     ?random     <public area> .
    #       ?sensors        type        <sensors> .
    #       <public area>   ?random2    ?sensors .
    #       ?sensors        hasValue    ?value
    #   }
    #
    # ______________________________________________________________________________________________
    # ||| What is the sum of designed capacity of all power plants versus the gdp of all countries
    # ______________________________________________________________________________________________
    #
    #  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    #    SELECT DISTINCT ?country (SUM(?nv) as ?tt) (COUNT(?plant) as ?npp)  WHERE {
    #       ?plant rdf:type <http://www.theworldavatar.com/OntoEIP/OntoEN/power_plant.owl#PowerGenerator> .
    #       ?plant <http://dbpedia.org/ontology/country> ?country .
    #       ?plant <http://www.theworldavatar.com/OntoEIP/system_aspects/system_realization.owl#designCapacity> ?capacity .
    #       ?capacity <http://www.theworldavatar.com/OntoEIP/OntoCAPE/OntoCAPE/upper_level/system.owl#hasValue> ?value .
    #        ?value <http://www.theworldavatar.com/OntoEIP/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue> ?nv
    #   }  GROUP BY ?country ORDER BY DESC(?tt)

    def __init__(self):

        self.PatternProcessor = PatternProcessor()
        self.main_tree = None

    def get_class_uri(self):
        """Take the term and go to both dbpedia and ols to find the class uri, triggered when FUNC_ALL is indicated """

        pass

    def main_tree_navigator(self, main_tree):
        """The main function for Interpreter, it navigate through the tree and do things to form the final sparql query"""
        # 1. To recognize the number of labels
        self.main_tree = main_tree

        level_1_labels = [sub_tree.label() for sub_tree in main_tree if toolbox.is_tree(sub_tree)]
        print('Level 1 labels', level_1_labels)
        if 'QI' in level_1_labels:
            level_1_labels.remove('QI')

        # Now we know the number and name of all the labels the tree contains. It will be simple if there is only one label. (it is usually the case)
        if len(level_1_labels):
            # It contains only one level on label, pass it to the simple_level_1_classifier to parse it accordingly
            return self.simple_level_1_classifier(level_1_labels[0])





    def simple_level_1_classifier(self, label):
        """This function is triggered if labels contains only one label, indicating it is a simple case"""
        return self.PatternProcessor.function_map[label](self.main_tree)


    def check_term_type_from_ols(self):
        """Identify the returned uri is a class, instance or property"""
        pass




