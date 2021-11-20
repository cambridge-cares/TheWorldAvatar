from owlready2 import get_ontology
import rdflib

from ontomatch.ontologyWrapper import Ontology
import tests.utils_for_testing

class TestOntology(tests.utils_for_testing.TestCaseOntoMatch):

    def test_valuemap(self):

        addr = './tests/data/KWL_20_power_plants.owl'
        onto_owlready2 = get_ontology(addr).load()
        graph = rdflib.Graph()
        graph.parse(addr)
        onto = Ontology(addr, ontology=onto_owlready2, graph=graph)

        # search for entity with (unique) postalCode 6425
        code = None
        locality = None
        year = None
        owner = None
        for _, propvalues in onto.valueMap.items():
            #print(entity_pos, propvalues)
            for propchain, value in propvalues:
                if propchain[0] == 'address' and len(propchain) > 1:
                    if propchain[1] == 'postalCode':
                        code = value.toPython()
                    if propchain[1] == 'addressLocality':
                        locality = value.toPython()
                elif propchain[0] == 'hasYearOfBuilt':
                    year = value.toPython()
                elif propchain[0] == 'isOwnedBy':
                    owner = value.toPython()

            if code == 6425:
                break

        # assert property values for the found entity
        self.assertEqual(code, 6425)
        self.assertEqual(locality, 'Alsleben (Saale)')
        self.assertEqual(year, 2016)
        self.assertEqual(owner, 'Dortmunder Energie- und Wasserversorgung GmbH')
