from pyderivationagent.kg_operations import PySparqlClient
from pyderivationagent.kg_operations import trimIRI
from pyderivationagent.kg_operations import PREFIX_RDF
from pyderivationagent.kg_operations import PREFIX_RDFS
import uuid

# Random number generation TBox
RANDOM_EXAMPLE_NUMOFPOINTS = 'http://www.example.com/ontology/random.owl#NumOfPoints'
RANDOM_EXAMPLE_UPPERLIMIT = 'http://www.example.com/ontology/random.owl#UpperLimit'
RANDOM_EXAMPLE_LOWERLIMIT = 'http://www.example.com/ontology/random.owl#LowerLimit'
RANDOM_EXAMPLE_POINT = 'http://www.example.com/ontology/random.owl#Point'
RANDOM_EXAMPLE_LISTOFPOINTS = 'http://www.example.com/ontology/random.owl#ListOfPoints'
RANDOM_EXAMPLE_MINVALUE = 'http://www.example.com/ontology/random.owl#MinValue'
RANDOM_EXAMPLE_MAXVALUE = 'http://www.example.com/ontology/random.owl#MaxValue'
RANDOM_EXAMPLE_DIFFERENCE = 'http://www.example.com/ontology/random.owl#Difference'
RANDOM_EXAMPLE_DIFFERENCEREVERSE = 'http://www.example.com/ontology/random.owl#DifferenceReverse'
RANDOM_EXAMPLE_HASVALUE = 'http://www.example.com/ontology/random.owl#hasValue'
RANDOM_EXAMPLE_HASPOINT = 'http://www.example.com/ontology/random.owl#hasPoint'
RANDOM_EXAMPLE_BASE_URL = 'https://www.example.com/triplestore/random/random_data_1/'
RANDOM_STRING_WITH_SPACES = 'Random string with spaces'

class PySparqlClientForTest(PySparqlClient):
    def getListOfPoints(self):
        query = PREFIX_RDF + \
                """SELECT ?listofpoints \
                WHERE { ?listofpoints rdf:type <%s> . }""" % (RANDOM_EXAMPLE_LISTOFPOINTS)
        response = self.performQuery(query)

        if len(response) > 1:
            raise Exception("There should only be one instance of RandomExample:ListOfPoints, found multiple: " + response)
        elif len(response) == 1:
            return response[0]['listofpoints']

        return None

    def createListOfPoints(self, points):
        listofpoints_iri = RANDOM_EXAMPLE_BASE_URL + 'ListOfPoints_' + str(uuid.uuid4())
        pt_iris = []
        update = PREFIX_RDF + \
                """INSERT DATA { <%s> rdf:type <%s> . """ % (listofpoints_iri, RANDOM_EXAMPLE_LISTOFPOINTS)
        for pt in points:
            pt_iri = RANDOM_EXAMPLE_BASE_URL + 'Point_' + str(uuid.uuid4())
            pt_iris.append(pt_iri)
            update += "<%s> <%s> <%s> . <%s> rdf:type <%s>; <%s> %s ." % (
                listofpoints_iri, RANDOM_EXAMPLE_HASPOINT, pt_iri, pt_iri, RANDOM_EXAMPLE_POINT, RANDOM_EXAMPLE_HASVALUE, pt)
        update += """}"""
        self.performUpdate(update)
        return listofpoints_iri, pt_iris

    def getPointsInList(self, listofpoints_iri: str) -> dict:
        listofpoints_iri = trimIRI(listofpoints_iri)
        query = """SELECT ?pt ?val \
                WHERE { <%s> <%s> ?pt . \
                ?pt <%s> ?val .}""" % (listofpoints_iri, RANDOM_EXAMPLE_HASPOINT, RANDOM_EXAMPLE_HASVALUE)
        response = self.performQuery(query)

        if len(response) > 0:
            pt_dict = { pt['pt'] : int(pt['val']) for pt in response }
            return pt_dict

        return None

    def getPointsInKG(self) -> dict:
        query = """SELECT ?pt ?val WHERE{ ?pt a <%s>. ?pt <%s> ?val.}""" % (
            RANDOM_EXAMPLE_POINT, RANDOM_EXAMPLE_HASVALUE)
        response = self.performQuery(query)

        if len(response) > 0:
            pt_dict = {pt['pt']: int(pt['val']) for pt in response}
            return pt_dict

        return None

    def getPointsRdfsCommentInKG(self) -> dict:
        query = f"""{PREFIX_RDFS} SELECT ?pt ?comment WHERE{{ ?pt a <{RANDOM_EXAMPLE_POINT}>. ?pt rdfs:comment ?comment.}}"""
        response = self.performQuery(query)

        if len(response) > 0:
            pt_dict = {pt['pt']: pt['comment'] for pt in response}
            return pt_dict

        return None

    def getValue(self, iri):
        iri = trimIRI(iri)
        query = """SELECT ?val \
                WHERE { <%s> <%s> ?val .}""" % (iri, RANDOM_EXAMPLE_HASVALUE)
        response = self.performQuery(query)

        return int(response[0]['val'])

    def getUpperLimit(self):
        query = PREFIX_RDF + \
                """SELECT ?upperlimit \
                WHERE { ?upperlimit rdf:type <%s> .}""" % (RANDOM_EXAMPLE_UPPERLIMIT)
        response = self.performQuery(query)

        if len(response) > 1:
            raise Exception("There should only be one instance of RandomExample:UpperLimit, found multiple: " + response)
        elif len(response) == 1:
            return response[0]['upperlimit']

        return None

    def getLowerLimit(self):
        query = PREFIX_RDF + \
                """SELECT ?lowerlimit \
                WHERE { ?lowerlimit rdf:type <%s> .}""" % (RANDOM_EXAMPLE_LOWERLIMIT)
        response = self.performQuery(query)

        if len(response) > 1:
            raise Exception("There should only be one instance of RandomExample:LowerLimit, found multiple: " + response)
        elif len(response) == 1:
            return response[0]['lowerlimit']

        return None

    def getNumOfPoints(self):
        query = PREFIX_RDF + \
                """SELECT ?numofpoints \
                WHERE { ?numofpoints rdf:type <%s> .}""" % (RANDOM_EXAMPLE_NUMOFPOINTS)
        response = self.performQuery(query)

        if len(response) > 1:
            raise Exception("There should only be one instance of RandomExample:NumOfPoints, found multiple: " + response)
        elif len(response) == 1:
            return response[0]['numofpoints']

        return None

    def getExtremeValueInList(self, listofpoints_iri: str, max: bool) -> int:
        listofpoints_iri = trimIRI(listofpoints_iri)
        query = """SELECT ?value WHERE { <%s> <%s>/<%s> ?value. } """ % (
            listofpoints_iri, RANDOM_EXAMPLE_HASPOINT, RANDOM_EXAMPLE_HASVALUE)
        query += "ORDER BY DESC(?value) " if max else "ORDER BY ?value "
        query += "LIMIT 1"

        response = self.performQuery(query)
        return int(response[0]['value'])

    def getMaxValueIRI(self):
        query = PREFIX_RDF + \
            """SELECT ?max WHERE { ?max rdf:type <%s> .}""" % (
                RANDOM_EXAMPLE_MAXVALUE)
        response = self.performQuery(query)

        if len(response) > 1:
            raise Exception(
                "There should only be one instance of RandomExample:MaxValue, found multiple: " + response)
        elif len(response) == 1:
            return response[0]['max']

        return None

    def createMaxValue(self, value):
        max_iri = RANDOM_EXAMPLE_BASE_URL + 'MaxValue_' + str(uuid.uuid4())
        update = PREFIX_RDF + """INSERT DATA {<%s> rdf:type <%s>. <%s> <%s> %s.}""" % (
            max_iri, RANDOM_EXAMPLE_MAXVALUE, max_iri, RANDOM_EXAMPLE_HASVALUE, value)

        self.performUpdate(update)
        return max_iri

    def getMinValueIRI(self):
        query = PREFIX_RDF + \
            """SELECT ?min WHERE { ?min rdf:type <%s> .}""" % (
                RANDOM_EXAMPLE_MINVALUE)
        response = self.performQuery(query)

        if len(response) > 1:
            raise Exception(
                "There should only be one instance of RandomExample:MinValue, found multiple: " + response)
        elif len(response) == 1:
            return response[0]['min']

        return None

    def createMinValue(self, value):
        min_iri = RANDOM_EXAMPLE_BASE_URL + 'MinValue_' + str(uuid.uuid4())
        update = PREFIX_RDF + """INSERT DATA {<%s> rdf:type <%s>. <%s> <%s> %s.}""" % (
            min_iri, RANDOM_EXAMPLE_MINVALUE, min_iri, RANDOM_EXAMPLE_HASVALUE, value)

        self.performUpdate(update)
        return min_iri

    def getDifferenceIRI(self):
        query = PREFIX_RDF + \
            """SELECT ?diff WHERE { ?diff rdf:type <%s> .}""" % (
                RANDOM_EXAMPLE_DIFFERENCE)
        response = self.performQuery(query)

        if len(response) > 1:
            raise Exception(
                "There should only be one instance of RandomExample:Difference, found multiple: " + response)
        elif len(response) == 1:
            return response[0]['diff']

        return None

    def getDiffReverseIRI(self):
        query = f"""{PREFIX_RDF} SELECT ?diff_reverse
            WHERE {{ ?diff_reverse rdf:type <{RANDOM_EXAMPLE_DIFFERENCEREVERSE}> .}}"""
        response = self.performQuery(query)
        return [res['diff_reverse'] for res in response]

    def createDiffValue(self, value):
        diff_iri = RANDOM_EXAMPLE_BASE_URL + 'Difference_' + str(uuid.uuid4())
        update = PREFIX_RDF + """INSERT DATA {<%s> rdf:type <%s>. <%s> <%s> %s.}""" % (
            diff_iri, RANDOM_EXAMPLE_DIFFERENCE, diff_iri, RANDOM_EXAMPLE_HASVALUE, value)

        self.performUpdate(update)
        return diff_iri

    def increaseNumOfPointsByOne(self):
        update = PREFIX_RDF + \
            """DELETE {?numofpoints <%s> ?value .} INSERT {?numofpoints <%s> ?increased .} \
            WHERE { ?numofpoints rdf:type <%s> . ?numofpoints <%s> ?value . BIND (?value+1 AS ?increased)}""" % (
            RANDOM_EXAMPLE_HASVALUE, RANDOM_EXAMPLE_HASVALUE, RANDOM_EXAMPLE_NUMOFPOINTS, RANDOM_EXAMPLE_HASVALUE)
        self.performUpdate(update)

    def getDiffReverseValues(self):
        query = f"""{PREFIX_RDF} SELECT ?diff_reverse ?value 
                WHERE {{?diff_reverse rdf:type <{RANDOM_EXAMPLE_DIFFERENCEREVERSE}> .
                    ?diff_reverse <{RANDOM_EXAMPLE_HASVALUE}> ?value .}}"""
        response = self.performQuery(query)

        return {res['diff_reverse']:int(res['value']) for res in response}
