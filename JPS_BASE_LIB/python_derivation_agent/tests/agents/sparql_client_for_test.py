from pyderivationagent.kg_operations import *
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
RANDOM_EXAMPLE_HASVALUE = 'http://www.example.com/ontology/random.owl#hasValue'
RANDOM_EXAMPLE_HASPOINT = 'http://www.example.com/ontology/random.owl#hasPoint'
RANDOM_EXAMPLE_BASE_URL = 'https://www.example.com/triplestore/random/random_data_1/'

class PySparqlClientForTest(PySparqlClient):
    def getListOfPoints(self):
        query = PREFIX_RDF + \
                """SELECT ?listofpoints \
                WHERE { ?listofpoints rdf:type <%s> . }""" % (RANDOM_EXAMPLE_LISTOFPOINTS)
        response = self.performQuery(query)

        if len(response) > 1:
            raise Exception("There should only be one instance of RandomExample:ListOfPoints, found multiple: " + response)
        else:
            return response[0]['listofpoints']

    def createListOfPoints(self, points):
        listofpoints_iri = RANDOM_EXAMPLE_BASE_URL + 'ListOfPoints_' + str(uuid.uuid4())
        update = PREFIX_RDF + \
                """INSERT DATA { <%s> rdf:type <%s> . """ % (listofpoints_iri, RANDOM_EXAMPLE_LISTOFPOINTS)
        for pt in points:
            pt_iri = RANDOM_EXAMPLE_BASE_URL + 'Point_' + str(uuid.uuid4())
            update += "<%s> <%s> <%s> . <%s> <%s> %s ." % (listofpoints_iri, RANDOM_EXAMPLE_HASPOINT, pt_iri, pt_iri, RANDOM_EXAMPLE_HASVALUE, pt)
        update += """}"""
        self.performUpdate(update)
        return listofpoints_iri

    def getPointsInList(self, listofpoints_iri: str) -> dict:
        listofpoints_iri = trimIRI(listofpoints_iri)
        query = """SELECT ?pt ?val \
                WHERE { <%s> <%s> ?pt . \
                ?pt <%s> ?val .}""" % (listofpoints_iri, RANDOM_EXAMPLE_HASPOINT, RANDOM_EXAMPLE_HASVALUE)
        response = self.performQuery(query)

        pt_dict = { pt['pt'] : int(pt['val']) for pt in response }

        return pt_dict

    def getPointsInKG(self) -> dict:
        query = """SELECT ?pt ?val WHERE{ ?pt a <%s>. ?pt <%s> ?val.}""" % (
            RANDOM_EXAMPLE_POINT, RANDOM_EXAMPLE_HASVALUE)
        response = self.performQuery(query)

        pt_dict = {pt['pt']: int(pt['val']) for pt in response}

        return pt_dict

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
        else:
            return response[0]['upperlimit']

    def getLowerLimit(self):
        query = PREFIX_RDF + \
                """SELECT ?lowerlimit \
                WHERE { ?lowerlimit rdf:type <%s> .}""" % (RANDOM_EXAMPLE_LOWERLIMIT)
        response = self.performQuery(query)

        if len(response) > 1:
            raise Exception("There should only be one instance of RandomExample:LowerLimit, found multiple: " + response)
        else:
            return response[0]['lowerlimit']

    def getNumOfPoints(self):
        query = PREFIX_RDF + \
                """SELECT ?numofpoints \
                WHERE { ?numofpoints rdf:type <%s> .}""" % (RANDOM_EXAMPLE_NUMOFPOINTS)
        response = self.performQuery(query)

        if len(response) > 1:
            raise Exception("There should only be one instance of RandomExample:NumOfPoints, found multiple: " + response)
        else:
            return response[0]['numofpoints']

    def getExtremeValueInList(self, listofpoints_iri: str, max: bool) -> int:
        listofpoints_iri = trimIRI(listofpoints_iri)
        query = """SELECT ?value WHERE { <%s> <%s>/<%s> ?value. } """ % (
            listofpoints_iri, RANDOM_EXAMPLE_HASPOINT, RANDOM_EXAMPLE_HASVALUE)
        query += "ORDER BY DESC(?value) " if max else "ORDER BY ?value "
        query += "LIMIT 1"
        print(query)

        response = self.performQuery(query)
        return response[0]['value']

    def getMaxValueIRI(self):
        query = PREFIX_RDF + \
            """SELECT ?max WHERE { ?max rdf:type <%s> .}""" % (
                RANDOM_EXAMPLE_MAXVALUE)
        response = self.performQuery(query)

        if len(response) > 1:
            raise Exception(
                "There should only be one instance of RandomExample:MaxValue, found multiple: " + response)
        else:
            return response[0]['max']

    def getMinValueIRI(self):
        query = PREFIX_RDF + \
            """SELECT ?min WHERE { ?min rdf:type <%s> .}""" % (
                RANDOM_EXAMPLE_MINVALUE)
        response = self.performQuery(query)

        if len(response) > 1:
            raise Exception(
                "There should only be one instance of RandomExample:MinValue, found multiple: " + response)
        else:
            return response[0]['min']

    def getDifferenceIRI(self):
        query = PREFIX_RDF + \
            """SELECT ?diff WHERE { ?diff rdf:type <%s> .}""" % (
                RANDOM_EXAMPLE_DIFFERENCE)
        response = self.performQuery(query)

        if len(response) > 1:
            raise Exception(
                "There should only be one instance of RandomExample:Difference, found multiple: " + response)
        else:
            return response[0]['diff']

    def increaseNumOfPointsByOne(self):
        update = PREFIX_RDF + \
            """DELETE {?numofpoints <%s> ?value .} INSERT {?numofpoints <%s> ?increased .} \
            WHERE { ?numofpoints rdf:type <%s> . ?numofpoints <%s> ?value . BIND (?value+1 AS ?increased)}""" % (
            RANDOM_EXAMPLE_HASVALUE, RANDOM_EXAMPLE_HASVALUE, RANDOM_EXAMPLE_NUMOFPOINTS, RANDOM_EXAMPLE_HASVALUE)
        self.performUpdate(update)
