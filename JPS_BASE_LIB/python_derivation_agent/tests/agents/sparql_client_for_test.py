from pyderivationagent.kg_operations import PySparqlClient
import pyderivationagent.data_model as dm
import uuid
import math

# Random number generation TBox
RANDOM_EXAMPLE_TBOX = 'http://www.example.com/ontology/random.owl#'
RANDOM_EXAMPLE_NUMOFPOINTS = RANDOM_EXAMPLE_TBOX + 'NumOfPoints'
RANDOM_EXAMPLE_UPPERLIMIT = RANDOM_EXAMPLE_TBOX + 'UpperLimit'
RANDOM_EXAMPLE_LOWERLIMIT = RANDOM_EXAMPLE_TBOX + 'LowerLimit'
RANDOM_EXAMPLE_POINT = RANDOM_EXAMPLE_TBOX + 'Point'
RANDOM_EXAMPLE_MINVALUE = RANDOM_EXAMPLE_TBOX + 'MinValue'
RANDOM_EXAMPLE_MAXVALUE = RANDOM_EXAMPLE_TBOX + 'MaxValue'
RANDOM_EXAMPLE_DIFFERENCE = RANDOM_EXAMPLE_TBOX + 'Difference'
RANDOM_EXAMPLE_DIFFERENCEREVERSE = RANDOM_EXAMPLE_TBOX + 'DifferenceReverse'
RANDOM_EXAMPLE_HASVALUE = RANDOM_EXAMPLE_TBOX + 'hasValue'
RANDOM_EXAMPLE_BASE_URL = 'https://www.example.com/triplestore/random/random_data_1/'
RANDOM_STRING_WITH_SPACES = 'Random string with spaces'
RANDOM_EXAMPLE_SPECIALVALUE = RANDOM_EXAMPLE_TBOX + 'specialValue'
RANDOM_EXAMPLE_INPUTPLACEHOLDEREXCEPTIONTHROW = RANDOM_EXAMPLE_TBOX + 'InputPlaceholderExceptionThrow'
RANDOM_EXAMPLE_OUTPUTPLACEHOLDEREXCEPTIONTHROW = RANDOM_EXAMPLE_TBOX + 'OutputPlaceholderExceptionThrow'
RANDOM_EXAMPLE_EXCEPTION_THROW_MSG = 'Throw an exception for ExceptionThrowAgent test.'

class PySparqlClientForTest(PySparqlClient):
    def pointHasAllSpecialValues(self, pt_iri):
        query = f"""SELECT ?value WHERE {{ <{pt_iri}> <{RANDOM_EXAMPLE_SPECIALVALUE}>  ?value. }}"""
        response = self.performQuery(query)
        if len(response) != 3:
            print(f"pointHasAllSpecialValues: {pt_iri} has {len(response)} special values: {response}, expected 3")
            return False

        special_values = [float(x['value']) for x in response]
        for val in [float('inf'), float('-inf')]:
            if val not in special_values:
                print(f"Special value {val} not found in {special_values} for point {pt_iri}")
                return False
        special_values.remove(float('inf'))
        special_values.remove(float('-inf'))
        if not math.isnan(special_values[0]):
            print(f"Special value NaN not found in {special_values} for point {pt_iri}")
            return False

        return True

    def getPointsInKG(self) -> dict:
        query = """SELECT ?pt ?val WHERE{ ?pt a <%s>. ?pt <%s> ?val.}""" % (
            RANDOM_EXAMPLE_POINT, RANDOM_EXAMPLE_HASVALUE)
        response = self.performQuery(query)

        if len(response) > 0:
            pt_dict = {pt['pt']: int(pt['val']) for pt in response}
            return pt_dict

        return {}

    def getPointsRdfsCommentInKG(self) -> dict:
        query = f"""{dm.PREFIX_RDFS} SELECT ?pt ?comment WHERE{{ ?pt a <{RANDOM_EXAMPLE_POINT}>. ?pt rdfs:comment ?comment.}}"""
        response = self.performQuery(query)

        if len(response) > 0:
            pt_dict = {pt['pt']: pt['comment'] for pt in response}
            return pt_dict

        return {}

    def getValue(self, iri):
        iri = dm.trimIRI(iri)
        query = """SELECT ?val \
                WHERE { <%s> <%s> ?val .}""" % (iri, RANDOM_EXAMPLE_HASVALUE)
        response = self.performQuery(query)

        return int(response[0]['val'])

    def getUpperLimit(self):
        query = dm.PREFIX_RDF + \
                """SELECT ?upperlimit \
                WHERE { ?upperlimit rdf:type <%s> .}""" % (RANDOM_EXAMPLE_UPPERLIMIT)
        response = self.performQuery(query)

        if len(response) > 1:
            raise Exception("There should only be one instance of RandomExample:UpperLimit, found multiple: " + response)
        elif len(response) == 1:
            return response[0]['upperlimit']

        return None

    def getLowerLimit(self):
        query = dm.PREFIX_RDF + \
                """SELECT ?lowerlimit \
                WHERE { ?lowerlimit rdf:type <%s> .}""" % (RANDOM_EXAMPLE_LOWERLIMIT)
        response = self.performQuery(query)

        if len(response) > 1:
            raise Exception("There should only be one instance of RandomExample:LowerLimit, found multiple: " + response)
        elif len(response) == 1:
            return response[0]['lowerlimit']

        return None

    def getNumOfPoints(self):
        query = dm.PREFIX_RDF + \
                """SELECT ?numofpoints \
                WHERE { ?numofpoints rdf:type <%s> .}""" % (RANDOM_EXAMPLE_NUMOFPOINTS)
        response = self.performQuery(query)

        if len(response) > 1:
            raise Exception("There should only be one instance of RandomExample:NumOfPoints, found multiple: " + response)
        elif len(response) == 1:
            return response[0]['numofpoints']

        return None

    def getExtremeValueInList(self, list_points_iri: list, max: bool) -> int:
        list_points_iri = dm.trimIRI(list_points_iri)
        query = f"""SELECT ?value
                WHERE {{
                    VALUES ?pt {{ <{'> <'.join(list_points_iri)}> }}
                    ?pt <{RANDOM_EXAMPLE_HASVALUE}> ?value.
                }} """
        query += "ORDER BY DESC(?value) " if max else "ORDER BY ?value "
        query += "LIMIT 1"

        response = self.performQuery(query)
        return int(response[0]['value'])

    def getMaxValueIRI(self):
        query = dm.PREFIX_RDF + \
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
        update = dm.PREFIX_RDF + """INSERT DATA {<%s> rdf:type <%s>. <%s> <%s> %s.}""" % (
            max_iri, RANDOM_EXAMPLE_MAXVALUE, max_iri, RANDOM_EXAMPLE_HASVALUE, value)

        self.performUpdate(update)
        return max_iri

    def getMinValueIRI(self):
        query = dm.PREFIX_RDF + \
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
        update = dm.PREFIX_RDF + """INSERT DATA {<%s> rdf:type <%s>. <%s> <%s> %s.}""" % (
            min_iri, RANDOM_EXAMPLE_MINVALUE, min_iri, RANDOM_EXAMPLE_HASVALUE, value)

        self.performUpdate(update)
        return min_iri

    def getDifferenceIRI(self):
        query = dm.PREFIX_RDF + \
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
        query = f"""{dm.PREFIX_RDF} SELECT ?diff_reverse
            WHERE {{ ?diff_reverse rdf:type <{RANDOM_EXAMPLE_DIFFERENCEREVERSE}> .}}"""
        response = self.performQuery(query)
        return [res['diff_reverse'] for res in response]

    def createDiffValue(self, value):
        diff_iri = RANDOM_EXAMPLE_BASE_URL + 'Difference_' + str(uuid.uuid4())
        update = dm.PREFIX_RDF + """INSERT DATA {<%s> rdf:type <%s>. <%s> <%s> %s.}""" % (
            diff_iri, RANDOM_EXAMPLE_DIFFERENCE, diff_iri, RANDOM_EXAMPLE_HASVALUE, value)

        self.performUpdate(update)
        return diff_iri

    def increaseNumOfPointsByOne(self):
        update = dm.PREFIX_RDF + \
            """DELETE {?numofpoints <%s> ?value .} INSERT {?numofpoints <%s> ?increased .} \
            WHERE { ?numofpoints rdf:type <%s> . ?numofpoints <%s> ?value . BIND (?value+1 AS ?increased)}""" % (
            RANDOM_EXAMPLE_HASVALUE, RANDOM_EXAMPLE_HASVALUE, RANDOM_EXAMPLE_NUMOFPOINTS, RANDOM_EXAMPLE_HASVALUE)
        self.performUpdate(update)

    def updateNumOfPointsTo(self, new_num_of_pts):
        update = dm.PREFIX_RDF + \
            """DELETE {?numofpoints <%s> ?value .} INSERT {?numofpoints <%s> %s .} \
            WHERE { ?numofpoints rdf:type <%s> . ?numofpoints <%s> ?value .}""" % (
            RANDOM_EXAMPLE_HASVALUE, RANDOM_EXAMPLE_HASVALUE, new_num_of_pts, RANDOM_EXAMPLE_NUMOFPOINTS, RANDOM_EXAMPLE_HASVALUE)
        self.performUpdate(update)

    def getDiffReverseValues(self):
        query = f"""{dm.PREFIX_RDF} SELECT ?diff_reverse ?value
                WHERE {{?diff_reverse rdf:type <{RANDOM_EXAMPLE_DIFFERENCEREVERSE}> .
                    ?diff_reverse <{RANDOM_EXAMPLE_HASVALUE}> ?value .}}"""
        response = self.performQuery(query)

        return {res['diff_reverse']:int(res['value']) for res in response}
