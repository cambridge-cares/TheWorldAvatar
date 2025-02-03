# This file to be executed from one directory upper:
# >python tests\unittest_zeolite.py

# This import and path guarantees correct operation between the sub-directories
import sys
#sys.path.append('../python')
sys.path.append('python')

import os
import unittest
from SPARQLWrapper import SPARQLWrapper, JSON

import json

import zeolist
# Older versions:
#from python import zeolist
#import python.zeolist

#SERVER = "laura"
SERVER = "local"

class TestKnowledgeGraph(unittest.TestCase):
    """ A class to test the actual abox uploaded to the blazegraph server.
    This test is not unit test strictly speaking. 
    It is closer to a 'system test' or at least an 'integration test',
    where integration means a combination of python+upload+sparql.
    Note: these tests do not cover Marie engine and web interface.
    """

    def setUp(self):
        """ The structure of each test:
        - Define a query template with placeholders
        - Create a list of placeholder values, possibly stored in an external file
        - Run a loop of the query template with each entry
          from the set of placeholders 
        - Return the errors if any.
        """
        # Set up the SPARQL endpoint

        if SERVER == "local":
            self.namespace = "zeo06h"

            self.address = "http://localhost:8080/blazegraph/namespace/" + self.namespace +"/sparql"
            self.USER, self.PASSWORD = None, None
        elif SERVER == "laura":
            self.namespace = "ontozeolite6"

            self.address = "https://theworldavatar.io/chemistry/blazegraph-dev/ui/namespace/" + self.namespace + "/sparql"  # < need authorization
            self.USER, self.PASSWORD = "bg_user", "admin"
        else:
            print("Unknown command line argument: '", sys.argv[1], "',",
                  " expect one of ['local', 'Laura']", sep="")
            self.USER, self.PASSWORD = None, None

        self.sparql = SPARQLWrapper(self.address)
        if self.USER is not None and self.PASSWORD is not None:
            self.sparql.setCredentials(self.USER, self.PASSWORD)

        file_path = os.path.join("tests", "test_cases_1.json")
        self.prepare_json(file_path, add_cases=1)

    # === end of setUp()

    def prepare_json(self, file_path, add_cases=0):
        """
        add_cases - number of additional cases to be added to the template.
        """

        # Read an existing json file:
        if not os.path.isfile(file_path):
            self.fail(f"Not found json file with test cases: '{file_path}'.")
        with open(file_path, encoding="utf-8") as fp:
            data = json.load(fp)

        names = list(data.keys())
        if len(names) == 0:
            self.fail(f"Missing data in json file: '{file_path}'.")

        # Create an empty template for a new test case:
        empty = {}
        name = names[0]
        for key in data[name].keys():
            if key == "query":
                empty[key] = [""]
            elif key == "draft":
                empty[key] = [""]
            elif key == "suites":
                empty[key] = {"s1": {"input": {"PLACE_HOLDER_1":""}, "output": {}, "comment": "" } }
            elif isinstance(data[name][key], list):
                empty[key] = []
            elif isinstance(data[name][key], dict):
                empty[key] = {}
            elif isinstance(data[name][key], str):
                empty[key] = ""
            else:
                self.fail(f"Unexpected data type in key: '{key}' " +
                          f"of type {type(data[name][key])}.")

        # Add templates for (several) new test case(s):
        for i in range(add_cases):
            data["empty" + str(i)] = empty

        # Save the new json (formatted and with template for a new test case):
        file_path = os.path.join("tests", "test_cases_1.json.bck")
        with open(file_path, "w", encoding="utf-8") as fp:
            json.dump(data, fp, indent=4)
      
    def load_json(self, file_path, name):
        if not os.path.isfile(file_path):
            self.fail("Not found json file with test cases: '" + file_path + "'")
        with open(file_path, encoding="utf-8") as fp:
            data = json.load(fp)

        #print(json.dumps(data, indent=4))
        if name not in data:
            self.fail("Missing test case '", name, "' in '" + file_path + "'")
        test_case = data[name]
        if not isinstance(test_case, dict):
            self.fail(f"Expect a test case of type dict, " +
                      f"but got '{type(test_case)}'.")
        return test_case
       
    def run_test_query(self, file_path, case_name, suite_case=None, comment=""):
        """ Default suite_case means run all available suite cases.
        """
        test_case = self.load_json(file_path, case_name)

        query_template = "\n".join(test_case["query"])

        #print(query_template)

        if suite_case is None:
            local_test_case = test_case["suites"]
        else:
            if suite_case in test_case["suites"]:
                local_test_case = {suite_case: test_case["suites"][suite_case]}
            else:
                self.fail(f"Missing suite case '{suite_case}'. {comment}")

        for suite_name, test_suite in local_test_case.items():
            #print(suite_name)
            msg = "in test case '" + case_name + "' in suite '" + suite_name + "':" 
            if "comment" in test_suite:
                msg += " " + test_suite["comment"]
            else:
                self.fail(f"Missing 'comment' in '{file_path}' " +
                          f"test '{case_name}' suite '{suite_name}'.")

            query = query_template
            for key, var in test_suite["input"].items():
                #var = test_suite["input"][key]
                query = query.replace(key, var)

            # Set the query
            self.sparql.setQuery(query)

            # Set the return format to JSON
            self.sparql.setReturnFormat(JSON)

            #print(query)

            # Execute the query and parse the results
            try:
                results = self.sparql.query().convert()
            except Exception as e:
                print("Invalid query:\n", query)
                raise e

            # Check if results are not empty
            self.assertEqual(len(results["results"]["bindings"]), 1, msg=msg)
            line = results["results"]["bindings"][0]
            #print("line =", line)
            for key, value in test_suite["output"].items():
                #print("to check", key, value )
                #self.assertEqual(len(results["results"]["bindings"]), value, msg=msg)
                if key in line:
                    self.assertEqual(line[key]["value"], value, msg=msg)
                else:
                    self.fail(f"Missing value for subject/predicate/object '{key}'")
                #print(query)
        pass
        # === end of run_test_query()


    def run_test_query_list(self, file_path, case_name, suite_case=None,
                            comment="", onlist=[]):
        """ Function checks completeness of the query result.
        The query should return a list, which should contain
        all items from onlist argument.
        Return the entire list of missing elements.
        """
        test_case = self.load_json(file_path, case_name)

        query_template = "\n".join(test_case["query"])

        #print(query_template)

        if suite_case is None:
            local_test_case = test_case["suites"]
        else:
            if suite_case in test_case["suites"]:
                local_test_case = {suite_case: test_case["suites"][suite_case]}
            else:
                self.fail(f"Missing suite case '{suite_case}'. {comment}")

        for suite_name, test_suite in local_test_case.items():
            #print(suite_name)
            msg = "in test case '" + case_name + "' in suite '" + suite_name + "':" 
            if "comment" in test_suite:
                msg += " " + test_suite["comment"]
            else:
                self.fail(f"Missing 'comment' in '{file_path}' " +
                          f"test '{case_name}' suite '{suite_name}'.", msg=msg)

            query = query_template
            for key, var in test_suite["input"].items():
                #var = test_suite["input"][key]
                query = query.replace(key, var)

            # Set the query
            self.sparql.setQuery(query)

            # Set the return format to JSON
            self.sparql.setReturnFormat(JSON)

            #print(query)

            # Execute the query and parse the results
            try:
                results = self.sparql.query().convert()
            except Exception as e:
                print("Invalid query:\n", query)
                raise e

            # Check if results are not empty
            #self.assertEqual(len(results["results"]["bindings"]), 1, msg=msg)
            #line = [0]
            #print("line =", line)

            if "list_element" in test_suite["output"]:
                list_element = test_suite["output"]["list_element"]
                query_list = []
                for line in results["results"]["bindings"]:
                    query_list.append(line[list_element]["value"])
                #print(query_list)

                missing = []
                for item in onlist:
                    if item not in query_list:
                        missing.append(item)
                self.assertEqual(len(missing), 0, msg="Missing in total " +
                                 str(len(missing)) + " values: "
                                 + str(missing) + comment + " " + test_case["comment"] +
                                 " " + test_suite["comment"]
                                 )
            else:
                self.fail(f"Function run_test_query_list() require: " +
                          f"'{file_path}' '{case_name}' '{suite_case}'")

            """
            for key, value in test_suite["output"].items():
                #print("to check", key, value )
                #self.assertEqual(len(results["results"]["bindings"]), value, msg=msg)
                if key in line:
                    self.assertEqual(line[key]["value"], value, msg=msg)
                else:
                    self.fail(f"Missing value for subject/predicate/object '{key}'")
                #print(query)
            """
        pass


    def test_uploaded_frameworks(self):
        """ 
        1) Check the total number of frameworks and their codes.
        2) Check that frameworks have the top level data structures:
           - crystal info,
           - few materials,
           - transformation,
           - citation(?)
        """
        case_name = "frameworks_count"
        file_path = os.path.join("tests", "test_cases_1.json")
        self.run_test_query(file_path, case_name)
        pass

    def test_uploaded_frameworks_total_count(self):
        case_name = "frameworks_count"
        file_path = os.path.join("tests", "test_cases_1.json")
        self.run_test_query(file_path, case_name, "s1")
        pass

    def test_uploaded_frameworks_cif_total_count(self):
        case_name = "frameworks_count"
        file_path = os.path.join("tests", "test_cases_1.json")
        self.run_test_query(file_path, case_name, "s2")
        pass

    def test_uploaded_frameworks_tiled_sructure_total_count(self):
        case_name = "frameworks_count"
        file_path = os.path.join("tests", "test_cases_1.json")
        self.run_test_query(file_path, case_name, "s3")
        pass

    def test_uploaded_frameworks_topological_total_count(self):
        case_name = "frameworks_count"
        file_path = os.path.join("tests", "test_cases_1.json")
        self.run_test_query(file_path, case_name, "s5")
        pass

    def test_uploaded_frameworks_check_all(self):
        zeo_list = zeolist.getZeoList(["main", "new"])
        case_name = "frameworks_all"
        file_path = os.path.join("tests", "test_cases_1.json")
        self.run_test_query_list(file_path, case_name, "s1", onlist=zeo_list)
        pass

    def test_uploaded_frameworks_cif_check_all(self):
        zeo_list = zeolist.getZeoList(["main", "new"])
        case_name = "frameworks_all"
        file_path = os.path.join("tests", "test_cases_1.json")
        self.run_test_query_list(file_path, case_name, "s2", onlist=zeo_list)
        pass

    def test_uploaded_frameworks_topology_check_all(self):
        zeo_list = zeolist.getZeoList(["main", "new"])
        case_name = "frameworks_all"
        file_path = os.path.join("tests", "test_cases_1.json")
        self.run_test_query_list(file_path, case_name, "s3", onlist=zeo_list)
        pass

    def test_uploaded_frameworks_tiledstruct_check_all(self):
        zeo_list = zeolist.getZeoList(["main", "new"])
        case_name = "frameworks_all"
        file_path = os.path.join("tests", "test_cases_1.json")
        self.run_test_query_list(file_path, case_name, "s4", onlist=zeo_list)
        pass

    def test_uploaded_frameworks_unitcell_check_all(self):
        zeo_list = zeolist.getZeoList(["main", "new"])
        case_name = "frameworks_all"
        file_path = os.path.join("tests", "test_cases_1.json")
        self.run_test_query_list(file_path, case_name, "s5", onlist=zeo_list)
        pass


    def test_uploaded_frameworks_cryst_atomic_iledstruct_check_all(self):
        zeo_list = zeolist.getZeoList(["main", "new"])
        case_name = "frameworks_all"
        file_path = os.path.join("tests", "test_cases_1.json")
        self.run_test_query_list(file_path, case_name, "s6", onlist=zeo_list)
        pass


    def test_uploaded_frameworks_cryst_transformation_check_all(self):
        zeo_list = zeolist.getZeoList(["main", "new"])
        case_name = "frameworks_all"
        file_path = os.path.join("tests", "test_cases_1.json")
        self.run_test_query_list(file_path, case_name, "s7", onlist=zeo_list)
        pass



    def test_uploaded_materials(self):
        pass

    def test_uploaded_crystals(self):
        pass

    def test_uploaded_bibliography(self):
        pass

    def atest_query_topology_sphere(self):
        # Example from file 'iza-sphere-diam.txt'
        QUERY = """
PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>
PREFIX ocr: <http://www.theworldavatar.com/kg/ontocrystal/>
PREFIX om:  <http://www.ontology-of-units-of-measure.org/resource/om-2/>

SELECT ?framecode ?sp_diam_a ?sp_diam_b ?sp_diam_c ?sp_diam_inc ?sp_diam_unit
WHERE {
  ?zeo        zeo:hasFrameworkCode            ?framecode .
  ?zeo        zeo:hasFrameworkCode            PLACE_HOLDER_1 .

  ?zeo        zeo:hasTopologicalProperties      ?topo .


  ?topo       zeo:hasSphereDiameter             ?sph_diam .
  ?sph_diam   ocr:hasVectorComponent            ?sph_diam_a, ?sph_diam_b, ?sph_diam_c, ?sph_diam_inc ;
               om:hasUnit                       ?sp_diam_u .

  OPTIONAL {
    ?sp_diam_u  rdfs:label                        ?sp_diam_unit .
  }

  ?sph_diam_a   ocr:hasComponentLabel            "a" ;
                ocr:hasComponentValue           ?sp_diam_a .
  ?sph_diam_b   ocr:hasComponentLabel            "b" ;
                ocr:hasComponentValue           ?sp_diam_b .
  ?sph_diam_c   ocr:hasComponentLabel            "c" ;
                ocr:hasComponentValue           ?sp_diam_c .
  ?sph_diam_inc ocr:hasComponentLabel            "included" ;
                ocr:hasComponentValue           ?sp_diam_inc .

}

ORDER BY (-xsd:decimal(xsd:string(?sp_diam_a)))
#ORDER BY (-xsd:decimal(xsd:string(?sp_diam_b)))
#ORDER BY (-xsd:decimal(xsd:string(?sp_diam_c)))
#ORDER BY (-xsd:decimal(xsd:string(?sp_diam_inc)))

#LIMIT 5
        """

        test_suite = {}
        test_suite['\"UOV\"'] = {"framecode": "UOV", "sp_diam_a": "6.44",
                                 "sp_diam_b": "5.38", "sp_diam_c": "5.03",
                                 "sp_diam_inc": "6.98", "sp_diam_unit": "angstrom"}

        test_suite['\"-CLO\"'] = {"framecode": "-CLO", "sp_diam_a": "6.31",
                                  "sp_diam_b": "6.31", "sp_diam_c": "6.31",
                                  "sp_diam_inc": "15.72", "sp_diam_unit": "angstrom"}

        test_suite['\"CHA\"'] = {"framecode": "CHA", "sp_diam_a": "3.72",
                                  "sp_diam_b": "3.72", "sp_diam_c": "3.72",
                                  "sp_diam_inc": "7.37", "sp_diam_unit": "angstrom"}


        for key, test_data in test_suite.items():
            query = QUERY.replace("PLACE_HOLDER_1", key)
            #print(key)

            # Set the query
            self.sparql.setQuery(query)
            #print(query)
            msg = "in " + test_data["framecode"]

            # Set the return format to JSON
            self.sparql.setReturnFormat(JSON)

            # Execute the query and parse the results
            results = self.sparql.query().convert()

            # Check if results are not empty
            self.assertEqual(len(results["results"]["bindings"]), 1, msg=msg)

            #print(results["results"]["bindings"])
            line = results["results"]["bindings"][0]
            self.assertEqual(line["framecode"]["value"], test_data["framecode"], msg=msg)
            self.assertEqual(line["sp_diam_a"]["value"], test_data["sp_diam_a"], msg=msg)
            self.assertEqual(line["sp_diam_b"]["value"], test_data["sp_diam_b"], msg=msg)
            self.assertEqual(line["sp_diam_c"]["value"], test_data["sp_diam_c"], msg=msg)
            self.assertEqual(line["sp_diam_inc"]["value"], test_data["sp_diam_inc"], msg=msg)
            if "sp_diam_unit" in line:
                self.assertEqual(line["sp_diam_unit"]["value"], test_data["sp_diam_unit"], msg=msg)

        # === end of test_query_topology_sphere()

    def atest_query_framework_unit_cell(self):
        # Example from file 'query-unitcell.txt'
        QUERY = """
# Query for Unit Cell Information (OntoCrystal, ver. 1)

PREFIX zeo:	<http://www.theworldavatar.com/kg/ontozeolite/>
PREFIX ocr:	<http://www.theworldavatar.com/kg/ontocrystal/>
PREFIX  om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
#PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

# To hide the uncertainty values:
SELECT ?framecode ?a ?b ?c ?alpha ?beta ?gamma ?volume ?lattice ?symmNum ?unit_length ?unit_angle ?unit_volume #?zeoFrame #?d_vol #?zeo

# To display the uncertainty values (if available):
#SELECT ?framecode ?a ?d_a ?b ?d_b ?c ?d_c ?alpha ?d_alpha ?beta ?d_beta ?gamma ?d_gamma ?volume ?d_vol ?lattice ?symmNum ?unit_length #?unit_angle #?unitlen ?d_vol #?zeo

WHERE {
  ?zeoFrame  zeo:hasFrameworkCode       ?framecode .
  ?zeoFrame  zeo:hasFrameworkCode       "AFN" .

  ?zeoFrame  ocr:hasCrystalInformation  ?cifdata .
  ?cifdata   ocr:hasUnitCell            ?unitcell .

  ?unitcell  ocr:hasUnitCellLengths     ?abc .
  ?abc        om:hasUnit                ?unitlen .
  OPTIONAL {
    ?unitlen  rdfs:label                  ?unit_length .
  }
  #?abc       ocr:hasVectorComponent     ?abc_a, ?abc_b, ?abc_c .

  ?unitcell   ocr:hasUnitCellAngles     ?abg .
  ?abg         om:hasUnit               ?unitangle .
  OPTIONAL {
    ?unitangle rdfs:label                 ?unit_angle .
  }
  #?abg        ocr:hasVectorComponent    ?abg_a, ?abg_b, ?abg_g .

  OPTIONAL{
    ?abc       ocr:hasVectorComponent   ?abc_a.
    ?abc_a     ocr:hasComponentLabel    "a";
               ocr:hasComponentValue    ?a .
  }
  OPTIONAL{
    ?abc       ocr:hasVectorComponent   ?abc_b.
    ?abc_b     ocr:hasComponentLabel    "b";
               ocr:hasComponentValue    ?b .
  }
  OPTIONAL{
    ?abc       ocr:hasVectorComponent   ?abc_c.
    ?abc_c     ocr:hasComponentLabel    "c";
               ocr:hasComponentValue    ?c .
  }

  OPTIONAL{
    ?abg       ocr:hasVectorComponent   ?abg_a .
    ?abg_a     ocr:hasComponentLabel    "alpha";
               ocr:hasComponentValue    ?alpha .
  }
  OPTIONAL{
    ?abg       ocr:hasVectorComponent   ?abg_b .
    ?abg_b     ocr:hasComponentLabel    "beta";
               ocr:hasComponentValue    ?beta .
  }
  OPTIONAL{
    ?abg       ocr:hasVectorComponent   ?abg_g .
    ?abg_g     ocr:hasComponentLabel    "gamma";
               ocr:hasComponentValue    ?gamma .
  }

  OPTIONAL {
    ?unitcell  ocr:hasUnitCellVolume      ?Volume .
    ?Volume    om:hasNumericalValue       ?volume ;
               om:hasUnit                 ?uVol .
    ?uVol    rdfs:label                   ?unit_volume .
  }

  OPTIONAL {
    ?abc_a   ocr:hasComponentUncertainty ?d_a .
  }
  OPTIONAL {
    ?abc_b   ocr:hasComponentUncertainty ?d_b .
  }
  OPTIONAL {
    ?abc_c   ocr:hasComponentUncertainty ?d_c .
  }

  OPTIONAL {
    ?abg_a   ocr:hasComponentUncertainty ?d_alpha .
  }
  OPTIONAL {
    ?abg_b   ocr:hasComponentUncertainty ?d_beta  .
  }
  OPTIONAL {
    ?abg_g   ocr:hasComponentUncertainty ?d_gamma .
  }

  OPTIONAL {
    ?Volume    ocr:hasUncertaintyValue   ?d_vol .
  }

  OPTIONAL {
    ?unitcell  ocr:hasLatticeSystem    ?lattice .
  }
  OPTIONAL {
    ?unitcell  ocr:hasSymmetryNumber   ?symmNum .
  }
  OPTIONAL {
    ?unitcell  ocr:hasSpaceGroupSymbol ?symmSymb .
  }

  #--- Examples of filters: ---#
  #FILTER ( ?volume  >  16000 && ?volume < 45000 )
  #FILTER ( ?volume  <  45000  )
  #FILTER ( xsd:decimal(?a) = 90 )
  #FILTER ( xsd:decimal(xsd:string(?alpha)) != 90 )

  #FILTER ( xsd:decimal(xsd:string(?a)) > 5 && xsd:decimal(xsd:string(?a)) < 20 )
  #FILTER ( xsd:decimal(xsd:string(?b)) > 5 && xsd:decimal(xsd:string(?b)) < 20 )
  #FILTER ( xsd:decimal(xsd:string(?c)) > 5 && xsd:decimal(xsd:string(?c)) < 20 )
  #FILTER ( xsd:decimal(xsd:string(?alpha)) > 60 && xsd:decimal(xsd:string(?alpha)) < 120 )
  #FILTER ( xsd:decimal(xsd:string(?beta )) > 60 && xsd:decimal(xsd:string(?beta )) < 120 )
  #FILTER ( xsd:decimal(xsd:string(?gamma)) > 60 && xsd:decimal(xsd:string(?gamma)) < 120 )
  #FILTER ( ?lattice = "orthorhombic" )
  #FILTER ( ?symmNum = 74 )
  }
#ORDER BY (-xsd:decimal(xsd:string(?volume)))
#LIMIT 10
        """

        test_suite = {}
        test_suite['\"AFN\"'] = {"framecode": "AFN", "a": "14.02",
                                 "b": "13.466", "c": "10.202", "alpha": "90.0",
                                 "beta": "107.239", "gamma": "90.0",
                                 "volume": "1839.5444", "lattice": "monoclinic",
                                 "symmNum": "12", "symmSymb": "C12/m1",
                                 "unit_length": "angstrom",
                                 "unit_angle": "degree",
                                 "unit_volume": "cubicAngstrom"
                                 }

        for key, test_data in test_suite.items():
            query = QUERY.replace("PLACE_HOLDER_1", key)
            #print(key)
            msg = "in " + test_data["framecode"]

            # Set the query
            self.sparql.setQuery(query)

            # Set the return format to JSON
            self.sparql.setReturnFormat(JSON)

            # Execute the query and parse the results
            results = self.sparql.query().convert()

            # Check if results are not empty
            self.assertEqual(len(results["results"]["bindings"]), 1)
            #print(results["results"]["bindings"])
            line = results["results"]["bindings"][0]
            self.assertEqual(line["framecode"]["value"], test_data["framecode"], msg=msg)
            self.assertEqual(line["a"]["value"], test_data["a"], msg=msg)
            self.assertEqual(line["b"]["value"], test_data["b"], msg=msg)
            self.assertEqual(line["c"]["value"], test_data["c"], msg=msg)
            self.assertEqual(line["alpha"]["value"], test_data["alpha"], msg=msg)
            self.assertEqual(line["beta"]["value"], test_data["beta"], msg=msg)
            self.assertEqual(line["gamma"]["value"], test_data["gamma"], msg=msg)
            self.assertEqual(line["volume"]["value"], test_data["volume"], msg=msg)
            self.assertEqual(line["lattice"]["value"], test_data["lattice"], msg=msg)
            self.assertEqual(line["symmNum"]["value"], test_data["symmNum"], msg=msg)
            if "symmSymb" in line:
                self.assertEqual(line["symmSymb"]["value"], test_data["symmSymb"], msg=msg)

            if "unit_length" in line:
                self.assertEqual(line["unit_length"]["value"], test_data["unit_length"], msg=msg)
            if "unit_angle" in line:
                self.assertEqual(line["unit_angle"]["value"], test_data["unit_angle"], msg=msg)
            if "unit_volume" in line:
                self.assertEqual(line["unit_volume"]["value"], test_data["unit_volume"], msg=msg)
        # === end of test_query_framework_unit_cell()

    def atest_query_material_unit_cell(self):
        # Example from file 'query-unitcell.txt'
        query = """
# Query for Unit Cell Information (OntoCrystal, ver. 1)
PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>
PREFIX ocr: <http://www.theworldavatar.com/kg/ontocrystal/>
PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT ?framecode  ?zeolite_name
       #?mat_unit_cell_volume_unit_iri
       #?mat_unit_cell_iri
       ?a  ?b  ?c  ?length_unit  ?alpha  ?beta  ?gamma  ?angle_unit
       ?mat_unit_cell_volume ?mat_unit_cell_volume_unit

WHERE {
  ?framework_iri  zeo:hasFrameworkCode            ?framecode .
  ?framework_iri  zeo:hasFrameworkCode            "CSV" .

  ?framework_iri  zeo:hasZeoliticMaterial         ?material_iri .

  ?material_iri      ocr:hasCrystalInformation     ?material_cif_iri .
  ?material_cif_iri  ocr:hasUnitCell               ?mat_unit_cell_iri .

  ?mat_unit_cell_iri ocr:hasUnitCellLengths        ?unit_cell_length_iri .
  ?unit_cell_length_iri ocr:hasVectorComponent     ?a_iri , ?b_iri , ?c_iri;
                        om:hasUnit                 ?length_unit_iri .
  OPTIONAL {
    ?length_unit_iri   rdfs:label                    ?length_unit .
  }
  ?a_iri             ocr:hasComponentLabel         "a" ;
                     ocr:hasComponentValue         ?a  .
  ?b_iri             ocr:hasComponentLabel         "b" ;
                     ocr:hasComponentValue         ?b  .
  ?c_iri             ocr:hasComponentLabel         "c" ;
                     ocr:hasComponentValue         ?c  .

  ?mat_unit_cell_iri ocr:hasUnitCellAngles        ?unit_cell_angles_iri .
  ?unit_cell_angles_iri ocr:hasVectorComponent     ?al_iri , ?be_iri , ?ga_iri;
                        om:hasUnit                 ?angle_unit_iri .
  OPTIONAL {
    ?angle_unit_iri   rdfs:label                    ?angle_unit .
  }

  ?al_iri             ocr:hasComponentLabel         "alpha" ;
                     ocr:hasComponentValue         ?alpha  .
  ?be_iri             ocr:hasComponentLabel         "beta" ;
                     ocr:hasComponentValue         ?beta  .
  ?ga_iri             ocr:hasComponentLabel         "gamma" ;
                     ocr:hasComponentValue         ?gamma  .

  ?mat_unit_cell_iri       ocr:hasUnitCellVolume   ?mat_unit_cell_volume_iri .
  ?mat_unit_cell_volume_iri om:hasNumericalValue   ?mat_unit_cell_volume .
  ?mat_unit_cell_volume_iri om:hasUnit             ?mat_unit_cell_volume_unit_iri .
  OPTIONAL {
    ?mat_unit_cell_volume_unit_iri rdfs:label        ?mat_unit_cell_volume_unit .
  }

  ?material_iri  os:name               ?zeolite_name .
}


        """

        # Set the query
        self.sparql.setQuery(query)

        # Set the return format to JSON
        self.sparql.setReturnFormat(JSON)

        # Execute the query and parse the results
        results = self.sparql.query().convert()

        # Check if results are not empty
        self.assertEqual(len(results["results"]["bindings"]), 1)
        #print(results["results"]["bindings"])
        line = results["results"]["bindings"][0]
        self.assertEqual(line["framecode"]["value"], "CSV")
        self.assertEqual(line["zeolite_name"]["value"], "CIT-7, calcined")
        self.assertEqual(line["a"]["value"], "13.0187")
        self.assertEqual(line["b"]["value"], "11.2063")
        self.assertEqual(line["c"]["value"], "9.3758")
        self.assertEqual(line["alpha"]["value"], "92.82")
        self.assertEqual(line["beta"]["value"], "107.2")
        self.assertEqual(line["gamma"]["value"], "103.26")
        self.assertEqual(line["mat_unit_cell_volume"]["value"], "1261.7262")
        #self.assertEqual(line["lattice"]["value"], "monoclinic")
        #self.assertEqual(line["symmNum"]["value"], "12")
        if "length_unit" in line:
            self.assertEqual(line["length_unit"]["value"], "angstrom")
        if "angle_unit" in line:
            self.assertEqual(line["angle_unit"]["value"], "degree")
        if "mat_unit_cell_volume_unit" in line:
            self.assertEqual(line["mat_unit_cell_volume_unit"]["value"], "cubicAngstrom")

    def atest_query_all_frameworks(self):
        # To confirm that there are 255 frameworks in the knowledge graph
        # Example from file ''
        query = """
PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>
PREFIX ocr: <http://www.theworldavatar.com/kg/ontocrystal/>
PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT ?framecode  #?zeolite_name
       #?mat_unit_cell_volume_unit_iri
       #?mat_unit_cell_iri
       #?a  ?b  ?c  ?length_unit  ?alpha  ?beta  ?gamma  ?angle_unit
       #?mat_unit_cell_volume ?mat_unit_cell_volume_unit

WHERE {
  ?framework_iri  zeo:hasFrameworkCode            ?framecode .

  #?material_iri  os:name               ?zeolite_name .
}

        """
        # Set the query
        self.sparql.setQuery(query)

        # Set the return format to JSON
        self.sparql.setReturnFormat(JSON)

        # Execute the query and parse the results
        results = self.sparql.query().convert()

        #print(results["results"]["bindings"])
        lines = results["results"]["bindings"]
        codes = []
        for line in lines:
            codes.append(line["framecode"]["value"])

        # All frameworks:
        zeo_list = zeolist.getZeoList(["main", "new"])
        self.assertEqual(len(zeo_list), 256)
        for zeo in zeo_list:
            if zeo not in codes:
                print("Missing framework", zeo, "in KG", self.address)
                pass

        step = 1
        file_id = 0
        for i_zeo in range(0, 256, step):
            #print(i_zeo)
            fr = i_zeo
            to = i_zeo + step
            if to > 256:
                to = 256
            err_count = 0
            for zeo in zeo_list[fr:to]:
                if zeo not in codes:
                    #print("missed aaaaa", zeo)
                    err_count += 1
                if err_count == to - fr:
                    print("Need to redo directory", file_id + 1, ":", i_zeo + step)
            file_id += 1

        # Check if results are not empty
        self.assertEqual(len(results["results"]["bindings"]), 256)
        print("Finished test all frameworks")


    def atest_query_x(self):
        # To confirm that there are 255 frameworks in the knowledge graph
        # Example from file ''
        query = """

        """
        # Set the query
        self.sparql.setQuery(query)

        # Set the return format to JSON
        self.sparql.setReturnFormat(JSON)

        # Execute the query and parse the results
        results = self.sparql.query().convert()

        # Check if results are not empty
        self.assertEqual(len(results["results"]["bindings"]), 255)
        #print(results["results"]["bindings"])
        line = results["results"]["bindings"][0]
        self.assertEqual(line["framecode"]["value"], "CSV")
        self.assertEqual(line["zeolite_name"]["value"], "CIT-7, calcined")

    def atest_query_xx(self):
        # To confirm that there are 255 frameworks in the knowledge graph
        # Example from file ''
        query = """

        """
        # Set the query
        self.sparql.setQuery(query)

        # Set the return format to JSON
        self.sparql.setReturnFormat(JSON)

        # Execute the query and parse the results
        results = self.sparql.query().convert()

        # Check if results are not empty
        self.assertEqual(len(results["results"]["bindings"]), 255)
        #print(results["results"]["bindings"])
        line = results["results"]["bindings"][0]
        self.assertEqual(line["framecode"]["value"], "CSV")
        self.assertEqual(line["zeolite_name"]["value"], "CIT-7, calcined")

if __name__ == '__main__':
    #aaa = """ aaa => x PLACE_HOLDER_1 x """
    #aaa = aaa.replace("PLACE_HOLDER_1", '\"UOV\"')
    #print(aaa)

    unittest.main()


