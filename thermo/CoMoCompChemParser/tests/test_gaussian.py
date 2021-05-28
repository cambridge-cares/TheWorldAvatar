import unittest
from compchemparser.parsers.ccgaussian_parser import CcGaussianParser
import json
import os
from tkinter.tix import DialogShell
from parameterized import parameterized

#Added by Nenad Krdzavac
from compchemparser.ontocompchemdata.ontocompchemdata import OntoCompChemData
from rdflib import URIRef, BNode, Literal, Graph, Namespace
from rdflib.namespace import RDF, RDFS, OWL, XSD
from pathlib import Path
import random
import uuid
import os,sys
import decimal
import rdflib
import pprint


THIS_DIR = os.path.dirname(os.path.abspath(__file__))
parser = CcGaussianParser()

# create OntoCompChemData object
CompChemObj = OntoCompChemData()

gaussianTestsLog = os.path.join(THIS_DIR, 'gaussianTests.log')

def getRefName(testLog,jobIndex,numJobs):
    if numJobs > 1:
        refName = testLog + '_' + str(jobIndex+1)+'.json'
    else:
        refName = testLog + '.json'
    return refName

class TestGaussianParser(unittest.TestCase):
    @parameterized.expand([
        #composite
        # type, log, regenerate results flag
        # e.g:
        #['g1','co2_g1_g09.log', True],  <- True here sets the regenerateResults flag to True
        # no need to set it to False as that is the default
        ['g1','co2_g1_g09.log'],
        ['g2','co2_g2_g09.log'],
        ['g2','co2_g2mp2_g09.log'],
        ['cbs-4m','co2_cbs4m_g09.log'],
        ['cbs-apno','co2_cbsapno_g09.log'],
        #linked
        ['linked','co2_linked_g09.log'],
        #cas
        ['cas','co2_cas_g09.log'],
        ['cas','co2_casmp2.log'],
        #dft
        ['dft','co2_freq_g09.log'],
        ['dft','co2_sp_dft_g09.log'],
        ['dft','co2_opt_freq_dft_p_g09.log'],
        ['dft','co2_opt_freq_dft_t_g09.log'],
        ['dft','h_sp_n_g09.log'],
        #ccsd/ci
        ['ccsd','co2_ccsd_g09.log'],
        ['ccsd','co2_qci_g09.log'],
        ['ci','co2_cis.log'],
        #mpn
        ['mpn','oh_sp_mp4_g03.log'],
        #extra
        ['extra','Cl.g09'],
        ['extra','Ti.g09'],
        ['extra','TiCl4.g09'],
        ['extra','O_3let.g09'],
        ['extra','71-41-0.g09'],
        ['extra','71-43-2.g09'],
        ['extra','A2R5H.log'],
        ['extra','1b.log'],
        #failed
        ['failed_jobs','c2h4_opt_dft_g09_failed.log'],
        ['failed_jobs','c2h4_opt_dft_g09_powercut.log'],
        ['failed_jobs','co2_linked_g09.log'],
        #scans
        #['scans','ethane_scan_short.log']
    ])
    def test_parser(self, testType, testLog, regenerateResults=False):
        print('========================================================')
        print('TEST TYPE: ', testType)
        print('TEST LOG: ', testLog)
        print()
        print()
        testPath = os.path.join(THIS_DIR, 'gaussian', testType)
        testLogFilePath = os.path.join(testPath, testLog)
        parsedJobsList = parser.parse(testLogFilePath)

        for jobIndex, jobDataJson in enumerate(parsedJobsList):
            #This converts test_data into a dictionary from JSON
            #For easy comparison and readability
            jobDataDict = json.loads(jobDataJson)

            refDataFileName = getRefName(testLog,jobIndex=jobIndex,numJobs=len(parsedJobsList))
            refDataFilePath = os.path.join(testPath, refDataFileName)

            if regenerateResults:
                # dump just parsed data as the ref data
                with open(refDataFilePath, 'w') as outFile:
                        json.dump(jobDataDict, outFile, indent = 4)

            # read the red data
            with open(refDataFilePath) as refFile:
                refDataDict = json.load(refFile)

            self.assertEqual(len(jobDataDict.keys()), len(refDataDict.keys()))
            for key in jobDataDict.keys():
                self.assertEqual(key in refDataDict.keys(), True)
                self.assertEqual(jobDataDict[key], refDataDict[key])
        print('========================================================')
        print()
        print()



#    def test_suite1(self):
#
#        test_path = os.path.join(THIS_DIR, 'gaussian')
#
#        print("test_path: ", test_path)
#
#        for method, logs in Test_suite1.items():
#            for log_file in logs:
#                test_name = 'Gaussian '+ method
#                file_path = os.path.join(test_path, method, log_file)
#
#                print("Test " + test_name+", File: "+ log_file)
#
#                test_data_list = parser.parse(file_path)
#
#                for i, test_data in enumerate(test_data_list):
#                    #This converts test_data into a dictionary from JSON
#                    #For easy comparison and readability
#                    test_data = json.loads(test_data)
#
#                    if len(test_data_list) > 1:
#                    #   ref_name = log_file.replace('.log','#'+str(i+1)+'.json')
#                         ref_name = log_file + '_' + str(i+1)+'.json'
#                         #print("ref_name (json): ", ref_name)
#
#                    else:
#                        #ref_name = log_file.replace('.log','.json')
#                        ref_name = log_file + '.json'
#                        #print("ref_name (json): ", ref_name)
#
#                    ref_path = os.path.join(test_path, method, ref_name)
#
#                    # uncomment to generate ref json file
#                    #---------------------------------------------------
#                    with open(ref_path, 'w') as outfile:
#                         json.dump(test_data, outfile, indent = 4)
#                    #---------------------------------------------------
#
#                    print("ref_path: ", ref_path)
#
#                    with open(ref_path) as ref_file:
#                        ref_data = json.load(ref_file)
#
#
#                    self.assertEqual(len(test_data.keys()), len(ref_data.keys()))
#                    for key in test_data.keys():
#                        self.assertEqual(key in ref_data.keys(), True)
#                        self.assertEqual(test_data[key], ref_data[key])
#
#
#    def test_suite2(self):
#        print()
#        test_path = os.path.join(THIS_DIR, 'gaussian')
#
#        print("test_path: ", test_path)
#
#        for method, logs in Test_suite1.items():
#            for log_file in logs:
#                test_name = 'Gaussian '+ method
#                file_path = os.path.join(test_path, method, log_file)
#
#                test_data_list = parser.parse(file_path)
#                k=1
#                for i, test_data in enumerate(test_data_list):
#                    #This converts test_data into a dictionary from JSON
#                    #For easy comparison and readability
#                    test_data = json.loads(test_data)
#
#                    if len(test_data_list) > 1:
#                    #   ref_name = log_file.replace('.log','#'+str(i+1)+'.json')
#                        ref_owl_name = log_file +'_'+ str(i+1)+'.owl'
#                        owl_name = log_file +"_"+ str(i+1)+'.owl'
#
#                    else:
#                        #ref_name = log_file.replace('.log','.json')
#                        ref_owl_name =  log_file+'.owl'
#                        owl_name = log_file + '.owl'
#
#                    owl_ref_path = os.path.join(test_path, method, ref_owl_name)
#
#                    print("owl_ref_path:", owl_ref_path)
#
#
##                   r = random.uniform(100000,1000000)
#                    r=1
#                    file_name= Path(log_file).stem
#
#
#                    test_graph = Graph()
#
#                    empirical_formula = test_data["Empirical formula"]
#                    program_version = test_data["Program version"]
#                    ontology_base_uri = "http://www.theworldavatar.com/kb/ontocompchem/" + file_name + "/" + owl_name + "#"
#                    source_kb_base_uri =  "http://theworldavatar.com/kb/ontocompchem/" +file_name +"/"
#                    source_data_base_uri =  "http://theworldavatar.com/data/ontocompchem/" + file_name + "/"
#
#                    """Namespace definition"""
#                    ontocompchem_namespace = Namespace("http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#")
#                    owl_namespace = Namespace("http://www.w3.org/2002/07/owl#")
#                    rdf_namespace= Namespace("http://www.w3.org/1999/02/22-rdf-syntax-ns#")
#                    gc_namespace=Namespace("http://purl.org/gc/")
#                    unit_namespace=Namespace("http://data.nasa.gov/qudt/owl/unit#")
#                    table_namespace=Namespace("http://www.daml.org/2003/01/periodictable/PeriodicTable.owl#")
#
#                    test_graph.bind("table",table_namespace)
#                    test_graph.bind("ontocompchem",ontocompchem_namespace)
#                    test_graph.bind("owl",owl_namespace)
#                    test_graph.bind("rdf", rdf_namespace)
#                    test_graph.bind("gc", gc_namespace)
#                    test_graph.bind("unit", unit_namespace)
#
#                    '''OntoCompChem ontology that is resolvable'''
#                    ontocompchem_ontology = URIRef("http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl")
#
#                    '''Create ontocompchem knowledge graph by generating owl file.'''
#                    CompChemObj = OntoCompChemData()
#                    CompChemObj.create_ontocompchem_graph(test_graph, test_data, ontology_base_uri, source_kb_base_uri,source_data_base_uri,ontocompchem_ontology, file_name, program_version, table_namespace, ontocompchem_namespace, gc_namespace, unit_namespace,log_file, owl_name,r)
#
#                    ref_graph = rdflib.Graph()
#
#                    ref_graph.parse(owl_ref_path, format='ttl')
#
#                    ''' Printing reference graph that is an instance of OntoCompChem ontology.'''
#                    #print(ref_graph.serialize(format="ttl").decode("utf-8"))
#
#                    '''Printing created ontology (test graph) that is an instance of OntoCompChem ontology.'''
#                    #print(test_graph.serialize(format="ttl").decode("utf-8"))
#
#                    self.assertEqual(len(test_graph.serialize(format="ttl").decode("utf-8")),len(ref_graph.serialize(format="ttl").decode("utf-8")))
#                    self.assertEqual(test_graph.serialize(format="ttl").decode("utf-8"),ref_graph.serialize(format="ttl").decode("utf-8"))
#                    '''To get a report of which line has a difference '''
#                    self.assertListEqual(list(test_graph.serialize(format="ttl").decode("utf-8")),list(ref_graph.serialize(format="ttl").decode("utf-8")))

def main(out = sys.stderr, verbosity = 2):
    loader = unittest.TestLoader()

    suite = loader.loadTestsFromModule(sys.modules[__name__])
    unittest.TextTestRunner(out, verbosity = verbosity).run(suite)

if __name__ == '__main__':
    with open(gaussianTestsLog, 'w') as f:
        main(f)
