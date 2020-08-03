from compchemparser.parsers.ccgaussian_parser import CcGaussianParser
import compchemparser.helpers.ccutils as ccutils
import compchemparser.helpers.utils as utils
import json
from pip._vendor.six import iteritems

#Added by Nenad Krdzavac
from rdflib import URIRef, BNode, Literal, Graph, Namespace
from rdflib.namespace import RDF, RDFS, OWL
from pathlib import Path
import os


# main class for parsed data
class OntoCompChemData:

    def __init__(self):
        self.log = ''
        self.parser = None
        # array of json objects
        self.data = []
        
        

    # routine that extracts data from a log ile
    def getData(self, logFile):
        # use cclib package "get_ccattr" utility to determine the log file type
        ccpackage = ccutils.get_ccattr(logFile,"metadata","package")

        # at the moment only Gaussian log files are supported
        if ccpackage in ccutils.CCPACKAGES:
            # set the parser
            self.parser = CcGaussianParser()
        else:
            utils.dienicely("ERROR: Provided log fie is either incorrect or comes from an unsupported quantum chemistry package.")

        # set and parse the log
        self.log = logFile
        self.data = self.parser.parse(self.log)

    # to be implemented by Nenad/Angiras
    def uploadToKG(self):
        print('Uploading to KG, File '+self.log)
        for i, json_data in enumerate(self.data):
            print('    uploading json entry '+str(i+1))
            # upload call ...    
            
    # Nenad Krdzavac/Angiras Menon
    def getAtomicMasses(self):        
        print('Generate o OWL, File '+self.log)
        for i, json_data in enumerate(self.data):
            #Nenad Krdzavac (caresssd@hermes.cam.ca.uk)
            data = json.loads(json_data)
            print('Atomic masses : ',data["Atomic masses"])
        for i in data["Atomic masses"]:
            print("atomic mass: ", i)
                
       
            
        
    def outputjson(self):
        print('Dumping to JSON, File '+self.log)
        for i, json_dat in enumerate(self.data):
            if len(self.data) > 1:
                json_name = self.log.replace('.log','#'+str(i+1)+'.json')
            else:
                json_name = self.log.replace('.log','.json')

            # dump call ...
            dict_data = json.loads(json_dat)
            with open(json_name, 'w') as outfile:
                json.dump(dict_data, outfile, indent = 4)
                
                #implemented by Nenad Krdzavac (caresssd@hermes.cam.ac.uk)
                #print()
                #print('JSON content: ', dict_data)
                #print()
                #print('Atomic masses : ',dict_data["Atomic masses"])
                #print('Empirical formula : ',dict_data["Empirical formula"])
                #print('Atom counts: ', dict_data["Atom counts"])
                #for atom in dict_data["Geometry"]:
                #    print("geometry: ", "[x=", atom[0],", y=",atom[1], ", z=",atom[2],"]" )
                #print()
                #print("Print all json key and values:")
                #for (key, value) in iteritems(dict_data):
                #    print("key: ", key, "value: ", value)
                #print("print i:")
                #for i in enumerate(self.data):
                #   print(i[1])
        
    def outputowl(self):
        print("output owl")
        for i, json_dat in enumerate(self.data):
                  dict_data = json.loads(json_dat)
            
        print("dict_data",dict_data)
            
        empirical_formula = dict_data["Empirical formula"]
        program_version = dict_data["Program version"]
      
        print("log file path: " , os.path.abspath(self.log))
        print("file name with folder path: ", os.path.splitext(self.log))
        print("online file name: " , Path(self.log).stem)
        
        file_name =Path(self.log).stem        
        
        ontology_base_uri = "http://theworldavatar.com/kb/ontocompchem/" + file_name + "/" + file_name + ".owl#" 
        
        print("base uri: ", ontology_base_uri)
        
        ontocompchemgraph = Graph()
        
        #Namespace definition
        ontocompchemnamespace = Namespace("http://theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#")   
        owlnamespace = Namespace("http://www.w3.org/2002/07/owl#")
        rdfnamespace= Namespace("http://www.w3.org/1999/02/22-rdf-syntax-ns#")
        
        ontocompchemgraph.bind("ontocompchem",ontocompchemnamespace)
        ontocompchemgraph.bind("owl",owlnamespace)
        ontocompchemgraph.bind("rdf", rdfnamespace)
        
        computationModule = URIRef("http://theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#cm")
        ontocompchemOntology = URIRef("http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl")
        
        #ontocompchem.computationalModule
        
    #   ontocompchemgraph.bind("owl",OWL.start_namespace_decl())
    #   ontocompchemgraph.add((computationModule, RDF.type, ontocompchem.ComputationModule))

        #import ontocompchem ontology    
        ontocompchemgraph.add((URIRef(ontology_base_uri), RDF.type, OWL.Ontology ))
        ontocompchemgraph.add((URIRef(ontology_base_uri), OWL.imports,ontocompchemOntology))
        
        #ontocompchemgraph.add((URIRef("http://theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#"), RDF.type, OWL.Ontology ))
        
        #printing created ontology that is an instance of OntoCompChem ontology.               
        print(ontocompchemgraph.serialize(format="pretty-xml").decode("utf-8"))
        
        ontocompchemgraph.serialize(destination=os.path.splitext(self.log)[0]+'.owl', format='pretty-xml')
        
        print()
        
        print("Directory Path:", __file__)
        
        
   
            
        
        
                   
               
                
                
                
    
            
                    
                
                
            