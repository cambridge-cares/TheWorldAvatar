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
        
    def outputowl(self,ontocompchem_graph, file_name, rnd):
        print("output owl")
        for i, json_dat in enumerate(self.data):
                  dict_data = json.loads(json_dat)
            
        print("dict_data",dict_data)
            
        empirical_formula = dict_data["Empirical formula"]
        #empirical_formula_literal = Literal(empirical_formula)
         
        program_version = dict_data["Program version"]
      
         
        #print("log file path: " , os.path.abspath(self.log))
        #print("file name with folder path: ", os.path.splitext(self.log))
        #print("online file name: " , Path(self.log).stem)
        
        #file_name =Path(self.log).stem        
        
        ontology_base_uri = "http://theworldavatar.com/kb/ontocompchem/" + file_name + "/" + file_name + ".owl#" 
        
        print("base uri: ", ontology_base_uri)
        
        #ontocompchem_graph = Graph()
        
        #Namespace definition
        ontocompchem_namespace = Namespace("http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#")   
        owl_namespace = Namespace("http://www.w3.org/2002/07/owl#")
        rdf_namespace= Namespace("http://www.w3.org/1999/02/22-rdf-syntax-ns#")
        gc_namespace=Namespace("http://purl.org/gc/")
        
        ontocompchem_graph.bind("ontocompchem",ontocompchem_namespace)
        ontocompchem_graph.bind("owl",owl_namespace)
        ontocompchem_graph.bind("rdf", rdf_namespace)
        ontocompchem_graph.bind("gc", gc_namespace)
        
        ontocompchem_ontology = URIRef("http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl")
        
                
        self.importontology(ontocompchem_graph,ontology_base_uri,ontocompchem_ontology)
        self.generate_gaussian_instance(program_version, ontocompchem_graph, ontology_base_uri, file_name, ontocompchem_namespace,rnd)
        self.generate_empirical_formula(ontocompchem_graph, ontology_base_uri, gc_namespace, ontocompchem_namespace,rnd)
        
        #printing created ontology that is an instance of OntoCompChem ontology.
        print(ontocompchem_graph.serialize(format="pretty-xml").decode("utf-8"))
        
        ontocompchem_graph.serialize(destination=os.path.splitext(self.log)[0]+'.owl', format='pretty-xml')

        
    def importontology(self,ontocompchem_graph,ontology_base_uri,ontocompchem_ontology):
        #import ontocompchem ontology    
        ontocompchem_graph.add((URIRef(ontology_base_uri), RDF.type, OWL.Ontology ))
        ontocompchem_graph.add((URIRef(ontology_base_uri), OWL.imports,ontocompchem_ontology))
    
    def generate_gaussian_instance(self,program_version,ontocompchem_graph,ontology_base_uri, file_name,ontocompchem_namespace,r):
        #Generates instance of calculation based on Gaussian software used. Currently we support G09 and G16
        if program_version.startswith("2009") :
             ontocompchem_graph.add((URIRef(ontology_base_uri+file_name), RDF.type, ontocompchem_namespace.G09))
             
        else: 
             ontocompchem_graph.add((URIRef(ontology_base_uri+file_name), RDF.type, ontocompchem_namespace.G16))
        
        ontocompchem_graph.add((URIRef(ontology_base_uri+file_name), RDF.type, OWL.Thing))
        ontocompchem_graph.add((URIRef(ontology_base_uri+file_name), ontocompchem_namespace.hasInitialization, URIRef(ontology_base_uri+"job_module_has_initilization_module_"+str(r))))
            
        
    def generate_empirical_formula(self,ontocompchem_graph,ontology_base_uri,gc_namespace,ontocompchem_namespace,rnd):
        for i, json_dat in enumerate(self.data):
                  dict_data = json.loads(json_dat)
            
        print("dict_data",dict_data)
            
        empirical_formula = dict_data["Empirical formula"]
        empirical_formula_literal = Literal(empirical_formula)
        #Generates graph that represents empirical formula
        ontocompchem_graph.add((URIRef(ontology_base_uri+"job_module_has_initilization_module_" + str(rnd)), RDF.type, ontocompchem_namespace.InitializationModule))
        ontocompchem_graph.add((URIRef(ontology_base_uri+"job_module_has_initilization_module_"+ str(rnd)), RDF.type, OWL.Thing))
        ontocompchem_graph.add((URIRef(ontology_base_uri+"job_module_has_initilization_module_"+ str(rnd)), gc_namespace.hasMoleculeProperty, URIRef(ontology_base_uri+"initialization_module_has_molecule_property_"+ str(rnd))))
        ontocompchem_graph.add((URIRef(ontology_base_uri+"initialization_module_has_molecule_property_"+ str(rnd)), RDF.type, gc_namespace.MoleculeProperty))
        ontocompchem_graph.add((URIRef(ontology_base_uri+"initialization_module_has_molecule_property_"+ str(rnd)), RDF.type, OWL.Thing))        
        ontocompchem_graph.add((URIRef(ontology_base_uri+"initialization_module_has_molecule_property_"+ str(rnd)), gc_namespace.hasName, empirical_formula_literal ))
          
             
        
            
        
        
                   
               
                
                
                
    
            
                    
                
                
            