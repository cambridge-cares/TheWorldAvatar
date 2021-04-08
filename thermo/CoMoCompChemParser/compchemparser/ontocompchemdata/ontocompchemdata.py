from compchemparser.parsers.ccgaussian_parser import CcGaussianParser
import compchemparser.helpers.ccutils as ccutils
import compchemparser.helpers.utils as utils
import json
import sys
from pip._vendor.six import iteritems

#Added by Nenad Krdzavac
from rdflib import URIRef, BNode, Literal, Graph, Namespace
from rdflib.namespace import RDF, RDFS, OWL, XSD
from pathlib import Path
import random
import uuid
import os
import decimal
import rdflib
from symbol import atom


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

    def outputjson(self,pathflag):
        print('Dumping to JSON, File '+self.log)
        print("self.log : ", self.log)
        k=1
        global t
        for i, json_dat in enumerate(self.data):
            if len(self.data) > 1:
            #    json_name = self.log.replace('.log','#'+str(i+1)+'.json')
                 json_name = self.log + '_' + str(i+1)+'.json'                
                 owl_name = base=os.path.basename(self.log) + '_' + str(i+1)+'.owl'
            else:
            #    json_name = self.log.replace('.log','.json')
                 json_name = self.log + '.json'                
                 owl_name = base=os.path.basename(self.log) + '.owl'
                
 
            
            dict_data = json.loads(json_dat)
            with open(json_name, 'w') as outfile:
                json.dump(dict_data, outfile, indent = 4)
            
                 
            ''' Generates knowledge graph as owl file for each generated josn file '''
#           r = random.uniform(100000,1000000)
            r=1
            file_name= Path(self.log).stem
            
            ontocompchem_graph = Graph()
            
            #gives log file name with extension without file path
            log_file_name = os.path.basename(self.log)
            
            if pathflag == True:
                self.outputowl(ontocompchem_graph, dict_data, file_name, log_file_name,owl_name,r, k, True)
                k = k+1
                for (key, value) in iteritems(dict_data):
                    print(" - ", key, " : ", value)
            
            if pathflag == False:
                self.outputowl(ontocompchem_graph, dict_data, file_name, log_file_name,owl_name,r, k, False)
                k = k+1
                for (key, value) in iteritems(dict_data):
                    print(" - ", key, " : ", value)
             

    def outputowl(self,ontocompchem_graph, dict_data,file_name, log_file_name, owl_name,rnd,k,pathflag):

        print("output owl: ")

        #for i, json_dat in enumerate(self.data):
        #    dict_data = json.loads(json_dat)

        empirical_formula = dict_data["Empirical formula"]
        program_version = dict_data["Program version"]

        ontology_base_uri = "http://www.theworldavatar.com/kb/ontocompchem/" + file_name + "/" + owl_name + "#"
        source_kb_base_uri =  "http://theworldavatar.com/kb/ontocompchem/" + file_name + "/"
        source_data_base_uri =  "http://theworldavatar.com/data/ontocompchem/" + file_name + "/"

        """Namespace definition"""
        ontocompchem_namespace = Namespace("http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#")
        owl_namespace = Namespace("http://www.w3.org/2002/07/owl#")
        rdf_namespace= Namespace("http://www.w3.org/1999/02/22-rdf-syntax-ns#")
        gc_namespace=Namespace("http://purl.org/gc/")
        unit_namespace=Namespace("http://data.nasa.gov/qudt/owl/unit#")
        table_namespace=Namespace("http://www.daml.org/2003/01/periodictable/PeriodicTable.owl#")

        ontocompchem_graph.bind("table",table_namespace)
        ontocompchem_graph.bind("ontocompchem",ontocompchem_namespace)
        ontocompchem_graph.bind("owl",owl_namespace)
        ontocompchem_graph.bind("rdf", rdf_namespace)
        ontocompchem_graph.bind("gc", gc_namespace)
        ontocompchem_graph.bind("unit", unit_namespace)

        '''OntoCompChem ontology that is resolvable'''
        ontocompchem_ontology = URIRef("http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl")


        '''Create ontocompchem knowledge graph by generating owl file.'''
        self.create_ontocompchem_graph(ontocompchem_graph, dict_data, ontology_base_uri, source_kb_base_uri,source_data_base_uri, ontocompchem_ontology, file_name, program_version, table_namespace, ontocompchem_namespace, gc_namespace, unit_namespace, log_file_name,owl_name,rnd)

        '''Printing created ontology that is an instance of OntoCompChem ontology.'''
        #print(ontocompchem_graph.serialize(format="ttl").decode("utf-8"))


        '''Serialize generated graph into owl file.'''
        #ontocompchem_graph.serialize(destination=os.path.splitext(self.log)[0]+'.owl', format='pretty-xml')

        '''File path to Gaussian calculation.'''
        g_path = str(os.path.abspath(self.log))
        
        ''' Check the number of jobs found in log file'''
        if pathflag == True:
            path = sys.argv[6]
            if len(self.data) > 1:
#                owl_path = os.path.splitext(g_path)[0]+"#"+str(k)+".owl"                 
                 #Line below is commented because it causes problem with generating owl file. Commented by Nenad Krdzavac (caresssd@hermes.cam.ac.uk)
                 #owl_name = Path(self.log).stem + '_' + str(k)+'.owl'                 
                 #owl_path = g_path+"#"+str(k)+".owl"
                 owl_path = path + owl_name
                
            else:
#                owl_path = os.path.splitext(g_path)[0]+".owl"
                 #Line below is commented because it causes problem with generating owl file. Commented by Nenad Krdzavac (caresssd@hermes.cam.ac.uk)
                 #owl_name = Path(self.log).stem + ".owl"                 
                 #owl_path = g_path+".owl" 
                 owl_path = path + owl_name
        
        elif pathflag == False:
            if len(self.data) > 1:
#                owl_path = os.path.splitext(g_path)[0]+"#"+str(k)+".owl"
                 owl_path = g_path+"_"+str(k)+".owl"
            else:
#                owl_path = os.path.splitext(g_path)[0]+".owl"
                 owl_path = g_path+".owl"


        print("owl_path: " , owl_path)

        '''Write owl content into owl file that is stored in the same folder where json file is saved.'''
        f = open(owl_path, "w")
        f.write(ontocompchem_graph.serialize(format="ttl").decode("utf-8"))
        f.close()

    def create_ontocompchem_graph(self,ontocompchem_graph, dict_data, ontology_base_uri,source_kb_base_uri,source_data_base_uri,ontocompchem_ontology,file_name,program_version,table_namespace,ontocompchem_namespace,gc_namespace,unit_namespace,log_file_name,owl_name,rnd):

         '''Create main ontocompchem knowledge graph (instance of ontocompchem Tbox)'''
         self.import_ontology(ontocompchem_graph,ontology_base_uri,ontocompchem_ontology)
         self.generate_gaussian_instance(program_version, dict_data, ontocompchem_graph, ontology_base_uri, file_name, ontocompchem_namespace,rnd)
         self.generate_empirical_formula(ontocompchem_graph, dict_data,ontology_base_uri, gc_namespace, ontocompchem_namespace,rnd)
         self.generate_level_of_theory(ontocompchem_graph, dict_data,ontology_base_uri, ontocompchem_namespace, gc_namespace, rnd)
         self.generate_basis_set(ontocompchem_graph, dict_data,ontology_base_uri, gc_namespace, rnd)
         self.generate_geometry_type(ontocompchem_graph, dict_data, ontology_base_uri, ontocompchem_namespace, gc_namespace, file_name, rnd)
         self.generate_frequencies(ontocompchem_graph,  dict_data,ontology_base_uri, ontocompchem_namespace, gc_namespace, file_name, rnd)
         self.generate_rotational_symmetry_number(ontocompchem_graph,  dict_data,ontocompchem_namespace, gc_namespace, ontology_base_uri, file_name, rnd)
         self.generate_spin_multiplicity(ontocompchem_graph, dict_data, ontocompchem_namespace, gc_namespace, ontology_base_uri, file_name, rnd)
         self.generate_formal_charge(ontocompchem_graph, dict_data, gc_namespace, ontology_base_uri, file_name, rnd)
         self.generate_program_name_run_date_program_version(ontocompchem_graph,  dict_data,ontocompchem_namespace, gc_namespace, ontology_base_uri, file_name, rnd)
         self.generate_rotational_constants(ontocompchem_graph,  dict_data,ontocompchem_namespace, gc_namespace, unit_namespace, ontology_base_uri, file_name, rnd)
         self.generate_geometry_atomic_masses(ontocompchem_graph,  dict_data,ontocompchem_namespace, table_namespace, ontology_base_uri, file_name, gc_namespace, rnd)
         self.generate_atom_count(ontocompchem_graph,  dict_data,ontocompchem_namespace, gc_namespace, ontology_base_uri, file_name, rnd)
         self.generate_electronic_and_zpe_energy(ontocompchem_graph,  dict_data,ontocompchem_namespace, gc_namespace, ontology_base_uri, file_name, rnd)

         self.generate_homo_energy(ontocompchem_graph,  dict_data,ontocompchem_namespace, gc_namespace, ontology_base_uri, file_name, rnd)
         self.generate_homo_minus_one_energy(ontocompchem_graph,  dict_data,ontocompchem_namespace, gc_namespace, ontology_base_uri, file_name, rnd)
         self.generate_homo_minus_two_energy(ontocompchem_graph,  dict_data,ontocompchem_namespace, gc_namespace, ontology_base_uri, file_name, rnd)

         self.generate_lumo_energy(ontocompchem_graph,  dict_data,ontocompchem_namespace, gc_namespace, ontology_base_uri, file_name, rnd)
         self.generate_lumo_plus_one_energy(ontocompchem_graph,  dict_data,ontocompchem_namespace, gc_namespace, ontology_base_uri, file_name, rnd)
         self.generate_lumo_plus_two_energy(ontocompchem_graph,  dict_data,ontocompchem_namespace, gc_namespace, ontology_base_uri, file_name, rnd)

         self.generate_scf_energy(ontocompchem_graph, dict_data, ontocompchem_namespace, gc_namespace, ontology_base_uri, file_name, rnd)
         self.generate_file_iri(ontocompchem_graph,  dict_data, ontocompchem_namespace, gc_namespace, ontology_base_uri, source_kb_base_uri, source_data_base_uri, file_name, log_file_name, owl_name,rnd)


    def import_ontology(self,ontocompchem_graph,ontology_base_uri,ontocompchem_ontology):

         '''Import ontocompchem ontology'''
         ontocompchem_graph.add((URIRef(ontology_base_uri), RDF.type, OWL.Ontology ))
         ontocompchem_graph.add((URIRef(ontology_base_uri), OWL.imports,ontocompchem_ontology))

    def generate_gaussian_instance(self,program_version,dict_data,ontocompchem_graph,ontology_base_uri, file_name,ontocompchem_namespace,rnd):

#        for i, json_dat in enumerate(self.data):
#                  dict_data = json.loads(json_dat)

        '''Extract empirical formula '''
        empirical_formula = dict_data["Empirical formula"]


        '''Generates instance of calculation based on Gaussian software used. Currently we support G09 and G16.'''
        if program_version.startswith("2009") :
             ontocompchem_graph.add((URIRef(ontology_base_uri+file_name), RDF.type, ontocompchem_namespace.G09))

        elif program_version.startswith("2003"):
             ontocompchem_graph.add((URIRef(ontology_base_uri + file_name), RDF.type, ontocompchem_namespace.G03))

        else:
             ontocompchem_graph.add((URIRef(ontology_base_uri+file_name), RDF.type, ontocompchem_namespace.G16))

        ontocompchem_graph.add((URIRef(ontology_base_uri+file_name), RDF.type, OWL.Thing))
        ontocompchem_graph.add((URIRef(ontology_base_uri+file_name), ontocompchem_namespace.hasInitialization, URIRef(ontology_base_uri+"job_module_has_initilization_module_"+str(empirical_formula)+"_"+str(rnd))))


    def generate_empirical_formula(self,ontocompchem_graph,dict_data,ontology_base_uri,gc_namespace,ontocompchem_namespace,rnd):

        # for i, json_dat in enumerate(self.data):
        #          dict_data = json.loads(json_dat)

         '''Extract empirical formula .'''
         empirical_formula = dict_data["Empirical formula"]
         #make space between characters
         #empirical_formula_space =' '.join(empirical_formula)
         #make empirical formula literal
         empirical_formula_literal = Literal(empirical_formula)
         '''Generates graph that represents empirical formula.'''
         ontocompchem_graph.add((URIRef(ontology_base_uri+"job_module_has_initilization_module_"+str(empirical_formula)+"_" + str(rnd)), RDF.type, ontocompchem_namespace.InitializationModule))
         ontocompchem_graph.add((URIRef(ontology_base_uri+"job_module_has_initilization_module_"+str(empirical_formula)+"_"+ str(rnd)), RDF.type, OWL.Thing))
         ontocompchem_graph.add((URIRef(ontology_base_uri+"job_module_has_initilization_module_"+str(empirical_formula)+ "_"+str(rnd)), gc_namespace.hasMoleculeProperty, URIRef(ontology_base_uri+"initialization_module_has_molecule_property_"+ str(empirical_formula)+ "_"+str(rnd))))
         ontocompchem_graph.add((URIRef(ontology_base_uri+"initialization_module_has_molecule_property_"+ str(empirical_formula)+ "_"+str(rnd)), RDF.type, gc_namespace.MoleculeProperty))
         ontocompchem_graph.add((URIRef(ontology_base_uri+"initialization_module_has_molecule_property_"+ str(empirical_formula)+ "_"+str(rnd)), RDF.type, OWL.Thing))
         ontocompchem_graph.add((URIRef(ontology_base_uri+"initialization_module_has_molecule_property_"+ str(empirical_formula)+ "_"+str(rnd)), gc_namespace.hasName, empirical_formula_literal))

    def generate_level_of_theory(self,ontocompchem_graph,dict_data,ontology_base_uri,ontocompchem_namespace,gc_namespace,rnd):
         '''Generates level of theory.'''
        # for i, json_dat in enumerate(self.data):
        #          dict_data = json.loads(json_dat)

         if "Method" in dict_data:
             method =  dict_data["Method"]
             empirical_formula = dict_data["Empirical formula"]

             '''[am2145@cam.ac.uk]: Generally speaking, I don' t think it is possible to run a Gaussian job without providing the method, so I would expect that method is present somewhere in any successfully run job.'''
             if "Basis set" in dict_data:
                 basis_set = dict_data["Basis set"]

                 '''if method and basis set are equal then level of theory has value equal to one of them. If method and basis set are different as strings, then level of theory has value as a string that contains both method
                    and basis set separated by "/" character. Explanation given by Angiras Menon (am2145@cam.ac.uk)'''
                 if method==basis_set :
                     level_of_theory = method
                 else:
                     level_of_theory = method +"/"+basis_set

                 level_of_theory_literal = Literal(level_of_theory)
             else:
                  level_of_theory = method
                  level_of_theory_literal = Literal(level_of_theory)

             '''Creating graph for level of theory quantity.'''
             ontocompchem_graph.add((URIRef(ontology_base_uri+"job_module_has_initilization_module_"+ str(empirical_formula)+ "_"+str(rnd)), gc_namespace.hasParameter, URIRef(ontology_base_uri+"initialization_module_has_level_of_theory_parameter_"+ str(empirical_formula)+ "_"+str(rnd))))
             ontocompchem_graph.add((URIRef(ontology_base_uri+"initialization_module_has_level_of_theory_parameter_" + str(empirical_formula)+ "_"+str(rnd)), RDF.type, ontocompchem_namespace.LevelOfTheory))
             ontocompchem_graph.add((URIRef(ontology_base_uri+"initialization_module_has_level_of_theory_parameter_" + str(empirical_formula)+ "_"+str(rnd)), RDF.type, gc_namespace.MethodologyFeature))
             ontocompchem_graph.add((URIRef(ontology_base_uri+"initialization_module_has_level_of_theory_parameter_"+ str(empirical_formula)+ "_"+str(rnd)), RDF.type, OWL.Thing))
             ontocompchem_graph.add((URIRef(ontology_base_uri+"initialization_module_has_level_of_theory_parameter_"+ str(empirical_formula)+ "_"+str(rnd)), ontocompchem_namespace.hasLevelOfTheory, level_of_theory_literal))

    def generate_basis_set(self,ontocompchem_graph,dict_data,ontology_base_uri,gc_namespace,rnd):

         '''Generates graph for basis set quantity.'''
        # for i, json_dat in enumerate(self.data):
        #          dict_data = json.loads(json_dat)

         if "Basis set" in dict_data:
             basis_set = dict_data["Basis set"]
             basis_set_literal = Literal(basis_set)
             empirical_formula = dict_data["Empirical formula"]
             ontocompchem_graph.add((URIRef(ontology_base_uri+"job_module_has_initilization_module_"+ str(empirical_formula)+ "_"+str(rnd)), gc_namespace.hasParameter, URIRef(ontology_base_uri+"initialization_module_has_basis_set_parameter_"+str(empirical_formula)+ "_"+ str(rnd))))
             ontocompchem_graph.add((URIRef(ontology_base_uri+"initialization_module_has_basis_set_parameter_" + str(empirical_formula)+ "_"+str(rnd)), RDF.type, gc_namespace.BasisSet))
             ontocompchem_graph.add((URIRef(ontology_base_uri+"initialization_module_has_basis_set_parameter_"+ str(empirical_formula)+ "_"+str(rnd)), RDF.type, OWL.Thing))
             ontocompchem_graph.add((URIRef(ontology_base_uri+"initialization_module_has_basis_set_parameter_"+ str(empirical_formula)+ "_"+str(rnd)), gc_namespace.hasBasisSet, basis_set_literal))

    def generate_geometry_type(self, ontocompchem_graph, dict_data, ontology_base_uri, ontocompchem_namespace,gc_namespace, file_name, rnd):

         '''Generate unique string.'''
         uuid_geometry_type = uuid.uuid3(uuid.NAMESPACE_DNS,"geometry.type")

         '''Generates graph for geometry type quantity.'''
         #for i, json_dat in enumerate(self.data):
         #        dict_data = json.loads(json_dat)

         if "Geometry type" in dict_data:
             geometry_type= dict_data["Geometry type"]
             geometry_type_literal = Literal(geometry_type)
             empirical_formula = dict_data["Empirical formula"]
             ontocompchem_graph.add((URIRef(ontology_base_uri+file_name), gc_namespace.isCalculationOn, URIRef(ontology_base_uri+"finalization_module_geometry_type_"+str(empirical_formula)+ "_"+str(uuid_geometry_type)+"_"+str(rnd))))
             ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_geometry_type_"+str(empirical_formula)+ "_"+str(uuid_geometry_type)+"_"+str(rnd)), RDF.type, ontocompchem_namespace.GeometryType))
             ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_geometry_type_"+str(empirical_formula)+ "_"+str(uuid_geometry_type)+"_"+str(rnd)), RDF.type, OWL.Thing))
             ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_geometry_type_"+str(empirical_formula)+ "_"+str(uuid_geometry_type)+"_"+str(rnd)), ontocompchem_namespace.hasGeometryType,geometry_type_literal))

    def generate_frequencies(self,ontocompchem_graph, dict_data,ontology_base_uri,ontocompchem_namespace,gc_namespace,file_name,rnd):

         '''Generate unique string.'''
         uuid_frequency = uuid.uuid3(uuid.NAMESPACE_DNS,"frequency")

         '''Generates graph for frequencies quantity.'''
        # for i, json_dat in enumerate(self.data):
        #          dict_data = json.loads(json_dat)


         if "Frequencies" in dict_data :
             empirical_formula = dict_data["Empirical formula"]
             frequency_string = "  "
             for fr in dict_data["Frequencies"]:
                 frequency_string = str(str(round(decimal.Decimal(fr),4))) + " " + frequency_string

             '''Removes empty space at the end of string'''
             frequency_string = frequency_string.rstrip()

             frequency_string_literal = Literal(frequency_string,  datatype=XSD.string)

             frequencies_size = dict_data["Frequencies number"]
             frequencies_size_literal = Literal(frequencies_size,  datatype=XSD.string)

             frequencies_unit = dict_data["Frequencies unit"]

             '''Creates graph for frequencies quantity that includes frequencies value, unit and size.'''
             ontocompchem_graph.add((URIRef(ontology_base_uri+file_name), gc_namespace.isCalculationOn, URIRef(ontology_base_uri+"finalization_module_vibrations_"+str(empirical_formula)+ "_"+str(uuid_frequency)+"_"+str(rnd))))
             ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_vibrations_"+str(empirical_formula)+ "_"+str(uuid_frequency)+"_"+str(rnd)), RDF.type, gc_namespace.VibrationalAnalysis))
             ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_vibrations_"+str(empirical_formula)+ "_"+str(uuid_frequency)+"_"+str(rnd)), RDF.type, OWL.Thing))
             ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_vibrations_"+str(empirical_formula)+ "_"+str(uuid_frequency)+"_"+str(rnd)), gc_namespace.hasResult, URIRef(ontology_base_uri+"finalization_module_vibrations_frequencies_"+str(empirical_formula)+ "_"+str(uuid_frequency)+"_"+str(rnd))))
             ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_vibrations_frequencies_"+str(empirical_formula)+ "_"+str(uuid_frequency)+"_"+str(rnd)), RDF.type, gc_namespace.Frequency))
             ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_vibrations_frequencies_"+str(empirical_formula)+ "_"+str(uuid_frequency)+"_"+str(rnd)), RDF.type, OWL.Thing))

             ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_vibrations_frequencies_"+str(empirical_formula)+ "_"+str(uuid_frequency)+"_"+str(rnd)), ontocompchem_namespace.hasFrequencies, frequency_string_literal))

             '''Creates iri for unit cm^-1 (gc:cm-1)'''
             if frequencies_unit == "cm^-1":
                     ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_vibrations_frequencies_"+str(empirical_formula)+ "_"+str(uuid_frequency)+"_"+str(rnd)), gc_namespace.hasUnit, URIRef(gc_namespace + "cm-1")))

             ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_vibrations_frequencies_"+str(empirical_formula)+ "_"+str(uuid_frequency)+"_"+str(rnd)), gc_namespace.hasVibrationCount, frequencies_size_literal))


    def generate_rotational_symmetry_number(self,ontocompchem_graph, dict_data,ontocompchem_namespace,gc_namespace,ontology_base_uri,file_name,rnd):

         '''Generate unique string'''
         uuid_rotational_symmetry_number = uuid.uuid3(uuid.NAMESPACE_DNS,"rotaional.symmetry.number")

         '''Generates graph for rotational symmetry quantity'''
        # for i, json_dat in enumerate(self.data):
        #          dict_data = json.loads(json_dat)

         if "Rotational symmetry number" in dict_data :
             empirical_formula = dict_data["Empirical formula"]
             rotational_symmetry_number= dict_data["Rotational symmetry number"]
             rotational_symmetry_number_literal = Literal(rotational_symmetry_number,datatype=XSD.string)

             ontocompchem_graph.add((URIRef(ontology_base_uri+file_name), gc_namespace.isCalculationOn,
                                 URIRef(ontology_base_uri+"finalization_module_rotational_symmetry_"+str(empirical_formula)+ "_"+str(uuid_rotational_symmetry_number)+"_"+str(rnd))))
             ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_rotational_symmetry_"+str(empirical_formula)+ "_"+str(uuid_rotational_symmetry_number)+"_"+str(rnd)), RDF.type, ontocompchem_namespace.RotationalSymmetry))
             ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_rotational_symmetry_"+str(empirical_formula)+ "_"+str(uuid_rotational_symmetry_number)+"_"+str(rnd)), RDF.type, OWL.Thing))
             ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_rotational_symmetry_"+str(empirical_formula)+ "_"+str(uuid_rotational_symmetry_number)+"_"+str(rnd)),ontocompchem_namespace.hasRotationalSymmetryNumber,rotational_symmetry_number_literal))

    def generate_spin_multiplicity(self,ontocompchem_graph, dict_data,ontocompchem_namespace,gc_namespace,ontology_base_uri,file_name,rnd):

         '''Generates graph for spin multiplicity quantity'''
        # for i, json_dat in enumerate(self.data):
        #          dict_data = json.loads(json_dat)

         if "Spin multiplicity" in dict_data :
             spin_multiplicity_number= dict_data["Spin multiplicity"]
             spin_multiplicity_number_literal = Literal(spin_multiplicity_number,datatype=XSD.string)
             empirical_formula = dict_data["Empirical formula"]

             ontocompchem_graph.add((URIRef(ontology_base_uri+file_name), gc_namespace.isCalculationOn, URIRef(ontology_base_uri+"finalization_module_geometry_optimization_"+str(empirical_formula)+ "_"+str(rnd))))
             ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_geometry_optimization_"+str(empirical_formula)+ "_"+str(rnd)), RDF.type, gc_namespace.GeometryOptimization))
             ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_geometry_optimization_"+str(empirical_formula)+ "_"+str(rnd)), RDF.type, OWL.Thing))
             ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_geometry_optimization_"+str(empirical_formula)+ "_"+str(rnd)), gc_namespace.hasMolecule,URIRef(ontology_base_uri+"finalization_module_has_molecule_"+str(empirical_formula)+ "_"+str(rnd))))

             ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_molecule_"+str(empirical_formula)+ "_"+str(rnd)), RDF.type, gc_namespace.Molecule))
             ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_molecule_"+str(empirical_formula)+ "_"+str(rnd)), RDF.type, OWL.Thing))

             ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_molecule_"+str(empirical_formula)+ "_"+str(rnd)), ontocompchem_namespace.hasSpinMultiplicity, spin_multiplicity_number_literal))


    def generate_formal_charge(self,ontocompchem_graph, dict_data, gc_namespace,ontology_base_uri,file_name,rnd):

         '''Generate graph for formal charge quantity'''
        # for i, json_dat in enumerate(self.data):
        #          dict_data = json.loads(json_dat)

         if "Formal charge" in dict_data:
             formal_charge_value = dict_data["Formal charge"]
             formal_charge_unit = dict_data["Formal charge unit"]
             empirical_formula = dict_data["Empirical formula"]

             formal_charge_value_literal = Literal(formal_charge_value)

             ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_molecule_"+str(empirical_formula)+ "_"+str(rnd)), gc_namespace.hasFormalCharge,URIRef(ontology_base_uri+"finalization_module_has_molecule_formal_charge_"+str(empirical_formula)+ "_"+str(rnd))))

             ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_molecule_formal_charge_"+str(empirical_formula)+ "_"+str(rnd)), RDF.type, gc_namespace.IntegerValue))
             ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_molecule_formal_charge_"+str(empirical_formula)+ "_"+str(rnd)), RDF.type, OWL.Thing))

             ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_molecule_formal_charge_"+str(empirical_formula)+ "_"+str(rnd)), gc_namespace.hasValue, formal_charge_value_literal))

             if str(formal_charge_unit) == "atomic":
                     ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_molecule_formal_charge_"+str(empirical_formula)+ "_"+str(rnd)), gc_namespace.hasUnit,URIRef(gc_namespace.atomicUnit)))


    def generate_program_name_run_date_program_version(self,ontocompchem_graph, dict_data,ontocompchem_namespace,gc_namespace,ontology_base_uri,file_name,rnd):

         '''generate graph that contains program name, program version, and run date'''
        # for i, json_dat in enumerate(self.data):
        #          dict_data = json.loads(json_dat)

         program_name = dict_data["Program name"]
         program_version = dict_data["Program version"]
         run_date = dict_data["Run date"]
         empirical_formula = dict_data["Empirical formula"]

         program_name_literal = Literal(program_name,datatype=XSD.string)
         program_version_literal = Literal(program_version,datatype=XSD.string)
         run_date_literal = Literal(run_date,datatype=XSD.string)

         ontocompchem_graph.add((URIRef(ontology_base_uri+file_name), ontocompchem_namespace.hasEnvironment,URIRef(ontology_base_uri+"job_module_has_environment_module_"+str(empirical_formula)+ "_"+str(rnd))))
         ontocompchem_graph.add((URIRef(ontology_base_uri+"job_module_has_environment_module_"+str(empirical_formula)+ "_"+str(rnd)), RDF.type, gc_namespace.SourcePackage))
         ontocompchem_graph.add((URIRef(ontology_base_uri+"job_module_has_environment_module_"+str(empirical_formula)+ "_"+str(rnd)), RDF.type, OWL.Thing))

         ontocompchem_graph.add((URIRef(ontology_base_uri+"job_module_has_environment_module_"+str(empirical_formula)+ "_"+str(rnd)), ontocompchem_namespace.hasProgram, program_name_literal))
         ontocompchem_graph.add((URIRef(ontology_base_uri+"job_module_has_environment_module_"+str(empirical_formula)+ "_"+str(rnd)), ontocompchem_namespace.hasProgramVersion, program_version_literal))
         ontocompchem_graph.add((URIRef(ontology_base_uri+"job_module_has_environment_module_"+str(empirical_formula)+ "_"+str(rnd)), ontocompchem_namespace.hasRunDate, run_date_literal))


    def generate_rotational_constants(self,ontocompchem_graph, dict_data,ontocompchem_namespace,gc_namespace,unit_namespace,ontology_base_uri,file_name,rnd):

         '''generates unique string'''
         uuid_rotational_constants = uuid.uuid3(uuid.NAMESPACE_DNS,"rotaional.constants")

         '''Generates graph for rotational constants quantity'''
        # for i, json_dat in enumerate(self.data):
        #          dict_data = json.loads(json_dat)

         if "Rotational constants" in dict_data:
             empirical_formula = dict_data["Empirical formula"]
             rotational_constants_string = "  "
             for rc in dict_data["Rotational constants"]:
                 rotational_constants_string = str(str(round(decimal.Decimal(rc),8))) + " " + rotational_constants_string

             '''removes empty space at the end of string'''
             rotational_constants_string = rotational_constants_string.rstrip()

             rotational_constants_string_literal = Literal(rotational_constants_string,  datatype=XSD.string)

             rotational_constants_size = dict_data["Rotational constants number"]
             rotational_constants_size_literal = Literal(rotational_constants_size,  datatype=XSD.string)

             rotational_constants_unit = dict_data["Rotational constants unit"]

             ontocompchem_graph.add((URIRef(ontology_base_uri+file_name), gc_namespace.isCalculationOn,URIRef(ontology_base_uri+"finalization_module_rotational_constants_"+str(empirical_formula)+ "_"+str(uuid_rotational_constants)+"_"+str(rnd))))
             ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_rotational_constants_"+str(empirical_formula)+ "_"+str(uuid_rotational_constants)+"_"+str(rnd)), RDF.type,ontocompchem_namespace.RotationalConstants))
             ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_rotational_constants_"+str(empirical_formula)+ "_"+str(uuid_rotational_constants)+"_"+str(rnd)), RDF.type, OWL.Thing))

             ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_rotational_constants_"+str(empirical_formula)+ "_"+str(uuid_rotational_constants)+"_"+str(rnd)), ontocompchem_namespace.hasRotationalConstants,rotational_constants_string_literal))
             ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_rotational_constants_"+str(empirical_formula)+ "_"+str(uuid_rotational_constants)+"_"+str(rnd)), ontocompchem_namespace.hasRotationalConstantsCount,rotational_constants_size_literal))

             if str(rotational_constants_unit) == "GHZ":
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_rotational_constants_"+str(empirical_formula)+ "_"+str(uuid_rotational_constants)+"_"+str(rnd)), gc_namespace.hasUnit,URIRef(unit_namespace.GigaHertz)))


    def generate_geometry_atomic_masses(self,ontocompchem_graph, dict_data,ontocompchem_namespace,table_namespace,ontology_base_uri,file_name,gc_namespace,rnd):

         '''Generates unique string'''
         uuid_geometry_atomic_mass = uuid.uuid3(uuid.NAMESPACE_DNS,"geometry.atomic.mass")

         '''Generates graph for geometry, atomic masses, and atom types quantities'''
        # for i, json_dat in enumerate(self.data):
        #          dict_data = json.loads(json_dat)

         atom_iterator = 0;
         if "Atom types" in dict_data:
             empirical_formula = dict_data["Empirical formula"]
             for akey in dict_data["Atom types"]:
                 
                 ontocompchem_graph.add((URIRef(ontology_base_uri+file_name), gc_namespace.isCalculationOn, URIRef(ontology_base_uri+"finalization_module_geometry_optimization_"+str(empirical_formula)+ "_"+str(rnd))))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_geometry_optimization_"+str(empirical_formula)+ "_"+str(rnd)), RDF.type, gc_namespace.GeometryOptimization))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_geometry_optimization_"+str(empirical_formula)+ "_"+str(rnd)), RDF.type, OWL.Thing))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_geometry_optimization_"+str(empirical_formula)+ "_"+str(rnd)), gc_namespace.hasMolecule,URIRef(ontology_base_uri+"finalization_module_has_molecule_"+str(empirical_formula)+ "_"+str(rnd))))

                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_molecule_"+str(empirical_formula)+ "_"+str(rnd)), RDF.type, gc_namespace.Molecule))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_molecule_"+str(empirical_formula)+ "_"+str(rnd)), RDF.type, OWL.Thing))


                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_molecule_"+str(empirical_formula)+ "_"+str(rnd)), gc_namespace.hasAtom, URIRef(ontology_base_uri+"finalization_module_has_atom_"+str(empirical_formula)+ "_"+str(akey)+str(atom_iterator)+"_"+str(uuid_geometry_atomic_mass)+"_"+str(rnd))))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_atom_"+str(empirical_formula)+ "_"+str(akey)+str(atom_iterator)+"_"+str(uuid_geometry_atomic_mass)+"_"+str(rnd)), RDF.type,gc_namespace.Atom))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_atom_"+str(empirical_formula)+ "_"+str(akey)+str(atom_iterator)+"_"+str(uuid_geometry_atomic_mass)+"_"+str(rnd)), RDF.type,OWL.Thing))

                 '''Generate atom element'''
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_atom_"+str(empirical_formula)+ "_"+str(akey)+str(atom_iterator)+"_"+str(uuid_geometry_atomic_mass)+"_"+str(rnd)),gc_namespace.isElement,URIRef("http://www.daml.org/2003/01/periodictable/PeriodicTable.owl#"+str(akey))))
                 ontocompchem_graph.add((URIRef("http://www.daml.org/2003/01/periodictable/PeriodicTable.owl#"+str(akey)),RDF.type,URIRef("http://www.daml.org/2003/01/periodictable/PeriodicTable.owl#Element")))                                                 
                 ontocompchem_graph.add((URIRef("http://www.daml.org/2003/01/periodictable/PeriodicTable.owl#"+str(akey)),RDF.type,OWL.Thing))

                 if "Atomic masses" in dict_data:
                     '''generate atomic mass'''
                     ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_atom_"+str(empirical_formula)+ "_"+str(akey)+str(atom_iterator)+"_"+str(uuid_geometry_atomic_mass)+"_"+str(rnd)),gc_namespace.hasMass,URIRef(ontology_base_uri+"finalization_module_has_mass_"+str(empirical_formula)+ "_"+str(akey)+str(atom_iterator)+"_"+str(uuid_geometry_atomic_mass)+"_"+str(rnd))))
                     ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_mass_"+str(empirical_formula)+ "_"+str(akey)+str(atom_iterator)+"_"+str(uuid_geometry_atomic_mass)+"_"+str(rnd)),RDF.type,gc_namespace.FloatValue))
                     ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_mass_"+str(empirical_formula)+ "_"+str(akey)+str(atom_iterator)+"_"+str(uuid_geometry_atomic_mass)+"_"+str(rnd)),RDF.type,OWL.Thing))
                     ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_mass_"+str(empirical_formula)+ "_"+str(akey)+str(atom_iterator)+"_"+str(uuid_geometry_atomic_mass)+"_"+str(rnd)),gc_namespace.hasValue,Literal(dict_data["Atomic masses"][atom_iterator])))

                 if "Atomic mass unit" in dict_data:
                     atomic_mass_unit = dict_data["Atomic mass unit"]
                     if atomic_mass_unit == "atomic":
                         ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_mass_"+str(empirical_formula)+ "_"+str(akey)+str(atom_iterator)+"_"+str(uuid_geometry_atomic_mass)+"_"+str(rnd)),gc_namespace.hasUnit,URIRef("http://data.nasa.gov/qudt/owl/unit#Dalton")))


                 if "Geometry" in dict_data:
                     print(atom_iterator)
                     for gkey in dict_data["Geometry"][atom_iterator]:
                         '''generate coordinate X'''
                         ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_atom_"+str(empirical_formula)+ "_"+str(akey)+str(atom_iterator)+"_"+str(uuid_geometry_atomic_mass)+"_"+str(rnd)),gc_namespace.hasAtomCoordinateX,URIRef(ontology_base_uri+"finalization_module_has_coordinate_x3_"+str(empirical_formula)+ "_"+str(akey)+str(atom_iterator)+"_"+str(uuid_geometry_atomic_mass)+"_"+str(rnd))))
                         ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_coordinate_x3_"+str(empirical_formula)+ "_"+str(akey)+str(atom_iterator)+"_"+str(uuid_geometry_atomic_mass)+"_"+str(rnd)),RDF.type,gc_namespace.FloatValue))
                         ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_coordinate_x3_"+str(empirical_formula)+ "_"+str(akey)+str(atom_iterator)+"_"+str(uuid_geometry_atomic_mass)+"_"+str(rnd)),RDF.type,OWL.Thing))
                         ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_coordinate_x3_"+str(empirical_formula)+ "_"+str(akey)+str(atom_iterator)+"_"+str(uuid_geometry_atomic_mass)+"_"+str(rnd)),gc_namespace.hasValue,Literal(dict_data["Geometry"][atom_iterator][0])))
                         ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_coordinate_x3_"+str(empirical_formula)+ "_"+str(akey)+str(atom_iterator)+"_"+str(uuid_geometry_atomic_mass)+"_"+str(rnd)),gc_namespace.hasUnit,URIRef("http://data.nasa.gov/qudt/owl/unit#Angstrom")))

                         '''generate coordinate Y'''
                         ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_atom_"+str(empirical_formula)+ "_"+str(akey)+str(atom_iterator)+"_"+str(uuid_geometry_atomic_mass)+"_"+str(rnd)),gc_namespace.hasAtomCoordinateY,URIRef(ontology_base_uri+"finalization_module_has_coordinate_y3_"+str(empirical_formula)+ "_"+str(akey)+str(atom_iterator)+"_"+str(uuid_geometry_atomic_mass)+"_"+str(rnd))))
                         ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_coordinate_y3_"+str(empirical_formula)+ "_"+str(akey)+str(atom_iterator)+"_"+str(uuid_geometry_atomic_mass)+"_"+str(rnd)),RDF.type,gc_namespace.FloatValue))
                         ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_coordinate_y3_"+str(empirical_formula)+ "_"+str(akey)+str(atom_iterator)+"_"+str(uuid_geometry_atomic_mass)+"_"+str(rnd)),RDF.type,OWL.Thing))
                         ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_coordinate_y3_"+str(empirical_formula)+ "_"+str(akey)+str(atom_iterator)+"_"+str(uuid_geometry_atomic_mass)+"_"+str(rnd)),gc_namespace.hasValue,Literal(dict_data["Geometry"][atom_iterator][1])))
                         ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_coordinate_y3_"+str(empirical_formula)+ "_"+str(akey)+str(atom_iterator)+"_"+str(uuid_geometry_atomic_mass)+"_"+str(rnd)),gc_namespace.hasUnit,URIRef("http://data.nasa.gov/qudt/owl/unit#Angstrom")))

                         '''generate coordinate Z'''
                         ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_atom_"+str(empirical_formula)+ "_"+str(akey)+str(atom_iterator)+"_"+str(uuid_geometry_atomic_mass)+"_"+str(rnd)),gc_namespace.hasAtomCoordinateZ,URIRef(ontology_base_uri+"finalization_module_has_coordinate_z3_"+str(empirical_formula)+ "_"+str(akey)+str(atom_iterator)+"_"+str(uuid_geometry_atomic_mass)+"_"+str(rnd))))
                         ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_coordinate_z3_"+str(empirical_formula)+ "_"+str(akey)+str(atom_iterator)+"_"+str(uuid_geometry_atomic_mass)+"_"+str(rnd)),RDF.type,gc_namespace.FloatValue))
                         ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_coordinate_z3_"+str(empirical_formula)+ "_"+str(akey)+str(atom_iterator)+"_"+str(uuid_geometry_atomic_mass)+"_"+str(rnd)),RDF.type,OWL.Thing))
                         ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_coordinate_z3_"+str(empirical_formula)+ "_"+str(akey)+str(atom_iterator)+"_"+str(uuid_geometry_atomic_mass)+"_"+str(rnd)),gc_namespace.hasValue,Literal(dict_data["Geometry"][atom_iterator][2])))
                         ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_has_coordinate_z3_"+str(empirical_formula)+ "_"+str(akey)+str(atom_iterator)+"_"+str(uuid_geometry_atomic_mass)+"_"+str(rnd)),gc_namespace.hasUnit,URIRef("http://data.nasa.gov/qudt/owl/unit#Angstrom")))
                 if atom_iterator < 2:        
                     atom_iterator = atom_iterator +1
                 elif atom_iterator == 2:
                     atom_iterator = 0


    def generate_atom_count(self,ontocompchem_graph, dict_data,ontocompchem_namespace,gc_namespace,ontology_base_uri,file_name,rnd):

         '''generates unique string'''
         uuid_atom_count = uuid.uuid3(uuid.NAMESPACE_DNS,"atom.count")

         '''Generate graph for atom counts quantity'''
        # for i, json_dat in enumerate(self.data):
        #          dict_data = json.loads(json_dat)

         if "Atom counts" in dict_data:
             atom_count = dict_data["Atom counts"]
             empirical_formula = dict_data["Empirical formula"]

             if atom_count is not None:
                 for key, value in atom_count.items():
                     number_of_atoms = Literal(value,datatype=XSD.string)
                     ontocompchem_graph.add((URIRef(ontology_base_uri+file_name), ontocompchem_namespace.hasInitialization, URIRef(ontology_base_uri+"job_module_has_initilization_module_"+str(empirical_formula)+ "_"+str(rnd))))
                     ontocompchem_graph.add((URIRef(ontology_base_uri+"job_module_has_initilization_module_"+str(empirical_formula)+ "_"+str(rnd)), gc_namespace.hasMoleculeProperty, URIRef(ontology_base_uri+"initialization_module_has_molecule_property_"+str(empirical_formula)+ "_"+str(rnd))))
                     ontocompchem_graph.add((URIRef(ontology_base_uri+"initialization_module_has_molecule_property_"+str(empirical_formula)+ "_"+str(rnd)), RDF.type,gc_namespace.MoleculeProperty))
                     ontocompchem_graph.add((URIRef(ontology_base_uri+"initialization_module_has_molecule_property_"+str(empirical_formula)+ "_"+str(rnd)), RDF.type, OWL.Thing))
                     ontocompchem_graph.add((URIRef(ontology_base_uri+"initialization_module_has_molecule_property_"+str(empirical_formula)+ "_"+str(rnd)), gc_namespace.hasMolecule, URIRef(ontology_base_uri+"initialization_module_has_molecule_"+str(empirical_formula)+ "_"+str(key)+str(value)+"_"+str(uuid_atom_count)+"_"+str(rnd))))
                     ontocompchem_graph.add((URIRef(ontology_base_uri+"initialization_module_has_molecule_"+str(empirical_formula)+ "_"+str(key)+str(value)+"_"+str(uuid_atom_count)+"_"+str(rnd)), RDF.type,gc_namespace.Molecule))
                     ontocompchem_graph.add((URIRef(ontology_base_uri+"initialization_module_has_molecule_"+str(empirical_formula)+ "_"+str(key)+str(value)+"_"+str(uuid_atom_count)+"_"+str(rnd)), RDF.type,OWL.Thing))
                     ontocompchem_graph.add((URIRef(ontology_base_uri+"initialization_module_has_molecule_"+str(empirical_formula)+ "_"+str(key)+str(value)+"_"+str(uuid_atom_count)+"_"+str(rnd)), gc_namespace.hasNumberOfAtoms,number_of_atoms))
                     ontocompchem_graph.add((URIRef(ontology_base_uri+"initialization_module_has_molecule_"+str(empirical_formula)+ "_"+str(key)+str(value)+"_"+str(uuid_atom_count)+"_"+str(rnd)), gc_namespace.hasAtom,URIRef(ontology_base_uri+"has_atom_"+str(empirical_formula)+ "_"+str(key)+str(value)+"_"+str(uuid_atom_count)+"_"+str(rnd))))
                     ontocompchem_graph.add((URIRef(ontology_base_uri+"has_atom_"+str(empirical_formula)+ "_"+str(key)+str(value)+"_"+str(uuid_atom_count)+"_"+str(rnd)), RDF.type,gc_namespace.Atom))
                     ontocompchem_graph.add((URIRef(ontology_base_uri+"has_atom_"+str(empirical_formula)+ "_"+str(key)+str(value)+"_"+str(uuid_atom_count)+"_"+str(rnd)), RDF.type,OWL.Thing))
                     ontocompchem_graph.add((URIRef(ontology_base_uri+"has_atom_"+str(empirical_formula)+ "_"+str(key)+str(value)+"_"+str(uuid_atom_count)+"_"+str(rnd)), gc_namespace.isElement,URIRef("http://www.daml.org/2003/01/periodictable/PeriodicTable.owl#"+str(key))))
                     ontocompchem_graph.add((URIRef("http://www.daml.org/2003/01/periodictable/PeriodicTable.owl#"+str(key)),RDF.type,URIRef("http://www.daml.org/2003/01/periodictable/PeriodicTable.owl#Element")))
                     ontocompchem_graph.add((URIRef("http://www.daml.org/2003/01/periodictable/PeriodicTable.owl#"+str(key)),RDF.type,OWL.Thing))

    def generate_electronic_and_zpe_energy(self,ontocompchem_graph, dict_data,ontocompchem_namespace,gc_namespace,ontology_base_uri,file_name,rnd):

         '''generates unique string'''
         uuid_electronic_and_zpe_energy = uuid.uuid3(uuid.NAMESPACE_DNS,"electronic.and.zpe.energy")

         '''Generate graph for electronic and zpe energy quantity'''
        # for i, json_dat in enumerate(self.data):
        #          dict_data = json.loads(json_dat)

         if "Electronic and ZPE energy" in dict_data:
             electronic_and_zpe_energy = dict_data["Electronic and ZPE energy"]
             empirical_formula = dict_data["Empirical formula"]

             if electronic_and_zpe_energy is not None:
                 electronic_and_zpe_energy_literal=Literal(electronic_and_zpe_energy)
                 ontocompchem_graph.add((URIRef(ontology_base_uri+file_name), gc_namespace.isCalculationOn, URIRef(ontology_base_uri+"finalization_module_electronic_and_zpe_energy_"+str(empirical_formula)+ "_"+str(uuid_electronic_and_zpe_energy)+"_"+str(rnd))))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_electronic_and_zpe_energy_"+str(empirical_formula)+ "_"+str(uuid_electronic_and_zpe_energy)+"_"+str(rnd)), RDF.type, ontocompchem_namespace.ElectronicAndZPEEnergy))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_electronic_and_zpe_energy_"+str(empirical_formula)+ "_"+str(uuid_electronic_and_zpe_energy)+"_"+str(rnd)), RDF.type, OWL.Thing))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_electronic_and_zpe_energy_"+str(empirical_formula)+ "_"+str(uuid_electronic_and_zpe_energy)+"_"+str(rnd)), gc_namespace.hasElectronicEnergy, URIRef(ontology_base_uri+"finalization_module_electronic_and_zpe_energy_value_"+str(empirical_formula)+ "_"+str(uuid_electronic_and_zpe_energy)+"_"+str(rnd))))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_electronic_and_zpe_energy_value_"+str(empirical_formula)+ "_"+str(uuid_electronic_and_zpe_energy)+"_"+str(rnd)), RDF.type, gc_namespace.FloatValue))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_electronic_and_zpe_energy_value_"+str(empirical_formula)+ "_"+str(uuid_electronic_and_zpe_energy)+"_"+str(rnd)), RDF.type, OWL.Thing))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_electronic_and_zpe_energy_value_"+str(empirical_formula)+ "_"+str(uuid_electronic_and_zpe_energy)+"_"+str(rnd)), gc_namespace.hasValue, electronic_and_zpe_energy_literal))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_electronic_and_zpe_energy_value_"+str(empirical_formula)+ "_"+str(uuid_electronic_and_zpe_energy)+"_"+str(rnd)),gc_namespace.hasUnit,URIRef("http://data.nasa.gov/qudt/owl/unit#Hartree")))

    def generate_scf_energy(self,ontocompchem_graph, dict_data,ontocompchem_namespace,gc_namespace,ontology_base_uri,file_name,rnd):

        '''generates unique string'''
        uuid_scf_energy = uuid.uuid3(uuid.NAMESPACE_DNS,"scf.energy")

        '''Generate graph for scf energy quantity'''
        #for i, json_dat in enumerate(self.data):
        #          dict_data = json.loads(json_dat)

        if "Electronic energy" in dict_data:
                 scf_energy = dict_data["Electronic energy"]
                 scf_energy_literal=Literal(scf_energy)
                 empirical_formula = dict_data["Empirical formula"]
                 ontocompchem_graph.add((URIRef(ontology_base_uri+file_name), gc_namespace.isCalculationOn, URIRef(ontology_base_uri+"finalization_module_scf_energy_"+str(empirical_formula)+ "_"+str(uuid_scf_energy)+"_"+str(rnd))))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_scf_energy_"+str(empirical_formula)+ "_"+str(uuid_scf_energy)+"_"+str(rnd)), RDF.type, ontocompchem_namespace.ScfEnergy))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_scf_energy_"+str(empirical_formula)+ "_"+str(uuid_scf_energy)+"_"+str(rnd)), RDF.type, OWL.Thing))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_scf_energy_"+str(empirical_formula)+ "_"+str(uuid_scf_energy)+"_"+str(rnd)), gc_namespace.hasElectronicEnergy, URIRef(ontology_base_uri+"finalization_module_scf_energy_value_"+str(empirical_formula)+ "_"+str(uuid_scf_energy)+"_"+str(rnd))))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_scf_energy_value_"+str(empirical_formula)+ "_"+str(uuid_scf_energy)+"_"+str(rnd)), RDF.type, gc_namespace.FloatValue))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_scf_energy_value_"+str(empirical_formula)+ "_"+str(uuid_scf_energy)+"_"+str(rnd)), RDF.type, OWL.Thing))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_scf_energy_value_"+str(empirical_formula)+ "_"+str(uuid_scf_energy)+"_"+str(rnd)), gc_namespace.hasValue, scf_energy_literal))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_scf_energy_value_"+str(empirical_formula)+ "_"+str(uuid_scf_energy)+"_"+str(rnd)),gc_namespace.hasUnit,URIRef("http://data.nasa.gov/qudt/owl/unit#Hartree")))


    def generate_file_iri(self,ontocompchem_graph,  dict_data, ontocompchem_namespace,gc_namespace,ontology_base_uri,source_kb_base_uri,source_data_base_uri,file_name,log_file_name,owl_name,rnd):

        '''Generate graph for output sources ".g09", ".log", ".g16", ".owl" '''
        #for i, json_dat in enumerate(self.data):
        #          dict_data = json.loads(json_dat)
                  
        empirical_formula = dict_data["Empirical formula"]
        ontocompchem_graph.add((URIRef(ontology_base_uri+file_name), ontocompchem_namespace.hasEnvironment,URIRef(ontology_base_uri+"job_module_has_environment_module_"+str(empirical_formula)+ "_"+str(rnd))))
        ontocompchem_graph.add((URIRef(ontology_base_uri+"job_module_has_environment_module_"+str(empirical_formula)+ "_"+str(rnd)), RDF.type, gc_namespace.SourcePackage))
        ontocompchem_graph.add((URIRef(ontology_base_uri+"job_module_has_environment_module_"+str(empirical_formula)+ "_"+str(rnd)), RDF.type, OWL.Thing))

        ontocompchem_graph.add((URIRef(ontology_base_uri+"job_module_has_environment_module_"+str(empirical_formula)+ "_"+str(rnd)), gc_namespace.hasOutputFile, URIRef(source_data_base_uri+ log_file_name)))
        ontocompchem_graph.add((URIRef(source_data_base_uri+ log_file_name), RDF.type, ontocompchem_namespace.OutputSource))
        ontocompchem_graph.add((URIRef(source_data_base_uri+ log_file_name), RDF.type, OWL.Thing))

        ontocompchem_graph.add((URIRef(ontology_base_uri+"job_module_has_environment_module_"+str(empirical_formula)+ "_"+str(rnd)), gc_namespace.hasOutputFile, URIRef(source_kb_base_uri+ owl_name)))#os.path.splitext(self.log)[0]+'.owl'
        ontocompchem_graph.add((URIRef(source_kb_base_uri+ owl_name), RDF.type, ontocompchem_namespace.OutputSource))#os.path.splitext(self.log)[0]+'.owl'
        ontocompchem_graph.add((URIRef(source_kb_base_uri+ owl_name), RDF.type, OWL.Thing))#os.path.splitext(self.log)[0]+'.owl'

    def generate_homo_energy(self,ontocompchem_graph, dict_data,ontocompchem_namespace,gc_namespace,ontology_base_uri,file_name,rnd):

         '''generates unique string'''
         uuid_homo_electronic_energy = uuid.uuid3(uuid.NAMESPACE_DNS,"homo.electronic.energy")

         '''Generate graph for homo electronic energy quantity'''
        # for i, json_dat in enumerate(self.data):
        #          dict_data = json.loads(json_dat)

         if "HOMO energy" in dict_data:
             homo_electronic_energy = dict_data["HOMO energy"]
             empirical_formula = dict_data["Empirical formula"]

             if homo_electronic_energy is not None:
                 homo_electronic_energy_literal=Literal(homo_electronic_energy)
                 ontocompchem_graph.add((URIRef(ontology_base_uri+file_name), gc_namespace.isCalculationOn, URIRef(ontology_base_uri+"finalization_module_homo_electronic_energy_"+str(empirical_formula)+ "_"+str(uuid_homo_electronic_energy)+"_"+str(rnd))))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_homo_electronic_energy_"+str(empirical_formula)+ "_"+str(uuid_homo_electronic_energy)+"_"+str(rnd)), RDF.type, ontocompchem_namespace.HomoEnergy))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_homo_electronic_energy_"+str(empirical_formula)+ "_"+str(uuid_homo_electronic_energy)+"_"+str(rnd)), RDF.type, OWL.Thing))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_homo_electronic_energy_"+str(empirical_formula)+ "_"+str(uuid_homo_electronic_energy)+"_"+str(rnd)), gc_namespace.hasSystemEnergiesResult, URIRef(ontology_base_uri+"finalization_module_homo_electronic_energy_value_"+str(empirical_formula)+ "_"+str(uuid_homo_electronic_energy)+"_"+str(rnd))))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_homo_electronic_energy_value_"+str(empirical_formula)+ "_"+str(uuid_homo_electronic_energy)+"_"+str(rnd)), RDF.type, gc_namespace.FloatValue))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_homo_electronic_energy_value_"+str(empirical_formula)+ "_"+str(uuid_homo_electronic_energy)+"_"+str(rnd)), RDF.type, OWL.Thing))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_homo_electronic_energy_value_"+str(empirical_formula)+ "_"+str(uuid_homo_electronic_energy)+"_"+str(rnd)), gc_namespace.hasValue, homo_electronic_energy_literal))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_homo_electronic_energy_value_"+str(empirical_formula)+ "_"+str(uuid_homo_electronic_energy)+"_"+str(rnd)),gc_namespace.hasUnit,URIRef("http://data.nasa.gov/qudt/owl/unit#Hartree")))

    def generate_homo_minus_one_energy(self,ontocompchem_graph, dict_data,ontocompchem_namespace,gc_namespace,ontology_base_uri,file_name,rnd):

         '''generates unique string'''
         uuid_homo_minus_one_electronic_energy = uuid.uuid3(uuid.NAMESPACE_DNS,"homo.minus.one.electronic.energy")

         '''Generate graph for homo minus one electronic energy quantity'''
        # for i, json_dat in enumerate(self.data):
        #          dict_data = json.loads(json_dat)

         if "HOMO-1 energy" in dict_data:

             homo_minus_one_electronic_energy = dict_data["HOMO-1 energy"]
             empirical_formula = dict_data["Empirical formula"]

             if homo_minus_one_electronic_energy is not None:
                 homo_minus_one_electronic_energy_literal=Literal(homo_minus_one_electronic_energy)
                 ontocompchem_graph.add((URIRef(ontology_base_uri+file_name), gc_namespace.isCalculationOn, URIRef(ontology_base_uri+"finalization_module_homo_minus_one_electronic_energy_"+str(empirical_formula)+ "_"+str(uuid_homo_minus_one_electronic_energy)+"_"+str(rnd))))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_homo_minus_one_electronic_energy_"+str(empirical_formula)+ "_"+str(uuid_homo_minus_one_electronic_energy)+"_"+str(rnd)), RDF.type, ontocompchem_namespace.HomoMinusOneEnergy))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_homo_minus_one_electronic_energy_"+str(empirical_formula)+ "_"+str(uuid_homo_minus_one_electronic_energy)+"_"+str(rnd)), RDF.type, OWL.Thing))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_homo_minus_one_electronic_energy_"+str(empirical_formula)+ "_"+str(uuid_homo_minus_one_electronic_energy)+"_"+str(rnd)), gc_namespace.hasSystemEnergiesResult, URIRef(ontology_base_uri+"finalization_module_homo_minus_one_electronic_energy_value_"+str(empirical_formula)+ "_"+str(uuid_homo_minus_one_electronic_energy)+"_"+str(rnd))))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_homo_minus_one_electronic_energy_value_"+str(empirical_formula)+ "_"+str(uuid_homo_minus_one_electronic_energy)+"_"+str(rnd)), RDF.type, gc_namespace.FloatValue))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_homo_minus_one_electronic_energy_value_"+str(empirical_formula)+ "_"+str(uuid_homo_minus_one_electronic_energy)+"_"+str(rnd)), RDF.type, OWL.Thing))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_homo_minus_one_electronic_energy_value_"+str(empirical_formula)+ "_"+str(uuid_homo_minus_one_electronic_energy)+"_"+str(rnd)), gc_namespace.hasValue, homo_minus_one_electronic_energy_literal))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_homo_minus_one_electronic_energy_value_"+str(empirical_formula)+ "_"+str(uuid_homo_minus_one_electronic_energy)+"_"+str(rnd)),gc_namespace.hasUnit,URIRef("http://data.nasa.gov/qudt/owl/unit#Hartree")))

    def generate_homo_minus_two_energy(self,ontocompchem_graph, dict_data,ontocompchem_namespace,gc_namespace,ontology_base_uri,file_name,rnd):

         '''generates unique string'''
         uuid_homo_minus_two_electronic_energy = uuid.uuid3(uuid.NAMESPACE_DNS,"homo.minus.two.electronic.energy")

         '''Generate graph for homo minus two electronic energy quantity'''
        # for i, json_dat in enumerate(self.data):
        #          dict_data = json.loads(json_dat)

         if "HOMO-2 energy" in dict_data:

             homo_minus_two_electronic_energy = dict_data["HOMO-2 energy"]
             empirical_formula = dict_data["Empirical formula"]

             if homo_minus_two_electronic_energy is not None:
                 homo_minus_two_electronic_energy_literal=Literal(homo_minus_two_electronic_energy)
                 ontocompchem_graph.add((URIRef(ontology_base_uri+file_name), gc_namespace.isCalculationOn, URIRef(ontology_base_uri+"finalization_module_homo_minus_two_electronic_energy_"+str(empirical_formula)+ "_"+str(uuid_homo_minus_two_electronic_energy)+"_"+str(rnd))))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_homo_minus_two_electronic_energy_"+str(empirical_formula)+ "_"+str(uuid_homo_minus_two_electronic_energy)+"_"+str(rnd)), RDF.type, ontocompchem_namespace.HomoMinusTwoEnergy))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_homo_minus_two_electronic_energy_"+str(empirical_formula)+ "_"+str(uuid_homo_minus_two_electronic_energy)+"_"+str(rnd)), RDF.type, OWL.Thing))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_homo_minus_two_electronic_energy_"+str(empirical_formula)+ "_"+str(uuid_homo_minus_two_electronic_energy)+"_"+str(rnd)), gc_namespace.hasSystemEnergiesResult, URIRef(ontology_base_uri+"finalization_module_homo_minus_two_electronic_energy_value_"+str(empirical_formula)+ "_"+str(uuid_homo_minus_two_electronic_energy)+"_"+str(rnd))))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_homo_minus_two_electronic_energy_value_"+str(empirical_formula)+ "_"+str(uuid_homo_minus_two_electronic_energy)+"_"+str(rnd)), RDF.type, gc_namespace.FloatValue))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_homo_minus_two_electronic_energy_value_"+str(empirical_formula)+ "_"+str(uuid_homo_minus_two_electronic_energy)+"_"+str(rnd)), RDF.type, OWL.Thing))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_homo_minus_two_electronic_energy_value_"+str(empirical_formula)+ "_"+str(uuid_homo_minus_two_electronic_energy)+"_"+str(rnd)), gc_namespace.hasValue, homo_minus_two_electronic_energy_literal))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_homo_minus_two_electronic_energy_value_"+str(empirical_formula)+ "_"+str(uuid_homo_minus_two_electronic_energy)+"_"+str(rnd)),gc_namespace.hasUnit,URIRef("http://data.nasa.gov/qudt/owl/unit#Hartree")))


    def generate_lumo_energy(self,ontocompchem_graph, dict_data,ontocompchem_namespace,gc_namespace,ontology_base_uri,file_name,rnd):

         '''generates unique string'''
         uuid_lumo_electronic_energy = uuid.uuid3(uuid.NAMESPACE_DNS,"lumo.electronic.energy")

         '''Generate graph for lumo electronic energy quantity'''
        # for i, json_dat in enumerate(self.data):
        #          dict_data = json.loads(json_dat)

         if "LUMO energy" in dict_data:
             lumo_electronic_energy = dict_data["LUMO energy"]
             empirical_formula = dict_data["Empirical formula"]

             if lumo_electronic_energy is not None:
                 lumo_electronic_energy_literal=Literal(lumo_electronic_energy)
                 ontocompchem_graph.add((URIRef(ontology_base_uri+file_name), gc_namespace.isCalculationOn, URIRef(ontology_base_uri+"finalization_module_lumo_electronic_energy_"+str(empirical_formula)+ "_"+str(uuid_lumo_electronic_energy)+"_"+str(rnd))))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_lumo_electronic_energy_"+str(empirical_formula)+ "_"+str(uuid_lumo_electronic_energy)+"_"+str(rnd)), RDF.type, ontocompchem_namespace.LumoEnergy))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_lumo_electronic_energy_"+str(empirical_formula)+ "_"+str(uuid_lumo_electronic_energy)+"_"+str(rnd)), RDF.type, OWL.Thing))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_lumo_electronic_energy_"+str(empirical_formula)+ "_"+str(uuid_lumo_electronic_energy)+"_"+str(rnd)), gc_namespace.hasSystemEnergiesResult, URIRef(ontology_base_uri+"finalization_module_lumo_electronic_energy_value_"+str(empirical_formula)+ "_"+str(uuid_lumo_electronic_energy)+"_"+str(rnd))))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_lumo_electronic_energy_value_"+str(empirical_formula)+ "_"+str(uuid_lumo_electronic_energy)+"_"+str(rnd)), RDF.type, gc_namespace.FloatValue))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_lumo_electronic_energy_value_"+str(empirical_formula)+ "_"+str(uuid_lumo_electronic_energy)+"_"+str(rnd)), RDF.type, OWL.Thing))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_lumo_electronic_energy_value_"+str(empirical_formula)+ "_"+str(uuid_lumo_electronic_energy)+"_"+str(rnd)), gc_namespace.hasValue, lumo_electronic_energy_literal))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_lumo_electronic_energy_value_"+str(empirical_formula)+ "_"+str(uuid_lumo_electronic_energy)+"_"+str(rnd)),gc_namespace.hasUnit,URIRef("http://data.nasa.gov/qudt/owl/unit#Hartree")))


    def generate_lumo_plus_one_energy(self,ontocompchem_graph, dict_data,ontocompchem_namespace,gc_namespace,ontology_base_uri,file_name,rnd):

         '''generates unique string'''
         uuid_lumo_plus_one_electronic_energy = uuid.uuid3(uuid.NAMESPACE_DNS,"lumo.plus.one.electronic.energy")

         '''Generate graph for lumo plus one electronic energy quantity'''
        # for i, json_dat in enumerate(self.data):
        #          dict_data = json.loads(json_dat)

         if "LUMO+1 energy" in dict_data:
             lumo_plus_one_electronic_energy = dict_data["LUMO+1 energy"]
             empirical_formula = dict_data["Empirical formula"]

             if lumo_plus_one_electronic_energy is not None:
                 lumo_plus_one_electronic_energy_literal=Literal(lumo_plus_one_electronic_energy)
                 ontocompchem_graph.add((URIRef(ontology_base_uri+file_name), gc_namespace.isCalculationOn, URIRef(ontology_base_uri+"finalization_module_lumo_plus_one_electronic_energy_"+str(empirical_formula)+ "_"+str(uuid_lumo_plus_one_electronic_energy)+"_"+str(rnd))))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_lumo_plus_one_electronic_energy_"+str(empirical_formula)+ "_"+str(uuid_lumo_plus_one_electronic_energy)+"_"+str(rnd)), RDF.type, ontocompchem_namespace.LumoPlusOneEnergy))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_lumo_plus_one_electronic_energy_"+str(empirical_formula)+ "_"+str(uuid_lumo_plus_one_electronic_energy)+"_"+str(rnd)), RDF.type, OWL.Thing))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_lumo_plus_one_electronic_energy_"+str(empirical_formula)+ "_"+str(uuid_lumo_plus_one_electronic_energy)+"_"+str(rnd)), gc_namespace.hasSystemEnergiesResult, URIRef(ontology_base_uri+"finalization_module_lumo_plus_one_electronic_energy_value_"+str(empirical_formula)+ "_"+str(uuid_lumo_plus_one_electronic_energy)+"_"+str(rnd))))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_lumo_plus_one_electronic_energy_value_"+str(empirical_formula)+ "_"+str(uuid_lumo_plus_one_electronic_energy)+"_"+str(rnd)), RDF.type, gc_namespace.FloatValue))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_lumo_plus_one_electronic_energy_value_"+str(empirical_formula)+ "_"+str(uuid_lumo_plus_one_electronic_energy)+"_"+str(rnd)), RDF.type, OWL.Thing))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_lumo_plus_one_electronic_energy_value_"+str(empirical_formula)+ "_"+str(uuid_lumo_plus_one_electronic_energy)+"_"+str(rnd)), gc_namespace.hasValue, lumo_plus_one_electronic_energy_literal))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_lumo_plus_one_electronic_energy_value_"+str(empirical_formula)+ "_"+str(uuid_lumo_plus_one_electronic_energy)+"_"+str(rnd)),gc_namespace.hasUnit,URIRef("http://data.nasa.gov/qudt/owl/unit#Hartree")))


    def generate_lumo_plus_two_energy(self,ontocompchem_graph, dict_data,ontocompchem_namespace,gc_namespace,ontology_base_uri,file_name,rnd):

         '''generates unique string'''
         uuid_lumo_plus_two_electronic_energy = uuid.uuid3(uuid.NAMESPACE_DNS,"lumo.plus.two.electronic.energy")

         '''Generate graph for lumo plus two electronic energy quantity'''
        # for i, json_dat in enumerate(self.data):
        #          dict_data = json.loads(json_dat)

         if "LUMO+2 energy" in dict_data:
             lumo_plus_two_electronic_energy = dict_data["LUMO+2 energy"]
             empirical_formula = dict_data["Empirical formula"]

             if lumo_plus_two_electronic_energy is not None:
                 lumo_plus_two_electronic_energy_literal=Literal(lumo_plus_two_electronic_energy)
                 ontocompchem_graph.add((URIRef(ontology_base_uri+file_name), gc_namespace.isCalculationOn, URIRef(ontology_base_uri+"finalization_module_lumo_plus_two_electronic_energy_"+str(empirical_formula)+ "_"+str(uuid_lumo_plus_two_electronic_energy)+"_"+str(rnd))))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_lumo_plus_two_electronic_energy_"+str(empirical_formula)+ "_"+str(uuid_lumo_plus_two_electronic_energy)+"_"+str(rnd)), RDF.type, ontocompchem_namespace.LumoPlusTwoEnergy))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_lumo_plus_two_electronic_energy_"+str(empirical_formula)+ "_"+str(uuid_lumo_plus_two_electronic_energy)+"_"+str(rnd)), RDF.type, OWL.Thing))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_lumo_plus_two_electronic_energy_"+str(empirical_formula)+ "_"+str(uuid_lumo_plus_two_electronic_energy)+"_"+str(rnd)), gc_namespace.hasSystemEnergiesResult, URIRef(ontology_base_uri+"finalization_module_lumo_plus_two_electronic_energy_value_"+str(empirical_formula)+ "_"+str(uuid_lumo_plus_two_electronic_energy)+"_"+str(rnd))))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_lumo_plus_two_electronic_energy_value_"+str(empirical_formula)+ "_"+str(uuid_lumo_plus_two_electronic_energy)+"_"+str(rnd)), RDF.type, gc_namespace.FloatValue))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_lumo_plus_two_electronic_energy_value_"+str(empirical_formula)+ "_"+str(uuid_lumo_plus_two_electronic_energy)+"_"+str(rnd)), RDF.type, OWL.Thing))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_lumo_plus_two_electronic_energy_value_"+str(empirical_formula)+ "_"+str(uuid_lumo_plus_two_electronic_energy)+"_"+str(rnd)), gc_namespace.hasValue, lumo_plus_two_electronic_energy_literal))
                 ontocompchem_graph.add((URIRef(ontology_base_uri+"finalization_module_lumo_plus_two_electronic_energy_value_"+str(empirical_formula)+ "_"+str(uuid_lumo_plus_two_electronic_energy)+"_"+str(rnd)),gc_namespace.hasUnit,URIRef("http://data.nasa.gov/qudt/owl/unit#Hartree")))
















