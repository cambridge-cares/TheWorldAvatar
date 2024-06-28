import re
import os
import sys
import uuid
# Define the processing directory path two levels up from the current file
PROCESSING_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), os.pardir, os.pardir))
# Add the processing directory to the system path
sys.path.append(PROCESSING_DIR)

# from Processing.src.kg_operations.update_kg import UpdateKG
from src.kg_operations.update_kg import UpdateKG
from src.config.a_box_updates_config import config_a_box_updates

class Reconnect(UpdateKG):
    # AM         -----------------------------------
    def reconnect_am_values(self):
        """
        Corrects incorrect assembly model predicates in the knowledge graph.
        
        Queries the triples to find the assembly model and its geometry,
        then generates and deletes the triples to correct the data.
        """
        where_triples   = """
        ?MOPIRI 	om:hasAssemblyModel 	?AMIri 		.
        ?AMIri 		om:value 				?geom		."""
        vars            = "?AMIri ?geom"
        response        = self.query_triple(where_triples, vars)
        for i, MopAm in enumerate(response):
            print(MopAm)
            print(f"MOP number: {i}\n")
            self.generate_triple(MopAm["AMIri"], "http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#value", MopAm["geom"], literal=True)
            self.delete_triple(MopAm["AMIri"], "http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#value", MopAm["geom"], literal=True)

    def correct_geometry(self):
        """
        Corrects the geometry literals in the knowledge graph.
        
        Identifies incorrect literals and updates them with the correct literals.
        """

        wrong_literal   = ["(3-pyramidal)x8(4-pyramidal)x6"]#, "(3-planar)x8(4-pyramidal)x6"], "(2-bent)x6(3-planar)x4", "(2-bent)x12(3-planar)x8", "(3-pyramidal)x4(3-planar)x4"]
        correct_literal = ["(4-pyramidal)x6(3-pyramidal)x8"]#, "(4-pyramidal)x6(3-planar)x8"], "(3-planar)x4(2-bent)x6", "(3-planar)x8(2-bent)x12", "(3-planar)x4(3-pyramidal)x4"]
        where_wrong_triples   = f"""
        ?AMIri 		os:value 				"{wrong_literal[0]}"		. """
        where_right_triples   = f"""
        ?AMIri 		os:value 				"{correct_literal[0]}"		. """
        vars                  = "?AMIri"
        response_wrong        = self.query_triple(where_wrong_triples, vars)[0]
        response_correct      = self.query_triple(where_right_triples, vars)[0]
        # update connection
        print(response_correct, response_wrong)
        self.reconnect_delete(response_wrong["AMIri"], response_correct["AMIri"])

    def delete_overhead_am(self):
        """
        Deletes redundant assembly models by keeping only distinct geometries.
        
        Identifies all distinct geometries and removes redundant assembly model connections.
        """
        # find all 18 distinct AMs:
        where_triples                   = """   ?MOPIRI 	om:hasAssemblyModel 	?AMIri 	    .
                                                ?AMIri      os:value     	        ?geom       . """
        select_variables                = """ DISTINCT ?geom"""
        distinct_geometries             = self.query_triple(where_triples, select_variables)
        # loop thorugh AMs and return all IRIs correpsonding to AM:
        for i, geom in enumerate(distinct_geometries):
            # loop through all IRIs with a certain AM and save the first
            geom_literal                = geom["geom"]
            print(geom_literal)
            where_triples_2             = f"""   ?MOPIRI 	om:hasAssemblyModel 	?AMIri 	                .
                                                 ?AMIri      os:value     	        "{geom_literal}"        . """
            select_variables_2          = """?AMIri"""
            am_iris                     = self.query_triple(where_triples_2, select_variables_2)

            self.delete_overhead(am_iris, "AMIri")
            
    def query_geometry(self):
        """
        Queries old MOPs and returns their geometry values to deduce GBUs.
        
        Constructs a SPARQL SELECT query to retrieve the MOPs and their associated geometry information.
        """
        query = f"""
        {self.prefix}
        SELECT ?MOPIRI 
        (GROUP_CONCAT(?modularity;      SEPARATOR=" ")   AS ?modularities)
        (GROUP_CONCAT(?planarity;       SEPARATOR=" ")   AS ?planarities)
        (GROUP_CONCAT(?num;             SEPARATOR=" ")   AS ?numbers)
        (GROUP_CONCAT(DISTINCT ?AMIRI;  SEPARATOR=" ")   AS ?AMIRIs)
        WHERE {{
        ?MOPIRI om:hasAssemblyModel           	?AMIRI     ;
                om:hasMOPFormula            	?MOPFormula .
        ?AMIRI  om:hasGenericBuildingUnit       ?GBUIRI     .
        ?GBUIRI om:hasPlanarity             	?planarity  ;
                om:hasModularity            	?modularity .
        ?GBUNUM om:isNumberOf 				    ?GBUIRI		.
        ?GBUNUM os:value				        ?num		.
                }}
        GROUP BY ?MOPIRI
        """
        return self.sparql_client.performQuery(query)
    
    def check_geometry(self, geometryName: str) -> bool: 
        """
        Checks if a given geometry exists in the knowledge graph.
        
        Args:
            geometry_name (str): The name of the geometry to check.
            
        Returns:
            bool: True if the geometry exists, False otherwise.
        """
        query = f"""
        {self.prefix}
        ASK WHERE {{
            ?AMID os:value "{geometryName}" .
        }}
        """
        return self.sparql_client.performQuery(query)
    
    def deduce_geometry(self):
        """
        Deduces and updates geometry information for each MOP.
        
        Queries the old MOPs, deduces their geometries based on planarity, modularity,
        and numbers, and updates the triples accordingly.
        """
        response                    = self.query_geometry()
        for i, mop in enumerate(response):
            geometry_string         = self.deduce_geometry(mop["planarities"], mop["modularities"], mop["numbers"])
            self.generate_triple(mop["AMIRIs"], 'http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#value', geometry_string, literal=True)
            print("---------- Finished MOP -------------- \n")
    
    def build_geometry(self, planarity, modularity, num):    
        """
        Generates a geometry string based on planarity, modularity, and number.
        
        Args:
            planarity (str):    The planarity value.
            modularity (str):   The modularity value.
            num (str):          The number value.
        
        Returns:
            str:                The constructed geometry string.
        
        Raises:
            Exception:          If the constructed geometry is not found.
        """
        # example: (4-pyramidal)x6(2-bent)x12
        s1          = f"({modularity.split()[0]}-{planarity.split()[0]})x{num.split()[0]}"
        s2          = f"({modularity.split()[1]}-{planarity.split()[1]})x{num.split()[1]}"
        if self.check_geometry(s1+s2):
            return s1+s2
        elif self.check_geometry(s2+s1):
            return s2+s1
        else:
            raise Exception(f"Geometry: {s1+s2} and {s2+s1} not found")
    # CBU           -----------------------------------
    
    def correct_cbu(self):
        """
        Corrects the chemical building unit (CBU) literals in the knowledge graph.
        
        This method identifies wrong and correct CBUs, queries the triples containing 
        the wrong CBUs, and updates them with the correct CBUs.
        """
        #based on chem formula and cut half
        """Removes wrong literals and inserts correct"""
        wrong_cbu               = "(C5NH3)2(CO2)2"#, "(3-planar)x8(4-pyramidal)x6"], "(2-bent)x6(3-planar)x4", "(2-bent)x12(3-planar)x8", "(3-pyramidal)x4(3-planar)x4"]
        correct_cbu             = "(C5H3N)2(CO2)2"#, "(4-pyramidal)x6(3-planar)x8"], "(3-planar)x4(2-bent)x6", "(3-planar)x8(2-bent)x12", "(3-planar)x4(3-pyramidal)x4"]
        where_mop_formula       = f"""  ?MOPIRI         om:hasMOPFormula            ?MOPFormula     ;
                                                        om:hasChemicalBuildingUnit  ?CBU            .
                                        ?CBU            om:hasCBUFormula            "[{wrong_cbu}]"   .   """
        # query all MOPFormulas and CBUIRIs with potentially wrong cbus
        select_mopformula       = "?MOPIRI ?MOPFormula ?CBU "
        response                = self.query_triple(where_mop_formula, select_mopformula)
        print(response)
        # check each potentially wrong candidate and correct if wrong
        for entry in response:
            cbus                = re.findall(r'\[([^\[\]]+)\]', entry["MOPFormula"])
            for cbu in cbus:
                if cbu == correct_cbu:
                    self.generate_triple(entry["MOPIRI"], "http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#hasChemicalBuildingUnit", "http://www.theworldavatar.com/kb/ontomops/ChemicalBuildingUnit_a647f897-95d5-4e35-b292-7383013f9e28_1", False)
                    self.delete_triple(entry["MOPIRI"], "http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#hasChemicalBuildingUnit", entry["CBU"], literal=False)

    def reconnect_cbu(self):
        where_species_charge            = f"""     	?CBUIRI 	os:hasUniqueSpecies 	        ?SpeciesIRI		.
                                                    ?SpeciesIRI os:hasCharge 			        ?ChargeIRI 		;
                                                                os:hasMolecularWeight  			?MWIRI 		    .   """
        select_species_charge           = f"""      ?CBUIRI ?ChargeIRI ?MWIRI"""
        response                        = self.query_triple(where_species_charge, select_species_charge)
        for iris in response:
            id_hash_value               = str(uuid.uuid4())
            # generate entry for charge measure:
            insert_statement            = f"""     	<{iris["ChargeIRI"]}>       <http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue>    	<http://www.ontology-of-units-of-measure.org/resource/om-2/Measure_Charge_{id_hash_value}>	            .
                                                    <{iris["MWIRI"]}>           <http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue>    	<http://www.ontology-of-units-of-measure.org/resource/om-2/Measure_MolecularWeight_{id_hash_value}>     .   """
            self.query_update("", "", insert_statement)

    def delete_overhead_cbu(self):
        # consider using a SPARQL query for this!
        """
        Deletes overhead CBUs from the knowledge graph.
        
        This method queries distinct CBUs and removes redundant entries, keeping only 
        the first occurrence of each unique CBU.
        """
        # Query distinct GBUs:
        where_triples       =   """
                                ?CBU 	os:hasUniqueSpecies ?Species
                                """
        vars                = " DISTINCT ?Species"
        responseCBUs        = self.query_triple(where_triples, vars)
        for i, cbu in enumerate(responseCBUs):
            species         = cbu['Species']
            # query all IRIs connected to a certain gbu = modularity + planarity
            where_cbu       =  f"""
                                    ?CBU 	os:hasUniqueSpecies <{species}> .
                                """
            var_cbu         = "?CBU"
            iris                        = self.query_triple(where_cbu, var_cbu)
            # go through all IRIs and reconnect them with the first occurence
            self.delete_overhead(iris, "CBU")
    
    # GBU           -----------------------------------
    def delete_overhead_gbu(self):
        """
        Deletes overhead GBUs from the knowledge graph.
        
        This method queries distinct GBUs and removes redundant entries, keeping only 
        the first occurrence of each unique GBU.
        """
        # Query distinct GBUs:
        where_triples       =   """
                                ?GBUIRI         om:hasPlanarity     	?planarity      ;
                                                om:hasModularity 		?modularity 	.
                                """
        vars                            = " DISTINCT ?planarity ?modularity"
        responseGBUs                    = self.query_triple(where_triples, vars)
        for i, gbu in enumerate(responseGBUs):
            planarity, modularity       = gbu['planarity'], gbu['modularity']
            # query all IRIs connected to a certain gbu = modularity + planarity
            where_gbu                   =  f"""
                                            ?GBUIRI        	om:hasPlanarity         "{planarity}"   ;
                                                            om:hasModularity 	    "{modularity}" 	.
                                            """
            var_gbu                     = "?GBUIRI"
            iris                        = self.query_triple(where_gbu, var_gbu)
            # go through all IRIs and reconnect them with the first occurence
            self.delete_overhead(iris, "GBUIRI")

    def split_geometry(geometry) ->str:
        """
        Deduce GBU by splitting the Geometry value string into two parts.
        
        Args:
            geometry (str): The geometry value string, e.g. (4-planar)x12(2-bent)x24.
        
        Returns:
            tuple: A tuple containing two parts of the GBU, e.g. ('4-planar', '2-bent').
        """
        pattern             = re.compile(r'\(([^)]+)\)x(\d+)')
        gbu_num             = pattern.findall(geometry)
        return gbu_num[0], gbu_num[1]
    # GBUnum        -----------------------------------

    def query_unique_gbu_num(self):
        """
        Finds all unique combinations of GBU_num and Assembly model ID.
        
        This method queries unique GBU numbers and their associated Assembly models,
        reducing the number of entries while preserving two numbers for each Assembly model.
        
        Returns:
            list: A list of unique combinations of GBU numbers and Assembly model IDs.
        """
        where_triples                   = """       ?AM 			om:hasGenericBuildingUnitNumber 	?GBUnum .
                                                    ?GBUnum 		os:value 			                ?num	. """
        select_variables                = """ DISTINCT  ?AM ?num"""
        return          self.query_triple(where_triples, select_variables)

    def delete_overhead_gbunum(self):
        """
        Deletes overhead GBU numbers from the knowledge graph.
        
        This method finds all distinct GBU numbers and removes redundant entries, 
        keeping only the first occurrence of each unique GBU number.
        """
        # find all 35 distinct GBU_nums:
        distinct_geometries             = self.query_unique_GBUnum()
        # loop thorugh AMs and return all IRIs correpsonding to AM:
        for i, number in enumerate(distinct_geometries):
            # loop through all IRIs with a certain AM and save the first
            num_literal                 = number["num"]
            am_iri                      = number["AM"]
            print(am_iri, num_literal)
            where_triples_2             = f"""  <{am_iri}>		    om:hasGenericBuildingUnitNumber 	?GBUnumIRI                  .
                                                ?GBUnumIRI          os:value     	                    "{num_literal}"             . """
            select_variables_2          = "?GBUnumIRI"
            gbu_num_iris     = self.query_triple(where_triples_2, select_variables_2)
            self.delete_overhead(gbu_num_iris, "GBUnumIRI")

    # Provenance    -----------------------------------

    def query_unique_doi(self):
        """
        Finds all unique DOIs in the knowledge graph.
        
        This method queries for distinct DOIs used as references in the knowledge graph.
        
        Returns:
            list: A list of unique DOIs.
        """
        # 77 unique 
        where_triples                   = """?Provenance	om:hasReferenceDOI ?DOI     . """
        select_variables                = """ DISTINCT  ?DOI"""
        return          self.query_triple(where_triples, select_variables)

    def delete_overhead_provenance(self):
        """
        Deletes overhead provenance entries from the knowledge graph.
        
        This method finds all distinct DOIs and removes redundant entries, keeping 
        only the first occurrence of each unique DOI.
        """
        distinct_doi             = self.query_unique_doi()
        # loop thorugh AMs and return all IRIs correpsonding to AM:
        for i, number in enumerate(distinct_doi):
            # loop through all IRIs with a certain AM and save the first
            doi_literal                 = number["DOI"]
            print(doi_literal)
            where_triples_2             = f"""?Provenance          om:hasReferenceDOI     	                    "{doi_literal}"         . """
            select_variables_2          = """?Provenance"""
            gbu_num_iris                = self.query_triple(where_triples_2, select_variables_2)
            self.delete_overhead(gbu_num_iris, "Provenance")

    def delete_overhead_binding_site(self):
        """
        Deletes overhead binding site entries from the knowledge graph.
        
        This method finds all distinct DOIs and removes redundant entries, keeping 
        only the first occurrence of each unique DOI.
        """
        # find all 35 distinct GBU_nums:
        where_triples                   = f"""  
                                            ?S          om:hasOuterCoordinationNumber   ?OCN        ;
                                                        rdfs:label                      ?label      .
                                            """
        select_variables                = """DISTINCT ?OCN ?label"""
        distinct_sites                  = self.query_triple(where_triples, select_variables)
        # loop thorugh AMs and return all IRIs correpsonding to AM:
        for i, unique_site in enumerate(distinct_sites):
            # loop through all IRIs with a certain AM and save the first
            site_number, label          = unique_site["OCN"], unique_site["label"]
            where_triples_2             = f"""?Subject      om:hasOuterCoordinationNumber   "{site_number}"        ;
                                                            rdfs:label                      "{label}"      . """
            select_variables_2          = """?Subject"""
            site_iris                   = self.query_triple(where_triples_2, select_variables_2)
            print("Binding site IRIs: ", site_iris)
            self.delete_overhead(site_iris, "Subject")

    def delete_overhead_binding_direction(self):
        """
        Deletes overhead binding site entries from the knowledge graph.
        
        This method finds all distinct DOIs and removes redundant entries, keeping 
        only the first occurrence of each unique DOI.
        """
        # loop thorugh AMs and return all IRIs correpsonding to AM:
            # loop through all IRIs with a certain AM and save the first
        where_binding_direction             = f"""?BindingDirection 					rdf:type						<http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#DirectBinding>	. """
        select_binding_direction            = """?BindingDirection"""
        direction_iris                      = self.query_triple(where_binding_direction, select_binding_direction)
        print("Binding site IRIs: ", direction_iris)
        self.delete_overhead(direction_iris, "BindingDirection")

    def reconnect_ontomops_ontospecies(self):
        script_dir = os.path.dirname(os.path.abspath(__file__))
        #   make file path dependent on script location
        a_box_updates_config_mop            = config_a_box_updates(os.path.join(script_dir, "../../OntoMOPConnection.env"))
        a_box_updates_config_os             = config_a_box_updates(os.path.join(script_dir, "../../OntoSpeciesConnection.env"))
        #   instantiate class
        updater_mop                         = UpdateKG(
        query_endpoint                      = a_box_updates_config_mop.SPARQL_QUERY_ENDPOINT,
        update_endpoint                     = a_box_updates_config_mop.SPARQL_UPDATE_ENDPOINT,
        kg_user                             = a_box_updates_config_mop.KG_USERNAME,
        kg_password                         = a_box_updates_config_mop.KG_PASSWORD
                                                        )
        updater_os                          = UpdateKG(
            query_endpoint                  = a_box_updates_config_os.SPARQL_QUERY_ENDPOINT,
            update_endpoint                 = a_box_updates_config_os.SPARQL_UPDATE_ENDPOINT,
            kg_user                         = a_box_updates_config_os.KG_USERNAME,
            kg_password                     = a_box_updates_config_os.KG_PASSWORD
                                                        )
        #   query cbu formulas
        where_cbu                           = """?CBU 	    om:hasCBUFormula    ?CBUFormula    . """
        select_cbu                          = """ distinct ?CBUFormula"""
        cbu_formulas                        = updater_mop.query_triple(where_cbu, select_cbu)
        #   extract dic values
        list_of_lists                       = [[v for v in d.values()] for d in cbu_formulas]
        #   flatten the list of lists and remove [] brackets.
        flatted                             = [c[0].strip('[]') for c in list_of_lists]
        print(flatted)
        for cbu in flatted:
            print(cbu)
            query_species                   = f"""      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
                                                        PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
                                                        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

                                                        SELECT  ?Species 
                                                        WHERE {{
                                                                ?Species a os:Species .
                                                                VALUES ?Text {{ "{cbu}" }}
                                                                ?Species (((os:hasIUPACName|os:hasMolecularFormula|os:hasSMILES)/os:value)|rdfs:label|skos:altLabel) ?Text .
                                                                }}"""
            species_formulas                = updater_os.run_query(query_species)
        print(species_formulas)
        return

def main():
    """
    Main function to initialize and run the knowledge graph update script.
    """
    script_dir = os.path.dirname(os.path.abspath(__file__))
    #   make file path dependent on script location
    a_box_updates_config_mop                = config_a_box_updates(os.path.join(script_dir, "../../OntoMOPConnection.env"))
    #   instantiate class
    updater_mop = Reconnect(
        query_endpoint                      = a_box_updates_config_mop.SPARQL_QUERY_ENDPOINT,
        update_endpoint                     = a_box_updates_config_mop.SPARQL_UPDATE_ENDPOINT,
        kg_user                             = a_box_updates_config_mop.KG_USERNAME,
        kg_password                         = a_box_updates_config_mop.KG_PASSWORD
    )
    updater_mop.reconnect_cbu()


if __name__ == "__main__":
    main()
