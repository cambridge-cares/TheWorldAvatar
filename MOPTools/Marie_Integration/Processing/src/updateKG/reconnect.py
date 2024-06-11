import UpdateKG
import re
class Reconnect(UpdateKG.UpdateKG):
    """Child class of UpdateKG with additional methods specific for GBU handling."""
    

    # AM         -----------------------------------
    
    def reconnect_values(self):
        """Removes all wrong predicates and inserts the correct."""
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
        """Removes wrong literals and inserts correct"""
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

    def reconnect_missing(self):
        where_stat              =   f"""{{
                                    ?MOPIRI 	rdf:type <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#MetalOrganicPolyhedra> .
                                    MINUS {{?MOPIRI om:hasAssemblyModel ?AMIri .}}
  		                            }}
                                    """
        Insert                  = """?MOPIRI        om:hasAssemblyModel     <http://www.theworldavatar.com/kb/ontomops/AssemblyModel_038c423f-c7f7-48c7-8701-6441b597e6cd>"""
        select_val              =   "?MOPIRI"
        missing_connections     = self.query_triple(where_stat, select_val)

    def delete_overhead_am(self):
        # find all 18 distinct AMs:
        where_triples           = """   ?MOPIRI 	om:hasAssemblyModel 	?AMIri 	    .
                                        ?AMIri      os:value     	        ?geom       . """
        select_variables        = """ DISTINCT ?geom"""
        distinct_geometries     = self.query_triple(where_triples, select_variables)
        # loop thorugh AMs and return all IRIs correpsonding to AM:
        for i, geom in enumerate(distinct_geometries):
            # loop through all IRIs with a certain AM and save the first
            geom_literal                = geom["geom"]
            print(geom_literal)
            where_triples_2             = f"""   ?MOPIRI 	om:hasAssemblyModel 	?AMIri 	                .
                                                 ?AMIri      os:value     	        "{geom_literal}"        . """
            select_variables_2          = """?AMIri"""
            am_iris     = self.query_triple(where_triples_2, select_variables_2)

            self.delete_overhead(am_iris, "AMIri")
            
    def delete_shape(self):
        delete_shape       = f"""
        {self.prefix}
        DELETE WHERE {{
        ?AMIri 		om:hasPolyhedralShape   ?Shape      .
        ?Shape  	?pred                   ?obje       .
        }}
        """
        self.sparql_client.performUpdate(delete_shape)
        return
    
    def query_geometry(self):
        """Query for old Mops and return the Geometry value to deduce the GBUs"""

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
    
    def check_geometry(self, geometryName):
        """Query the old MOPs for the CBU and check if it exists."""
        query = f"""
        {self.prefix}
        ASK WHERE {{
            ?AMID os:value "{geometryName}" .
        }}
        """
        return self.sparql_client.performQuery(query)
    
    def deduce_geometry(self):
        response                    = self.query_geometry()
        for i, mop in enumerate(response):
            print({mop["planarities"]}, {mop["modularities"]}, {mop["numbers"]}, "\n")
            print(f"MOP {i+1}   \n")
            geometry_string         = self.deduce_geometry(mop["planarities"], mop["modularities"], mop["numbers"])
            print(geometry_string)
            print(mop["AMIRIs"])
            self.generate_triple(mop["AMIRIs"], 'http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#value', geometry_string, literal=True)
            print("---------- Finished MOP -------------- \n")
    
    def build_geometry(self, planarity, modularity, num):    
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



    def delete_spacer_core(self):
        delete_spacer_query       = f"""
        {self.prefix}
        DELETE WHERE {{
        ?CBU            om:hasSpacer            ?Spacer             .
        ?Spacer         ?SpacerPredicate        ?SpacerObject       .
        ?Spacer         om:hasSubstituent       ?Substituent        .
        ?Substituent    ?SubstituentPredicate   ?SubstituentObject  . 
        }}
        """
        delete_core_query       = f"""
        {self.prefix}
        DELETE WHERE {{
        ?CBU            om:hasCore              ?Core               .
        ?Core           ?CorePredicate          ?CoreObject         .
        ?Core           om:hasSubstituent       ?Substituent        .
        ?Substituent    ?SubstituentPredicate   ?SubstituentObject  . 
        }}
        """
        self.sparql_client.performUpdate(delete_core_query)
        self.sparql_client.performUpdate(delete_spacer_query)

    def delete_overhead_cbu(self):
        # Query distinct GBUs:
        where_triples       =   """
                                ?CBU 	os:hasUniqueSpecies ?Species
                                """
        vars                = " DISTINCT ?Species"
        responseCBUs        = self.query_triple(where_triples, vars)
        for i, cbu in enumerate(responseCBUs):
            species   = cbu['Species']
            # query all IRIs connected to a certain gbu = modularity + planarity
            where_cbu       =  f"""
                                    ?CBU 	os:hasUniqueSpecies <{species}> .
                                """
            var_cbu         = "?CBU"
            iris                        = self.query_triple(where_cbu, var_cbu)
            # go through all IRIs and reconnect them with the first occurence
            self.delete_overhead(iris, "CBU")
    
    # GBU           -----------------------------------

    def delete_overhead_GBU(self):
            # Query distinct GBUs:
            where_triples       =   """
                                    ?GBUIRI         om:hasPlanarity     	?planarity      ;
                                                    om:hasModularity 		?modularity 	.
                                    """
            vars                = " DISTINCT ?planarity ?modularity"
            responseGBUs        = self.query_triple(where_triples, vars)
            for i, gbu in enumerate(responseGBUs):
                planarity, modularity   = gbu['planarity'], gbu['modularity']
                # query all IRIs connected to a certain gbu = modularity + planarity
                where_gbu       =  f"""
                                    ?GBUIRI        	om:hasPlanarity         "{planarity}"   ;
                                                    om:hasModularity 	    "{modularity}" 	.
                                    """
                var_gbu         = "?GBUIRI"
                iris                        = self.query_triple(where_gbu, var_gbu)
                # go through all IRIs and reconnect them with the first occurence
                self.delete_overhead(iris, "GBUIRI")

    def split_geometry(geometry) ->str:
        """Deduce GBU by splitting the Geometry value string into the two parts, 
            e.g. (4-planar)x12(2-bent)x24 => GBU1 = 4-planar, GBU 2 = 2-bent"""
        pattern             = re.compile(r'\(([^)]+)\)x(\d+)')
        gbu_num             = pattern.findall(geometry)
        return gbu_num[0], gbu_num[1]

    # GBUnum        -----------------------------------

    def query_unique_GBUnum(self):
        """find all 36 unique combinations of GBU_num and Assembly model ID. Unique numbers would result in less entries,
        but ideally we want to preserve 2 numbers for each of the 18 AMs to make it easier to identify the geometry. """
        where_triples                   = """       ?AM 			om:hasGenericBuildingUnitNumber 	?GBUnum .
                                                    ?GBUnum 		os:value 			                ?num	. """
        select_variables                = """ DISTINCT  ?AM ?num"""
        return          self.query_triple(where_triples, select_variables)

    def delete_overhead_gbunum(self):
        """"""
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
        # 77 unique 
        where_triples                   = """?Provenance	om:hasReferenceDOI ?DOI     . """
        select_variables                = """ DISTINCT  ?DOI"""
        return          self.query_triple(where_triples, select_variables)

    def delete_overhead_provenance(self):
        """"""
        # find all 35 distinct GBU_nums:
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

def main():
    SPARQL_QUERY_ENDPOINT       = 'http://localhost:7578/blazegraph/namespace/OntoMOPs'
    SPARQL_UPDATE_ENDPOINT      = 'http://localhost:7578/blazegraph/namespace/OntoMOPs'
    KG_USERNAME                 = 'bg_user'
    KG_PASSWORD                 = 'admin'
    FILE_DIRECTORY              = "../data/MOPGeometryData"
    updater = Reconnect(
        query_endpoint          = SPARQL_QUERY_ENDPOINT,
        update_endpoint         = SPARQL_UPDATE_ENDPOINT,
        kg_user                 = KG_USERNAME,
        kg_password             = KG_PASSWORD
    )
    


if __name__ == "__main__":
    main()
