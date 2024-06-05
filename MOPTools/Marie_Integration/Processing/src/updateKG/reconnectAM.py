import UpdateKG
class AMUpdater(UpdateKG.UpdateKG):
    """Child class of UpdateKG with additional methods specific for GBU handling."""
    

    # Reconnect: -----------------------------------
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
        wrong_literal   = ["(3-planar)x8(4-pyramidal)x6", "(2-bent)x6(3-planar)x4", "(2-bent)x12(3-planar)x8", "(3-pyramidal)x4(3-planar)x4"]
        correct_literal = ["(4-pyramidal)x6(3-planar)x8", "(3-planar)x4(2-bent)x6", "(3-planar)x8(2-bent)x12", "(3-planar)x4(3-pyramidal)x4"]
        for j, wliteral in enumerate(wrong_literal):
            where_triples   = f"""
            ?MOPIRI 	om:hasAssemblyModel 	?AMIri 		            .
            ?AMIri 		os:value 				"{wliteral}"		. """
            vars            = "?AMIri ?geom"
            response        = self.query_triple(where_triples, vars)
            print(f"Literal number: {j+1}\n")
            for i, MopAm in enumerate(response):
                print(MopAm)
                print(f"MOP number: {i+1}\n")
                # update tripple
                self.generate_triple(MopAm["AMIri"], "http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#value", correct_literal[j], literal=True)
                # delete triple
                self.delete_triple(MopAm["AMIri"], "http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#value", wliteral, literal=True)

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
    # Generate new entries: ----------------------------
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

def main():
    SPARQL_QUERY_ENDPOINT       = 'http://localhost:7578/blazegraph/namespace/OntoMOPs'
    SPARQL_UPDATE_ENDPOINT      = 'http://localhost:7578/blazegraph/namespace/OntoMOPs'
    KG_USERNAME                 = 'bg_user'
    KG_PASSWORD                 = 'admin'
    FILE_DIRECTORY              = "../data/MOPGeometryData"
    updater = AMUpdater(
        query_endpoint          = SPARQL_QUERY_ENDPOINT,
        update_endpoint         = SPARQL_UPDATE_ENDPOINT,
        kg_user                 = KG_USERNAME,
        kg_password             = KG_PASSWORD
    )
    #-------------
    # reconect new MOPs from om:value to os:value
    updater.delete_shape()
    updater.delete_overhead_am()
    #-------------
    # query all distinct


if __name__ == "__main__":
    main()
