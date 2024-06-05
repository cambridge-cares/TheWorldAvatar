import UpdateKG
class AMUpdater(UpdateKG.UpdateKG):
    """Child class of UpdateKG with additional methods specific for GBU handling."""

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
            
            """
            for j, iri in enumerate(am_iris):
                # store in variable to reduce dictionairy calls:
                subjectx                = iri["AMIri"]
                incoming, outgoing      = self.find_all_connections(subjectx)
                # save the IR of the first GBU-IRI
                if j == 0:
                        print("-------------------------\n")
                        new_subject        = iri["AMIri"]
                        print(print(new_subject))
                        print("-------------------------\n")
                # all other GBU-IRIs are redundant -> delete existing tripples and reconect with the first IRI
                else:

                    # reconnect incoming to the i==0
                    for k, am_inconnection in enumerate(incoming):
                        self.generate_triple(am_inconnection["subject_incoming"],am_inconnection["predicate_incoming"], new_subject, literal=False)
                    for l, am_outconnection in enumerate(outgoing):
                        self.generate_triple(new_subject, am_outconnection["predicate_outgoing"], am_outconnection["object_outgoing"], literal=False)
                    # delete all outgoing and incoming triples
                    self.delete_connections(iri["AMIri"])
                    """
                    

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
