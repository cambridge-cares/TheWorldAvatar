import UpdateKG
class GBU_num_Updater(UpdateKG.UpdateKG):
    """Child class of UpdateKG with additional methods specific for GBUnum handling."""
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
            where_triples_2             = f"""  <{am_iri}>		    om:hasGenericBuildingUnitNumber 	?GBUnumIRI                 .
                                                ?GBUnumIRI          os:value     	                    "{num_literal}"         . """
            select_variables_2          = """?GBUnumIRI"""
            gbu_num_iris     = self.query_triple(where_triples_2, select_variables_2)
            self.delete_overhead(gbu_num_iris, "GBUnumIRI")
            
def main():
    SPARQL_QUERY_ENDPOINT       = 'http://localhost:7578/blazegraph/namespace/OntoMOPs'
    SPARQL_UPDATE_ENDPOINT      = 'http://localhost:7578/blazegraph/namespace/OntoMOPs'
    KG_USERNAME                 = 'bg_user'
    KG_PASSWORD                 = 'admin'
    updater = GBU_num_Updater(
        query_endpoint          = SPARQL_QUERY_ENDPOINT,
        update_endpoint         = SPARQL_UPDATE_ENDPOINT,
        kg_user                 = KG_USERNAME,
        kg_password             = KG_PASSWORD
    )
    #-------------
    # reconect new MOPs from om:value to os:value
    updater.delete_overhead_gbunum()
    #-------------
    # query all distinct


if __name__ == "__main__":
    main()
