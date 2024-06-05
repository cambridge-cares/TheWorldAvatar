import UpdateKG
class GBUUpdater(UpdateKG.UpdateKG):
    """Child class of UpdateKG with additional methods specific for GBU handling.
    
    -----------
    Inputs: 
        query_endpoint          Endpoint URL for queries    e.g. 'http://localhost:7578/blazegraph/namespace/OntoMOPs'
        update_endpoint         Endpoint URL for updates    e.g. 'http://localhost:7578/blazegraph/namespace/OntoMOPs' (could be different than query endpoint)    
        kg_user                 KG username
        kg_password             KG password"""

    def find_unique_gbu(self):
        """Query: find unique GBUs primary GBU = planarity (geometry) + modularity (#connections)"""
        queryGBUs = f"""
        {self.prefix}
        SELECT DISTINCT ?planarity ?modularity
        WHERE {{
        ?GBUIRI         om:hasPlanarity     	?planarity      ;
                        om:hasModularity 		?modularity 	.
        }}
        """
        return self.sparql_client.performQuery(queryGBUs) 

    def find_gbu_iris(self, gbu):
        """Query all iris associated with existing GBU (ideally one)"""
        planarity, modularity   = gbu['planarity'], gbu['modularity']
        query_gbu_iri = f"""
        {self.prefix}
        SELECT ?GBUIRI 
        WHERE {{
        ?GBUIRI        	om:hasPlanarity         "{planarity}"   ;
                        om:hasModularity 	    "{modularity}" 	.
            }}
        """
        return self.sparql_client.performQuery(query_gbu_iri)

def main():
    SPARQL_QUERY_ENDPOINT       = 'http://localhost:7578/blazegraph/namespace/OntoMOPs'
    SPARQL_UPDATE_ENDPOINT      = 'http://localhost:7578/blazegraph/namespace/OntoMOPs'
    KG_USERNAME                 = 'bg_user'
    KG_PASSWORD                 = 'admin'
    FILE_DIRECTORY              = "../data/MOPGeometryData"
    updater = GBUUpdater(
        query_endpoint          = SPARQL_QUERY_ENDPOINT,
        update_endpoint         = SPARQL_UPDATE_ENDPOINT,
        kg_user                 = KG_USERNAME,
        kg_password             = KG_PASSWORD
    )
    # query all distinct GBUs = modularity + planarity
    responseGBUs                = updater.find_unique_gbu()
    for i, gbu in enumerate(responseGBUs):
        # query all IRIs connected to a certain gbu = modularity + planarity
        iris                        = updater.find_gbu_iris(gbu=gbu)
        print(gbu)
        """
        bent2       = "http://www.theworldavatar.com/kb/ontomops/GenericBuildingUnit_9d9ab0eb-cec0-4dd3-aaa0-0e16d8b154f0_1"
        linear2     = "http://www.theworldavatar.com/kb/ontomops/GenericBuildingUnit_3d71c19a-ab54-4993-8c94-267dcfe41792_1"
        planar3     = "http://www.theworldavatar.com/kb/ontomops/GenericBuildingUnit_038c423f-c7f7-48c7-8701-6441b597e6cd_0"
        pyramidal3  = "http://www.theworldavatar.com/kb/ontomops/GenericBuildingUnit_793984ba-26f8-4770-aca3-e94b07f632f4_0"
        planar4     = "http://www.theworldavatar.com/kb/ontomops/GenericBuildingUnit_09b873db-712e-428b-9a80-82ee22168bcd_0"
        pyramidal4  = "http://www.theworldavatar.com/kb/ontomops/GenericBuildingUnit_038c423f-c7f7-48c7-8701-6441b597e6cd_1"
        pyramidal5  = "http://www.theworldavatar.com/kb/ontomops/GenericBuildingUnit_f664df33-ef4a-44f8-a76f-930234465ea5_0"
        """
        # go through all IRIs
        for j, res in enumerate(iris):
            subjectx                = res["GBUIRI"]
            incoming, outgoing      = updater.find_all_connections(subjectx)
            # save the IR of the first GBU-IRI
             
            if j==0:
                new_subject         = subjectx
                print("--------------------------\n")
                print(f"saveIRI: {new_subject}\n")
                print("--------------------------\n")
            # all other GBU-IRIs are redundant -> delete existing tripples and reconect with the first IRI
            #elif res["GBUIRI"]==bent2 or res["GBUIRI"]==linear2 or res["GBUIRI"]==pyramidal4 or :
             #   continue
            else:
                print(subjectx)
                # reconnect incoming to the i==0
                for k, gbu_inconnection in enumerate(incoming):
                    updater.generate_triple(gbu_inconnection["subject_incoming"],gbu_inconnection["predicate_incoming"], new_subject, literal=False)
                # delete all outgoing and incoming triples
                updater.delete_connections(res["GBUIRI"])
                #for l, gbu_outconnection in enumerate(outgoing):
                    # print(gbu_outconnection)
                    # updater.generate_triple(new_subject, gbu_outconnection["predicate_outgoing"], gbu_outconnection["object_outgoing"], literal=True)


if __name__ == "__main__":
    main()
