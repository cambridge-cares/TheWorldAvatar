import re
import UpdateKG
class GBUUpdater(UpdateKG.UpdateKG):
    """Child class of UpdateKG with additional methods specific for GBU handling.
    
    -----------
    Inputs: 
        query_endpoint          Endpoint URL for queries    e.g. 'http://localhost:7578/blazegraph/namespace/OntoMOPs'
        update_endpoint         Endpoint URL for updates    e.g. 'http://localhost:7578/blazegraph/namespace/OntoMOPs' (could be different than query endpoint)    
        kg_user                 KG username
        kg_password             KG password"""
    # Reconnect: -----------------------------------
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

    def split_geometry(self, geometry) ->str:
        """Deduce GBU by splitting the Geometry value string into the two parts, 
            e.g. (4-planar)x12(2-bent)x24 => GBU1 = 4-planar, GBU 2 = 2-bent"""
        gbu                 = re.findall(r'\(([^)]+)\)', geometry)
        gbu_num             = re.sub(r'x\d+', 'x', geometry)
        return gbu[0], gbu[1], gbu_num[0], gbu_num[1]

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



if __name__ == "__main__":
    main()
