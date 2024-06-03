import os
import re
import pyderivationagent

class MOPGBU:
    OMPrefix      = f"""PREFIX om:      <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#>
                        PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#> """

    def __init__(self, query_endpoint, update_endpoint, kg_user, kg_password, file_directory):
        self.sparql_client      = pyderivationagent.kg_operations.PySparqlClient(
            query_endpoint      = query_endpoint,
            update_endpoint     = update_endpoint,
            kg_user             = kg_user,
            kg_password         = kg_password
        )
        self.file_directory     = file_directory
        gbus                    = []

    def query_geometry(self):
        """Query for newly instantiated Mops and return the Geometry value to deduce the GBUs"""
        query = f"""
        {self.OMPrefix}
        SELECT ?Geometry
        WHERE {{
        ?MOPID          om:hasAssemblyModel     ?AMID       .
        ?AMID           om:value                ?Geometry   .
        }}
        LIMIT 2
        """
        return self.sparql_client.performQuery(query)

    def deduce_gbu(self, Geometry):
        """Deduce GBU by splitting the Geometry value string into the two parts, 
        e.g. (4-planar)x12(2-bent)x24 => GBU1 = 4-planar, GBU 2 = 2-bent"""
        matches = re.findall(r'\(([^)]+)\)', Geometry)
        if len(matches) == 2:
            return matches[0], matches[1]
        else:
            raise ValueError("The input string does not contain exactly two bracketed parts.")
    
    def check_gbu(self, gbu):
        """Query the old MOPs for the GBU and check if it exists. Error if there is one wrongly identified."""
        query = f"""
        {self.OMPrefix}
        ASK WHERE {{
            ?MOPID om:hasGeneralBuildingUnit "{gbu}" .
        }}
        """
        return self.sparql_client.performQuery(query)

def main():
    SPARQL_QUERY_ENDPOINT       = 'http://localhost:7578/blazegraph/namespace/OntoMOPs'
    SPARQL_UPDATE_ENDPOINT      = 'http://localhost:7578/blazegraph/namespace/OntoMOPs'
    KG_USERNAME                 = 'bg_user'
    KG_PASSWORD                 = 'admin'
    FILE_DIRECTORY              = "../data/MOPGeometryData"

    updater = MOPGBU(
        query_endpoint          = SPARQL_QUERY_ENDPOINT,
        update_endpoint         = SPARQL_UPDATE_ENDPOINT,
        kg_user                 = KG_USERNAME,
        kg_password             = KG_PASSWORD,
        file_directory          = FILE_DIRECTORY
    )
    response                    = updater.query_geometry()

    for i, res in enumerate(response):
        geometry        = res['Geometry']
        gbus            = updater.deduce_gbu(geometry)
        print(f"MOP {i} geometry: {geometry}")
        print(f"GBU1: {gbus[0]}, GBU2: {gbus[1]}")
        # check if CBUs exist:
        for gbu in gbus:
            if updater.check_gbu(gbu):
                print(f"Successfully identified gbu: {gbu}!")
            else:
                print(f"Gbu: {gbu} is not yet in the database check if there is an error!\n")
                raise Exception("Deduced GBU is not in KG! Check the code or KG.")
    
    print("---------- Finished MOP -------------- \n")
    # updater.process_results(response)
    



if __name__ == "__main__":
    main()
