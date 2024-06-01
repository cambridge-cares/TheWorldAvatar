import os
from pyderivationagent.kg_operations import PySparqlClient
# make sure to open ssh tunnel: ssh -fNT -L 7578:127.0.0.1:3838 frs30@68.183.227.15
class MOPGeometryUpdater:
    OM      = 'http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#'
    RDF     = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'
    RDFS    = 'http://www.w3.org/2000/01/rdf-schema#'
    
    def __init__(self, query_endpoint, update_endpoint, kg_user, kg_password, file_directory):
        self.sparql_client      = PySparqlClient(
            query_endpoint      = query_endpoint,
            update_endpoint     = update_endpoint,
            kg_user             = kg_user,
            kg_password         = kg_password
        )
        self.file_directory     = file_directory

    def query_ccdc_numbers(self):
        query = f"""
        PREFIX om: <{self.OM}>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX rdfs: <{self.RDFS}>

        SELECT ?MOPIRI ?CCDCNum
        WHERE {{
            ?MOPIRI om:hasCCDCNumber ?CCDCNum
        }}
        LIMIT 10
        """
        return self.sparql_client.performQuery(query)

    def update_geometry(self, mop_iri, ccdc_num):
        geometry_update     = f"{ccdc_num}.xyz"
        update_query        = f"""
        PREFIX om: <{self.OM}>

        INSERT DATA {{
            <{mop_iri}> om:hasGeometry "{geometry_update}" .
        }}
        """
        self.sparql_client.performUpdate(update_query)
        print(f"Triple added: <{mop_iri}> om:hasGeometry \"{geometry_update}\"")

    def delete_geometry(self, response):
        for res in response:
            mop_iri         = res['MOPIRI']
            ccdc_num        = res['CCDCNum']
            delete_query    = f"""
            PREFIX om: <{self.OM}>

            DELETE DATA {{
                <{mop_iri}> om:hasGeometry "{ccdc_num}" .
            }}
            """
            self.sparql_client.performUpdate(delete_query)
            print(f"Triple deleted: <{mop_iri}> om:hasGeometry \"{ccdc_num}\"")

    def process_results(self, response):
        for res in response:
            mop_iri     = res['MOPIRI']
            ccdc_num    = res['CCDCNum']
            file_path   = os.path.join(self.file_directory, f"{ccdc_num}.xyz")
            if os.path.isfile(file_path):
                print(f"File found for CCDC number {ccdc_num}: {file_path}")
                self.update_geometry(mop_iri, ccdc_num)
            else:
                print(f"File not found for CCDC number {ccdc_num}: {file_path}")

def main():
    SPARQL_QUERY_ENDPOINT       = 'http://68.183.227.15:3838/blazegraph/namespace/OntoMOPs'
    SPARQL_UPDATE_ENDPOINT      = 'http://68.183.227.15:3838/blazegraph/namespace/OntoMOPs'
    KG_USERNAME                 = 'bg_user'
    KG_PASSWORD                 = 'admin'
    FILE_DIRECTORY              = "../data/MOPGeometryData"

    updater = MOPGeometryUpdater(
        query_endpoint          = SPARQL_QUERY_ENDPOINT,
        update_endpoint         = SPARQL_UPDATE_ENDPOINT,
        kg_user                 = KG_USERNAME,
        kg_password             = KG_PASSWORD,
        file_directory          = FILE_DIRECTORY
    )
    response                    = updater.query_ccdc_numbers()
    # updater.process_results(response)
    #updater.delete_geometry(response)



if __name__ == "__main__":
    main()
