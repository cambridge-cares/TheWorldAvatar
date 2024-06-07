import subprocess
import sys
import os
# Get the parent directory
parent_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), os.pardir))
# Add the parent directory to the system path
sys.path.append(parent_dir)
from updateKG import UpdateKG as KG

def get_paper(doi): 
    print(doi)
    command = [
        "python3", "-m", "PyPaperBot",
        f'--doi={doi}',
        '--dwn-dir=.',
    ]
    # Run the command
    result = subprocess.run(command, capture_output=True, text=True)
    print("Output:", result.stdout)
    print("Errors:", result.stderr)

# initialize KG class
SPARQL_QUERY_ENDPOINT       = 'http://localhost:7578/blazegraph/namespace/OntoMOPs'
SPARQL_UPDATE_ENDPOINT      = 'http://localhost:7578/blazegraph/namespace/OntoMOPs'
KG_USERNAME                 = 'bg_user'
KG_PASSWORD                 = 'admin'
FILE_DIRECTORY              = "../data/MOPGeometryData"

sparql_point = KG.UpdateKG(
    query_endpoint          = SPARQL_QUERY_ENDPOINT,
    update_endpoint         = SPARQL_UPDATE_ENDPOINT,
    kg_user                 = KG_USERNAME,
    kg_password             = KG_PASSWORD
)
where_lit                   = """?Provenance	om:hasReferenceDOI ?DOI     . """
select_variables            = """ DISTINCT  ?DOI"""
literature_dois             = sparql_point.query_triple(where_lit, select_variables)
# go thorugh all the papers and save them:
for literature_doi in literature_dois:
    print(literature_doi["DOI"])
    if literature_doi["DOI"]=="Not in OntoMOPs KG":
        continue
    get_paper(literature_doi["DOI"])

