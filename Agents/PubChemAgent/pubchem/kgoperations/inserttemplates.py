from pubchem.kgoperations.queryendpoints import SPARQL_ENDPOINTS
from pubchem.kgoperations.querykg import querykg
import json


def test_data_insert():
    insert = """
    PREFIX dc: <http://purl.org/dc/elements/1.1/>
    INSERT DATA
    { 
      <http://example/book10> dc:title "CoMo Book" ;
                             dc:creator "A.Naseri" .
    }
    """
    return insert