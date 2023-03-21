from rdflib import RDF

from pyderivationagent.data_model import TIME_HASTIME
from pyderivationagent.data_model import TIME_INTIMEPOSITION
from pyderivationagent.data_model import TIME_NUMERICPOSITION

import hplcagent.tests.conftest as cf

# ----------------------------------------------------------------------------------
# Utility functions
# ----------------------------------------------------------------------------------

def get_hplc_job(
    hplc_digital_twin,
    rxn_exp_iri,
    chemical_solution_iri,
    sparql_client
):
    query = """SELECT ?hplc_job WHERE {?hplc_job ^<%s> <%s>; <%s> <%s>; <%s> <%s>; <%s>/<%s> <%s>.}""" % (
        cf.ONTOHPLC_HASJOB, hplc_digital_twin,
        RDF.type.toPython(), cf.ONTOHPLC_HPLCJOB,
        cf.ONTOHPLC_CHARACTERISES, rxn_exp_iri,
        cf.ONTOHPLC_HASREPORT, cf.ONTOHPLC_GENERATEDFOR, chemical_solution_iri
    )
    response = sparql_client.performQuery(query)
    return [response[i]['hplc_job'] for i in range(len(response))]


def get_derivation_outputs(derivation_iri: str, sparql_client):
    query = """SELECT ?derivation_outputs WHERE{?derivation_outputs <%s> <%s>.}""" % (
        cf.ONTODERIVATION_BELONGSTO, derivation_iri
    )
    response = sparql_client.performQuery(query)
    return [response[i]['derivation_outputs'] for i in range(len(response))]


def get_timestamp(derivation_iri: str, sparql_client):
    query_timestamp = """SELECT ?time WHERE { <%s> <%s>/<%s>/<%s> ?time .}""" % (
        derivation_iri, TIME_HASTIME, TIME_INTIMEPOSITION, TIME_NUMERICPOSITION)
    # the queried results must be converted to int, otherwise it will not be comparable
    return int(sparql_client.performQuery(query_timestamp)[0]['time'])
