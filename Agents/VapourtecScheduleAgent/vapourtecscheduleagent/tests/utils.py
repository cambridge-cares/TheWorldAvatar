from rdflib import RDF, Graph
import pkgutil
import os

from pyderivationagent.data_model import TIME_HASTIME
from pyderivationagent.data_model import TIME_INTIMEPOSITION
from pyderivationagent.data_model import TIME_NUMERICPOSITION

import vapourtecscheduleagent.tests.conftest as cf

# ----------------------------------------------------------------------------------
# Utility functions
# ----------------------------------------------------------------------------------

def initialise_triples(generate_random_download_path, sparql_client):
    # Delete all triples before initialising prepared triples
    sparql_client.performUpdate("""DELETE WHERE {?s ?p ?o.}""")

    # Create folder for downloaded files
    if not os.path.exists(cf.DOWNLOADED_DIR):
        os.mkdir(cf.DOWNLOADED_DIR)

	# Upload all relevant example triples provided in the resources folder of 'chemistry_and_robots' package to triple store
    for f in [
		'sample_data/dummy_lab.ttl', 'sample_data/new_exp_data.ttl', 'sample_data/duplicate_ontorxn.ttl', 'sample_data/rxn_data.ttl',
	]:
        data = pkgutil.get_data('chemistry_and_robots', 'resources/'+f).decode("utf-8")
        g = Graph().parse(data=data)
        filePath = generate_random_download_path("ttl")
        g.serialize(filePath, format='ttl')
        sparql_client.uploadOntology(filePath)
        # the serialised files will be deleted at the end of testing session


def get_timestamp(derivation_iri: str, sparql_client):
    query_timestamp = """SELECT ?time WHERE { <%s> <%s>/<%s>/<%s> ?time .}""" % (
        derivation_iri, TIME_HASTIME, TIME_INTIMEPOSITION, TIME_NUMERICPOSITION)
    # the queried results must be converted to int, otherwise it will not be comparable
    return int(sparql_client.performQuery(query_timestamp)[0]['time'])

def get_hplc_derivation(rxn_exp_iri: str, sparql_client):
    query = """SELECT ?hplc_derivation WHERE {?hplc_derivation <%s> <%s>; <%s> ?hplc_agent. ?hplc_agent ^<%s>/a <%s>.}""" % (
        cf.ONTODERIVATION_ISDERIVEDFROM, rxn_exp_iri, cf.ONTODERIVATION_ISDERIVEDUSING, cf.ONTOLAB_ISMANAGEDBY, cf.ONTOHPLC_HIGHPERFORMANCELIQUIDCHROMATOGRAPHY
    )
    response = sparql_client.performQuery(query)
    return response[0]['hplc_derivation'] if len(response) > 0 else None

def get_vapourtec_derivation(rxn_exp_iri: str, sparql_client):
    query = """SELECT ?vapourtec_derivation WHERE {?vapourtec_derivation <%s> <%s>; <%s> ?vapourtec_agent. ?vapourtec_agent ^<%s>/a <%s>.}""" % (
        cf.ONTODERIVATION_ISDERIVEDFROM, rxn_exp_iri, cf.ONTODERIVATION_ISDERIVEDUSING, cf.ONTOLAB_ISMANAGEDBY, cf.ONTOVAPOURTEC_VAPOURTECRS400
    )
    response = sparql_client.performQuery(query)
    return response[0]['vapourtec_derivation'] if len(response) > 0 else None

def get_derivation_outputs(derivation_iri: str, sparql_client):
    query = """SELECT ?derivation_outputs WHERE {?derivation_outputs <%s> <%s>.}""" % (cf.ONTODERIVATION_BELONGSTO, derivation_iri)
    response = sparql_client.performQuery(query)
    return [r['derivation_outputs'] for r in response]

def if_hplc_derivation_is_in_progress(derivation_iri: str, sparql_client):
    query = """SELECT ?status_type WHERE {<%s> <%s>/a ?status_type.}""" % (derivation_iri, cf.ONTODERIVATION_HASSTATUS)
    response = sparql_client.performQuery(query)
    status_type = response[0]['status_type'] if len(response) > 0 else None
    if status_type == cf.ONTODERIVATION_INPROGRESS:
        return True
    return False

def get_vapourtec_input_file_iri(derivation_iri: str, sparql_client):
    query = """SELECT ?vapourtec_input_file WHERE {?vapourtec_input_file <%s> <%s>; a <%s>.}""" % (
        cf.ONTODERIVATION_BELONGSTO, derivation_iri, cf.ONTOVAPOURTEC_VAPOURTECINPUTFILE
    )
    return sparql_client.performQuery(query)[0]['vapourtec_input_file']


def get_chemical_solution_iri(derivation_iri: str, sparql_client):
    query = """SELECT ?chemical_solution WHERE {?chemical_solution <%s> <%s>; a <%s>.}""" % (
        cf.ONTODERIVATION_BELONGSTO, derivation_iri, cf.ONTOLAB_CHEMICALSOLUTION
    )
    return sparql_client.performQuery(query)[0]['chemical_solution']

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

def get_hplc_report_of_hplc_job(hplc_job_iri: str, sparql_client):
    query = """SELECT ?hplc_report WHERE {<%s> <%s> ?hplc_report.}""" % (hplc_job_iri, cf.ONTOHPLC_HASREPORT)
    response = sparql_client.performQuery(query)
    return [response[i]['hplc_report'] for i in range(len(response))]
