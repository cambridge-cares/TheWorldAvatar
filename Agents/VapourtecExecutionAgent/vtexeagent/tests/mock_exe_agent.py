from typing import Tuple
from rdflib import Graph, URIRef
from vtexeagent.agent.execution_agent import VapourtecExecutionAgent
from vtexeagent.kg_operations import ChemistryAndRobotsSparqlClient
from vtexeagent.data_model import *
from pyderivationagent import DerivationInputs
from pyderivationagent import DerivationOutputs

import pkgutil
import random
import uuid
import os

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
DOWNLOADED_DIR = os.path.join(THIS_DIR,'downloaded_files_for_mock')

HPLC_REPORT_XLS_PATH_IN_PKG = 'sample_data/raw_hplc_report_xls.xls'
HPLC_REPORT_TXT_PATH_IN_PKG = 'sample_data/raw_hplc_report_txt.txt'
HPLC_DIGITAL_TWIN__1 = "http://placeholder_hplc_digital_twin/HPLC_1_xls"
HPLC_DIGITAL_TWIN__2 = "http://placeholder_hplc_digital_twin/HPLC_2_txt"


class MockExecutionAgent(VapourtecExecutionAgent):
    def process_request_parameters(self, derivation_inputs: DerivationInputs, derivation_outputs: DerivationOutputs):
        self.sparql_client = ChemistryAndRobotsSparqlClient(
            self.kgUrl, self.kgUpdateUrl, self.kgUser, self.kgPassword, self.fs_url, self.fs_user, self.fs_password
        )

        self._rxn_exp_iri = derivation_inputs.getIris(ONTOREACTION_REACTIONEXPERIMENT)[0]

        # Random choose the type of report to be used for mock testing
        num = random.randint(0, 1)
        report_path_in_pkg = HPLC_REPORT_XLS_PATH_IN_PKG if num == 0 else HPLC_REPORT_TXT_PATH_IN_PKG
        hplc_digital_twin = HPLC_DIGITAL_TWIN__1 if num == 0 else HPLC_DIGITAL_TWIN__2
        extension = DBPEDIA_XLSFILE if num == 0 else DBPEDIA_TXTFILE

        # Upload HPLC report to file server
        local_file_path, timestamp_last_modified = retrieve_hplc_report(report_path_in_pkg)
        hplc_report_iri, g = self.upload_raw_hplc_report_to_fs_y_collect_triples(local_file_path=local_file_path,
            timestamp_last_modified=timestamp_last_modified, hplc_digital_twin=hplc_digital_twin)

        # Make the connection between HPLCReport and ChemicalSolution
        # In normal operation, this should be done as part of Execution Agent
        chemical_solution_iri = "http://placeholder_chemical_solution/ChemicalSolution_" + str(uuid.uuid4())
        self.sparql_client.connect_hplc_report_with_chemical_solution(hplc_report_iri, chemical_solution_iri)

        g.add((URIRef(self._rxn_exp_iri), URIRef(ONTOREACTION_ISASSIGNEDTO), URIRef("http://example.com/blazegraph/namespace/testlab/dummy_lab/VapourtecR4_Dummy")))
        g.add((URIRef(hplc_digital_twin), URIRef(ONTOHPLC_REPORTEXTENSION), URIRef(extension)))
        vial_iri = "http://placeholder_vial/Vial_" + str(uuid.uuid4())
        g.add((URIRef(chemical_solution_iri), URIRef(ONTOVAPOURTEC_FILLS), URIRef(vial_iri)))
        g.add((URIRef(vial_iri), URIRef(ONTOVAPOURTEC_ISFILLEDWITH), URIRef(chemical_solution_iri)))

        derivation_outputs.addGraph(g)

    def upload_raw_hplc_report_to_fs_y_collect_triples(self, local_file_path, timestamp_last_modified, hplc_digital_twin) -> Tuple[str, Graph]:
        try:
            remote_file_path, timestamp_upload = self.sparql_client.uploadFile(local_file_path)
            self.logger.info("HPLC raw report (%s) was uploaded to fileserver <%s> at %f with remote file path at: %s " % (
                    local_file_path, self.fs_url, timestamp_upload, remote_file_path))
        except Exception as e:
            self.logger.error(e, exc_info=True)
            # TODO need to think a way to inform the post proc agent about the failure of uploading the file
            raise Exception("HPLC raw report (%s) upload failed with exception %s" % (local_file_path, str(e.args)))

        hplc_report_iri = initialiseInstanceIRI(getNameSpace(hplc_digital_twin), ONTOHPLC_HPLCREPORT)
        hplc_job_iri = initialiseInstanceIRI(getNameSpace(hplc_digital_twin), ONTOHPLC_HPLCJOB)
        self.logger.info("The initialised HPLCReport IRI is: <%s>; the initialised HPLCJob IRI is: <%s>" % (hplc_report_iri, hplc_job_iri))

        # rxn_exp_iri = self.sparql_client.identify_rxn_exp_when_uploading_hplc_report(hplc_digital_twin, remote_file_path)
        rxn_exp_iri = self._rxn_exp_iri
        self.logger.info("The identified ReactionExperiment for HPLCReport <%s> (remote path: %s) is: <%s>" % (hplc_report_iri, remote_file_path, rxn_exp_iri))

        # TODO
        hplc_method_iri = self.sparql_client.identify_hplc_method_when_uploading_hplc_report()
        self.logger.info("The HPLCReport <%s> (remote path: %s) was generated using HPLCMethod <%s>" % (hplc_report_iri, remote_file_path, hplc_method_iri))

        # update = PREFIX_XSD + """INSERT DATA {<%s> <%s> <%s>.
        #     <%s> a <%s>; <%s> <%s>; <%s> <%s>; <%s> <%s>.
        #     <%s> a <%s>; <%s> <%s>; <%s> "%s"^^xsd:string; <%s> %f; <%s> %f.}""" % (
        #     hplc_digital_twin, ONTOHPLC_HASJOB, hplc_job_iri,
        #     hplc_job_iri, ONTOHPLC_HPLCJOB, ONTOHPLC_CHARACTERISES, rxn_exp_iri, ONTOHPLC_USESMETHOD, hplc_method_iri, ONTOHPLC_HASREPORT, hplc_report_iri,
        #     hplc_report_iri, ONTOHPLC_HPLCREPORT, ONTOHPLC_HASREPORTPATH, remote_file_path, ONTOHPLC_LOCALREPORTFILE, local_file_path,
        #     ONTOHPLC_LASTLOCALMODIFIEDAT, timestamp_last_modified, ONTOHPLC_LASTUPLOADEDAT, timestamp_upload)

        g = Graph()
        g.add((URIRef(hplc_digital_twin), URIRef(ONTOHPLC_HASJOB), URIRef(hplc_job_iri)))
        g.add((URIRef(hplc_job_iri), RDF.type, URIRef(ONTOHPLC_HPLCJOB)))
        g.add((URIRef(hplc_job_iri), URIRef(ONTOHPLC_CHARACTERISES), URIRef(rxn_exp_iri)))
        g.add((URIRef(hplc_job_iri), URIRef(ONTOHPLC_USESMETHOD), URIRef(hplc_method_iri)))
        g.add((URIRef(hplc_job_iri), URIRef(ONTOHPLC_HASREPORT), URIRef(hplc_report_iri)))
        g.add((URIRef(hplc_report_iri), RDF.type, URIRef(ONTOHPLC_HPLCREPORT)))
        g.add((URIRef(hplc_report_iri), URIRef(ONTOHPLC_HASREPORTPATH), Literal(remote_file_path)))
        g.add((URIRef(hplc_report_iri), URIRef(ONTOHPLC_LOCALREPORTFILE), Literal(local_file_path)))
        g.add((URIRef(hplc_report_iri), URIRef(ONTOHPLC_LASTLOCALMODIFIEDAT), Literal(timestamp_last_modified)))
        g.add((URIRef(hplc_report_iri), URIRef(ONTOHPLC_LASTUPLOADEDAT), Literal(timestamp_upload)))
        return hplc_report_iri, g


def generate_random_download_path(filename_extension):
    return os.path.join(DOWNLOADED_DIR,f'{str(uuid.uuid4())}.'+filename_extension)

def retrieve_hplc_report(report_path_in_pkg):
    def _retrieve_hplc_report(report_path_in_pkg):
        if report_path_in_pkg.endswith('.xls'):
            local_file_path = generate_random_download_path('xls')
        elif report_path_in_pkg.endswith('.txt'):
            local_file_path = generate_random_download_path('txt')
        else:
            raise NotImplementedError("Handling HPLC raw report (%s) in the chemistry_and_robots package is NOT yet supported due to its file extension." % 
                report_path_in_pkg)
        data = pkgutil.get_data('chemistry_and_robots', 'resources/'+report_path_in_pkg)
        with open(local_file_path, 'wb') as file_obj:
            file_obj.write(data)
        timestamp_last_modified = os.path.getmtime(local_file_path)

        return local_file_path, timestamp_last_modified
    return _retrieve_hplc_report(report_path_in_pkg)
