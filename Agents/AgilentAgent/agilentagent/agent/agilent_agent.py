from datetime import datetime
import glob
import time
import os

from pyderivationagent import DerivationAgent
from pyderivationagent import DerivationInputs
from pyderivationagent import DerivationOutputs

from agilentagent.kg_operations import *
from agilentagent.data_model import *
from agilentagent.conf import *

class AgilentAgent(DerivationAgent):
    # TODO consider making __init__ of DerivationAgent to accept **kwargs
    def __init__(self,
        hplc_digital_twin: str,
        hplc_report_periodic_timescale: str,
        hplc_report_container_dir: str,
        hplc_report_file_extension: str,
        **kwargs
    ):
        register = kwargs.pop('register', True)
        super().__init__(**kwargs)
        self.hplc_digital_twin = hplc_digital_twin
        self.hplc_report_periodic_timescale = hplc_report_periodic_timescale
        self.hplc_report_container_dir = hplc_report_container_dir if hplc_report_container_dir.endswith("/") else hplc_report_container_dir + "/"
        self.hplc_report_file_extension = hplc_report_file_extension
        print("---------------------------1----------------------------")


        # Initialise the sparql_client
        self.sparql_client = ChemistryAndRobotsSparqlClient(
            self.kgUrl, self.kgUrl, kg_user=self.kgUser, kg_password=self.kgPassword,
            fs_url=self.fs_url, fs_user=self.fs_user, fs_pwd=self.fs_password
        )
        print("---------------------------2----------------------------")
        print(register)
        

        # TODO think about standardised way of specify if to register?
        if register:
            self.sparql_client.generate_ontoagent_instance(
                self.agentIRI,
                self.agentEndpoint,
                [ONTOREACTION_REACTIONEXPERIMENT, ONTOLAB_CHEMICALSOLUTION],
                [ONTOHPLC_HPLCJOB]
            )
            self.sparql_client.register_agent_with_hardware(self.agentIRI, self.hplc_digital_twin)
            print("---------------------------5----------------------------")

    def process_request_parameters(self, derivation_inputs: DerivationInputs, derivation_outputs: DerivationOutputs):
        # Record the time when the job starts
        start_timestamp = datetime.now().timestamp()

        # Get the ChemicalSolution iri from the agent inputs (derivation_inputs)
        try:
            chemical_solution_iri = derivation_inputs.getIris(ONTOLAB_CHEMICALSOLUTION)[0]
            rxn_exp_iri = derivation_inputs.getIris(ONTOREACTION_REACTIONEXPERIMENT)[0]
        except Exception as e:
            self.logger.error(e)

        hplc_report_detected = None
        while not hplc_report_detected:
            time.sleep(self.hplc_report_periodic_timescale)
            end_timestamp = datetime.now().timestamp()
            hplc_report_detected = self.sparql_client.detect_new_hplc_report(
                self.hplc_digital_twin, start_timestamp, end_timestamp
            )

        # NOTE here we initialise a new g=Graph() to prevent the variable somehow saved in memory of
        # NOTE g in collect_triples_for_hplc_job got used, i.e. previous collected triples not removed
        g = Graph()
        g = self.sparql_client.collect_triples_for_hplc_job(
            rxn_exp_iri, chemical_solution_iri,
            self.hplc_digital_twin, hplc_report_detected, g
        )

        derivation_outputs.addGraph(g)

    def monitor_local_report_folder(self):
        # NOTE assumptions here:
        # (1) all HPLC reports are no longer modified after its creation
        # (2) all new appeared HPLC reports are newly generated from the HPLC job, i.e. ppl won't copy old HPLC reports from other places and put them in the folder
        # (3) only one new HPLC report will be generated after the last check, i.e. the timescale of the period for checking needs to be small enough to make sure only one HPLC job can be done within that period
        # so we can pop out all the files from last check, and upload all the new files

        timestamp_last_check = self.timestamp_check
        files_last_check = self.lst_files_check
        self.get_dict_of_hplc_files()
        # NOTE below assignment is safe as self.lst_files_check is assigned with a new created list so that files_last_check is not affected
        files_this_check = self.lst_files_check
        # Generate the list of files to be uploaded to the KG file server
        files_to_upload = [file for file in files_this_check if file not in files_last_check]

        if len(files_to_upload) > 1:
            raise Exception("More than one HPLC report were generated between %f (last check) and %f (this check) at the local folder (%s) of HPLC <%s>: %s" % (
                timestamp_last_check, self.timestamp_check, self.hplc_report_container_dir, self.hplc_digital_twin, files_to_upload))
        elif len(files_to_upload) < 1:
            self.logger.info("No HPLC report was generated between %f (last check) and %f (this check) at the local folder (%s) of HPLC <%s>." % (
                timestamp_last_check, self.timestamp_check, self.hplc_report_container_dir, self.hplc_digital_twin))
        else:
            # Upload the new generated file to KG file server
            hplc_report_path = files_to_upload[0]
            timestamp_last_modified = self.dct_files_check.get(hplc_report_path)
            if timestamp_last_modified + min(0.5, 0.2*self.hplc_report_periodic_timescale) < timestamp_last_check:
                # NOTE added 0.5 sec or 20% of the self.hplc_report_periodic_timescale (whichever is smaller) for contingency
                # NOTE as sometimes there's a delay in detecting the files
                raise Exception(
                    "HPLC report (%s, last modified at %f) is generated at the local folder (%s) of HPLC <%s> before the last check %f and outside the contingency (%f) but not uploaded to fileserver %s" % (
                    hplc_report_path, timestamp_last_modified, self.hplc_report_container_dir, self.hplc_digital_twin, timestamp_last_check, min(0.5, 0.2*self.hplc_report_periodic_timescale), self.fs_url))
            # Prepare for the local and remote file path if it starts with '/' or '\'
            local_file_path = hplc_report_path
            hplc_report_path = hplc_report_path[1:] if hplc_report_path.startswith('/') else hplc_report_path
            hplc_report_path = hplc_report_path[1:] if hplc_report_path.startswith('\\') else hplc_report_path
            remote_report_subdir = os.path.join(getShortName(self.hplc_digital_twin),hplc_report_path)
            # Upload file to KG file server, inset triples to KG, and get the hplc_report_iri back
            hplc_report_iri = self.sparql_client.upload_raw_hplc_report_to_kg(
                local_file_path=local_file_path,
                timestamp_last_modified=timestamp_last_modified,
                remote_report_subdir=remote_report_subdir,
                hplc_digital_twin=self.hplc_digital_twin
            )
            self.logger.info("Uploaded HPLCReport iri is: <%s>" % (hplc_report_iri))

    def get_list_of_hplc_files(self, log=True):
        self.timestamp_check, self.lst_files_check = datetime.now().timestamp(), glob.glob(
            self.hplc_report_container_dir + "*." + self.hplc_report_file_extension, recursive=True)
        if log:
            self.logger.info(
                """The list of HPLC report files with filename extension (%s) found at local report directory (%s) of HPLC <%s> at timestamp %f: %s""" % (
                    self.hplc_report_file_extension, self.hplc_report_container_dir, self.hplc_digital_twin, self.timestamp_check, str(self.lst_files_check)
                ))

    def get_dict_of_hplc_files(self):
        """Assign values to self.timestamp_check (567), self.lst_files_check (["file_1.xls", "file_1.xls"]) and self.dct_files_check ({"file_1.xls": 123, "file_1.xls": 234})."""
        # NOTE here we actually rely on the fact that the HPLC report files are never modified after its creation
        # so that the last modification time retrieved from os.path.getmtime(one_file) reflects the truth at timestamp returned from self.get_list_of_hplc_files()
        self.get_list_of_hplc_files(log=False)
        self.dct_files_check = {one_file:os.path.getmtime(one_file) for one_file in self.lst_files_check}
        self.logger.info(
            """The dict of HPLC report files with filename extension (%s) found at local report directory (%s) of HPLC <%s> at timestamp %f (in the format of {"report_file_path": last_modified_timestamp}): %s""" % (
                self.hplc_report_file_extension, self.hplc_report_container_dir, self.hplc_digital_twin, self.timestamp_check, str(self.dct_files_check)
            ))

    def start_monitoring_local_report_folder(self):
        """
            This method starts the periodical job to monitor the HPLC local report folder.
        """
        if self.scheduler.app is None:
            self.scheduler.init_app(self.app)

        # NOTE It is assumed that all existing files before the agent got spun up will NOT be uploaded to the KG
        self.logger.info("Checking the files already exist before starting monitoring the new files...")
        self.get_dict_of_hplc_files()

        self.scheduler.add_job(
            id='monitor_local_report_foler',
            func=self.monitor_local_report_folder,
            trigger='interval',
            seconds=self.hplc_report_periodic_timescale
        )
        print("monitoring local report folder job added")

        if not self.scheduler.running:
            self.scheduler.start()
        self.logger.info("Monitor local report folder job is started with a time interval of %d seconds." % (self.hplc_report_periodic_timescale))


# Show an instructional message at the HPLCInputAgent servlet root
def default():
    """
        Instructional message at the app root.
    """
    msg  = "This is an asynchronous agent that capable of monitoring the HPLC local report folder and upload any new generated reports to the KG file server.<BR>"
    msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/134-dev-lab-equipment-digital-twin/Agents/HPLCInputAgent#readme<BR>"
    # TODO change above line to https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/Agents/HPLCInputAgent#readme, before merging back to develop branch
    return msg
