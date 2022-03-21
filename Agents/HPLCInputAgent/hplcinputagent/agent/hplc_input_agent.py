from pathlib import Path
from datetime import datetime
from typing import Tuple
import requests
from requests import status_codes
import glob
import os

from pyasyncagent import AsyncAgent, FlaskConfig
from flask import Flask
from hplcinputagent.kg_operations import *
from hplcinputagent.data_model import *
from hplcinputagent.conf import *

class HPLCInputAgent(AsyncAgent):
    # TODO consider making __init__ of AsyncAgent to accept **kwargs
    def __init__(self, hplc_digital_twin: str, hplc_report_periodic_timescale: str, fs_url: str, fs_user: str, fs_pwd: str,
        agent_iri: str, time_interval: int, derivation_instance_base_url: str, kg_url: str, kg_user: str = None, kg_password: str = None, app: Flask = Flask(__name__), flask_config: FlaskConfig = FlaskConfig(), logger_name: str = "dev"
    ):
        super().__init__(agent_iri, time_interval, derivation_instance_base_url, kg_url, kg_user, kg_password, app, flask_config, logger_name)
        self.hplc_digital_twin = hplc_digital_twin
        self.hplc_report_periodic_timescale = hplc_report_periodic_timescale
        self.fs_url = fs_url
        self.fs_user = fs_user
        self.fs_pwd = fs_pwd

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
                timestamp_last_check, self.timestamp_check, self.hplc_dir, self.hplc_digital_twin, files_to_upload))
        elif len(files_to_upload) < 1:
            self.logger.info("No HPLC report was generated between %f (last check) and %f (this check) at the local folder (%s) of HPLC <%s>." % (
                timestamp_last_check, self.timestamp_check, self.hplc_dir, self.hplc_digital_twin))
        else:
            # Upload the new generated file to KG file server
            hplc_report_path = files_to_upload[0]
            timestamp_last_modified = self.dct_files_check.get(hplc_report_path)
            if timestamp_last_modified < timestamp_last_check:
                raise Exception("HPLC report (%s, last modified at %f) is generated at the local folder (%s) of HPLC <%s> before the last check %f but not uploaded to fileserver %s" % (
                    hplc_report_path, timestamp_last_modified, self.hplc_dir, self.hplc_digital_twin, timestamp_last_check, self.fs_url))
            hplc_report_iri = self.sparql_client.upload_raw_hplc_report_to_fs_kg(local_file_path=hplc_report_path,
                timestamp_last_modified=timestamp_last_modified, hplc_digital_twin=self.hplc_digital_twin)
            self.logger.info("Uploaded HPLCReport iri is: <%s>" % (hplc_report_iri))

    def get_list_of_hplc_files(self, log=True):
        self.timestamp_check, self.lst_files_check = datetime.now().timestamp(), glob.glob(self.hplc_dir + "*." + self.file_extension, recursive=True)
        if log:
            self.logger.info(
                """The list of HPLC report files found at local report directory (%s) of HPLC <%s> at timestamp %f: %s""" % (
                    self.hplc_dir, self.hplc_digital_twin, self.timestamp_check, str(self.lst_files_check)
                ))

    def get_dict_of_hplc_files(self):
        """Assign values to self.timestamp_check (567), self.lst_files_check (["file_1.xls", "file_1.xls"]) and self.dct_files_check ({"file_1.xls": 123, "file_1.xls": 234})."""
        # NOTE here we actually rely on the fact that the HPLC report files are never modified after its creation
        # so that the last modification time retrieved from os.path.getmtime(one_file) reflects the truth at timestamp returned from self.get_list_of_hplc_files()
        self.get_list_of_hplc_files(log=False)
        self.dct_files_check = {one_file:os.path.getmtime(one_file) for one_file in self.lst_files_check}
        self.logger.info(
            """The dict of HPLC report files found at local report directory (%s) of HPLC <%s> at timestamp %f (in the format of {"report_file_path": last_modified_timestamp}): %s""" % (
                self.hplc_dir, self.hplc_digital_twin, self.timestamp_check, str(self.dct_files_check)
            ))

    def start_monitoring_local_report_folder(self):
        """
            This method starts the periodical job to monitor the HPLC local report folder.
        """
        self.scheduler.init_app(self.app) # TODO this should be part of AsyncAgent
        self.scheduler.add_job(id='monitor_local_report_foler', func=self.monitor_local_report_folder, trigger='interval', seconds=self.hplc_report_periodic_timescale)

        # Initialise the sparql_client
        self.sparql_client = ChemistryAndRobotsSparqlClient(self.kgUrl, self.kgUrl, fs_url=self.fs_url, fs_user=self.fs_user, fs_pwd=self.fs_pwd)

        # Get the local report folder path of the HPLC
        self.hplc_dir, self.file_extension = self.sparql_client.get_hplc_local_report_folder_path_n_file_extension(self.hplc_digital_twin)

        # NOTE It is assumed that all existing files before the agent got spun up will not be uploaded to the KG
        self.logger.info("Checking the files already exist before starting monitoring the new files...")
        self.get_dict_of_hplc_files()

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
