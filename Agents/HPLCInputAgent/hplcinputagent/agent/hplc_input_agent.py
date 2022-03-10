from pathlib import Path
from datetime import datetime
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
    def __init__(self, hplc_digital_twin: str, hplc_report_periodic_timescale: str, file_server_upload: str,
        agent_iri: str, time_interval: int, derivation_instance_base_url: str, kg_url: str, kg_user: str = None, kg_password: str = None, app: Flask = Flask(__name__), flask_config: FlaskConfig = FlaskConfig(), logger_name: str = "dev"
    ):
        self.hplc_digital_twin = hplc_digital_twin
        self.hplc_report_periodic_timescale = hplc_report_periodic_timescale
        self.file_server_upload = file_server_upload
        super().__init__(agent_iri, time_interval, derivation_instance_base_url, kg_url, kg_user, kg_password, app, flask_config, logger_name)

    def monitor_local_report_folder(self):
        self.sparql_client = ChemistryAndRobotsSparqlClient(self.kgUrl, self.kgUrl)

        # Get the local report folder path of the HPLC
        hplc_dir, file_extension = self.sparql_client.get_hplc_local_report_folder_path(self.hplc_digital_twin)
        if file_extension == DBPEDIA_TXTFILE:
            f_e = TXTFILE_EXTENSION
        elif file_extension == DBPEDIA_XLSFILE:
            f_e = XLSFILE_EXTENSION
        else:
            raise NotImplementedError("HPLC report with (%s) as filename extension is NOT supported yet." % (file_extension))

        # Searches hplc directory for new generated reports
        # NOTE "\**\*." should be used when deploying it on windows machine, "/**/*." is currently used for testing purpose
        hplc_files = glob.glob(hplc_dir + "/**/*." + f_e, recursive=True)

        # Loop through to get the time when the HPLC report was generated
        hplc_times = [os.path.getmtime(one_file) for one_file in hplc_files]

        # Upload file to KG file server if the latest report was generated in the last period
        last_report_time = max(hplc_times)
        currt_time = datetime.now().timestamp()
        if currt_time - last_report_time < self.hplc_report_periodic_timescale:
            # Upload the new generated file to KG file server
            hplc_report_path = hplc_files[hplc_times.index(last_report_time)]
            with open(hplc_report_path, 'rb') as file_obj:
                files = {'file': file_obj}
                # TODO parameterise authorisation
                response = requests.post(self.file_server_upload, auth=('fs_user', 'fs_pass'), files=files)

                # If the upload succeeded, write the remote file path to KG
                if (response.status_code == status_codes.codes.OK):
                    remote_file_path = response.headers['file']
                    # TODO maybe also write when the report was generated and when uploaded?
                    hplc_report_instance = HPLCReport(instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,namespace_for_init=getNameSpace(self.hplc_digital_twin),hasReportPath=remote_file_path)
                    self.sparql_client.write_hplc_report_path_to_kg(hplc_report_instance)
                else:
                    # TODO need to think a way to inform the post proc agent about the failure of uploading the file
                    raise Exception("File %s upload failed with code %d" % (hplc_report_path, response.status_code))

    def start_monitoring_local_report_folder(self):
        """
            This method starts the periodical job to monitor the HPLC local report folder.
        """
        self.scheduler.init_app(self.app)
        self.scheduler.add_job(id='monitor_local_report_foler', func=self.monitor_local_report_folder, trigger='interval', seconds=self.hplc_report_periodic_timescale)
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
