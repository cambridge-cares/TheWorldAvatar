from datetime import datetime

from pyderivationagent import DerivationAgent

from vapourtecagent.kg_operations import *
from vapourtecagent.data_model import *

import clr
clr.AddReference("FCRemote")
from FCRemote import FlowCommander

clr.AddReference("FCRemoteCSV")
from FCRemoteCSV import CSVParser

import requests

class VapourtecAgent(DerivationAgent):
    def __init__(
        self,
        vapourtec_digital_twin: str,
        vapourtec_state_periodic_timescale: int = 60,
        vapourtec_ip_address: str = "127.0.0.1",
        **kwargs
    ):
        super().__init__(**kwargs)
        self.vapourtec_digital_twin = vapourtec_digital_twin
        self.vapourtec_state_periodic_timescale = vapourtec_state_periodic_timescale
        self.vapourtec_ip_address = vapourtec_ip_address

        # Connect to FlowCommander instance opened at the given IP address
        self.fc = FlowCommander.Connect(self.vapourtec_ip_address)
        self.logger.info("Connected to FlowCommander instance at IP address: %s" % (self.vapourtec_ip_address))


    def monitor_vapourtec_rs400_state(self):
        print("=======================")
        try:
            # Connect to FlowCommander instance opened at the given IP address
            timestamp, fcstate = datetime.now().timestamp(), FlowCommander.GetState(self.fc)
            # TODO update KG with triples:
            # <VapourtecRS400_lab_1> <saref:hasState> <state_1>.
            # <state_1> <rdf:type> <ontovapourtec:Idle>.
            # <state_1> <ontovapourtec:recordedAtTimestamp> timestamp.
            # TODO maybe move the connection bit to vapourtec.py?
            self.logger.info("FlowCommander instance at IP address (%s) has state <%s> at timestamp %s" %(
                self.vapourtec_ip_address, str(fcstate), str(timestamp)
                ))
        except Exception as e:
            self.logger.error(e, stack_info=True, exc_info=True)
            try:
                self.fc = FlowCommander.Connect(self.vapourtec_ip_address)
            except Exception as e1:
                self.logger.error(e1, stack_info=True, exc_info=True)
        print("=======================")


    def start_monitoring_vapourtec_rs400_state(self):
        """
            This method starts the periodical job to monitor the state of VapourtecRS400 module.
        """
        if self.scheduler.app is None:
            self.scheduler.init_app(self.app)

        # Add state monitoring job to scheduler
        self.scheduler.add_job(
            id='monitor_vapourtec_rs400_state',
            func=self.monitor_vapourtec_rs400_state,
            trigger='interval',
            seconds=self.vapourtec_state_periodic_timescale
        )

        # Start the scheduler if it's not already started
        if not self.scheduler.running:
            self.scheduler.start()
        self.logger.info("Monitor VapourtecRS400 state job is started with a time interval of %d seconds." % (
            self.vapourtec_state_periodic_timescale)
        )


# Show an instructional message at the VapourtecAgent servlet root
def default():
    """
        Instructional message at the app root.
    """
    msg  = "This is an asynchronous agent that capable of monitoring the state of VapourtecRS400 module and keeping its digital twin in sync with its physical counterpart.<BR>"
    msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/134-dev-lab-equipment-digital-twin/Agents/VapourtecAgent#readme<BR>"
    # TODO change above line to https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Agents/VapourtecAgent#readme, before merging back to develop branch
    return msg
