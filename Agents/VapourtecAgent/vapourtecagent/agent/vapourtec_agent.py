from datetime import datetime
import json
import random
import time
import os

from pyderivationagent import DerivationAgent
from pyderivationagent import DerivationInputs
from pyderivationagent import DerivationOutputs

import vapourtecagent.hardware.vapourtec as vapourtec

from vapourtecagent.kg_operations import *
from vapourtecagent.data_model import *

import clr
clr.AddReference("FCRemote")
from FCRemote import FlowCommander

clr.AddReference("FCRemoteCSV")
from FCRemoteCSV import CSVParser


class VapourtecAgent(DerivationAgent):
    def __init__(
        self,
        vapourtec_digital_twin: str,
        vapourtec_state_periodic_timescale: int = 60,
        vapourtec_ip_address: str = "127.0.0.1",
        fcexp_file_container_folder: str = "/app/fcexp/",
        fcexp_file_host_folder: str = "D:\Vapourtec\FCEXP",
        fcexp_template_filename: str = "ReactionTemplate.fcexp",
        dry_run: bool = True,
        **kwargs
    ):
        super().__init__(**kwargs)

        self.vapourtec_digital_twin = vapourtec_digital_twin
        self.vapourtec_state_periodic_timescale = vapourtec_state_periodic_timescale
        self.vapourtec_ip_address = vapourtec_ip_address
        self.fcexp_file_container_folder = fcexp_file_container_folder
        self.fcexp_file_host_folder = fcexp_file_host_folder
        self.fcexp_template_filename = fcexp_template_filename
        self.dry_run = dry_run

        self.fc_exe_connected = False
        self.vapourtec_state = ONTOVAPOURTEC_NULL

        # Initialise the sparql_client
        self.sparql_client = self.get_sparql_client(ChemistryAndRobotsSparqlClient)

        # Register the agent with vapourtec hardware
        if self.register_agent:
            try:
                self.sparql_client.register_agent_with_hardware(self.agentIRI, self.vapourtec_digital_twin)
            except Exception as e:
                self.logger.error(e, stack_info=True, exc_info=True)
                raise Exception("Agent <%s> registration with hardware <%s> failed." % (self.agentIRI, self.vapourtec_digital_twin))


    def agent_input_concepts(self) -> list:
        return [ONTOREACTION_REACTIONEXPERIMENT]

    def agent_output_concepts(self) -> list:
        return [ONTOLAB_EQUIPMENTSETTINGS, ONTOVAPOURTEC_VAPOURTECINPUTFILE, ONTOLAB_CHEMICALSOLUTION]

    def validate_inputs(self, http_request) -> bool:
        return super().validate_inputs(http_request)

    def connect_flowcommander_exe(self):
        # Connect to FlowCommander instance opened at the given IP address
        if not self.fc_exe_connected:
            self.fc = FlowCommander.Connect(self.vapourtec_ip_address)
            self.fc_exe_connected = True
            self.logger.info("Connected to FlowCommander instance at IP address: %s" % (self.vapourtec_ip_address))
        else:
            self.logger.info("FlowCommander instance is already connected at IP address: %s" % (self.vapourtec_ip_address))

    def reconnect_flowcommander_exe(self):
        # Disconnect first then try to connect
        FlowCommander.Disconnect(self.fc)
        self.fc_exe_connected = False
        self.connect_flowcommander_exe()

    def process_request_parameters(self, derivation_inputs: DerivationInputs, derivation_outputs: DerivationOutputs):
        # Get ReactionExperiment dataclasses instances from the KG
        list_rxn_exp_instance = self.sparql_client.getReactionExperiment(
            derivation_inputs.getIris(ONTOREACTION_REACTIONEXPERIMENT))
        if len(list_rxn_exp_instance) > 1:
            raise Exception(
                "Only one instance of OntoReaction:ReactionExperiment should be specified per Vapourtec Derivation, collected: <%s>" % (
                    ">, <".join([rxnexp.instance_iri for rxnexp in list_rxn_exp_instance]))
            )

        rxn_exp_instance = list_rxn_exp_instance[0]

        if not self.dry_run:
            # Make sure the hardware is idle
            while not self.vapourtec_is_idle():
                time.sleep(30)

        # Get local variables of vapourtec_rs400 and autosampler for reaction
        vapourtec_rs400 = self.sparql_client.get_vapourtec_rs400(list_vapourtec_rs400_iri=[self.vapourtec_digital_twin])[0]
        autosampler = vapourtec_rs400.get_autosampler()

        # Get the collection method from the vapourtec_rs400
        # NOTE two types of collection: (1) send outlet to waste, (2) send outlet to autosampler site
        # NOTE the collection information should be stored in KG, and VapourtecRS400 instance
        # NOTE so that if it's sent to waste, then we don't care
        # NOTE but if it's sent to autosampler site, then we need to know the collection site
        # NOTE the collection site should be suggested by the vapourtec_rs400 instance
        if vapourtec_rs400.collects_to_fraction_collector():
            # need to know the number of collection site
            col_site_iri, col_site_num, col_site_vial_iri = vapourtec_rs400.get_collection_site()
            do_not_collect = False
        else:
            # no need to know the number of collection site, as it will go to waste bottle
            col_site_num = None
            do_not_collect = True
            waste_bottle_iri = vapourtec_rs400.get_collection_receptacle()

        # Call function to create a list of ontolab.EquipmentSettings instances
        # TODO [future work] the collection part should be further developed as a dataclass
        # TODO [limitation of API for now] the amount of collected chemicals (collection volume) should be stored in KG
        # TODO [future work, when we add pressure into optimisation] add support for pressure settings
        list_equip_settings = vapourtec_rs400.create_equip_settings_for_rs400_from_rxn_exp(rxn_exp=rxn_exp_instance)

        # Configure the vapourtec digital twin in KG with the created OntoLab:EquipmentSettings triples
        # NOTE here the triples written to the KG include links between the settings and the hardware
        # NOTE these links will be removed when the experiment is done
        equip_settings_graph_for_configure = self.sparql_client.collect_triples_for_equip_settings(list_equip_settings, True)
        self.sparql_client.uploadGraph(equip_settings_graph_for_configure)
        self.logger.info(f"The VapourtecRS400 <{self.vapourtec_digital_twin}> module is now configured with equipment settings: <{'>, <'.join([es.instance_iri for es in list_equip_settings])}>.")

        # NOTE here we also add the triples to derivation_outputs
        equip_settings_graph_for_derivation = self.sparql_client.collect_triples_for_equip_settings(list_equip_settings, False)
        derivation_outputs.addGraph(equip_settings_graph_for_derivation)

        # Generate the execution CSV file
        local_run_csv_path = vapourtec.create_exp_run_csv(
            folder_path=self.fcexp_file_container_folder,
            rxnexp=rxn_exp_instance,
            list_equip_settings=list_equip_settings,
            rs_400=vapourtec_rs400,
            start_vial_override=col_site_num,
            do_not_collect=do_not_collect,
        )
        self.logger.info(f"The CSV run file generated within container at: {local_run_csv_path}")
        # Upload the vapourtec input file to file server for record
        local_file_path = local_run_csv_path
        local_run_csv_path = local_run_csv_path[1:] if local_run_csv_path.startswith('/') else local_run_csv_path
        local_run_csv_path = local_run_csv_path[1:] if local_run_csv_path.startswith('\\') else local_run_csv_path
        remote_report_subdir = os.path.join(getShortName(self.vapourtec_digital_twin),local_run_csv_path)
        vapourtec_input_file_iri = self.sparql_client.upload_vapourtec_input_file_to_kg(
            vapourtec_digital_twin=self.vapourtec_digital_twin,
            local_file_path=local_file_path,
            remote_file_subdir=remote_report_subdir
        )
        # Add the iri of VapourtecInputFile to derivation_outputs
        vapourtec_input_file_graph = Graph()
        vapourtec_input_file_graph.add((URIRef(vapourtec_input_file_iri), RDF.type, URIRef(ONTOVAPOURTEC_VAPOURTECINPUTFILE)))
        vapourtec_input_file_graph.add((URIRef(rxn_exp_instance.instance_iri), URIRef(ONTOVAPOURTEC_HASVAPOURTECINPUTFILE), URIRef(vapourtec_input_file_iri)))
        derivation_outputs.addGraph(vapourtec_input_file_graph)

        # Send the experiment csv for execution, also update the autosampler liquid level
        self.dry_run_duration = random.randint(10, 180)
        self.send_fcexp_csv_for_execution(local_file_path, list_equip_settings)

        # Wait until the vapourtec is running reaction (if not dry_run)
        if not self.dry_run:
            vapourtec_running_reaction = False
            while not vapourtec_running_reaction:
                time.sleep(30)
                vapourtec_running_reaction = self.sparql_client.vapourtec_rs400_is_running_reaction(vapourtec_rs400.instance_iri)

        # Create chemical solution instance for the reaction outlet stream
        # <chemical_solution> <fills> <vial>. <vial> <isFilledWith> <chemical_solution>. here we should write the vial location to the KG
        # update <vial> <hasFillLevel>/<hasValue>/<hasNumericalValue> <xxx>.
        # (the rest information about vial should already be known as part of digital twin of autosampler)
        # TODO [limitation of API for now] the amount of collected chemicals (collection volume) should be retrieved properly
        _amount_of_chemical_solution_ = 3
        if vapourtec_rs400.collects_to_fraction_collector():
            chemical_solution_graph = self.sparql_client.create_chemical_solution_for_reaction_outlet(
                autosampler_site_iri=col_site_iri,
                amount_of_chemical_solution=_amount_of_chemical_solution_,
            )
        else:
            chemical_solution_graph = self.sparql_client.create_chemical_solution_for_outlet_to_waste(
                waste_bottle_iri=waste_bottle_iri,
                amount_of_chemical_solution=_amount_of_chemical_solution_,
            )
        derivation_outputs.addGraph(chemical_solution_graph)

        # Remove the link between equipment settings and the digital twin of hardware
        # --> so within execution operation of each reaction experiment, the hardware got configured and released
        self.sparql_client.release_vapourtec_rs400_settings(vapourtec_rs400.instance_iri)

    def send_fcexp_csv_for_execution(self, run_csv_file_container_path: str, list_equip_settings: List[EquipmentSettings]):
        # TODO [nice-to-have, when run in loop, double check] should we load experiment file to check if the files are generated correctly for dry_run?
        # TODO [nice-to-have, when run in loop, double check] consider if we need to distinguish between actual execution and dry-run on per-reaction-basis
        if not self.dry_run:
            print("#######################")
            # NOTE The fcexp file should be loaded before the optimisation campaign is started
            # # TODO [until further notice] should we load experiment everytime before we add and run new experiment?
            # template_fcexp_filepath = os.path.join(self.fcexp_file_host_folder, self.fcexp_template_filename)
            # FlowCommander.LoadExperiment(self.fc, template_fcexp_filepath) # e.g. "D:\Vapourtec\ReactionCont.fcexp"
            # self.logger.info("FlowCommander instance loaded experiment file: %s" % (template_fcexp_filepath))

            # Clear existing reaction experiments before adding new one, otherwise the old experiments will be executed again
            FlowCommander.ClearReactions(self.fc)
            # Add reaction in CSV file to FlowCommander
            CSVParser.AddReactions(run_csv_file_container_path, self.fc)
            self.logger.info("Experiment recorded in the CSV file (%s) is added." % (run_csv_file_container_path))

            # Run real reaction experiment
            FlowCommander.Run(self.fc)
            print("#######################")

            # NOTE now the liquid consumption is not updated in the KG after each reaction, this should be improved ASAP
            # !!!TODO [limitation of API for now] Update autosampler liquid amount immediately after send the experiment for execution
            # Construct a dict of {"autosampler_site_iri": sampler_loop_volume * 1.2}
            # NOTE here "*1.2" is to reflect the default setting in vapourtec which takes 20% more liquid compared to the sample loop volume settings
            # dct_site_loop_volume = {s.pumpsLiquidFrom.instance_iri:s.hasSampleLoopVolumeSetting.hasQuantity.hasValue.hasNumericalValue*1.2 for s in [
            #     setting for setting in list_equip_settings if setting.clz == ONTOVAPOURTEC_PUMPSETTINGS and setting.hasSampleLoopVolumeSetting]}
            # self.sparql_client.update_vapourtec_autosampler_liquid_level_millilitre(dct_site_loop_volume, True)

            # TODO [nice-to-have, limitation of current API] send warnings if the liquid level of any vials is lower than the warning level
        else:
            self.update_vapourtec_state_for_dry_run(self.dry_run_duration)

    @DerivationAgent.send_email_when_exception(func_return_value=False)
    def monitor_vapourtec_rs400_state(self):
        print("=======================")
        try:
            # Connect to FlowCommander instance opened at the given IP address if not already connected
            if not self.fc_exe_connected:
                self.connect_flowcommander_exe()
            # Check the state and update digital twin in the knowledge graph
            self.update_flowcommander_state()
        except Exception as e:
            self.logger.error(e, stack_info=True, exc_info=True)
            try:
                # Reconnect to FlowCommander instance opened at the given IP address
                self.reconnect_flowcommander_exe()
                self.update_flowcommander_state()
            except Exception as e1:
                self.logger.error(e1, stack_info=True, exc_info=True)
                raise Exception(f"Failed to reconnect to FlowCommander instance opened at the given IP address: {self.vapourtec_ip_address}") from e1
        print("=======================")

    def update_flowcommander_state(self):
        try:
            timestamp, fcstate = datetime.now().timestamp(), FlowCommander.GetState(self.fc)
            self.vapourtec_state = vapourtec.MAPPING_VAPOURTEC_STATE[fcstate]
            self.sparql_client.update_vapourtec_rs400_state(self.vapourtec_digital_twin, self.vapourtec_state, timestamp)
            self.logger.info("FlowCommander instance at IP address (%s) has state <%s> at timestamp %s, updated records in knowledge graph." %(
                self.vapourtec_ip_address, self.vapourtec_state, str(timestamp)))
        except Exception as e:
            self.logger.error(e, stack_info=True, exc_info=True)

    def update_vapourtec_state_for_dry_run(self, dry_run_duration: int):
        if self.dry_run:
            try:
                if not self.fc_exe_connected:
                    timestamp = datetime.now().timestamp()
                    self.sparql_client.update_vapourtec_rs400_state(self.vapourtec_digital_twin, ONTOVAPOURTEC_DRYRUNSTATE, timestamp)
                    self.logger.info(f"VapourtecRS400 instance {self.vapourtec_digital_twin} is in DryRunState at timestamp {timestamp}, updated records in knowledge graph.")
                    time.sleep(dry_run_duration)
                    timestamp = datetime.now().timestamp()
                    self.sparql_client.update_vapourtec_rs400_state(self.vapourtec_digital_twin, ONTOVAPOURTEC_IDLE, timestamp)
                    self.logger.info(f"VapourtecRS400 instance {self.vapourtec_digital_twin} finished DryRun timestamp {timestamp}, updated its state to {ONTOVAPOURTEC_IDLE} in knowledge graph.")
                else:
                    self.logger.info("Pausing monitor_vapourtec_rs400_state job for dry run.")
                    self.scheduler.get_job('monitor_vapourtec_rs400_state').pause()
                    timestamp = datetime.now().timestamp()
                    self.sparql_client.update_vapourtec_rs400_state(self.vapourtec_digital_twin, ONTOVAPOURTEC_DRYRUNSTATE, timestamp)
                    self.logger.info(f"VapourtecRS400 instance {self.vapourtec_digital_twin} is in DryRunState at timestamp {timestamp}, updated records in knowledge graph.")
                    time.sleep(dry_run_duration)
                    self.logger.info("Resuming monitor_vapourtec_rs400_state job after dry run.")
                    self.scheduler.get_job('monitor_vapourtec_rs400_state').resume()
            except Exception as e:
                self.logger.error(e, stack_info=True, exc_info=True)
                raise Exception("Failed to update VapourtecRS400 state for dry run.") from e

    def vapourtec_is_idle(self) -> bool:
        return True if self.vapourtec_state == ONTOVAPOURTEC_IDLE else False

    @DerivationAgent.periodical_job
    def _start_monitoring_vapourtec_rs400_state(self):
        """
            This method starts the periodical job to monitor the state of VapourtecRS400 module.
        """

        # Add state monitoring job to scheduler
        self.scheduler.add_job(
            id='monitor_vapourtec_rs400_state',
            func=self.monitor_vapourtec_rs400_state,
            trigger='interval',
            seconds=self.vapourtec_state_periodic_timescale
        )

        # Start the scheduler if it's not already started
        self.logger.info("Monitor VapourtecRS400 state job is scheduled with a time interval of %d seconds." % (
            self.vapourtec_state_periodic_timescale)
        )


# Show an instructional message at the VapourtecAgent servlet root
def default():
    """
        Instructional message at the app root.
    """
    msg  = "This is an asynchronous agent that capable of monitoring the state of VapourtecRS400 module and keeping its digital twin in sync with its physical counterpart.<BR>"
    msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/VapourtecAgent#readme<BR>"
    return msg
