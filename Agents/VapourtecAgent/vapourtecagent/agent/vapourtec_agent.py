from datetime import datetime
import json
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
        register_agent: bool = True,
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
        self.register_agent = register_agent

        self.sparql_client = ChemistryAndRobotsSparqlClient(
            self.kgUrl, self.kgUpdateUrl, self.kgUser, self.kgPassword,
            self.fs_url, self.fs_user, self.fs_password
        )

        self.fc_exe_connected = False
        self.vapourtec_state = ONTOVAPOURTEC_NULL

    def connect_flowcommander_exe(self):
        # Connect to FlowCommander instance opened at the given IP address
        if not self.fc_exe_connected:
            self.fc = FlowCommander.Connect(self.vapourtec_ip_address)
            self.fc_exe_connected = True
            self.logger.info("Connected to FlowCommander instance at IP address: %s" % (self.vapourtec_ip_address))
        else:
            self.logger.info("FlowCommander instance is already connected at IP address: %s" % (self.vapourtec_ip_address))

    def register(self):
        if self.register_agent:
            try:
                self.sparql_client.generate_ontoagent_instance(
                    self.agentIRI,
                    self.agentEndpoint,
                    [ONTOREACTION_REACTIONEXPERIMENT],
                    [ONTOLAB_EQUIPMENTSETTINGS, ONTOVAPOURTEC_VAPOURTECINPUTFILE, ONTOLAB_CHEMICALSOLUTION]
                )
                self.sparql_client.register_agent_with_hardware(self.agentIRI, self.vapourtec_digital_twin)
            except Exception as e:
                self.logger.error(e, stack_info=True, exc_info=True)
                raise Exception("Agent <%s> registration failed." % self.agentIRI)

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
        self.logger.debug("Collected inputs from the knowledge graph: ")
        self.logger.debug(json.dumps(rxn_exp_instance.dict()))

        if not self.dry_run:
            # Make sure the hardware is idle
            while not self.vapourtec_is_idle():
                time.sleep(30)

        # Get local variables of vapourtec_rs400, vapourtec_r4_reactor, and autosampler for reaction
        vapourtec_rs400 = self.sparql_client.get_vapourtec_rs400(self.vapourtec_digital_twin)
        vapourtec_r4_reactor = [reactor for reactor in vapourtec_rs400.consistsOf if reactor.instance_iri == rxn_exp_instance.isAssignedTo][0]
        autosampler = self.sparql_client.get_autosampler_from_vapourtec_rs400(vapourtec_rs400)

        # Call function to create a list of ontolab.EquipmentSettings instances
        # TODO [before deployment, double check] Also generate settings for AutoSampler (collection bits)?
        # TODO [before deployment, double check] add support for pressure settings
        list_equip_settings = self.sparql_client.create_equip_settings_for_rs400_from_rxn_exp(
            rxn_exp_instance, vapourtec_rs400, vapourtec_r4_reactor)

        # Configure the vapourtec digital twin in KG with the created OntoLab:EquipmentSettings triples
        # NOTE here the triples written to the KG include links between the settings and the hardware
        # NOTE these links will be removed when the experiment is done
        equip_settings_graph_for_configure = self.sparql_client.collect_triples_for_equip_settings(list_equip_settings, True)
        self.sparql_client.uploadGraph(equip_settings_graph_for_configure)
        self.logger.info(f"The VapourtecRS400 <{self.vapourtec_digital_twin}> module is now configured with equipment settings: <{'>, <'.join([es.instance_iri for es in list_equip_settings])}>.")

        # NOTE here we also add the triples to derivation_outputs
        equip_settings_graph_for_derivation = self.sparql_client.collect_triples_for_equip_settings(list_equip_settings, False)
        derivation_outputs.addGraph(equip_settings_graph_for_derivation)

        # TODO [before deployment, double check] refine the way of getting the autosampler site for collecting reaction outlet stream
        # TODO [before deployment, double check] get settings for AutoSampler? collection site and collection volume?
        _autosampler_site_for_collection = [site.instance_iri for site in autosampler.hasSite if site.holds.isFilledWith is None and site.holds.hasFillLevel.hasValue.hasNumericalValue == 0][0]
        # TODO [before deployment, double check] 1.5 is only as placeholder for testing, get this information from AutoSampler
        _autosampler_collection_amount = 1.5

        # Generate the execution CSV file
        local_run_csv_path = vapourtec.create_exp_run_csv(self.fcexp_file_container_folder, rxn_exp_instance, list_equip_settings)
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
        self.send_fcexp_csv_for_execution(local_file_path, list_equip_settings)

        # Create chemical solution instance for the reaction outlet stream
        # <chemical_solution> <fills> <vial>. <vial> <isFilledWith> <chemical_solution>. here we should write the vial location to the KG
        # update <vial> <hasFillLevel>/<hasValue>/<hasNumericalValue> <xxx>.
        # (the rest information about vial should already be known as part of digital twin of autosampler)
        chemical_solution_graph = self.sparql_client.create_chemical_solution_for_reaction_outlet(
            autosampler_site_iri=_autosampler_site_for_collection,
            amount_of_chemical_solution=_autosampler_collection_amount
        )
        derivation_outputs.addGraph(chemical_solution_graph)

        # Remove the link between equipment settings and the digital twin of hardware
        # --> so within execution operation of each reaction experiment, the hardware got configured and released
        self.sparql_client.release_vapourtec_rs400_settings(vapourtec_rs400.instance_iri)

    def send_fcexp_csv_for_execution(self, run_csv_file_container_path: str, list_equip_settings: List[EquipmentSettings]):
        # TODO [before deployment, double check] should we load experiment file to check if the files are generated correctly for dry_run?
        # TODO [before deployment, double check] consider if we need to distinguish between actual execution and dry-run on per-reaction-basis
        if not self.dry_run:
            print("#######################")
            # TODO [before deployment, double check] should we load experiment everytime before we add and run new experiment?
            template_fcexp_filepath = os.path.join(self.fcexp_file_host_folder, self.fcexp_template_filename)
            FlowCommander.LoadExperiment(self.fc, template_fcexp_filepath) # e.g. "D:\Vapourtec\ReactionCont.fcexp"
            self.logger.info("FlowCommander instance loaded experiment file: %s" % (template_fcexp_filepath))

            # Add reaction in CSV file to FlowCommander
            CSVParser.AddReactions(run_csv_file_container_path, self.fc)
            self.logger.info("Experiment recorded in the CSV file (%s) is added." % (run_csv_file_container_path))

            # Run real reaction experiment
            FlowCommander.Run(self.fc)
            print("#######################")

            # Update autosampler liquid amount immediately after send the experiment for execution
            # Construct a dict of {"autosampler_site_iri": sampler_loop_volume * 1.2}
            # NOTE here "*1.2" is to reflect the default setting in vapourtec which takes 20% more liquid compared to the sample loop volume settings
            # TODO [before deployment, double check] double check how was the sample loop volume calculated
            # TODO [before deployment, double check] the implementation here actually only updates the autosampler site pumped by reference_pump
            dct_site_loop_volume = {s.pumpsLiquidFrom.instance_iri:s.hasSampleLoopVolumeSetting.hasQuantity.hasValue.hasNumericalValue*1.2 for s in [
                setting for setting in list_equip_settings if setting.clz == ONTOVAPOURTEC_PUMPSETTINGS and setting.hasSampleLoopVolumeSetting]}
            self.sparql_client.update_vapourtec_autosampler_liquid_level_millilitre(dct_site_loop_volume, True)

            # TODO [before deployment, double check] send warnings if the liquid level of any vials is lower than the warning level

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
                # self.fc = FlowCommander.Connect(self.vapourtec_ip_address)
                self.connect_flowcommander_exe()
                self.update_flowcommander_state()
            except Exception as e1:
                self.logger.error(e1, stack_info=True, exc_info=True)
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

    def vapourtec_is_idle(self) -> bool:
        return True if self.vapourtec_state == ONTOVAPOURTEC_IDLE else False

    def add_job_monitoring_vapourtec_rs400_state(self, start=False):
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

        if start:
            self.start()


# Show an instructional message at the VapourtecAgent servlet root
def default():
    """
        Instructional message at the app root.
    """
    msg  = "This is an asynchronous agent that capable of monitoring the state of VapourtecRS400 module and keeping its digital twin in sync with its physical counterpart.<BR>"
    msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/134-dev-lab-equipment-digital-twin/Agents/VapourtecAgent#readme<BR>"
    # TODO change above line to https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Agents/VapourtecAgent#readme, before merging back to develop branch
    return msg
