# Disable excessive debug logging from numba and matplotlib module
import logging
logging.getLogger("numba").setLevel(logging.WARNING)
logging.getLogger("matplotlib").setLevel(logging.WARNING)

from matplotlib.backends.backend_agg import FigureCanvasAgg as FigureCanvas
import matplotlib.pyplot as plt
from abc import ABC
from multiprocessing import Process
from flask_apscheduler import APScheduler
from flask import Flask
from flask import request
from flask import jsonify
from flask import send_file
from flask import render_template
from urllib.parse import unquote
from urllib.parse import urlparse
from rdflib import Graph, URIRef, Literal
from datetime import datetime
import pandas as pd
import yagmail
import base64
import json
import time
import os
import io

from py4jps import agentlogging
from pyderivationagent import PyDerivationClient

from rxnoptgoalagent.kg_operations import RxnOptGoalSparqlClient
from rxnoptgoalagent.data_model import *


class FlaskConfig(object):
    """
        This class provides the configuration for flask app object. Each config should be provided as constant. For more information, visit https://flask.palletsprojects.com/en/2.0.x/config/.
    """
    SCHEDULER_API_ENABLED = True


class RxnOptGoalAgent(ABC):
    GOAL_SPECS_RESPONSE_KEY = "Created a RxnOptGoalIter (ROGI) Derivation"
    GOAL_SET_IRI_KEY = ONTOGOAL_GOALSET

    def __init__(
        self,
        goal_agent_iri: str,
        goal_agent_endpoint: str,
        goal_monitor_time_interval: int,
        goal_iter_agent_iri: str,
        derivation_instance_base_url: str,
        kg_url: str,
        kg_update_url: str = None,
        kg_user: str = None,
        kg_password: str = None,
        fs_url: str = None,
        fs_user: str = None,
        fs_password: str = None,
        app: Flask = Flask(__name__, template_folder="/app/templates"),
        flask_config: FlaskConfig = FlaskConfig(),
        logger_name: str = "dev",
        email_recipient: str = '',
        email_subject_prefix: str = '',
        email_username: str = '',
        email_auth_json_path: str = '',
        email_goal_iteration_progress: bool = False,
    ):
        """
            This method initialises the instance of RxnOptGoalAgent.

            Arguments:
                goal_agent_iri - OntoAgent:Service IRI of the goal agent, an example: "http://www.example.com/triplestore/agents/Service__XXXAgent#Service"
                goal_agent_endpoint - HTTP URL of the goal agent which takes goal request via HTTP POST, an example: "http://localhost:5000/goal_specification"
                goal_monitor_time_interval - time interval between two runs of goal monitoring job (in SECONDS)
                goal_iter_agent_iri - OntoAgent:Service IRI of the RxnOptGoalIter Agent, NOTE this is different from goal_agent_iri
                derivation_instance_base_url - namespace to be used when creating derivation instance, an example: "http://www.example.com/triplestore/repository/"
                kg_url - SPARQL query endpoint, an example: "http://localhost:8080/blazegraph/namespace/triplestore/sparql"
                kg_update_url - SPARQL update endpoint, will be set to the same value as kg_url if not provided, an example: "http://localhost:8080/blazegraph/namespace/triplestore/sparql"
                kg_user - username used to access the SPARQL query/update endpoint specified by kg_url/kg_update_url
                kg_password - password that set for the kg_user used to access the SPARQL query/update endpoint specified by kg_url/kg_update_url
                fs_url - file server endpoint, an example: "http://localhost:8080/FileServer/"
                fs_user - username used to access the file server endpoint specified by fs_url
                fs_password - password that set for the fs_user used to access the file server endpoint specified by fs_url
                app - flask app object, an example: app = Flask(__name__)
                flask_config - configuration object for flask app, should be an instance of the class FlaskConfig provided as part of this package
                logger_name - logger names for getting correct loggers from py4jps.agentlogging package, valid logger names: "dev" and "prod", for more information, visit https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_BASE_LIB/python_wrapper/py4jps/agentlogging/logging.py
                email_recipient - email address to send email notification to
                email_subject_prefix - prefix to be added to the subject of email notification
                email_username - username used to access the email server
                email_auth_json_path - path to the json file that contains the authentication information for the email server
                email_goal_iteration_progress - boolean value to indicate whether to send emails to the recipient when a goal iteration enters the next iteration
        """

        # initialise flask app with its configuration
        self.app = app
        self.app.config.from_object(flask_config)

        # initialise flask scheduler and assign time interval for monitorDerivations job
        self.scheduler = APScheduler(app=self.app)
        self.goal_monitor_time_interval = goal_monitor_time_interval

        # assign IRI and HTTP URL of the agent
        self.goal_agent_iri = goal_agent_iri
        self.goal_agent_endpoint = goal_agent_endpoint

        # assign IRI of goal iteration agent
        self.goal_iter_agent_iri = goal_iter_agent_iri

        # assign KG related information
        self.kg_url = kg_url
        self.kg_update_url = kg_update_url if kg_update_url is not None else kg_url
        self.kg_user = kg_user
        self.kg_password = kg_password

        # assign file server related information
        self.fs_url = fs_url
        self.fs_user = fs_user
        self.fs_password = fs_password

        # initialise the RxnOptGoalSparqlClient
        self.sparql_client = RxnOptGoalSparqlClient(
            query_endpoint=self.kg_url, update_endpoint=self.kg_update_url,
            kg_user=self.kg_user, kg_password=self.kg_password,
            fs_url=self.fs_url, fs_user=self.fs_user, fs_pwd=self.fs_password
        )

        # initialise the derivation_client with SPARQL Query and Update endpoint
        self.derivation_instance_base_url = derivation_instance_base_url
        self.derivation_client = PyDerivationClient(
            derivation_instance_base_url=self.derivation_instance_base_url,
            query_endpoint=self.kg_url, update_endpoint=self.kg_update_url,
            kg_user=self.kg_user, kg_password=self.kg_password,
        )

        # initialise the logger
        self.logger = agentlogging.get_logger(logger_name)

        # add the default routes for the flask app
        self.app.add_url_rule('/', 'root', self.default, methods=['GET'])

        # add the route for the rxn opt goal specification
        self.app.add_url_rule('/goal', 'rxn_opt_goal', self.goal_page, methods=['GET'])

        # add the route for the goal result plot
        self.app.add_url_rule('/goal/result', 'rxn_opt_goal_result', self.goal_result_page, methods=['GET'])

        # add the route for activating existing goal set
        self.app.add_url_rule('/goal/reactivate', 'rxn_opt_goal_reactivate', self.goal_reactivate_page, methods=['GET'])
        # add the url pattern that handles the POST request for activating existing goal set
        self.app.add_url_rule('/goal_reactivate', 'goal_reactivate_rxnoptgoal', self.reactivate_existing_goal_set, methods=['POST'])

        # add the url pattern that handles the goal request
        url_pattern = urlparse(self.goal_agent_endpoint).path
        url_pattern_name = url_pattern.strip('/').replace('/', '_') + '_rxnoptgoal'
        self.app.add_url_rule(url_pattern, url_pattern_name, self.handle_rxn_opt_goal_request, methods=['POST'])
        self.logger.info(f"The endpoint to handle goal request is added as: {url_pattern}")

        # initialise the current_active_goal_set as None
        self.current_active_goal_set = None

        # initialise the email object and email_goal_iteration_progress flag
        if all([bool(param) for param in [email_recipient, email_username, email_auth_json_path]]):
            self.yag = yagmail.SMTP(email_username, oauth2_file=email_auth_json_path)
            self.email_recipient = email_recipient.split(';')
            self.email_subject_prefix = email_subject_prefix if bool(email_subject_prefix) else str(self.__class__.__name__)
        else:
            self.yag = None
        self.email_goal_iteration_progress = email_goal_iteration_progress

        self.logger.info(f"RxnOptGoalAgent initialised with IRI: {self.goal_agent_iri}")


    def send_email(self, subject, contents, attachments=None):
        timeout = 2
        process_email = Process(target=self.yag.send, args=(self.email_recipient, subject, contents, attachments))
        process_email.start()
        process_email.join(timeout=timeout)
        if process_email.is_alive():
            process_email.kill()
            process_email.join()
        if process_email.exitcode != 0:
            self.logger.error(f"Timed out sending email notification after {timeout} seconds.\n Recipient: {self.email_recipient}\n Subject: {subject}\n Contents: {contents}\n Attachments: {attachments}")


    def default(self):
        """Instruction for the RxnOptGoalAgent usage."""
        msg = "Welcome to the RxnOptGoalAgent!<BR>"
        msg += "This is a goal agent that capable of persure a reaction optimisation goal.<BR>"
        msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/160-dev-rxn-opt-goal-agent/Agents/RxnOptGoalAgent#readme<BR>"    
        return msg


    def goal_page(self):
        return render_template(
            'rxn_opt_goal.html',
            # TODO [nice-to-have] show more information about the chemical reaction once selected in dropdown
            # TODO once decided on the triple store to deploy, change the IRI to the correct one
            chem_rxn_iri=[{'iri': cr, 'display': cr} for cr in self.sparql_client.get_all_chemical_reaction_iri()],
            # TODO [nice-to-have] specify the limits of the goal, e.g. yield within 0-100%
            # TODO [nice-to-have] put the unit as symbol in the dropdown list, e.g. %, g/mol, kg, etc.
            goal_spec_from_flask={
                perf_iri: {
                    'iri': perf_iri,
                    'display': getShortName(perf_iri),
                    'units': [{
                        'iri': u, 'display': getShortName(u)
                    } for u in AVAILABLE_PERFORMANCE_INDICATOR_UNIT_DICT[perf_iri]]
                } for perf_iri in AVAILABLE_PERFORMANCE_INDICATOR_LIST
            },
            desires_type=[{'iri': des, 'display': getShortName(des)} for des in [ONTOGOAL_DESIRESGREATERTHAN, ONTOGOAL_DESIRESLESSTHAN]],
            rxn_opt_goal_plan=[{'iri': plan, 'display': plan} for plan in self.sparql_client.get_all_rxn_opt_plans()],
            available_labs=[{'iri': lab, 'display': lab} for lab in self.get_available_labs()],
        )


    def handle_rxn_opt_goal_request(self):
        """
        This function is called when a goal request is received.
        """
        if request.method == 'POST':
            parameters = request.form
        else:
            self.logger.error("The method is not supported.")
            return f"The method [{request.method}] is not supported."

        # Varify the parameters
        self.logger.info(f"Received a goal request with parameters: {parameters}")
        # TODO [next iteration] provide a more scalable way to deal with the multiple goals
        all_parameters = ["chem_rxn", "cycleAllowance", "deadline",
                          "first_goal_clz", "first_goal_desires", "first_goal_num_val", "first_goal_unit",
                          "rxn_opt_goal_plan", "labs"]
        if not all([p in parameters and bool(parameters[p]) for p in all_parameters]):
            return f"""The request parameters are incomplete, required parameters: {all_parameters}.
                    Received parameters: {parameters}.
                    Please provided the missing fields: {[p for p in all_parameters if p not in parameters or not bool(parameters[p])]}."""

        # Parse request form parameters to construct goal related instances
        chem_rxn_iri = parameters['chem_rxn']

        rxn_opt_goal_plan = self.sparql_client.get_goal_plan(parameters['rxn_opt_goal_plan'])

        first_goal_desires = parameters['first_goal_desires']
        first_goal_desires_quantity = OM_Quantity(
            instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
            namespace_for_init=self.derivation_instance_base_url,
            clz=parameters['first_goal_clz'],
            hasValue=OM_Measure(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                namespace_for_init=self.derivation_instance_base_url,
                hasUnit=parameters['first_goal_unit'],
                hasNumericalValue=parameters['first_goal_num_val']
            )
        )
        first_goal = Goal(
            instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
            namespace_for_init=self.derivation_instance_base_url,
            hasPlan=rxn_opt_goal_plan,
            desiresGreaterThan=first_goal_desires_quantity if first_goal_desires == ONTOGOAL_DESIRESGREATERTHAN else None,
            desiresLessThan=first_goal_desires_quantity if first_goal_desires == ONTOGOAL_DESIRESLESSTHAN else None,
        )
        goal_list = [first_goal]

        if parameters.get('second_goal_desires') and parameters.get('second_goal_clz') and parameters.get('second_goal_unit') and parameters.get('second_goal_num_val'):
            second_goal_desires = parameters['second_goal_desires']
            second_goal_desires_quantity = OM_Quantity(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                namespace_for_init=self.derivation_instance_base_url,
                clz=parameters['second_goal_clz'],
                hasValue=OM_Measure(
                    instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                    namespace_for_init=self.derivation_instance_base_url,
                    hasUnit=parameters['second_goal_unit'],
                    hasNumericalValue=parameters['second_goal_num_val']
                )
            )
            second_goal = Goal(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                namespace_for_init=self.derivation_instance_base_url,
                hasPlan=rxn_opt_goal_plan,
                desiresGreaterThan=second_goal_desires_quantity if second_goal_desires == ONTOGOAL_DESIRESGREATERTHAN else None,
                desiresLessThan=second_goal_desires_quantity if second_goal_desires == ONTOGOAL_DESIRESLESSTHAN else None,
            )
            goal_list.append(second_goal)

        # Get the list of available labs
        # NOTE The remaining cycleAllowance will be deducted by the number of labs
        # as the number of ROGI derivation instances will be equal to the number of labs
        available_labs = parameters.getlist('labs')

        restriction = Restriction(
            instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
            namespace_for_init=self.derivation_instance_base_url,
            cycleAllowance=int(parameters['cycleAllowance']) - len(available_labs),
            deadline=datetime.timestamp(datetime.fromisoformat(parameters['deadline']))
        )

        # Now we need to construct a GoalSet object with above information
        goal_set_instance = GoalSet(
            instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
            namespace_for_init=self.derivation_instance_base_url,
            hasGoal=goal_list,
            hasRestriction=restriction
        )

        # Upload the goal set instance to KG, also add timestamp to the goal set instance
        g = Graph()
        g = goal_set_instance.create_instance_for_kg(g)
        self.sparql_client.uploadGraph(g)
        self.derivation_client.addTimeInstance(goal_set_instance.instance_iri)
        self.derivation_client.updateTimestamp(goal_set_instance.instance_iri)

        # Query the KG to get all previous ReactionExperiment with specific PerformanceIndicator for the requested ChemicalReaction
        lst_rxn_exp = self.sparql_client.get_all_rxn_exp_with_target_perfind_given_chem_rxn(
            chem_rxn_iri,
            [goal.desires().clz for goal in goal_list]
        )

        # Construct the list for derivation inputs
        derivation_inputs = [goal_set_instance.instance_iri] + lst_rxn_exp + [chem_rxn_iri]

        # Create a RxnOptGoalIter (ROGI) derivation for new info
        # NOTE: the ROGI derivations needs to be created with the IRI of the ROGI agent
        # which is DIFFERENT from the IRI of ROG agent (self.goal_agent_iri)
        # TODO [next iteration] in this iteration, we provide the ROGI agent IRI as a parameter (self.goal_iter_agent_iri)
        # TODO [next iteration] but in the future, this information should obtained by ROG agent from the KG
        # NOTE one derivation is assigend to one lab, therefore we create amount of derivations based on the amount of labs available
        lst_rogi_derivation = [self.derivation_client.createAsyncDerivationForNewInfo(self.goal_iter_agent_iri, derivation_inputs + [_]) for _ in available_labs]

        # TODO [next iteration] optimise the following code that deals with the ROGI iterations
        # Add a periodical job to monitor the goal iterations for the created ROGI derivation
        self.current_active_goal_set = goal_set_instance.instance_iri
        self.scheduler.add_job(
            id=f'monitor_goal_set__{getShortName(goal_set_instance.instance_iri)}',
            func=self.monitor_goal_iterations,
            trigger='interval', seconds=self.goal_monitor_time_interval
        )
        if not self.scheduler.running:
            self.scheduler.start()
        self.logger.info("Monitor goal iteration is scheduled with a time interval of %d seconds." % (self.goal_monitor_time_interval))

        # send email about the start of goal iterations
        if self.yag is not None:
            self.send_email(
                subject=f"[{self.email_subject_prefix}] Goal Iteration Scheduled",
                contents=[
                    format_current_time(),
                    f"Iterations to pursue GoalSet {goal_set_instance.instance_iri} is scheduled.",
                    f"Goal Iteration will be monitored every {self.goal_monitor_time_interval} seconds.",
                    f"The reaction to be optimised is {chem_rxn_iri}.",
                    f"The laboratories taking part of this optimisation campaign are {available_labs}."
                ]
            )

        return jsonify({self.GOAL_SPECS_RESPONSE_KEY: lst_rogi_derivation, self.GOAL_SET_IRI_KEY: goal_set_instance.instance_iri})

    def monitor_goal_iterations(self):
        """
        This function is called by the scheduler to monitor the goal iterations.
        """
        self.logger.info(f"Monitoring the goal iterations of GoalSet <{self.current_active_goal_set}>...")
        # for the current active goal set, get the ROGI derivations
        # 1. Check if any of the current running ROGI derivation is finished (up-to-date)
        # 1.1. if no, skip all below steps
        # 1.2. if yes, query the goal set instance from the KG, proceed to step 2
        # 2. compare if the best result meet the goal
        # 2.1. if yes, stop the scheduler, skip all below steps
        # 2.2. if no, proceed to step 3
        # 3. check if the restriction is still okay
        # 3.1. if still okay, update goal set with new restriction, update all rogi derivation to take the rxn exp as inputs, request an update for those finished rogi derivation
        # 3.2. if the restriction is not okay, stop the scheduler
        rogi_derivation_lst = self.sparql_client.get_rogi_derivations_of_goal_set(
            goal_set_iri=self.current_active_goal_set,
            rogi_agent_iri=self.goal_iter_agent_iri
        )
        rogi_derivation_lst_up_to_date = [rogi for rogi in rogi_derivation_lst if self.sparql_client.check_if_rogi_complete_one_iter(rogi)]
        if not bool(rogi_derivation_lst_up_to_date):
            self.logger.info(f"The ROGI derivations {rogi_derivation_lst} of GoalSet <{self.current_active_goal_set}> is still running. Will check again in {self.goal_monitor_time_interval} seconds.")

        else:
            # get the latest goal set instance
            goal_set_instance = self.sparql_client.get_goal_set_instance(self.current_active_goal_set)
            # check if any of the goals are met
            unmet_goals = goal_set_instance.get_unmet_goals()
            if len(unmet_goals) == 0:
                self.scheduler.remove_job(f'monitor_goal_set__{getShortName(self.current_active_goal_set)}')
                self.current_active_goal_set = None
                self.logger.info(f"All goals are met. Stop monitoring the iterations of goal set {self.current_active_goal_set}.")
                # send email about the completion of goal iterations
                if self.yag is not None:
                    self.send_email(
                        subject=f"[{self.email_subject_prefix}] Goal Iteration Completed",
                        contents=[
                            format_current_time(),
                            f"Iterations to pursue GoalSet {goal_set_instance.instance_iri} are completed.",
                            f"All goals are met and the best results are:",
                        ] + [
                            f"{_goal_str} [{_goal.clz}] = {_goal.hasValue.hasNumericalValue} {_goal.hasValue.hasUnit}" for _goal_str, _goal in goal_set_instance.get_best_results().items() if bool(goal_set_instance.get_best_results())
                        ]
                    )

            else:
                # check if the restriction is still okay
                if goal_set_instance.if_restrictions_are_okay():
                    # update the rogi derivation with new restriction and rxn exp, request for an update
                    self.logger.info(f"Restrictions are still okay. Updating the ROGI derivations {rogi_derivation_lst} with new restriction and rxn exp, also requesting for an update of {rogi_derivation_lst_up_to_date}.")

                    # update ReactionExperiment/Restriction accordingly
                    # the cycleAllowance will be updated depending on how many ROGI derivation is to be updated
                    # example SPARQL update with sub query:
                    # DELETE {
                    # <http://www.theworldavatar.com/triplestore/repository/Restriction_8be831da-8566-48cd-9966-24ea96101c44> <https://www.theworldavatar.com/kg/ontogoal/cycleAllowance> ?cycle_allowance.
                    # }
                    # INSERT {
                    # <http://a_rogi_derivation_up_to_date> <https://www.theworldavatar.com/kg/ontoderivation/isDerivedFrom> ?rxn_exp.
                    # <http://another_rogi_derivation_up_to_date> <https://www.theworldavatar.com/kg/ontoderivation/isDerivedFrom> ?rxn_exp.
                    # <http://another_rogi_derivation_STILL_IN_PROGRESS> <https://www.theworldavatar.com/kg/ontoderivation/isDerivedFrom> ?rxn_exp.
                    # <http://www.theworldavatar.com/triplestore/repository/Restriction_8be831da-8566-48cd-9966-24ea96101c44> <https://www.theworldavatar.com/kg/ontogoal/cycleAllowance> ?cycle_allowance_update.
                    # }
                    # WHERE {
                    # SELECT DISTINCT ?rxn_exp ?cycle_allowance ?cycle_allowance_update
                    # WHERE {
                    #     VALUES ?rogi_derivation { <http://a_rogi_derivation_up_to_date> <http://another_rogi_derivation_up_to_date> }
                    #     ?result <https://www.theworldavatar.com/kg/ontoderivation/belongsTo> ?rogi_derivation;
                    #             <https://www.theworldavatar.com/kg/ontogoal/refersTo> ?pi.
                    #     ?pi ^<https://www.theworldavatar.com/kg/ontoreaction/hasPerformanceIndicator> ?rxn_exp.
                    #     <http://www.theworldavatar.com/triplestore/repository/Restriction_8be831da-8566-48cd-9966-24ea96101c44> <https://www.theworldavatar.com/kg/ontogoal/cycleAllowance> ?cycle_allowance.
                    #     BIND (?cycle_allowance -2 AS ?cycle_allowance_update)
                    # }
                    # }

                    # delete clause: delete the old restriction
                    # insert clause: insert the new restriction, add the rxn exp as input to rogi derivation
                    # where clause: get the rxn exp, the old cycle allowance, and compute the new one
                    update = f"""
                        DELETE {{
                            <{goal_set_instance.hasRestriction.instance_iri}> <{ONTOGOAL_CYCLEALLOWANCE}> ?cycle_allowance .
                        }}
                        INSERT {{
                            <{f'> <{ONTODERIVATION_ISDERIVEDFROM}> ?rxn_exp. <'.join(rogi_derivation_lst)}> <{ONTODERIVATION_ISDERIVEDFROM}> ?rxn_exp.
                            <{goal_set_instance.hasRestriction.instance_iri}> <{ONTOGOAL_CYCLEALLOWANCE}> ?cycle_allowance_update .
                        }}
                        WHERE {{
                            SELECT DISTINCT ?rxn_exp ?cycle_allowance ?cycle_allowance_update
                            WHERE {{
                                VALUES ?rogi_derivation {{ <{'> <'.join(rogi_derivation_lst_up_to_date)}> }}
                                ?result <{ONTODERIVATION_BELONGSTO}> ?rogi_derivation; <{ONTOGOAL_REFERSTO}> ?pi.
                                ?pi ^<{ONTOREACTION_HASPERFORMANCEINDICATOR}> ?rxn_exp.
                                <{goal_set_instance.hasRestriction.instance_iri}> <{ONTOGOAL_CYCLEALLOWANCE}> ?cycle_allowance .
                                BIND (?cycle_allowance -{len(rogi_derivation_lst_up_to_date)} AS ?cycle_allowance_update)
                            }}
                    }}"""
                    self.logger.debug(f"SPARQL update: {update}")
                    self.sparql_client.performUpdate(update)
                    # update the timestamp of the goal_set instance as the restriction (cycleAllowance) is updated
                    self.derivation_client.updateTimestamp(goal_set_instance.instance_iri)
                    # finally request another round of update
                    for rogi_derivation in rogi_derivation_lst_up_to_date:
                        self.derivation_client.unifiedUpdateDerivation(rogi_derivation)
                    # send email about the goal iteration entering the next round
                    if self.yag is not None and self.email_goal_iteration_progress:
                        self.send_email(
                            subject=f"[{self.email_subject_prefix}] Goal Iteration Next Round",
                            contents=[
                                format_current_time(),
                                f"Iterations to pursue GoalSet {goal_set_instance.instance_iri} entered the next round.",
                                f"Restriction (cycleAllowance) is updated to {goal_set_instance.hasRestriction.cycleAllowance - len(rogi_derivation_lst_up_to_date)}.",
                                f"Restriction (deadline) is {datetime.fromtimestamp(goal_set_instance.hasRestriction.deadline)}.",
                                f"Current best results are:",
                                f"(NOTE: If the yield is slightly above 100%, it is most likely due to rounding errors, so don't worry.",
                                f"       However, please report to the developers if it is significantly above 100%, which is not expected.)",
                            ] + [
                                f"{_goal_str} [{_goal.clz}] = {_goal.hasValue.hasNumericalValue} {_goal.hasValue.hasUnit}" for _goal_str, _goal in goal_set_instance.get_best_results().items() if bool(goal_set_instance.get_best_results())
                            ]
                        )
                else:
                    self.logger.info(f"Restrictions are not okay. Stop monitoring the goal iterations. Current best results: { goal_set_instance.get_best_results() }")
                    self.scheduler.remove_job(f'monitor_goal_set__{getShortName(self.current_active_goal_set)}')
                    self.current_active_goal_set = None
                    # send email about the goal iteration stopped
                    if self.yag is not None:
                        self.send_email(
                            subject=f"[{self.email_subject_prefix}] Goal Iteration Stopped",
                            contents=[
                                format_current_time(),
                                f"Iterations to pursue GoalSet {goal_set_instance.instance_iri} has stopped due to restrictions not satisfied.",
                                f"Current best results are:",
                            ] + [
                                f"{_goal_str} [{_goal.clz}] = {_goal.hasValue.hasNumericalValue} {_goal.hasValue.hasUnit}" for _goal_str, _goal in goal_set_instance.get_best_results().items() if bool(goal_set_instance.get_best_results())
                            ]
                        )


    def goal_result_page(self):
        """
        Plot the goal iteration results of the given GoalSet IRI.
        """
        if 'goal_set' in request.args:
            _goal_set_iri = request.args['goal_set']
        elif self.current_active_goal_set is not None:
            _goal_set_iri = self.current_active_goal_set
        else:
            return f"""No GoalSet IRI is provided. Nor is any GoalSet currently running.
                       Please provide a GoalSet IRI in the URL, e.g. <br><br> {request.base_url}?goal_set=http://www.theworldavatar.com/GoalSet/GoalSet_1"""

        # get the goal set instance
        goal_set_instance = self.sparql_client.get_goal_set_instance(_goal_set_iri)
        if goal_set_instance is None:
            return f"""The GoalSet IRI {_goal_set_iri} is not found in the triplestore {self.kg_url}. Please check the IRI and try again."""

        # the goal set is not iterated for the first time yet if no results are found
        if len(goal_set_instance.get_best_results()) == 0:
            return f"""The current active GoalSet IRI {_goal_set_iri} is not iterated for the first time yet. Please wait for the first iteration to finish and try again."""

        # query the devTime of each reaction experiment that the performance indicators referring to
        _obj_dct = {}
        _goal_res_query_lst = []
        _result_val_dct = {}
        for goal in goal_set_instance.hasGoal:
            _result_iri_dct = {res.instance_iri:{'value': res.hasValue.hasNumericalValue, 'unit': res.hasValue.hasUnit} for res in goal.hasResult}
            _obj = goal.desires().clz
            _obj_key = f"{getShortName(_obj)}"
            _obj_dct[_obj_key] = _obj
            _goal_res_query_lst.append(f"""VALUES ?{_obj_key} {{ <{'> <'.join(list(_result_iri_dct.keys()))}> }} ?rxn <{ONTOREACTION_HASPERFORMANCEINDICATOR}> ?{_obj_key} .""")
            _result_val_dct.update(_result_iri_dct)

        query = f"""
            SELECT ?devTime ?rxn ?{" ?".join(list(_obj_dct.keys()))}
            WHERE {{ {" ".join(_goal_res_query_lst)}
                ?rxn <{ONTODERIVATION_BELONGSTO}>/<{TIME_HASTIME}>/<{TIME_INTIMEPOSITION}>/<{TIME_NUMERICPOSITION}> ?devTime .
            }}
            ORDER BY ?devTime
        """
        response = self.sparql_client.performQuery(query)

        if len(response) == 0:
            return f"No results found for GoalSet {_goal_set_iri} when executing query: {query}. Check if any of its ROGI derivations are finished. Or if the GoalSet IRI is correct. Or if the triplestore is specified correctly."

        # convert the response to a dataframe and plot
        _df = pd.DataFrame(response)
        _df.sort_values(by=['devTime'], ascending=True, inplace=True, ignore_index=True)
        _df['Goal Iteration (-)'] = _df.index + 1
        fig, ax = plt.subplots(figsize=(10, 10))
        _legend_lst = []
        for key, value in _obj_dct.items():
            ax.plot(_df['Goal Iteration (-)'], _df[key].apply(lambda x: unit_conv.unit_conversion_return_value(_result_val_dct[x]['value'], _result_val_dct[x]['unit'], DISPLAY_UNIT_FOR_PERFORMANCE_INDICATOR_DICT[value])), 'o')
            _legend_lst.append(key+f' ({getShortName(DISPLAY_UNIT_FOR_PERFORMANCE_INDICATOR_DICT[value])})')
        ax.set_xlabel('Goal Iteration (-)')
        ax.legend(_legend_lst)
        canvas = FigureCanvas(fig)
        img = io.BytesIO()
        fig.savefig(img)
        img.seek(0)
        return send_file(img, mimetype='image/png')

    def goal_reactivate_page(self):
        return render_template(
            'rxn_opt_goal_reactivate.html',
            goal_set_iri=[{'iri': _, 'display': _} for _ in self.sparql_client.get_all_existing_goal_set()]
        )

    def reactivate_existing_goal_set(self):
        goal_set_iri = request.form['goal_set']
        # Add a periodical job to monitor the goal iterations for the created ROGI derivation
        if self.current_active_goal_set is not None:
            return jsonify({'status': 'null', 'message': f"Another GoalSet <{self.current_active_goal_set}> is currently running. For now, only one GoalSet can be run at a time."})

        # Assign the current active goal set
        self.current_active_goal_set = goal_set_iri
        # Update the restriction with the new information from the form
        new_cycle_allowance = request.form['cycleAllowance'] if 'cycleAllowance' in request.form and bool(request.form['cycleAllowance']) else None
        new_deadline = datetime.timestamp(datetime.fromisoformat(request.form['deadline'])) if 'deadline' in request.form and bool(request.form['deadline']) else None
        self.sparql_client.update_goal_set_restrictions(
            goal_set_iri,
            cycle_allowance=new_cycle_allowance,
            deadline=new_deadline,
        )
        self.derivation_client.updateTimestamp(goal_set_iri)
        self.scheduler.add_job(
            id=f'monitor_goal_set__{getShortName(goal_set_iri)}',
            func=self.monitor_goal_iterations,
            trigger='interval', seconds=self.goal_monitor_time_interval
        )
        if not self.scheduler.running:
            self.scheduler.start()
        self.logger.info("Monitor goal iteration is scheduled with a time interval of %d seconds." % (self.goal_monitor_time_interval))

        # Send email about goal set re-activation
        if self.yag is not None:
            self.send_email(
                subject=f"[{self.email_subject_prefix}] Goal Iteration Reactivated",
                contents=[
                    format_current_time(),
                    f"Iterations to pursue GoalSet {goal_set_iri} is reactivated.",
                    f"Goal Iteration will be monitored every {self.goal_monitor_time_interval} seconds.",
                    f"The new cycleAllowance: {new_cycle_allowance}."
                    f"The new deadline: {new_deadline}."
                    f"If the above restrictions are 'None', it means the restrictions remain the same as their previous values."
                ]
            )
        return jsonify({'status': 'success', 'message': f"GoalSet <{goal_set_iri}> is reactivated."})

    def get_available_labs(self):
        # TODO [future work] display the available labs to webpage dynamically after specified the chemical reaction to optimise
        # query the laboratory that has vapourtec reactors
        return self.sparql_client.get_all_laboratories()


def format_current_time() -> str:
    return str(time.strftime("%Y-%m-%d %H:%M:%S", time.localtime())) + f" {str(time.localtime().tm_zone)}"
