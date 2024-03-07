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

from equipmentbookingagent.kg_operations import EquipmentBookingSparqlClient
from equipmentbookingagent.data_model import *


class FlaskConfig(object):
    """
        This class provides the configuration for flask app object. Each config should be provided as constant. For more information, visit https://flask.palletsprojects.com/en/2.0.x/config/.
    """
    SCHEDULER_API_ENABLED = True


class EquipmentBookingAgent(ABC):

    def __init__(
        self,
        kg_url: str,
        kg_update_url: str = None,
        kg_user: str = None,
        kg_password: str = None,
        app: Flask = Flask(__name__, template_folder="/app/templates"),
        flask_config: FlaskConfig = FlaskConfig(),
        logger_name: str = "dev",
    ):
        """
            This method initialises the instance of EquipmentBookingAgent.

            Arguments:
                kg_url - SPARQL query endpoint, an example: "http://localhost:8080/blazegraph/namespace/triplestore/sparql"
                kg_update_url - SPARQL update endpoint, will be set to the same value as kg_url if not provided, an example: "http://localhost:8080/blazegraph/namespace/triplestore/sparql"
                kg_user - username used to access the SPARQL query/update endpoint specified by kg_url/kg_update_url
                kg_password - password that set for the kg_user used to access the SPARQL query/update endpoint specified by kg_url/kg_update_url
                app - flask app object, an example: app = Flask(__name__)
                flask_config - configuration object for flask app, should be an instance of the class FlaskConfig provided as part of this package
                logger_name - logger names for getting correct loggers from py4jps.agentlogging package, valid logger names: "dev" and "prod", for more information, visit https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_BASE_LIB/python_wrapper/py4jps/agentlogging/logging.py
        """

        # initialise flask app with its configuration
        self.app = app
        self.app.config.from_object(flask_config)


        # assign KG related information
        self.kg_url = kg_url
        self.kg_update_url = kg_update_url if kg_update_url is not None else kg_url
        self.kg_user = kg_user
        self.kg_password = kg_password


        # initialise the EquipmentBookingSparqlClient
        self.sparql_client = EquipmentBookingSparqlClient(
            query_endpoint=self.kg_url, update_endpoint=self.kg_update_url,
            kg_user=self.kg_user, kg_password=self.kg_password
        )

        # initialise the logger
        self.logger = agentlogging.get_logger(logger_name)

        # add the default routes for the flask app
        self.app.add_url_rule('/', 'root', self.booking_page, methods=['GET'])

        # add the route for the rxn opt goal specification
        self.app.add_url_rule('/booking', 'equipment_booking', self.booking_page, methods=['GET'])

        # initialise the current_active_goal_set as None
        self.current_active_goal_set = None

    def default(self):
        """Instruction for the EquipmentBookingAgent usage."""
        msg = "Welcome to the EquipmentBookingAgent!<BR>"
        msg += "This is a booking agent that is capable of making bookings for equipment.<BR>"
        msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar<BR>"    
        return msg


    def booking_page(self):
        return render_template(
            'equipment_booking_make_booking.html',
            lab_user_iri=[{'iri': usr, 'display': usr} for usr in self.sparql_client.get_all_users_iri()],
            bookable_assets_iri=[{'iri': eq, 'display': eq} for eq in self.sparql_client.get_all_bookable_equipment_iri()],
        )


def format_current_time() -> str:
    return str(time.strftime("%Y-%m-%d %H:%M:%S", time.localtime())) + f" {str(time.localtime().tm_zone)}"
