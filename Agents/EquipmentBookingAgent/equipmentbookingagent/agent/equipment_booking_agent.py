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

        # add the route for the equipment booking
        self.app.add_url_rule('/booking', 'equipment_booking', self.confirmation_page, methods=['GET','POST'])

    def default(self):
        """Instruction for the EquipmentBookingAgent usage."""
        msg = "Welcome to the EquipmentBookingAgent!<BR>"
        msg += "This is a booking agent that is capable of making bookings for equipment.<BR>"
        msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar<BR>"    
        return msg


    def confirmation_page(self):
        if request.method == 'POST':
            parameters = request.form
            self.logger.info(f"Received a goal request with parameters: {parameters}")

            errors_list = list()
            if "user_def" in parameters:
                selected_user=parameters['user_def']
            else:
                selected_user = None
                errors_list.append('No user was selected.')

            if "equipment_def" in parameters:
                selected_equipment=parameters['equipment_def']
            else:
                selected_equipment = None
                errors_list.append('No equipment was selected.')

            if parameters['bookingStart'] == '':
                selected_start = 0
                errors_list.append('No valid date and time for booking start were chosen.')
            else:
                selected_start = datetime.timestamp(datetime.fromisoformat(parameters['bookingStart']))
            
            if parameters['bookingEnd'] == '':
                selected_end = 0
                errors_list.append('No valid date and time for booking end were chosen.')
            else:
                selected_end = datetime.timestamp(datetime.fromisoformat(parameters['bookingEnd']))

            if selected_start > 0 and selected_end > 0:
                if selected_end <= selected_start:
                    errors_list.append('The booking start needs to be before booking end.')

            if len(errors_list) == 0:
                conflicting_bookings = self.sparql_client.get_all_conflicting_bookings(selected_equipment,selected_start,selected_end)
                print(conflicting_bookings)
                if len(conflicting_bookings) > 0:
                    return render_template('equipment_booking_confirm_booking.html',
                                   header_text='Booking unavailable', content_primer='It conflicts with the following existing bookings:' , content_text=conflicting_bookings)
                else:
                    booking = self.sparql_client.create_booking_within_system(selected_equipment,selected_user,selected_start,selected_end)
                    return render_template('equipment_booking_confirm_booking.html',
                                   header_text='Booking successful', content_primer='A new booking was saved:' , content_text=booking)
            else:
                error_text = ""
                for errs in errors_list:
                    error_text = error_text + " " + errs
                return render_template('equipment_booking_confirm_booking.html',
                                   header_text='Faulty booking request', content_primer='The following errors occured:' , content_text=error_text)


            #Faulty booking request
            #Booking unavailable

    def booking_page(self):
        return render_template(
            'equipment_booking_make_booking.html',
            lab_users=[{'iri': usr.instance_iri, 'display': usr.name} for usr in self.sparql_client.get_all_users()],
            bookable_assets=[{'iri': eq.hasBookingSystem.instance_iri, 'display': eq.label, 'id': eq.hasItemInventoryIdentifier,
                              'supplier': eq.isSuppliedBy, 'manufacturer': eq.isManufacturedBy, 'location': eq.isLocatedIn} for eq in self.sparql_client.get_all_bookable_equipment()],
        )


def format_current_time() -> str:
    return str(time.strftime("%Y-%m-%d %H:%M:%S", time.localtime())) + f" {str(time.localtime().tm_zone)}"
