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
import pandas as pd
import yagmail
import base64
import json
import time
import os
import io
from datetime import timedelta

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
        start_working_hours = '08:00',
        end_working_hours = '19:00'
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
                start_working_hours - start of work day as displayed in daily booking overview
                end_working_hours - end of work day as displayed in daily booking overview
        """

        # initialise flask app with its configuration
        self.app = app
        self.app.config.from_object(flask_config)


        # assign KG related information
        self.kg_url = kg_url
        self.kg_update_url = kg_update_url if kg_update_url is not None else kg_url
        self.kg_user = kg_user
        self.kg_password = kg_password

        # assign UI related information
        self.start_working_hours = start_working_hours
        self.end_working_hours = end_working_hours


        # initialise the EquipmentBookingSparqlClient
        self.sparql_client = EquipmentBookingSparqlClient(
            query_endpoint=self.kg_url, update_endpoint=self.kg_update_url,
            kg_user=self.kg_user, kg_password=self.kg_password
        )

        # initialise the logger
        self.logger = agentlogging.get_logger(logger_name)

        # add the default routes for the flask app
        self.app.add_url_rule('/', 'root', self.start_page, methods=['GET'])

        # add the route for the equipment booking
        self.app.add_url_rule('/booking', 'equipment_booking', self.booking_page, methods=['GET'])

        # add the route for user administration
        self.app.add_url_rule('/add_user', 'add_lab_user', self.user_admin_page, methods=['GET','POST'])

        # add the route for the booking confirmation
        self.app.add_url_rule('/booking_confirmation', 'booking_confirmation', self.confirmation_page, methods=['POST'])

        # add the route for daily booking overview
        self.app.add_url_rule('/overview', 'booking_overview', self.booking_overview, methods=['GET'])

         # add the route for equipment-specific booking overview
        self.app.add_url_rule('/view', 'bookings_view', self.equipment_overview, methods=['GET'])

        # add the route for the deletion confirmation
        self.app.add_url_rule('/delete_booking', 'deletion_confirmation', self.deletion_page, methods=['POST'])

    def deletion_page(self):
        data = request.get_json()
        if 'iri' in data:
            booking_iri = data['iri']
            try:
                self.sparql_client.remove_booking_from_system(booking_iri=booking_iri)
                return render_template('equipment_booking_confirm_deletion.html',
                                   header_text='Booking deleted', content_primer='The booking was successfully deleted from TWA', content_text=booking_iri)
            except Exception as e:
                return render_template('equipment_booking_confirm_deletion.html',
                                   header_text='Error', content_primer='Could not delete booking', content_text=str(e))        
        else:
            return render_template('equipment_booking_confirm_deletion.html',
                                   header_text='Error', content_primer='Faulty request', content_text='Booking IRI not fond')

    def equipment_overview(self):
        try:
            selected_eq = request.args.get('eq')
        except:
            selected_eq = None

        if selected_eq == None:
            bookings = []
        else:
            bookings = self.sparql_client.get_all_bookings_in_system(selected_eq)
        
        all_bookings = {}

        for booking in bookings:
            all_bookings[booking.instance_iri] = {
                'booker': booking.hasBooker,
                'start': booking.hasBookingStart.strftime('%d.%m.%Y. %H:%M'),
                'end': booking.hasBookingEnd.strftime('%d.%m.%Y. %H:%M'),
            }

        return render_template(
                'equipment_booking_delete_booking.html',
                bookable_assets=[{'iri': eq.hasBookingSystem.instance_iri, 'display': eq.hasBookingSystem.label} for eq in self.sparql_client.get_all_bookable_equipment()],
                selected_equipment = selected_eq, active_bookings = all_bookings
            )

    def default(self):
        """Instruction for the EquipmentBookingAgent usage."""
        msg = "Welcome to the EquipmentBookingAgent!<BR>"
        msg += "This is a booking agent that is capable of making bookings for equipment.<BR>"
        msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar<BR>"    
        return msg

    def start_page(self):
        return render_template('equipment_booking_main.html')

    def booking_overview(self):
        try:
            selected_date = request.args.get('date')
            this_date = datetime.strptime(selected_date, '%Y-%m-%d')
        except:
            this_date = datetime.today()
            this_date = this_date.replace(hour=0,minute=0,second=0,microsecond=0)
   
        time_slots = []
        current_time = self.start_working_hours
        while current_time <= self.end_working_hours:
            time_slots.append(current_time)
            current_time = (datetime.strptime(current_time, '%H:%M') + timedelta(minutes=15)).strftime('%H:%M')

        equipments = self.sparql_client.get_all_bookable_equipment()
        booking_data = {}
        for equipment in equipments:
            bookings = self.sparql_client.get_all_bookings_of_date(booking_system_iri=equipment.hasBookingSystem.instance_iri,
                                                                   this_day=datetime.timestamp(this_date),next_day=datetime.timestamp(this_date+ timedelta(1)))
            equipment_name = equipment.hasBookingSystem.label
            booking_data[equipment_name] = {}
            for booking in bookings:
                for time_slot in time_slots:
                    current_slot = datetime.strptime(time_slot+ ' ' + this_date.strftime('%Y-%m-%d') , '%H:%M %Y-%m-%d')
                    if current_slot >= booking.hasBookingStart and current_slot < booking.hasBookingEnd:
                        booking_data[equipment_name][current_slot.strftime('%H:%M')] = booking.hasBooker
                                            
        return render_template('equipment_booking_overview.html', time_slots=time_slots, booking_data=booking_data, selected_date=this_date.strftime('%Y-%m-%d'))


    def confirmation_page(self):
        if request.method == 'POST':
            parameters = request.form
            self.logger.info(f"Received a booking request with parameters: {parameters}")

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
            lab_users=[{'iri': usr.instance_iri, 'display': usr.name} for usr in self.sparql_client.get_all_lab_users()],
            bookable_assets=[{'iri': eq.hasBookingSystem.instance_iri, 'display': eq.hasBookingSystem.label, 'id': eq.hasItemInventoryIdentifier,
                              'supplier': eq.isSuppliedBy, 'manufacturer': eq.isManufacturedBy, 'location': eq.isLocatedIn, 
                              'assignee': eq.assignedTo, 'type': eq.clz} for eq in self.sparql_client.get_all_bookable_equipment()],
        )
    
    def user_admin_page(self):
        if request.method == 'POST':
            parameters = request.form
            self.logger.info(f"Received a user creation request with parameters: {parameters}")
            if parameters['user_name'] != "":
                new_user = self.sparql_client.create_lab_user(parameters['user_name'],parameters['first_lab_user'])
                msg = "New user added with IRI " + new_user
            else:
                msg = "No new user added. Name cannot be empty!"
        else:
            msg = ""
        return render_template(
            'equipment_booking_add_user.html',
            lab_users=[{'iri': usr.instance_iri, 'display': usr.name} for usr in self.sparql_client.get_all_lab_users()],
            request_response = msg
        )


def format_current_time() -> str:
    return str(time.strftime("%Y-%m-%d %H:%M:%S", time.localtime())) + f" {str(time.localtime().tm_zone)}"
