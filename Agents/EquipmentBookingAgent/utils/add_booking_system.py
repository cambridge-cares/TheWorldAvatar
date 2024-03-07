from equipmentbookingagent.conf import config_equipment_booking_agent
from equipmentbookingagent.agent import EquipmentBookingAgent
from flask import Flask
import os
from equipmentbookingagent.kg_operations import EquipmentBookingSparqlClient
from equipmentbookingagent.data_model import *
from pyderivationagent.kg_operations import PySparqlClient

equipment_booking_agent_config = config_equipment_booking_agent()

sparql_client = PySparqlClient(
            query_endpoint=equipment_booking_agent_config.SPARQL_QUERY_ENDPOINT, 
            update_endpoint=equipment_booking_agent_config.SPARQL_UPDATE_ENDPOINT,
            kg_user=equipment_booking_agent_config.KG_USERNAME, 
            kg_password=equipment_booking_agent_config.KG_PASSWORD
        )