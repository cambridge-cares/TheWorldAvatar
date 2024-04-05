from equipmentbookingagent.conf import config_equipment_booking_agent
from equipmentbookingagent.agent import EquipmentBookingAgent
from flask import Flask
import os
from equipmentbookingagent.kg_operations import EquipmentBookingSparqlClient
from equipmentbookingagent.data_model import *
from pyderivationagent.kg_operations import PySparqlClient

equipment_booking_agent_config = config_equipment_booking_agent()

sparql_client = EquipmentBookingSparqlClient(
            query_endpoint=equipment_booking_agent_config.SPARQL_QUERY_ENDPOINT, 
            update_endpoint=equipment_booking_agent_config.SPARQL_UPDATE_ENDPOINT,
            kg_user=equipment_booking_agent_config.KG_USERNAME, 
            kg_password=equipment_booking_agent_config.KG_PASSWORD
        )


equipment_iris : list
technicalsystem_label: str

sparql_client.create_technicalystem_of_devices(equipment_iris=equipment_iris,system_label=technicalsystem_label)