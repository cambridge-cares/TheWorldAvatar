from equipmentbookingagent.conf import config_equipment_booking_agent
from equipmentbookingagent.agent import EquipmentBookingAgent

from flask import Flask
import os

def create_app():
    equipment_booking_agent_config = config_equipment_booking_agent()

    agent = EquipmentBookingAgent(
        kg_url=equipment_booking_agent_config.SPARQL_QUERY_ENDPOINT,
        kg_update_url=equipment_booking_agent_config.SPARQL_UPDATE_ENDPOINT,
        kg_user=equipment_booking_agent_config.KG_USERNAME,
        kg_password=equipment_booking_agent_config.KG_PASSWORD,
        logger_name="prod",
        app=Flask(__name__, template_folder=os.path.join(os.path.dirname(os.path.dirname(os.path.realpath(__file__))), "templates")),
    )

    return agent.app

if __name__ == '__main__':
    app = create_app()
    app.run(debug=True)