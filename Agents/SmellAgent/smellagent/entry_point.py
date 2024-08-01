import logging
from flask import Flask, jsonify
from smellagent.utils.tools import read_properties_file
from smellagent.agent.smellagent import SmellAgent

logging.getLogger("py4j").setLevel(logging.ERROR)
logger = logging.getLogger('prod')

app = Flask(__name__)

@app.route('/predict', methods=['GET'])
def predict_route():
    agent = create_agent()
    data, last_time = agent.get_latest_sensor_data()
    prediction = agent.predict(data)
    return jsonify({'timestamp': str(last_time) if last_time else "N/A", 'prediction': prediction})

def create_agent():
    # Read the properties file
    props = read_properties_file('./smellagent/resources/smellagent.properties')

    # Extracting the settings
    agent = SmellAgent(
        db_org=props['db.influxdb.org'],
        db_url=props['db.influxdb.url'],
        db_token=props['db.influxdb.token'],
        db_bucket=props['db.influxdb.bucket']
    )

    return agent

if __name__ == "__main__":
    app.run(host='0.0.0.0', port=9047)
    logger.info('App started')