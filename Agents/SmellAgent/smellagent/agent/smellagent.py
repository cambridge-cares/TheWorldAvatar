from joblib import load
import influxdb_client 

class SmellAgent:

    def __init__(self, db_org, db_url, db_token, db_bucket, model_path='./smellagent/resources/model.joblib'):
        self.model = load(model_path)
        self.client = influxdb_client.InfluxDBClient(url=db_url, token=db_token, org=db_org)
        self.bucket = db_bucket
        self.org = db_org

    def get_latest_sensor_data(self):
        query_api = self.client.query_api()
        query = f'from(bucket: "{self.bucket}") |> range(start: -7d) |> last()'
        tables = query_api.query(org=self.org, query=query)

        # Initialize empty dictionary to store sensor readings
        sensor_data = {}
        last_time = None  # To store the timestamp of the last record

        for table in tables:
            for record in table.records:
                measurement_type = record['_measurement']
                value = record['_value']
                sensor_data[measurement_type] = value
                last_time = record['_time']  # Update the timestamp for each record

        return sensor_data, last_time

    def predict(self, data):
        aqi = data.get('aqi', 0)  # Default to 0 if not found
        humidity = data.get('humidity', 0)
        pressure = data.get('pressure', 0)
        temperature = data.get('temperature', 0)
        
        prediction = self.model.predict([[aqi, humidity, pressure, temperature]])

        return prediction[0]