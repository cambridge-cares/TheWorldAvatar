from flask import Flask
import episode.getGeoJSON as getGeoJSON
import speedLoadMap.getEmissions as getEmissions

app = Flask(__name__)
app.register_blueprint(getGeoJSON.getGeoJSON_bp)
app.register_blueprint(getEmissions.getEmissions_bp)

if __name__ == "__main__":
    app.run()
