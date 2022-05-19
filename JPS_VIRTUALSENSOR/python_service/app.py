from flask import Flask
import episode.getGeoJSON as getGeoJSON

app = Flask(__name__)
app.register_blueprint(getGeoJSON.getGeoJSON_bp)

if __name__ == "__main__":
    app.run()
