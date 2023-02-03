from flask import Flask

import speedLoadMap.getEmissions as getEmissions
import aermod.postprocess_aermod as postprocess_aermod

app = Flask(__name__)
app.register_blueprint(getEmissions.getEmissions_bp)
app.register_blueprint(postprocess_aermod.get_aermod_geojson_bp)

if __name__ == "__main__":
    app.run()
