################################################
# Authors: Magnus Mueller (mm2692@cam.ac.uk)   #
# Date: 30 Nov 2022                            #
################################################
# this is the entry point for the WSGI server, start this to run the application


# To avoid unnecessary logging information from py4j package, set logger level before 
# first creation of JPS_BASE_LIB module view (i.e. jpsBaseLibView = jpsBaseLibGW.createModuleView())
import logging
logging.getLogger("py4j").setLevel(logging.INFO)
from forecasting.flaskapp import create_app

app = create_app()
app.config['JSONIFY_PRETTYPRINT_REGULAR'] = True

if __name__ == "__main__":
    print('start')
    app.run(host='127.0.0.1', port="5000")
    print('done')
