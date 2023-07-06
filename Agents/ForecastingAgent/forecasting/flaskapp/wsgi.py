################################################
# Authors: Magnus Mueller (mm2692@cam.ac.uk)   #
#          Markus Hofmeister (mh807@cam.ac.uk) #
# Date: 30 Nov 2022                            #
################################################

# This is the entry point for the WSGI server, start this to run the application

from py4jps import agentlogging

from forecasting.flaskapp import create_app

# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


app = create_app()

if __name__ == "__main__":
    app.run(host='localhost', port="5000")
    logger.info('App started')
