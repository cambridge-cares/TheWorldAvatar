################################################
# Authors: Magnus Mueller (mm2692@cam.ac.uk)   #
# based on: Markus Hofmeister (mh807@cam.ac.uk)#
# Date: 30 Nov 2022                            #
################################################
# this is the entry point for the WSGI server, start this to run the application


from forecasting.flaskapp import create_app
from py4jps import agentlogging

# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


app = create_app()

if __name__ == "__main__":
    app.run(host='127.0.0.1', port="5000")
    logger.info('done')
