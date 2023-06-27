################################################
# Authors: Feroz Farazi (msff2@cam.ac.uk)      #    
# Date: 27 June 2023                           #
################################################

# Start Web Server Gateway Interface (WSGI) as simple web server to forward
# requests to actual application and initialise recurring tasks

# To avoid unnecessary logging information from py4j package, set logger level before 
# first creation of JPS_BASE_LIB module view (i.e. jpsBaseLibView = jpsBaseLibGW.createModuleView())
import logging
logging.getLogger("py4j").setLevel(logging.ERROR)

from pytz import utc

from agent.flaskapp import create_app

app = create_app()


if __name__ == "__main__":
    app.run(host='localhost:5000')
