################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 18 Apr 2022                            #
################################################

# Start Web Server Gateway Interface (WSGI) as simple web server to forward
# requests to actual application and initialise recurring tasks

# To avoid unnecessary logging information from py4j package, set logger level before 
# first creation of JPS_BASE_LIB module view (i.e. jpsBaseLibView = jpsBaseLibGW.createModuleView())
import logging
logging.getLogger("py4j").setLevel(logging.ERROR)

from apscheduler.schedulers.background import BackgroundScheduler
from pytz import utc

from agent.datainstantiation.warnings import update_warnings
from agent.flaskapp import create_app


# Initialise background scheduler and add recurring background task to 
# assimilate latest time series data once per day
sched = BackgroundScheduler(daemon=True)
sched.add_job(update_warnings, trigger='cron', minute=0, timezone=utc)
sched.start()

app = create_app()


if __name__ == "__main__":
    app.run(host='localhost:5000')
