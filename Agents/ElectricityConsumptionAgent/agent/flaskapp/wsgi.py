################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

# Start Web Server Gateway Interface (WSGI) as simple web server to forward
# requests to actual application and initialise recurring tasks

# To avoid unnecessary logging information from py4j package, set logger level before 
# first creation of JPS_BASE_LIB module view (i.e. jpsBaseLibView = jpsBaseLibGW.createModuleView())
import logging
logging.getLogger("py4j").setLevel(logging.INFO)

from pytz import utc
from apscheduler.schedulers.background import BackgroundScheduler

from agent.flaskapp import create_app
from agent.datainstantiation.readings import upload_all


# Initialise background scheduler and add recurring background task to 
# assimilate latest time series data once per year
sched = BackgroundScheduler(daemon=True)
sched.add_job(upload_all, trigger='cron', day='1', month='1', hour='0', minute='0', timezone=utc)
sched.start()

app = create_app()


if __name__ == "__main__":
    app.run(host='localhost', port=5000)
