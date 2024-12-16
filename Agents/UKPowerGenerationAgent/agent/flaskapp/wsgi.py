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
from apscheduler.schedulers.background import BackgroundScheduler
from apscheduler.triggers.interval import IntervalTrigger
from datetime import datetime, timedelta


from agent.flaskapp import create_app
from agent.scriptmapquery.BMRS_API_Input_JA_7 import download_bmrs_data

# Executes download_bmrs_data immediately to download data
def execute_download_bmrs_data():
    download_bmrs_data()

# Initialises background scheduler and add recurring background task to 
# assimilate latest time series data once per day
sched = BackgroundScheduler(daemon=True)
# Adds the immediate execution of download_bmrs_data as a job
download_bmrs_data()  # Execute the download immediately when the code starts
interval_trigger = IntervalTrigger(days=10)
next_run_time = datetime.now() + timedelta(days=10)  # Set the first run to occur 10 days from now
sched.add_job(download_bmrs_data, trigger=interval_trigger, next_run_time=next_run_time)
sched.start()

app = create_app()


if __name__ == "__main__":
    app.run(host='localhost:5000')
