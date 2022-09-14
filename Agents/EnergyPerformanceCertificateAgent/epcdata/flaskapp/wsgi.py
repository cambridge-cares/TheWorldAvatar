################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 18 Apr 2022                            #
################################################

# Start Web Server Gateway Interface (WSGI) as simple web server to forward
# requests to actual application and initialise recurring tasks

import os
from pathlib import Path

from epcdata.flaskapp import create_app
from apscheduler.schedulers.background import BackgroundScheduler
from pytz import utc

#from epcdata.datainstantiation.readings import update_all_stations

#TODO: Update scheduler
# Add recurring background tasks
# 1) Assimilate latest time series data once per day
#sched = BackgroundScheduler(daemon=True)
#sched.add_job(update_all_stations, trigger='cron', hour='3', timezone=utc)
# Create path to output directory
#sched.start()

app = create_app()


if __name__ == "__main__":
    app.run(host='localhost:5000')
