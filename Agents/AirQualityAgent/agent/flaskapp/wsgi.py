################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 18 Apr 2022                            #
################################################

# Start Web Server Gateway Interface (WSGI) as simple web server to forward
# requests to actual application and initialise recurring tasks

import os
from pathlib import Path

from agent.flaskapp import create_app
from apscheduler.schedulers.background import BackgroundScheduler
from pytz import utc

from agent.datainstantiation.readings import update_all_stations
from agent.dataretrieval.stations import create_json_output_files

# Add recurring background tasks
# 1) Assimilate latest time series data once per day
# 2) Write latest output files once per day
sched = BackgroundScheduler(daemon=True)
sched.add_job(update_all_stations, trigger='cron', hour='5', timezone=utc)
# Create path to output directory
# (dependent on whether called from Docker container or as local agent)
outdir = os.path.join(Path(__file__).parent.parent.parent, 'output')
sched.add_job(create_json_output_files, trigger='cron', hour='7', 
              kwargs={'outdir': str(outdir)}, timezone=utc)
sched.start()

app = create_app()


if __name__ == "__main__":
    app.run(host='localhost:5000')
