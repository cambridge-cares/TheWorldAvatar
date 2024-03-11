################################################
# Authors: Jiying Chen (jc2341@cam.ac.uk) #
# Date: Current Date                           #
################################################

# Start Web Server Gateway Interface (WSGI) as a simple web server to forward
# requests to the actual application and initialise recurring tasks

import logging
logging.getLogger("py4j").setLevel(logging.INFO)

from pytz import utc
from apscheduler.schedulers.background import BackgroundScheduler
from flaskapp import create_app
# Adjust the import path based on your project structure
from agent.datainstantiation.data_instantiation import main as instantiate_gps_data

# Initialise background scheduler and add a recurring background task
# Here, you can define how often you want your data instantiation process to run
# For example, setting it to run daily at midnight
sched = BackgroundScheduler(daemon=True)
sched.add_job(instantiate_gps_data, trigger='cron', hour=0, minute=0, second=0, timezone=utc)
sched.start()

app = create_app()

if __name__ == "__main__":
    app.run(host='localhost', port=5100)
