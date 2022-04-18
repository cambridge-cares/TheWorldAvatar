# Start Web Server Gateway Interface (WSGI) as simple web server to forward
# requests to actual application
 
from metoffice.flaskapp import create_app
from apscheduler.schedulers.background import BackgroundScheduler

from metoffice.datainstantiation.readings import update_all_stations
from metoffice.dataretrieval.stations import create_json_output_files

# Add recurring background tasks
# 1) Assimilate latest time series data every hour
# 2) Write latest output files once per day
sched = BackgroundScheduler(daemon=True)
sched.add_job(update_all_stations, 'interval', minutes=60, max_instances=1)
sched.add_job(create_json_output_files, 'interval', minutes=24*60, 
              kwargs={'outdir': 'C:\TheWorldAvatar-git\Agents\MetOfficeAgent\output'}, 
              max_instances=1)
sched.start()

app = create_app()


if __name__ == "__main__":
    app.run(host='localhost:5000')
