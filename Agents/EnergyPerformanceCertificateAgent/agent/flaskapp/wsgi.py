################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 18 Apr 2022                            #
################################################

# Start Web Server Gateway Interface (WSGI) as simple web server to forward
# requests to actual application and initialise recurring tasks

from agent.flaskapp import create_app
from agent.datainstantiation.epc_instantiation import instantiate_epc_data_for_all_postcodes, \
                                                      add_ocgml_building_data
from apscheduler.schedulers.background import BackgroundScheduler


# Add recurring background task to assimilate latest EPC data
# "The department will publish register data every four to six months for new EPCs
# and DECs or where the status of the EPC or DEC has changed" 
# (https://epc.opendatacommunities.org/docs/guidance#faq-updates)
sched = BackgroundScheduler(daemon=True)
# Update EPC data every first of the month
# sched.add_job(instantiate_epc_data_for_all_postcodes, trigger='cron', day=1,
#               kwargs={'epc_endpoint': None})
sched.add_job(instantiate_epc_data_for_all_postcodes, trigger='interval', minutes=100,
              kwargs={'epc_endpoint': None})
# Update PostGIS database (i.e. changed usages) every second of the month
# sched.add_job(add_ocgml_building_data, trigger='cron', day=2)
sched.add_job(add_ocgml_building_data, trigger='interval', minutes=105)

sched.start()

# To use celery commands, Celery needs an app object
app = create_app()
celery_app = app.extensions["celery"]


if __name__ == "__main__":
    app.run(host='localhost:5000')
