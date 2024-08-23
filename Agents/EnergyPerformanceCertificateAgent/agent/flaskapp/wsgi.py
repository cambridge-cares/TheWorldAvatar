################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 18 Apr 2022                            #
################################################

# Start Web Server Gateway Interface (WSGI) as simple web server to forward
# requests to actual application and initialise recurring tasks

from celery.schedules import crontab

from agent.flaskapp import create_app


# To use celery commands, Celery needs an app object
app = create_app()
celery_app = app.extensions["celery"]

# Add recurring tasks to Celery beat scheduler
# 1) assimilate latest EPC data
# "The department will publish register data every four to six months for new EPCs
# and DECs or where the status of the EPC or DEC has changed" 
# (https://epc.opendatacommunities.org/docs/guidance#faq-updates)
#
# 2) update PostGIS database (i.e. changed usages) every second of the month
celery_app.conf.beat_schedule = {
    'assimilate_epcs': {
        'task': 'agent.flaskapp.inputtasks.tasks.task_instantiate_epc_data_for_all_uprns',
        'schedule': crontab(minute=0, hour=0, day_of_month=1)
    },
    'update_ocgml_infos': {
        'task': 'agent.flaskapp.inputtasks.tasks.task_add_ocgml_building_data',
        'schedule': crontab(minute=0, hour=12, day_of_month=1)
    },  
}


if __name__ == "__main__":
    app.run(host='localhost:5000')
