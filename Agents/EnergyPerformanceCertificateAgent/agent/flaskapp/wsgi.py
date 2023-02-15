################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 18 Apr 2022                            #
################################################

# Start Web Server Gateway Interface (WSGI) as simple web server to forward
# requests to actual application and initialise recurring tasks

from agent.flaskapp import create_app
from agent.datainstantiation.epc_instantiation import instantiate_epc_data_for_all_postcodes
from apscheduler.schedulers.background import BackgroundScheduler


# Add recurring background task to assimilate latest EPC data every 6 weeks
# "The department will publish register data every four to six months for new EPCs
# and DECs or where the status of the EPC or DEC has changed" 
# (https://epc.opendatacommunities.org/docs/guidance#faq-updates)
sched = BackgroundScheduler(daemon=True)
sched.add_job(instantiate_epc_data_for_all_postcodes, trigger='interval', weeks=6)
sched.start()

app = create_app()


if __name__ == "__main__":
    app.run(host='localhost:5000')
