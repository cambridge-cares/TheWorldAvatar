################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 18 Oct 2022                            #
################################################

# Start Web Server Gateway Interface (WSGI) as simple web server to forward
# requests to actual application and initialise recurring tasks

from landregistry.flaskapp import create_app
from landregistry.datainstantiation.sales_instantiation import update_all_transaction_records
from apscheduler.schedulers.background import BackgroundScheduler


# Add recurring background task to assimilate latest HM Land Registry Price Paid Data
# every 4 weeks ("HM Land Registry publish Price Paid Data for England and Wales on a monthly basis." 
# (https://landregistry.data.gov.uk/app/root/doc/ppd)
sched = BackgroundScheduler(daemon=True)
sched.add_job(update_all_transaction_records, trigger='interval', weeks=4)
sched.start()

app = create_app()


if __name__ == "__main__":
    app.run(host='localhost:5000')
