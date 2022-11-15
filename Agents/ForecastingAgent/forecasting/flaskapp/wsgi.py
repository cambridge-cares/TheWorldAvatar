################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 18 Oct 2022                            #
################################################

# Start Web Server Gateway Interface (WSGI) as simple web server to forward
# requests to actual application and initialise recurring tasks

from forecasting.flaskapp import create_app
#from apscheduler.schedulers.background import BackgroundScheduler



app = create_app()
app.config['JSONIFY_PRETTYPRINT_REGULAR'] = True

if __name__ == "__main__":
    print('start')
    app.run(host='127.0.0.1', port="5000")
    print('done')
