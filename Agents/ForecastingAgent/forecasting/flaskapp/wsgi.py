################################################
# Authors: Magnus Mueller (mm2692@cam.ac.uk)   #
# Date: 30 Nov 2022                            #
################################################

from forecasting.flaskapp import create_app

app = create_app()
app.config['JSONIFY_PRETTYPRINT_REGULAR'] = True

if __name__ == "__main__":
    print('start')
    app.run(host='127.0.0.1', port="5000")
    print('done')
