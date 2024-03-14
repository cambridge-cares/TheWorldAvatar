from flask import Flask, jsonify, request
from .trading import Trading
import logging
import numpy as np

# Create the Flask app object
app = Flask(__name__)

# Check whether it is running in a stack
def check_stack_status():
    if 'stack' in request.args:
        try:
            if str(request.args['stack']).lower() in ['true', '1', 't', 'y', 'yes']:
                logging.info("The stack parameter was set to true. Looking for stack blazegraph and RDB. ")
                global DB_QUERY_URL, DB_QUERY_USER, DB_QUERY_PASSWORD, DB_UPDATE_URL, DB_UPDATE_USER, DB_UPDATE_PASSWORD, QUERY_ENDPOINT, UPDATE_ENDPOINT
                from NTUP2PEnergyAgent.stack_utils.stack_configs import QUERY_ENDPOINT_STACK, UPDATE_ENDPOINT_STACK
                from NTUP2PEnergyAgent.stack_utils.stack_configs import DB_UPDATE_URL_STACK, DB_UPDATE_USER_STACK, DB_UPDATE_PASSWORD_STACK
                from NTUP2PEnergyAgent.stack_utils.stack_configs import DB_QUERY_URL_STACK, DB_QUERY_USER_STACK, DB_QUERY_PASSWORD_STACK
                DB_QUERY_URL = DB_QUERY_URL_STACK
                DB_QUERY_USER = DB_QUERY_USER_STACK
                DB_QUERY_PASSWORD = DB_QUERY_PASSWORD_STACK
                DB_UPDATE_URL = DB_UPDATE_URL_STACK
                DB_UPDATE_USER = DB_UPDATE_USER_STACK
                DB_UPDATE_PASSWORD = DB_UPDATE_PASSWORD_STACK
                QUERY_ENDPOINT = QUERY_ENDPOINT_STACK
                UPDATE_ENDPOINT = UPDATE_ENDPOINT_STACK
                logging.info("QUERY ENDPOINT: "+QUERY_ENDPOINT+" UPDATE_ENDPOINT: "+UPDATE_ENDPOINT)
            else:
                logging.info("The stack parameter was set to false. Looking for local blazegraph and RDB. ")
        except ValueError:
            logging.error("Unable to parse stack parameter.")
            return "Unable to interpret stack parameter ('%s') as a string." % request.args['stack']
    else:
        logging.error("Unable to parse stack parameter.")


# Define a route for API requests
@app.route('/', methods=['GET'])
def default():  
    check_stack_status()
    
    
    ################## some variables for testing
    max_iter=100
    nodal_price = 20

    #      bus max min a b c
    buyer_info = np.array([[4,	2.20000000000000,	0.0,	-0.100000000000000,	4.50000000000000],
                        [7,	1.30000000000000,	0.0,	-0.200000000000000,	5.0],
                        [11,	1.60000000000000,	0.0,	-0.0500000000000000,	5.0],
                        [15,	1.70000000000000,	0.0,	-0.0500000000000000,	4.80000000000000],
                        [20,	1.50000000000000,	0.0,	-0.0500000000000000,	4.0],
                        [24,	2.50000000000000,	0.0,	-0.100000000000000,	5.0],
                        [30,	2.40000000000000,	0.0,	-0.100000000000000,	5.0]])

    seller_info = np.array([[18,	1.60000000000000,	0.0,	0.0300000000000000,	3.20000000000000],
                        [22,	2.30000000000000,	0.0,	0.0200000000000000,	4.0],
                        [25,	2.90000000000000,	0.0,	0.0300000000000000,	3.0],
                        [31,	2.50000000000000,	0.0,	0.0400000000000000,	4.50000000000000],
                        [2,	2.50000000000000,	0.0,	0.0500000000000000,	3.20000000000000],
                        [6,	3.50000000000000,	0.0,	0.0600000000000000,	3.80000000000000]])

    try:
        trading = Trading(max_iter)
        result = trading.trade(buyer_info,seller_info,nodal_price)
        logging.info(result)
        print(result)
        return jsonify("Success")
    except ValueError as ex:
            return str(ex)


    # Check arguments (query parameters)
  #  logger.info("Checking arguments...")
  #  if 'val' in request.args:
  #      try:
  #          val = float(request.args['val'])
  #      except ValueError:
  #          logger.error("Unable to parse number.")
  #          return "Unable to interpret val ('%s') as a float." % request.args['val']
  #  else:
  #      return "Error: No 'val' parameter provided."

   