################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 15 Sep 2022                            #
################################################

# The purpose of this module is to instantiate data retrieved from
# the energy performance certificates API according to OntoBuiltEnv

import agentlogging

from epcdata.datainstantiation.epc_retrieval import obtain_data_for_certificate


# Initialise logger
logger = agentlogging.get_logger("prod")


def instantiate_data_for_certificate(lmk_key: str, endpoint='domestic'):
    """
    Retrieves EPC data for provided certificate from given endpoint

    Arguments:
        lmk_key - certificate id (i.e. individual lodgement identifier)
        endpoint (str) - EPC endpoint from which to retrieve data
                            ['domestic', 'non-domestic', 'display']
    Returns:
        Dictionary of relevant EPC data (empty dictionary if no data available)
    """

    # Retrieve EPC data
    epc_data = obtain_data_for_certificate(lmk_key, endpoint)
    # Instantiate data (if successfully retrieved)
    if epc_data:
        # Check if same EPC is already instantiated
        

        # Condition data
        pass
        
        # Map data to OntoBuiltEnv data model

    
        # Create instantiation query

if __name__ == '__main__':

    # Download and store all Domestic EPC data from API for data analysis
    instantiate_data_for_certificate('fadff9d58f3539ef0096883e195bbe93e00fc7eb4af4ecf824e991a429335557')
