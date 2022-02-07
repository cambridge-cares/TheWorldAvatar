##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date:  27 Jan 2022                      #
##########################################

# Get settings and functions from the kg utils module
import kg_utils
# Get the JVM module view (via jpsBaseLibGateWay instance) from the jpsSingletons module to access
# the TimeSeriesClient in the JPB_BASE_LIB
from jpsSingletons import jpsBaseLibView

def generate_all_visualisation_data():
    """
       Generates all data for Gas Grid visualisation.
    """
    # Set Mapbox API key in DTVF 'index.html' file
    kg_utils.set_mapbox_apikey()

    # Initialise remote KG client with only query endpoint specified
    KGClient = jpsBaseLibView.RemoteStoreClient(kg_utils.QUERY_ENDPOINT)

    # Retrieve Java's Instant class to initialise TimeSeriesClient
    Instant = jpsBaseLibView.java.time.Instant
    instant_class = Instant.now().getClass()
    # Initialise TimeSeriesClass
    TSClient = jpsBaseLibView.TimeSeriesClient(instant_class, kg_utils.PROPERTIES_FILE)


if __name__ == '__main__':
    """
    If this module is executed, it will generate all data required for the
    Digital Twin Visualisation Framework.
    """
    generate_all_visualisation_data()