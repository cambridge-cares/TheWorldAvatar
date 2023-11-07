################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 25 Sep 2023                            #
################################################

# The purpose of this module is to provide the actual optimisation logic and
# methods for the DHOptimisationAgent

import pandas as pd
import CoolProp.CoolProp as CP

from dhoptimisation.utils import logger
from dhoptimisation.datamodel.iris import *
from dhoptimisation.kgutils.kgclient import KGClient
from dhoptimisation.kgutils.tsclient import TSClient


def retrieve_consolidated_timeseries_as_dataframe(kg_client:KGClient, ts_client:TSClient,
                                                  instance_iri:str, unit=None, lowerbound=None, 
                                                  upperbound=None):
    """
    Retrieves and combines both forecast and historical time series data for
    given instance, with forecast values taking precedence
    
    Arguments:
        kg_client {KGClient} -- pre-initialised SPARQL client
        ts_client {TSClient} -- pre-initialised TimeSeries client
        instance_iri {str} -- instance IRI which can have a historical and forecast
                              time series IRI attached
                              (via om:hasValue and ts:hasForecast, respectively)
        unit {str} -- target unit associated with dataIRI
                      (if given, only dataIRIs with matching unit are returned)
        lowerbound (str): Lower bound of time series data
        upperbound (str): Upper bound of time series data
        
    Returns:
        DataFrame with consolidated forecast and historical time series;
        index: 'time', column: 'value'
    """
    
    # Retrieve time series data
    # 1) Historical data
    hist_iri, _ = kg_client.get_associated_dataIRI(instance_iri=instance_iri, 
                            unit=unit, forecast=False)
    ts_hist = pd.DataFrame(columns=['historical']) if not hist_iri else \
              ts_client.retrieve_timeseries_as_dataframe(dataIRI=hist_iri, 
                            column_name='historical',lowerbound=lowerbound,
                            upperbound=upperbound)
    # 2) Forecast data
    fc_iri, _ = kg_client.get_associated_dataIRI(instance_iri=instance_iri, 
                            unit=unit, forecast=True)
    ts_fc = pd.DataFrame(columns=['forecast']) if not fc_iri else \
            ts_client.retrieve_timeseries_as_dataframe(dataIRI=fc_iri, 
                            column_name='forecast',lowerbound=lowerbound,
                            upperbound=upperbound)
    
    # Combine time series DataFrames, with forecast values taking priority, i.e.,
    # superseding historical values
    # NOTE: This ensures that progressing optimisation can use previously optimised
    #       timesteps without altering instantiated actual historical values
    
    # Merge dfs including all time steps, with forecast taking precedence 
    merged = ts_fc.merge(ts_hist, on='time', how='outer')
    merged['value'] = merged['forecast'].combine_first(merged['historical'])
    # Drop the auxiliary columns
    merged.drop(columns=['historical', 'forecast'], inplace=True)
    
    return merged