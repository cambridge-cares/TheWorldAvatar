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


def define_optimisation_setup(kg_client: KGClient, ts_client: TSClient):
    """
    ...
    
    Arguments:
        kg_client {KGClient} -- pre-initialised SPARQL client
        ts_client {TSClient} -- pre-initialised TimeSeries client
    
    Returns:
        setup {dict} -- dictionary describing the full optimisation setup with
                        structure shown below
        index {DateTimeIndex} -- DateTimeIndex for optimisation interval
        (internally replaced with 'dummy' integer index for consistency reasons)

        ###  Required optimisation model input parameters  ###    
        {
        # Market prices
            'mp1':        # gas price boiler, €/MWh (wrt ho)
            'mp2':        # gas price gt, €/MWh (wrt ho)
            'mp3':        # electricity spot price, €/MWh
            'mp4':        # co2 price, €/t
            'mp5':        # chp bonus, €/MWh
            'mp6':        # grid savings, €/MWh
        # Gas properties
            'gp1':        # ho, kWh/m³
            'gp2':        # hu, kWh/m³
            'gp3':        # co2 factor, t_CO2/MWh_g (wrt hu)
        # (List of) heat sourcing contract(s)
            'sc1':        # name
            'sc2':        # annual sourcing limits [min, max], MWh/a
            'sc3':        # base unit price [p_reg, p_red], €/MWh
            'sc4':        # current heat unit price, €/MWh
            'sc5':        # associated heating grid entry point
            'sc6':        # availability
            'sc7':        # technical min. supply, MWh/h
            'sc8':        # technical max. supply, MWh/h
            'sc9':        # (annual) heat sourcing history, MWh/h
        # (List of) conventional heat boiler(s)
            'hb1':        # name(s)
            'hb2':        # capacity, MW
            'hb4':        # gas consumption/demand models (wrt hu)
            'hb5':        # start up cost, €/startup
            'hb6':        # shut down cost, €/shut-down
            'hb7':        # wear cost, €/MWh
            'hb8':        # availability
            'hb9':        # labour cost
            'hb10':       # (annual) heat generation history, MWh/h
        # (List of) gas turbine(s)
            'gt1':        # name
            'gt2':        # max. el. load, MW - irrelevant since using generator model
            'gt3':        # max. heat load, MW
            'gt4':        # min. heat load (equiv. to 70% el. load), MW
            'gt6':        # gas consumption/demand model (wrt hu)
            'gt7':        # electricity output model
            'gt8':        # minimum idle time, h
            'gt9':        # wear cost, €/h
            'gt10':       # availability
            'gt11':       # labour cost, €/h
            'gt12':       # start up cost, €/startup
            'gt13':       # shut down cost, €/shut-down
            'gt14':       # (annual) heat generation history, MWh/h
        # District heating grid
            'dh1':        # name
            'dh2':        # dict of heat entry points
            'dh3':        # heat entry point names
            'dh4':        # minimum circulation, m³/h
            'dh5':        # network pressures, bar
            'dh6':        # flow temperatures (Vorlauf), °C
            'dh7':        # return temperatures (Ruecklauf), °C
        # Municipal utility company (contracts, boilers, and gas turbines not yet assigned)
            'mu1':        # name
            'mu2': [],    # list of conv_boilers
            'mu3': [],    # list of gas turbines
            'mu4': [],    # list of sourcing contracts
            'mu5': None,  # fuel/gas properties object
            'mu6': None,  # attached district heating grid
            'mu7':        # q_demand, MWh/h
        }
    """
    
    # Initialise setup dictionary
    setup = {}
    
    # Add gas properties
    gp = kg_client.get_gas_properties()    
    setup.update(gp)    
    
    return setup, 2
    

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