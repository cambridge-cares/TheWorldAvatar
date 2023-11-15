################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 25 Sep 2023                            #
################################################

# The purpose of this module is to provide the actual optimisation logic and
# methods for the DHOptimisationAgent

import pandas as pd
import numpy as np
import CoolProp.CoolProp as CP

from dhoptimisation.utils import *
from dhoptimisation.datamodel.iris import *
from dhoptimisation.datamodel.unit_mapping import UNITS
from dhoptimisation.kgutils.kgclient import KGClient
from dhoptimisation.kgutils.tsclient import TSClient


def define_optimisation_setup(kg_client: KGClient, ts_client: TSClient,
                              consumption_models: dict, cogen_models: dict,
                              optimisation_input_iris: dict, 
                              opti_start_dt: str, opti_end_dt: str):
    """
    Returns a dictionary describing the full SWPS optimization setup, with first
    level keys referring to object instances, i.e., ['market_prices', 'gas_properties',
    'sourcing_contracts', 'heat_boilers', 'gas_turbines', 'district_heating_grid',
    'municipal_utility'] and values describing all parameters to create the respective
    objects
    
    Arguments:
        kg_client {KGClient} -- pre-initialised SPARQL client
        ts_client {TSClient} -- pre-initialised TimeSeries client
        consumption_models {dict} -- dict with pre-trained gas consumption models;
                                     generator IRIs as keys and model objects as values
        cogen_models {dict} -- dict with pre-trained electricity co-gen models;
                               generator IRIs as keys and model objects as values
        optimisation_input_iris {dict} -- optimisation derivation inputs as returned
                                          by `validate_input_values`
        opti_start_dt {str} -- optimisation start datetime as string
        opti_end_dt {str} -- optimisation end datetime as string
            
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
    
    ###########################################################################
    # 1) Construct overall optimisation setup parameter dict by querying KG
    
    logger.info('Retrieving optimisation setup inputs from KG...')
    
    # Initialise setup parameters dictionary
    params = {}
    
    # Add market prices
    mp = kg_client.get_market_prices()
    params.update(mp)
    
    # Add gas properties
    gp = kg_client.get_gas_properties()
    params.update(gp)
    
    # Add municipal utility details
    mu = kg_client.get_municipal_utility_details()
    # Initialise further "placeholder" entries required later, i.e., contracts, 
    # boilers, and gas turbines not yet assigned
    mu.update({'mu2': [], 'mu3': [], 'mu4': [], 'mu5': None, 'mu6': None,
               'mu7': optimisation_input_iris['q_demand']})    
    params.update(mu)
    
    # Add district heating grid details
    dh = kg_client.get_dh_grid_details()
    # Initialise further "placeholder" entries required later
    dh.update({'dh2': {}})
    # 1) Municipal utility
    sup = kg_client.get_dh_grid_supplier_details(dh['dh1'], OHN_MUNICIPAL_UTILITY)
    sup.update({'dh6': optimisation_input_iris['t_flow_mu'],
                'dh7': optimisation_input_iris['t_return_mu']})
    params = extend_setup_dictionary(params, sup)
    # 2) EfW plant
    sup = kg_client.get_dh_grid_supplier_details(dh['dh1'], OHN_INCINERATIONPLANT)
    sup.update({'dh6': optimisation_input_iris['t_flow_efw'],
                'dh7': optimisation_input_iris['t_return_efw']})
    params = extend_setup_dictionary(params, sup)    
    params.update(dh)
    
    # Get all heat providers
    heat_providers = kg_client.get_heat_providers()
    
    # Add heat sourcing contracts
    for provider in heat_providers.get('efw_plant', []):
        # Get static contract details 
        # (include instance IRIs for which to retrieve ts data subsequently)
        sc = kg_client.get_sourcing_contract_properties(provider)
        # Get tiered unit price structure
        sc.update(kg_client.get_tiered_unit_prices(sc.pop('tiered_price')))        
        # Add to overall optimisation detup
        params = extend_setup_dictionary(params, sc)  
            
    # Add gas boilers
    for boiler in heat_providers.get('boilers', []):
        # Get static boiler details 
        # (include instance IRIs for which to retrieve ts data subsequently)
        hb = kg_client.get_heat_boiler_properties(boiler)
        # Set gas consumption model
        hb.update({'hb4': consumption_models[boiler]})
        # Set start-up and shut-down cost to zero
        # NOTE: Negligible/NA for conventional gas boilers
        hb.update({'hb5': 0.0, 'hb6': 0.0})
        # Add to overall optimisation detup
        params = extend_setup_dictionary(params, hb)  
            
    # Add gas turbine
    for turbine in heat_providers.get('gt', []):
        # Get static gas turbine details
        # (include instance IRIs for which to retrieve ts data subsequently)
        gt = kg_client.get_gas_turbine_properties(turbine)
        # Set gas consumption and co-gen model
        gt.update({'gt6': consumption_models[turbine]})
        gt.update({'gt7': cogen_models[turbine]})
        #NOTE: As start-up and shut-down cost are f(labour, fuel, wear cost),
        #      they will be set when initialising GT object and keys 'gt12' 
        #      and 'gt13' here are neglected
        # Add to overall optimisation detup
        params = extend_setup_dictionary(params, gt)
    
    logger.info('Optimisation inputs queried from KG with placeholder IRIs for ts data.')
    
    ###########################################################################
    # 2) Construct overall DataFrame for all time series data
    
    logger.info('Querying and validating time series data...')
    
    # Initialise list of heat generation/sourcing history IRIs
    histories = []
    # Initialise overarching ts DataFrame with 'time' column to merge data on
    all_ts = pd.DataFrame()
    all_ts.index.name = 'time'
    # Query ts data for all placeholder IRIs
    ts_data_iris = extract_iris_from_setup_dict(params)
    for iri in ts_data_iris:
        # NOTE: Optimisation assumes data in certain units; hence, query ts 
        #       data for expected units!
        rdf_type = kg_client.get_rdftype(iri)
        unit = UNITS[rdf_type]
        df = retrieve_consolidated_timeseries_as_dataframe(kg_client, ts_client,
                        instance_iri=iri, unit=unit, lowerbound=opti_start_dt, 
                        upperbound=opti_end_dt, column_name=iri)
        all_ts = all_ts.merge(df, on='time', how='outer')
        # Add heat generation/sourcing history IRIs to list
        if rdf_type in [OHN_GENERATED_HEAT_AMOUNT, OHN_PROVIDED_HEAT_AMOUNT]:
            histories.append(iri)
        
    # Condition time series collectively
    check_interval_spacing(all_ts)
    # Check for missing data
    if all_ts.isna().sum().sum() != 0:
        logger.warn('Some time series data is missing and will be forward filled.')
        logger.info(all_ts.isna().sum())
        all_ts.fillna(method='ffill', inplace=True)
    # Verify data availability for entire optimisation interval
    if not pd.Timestamp(opti_start_dt).tz_localize(None) == all_ts.index.min() or \
       not pd.Timestamp(opti_end_dt).tz_localize(None) == all_ts.index.max():
           raise_error(ValueError, 'Not all time series data available for optimisation interval.')
    # Convert boolean availabilities to one-hot encoded data
    bool_columns = all_ts.select_dtypes(include=['bool']).columns
    all_ts[bool_columns] = all_ts[bool_columns].astype(int) 
    # Reset historical heat generation/sourcing data
    all_ts[histories] = np.nan
        
    # Extract DataTime index (for later use); then drop to ensure internal consistency
    index = pd.to_datetime(all_ts.index)
    all_ts.reset_index(drop=True, inplace=True)
    
    logger.info('Queried and validated time series data.')
    
    ###########################################################################
    # 3) Replace placeholder instance IRIs with validated time series data
    #
    logger.info('Replacing placeholder IRIs with time series data...')
    
    replacements = all_ts.to_dict(orient='list')
    # Iterate through the original dictionary and apply replacements
    for key, value in params.items():
        params[key] = replace_values(replacements, value) 
    
    logger.info('Optimisation input parameters complete.')
    
    ###########################################################################
    # 4) Construct overall optimisation setup dict
    #       - constant parameters as 'normal' values 
    #       - dynamic parameters as pd.Series (to allow for indexing)
    logger.info('Constructing overall optimisation setup dictionary...')
    setup = {
        'market_prices': {'gas_q': pd.Series(params['mp1'], name='gas_q'),
                          'gas_gt': pd.Series(params['mp2'], name='gas_gt'),
                          'el_spot': pd.Series(params['mp3'], name='el_spot'),
                          'co2': pd.Series(params['mp4'], name='co2'),
                          'chp_bonus': pd.Series(params['mp5'], name='chp_bonus'),
                          'grid_save': pd.Series(params['mp6'], name='grid_save')},
        'gas_properties': {'ho': params['gp1'],
                           'hu': params['gp2'],
                           'co2_factor': params['gp3']},
        'district_heating_grid': {'name': params['dh1'],
                                  'entry_points': params['dh2']},
        'municipal_utility': {'name': params['mu1'],
                              'conv_boilers': params['mu2'],
                              'gas_turbines': params['mu3'],
                              'contracts': params['mu4'],
                              'fuel': params['mu5'],
                              'network': params['mu6'],
                              'q_demand': pd.Series(params['mu7'], name='q_demand')},
        'sourcing_contracts': [],
        'heat_boilers': [],
        'gas_turbines': [],
        'grid_entry_points': []
    }

    # Add heat sourcing contract(s) to setup
    for i in range(len(params['sc1'])):
        setup['sourcing_contracts'].append({'name': params['sc1'][i], 'qlimits_pa': params['sc2'][i],
                                            'prices': params['sc3'][i], 'current_price': params['sc4'][i],
                                            'entry_point': params['sc5'][i],
                                            'availability': pd.Series(params['sc6'][i], name='availability'),
                                            'qmin': pd.Series(params['sc7'][i], name='qmin'),
                                            'qmax': pd.Series(params['sc8'][i], name='qmax'),
                                            'q_hist': pd.Series(params['sc9'][i], name='q_hist')})
    # Add boiler(s) to setup
    for i in range(len(params['hb1'])):
        setup['heat_boilers'].append({'name': params['hb1'][i], 'capacity': params['hb2'][i],
                                      'gas_demand': params['hb4'][i], 'start_up_cost': params['hb5'][i], 
                                      'shut_down_cost': params['hb6'][i], 
                                      'wear_cost': pd.Series(params['hb7'][i], name='wear_cost'),
                                      'availability': pd.Series(params['hb8'][i], name='availability'),
                                      'labour_cost': pd.Series(params['hb9'][i], name='labour_cost'),
                                      'q_hist': pd.Series(params['hb10'][i], name='q_hist')})
    # Add gas turbine(s) to setup
    for i in range(len(params['gt1'])):
        setup['gas_turbines'].append({'name': params['gt1'][i], 'power_el': params['gt2'][i],
                                      'power_q': params['gt3'][i], 'min_load': params['gt4'][i],
                                      'gas_demand': params['gt6'][i], 'el_output': params['gt7'][i],
                                      'idle_period': params['gt8'][i],
                                      'wear_cost': pd.Series(params['gt9'][i], name='wear_cost'),
                                      'availability': pd.Series(params['gt10'][i], name='availability'),
                                      'labour_cost': pd.Series(params['gt11'][i], name='labour_cost'),
                                      'q_hist': pd.Series(params['gt14'][i], name='q_hist')})
    # Add grid entry point(s) to setup
    for i in range(len(params['dh3'])):
        setup['grid_entry_points'].append({'name': params['dh3'][i], 'min_circulation': params['dh4'][i],
                                           'pressure': pd.Series(params['dh5'][i], name='pressure'),
                                           'temp_flow': pd.Series(params['dh6'][i], name='temp_flow'),
                                           'temp_return': pd.Series(params['dh7'][i], name='temp_return')})
    logger.info('Overall optimisation setup dictionary successfully constructed.')
    
    return setup, index
    

def retrieve_consolidated_timeseries_as_dataframe(kg_client:KGClient, ts_client:TSClient,
                                                  instance_iri:str, unit=None, lowerbound=None, 
                                                  upperbound=None, column_name='value'):
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
    try:
        hist_iri, u = kg_client.get_associated_dataIRI(instance_iri=instance_iri, 
                                unit=unit, forecast=False)
        ts_hist = ts_client.retrieve_timeseries_as_dataframe(dataIRI=hist_iri, 
                            column_name='historical',lowerbound=lowerbound,
                            upperbound=upperbound)
        logger.info(f'Retrieved historical ts data for {hist_iri} with unit {u}')
    except:
        logger.info(f'No historical ts data available for {instance_iri}.')
        # Create empty DataFrame with 'time' index to avoid merging error
        ts_hist = pd.DataFrame(columns=['historical'])
        ts_hist.index.name = 'time'
        
    # 2) Forecast data
    try:        
        fc_iri, u = kg_client.get_associated_dataIRI(instance_iri=instance_iri, 
                                unit=unit, forecast=True)
        ts_fc = ts_client.retrieve_timeseries_as_dataframe(dataIRI=fc_iri, 
                            column_name='forecast',lowerbound=lowerbound,
                            upperbound=upperbound)
        logger.info(f'Retrieved forecast ts data for {fc_iri} with unit {u}')
    except:
        logger.info(f'No forecast ts data available for {instance_iri}.')
        ts_fc = pd.DataFrame(columns=['forecast'])
        ts_fc.index.name = 'time'
    
    # Combine time series DataFrames, with forecast values taking priority, i.e.,
    # superseding historical values
    # NOTE: This ensures that progressing optimisation can use previously optimised
    #       timesteps without altering instantiated actual historical values
    
    # Merge dfs including all time steps, with forecast taking precedence 
    merged = ts_fc.merge(ts_hist, on='time', how='outer')
    merged[column_name] = merged['forecast'].combine_first(merged['historical'])
    # Drop the auxiliary columns
    merged.drop(columns=['historical', 'forecast'], inplace=True)
    
    return merged