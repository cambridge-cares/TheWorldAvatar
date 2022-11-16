"""
Load input timeseries data from the knowledge graph
"""
import os
import pickle

from district_heating.generation_optimisation import create_queries
import utils

import numpy as np
import pandas as pd
from javagateway import jpsBaseLibView

def define_data_setup(opt_period, method='efficiency', scaler=None):
    """
    Returns a dictionary describing the full SWPS optimization setup as of 2018, with first level keys referring to
    object instances, i.e., ['market_prices', 'gas_properties', 'sourcing_contracts', 'heat_boilers', 'gas_turbines',
    'district_heating_grid','municipal_utility'] and values describing all parameters to create the respective objects

    :param int opt_period: length of optimization period to prepare data for
    :param str method: string defining the method used to calculate gas demand ['efficiency', 'models']
    :param dict scaler: dictionary containing potential scaling factor for model parameters

    :returns dict setup: dictionary describing the full optimization setup for 2018
    :returns DateTimeIndex index: DateTimeIndex of all entries in length 'opt_period' (internally replaced with
                                  'dummy' integer index for consistency reasons)
    :returns DataFrame history: DataFrame containing historic heat generation by SWPS (for comparison purposes)
    """

    # Initialise TimeSeriesClass
    Instant = jpsBaseLibView.java.time.Instant
    instant_class = Instant.now().getClass()
    TSClient = jpsBaseLibView.TimeSeriesClient(instant_class, utils.PROPERTIES_FILE)

    # Execute queries to obtain optimisation inputs
    response_list, title_list = create_queries.query_optimisation_inputs()

    # Unpack nested list
    unpacked_list = [x for l in response_list for x in l]

    df_list = []

    # Append query results to dataframe
    for r in unpacked_list:
        timeseries = TSClient.getTimeSeries([r['dataIRI']])
        values = timeseries.getValues(r['dataIRI'])
        df = pd.DataFrame(list(values))
        df_list.append(df)
        dates = timeseries.getTimes()

    # Create overall dataframe and assign titles
    opt_data = pd.concat(df_list, axis=1)
    opt_data.columns = title_list

    # Set dataframe index as the dates extracted from timeseries data
    opt_data = opt_data.set_index((d.toString().replace("T", " ").replace("Z", "")) for d in dates)

    # keep only data up to length 'opt_period' time steps ('1h')
    data = opt_data.iloc[:opt_period].copy()

    # check whether ALL data is equally spaced ('1h' intervals)
    #check_interval_spacing(data)

    # extract DataTime index (for potential later use)
    index = pd.to_datetime(opt_data.index)

    # # drop DateTime index in 'data' DataFrame to ensure internal consistency
    data.reset_index(drop=True, inplace=True)

    #TODO: Update this with model created using data extracted from KG
    ###   load fitted gas consumption/demand models for heat boilers and gas turbine  ###

    if method == 'models':
        # load boiler models
        hb_demand_models = []
        for boiler in ['Kessel4', 'Kessel5', 'Kessel6']:
            path_hb_model = '..\\..\\data\\models\\gas_consumption_boilers'
            hb_demand_models.append(pickle.load(open(os.path.join(root, path_hb_model, boiler+'.sav'), 'rb')))

        # load gas turbine models
        path_gt_model = '..\\..\\data\\models\\gas_consumption_gts'
        gt_demand_model = pickle.load(open(os.path.join(root, path_gt_model, 'SWPS_GT_gas_demand.sav'), 'rb'))
        gt_el_power_model = pickle.load(open(os.path.join(root, path_gt_model, 'SWPS_GT_el_power.sav'), 'rb'))


    ##### Following part not currently included in agent (calculating non-optimised actual heat demand/cost) #####
    ###   extract heat generation history (for comparison later)   ###

    # ensure same column structure as used by 'minimize_generation_cost' and 'optimize_operating_modes'
    # history = data[['GT Waermeleistung (MW)', 'Waermemenge MHKW (MW)', 'Waermeleistung Kessel4 (MW)',
    #                 'Waermeleistung Kessel5 (MW)', 'Waermeleistung Kessel6 (MW)']].copy()
    # # align headers with optimization naming structure
    # history.columns = ['SWPS_GT', 'MHKW_ToP', 'Kessel4', 'Kessel5', 'Kessel6']

    ###   define model parameters   ###
    params = {
        # define market prices
        'mp1': data['GaspreisKessel'].squeeze(),                           # gas_q, €/MWh (wrt ho)
        'mp2': data['GaspreisGT'].squeeze(),                               # gas_gt, €/MWh (wrt ho)
        'mp3': data['Spotpreis'].squeeze(),                                 # el_spot, €/MWh
        'mp4': data['CO2Preis'].squeeze(),                                   # co2, €/t
        'mp5': [15.00 for i in range(opt_period)],                          # chp_bonus, €/MWh
        'mp6': [4.70 for i in range(opt_period)],                           # grid_savings, €/MWh
        # define gas properties
        'gp1': 11.38,                                                       # ho, kWh/m³
        'gp2': 10.272,                                                      # hu, kWh/m³
        'gp3': 0.056 * 3.6,                                                 # CO2_factor, t_CO2/MWh_g (wrt hu)
        # define (list of) heat sourcing contract(s)
        'sc1': ['MHKW_ToP'],                                                # names
        'sc2': [[15000.0, 65000.0]],                                        # annual limits [min, max], MWh/a
        'sc3': [[11.84, 11.84]],                                            # base unit price [p_reg, p_red], €/MWh
        'sc4': [np.nan],                                                    # current heat unit price, €/MWh
        'sc5': ['MHKW'],                                                    # associated heating grid entry point
        'sc6': [[1 for i in range(opt_period)]],                            # availability
        'sc7': [[1.0 for i in range(opt_period)]],                          # technical min. supply, MWh/h
        'sc8': [[11.0 for i in range(opt_period)]],                         # technical max. supply, MWh/h
        'sc9': [[np.nan for i in range(opt_period)]],                       # heat sourcing history, MWh/h
        # define (list of) conventional heat boiler(s)
        'hb1': ['Kessel4', 'Kessel5', 'Kessel6'],                           # names
        'hb2': [7.5, 7.5, 4.5],                                             # capacity, MW
        'hb3': [0.9, 0.9, 0.95],                                            # efficiency, MWh_q/MWh_g (wrt hu)
        'hb4': [None] * 3 if method == 'efficiency' else hb_demand_models,  # gas consumption/demand models (wrt hu)
        'hb5': [0.00] * 3,                                                  # start_up_cost, €/startup
        'hb6': [0.00] * 3,                                                  # shut_down_cost, €/shut-down
        'hb7': [1.50] * 3,                                                  # wear_cost, €/MWh
        'hb8': [[1 for i in range(opt_period)]] * 3,                        # availability
        'hb9': [[0.0 for i in range(opt_period)]] * 3,                      # labour_cost
        'hb10': [[np.nan for i in range(opt_period)]] * 3,                  # heat generation history, MWh/h
        # define (list of) gas turbine(s)
        'gt1': ['SWPS_GT'],                                                 # name
        'gt2': [6.5],                                                       # power_el (max. el. load), MW - irrelevant when method='models'
        'gt3': [11.7],                                                      # power_q (max heat load), MW
        'gt4': [8.2] if method == 'efficiency' else [8.8],                  # minimum heat load (equiv. to 70% el. load), MW
        'gt5': [0.88],                                                      # efficiency, (MWh_q+MWh_el)/MWh_g (wrt hu)
        'gt6': [None] if method == 'efficiency' else [gt_demand_model],     # gas consumption/demand model (wrt hu)
        'gt7': [None] if method == 'efficiency' else [gt_el_power_model],   # electricity output model
        'gt8': [5],                                                         # minimum idle time, h
        'gt9': [53.33],                                                     # wear_cost, €/h
        'gt10': [[1 for i in range(opt_period)]],                           # availability
        'gt11': [[53.23 for i in range(opt_period)]],                       # labour_cost, €/h
        'gt12': [[800.0 for i in range(opt_period)]],                       # start_up_cost, €/startup
        'gt13': [[0.0 for i in range(opt_period)]],                         # shut_down_cost, €/shut-down
        'gt14': [[np.nan for i in range(opt_period)]],                      # heat generation history, MWh/h
        # define district heating grid
        'dh1': 'Pirmasens',                                                 # name
        'dh2': {},                                                          # dict of heat entry points
        'dh3': ['HKW', 'MHKW'],                                             # heat entry point names
        'dh4': [80.0, 30.0],                                                # minimum circulation, m³/h
        'dh5': [[8.0 for i in range(opt_period)],                           # network pressures, bar
                [6.5 for i in range(opt_period)]],
        'dh6': [data['TempVorlauf'].squeeze(),                                # flow temperatures (Vorlauf), °C
                data['MHKWTempVorlauf'].squeeze()],
        'dh7': [data['TempRuecklauf'].squeeze(),                              # return temperatures (Ruecklauf), °C
                data['MHKWTempRuecklauf'].squeeze()],
        # define municipal utility company (contracts, boilers, and gas turbines not yet assigned)
        'mu1': 'SWPS',                                                      # name
        'mu2': [],                                                          # list of conv_boilers
        'mu3': [],                                                          # list of gas turbines
        'mu4': [],                                                          # list of sourcing contracts
        'mu5': None,                                                        # fuel/gas properties object
        'mu6': None,                                                        # attached district heating grid
        'mu7': data['Waermeeinspeisung'].squeeze()                               # q_demand, MWh/h
    }

    ####  Can be removed since we are not considering sensitivity analysis ####
    # potentially scale parameter(s) - primarily for sensitivity analyses
    if scaler:
        # amend price structure of sourcing contract to 'enable' volume discount influence
        params['sc3'][0][1] *= 0.95

        for p in scaler.keys():
            if p in ['hb4', 'gt6', 'gt7']:
                # adjust only intercept (not coefficient) of gas consumption and electricity output models
                for model in params[p]:
                    # gas turbine models have negative intercepts
                    if model.intercept_ < 0:
                        model.intercept_ *= (1 - scaler[p])
                    else:
                        model.intercept_ *= (1 + scaler[p])
            else:
                # extract 'base' values for parameter and convert to np.array to allow for element wise operations
                if p in ['dh4_hkw', 'dh4_mhkw']:
                    p_adj = 'dh4'
                elif p == 'sc3_r':
                    p_adj = 'sc3'
                else:
                    p_adj = p
                val = np.array(params[p_adj])

                # 'scale'/disturb each value as defined by relative scaler value
                if p == 'sc2':
                    # adjust only minimum annual limit of MHKW contract
                    val[:, 0] *= (1 + scaler[p])
                elif p == 'sc3_r':
                    # adjust only unit price incl. volume discount (positive rebate disturbance results in lower price)
                    val[:, 1] *= (1 - scaler[p])
                elif p == 'dh4_hkw':
                    # adjust minimum HKW circulation
                    val[0] *= (1 + scaler[p])
                elif p == 'dh4_mhkw':
                    # adjust minimum HKW circulation
                    val[1] *= (1 + scaler[p])
                else:
                    val *= (1 + scaler[p])

                if p in ['hb3', 'gt5']:
                    # exclude evaluations for efficiencies > 1 (set to NaN)
                    if val[val > 1.0].any():
                        raise ValueError('Efficiencies cannot be greater than 1.0')

                # re-assign updated/scaled values to model setup parameters
                params[p_adj] = val


    ###   create optimization setup dict   ###

    # define constant parameters as 'normal' values and dynamic parameters as pd.Series (to allow for indexing)
    setup = {
        # define MarketPrices as pd.Series, since all data is dynamic
        'market_prices': {'gas_q': pd.Series(params['mp1'], name='gas_q'),
                          'gas_gt': pd.Series(params['mp2'], name='gas_gt'),
                          'el_spot': pd.Series(params['mp3'], name='el_spot'),
                          'co2': pd.Series(params['mp4'], name='co2'),
                          'chp_bonus': pd.Series(params['mp5'], name='chp_bonus'),
                          'grid_save': pd.Series(params['mp6'], name='grid_save')},
        # define GasProperties
        'gas_properties': {'ho': params['gp1'],
                           'hu': params['gp2'],
                           'co2_factor': params['gp3']},
        # define DistrictHeatingGrid
        'district_heating_grid': {'name': params['dh1'],
                                  'entry_points': params['dh2']},
        # define municipal utility
        'municipal_utility': {'name': params['mu1'],
                              'conv_boilers': params['mu2'],
                              'gas_turbines': params['mu3'],
                              'contracts': params['mu4'],
                              'fuel': params['mu5'],
                              'network': params['mu6'],
                              'q_demand': pd.Series(params['mu7'], name='q_demand')},
        # define SourcingContracts, HeatBoilers, GasTurbine(s), and DistrictHeatingGrid entry points
        'sourcing_contracts': [],
        'heat_boilers': [],
        'gas_turbines': [],
        'grid_entry_points': []
    }

    # add contract(s) to setup dictionary
    for i in range(len(params['sc1'])):
        setup['sourcing_contracts'].append({'name': params['sc1'][i], 'qlimits_pa': params['sc2'][i],
                                            'prices': params['sc3'][i], 'current_price': params['sc4'][i],
                                            'entry_point': params['sc5'][i],
                                            # dynamic parameters as pd.Series
                                            'availability': pd.Series(params['sc6'][i], name='availability'),
                                            'qmin': pd.Series(params['sc7'][i], name='qmin'),
                                            'qmax': pd.Series(params['sc8'][i], name='qmax'),
                                            'q_hist': pd.Series(params['sc9'][i], name='q_hist')})
    # add boiler(s) to setup dictionary
    for i in range(len(params['hb1'])):
        setup['heat_boilers'].append({'name': params['hb1'][i], 'capacity': params['hb2'][i],
                                      'efficiency': params['hb3'][i], 'gas_demand': params['hb4'][i],
                                      'start_up_cost': params['hb5'][i], 'shut_down_cost': params['hb6'][i],
                                      'wear_cost': params['hb7'][i],
                                      # dynamic parameters as pd.Series
                                      'availability': pd.Series(params['hb8'][i], name='availability'),
                                      'labour_cost': pd.Series(params['hb9'][i], name='labour_cost'),
                                      'q_hist': pd.Series(params['hb10'][i], name='q_hist')})
    # add gas turbine(s) to setup dictionary
    for i in range(len(params['gt1'])):
        setup['gas_turbines'].append({'name': params['gt1'][i], 'power_el': params['gt2'][i],
                                      'power_q': params['gt3'][i], 'min_load': params['gt4'][i],
                                      'efficiency': params['gt5'][i], 'gas_demand': params['gt6'][i],
                                      'el_output': params['gt7'][i], 'idle_period': params['gt8'][i],
                                      'wear_cost': params['gt9'][i],
                                      # dynamic parameters as pd.Series
                                      'availability': pd.Series(params['gt10'][i], name='availability'),
                                      'labour_cost': pd.Series(params['gt11'][i], name='labour_cost'),
                                      'start_up_cost': pd.Series(params['gt12'][i], name='start_up_cost'),
                                      'shut_down_cost': pd.Series(params['gt13'][i], name='shut_down_cost'),
                                      'q_hist': pd.Series(params['gt14'][i], name='q_hist')})
    # add grid entry point(s) to setup dictionary
    for i in range(len(params['dh3'])):
        setup['grid_entry_points'].append({'name': params['dh3'][i], 'min_circulation': params['dh4'][i],
                                           # dynamic parameters as pd.Series
                                           'pressure': pd.Series(params['dh5'][i], name='pressure'),
                                           'temp_flow': pd.Series(params['dh6'][i], name='temp_flow'),
                                           'temp_return': pd.Series(params['dh7'][i], name='temp_return')})

    return setup, index


# Create input dataframe needed for forecasting data
def define_forecasting_inputs():

    # Initialise TimeSeriesClass
    Instant = jpsBaseLibView.java.time.Instant
    instant_class = Instant.now().getClass()
    TSClient = jpsBaseLibView.TimeSeriesClient(instant_class, utils.PROPERTIES_FILE)

    # Execute queries to obtain forecasting inputs
    response_list, title_list = create_queries.query_forecasting_inputs()

    # Unpack nested list
    unpacked_list = [x for l in response_list for x in l]

    df_list = []

    # Append query results to dataframe
    for r in unpacked_list:
        timeseries = TSClient.getTimeSeries([r['dataIRI']])
        values = timeseries.getValues(r['dataIRI'])
        df = pd.DataFrame(list(values))
        df_list.append(df)
        dates = timeseries.getTimes()

    # Create overall dataframe and assign titles
    data = pd.concat(df_list, axis=1)
    data.columns = title_list

    # Set dataframe index as the dates extracted from timeseries data
    data = data.set_index((d.toString().replace("T", " ").replace("Z", "")) for d in dates)

    # Add two more columns for MHKW Temp Vorlauf (degC) and MHKW Temp Ruecklauf (degC). These columns are updated later.
    data["MHKWTempVorlauf"] = np.nan
    data["MHKWTempRuecklauf"] = np.nan

    return data

