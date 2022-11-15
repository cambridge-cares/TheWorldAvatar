"""
Run heat generation cost optimization as model-predictive-control model

@author: Markus Hofmeister
"""

import os
import pickle
import copy
import numpy as np
import pandas as pd
import datetime as dt
from pathlib import Path
import matplotlib.pyplot as plt
import matplotlib.dates as mdates

from statsmodels.tsa.arima.model import ARIMA
from pmdarima.preprocessing import FourierFeaturizer

from district_heating.generation_optimisation import cost_optimization as optima
from district_heating.generation_optimisation import cost_optimization_preprocessing as preprocessing
from district_heating.generation_optimisation import cost_optimization_postprocessing as postprocessing
from district_heating.timeseries_forecasting import timeseries_forecasting
from district_heating.timeseries_forecasting import create_SARIMAX_model


####################     MPC OPTIMIZATION RUN    ####################

if __name__ == '__main__':

    # define optimization period
    start = '2020-01-01 00:00:00'   # optimization start (first time step to be forecasted): [2018, 2020]
    opt_period = 24 * 1          # number of time steps to forecast/optimise (max. 24 * 364)
    mpc_horizon = 24                # number of time steps to forecast and optimise per MPC cycle (MPC horizon)

    # specify gas demand/electricity output model ['efficiency', 'models']
    method = 'models'
    #method = 'efficiency'

    ############# 1)  load data and set up optimisation framework   #############

    # derive optimization setup (as dict), DateTimeIndex, and actual historic heat generation data
    if start[:4] == '2018':
        setup_dict, index, generation_hist = preprocessing.define_2018_setup(opt_period + mpc_horizon, method=method)
    elif start[:4] == '2020':
        setup_dict, index, generation_hist = preprocessing.define_2020_setup(opt_period + mpc_horizon, method=method)
    # re-adjusted index and generation_hist length to opt_period only
    index = index[:opt_period + mpc_horizon]
    generation_hist = generation_hist[:opt_period]

    # create MarketPrices and MunicipalUtility objects
    prices, swps = optima.create_optimization_setup(setup_dict)

    # load (external) forecast data required for heat load and grid temperature forecasting which are not explicit
    # parts of municipal utility or market prices objects, i.e. ambient temperature
    root = Path(__file__).parent
    ts_input = '..\\..\\data\\input\\processed\\fully_conditioned_timeseries.csv'
    ts_input = pd.read_csv(os.path.join(root, ts_input), index_col=0, parse_dates=True, dayfirst=True)

    if start[:4] == '2018':
        # create artificial training/fit history for MHKW grid temperatures for 2018 forecasts
        ts_input.loc['2017', ['MHKW Temp Vorlauf (degC)', 'MHKW Temp Ruecklauf (degC)']] = \
            ts_input.loc['2018', ['MHKW Temp Vorlauf (degC)', 'MHKW Temp Ruecklauf (degC)']].values
    ts_input.dropna(inplace=True)

    # define continuous output/log files for mpc optimisation
    out_file_gt = os.path.join(root, '..\\..\\data\\output\\optimization\\GasTurbine_planning_' + str(mpc_horizon)
                               + 'h_' + dt.datetime.now().strftime("%Y%m%d-%H%M") + '.txt')
    out_file_opt = os.path.join(root, '..\\..\\data\\output\\optimization\\HeatGeneration_optimisation_'
                                + str(mpc_horizon) + 'h_' + dt.datetime.now().strftime("%Y%m%d-%H%M") + '.txt')

    ############# 2)  optimze heat generation modes   #############

    # optimize operating modes
    res, res_wogt, res_wgt, fcs = optima.mpc_optimization(swps, prices, ts_input, index, opt_period, mpc_horizon,
                                                          out_file_gt, out_file_opt, histeval=False, live_updates=False)

    # output to console
    titles = {'Heat generation w/o GT': res_wogt, 'Heat generation w/ GT': res_wgt, 'Optimized heat generation': res}
    for i in titles:
        print('\n#####   ' + i + '   #####')
        print('Total heat generation: %.2f MWh' % titles[i][
            ['SWPS_GT', 'MHKW_ToP', 'Kessel4', 'Kessel5', 'Kessel6']].sum().sum())
        print('Minimal heat generation/sourcing cost: %.2f €' % titles[i]['Min_cost'].sum())
        print('Average heat generation/sourcing cost: %.2f €/MWh' % (titles[i]['Min_cost'].sum()/titles[i]['Q_demand'].sum()))

    # write results
    root = Path(__file__).parent
    file = os.path.join(root, '..\\..\\data\\output\\optimization\\optimization_results_' + method + '_' +
                        dt.datetime.now().strftime("%Y%m%d-%H%M") + '.csv')
    res.to_csv(file)
    if not fcs.empty:
        file = os.path.join(root, '..\\..\\data\\output\\optimization\\optimization_forecasts_' +
                             dt.datetime.now().strftime("%Y%m%d-%H%M") + '.csv')
        fcs.to_csv(file)

    ############# 3)  calculate cost, gas demand, and electricity for actual 'non-optimized' generation   #############

    # get list of all potential heat generators
    sources = []
    sources.extend(swps.boilers)
    sources.extend(swps.contracts)
    sources.extend(swps.gas_turbines)

    # create dictionary to map heat generation object to column header names in 'generation_hist'
    gen_obj = {}
    for i in generation_hist.columns:
        gen_obj[i] = list(filter(lambda x: x.name == i, sources))

    # append further columns to mimic DataFrame layout of optimized generation objects
    generation_hist = pd.concat([pd.DataFrame(columns=['Q_demand', 'Cost']), generation_hist,
                                 pd.DataFrame(columns=['Gas_consumption', 'Electricity_generation'])], axis=1)
    # append heat demand
    generation_hist['Q_demand'] = res['Q_demand'].copy()
    # evaluate non-optimized heat generation cost, gas consumption, and electricity generation (per source and timestep)
    cost, gas, el = postprocessing.evaluate_historic_generation(generation_hist[['SWPS_GT', 'MHKW_ToP', 'Kessel4',
                                                                'Kessel5', 'Kessel6']], gen_obj, swps.fuel, prices)
    generation_hist['Cost'] = cost.sum(axis=1)
    generation_hist['Gas_consumption'] = gas.sum(axis=1)
    generation_hist['Electricity_generation'] = el.sum(axis=1)

    # write historical evaluation
    file = os.path.join(root, '..\\..\\data\\output\\optimization\\generation_history_evaluation_' + method + '_' +
                        dt.datetime.now().strftime("%Y%m%d-%H%M") + '.csv')
    generation_hist.to_csv(file)

    # print results for actual heat generation/sourcing
    print('\n#####   Actual historic heat generation   #####')
    print('Total heat generation: %.2f MWh' % generation_hist[
        ['SWPS_GT', 'MHKW_ToP', 'Kessel4', 'Kessel5', 'Kessel6']].sum().sum())
    print('Minimal heat generation/sourcing cost: %.2f €' % generation_hist['Cost'].sum())
    print('Average heat generation/sourcing cost: %.2f €/MWh' % (
                generation_hist['Cost'].sum() / generation_hist['Q_demand'].sum()))

    ### 4)  plot results   ##########

    # restore DateTime indices
    generation_hist.index = index[:opt_period]
    res_wogt.index = index[:opt_period]
    res_wgt.index = index[:opt_period]
    res.index = index[:opt_period]

    # extract electricity spot prices (and restore DateTime index)
    el_spot = prices.el_spot.copy()
    el_spot = el_spot[:opt_period]
    el_spot.index = index[:opt_period]

    # plot comparison of GT operation
    fig1 = postprocessing.plot_gt_operations(generation_hist['SWPS_GT'], res, el_spot)
    fig1.show()

    # plot comparison of total heat generation/sourcing
    fig2 = postprocessing.plot_entire_heat_generation(generation_hist, res, el_spot)
    fig2.show()

    # plot cost of historical non-optimized heat generation/sourcing
    fig3 = postprocessing.plot_generation_cost(generation_hist)
    fig3.show()

    # plot cost of optimized heat generation/sourcing incl. pre-optimised time series
    fig4 = postprocessing.plot_generation_cost(generation_hist, res_wogt, res_wgt, res)
    fig4.show()

    # plot cost of optimized heat generation/sourcing
    fig5 = postprocessing.plot_generation_cost(generation_hist, None, None, res)
    fig5.show()

    if not fcs.empty:
        for var in fcs.columns:
            # plot forecast analysis for each forecasted variable
            fig = postprocessing.plot_forecast_quality(ts_input.loc[index[:opt_period], var],
                                                       fcs.loc[index[:opt_period], var])
            fig.show()