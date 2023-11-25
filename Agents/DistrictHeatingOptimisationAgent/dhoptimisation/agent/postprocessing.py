################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 20 Nov 2023                            #
################################################

# The purpose of this module is to provide multiple post-processing methods to
# analyse the optimised heat generation in comparison to the actual historical 
# data (i.e., through creating plots stored inside the container)

import os
import math
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.dates as mdates

from dhoptimisation.agent.config import *
from dhoptimisation.kgutils.kgclient import KGClient
from dhoptimisation.kgutils.tsclient import TSClient
from dhoptimisation.agent.optimisation_setup import HeatBoiler, GasTurbine, \
                                                    SourcingContract, MarketPrices, \
                                                    MunicipalUtility 
from dhoptimisation.agent.optimisation_tasks import *


# Specify relative file path to store optimisation outputs
OUTPUTS_REPO = '/app/dhoptimisation/resources/optimisation_outputs/'


def clear_repository(repo_path=OUTPUTS_REPO):
    """
    Delete previous optimisation outputs (figures, consolidated csv) from 
    non-related optimisation runs
    """
    # Ensure the path is valid
    if not os.path.exists(repo_path):
        raise_error(ValueError, f"The repository '{repo_path}' does not exist.")

    # Output filetypes to delete
    extensions = ['.png', '.csv']
    
    # Clear the repository
    try:
        for f in os.listdir(repo_path):
            if any(f.lower().endswith(ext) for ext in extensions):
                os.remove(os.path.join(repo_path, f))
        logger.info(f"Repository at '{repo_path}' has been cleared.")
    except Exception as ex:
        raise_error(Exception, f"Unable to clear repository - {ex}")


def instantiate_generation_cost(times:list, historic_gen:pd.DataFrame, 
                                optimised_gen: pd.DataFrame, kg_client: KGClient, 
                                ts_client: TSClient, time_format=TIME_FORMAT):
    """
    Instantiate total heat generation/sourcing cost for municipal utility for 
    both historical and optimised forecast time series
    
    Arguments:
        times {list} -- timestamps to instantiate (i.e., covered by optimisation)
        historic_gen {pd.DataFrame} -- historical generation and cost DataFrame
                                       as returned by 'get_historical_generation'
        optimised_gen {pd.DataFrame} -- extract of optimised generation DataFrame
                                        as returned by 'generation_optimization'
        kg_client {KGClient} -- pre-initialised SPARQL client
        ts_client {TSClient} -- pre-initialised TimeSeries client        
        time_format {str} -- Python compliant time format to parse time values
    """
    
    # Retrieve potentially already existing cost instances (to be updated)
    cost = kg_client.get_total_generation_cost()
    if not cost.get('hist_data_iri'):
        logger.info('Instantiating new cost outputs ...')
        # Instantiate new cost instances for historical and optimised forecast
        data_iris = kg_client.instantiate_generation_cost(cost.get('mu'))
        # Initialise cost time series
        ts_client.init_timeseries(dataIRI=data_iris.get('hist_data_iri'), times=times, 
                                    values=historic_gen.get('Min_cost').values, 
                                    time_format=time_format, ts_type=DOUBLE)
        ts_client.init_timeseries(dataIRI=data_iris.get('fc_data_iri'), times=times, 
                                    values=optimised_gen.get('Min_cost').values, 
                                    time_format=time_format, ts_type=DOUBLE)
    else:
        logger.info('Overwriting previous cost data ...')
        # Overwrite existing cost data with new (optimised) results
        ts_client.replace_ts_data(dataIRI=cost.get('hist_data_iri'), times=times, 
                                    values=historic_gen.get('Min_cost').values)
        ts_client.replace_ts_data(dataIRI=cost.get('fc_data_iri'), times=times, 
                                    values=optimised_gen.get('Min_cost').values)


def get_historical_generation(providers:list, mu:MunicipalUtility, prices:MarketPrices, 
                              kg_client: KGClient, ts_client: TSClient,
                              opti_start_dt: str, opti_end_dt: str, ):
    """
    Create a consolidated DataFrame for historical generation amounts and corresponding
    cost (according to the same evaluation logic as used by the optimisation)
    
    Arguments:
        providers {list} -- list of relevant heat provider/generator objects
        mu {MunicipalUtility} --municipal utility object as used by optimisation
        prices {MarketPrices} -- market prices object as used by optimisation
        kg_client {KGClient} -- pre-initialised SPARQL client
        ts_client {TSClient} -- pre-initialised TimeSeries client        
        opti_start_dt {str} -- optimisation start datetime as string
        opti_end_dt {str} -- optimisation end datetime as string
        
    Returns:
        DataFrame of historical generation time series and associated cost
    """
    
    # Initialise consolidated DataFrame for historical generation
    logger.info('Create DataFrame of historical generation ...')
    dataIRI = kg_client.get_historic_qdemand_datairi()
    generation_hist = ts_client.retrieve_timeseries_as_dataframe(dataIRI, 
                                    'Q_demand', opti_start_dt, opti_end_dt)
    for pro in providers:
        # Add historical heat generation for all providers/generators
        dataIRI = kg_client.get_historic_generation_datairi(pro.iri)
        df = ts_client.retrieve_timeseries_as_dataframe(dataIRI, pro.name, opti_start_dt, 
                                                        opti_end_dt)
        generation_hist = generation_hist.merge(df, on='time', how='outer')        
    
    # Assess historical generation cost
    logger.info('Assessing historical generation cost ...')
    cost, _, _ = evaluate_historic_generation(generation_hist, providers, mu.fuel, prices)
    generation_hist = generation_hist.merge(cost[['Min_cost']], on='time', how='outer')
    
    return generation_hist


def evaluate_historic_generation(historic_generation, gen_objects, gas_props, market_prices):
    """
    Returns total generation/sourcing cost for given heat amounts (per source and time interval)

    :param pd.DataFrame historic_generation: DataFrame with heat generation per source and time interval
    :param list gen_objects: list of heat provider/generator objects, i.e., 
                             HeatBoiler, GasTurbine, SourcingContract
    :param GasProperties gas_props: gas property object describing energy content and CO2 factor
    :param MarketPrices market_prices: price object containing electricity and gas prices
    
    :returns pd.DataFrame: DataFrame of total cost incurred per heat source in each time interval
    :returns pd.DataFrame: DataFrame of total gas consumption per heat source in each time interval
    :returns pd.DataFrame: DataFrame of total electricity generation per heat source in each time interval
    """

    first = True
    
    # Reset index to default integer index (as used by cost etc.)
    hist_gen = historic_generation.copy()
    hist_gen.reset_index(drop=True, inplace=True)

    for source in gen_objects:
        # Evaluate generation cost for each provider/generator
        cost, gas, el = evaluate_generation_series(hist_gen[[source.name]], source,
                                                   gas_props, market_prices)
        # Construct overarching DataFrame
        if first:
            cost_overview = cost.copy()
            gas_overview = gas.copy()
            el_overview = el.copy()
            first = False
        else:
            cost_overview = pd.concat([cost_overview, cost], axis=1)
            gas_overview = pd.concat([gas_overview, gas], axis=1)
            el_overview = pd.concat([el_overview, el], axis=1)
            
    # Add summary columns
    cost_overview['Min_cost'] = cost_overview.sum(axis=1)
    gas_overview['Gas_consumption'] = gas_overview.sum(axis=1)
    el_overview['Electricity_generation'] = el_overview.sum(axis=1)
    
    # Readd meaningful index
    cost_overview.index = historic_generation.index
    gas_overview.index = historic_generation.index
    el_overview.index = historic_generation.index

    return cost_overview, gas_overview, el_overview


def evaluate_generation_series(generation_series, generator, gas_props, market_prices):
    """
    Returns total generation/sourcing cost for heat amounts given by 'generation_series'
    on 'generator'

    :param pd.DataFrame generation_series: one-column DataFrame of heat generation 
                                           history of 'generator' (MWh/timestep)
    :param generator: heat provider/generator object, i.e., HeatBoiler, GasTurbine, 
                      SourcingContract
    :param GasProperties gas_props: gas property object describing energy content and CO2 factor
    :param MarketPrices market_prices: price object containing electricity and gas prices
    
    :returns pd.Series: Pandas.Series of total cost incurred in each time interval
    :returns pd.Series: Pandas.Series of total gas amount consumed in each time interval
    :returns pd.Series: Pandas.Series of total electricity co-generated in each time interval
    """

    # initialize heat generation 'history'
    gen_hist = generation_series.copy()

    # initialize return cost DataFrame
    cost = generation_series.copy()
    cost.values[:] = 0
    gas_demand = generation_series.iloc[:, 0].copy()
    gas_demand.values[:] = 0
    el_gen = generation_series.iloc[:, 0].copy()
    el_gen.values[:] = 0

    # evaluate generation cost for heat boilers and gas turbine
    if isinstance(generator, (HeatBoiler, GasTurbine)):

        # adjust historic generation values below GT min load
        if isinstance(generator, GasTurbine):
            gen_hist[(gen_hist.values > 0) & (gen_hist.values < generator.min_load / 2)] = 0.0
            gen_hist[(gen_hist.values > 0) & (gen_hist.values < generator.min_load)] = generator.min_load

        # derive whether heat generation was active in each time step
        gen_hist['on'] = [1 if (gen_hist.iloc[:, 0].values[i] > 0) else 0 for i in gen_hist.index]
        # derive start-up and shut-down time intervals
        gen_hist['switch'] = gen_hist['on'].diff()

        # derive hourly OPEX for all active intervals
        cost['OPEX_per_h'] = [opex_per_h(generator, i) if
                              (gen_hist.iloc[:, 0].values[i] > 0) else 0 for i in gen_hist.index]
        # derive OPEX, gas demand, and (potential) electricity generation associated with heat amount
        ops = np.array([opex_per_MWh(generator, gas_props, market_prices, i, quant_new=gen_hist.iloc[i, 0])
                                for i in gen_hist.index])
        cost['OPEX_per_MWh'] = ops[:, 0]
        cost['OPEX_all_MWhs'] = cost['OPEX_per_MWh'] * gen_hist.iloc[:, 0]
        # derive switching cost (only relevant for time steps > 0)
        cost['switching'] = 0.0     # initialise column
        cost.iloc[1:, -1] = [switching_cost([], i-1, [generator], 1) if (gen_hist['switch'].values[i] == 1)
                             else 0 for i in gen_hist.index[1:]]        # start-up
        cost.iloc[1:, -1] += [switching_cost([generator], i-1, [], i) if (gen_hist['switch'].values[i] == -1)
                              else 0 for i in gen_hist.index[1:]]       # shut-down
        # add up hourly, consumption-driven, and switching cost for each time step
        cost.iloc[:, 0] = cost[['OPEX_per_h', 'OPEX_all_MWhs', 'switching']].sum(axis=1)

        # derive associated gas consumption and electricity co-generation
        gas_demand = ops[:, 1] * gen_hist.iloc[:, 0]
        el_gen = ops[:, 2] * gen_hist.iloc[:, 0]

    # evaluate sourcing cost from MHKW
    if isinstance(generator, SourcingContract):

        # calculate cumulative heat sourcing (for respective year) to assess applicable heat unit price
        gen_hist['cumulative'] = gen_hist.cumsum()

        # derive applicable heat unit price based on already sourced annual volume
        cost['Unit_price'] = [generator.prices[0] if gen_hist['cumulative'][i] <= generator.qlimits_pa[0]
                             else generator.prices[1] for i in gen_hist.index]

        # calculate sourcing cost per time step
        cost.iloc[:, 0] = gen_hist.iloc[:, 0] * cost['Unit_price']

    return cost.iloc[:, 0], gas_demand, el_gen


def create_optimised_csv_output(optimized_generation, csv_file='Optimised_generation'):
    """
    Creates a consolidated csv file of all optimised generations for consecutive
    runs in related optimisation intervals, i.e., for all related optimisation runs
        1) extract the first optimised time step and 
        2) append it as consecutive row (to resemble MPC-style optimisation)
    
    Arguments:
        optimized_generation {pd.DataFrame} -- extract of optimised generation DataFrame
                                               as returned by 'generation_optimization'
        csv_file {str} -- file name
    """
    
    # Create filenames for current and continuous outputs
    # current: optimisation results from current run (looking into the future)
    current = csv_file + '_' + optimized_generation.index[0].strftime(TIME_FORMAT) + '.csv'
    current = current.replace(':', '-')
    current = os.path.join(OUTPUTS_REPO, current)
    # continuous: first time steps of all consecutive related optimisation runs
    continuous = csv_file + '_continuous.csv'
    continuous = os.path.join(OUTPUTS_REPO, continuous)
    
    # Write current output (overwrite existing file)
    optimized_generation.sort_index(axis=1).to_csv(current, header=True, index=True)
    
    # Write continuous output
    # NOTE: sort columns to ensure correct order of outputs in subsequent runs
    if not os.path.exists(continuous):
        # Initialise output csv
        optimized_generation.sort_index(axis=1).head(1).to_csv(continuous, 
                                header=True, index=True)        
    else:
        # Simply append new time steps as new row
        optimized_generation.sort_index(axis=1).head(1).to_csv(continuous, 
                                mode='a', header=False, index=True)


def plot_entire_heat_generation(historic_generation, optimized_generation, el_prices,
                                fig_name='Generation_comparison', hour_spacing=3):
    """
    Creates plot of non-optimized vs. optimized heat generation in three subplots:
        1) heat load and electricity spot price vs. time
        2) historic heat generation distribution
        3) optimized heat generation distribution

    :param pd.DataFrame historic_generation: historic heat generation for each time step in optimization period
                                             (DataFrame layout similar as returned by 'optimize_operating_modes')
    :param pd.DataFrame optimized_generation: DataFrame as returned by 'optimize_operating_modes' with overall
                                              optimal operation
    :param pd.DataFrame el_prices: electricity spot price for each time step in optimization period
    """
    
    # Extract relevant heat generator column names
    cols = optimized_generation.columns.difference(['Q_demand', 'Min_cost'])
    # Update integer based default index for price
    el_prices.index = optimized_generation.index

    # set global plotting parameters
    plt.rcParams['font.size'] = 14

    # derive limits and ticks for subplots
    y_max1 = math.ceil(1.1 * optimized_generation['Q_demand'].max())
    yticks1 = [i for i in range(0, y_max1, 2)]
    y_max2 = math.ceil(1.1 * optimized_generation[cols].max().max())
    yticks2 = [i for i in range(0, y_max2, 2)]

    # define legend colors
    colors = ['tab:blue', 'tab:red', 'tab:orange', 'tab:purple', 'tab:green']

    # create figure
    f, ax = plt.subplots(3, 1, figsize=(16, 9), sharex=True)
    f.suptitle('Total heat generation/sourcing')

    # first subplot with heat load and electricity spot price
    color = 'tab:red'
    ax[0].grid(True, 'both', ls='--', lw=0.5)
    ax[0].plot(optimized_generation['Q_demand'], '.-', color=color, ms=2.0, lw=1.0)
    ax[0].set_ylim([0, y_max1])  # align y axis between plots
    ax[0].set_yticks(yticks1)
    ax[0].set_ylabel('Heat load (MWh/h)', color=color)
    ax[0].tick_params(axis='y', labelcolor=color)
    # add second y axis
    ax2 = ax[0].twinx()
    color = 'tab:blue'
    ax2.plot(el_prices, '.-', color=color, ms=2.0, lw=1.0)
    ax2.set_ylabel('Electricity spot \n price (€/MWh)', color=color)
    ax2.tick_params(axis='y', labelcolor=color)

    # second subplot with historic heat generation
    plot_data = historic_generation[cols].copy()
    ax[1].grid(True, 'both', ls='--', lw=0.5)
    for i in range(len(cols)):
        ax[1].plot(plot_data.iloc[:, i], '.-', color=colors[i], ms=2.0, lw=1.0)
    ax[1].legend(cols, loc='upper right')
    ax[1].set_ylim([0, y_max2])  # align y axis between plots
    ax[1].set_yticks(yticks2)
    ax[1].set_ylabel('Historical heat \n generation (MWh/h)')

    # third subplot with optimized heat generation
    plot_data = optimized_generation[cols].copy()
    ax[2].grid(True, 'both', ls='--', lw=0.5)
    for i in range(len(cols)):
        ax[2].plot(plot_data.iloc[:, i], '.-', color=colors[i], ms=2.0, lw=1.0)
    ax[2].legend(cols, loc='upper right')
    ax[2].set_ylim([0, y_max2])  # align y axis between plots
    ax[2].set_yticks(yticks2)
    ax[2].set_ylabel('Optimized heat \n generation (MWh/h)')
    ax[2].set_xlabel('\nTime (dd/mm/yy hh:mm)')

    # Formatting the datetime axis
    hours = list(range(0,24,hour_spacing))
    ax[2].xaxis.set_major_locator(mdates.DayLocator())                  # major locator for days
    ax[2].xaxis.set_minor_locator(mdates.HourLocator(byhour=hours))     # minor locator for hours
    ax[2].xaxis.set_major_formatter(mdates.DateFormatter("%d/%m/%y"))   # formatter
    ax[2].xaxis.set_minor_formatter(mdates.DateFormatter("%H:%M"))
    ax[2].set_xlim([historic_generation.index[0], historic_generation.index[-1]])
    # Adjust number of minor ticks as appropriate
    remove_order = [['03:00', '09:00', '15:00', '21:00'], ['06:00', '18:00'], ['12:00']]
    i = 0
    while len(ax[2].xaxis.get_ticklabels(minor=True)) > 10:
        for l in ax[2].xaxis.get_ticklabels(minor=True):
            if l._text in remove_order[i]:
                l.set_visible(False)
        i += 1
    
    plt.tight_layout()  
    
    # Save figure with timestamp in name
    fig_name += '_' + historic_generation.index[0].strftime(TIME_FORMAT) + '.png'
    fig_name = fig_name.replace(':', '-')
    plt.savefig(os.path.join(OUTPUTS_REPO, fig_name))
    plt.close()


def plot_generation_cost(generation_hist, optimized_generation,
                         fig_name='Cost_comparison', hour_spacing=3):
    """
    Creates plot of heat generation induced cost in three subplots:
        1) heat generation distribution
        2) incremental cost  per time interval
        3) accumulated cost over time

    :param pd.DataFrame generation_hist: historic/non-optimized heat generation for each time step in optimization period
                                         (DataFrame layout similar as returned by 'optimize_operating_modes')
    :param pd.DataFrame optimized_generation: DataFrame as returned by 'optimize_operating_modes' with overall optimal operation
    :returns figure: plot of heat generation cost
    """
    
    # Extract relevant heat generator column names
    cols = optimized_generation.columns.difference(['Q_demand', 'Min_cost'])

    # set global plotting parameters
    plt.rcParams['font.size'] = 14

    # derive limit and ticks for heat load
    y_max = math.ceil(1.1 * optimized_generation[cols].max().max())
    yticks = [i for i in range(0, y_max, 2)]

    # define legend and colors
    colors = ['tab:blue', 'tab:red', 'tab:orange', 'tab:purple', 'tab:green']

    # create figure
    f, ax = plt.subplots(3, figsize=(16, 9), sharex=True)
    f.suptitle('Fully optimized heat generation/sourcing')

    # first subplot with heat generation
    plot_data = optimized_generation[cols].copy()
    ax[0].grid(True, 'both', ls='--', lw=0.5)
    for i in range(len(cols)):
        ax[0].plot(plot_data.iloc[:, i], '.-', color=colors[i], ms=2.0, lw=1.0)
    ax[0].legend(cols, loc='upper right')
    ax[0].set_ylim([0, y_max])  # align y axis between plots
    ax[0].set_yticks(yticks)
    ax[0].set_ylabel('Heat generation \n (MWh/h)')

    # second subplot with incremental cost lines
    ax[1].grid(True, 'both', ls='--', lw=0.5)
    ax[1].plot(generation_hist['Min_cost'], '.-', color='tab:gray', ms=2.0, lw=1.0)
    ax[1].plot(optimized_generation['Min_cost'], '.--', color='tab:red', ms=2.0, lw=1.0)
    legend = ['Actual historic generation', 'Fully optimized heat generation']
    ax[1].legend(legend, loc='upper left')
    ax[1].set_ylabel('Incremental \n generation cost (€/h)')

    # third subplot with accumulated profit
    ax[2].grid(True, 'both', ls='--', lw=0.5)
    ax[2].plot(generation_hist['Min_cost'].cumsum()/1000, '.-', color='tab:gray', ms=2.0, lw=1.0)
    ax[2].plot(optimized_generation['Min_cost'].cumsum()/1000, '.--', color='tab:red', ms=2.0, lw=1.0)
    legend = ['Actual historic generation', 'Fully optimized heat generation']
    ax[2].legend(legend, loc='upper left')
    ax[2].set_ylabel('Cumulative \n generation cost (k€)')
    ax[2].set_xlabel('\nTime (dd/mm/yy hh:mm)')

    # Formatting the datetime axis
    hours = list(range(0,24,hour_spacing))
    ax[2].xaxis.set_major_locator(mdates.DayLocator())                  # major locator for days
    ax[2].xaxis.set_minor_locator(mdates.HourLocator(byhour=hours))     # minor locator for hours
    ax[2].xaxis.set_major_formatter(mdates.DateFormatter("%d/%m/%y"))   # formatter
    ax[2].xaxis.set_minor_formatter(mdates.DateFormatter("%H:%M"))
    ax[2].set_xlim([generation_hist.index[0], generation_hist.index[-1]])
    # Adjust number of minor ticks as appropriate
    remove_order = [['03:00', '09:00', '15:00', '21:00'], ['06:00', '18:00'], ['12:00']]
    i = 0
    while len(ax[2].xaxis.get_ticklabels(minor=True)) > 10:
        for l in ax[2].xaxis.get_ticklabels(minor=True):
            if l._text in remove_order[i]:
                l.set_visible(False)
        i += 1

    plt.tight_layout()   
     
    # Save figure with timestamp in name
    fig_name += '_' + generation_hist.index[0].strftime(TIME_FORMAT) + '.png'
    fig_name = fig_name.replace(':', '-')
    plt.savefig(os.path.join(OUTPUTS_REPO, fig_name))
    plt.close()


def plot_forecast_quality(historical_ts, forecasted_ts,
                          fig_name='Forecast_analysis', hour_spacing=3):
    """
    Creates figure with 2 subplots
        1) comparison of time series
        2) histogram of forecast errors

    :param pd.DataFrame historical_ts: historical time series of variable
    :param pd.DataFrame forecasted_ts: forecasted time series of variable
    """

    # evaluate forecast errors
    error = forecasted_ts - historical_ts
    bins = np.arange(math.floor(error.min()), math.ceil(error.max()), step=0.1)

    # set global plotting parameters
    plt.rcParams['font.size'] = 14

    # create figure
    f, ax = plt.subplots(2, figsize=(16, 9))
    # plot time series comparison
    ax[0].grid('both')
    ax[0].plot(historical_ts)
    ax[0].plot(forecasted_ts)
    ax[0].set_xlabel('Time')
    ax[0].set_ylabel(historical_ts.name.replace('_', ' '))
    ax[0].legend(['Historical time series', 'Forecast'])
    ax[0].set_title('Time series comparison')
    # Formatting the datetime axis
    hours = list(range(0,24,hour_spacing))
    ax[0].xaxis.set_major_locator(mdates.DayLocator())                  # major locator for days
    ax[0].xaxis.set_minor_locator(mdates.HourLocator(byhour=hours))     # minor locator for hours
    ax[0].xaxis.set_major_formatter(mdates.DateFormatter("%d/%m/%y"))   # formatter
    ax[0].xaxis.set_minor_formatter(mdates.DateFormatter("%H:%M"))
    ax[0].set_xlim([historical_ts.index[0], historical_ts.index[-1]])
    # Adjust number of minor ticks as appropriate
    remove_order = [['03:00', '09:00', '15:00', '21:00'], ['06:00', '18:00'], ['12:00']]
    i = 0
    while len(ax[0].xaxis.get_ticklabels(minor=True)) > 10:
        for l in ax[0].xaxis.get_ticklabels(minor=True):
            if l._text in remove_order[i]:
                l.set_visible(False)
        i += 1
    
    # plot histogram of discrepancies
    ax[1].grid('both')
    ax[1].hist(error, bins=bins)
    ax[1].axvline(error.mean(), color='orange')
    ax[1].axvline(error.mean() - error.std(), ls='--', color='orange')
    ax[1].axvline(error.mean() + error.std(), ls='--', color='orange', label='_nolegend_')
    ax[1].set_xlabel('Forecast error (forecasted value - actual value)')
    ax[1].set_ylabel('Count of occurrences')
    ax[1].set_title('Forecast error histogram')
    ax[1].legend(['Mean', '+/- Standard deviation', error.name.replace('_', ' ')])

    f.suptitle(historical_ts.name.replace('_', ' ') + ' - forecast analysis')
    plt.tight_layout()
    
    # Save figure with timestamp in name
    fig_name += '_' + historical_ts.index[0].strftime(TIME_FORMAT) + '.png'
    fig_name = fig_name.replace(':', '-')
    plt.savefig(os.path.join(OUTPUTS_REPO, fig_name))
    plt.close()
