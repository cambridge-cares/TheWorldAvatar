"""
Postprocess heat generation cost optimization model outputs
    - evaluate actual cost for 'historic'/non-optimized heat generation
    - create plots

@author: Markus Hofmeister
"""

import math
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

from sklearn.metrics import mean_squared_error

from district_heating.generation_optimisation import cost_optimization as optima


####################     FUNCTIONS     ####################

def plot_gt_operations(gt_hist, optimized_generation, el_prices):
    """
    Creates plot of non-optimized vs. optimized GT operation in two subplots:
        1) heat load and electricity spot price vs. time
        2) comparison of GT operation times

    :param pd.DataFrame gt_hist: historic/non-optimized GT operation for each time step in optimization period
    :param pd.DataFrame optimized_generation: DataFrame as returned by 'optimize_operating_modes' with optimal operation
    :param pd.DataFrame el_prices: electricity spot price for each time step in optimization period
    :returns figure: plot of non-optimized vs. optimized GT operation
    """

    # set global plotting parameters
    plt.rcParams['font.size'] = 14

    # derive limit and ticks for heat load
    y_max = math.ceil(1.1 * optimized_generation['Q_demand'].max())
    yticks = [i for i in range(0, y_max, 2)]

    # create figure
    f, ax = plt.subplots(2, figsize=(16, 9))
    f.suptitle('Gas turbine operation')

    # first subplot with heat load and electricity spot price
    color = 'tab:red'
    ax[0].grid('both', ls='--', lw=0.5)
    ax[0].plot(optimized_generation['Q_demand'], '.-', color=color, ms=2.0, lw=1.0)
    ax[0].set_ylim([0, y_max])  # align y axis between plots
    ax[0].set_yticks(yticks)
    ax[0].set_ylabel('Heat load (MWh/h)', color=color)
    ax[0].tick_params(axis='y', labelcolor=color)
    # add second y axis
    ax2 = ax[0].twinx()
    color = 'tab:blue'
    ax2.plot(el_prices, '.-', color=color, ms=2.0, lw=1.0)
    ax2.set_ylabel('Electricity spot price (€/MWh)', color=color)
    ax2.tick_params(axis='y', labelcolor=color)

    # second subplot with GT operation times
    ax[1].grid('both', ls='--', lw=0.5)
    ax[1].plot(gt_hist, '.-', color='tab:blue', ms=2.0, lw=1.0)
    ax[1].set_ylim([0, y_max])
    ax[1].set_yticks(yticks)
    ax[1].set_ylabel('GT heat generation (MWh/h)')
    ax[1].plot(optimized_generation['SWPS_GT'], '.--', color='tab:red', ms=2.0, lw=1.0)
    ax[1].legend(['Historic GT operation', 'Optimized GT operation'], loc='upper left')
    ax[1].set_xlabel('Date')

    plt.close()

    return f


def plot_entire_heat_generation(historic_generation, optimized_generation, el_prices):
    """
        Creates plot of non-optimized vs. optimized heat generation in three subplots:
            1) heat load and electricity spot price vs. time
            2) historic heat generation distribution
            3) optimized heat generation distribution

        :param pd.DataFrame historic_generation: historic heat generation for each time step in optimization period
                                                 (DataFrame layout similar as returned by 'optimize_operating_modes')
        :param pd.DataFrame optimized_generation: DataFrame as returned by 'optimize_operating_modes' with overall optimal operation
        :param pd.DataFrame el_prices: electricity spot price for each time step in optimization period
        :returns figure: plot of non-optimized vs. optimized heat generation
        """

    # set global plotting parameters
    plt.rcParams['font.size'] = 14

    # derive limits and ticks for subplots
    y_max1 = math.ceil(1.1 * optimized_generation['Q_demand'].max())
    yticks1 = [i for i in range(0, y_max1, 2)]
    y_max2 = math.ceil(1.1 * optimized_generation[['SWPS_GT', 'Kessel4', 'Kessel5', 'Kessel6', 'MHKW_ToP']].max().max())
    yticks2 = [i for i in range(0, y_max2, 2)]

    # define legend and colors
    legend = ['Gas turbine', 'Kessel 4', 'Kessel 5', 'Kessel 6', 'MHKW']
    colors = ['tab:blue', 'tab:red', 'tab:orange', 'tab:purple', 'tab:green']

    # create figure
    f, ax = plt.subplots(3, 1, figsize=(16, 9))
    f.suptitle('Total heat generation/sourcing')

    # first subplot with heat load and electricity spot price
    color = 'tab:red'
    ax[0].grid('both', ls='--', lw=0.5)
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
    plot_data = historic_generation[['SWPS_GT', 'Kessel4', 'Kessel5', 'Kessel6', 'MHKW_ToP']].copy()
    ax[1].grid('both', ls='--', lw=0.5)
    for i in range(len(legend)):
        ax[1].plot(plot_data.iloc[:, i], '.-', color=colors[i], ms=2.0, lw=1.0)
    ax[1].legend(legend, loc='upper center')
    ax[1].set_ylim([0, y_max2])  # align y axis between plots
    ax[1].set_yticks(yticks2)
    ax[1].set_ylabel('Historical heat \n generation (MWh/h)')

    # third subplot with optimized heat generation
    plot_data = optimized_generation[['SWPS_GT', 'Kessel4', 'Kessel5', 'Kessel6', 'MHKW_ToP']].copy()
    ax[2].grid('both', ls='--', lw=0.5)
    for i in range(len(legend)):
        ax[2].plot(plot_data.iloc[:, i], '.-', color=colors[i], ms=2.0, lw=1.0)
    ax[2].legend(legend, loc='upper center')
    ax[2].set_ylim([0, y_max2])  # align y axis between plots
    ax[2].set_yticks(yticks2)
    ax[2].set_ylabel('Optimized heat \n generation (MWh/h)')
    ax[2].set_xlabel('Date')

    plt.close()

    return f


def plot_generation_cost(generation_hist, opt_mode_wogt=[], opt_mode_wgt=[], optimized_generation=[]):
    """
    Creates plot of heat generation induced cost in three subplots:
        1) heat generation distribution
        2) incremental cost  per time interval
        3) accumulated cost over time

    :param pd.DataFrame generation_hist: historic/non-optimized heat generation for each time step in optimization period
                                         (DataFrame layout similar as returned by 'optimize_operating_modes')
    :param pd.DataFrame opt_mode_wogt: DataFrame as returned by 'derive_max_profit_conditions' for mode w/o GT
    :param pd.DataFrame opt_mode_wgt: DataFrame as returned by 'derive_max_profit_conditions' for mode w/ GT
    :param pd.DataFrame optimized_generation: DataFrame as returned by 'optimize_operating_modes' with overall optimal operation
    :returns figure: plot of heat generation cost
    """

    # set global plotting parameters
    plt.rcParams['font.size'] = 14

    # derive limit and ticks for heat load
    if isinstance(optimized_generation, pd.DataFrame):
        y_max = math.ceil(1.1 * optimized_generation[['SWPS_GT', 'Kessel4', 'Kessel5', 'Kessel6', 'MHKW_ToP']].max().max())
    else:
        y_max = math.ceil(1.1 * generation_hist[['SWPS_GT', 'Kessel4', 'Kessel5', 'Kessel6', 'MHKW_ToP']].max().max())
    yticks = [i for i in range(0, y_max, 2)]

    # define legend and colors
    legend = ['Gas turbine', 'Kessel 4', 'Kessel 5', 'Kessel 6', 'MHKW']
    colors = ['tab:blue', 'tab:red', 'tab:orange', 'tab:purple', 'tab:green']

    # create figure
    f, ax = plt.subplots(3, figsize=(16, 9))
    if isinstance(optimized_generation, pd.DataFrame):
        f.suptitle('Fully optimized heat generation/sourcing')
    else:
        f.suptitle('Historic heat generation/sourcing')

    # first subplot with heat generation
    if isinstance(optimized_generation, pd.DataFrame):
        plot_data = optimized_generation[['SWPS_GT', 'Kessel4', 'Kessel5', 'Kessel6', 'MHKW_ToP']].copy()
    else:
        plot_data = generation_hist[['SWPS_GT', 'Kessel4', 'Kessel5', 'Kessel6', 'MHKW_ToP']].copy()
    ax[0].grid('both', ls='--', lw=0.5)
    for i in range(len(legend)):
        ax[0].plot(plot_data.iloc[:, i], '.-', color=colors[i], ms=2.0, lw=1.0)
    ax[0].legend(legend, loc='upper center')
    ax[0].set_ylim([0, y_max])  # align y axis between plots
    ax[0].set_yticks(yticks)
    ax[0].set_ylabel('Heat generation \n (MWh/h)')

    # second subplot with incremental cost lines
    ax[1].grid('both', ls='--', lw=0.5)
    ax[1].plot(generation_hist['Cost'], '.-', color='tab:gray', ms=2.0, lw=1.0)
    legend = ['Actual historic generation']
    if isinstance(opt_mode_wogt, pd.DataFrame):
        ax[1].plot(opt_mode_wogt['Min_cost'], '.-', color='tab:orange', ms=2.0, lw=1.0)
        legend.append('Pre-optimized operation w/o GT')
    if isinstance(opt_mode_wgt, pd.DataFrame):
        ax[1].plot(opt_mode_wgt['Min_cost'], '.-', color='tab:blue', ms=2.0, lw=1.0)
        legend.append('Pre-optimized operation w/ GT')
    if isinstance(optimized_generation, pd.DataFrame):
        ax[1].plot(optimized_generation['Min_cost'], '.--', color='tab:red', ms=2.0, lw=1.0)
        legend.append('Fully optimized heat generation')
    ax[1].legend(legend, loc='upper center')
    ax[1].set_ylabel('Incremental \n generation cost (€/h)')

    # third subplot with accumulated profit
    ax[2].grid('both', ls='--', lw=0.5)
    ax[2].plot(generation_hist['Cost'].cumsum()/1000, '.-', color='tab:gray', ms=2.0, lw=1.0)
    legend = ['Actual historic generation']
    if isinstance(opt_mode_wogt, pd.DataFrame):
        ax[2].plot(opt_mode_wogt['Min_cost'].cumsum() / 1000, '.-', color='tab:orange', ms=2.0, lw=1.0)
        legend.append('Pre-optimized operation w/o GT')
    if isinstance(opt_mode_wgt, pd.DataFrame):
        ax[2].plot(opt_mode_wgt['Min_cost'].cumsum()/1000, '.-', color='tab:blue', ms=2.0, lw=1.0)
        legend.append('Pre-optimized operation w/ GT')
    if isinstance(optimized_generation, pd.DataFrame):
        ax[2].plot(optimized_generation['Min_cost'].cumsum()/1000, '.--', color='tab:red', ms=2.0, lw=1.0)
        legend.append('Fully optimized heat generation')
    ax[2].legend(legend, loc='upper left')
    ax[2].set_ylabel('Cumulative \n generation cost (k€)')
    ax[2].set_xlabel('Date')

    plt.close()

    return f


def plot_forecast_quality(historical_ts, forecasted_ts):
    """
    Creates figure with 2 subplots
        1) comparison of time series
        2) histogram of forecast errors

    :param pd.DataFrame historical_ts: historical time series of variable
    :param pd.DataFrame forecasted_ts: forecasted time series of variable
    :returns figure: figure of forecasted vs. historical time series
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
    ax[0].set_xlabel('Date')
    ax[0].set_ylabel(historical_ts.name)
    ax[0].legend(['Historical time series', 'Forecast (1 day ahead)'])
    ax[0].set_title('Time series comparison')
    # plot histogram of discrepancies
    ax[1].grid('both')
    ax[1].hist(error, bins=bins)
    ax[1].axvline(error.mean(), color='orange')
    ax[1].axvline(error.mean() - error.std(), ls='--', color='orange')
    ax[1].axvline(error.mean() + error.std(), ls='--', color='orange', label='_nolegend_')
    ax[1].set_xlabel('Forecast error (forecasted value - actual value)')
    ax[1].set_ylabel('Count of occurrences')
    ax[1].set_title('Forecast error histogram')
    ax[1].legend(['Mean', '+/- Standard deviation', error.name])

    f.suptitle(historical_ts.name + ' - forecast analysis')
    plt.tight_layout()

    plt.close()

    return f


def evaluate_historic_generation(historic_generation, gen_objects, gas_props, market_prices):
    """
    Returns total generation/sourcing cost for given heat amounts (per source and time interval)

    :param pd.DataFrame historic_generation: DataFrame with heat generation per source and time interval
    :param dict gen_objects: dictionary mapping heat generation objects to column header names in 'historic_generation'
    :param optima.GasProperties gas_props: gas property object describing energy content and CO2 factor
    :param optima.MarketPrices market_prices: price object containing electricity and gas prices
    :returns pd.DataFrame: DataFrame of total cost incurred per heat source in each time interval
    :returns pd.DataFrame: DataFrame of total gas consumption per heat source in each time interval
    :returns pd.DataFrame: DataFrame of total electricity generation per heat source in each time interval
    """

    first = True

    # loop over all heat sources
    for source in historic_generation.columns:
        # extract each source(column) as DataFrame and evaluate generation cost
        cost, gas, el = evaluate_generation_series(historic_generation[[source]], gen_objects[source],
                                                 gas_props, market_prices)
        # construct overarching DataFrame for cost of all heat sources
        if first:
            cost_overview = cost.copy()
            gas_overview = gas.copy()
            el_overview = el.copy()
            first = False
        else:
            cost_overview = pd.concat([cost_overview, cost], axis=1)
            gas_overview = pd.concat([gas_overview, gas], axis=1)
            el_overview = pd.concat([el_overview, el], axis=1)

    return cost_overview, gas_overview, el_overview


def evaluate_generation_series(generation_series, generator, gas_props, market_prices):
    """
    Returns total generation/sourcing cost for heat amounts given by 'generation_series' on 'generator'

    :param pd.DataFrame generation_series: one-column DataFrame of heat generation history of 'generator' (MWh/timestep)
    :param list generator: single-entry list with heat generator object
    :param optima.GasProperties gas_props: gas property object describing energy content and CO2 factor
    :param optima.MarketPrices market_prices: price object containing electricity and gas prices
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
    if isinstance(generator[0], (optima.HeatBoiler, optima.GasTurbine)):

        # adjust historic generation values below GT min load
        if isinstance(generator[0], optima.GasTurbine):
            gen_hist[(gen_hist.values > 0) & (gen_hist.values < generator[0].min_load / 2)] = 0.0
            gen_hist[(gen_hist.values > 0) & (gen_hist.values < generator[0].min_load)] = generator[0].min_load

        # derive whether heat generation was active in each time step
        gen_hist['on'] = [1 if (gen_hist.iloc[:, 0].values[i] > 0) else 0 for i in gen_hist.index]
        # derive start-up and shut-down time intervals
        gen_hist['switch'] = gen_hist['on'].diff()

        # derive hourly OPEX for all active intervals
        cost['OPEX_per_h'] = [optima.opex_per_h(generator[0], i) if
                              (gen_hist.iloc[:, 0].values[i] > 0) else 0 for i in gen_hist.index]
        # derive OPEX, gas demand, and (potential) electricity generation associated with heat amount
        ops = np.array([optima.opex_per_MWh(generator[0], gas_props, market_prices, i, quant_new=gen_hist.iloc[i, 0])
                                for i in gen_hist.index])
        cost['OPEX_per_MWh'] = ops[:, 0]
        cost['OPEX_all_MWhs'] = cost['OPEX_per_MWh'] * gen_hist.iloc[:, 0]
        # derive switching cost (only relevant for time steps > 0)
        cost['switching'] = 0.0     # initialise column
        cost.iloc[1:, -1] = [optima.switching_cost([], i-1, generator, 1) if (gen_hist['switch'].values[i] == 1)
                             else 0 for i in gen_hist.index[1:]]        # start-up
        cost.iloc[1:, -1] += [optima.switching_cost(generator, i-1, [], i) if (gen_hist['switch'].values[i] == -1)
                              else 0 for i in gen_hist.index[1:]]       # shut-down
        # add up hourly, consumption-driven, and switching cost for each time step
        cost.iloc[:, 0] = cost[['OPEX_per_h', 'OPEX_all_MWhs', 'switching']].sum(axis=1)

        # derive associated gas consumption and electricity co-generation
        gas_demand = ops[:, 1] * gen_hist.iloc[:, 0]
        el_gen = ops[:, 2] * gen_hist.iloc[:, 0]

    # evaluate sourcing cost from MHKW
    if isinstance(generator[0], optima.SourcingContract):

        # calculate cumulative heat sourcing (for respective year) to assess applicable heat unit price
        gen_hist['cumulative'] = gen_hist.cumsum()

        # derive applicable heat unit price based on already sourced annual volume
        cost['Unit_price'] = [generator[0].prices[0] if gen_hist['cumulative'][i] <= generator[0].qlimits_pa[0]
                             else generator[0].prices[1] for i in gen_hist.index]

        # calculate sourcing cost per time step
        cost.iloc[:, 0] = gen_hist.iloc[:, 0] * cost['Unit_price']

    return cost.iloc[:, 0], gas_demand, el_gen


def check_annual_sourcing_compliance(generation, contracts):
    """
    Checks whether total annual heat sourcing by municipal utility is compliant with contractual obligations /
    technical limitations of waste incineration plant (and potentially further external sourcing contracts)

    :params pd.DataFrame generation: optimized heat generation/sourcing distribution (quantity per source per interval)
                                     as returned by 'optimize_operating_modes'
    :params list contracts: list of all optima.SourcingContracts used by the optima.MunicipalUtility
    :returns boolean: boolean flag whether annual sourcing is compliant to contractual obligations/limitations
    """

    compliant = True

    # loop over all sourcing contracts
    for c in range(len(contracts)):
        # extract contract
        contract = contracts[c]
        # extract annual sourced heat of this contract
        quant = generation[contract.name].sum()
        # check whether actual sourced volume is between limits
        print('\nHeat sourcing contract: ' + contract.name)
        print('Minimum annual sourcing obligation: %.1f GWh' % (contract.qlimits_pa[0]/1000))
        print('Maximum annual supply limitation: %.1f GWh' % (contract.qlimits_pa[1]/1000))
        print('Total annual heat quantity sourced from MHKW: %.1f GWh' % (quant/1000))
        if not (contract.qlimits_pa[0] <= quant <= contract.qlimits_pa[1]):
            compliant = False

    if compliant:
        print('External heat sourcing is compliant with all annual obligations and limitations.')
    else:
        print('External heat sourcing is NOT compliant with all annual obligations and limitations.')

    return compliant


def check_gt_idle_time_compliance(generation, gas_turbines):
    """
    Checks whether minimum idle time (rest duration) requirements between two subsequent gas turbine deployments are
    adhered to, and returns a DataFrame containing all (potential) idle time violations

    :params pd.DataFrame generation: optimized heat generation/sourcing distribution (quantity per source per interval)
                                     as returned by 'optimize_operating_modes'
    :params list gas_turbines: list of all optima.GasTurbines used by the optima.MunicipalUtility
    :returns pd.DataFrame: DataFrame containing all minimum idle time violations of all gas turbines
    """

    # initialize DataFrame containing all violations
    all_violations = pd.DataFrame()

    # loop over all gas turbines
    for gt in range(len(gas_turbines)):
        # extract turbine
        turbine = gas_turbines[gt]
        # extract optimized gas turbine operation as DataFrame
        ops = generation[[turbine.name]].copy()
        # derive switching points in time (1: switched on in that interval, -1: switched off in that interval)
        ops['on/off'] = ops[turbine.name].apply(lambda x: 1 if x > 0 else 0)     # map 1 to all active time steps
        ops['switch'] = ops['on/off'].diff()

        # create new DataFrame with start-ups and previous shut-downs
        start = ops['switch'][ops['switch'] == 1].reset_index()['index'].rename('Start-up')
        shut = ops['switch'][ops['switch'] == -1].reset_index()['index'].rename('Previous shut-down')
        idle_time = pd.concat([shut, start], axis=1)

        if len(idle_time) > 0:
            # if first shut-down entry is larger than first start-up entry, shift shut-down column 1 row down to have
            # start-up and corresponding PREVIOUS shut-down as entries per row (and drop first row)
            if idle_time['Previous shut-down'][0] > idle_time['Start-up'][0]:
                idle_time['Previous shut-down'] = idle_time['Previous shut-down'].shift()
            idle_time.dropna(inplace=True)
            idle_time = idle_time.astype(int)

            # calculate idle time / resting duration between shut-down and subsequent start-up
            idle_time['idle time'] = idle_time['Start-up'] - idle_time['Previous shut-down']

            # extract DataFrame of all idle time violations and append to overall violations DataFrame
            violations = idle_time[idle_time['idle time'] < turbine.idle_period].copy()
            violations['GT'] = turbine.name
            all_violations = pd.concat([all_violations, violations], axis=0)

            # in case there are any idle time violations
            if len(violations) > 0:
                print('\nGas turbine: ' + turbine.name)
                print('Number of total idle time violations: %i' % len(violations))
                print('Share of idle time violations on total GT deployments: %.1f %%' % (len(violations)/len(idle_time)*100))
                print('Average idle time of idle time violations: %.1f h' % violations['idle time'].mean())
            # in case there are no idle time violations
            else:
                print('\nGas turbine: ' + turbine.name)
                print('Number of total idle time violations: %i \n' % 0)

        # in case there are no idle time violations
        else:
            print('\nGas turbine ' + turbine.name + ' has not been activated.')

    return all_violations


def analyse_idle_capacity(generation):
    """
    Analyses capacity usage on most expensive active heat generator per time step

    :params pd.DataFrame generation: optimized heat generation/sourcing distribution (quantity per source per interval)
                                     as returned by 'optimize_operating_modes'
    """

    # extract used and idle capacity on most expensive active heat generator
    used = generation['Used_capacity'].values
    idle = generation['Idle_capacity'].values
    # derive 'swing' capacity as min of used and idle capacity
    swing = pd.Series([min(used[i], idle[i]) for i in range(len(used))]).values

    # print results
    print('\nAverage used capacity on most expensive heat generator: %.1f' % used.mean())
    print('Average idle capacity on most expensive heat generator: %.1f' % idle.mean())
    print('Average swing capacity on most expensive heat generator: %.1f' % swing.mean())

    # derive bin width
    b = int(max(max(used), max(idle)) // 0.1)

    # plot histograms
    plt.rcParams['font.size'] = 14
    f, ax = plt.subplots(3, sharex=True, figsize=(16, 9))
    ax[0].grid('both')
    ax[0].hist(used, bins=b)
    ax[0].axvline(used.mean(), color='orange')
    ax[0].set_ylabel('Count of occurrences')
    ax[0].legend(['Average', 'Used capacity on last heat generator'])
    ax[1].grid('both')
    ax[1].hist(idle, bins=b)
    ax[1].axvline(idle.mean(), color='orange')
    ax[1].set_ylabel('Count of occurrences')
    ax[1].legend(['Average', 'Idle capacity on last heat generator'])
    ax[2].grid('both')
    ax[2].hist(swing, bins=b)
    ax[2].axvline(swing.mean(), color='orange')
    ax[2].set_ylabel('Count of occurrences')
    ax[2].legend(['Average', 'Swing capacity on last heat generator'])
    ax[2].set_xlabel('Capacity (MW)')
    f.suptitle('Used/idle capacity on most expensive heat generator')
    plt.close()

    return f



####################     POSTPROCESSING TEST     ####################

if __name__ == '__main__':
    pass
