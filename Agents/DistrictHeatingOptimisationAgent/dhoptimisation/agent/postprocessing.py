################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 20 Nov 2023                            #
################################################

# The purpose of this module is to provide methods for fitting, saving and
# loading gas consumption and electricity co-generation models for heat generators

import pandas as pd

from dhoptimisation.agent.optimisation_setup import *
from dhoptimisation.agent.optimisation_tasks import *


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
    cost_overview['Min_Cost'] = cost_overview.sum(axis=1)
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
