################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 25 Sep 2023                            #
################################################

# The purpose of this module is to provide the main optimisation logic and
# methods for the DHOptimisationAgent

import numpy as np
import pandas as pd
from CoolProp.CoolProp import PropsSI

from dhoptimisation.utils import *
from dhoptimisation.datamodel import *
from dhoptimisation.agent.config import TIME_FORMAT
from dhoptimisation.agent.optimisation_setup import HeatBoiler, GasTurbine, SourcingContract


def generation_optimization(municipal_utility, market_prices, datetime_index, previous_state,
                            rounding=2):
    """
    Run heat generation/sourcing cost optimization as "model-predictive control" implementation
    NOTE: Using the DIF, receding optimisation periods are sent to the agent as subsequent
          requests (get triggered when optimisation period gets updated), i.e.,
          If an overall period [2020-01-01 00:00:00, 2020-01-03 00:00:00] shall be
          optimised with an MPC horizon of 24h, 24 subsequent requests/optimisations
          will be handled by the agent:
          1) Optimise [2020-01-01 00:00:00, 2020-01-02 00:00:00]
          2) Populate optimised results as forecasts back to KG for entire period (24h)
          3) The subsequent request will retrieve and optimise data for
             [2020-01-01 01:00:00, 2020-01-02 01:00:00] and, hence, use the output
             for the first optimised time step from the previous step
          etc.

    :param MunicipalUtility municipal_utility: municipal utility object (with heat demand, connected grid, etc.)
    :param MarketPrices market_prices: market prices object with electricity, co2, gas, etc. prices as time series
    :param DateTimeIndex datetime_index: original datetime index corresponding to integer-based indices of objects
    :param PreviousSystemState previous_state: object describing the generation setup of the previous time step
    :param int rounding: Number of meaningful decimal places to round results
    
    :returns pd.DataFrames: optimised heat generation across modes w/ and w/o GT ('opt'),
                            as well as internally optimised modes w/ GT ('wgt') and w/o GT ('wogt')
    """

    # Load previous generation setup/generator states to define starting conditions
    # (i.e., whether GT has been active in previous time step)
    gt_active = previous_state.gt_active
    gt_benefit = previous_state.gt_benefit
    previous_setup_woGT = previous_state.setup_woGT
    previous_setup_wGT = previous_state.setup_wGT
    logger.info(f'Starting with active GT: {gt_active}')
    logger.info(f'Starting GT benefit: {gt_benefit}')

    # Incorporate GT idle time requirement (i.e., potentially mark GT as unavailable)
    gts = [gt.name for gt in municipal_utility.gas_turbines]
    for gt in municipal_utility.gas_turbines:
        # Reset potentially previously amended availability (to ensure that
        # unrelated optimisation runs, are actually independent of one another)
        municipal_utility.gas_turbines[gts.index(gt.name)].available[:] = 1
        if (not gt_active) and (gt.get_idle_time() < gt.idle_period):
            # Only for currently not active GT: if current idle time since last active operation is
            # shorter than required idle_period, flag gas turbine as unavailable for remaining time steps
            remaining = gt.idle_period - gt.get_idle_time()
            municipal_utility.gas_turbines[gts.index(gt.name)].available[0: remaining] = 0

    # Define heat sources for both heat generation modes: mode1 w/o GT, mode2 w/ GT
    sources_mode1 = []
    sources_mode1.extend(municipal_utility.boilers)
    sources_mode1.extend(municipal_utility.contracts)
    sources_mode2 = sources_mode1.copy()
    sources_mode2.extend(municipal_utility.gas_turbines)

    # Create profit lines for both heat generation modes
    logger.info('Minimising generation cost for heat generation w/o GT ...')
    wogt = minimize_generation_cost(municipal_utility, sources_mode1, market_prices, 
                                    datetime_index, preceding_setup=previous_setup_woGT)
    logger.info('Minimising generation cost for heat generation with GT ...')
    wgt = minimize_generation_cost(municipal_utility, sources_mode2, market_prices, 
                                   datetime_index, preceding_setup=previous_setup_wGT)

    # Optimize operating modes
    logger.info('Optimising both operating modes ...')
    opt = optimize_operating_modes(wogt, wgt, gt_active=gt_active, prev_gt_benefit=gt_benefit)
    # Extract optimised setups for first time step as new previous setups (for subsequent run)
    new_previous_woGT = wogt['Active_generator_objects'][0]
    new_previous_wGT = wgt['Active_generator_objects'][0]

    # Update parameters describing GT state and benefit (max, cumulative) based 
    # on first optimised time step in current interval
    logger.info('Assessing GT activity flag and (max, cumulative) benefit ...')
    for gt in municipal_utility.gas_turbines:
        if opt[gt.name+'_q'][datetime_index[0]] > 0:
            gt_active = True
            if gt.q_hist.iloc[-1] == 0:
                # in case GT has just been activated, initialise max and cumulative
                # benefit from current operation
                gt_benefit = [0.0, 0.0]
                gt_benefit[1] += wogt['Min_cost'].iloc[0] - opt['Min_cost'].iloc[0] + switching_cost([], 0,
                            [gt for gt in wgt.iloc[0]['Active_generator_objects'] if isinstance(gt, GasTurbine)], 0)
            else:
                # update cumulative GT benefit from current operation
                gt_benefit[1] += wogt['Min_cost'].iloc[0] - opt['Min_cost'].iloc[0]
            # update max benefit from current operation
            gt_benefit[0] = max(gt_benefit[0], gt_benefit[1])
        else:
            # Mark GT as inactive and reset benefit
            gt_active = False
            gt_benefit = None

    if rounding:
        logger.info(f'Rounding numeric results to {rounding} digits ...')
        # Exclude list of generator objects from rounding
        columns_to_round = opt.columns.difference(['Active_generator_objects'])  
        # Round results to meaningful number of decimal places
        opt[columns_to_round] = opt[columns_to_round].astype(float).round(rounding)
        wogt[columns_to_round] = wogt[columns_to_round].astype(float).round(rounding)
        wgt[columns_to_round] = wgt[columns_to_round].astype(float).round(rounding)
    
    # Set optimisation results for first time step as new previous system state
    # (i.e., to be used in subsequent optimisation run for next time step)
    logger.info('Updating system state for (potential) subsequent optimisation ...')
    opti_start_dt = datetime_index[0].strftime(TIME_FORMAT)
    previous_state.update_system_state(opti_start_dt, gt_active, gt_benefit,
                                       new_previous_woGT, new_previous_wGT)

    return opt, wogt, wgt
    
    
def minimize_generation_cost(municipal_utility, sourcing_mode, market_prices,
                             datetime_index, preceding_setup=[]):
    """
    Returns DataFrame with operating conditions (i.e., generation amount per heat source)
    for timesteps contained in "datetime_index", as well as associated cost

    :param MunicipalUtility municipal_utility: municipal utility object (with heat demand, connected grid, etc.)
    :param list sourcing_mode: list of heat generator/sourcing objects to be generally considered 
                               (HeatBoiler, GasTurbine, SourcingContract)
    :param MarketPrices market_prices: market price object
    :param pd.DatetimeIndex datetime_index: datetime time steps to optimise
    :param list preceding_setup: list of heat generator/sourcing objects active in time step
                                 prior to current optimisation interval
    
    :returns pd.DataFrame: DataFrame with operating conditions (heat generation per source) for each time step in
                          'datetime_index', along with associated minimal cost and active heat generator objects
    """

    # Create empty DataFrame with required columns
    columns = ['Q_demand', 'Min_cost', 'Gas_consumption', 'Electricity_generation', 
               'Used_capacity', 'Idle_capacity', 'Active_generator_objects']
    # Add heat generation columns
    columns.extend([c.name+'_q' for c in municipal_utility.contracts])
    columns.extend([g.name+'_q' for g in municipal_utility.gas_turbines])
    columns.extend([b.name+'_q' for b in municipal_utility.boilers])
    # Add gas consumption columns
    columns.extend([g.name+'_gas' for g in municipal_utility.gas_turbines])
    columns.extend([b.name+'_gas' for b in municipal_utility.boilers])
    ops_overview = pd.DataFrame(columns=columns)

    # initialise generator setup from preceding time step
    old = preceding_setup

    # loop over all time steps in datetime_index (using generic integer index)
    for t in range(len(datetime_index)):
        logger.info(f'Minimising cost for timestep {t}/{len(datetime_index)} ...')

        # Retrieve minimum amount of heat to be supplied by SWPS to ensure grid stability
        # (due to min circulation volume by SWPS), MWh
        min_supply = minimum_supply(municipal_utility, t)
        # ensure that heat demand is ALWAYS equal / greater than required minimum supply
        demand = max(min_supply, municipal_utility.q_demand[t])

        # get sorted DataFrame of available heat capacities for respective heat generation mode and timestep
        logger.info('Deriving available heat generators, sorted by unit cost and capacity ...')
        capas = get_available_heat_capacities(sourcing_mode, min_supply, municipal_utility.fuel, market_prices, t)

        # calculate minimum cost required to satisfy heat demand
        logger.info('Deriving minimal cost to satisfy heat demand ...')
        ops_data = get_min_cost_for_timestep(demand, capas, municipal_utility.fuel, market_prices, t)

        ###   include switching cost WITHIN same operating mode   ###
        # i.e., switching cost incurred by demand-driven shut-down/start-up of heat generators 
        # and NOT cost associated with switching from mode w/o GT to mode w/ GT or vice versa

        # check for potential switching cost
        # derive current heat generation set-up
        new = ops_data.copy()
        # assess potential switching cost between old and new heat generation set-up
        if t == 0:
            cost = switching_cost(old, t, new['Active_generator_objects'], t)
        else:
            cost = switching_cost(old['Active_generator_objects'], t-1, new['Active_generator_objects'], t)
        # add switching cost
        ops_data['Min_cost'] += cost
        # store current heat generation set-up for next time step
        old = ops_data.copy()

        # append results for respective timestep at bottom of DataFrame
        ops_overview = pd.concat([ops_overview, pd.DataFrame([ops_data])], ignore_index=True)

    # fill all empty cells with 0's
    ops_overview.fillna(0, inplace=True)
    # replace generic integer index with corresponding datetime_index
    ops_overview.index = datetime_index

    return ops_overview


def optimize_operating_modes(operating_mode_woGT, operating_mode_wGT, gt_active=False,
                             prev_gt_benefit=None):
    """
    Returns a DataFrame of optimized heat generation modes and associated cost

    :param pd.DataFrame operating_mode_woGT: optimized heat generation for each time step based on mode w/o GT
    :param pd.DataFrame operating_mode_wGT: optimized heat generation for each time step based on mode w/ GT
    :param boolean gt_active: flag whether gas turbine is currently active
    :param float prev_gt_benefit: cumulative previous benefit from current gas turbine operation
    
    :returns pd.DataFrame: DataFrame with optimized heat generation conditions (based on mode 1 and 2) for each time step
    """

    # initialize DataFrame with optimized operating conditions
    optimized = pd.DataFrame(columns=operating_mode_woGT.columns)

    # initialize current best AND alternative operating mode for initial time step
    if gt_active:
        current_best = operating_mode_wGT.copy()
        alternative = operating_mode_woGT.copy()
    else:
        current_best = operating_mode_woGT.copy()
        alternative = operating_mode_wGT.copy()

    # initialize previous GT benefit parameters
    gt_benefit = prev_gt_benefit if prev_gt_benefit else [0.0, 0.0]

    t = 0
    # loop over all time steps in operating_modes
    while t < len(current_best.index):
        # check whether current operating mode is still more profitable
        if current_best['Min_cost'].iloc[t] <= alternative['Min_cost'].iloc[t]:
            # append current_best operating conditions to optimized conditions for respective time step
            optimized = pd.concat([optimized, current_best.iloc[t].to_frame().transpose()])
            if gt_active:
                # update cumulative and max benefit from CURRENT GT operation
                gt_benefit[1] += alternative['Min_cost'][t] - current_best['Min_cost'][t]
                gt_benefit[0] = max(gt_benefit[0], gt_benefit[1])
            t += 1
        else:
            # extract profit lines and heat generation set-ups for current time step and onwards & reset indices
            current_cost = current_best['Min_cost'][t:].reset_index(drop=True)
            alternative_cost = alternative['Min_cost'][t:].reset_index(drop=True)
            current_setup = current_best['Active_generator_objects'][t:].reset_index(drop=True)
            alternative_setup = alternative['Active_generator_objects'][t:].reset_index(drop=True)
            if gt_active:
                switch, period, cost = assess_switching_period(current_cost, alternative_cost, current_setup,
                                                               alternative_setup, prev_gt_benefit=gt_benefit)
            else:
                switch, period, cost = assess_switching_period(current_cost, alternative_cost, current_setup,
                                                               alternative_setup)
            # loop over length of evaluated 'switching' period
            for dt in range(period+1):
                if switch:
                    if dt == 0:
                        # reset previously accumulated gt benefit
                        gt_benefit = [0, 0]
                        # add switching cost to first interval in switching period
                        switching_interval = alternative.iloc[t].copy()
                        switching_interval['Min_cost'] += cost
                        optimized = pd.concat([optimized, switching_interval.to_frame().transpose()])
                    else:
                        optimized = pd.concat([optimized, alternative.iloc[t].to_frame().transpose()])
                else:
                    optimized = pd.concat([optimized, current_best.iloc[t].to_frame().transpose()])
                    if gt_active:
                        # update cumulative and max benefit from CURRENT GT operation
                        gt_benefit[1] += alternative['Min_cost'][t] - current_best['Min_cost'][t]
                        gt_benefit[0] = max(gt_benefit[0], gt_benefit[1])
                t += 1

    return optimized


def assess_switching_period(current_cost_line, alternative_cost_line, current_set_up,
                            alternative_set_up, prev_gt_benefit=None):
    """
    Returns a boolean flag whether heat generation modes shall be switched, associated switching cost and
    the respective time period for which the modes shall/shall not be switched

    :param pd.DataFrame current_cost_line: series of minimal cost for each time step in optimization period
    :param pd.DataFrame alternative_cost_line: series of minimal cost for each time step in optimization period
    :param pd.DataFrame current_set_up: list of active heat generation/sourcing objects associated with max profit for
                                        each time step in optimization period
    :param pd.DataFrame alternative_set_up: list of active heat generation/sourcing objects associated with max profit for
                                            each time step in optimization period
    :param float prev_gt_benefit: max and cumulative previous benefit from current gas turbine operation
    
    :returns boolean switch: flag whether to switch profitably or not
    :returns int timesteps: number of time steps for which switching is/is not profitable
    :returns float cost: switching cost incurred by switching between modes
    """

    # initialize parameters
    switch = False              # switching flag
    first_neg = True            # flag to indicate when accumulated benefit trend reverses,
                                # i.e., acc. benefit decreases
    # accumulated benefit from CURRENT gas turbine operation (over multiple time steps)
    if prev_gt_benefit:
        benefit = prev_gt_benefit[1]
        acc_benefit = np.array([prev_gt_benefit[0]])
    else:
        benefit = 0.0
        acc_benefit = np.array([benefit])   # track accumulated benefit over time
    dt = 0                                  # relative time stepping wrt start of benefit evaluation

    # add incremental switching benefits (until sum decreases by more than switching cost OR time series end is reached)
    while dt < len(current_cost_line):
        # evaluate benefit increment of alternative mode vs. current mode for timestep dt
        inc = current_cost_line[dt] - alternative_cost_line[dt]
        # if gas turbine currently active, adjust sign to account for incremental benefit loss (as cost lines only cross
        # and 'assess_switching_period' is called when alternative becomes more economical than current)
        if acc_benefit[0] > 0:
            inc *= -1
        benefit += inc
        acc_benefit = np.append(acc_benefit, benefit)

        # in case of negative incremental benefit
        if inc < 0:
            # assess switching cost to timestep dt with maximum benefit
            if first_neg:
                # get time step with maximum accumulated benefit, i.e. find local maximum in current assessment interval
                tmax = acc_benefit[1:].argmax()
                # evaluate switching cost @ time steps 0 and tmax
                # initial switch to alternative set_up
                cost1 = switching_cost(current_set_up[0], 0, alternative_set_up[0], 0)
                # final switch back to current set_up
                cost2 = switching_cost(alternative_set_up[tmax], tmax, current_set_up[tmax], tmax)
                first_neg = False
            # if accumulated benefit drops by more than switching cost, stop further evaluation and assess "only"
            # period until tmax (period of decreasing acc. benefit thereafter is more profitable without switching)
            if (((acc_benefit.max() - benefit) > (cost1 + cost2)) or (benefit < 0)):
                # assess whether switching modes is beneficial considering switching cost (if gas turbine not active)
                if ((acc_benefit[0] == 0)) and (acc_benefit.max() > (cost1 + cost2)):
                    switch = True
                # in case gas turbine is active, switch only if benefit 'continuously' decreases from maximum @ dt == 0
                elif ((acc_benefit[0] > 0) and (tmax == 0)):
                    switch = True
                    tmax = dt
                    # in case gas turbine has not generated any previous benefit, adjust switching cost to suppress
                    # artificially high switching cost for occasions when 'optimize_operating_modes' is called with
                    # 'active' gt, but mode w/o gt is lastingly more economical
                    if acc_benefit.max() == 0:
                        cost1, cost2 = 0.0, 0.0
                    # NOTE: Return only initial switching cost due to MPC style optimization;
                    # Switching back will be included in subsequent optimisation at later time step
                    return switch, tmax, cost1

        else:
            # potentially reset 'first_neg' flag (e.g., if acc. benefit again increases after a single drop)
            first_neg = True

        # increment time
        dt += 1

    # in case end of time series is reached (without any larger drops etc.):
    # evaluate initial switching cost @ time step 0 (without switching back)
    cost = switching_cost(current_set_up[0], 0, alternative_set_up[0], 0)

    # assess whether switching modes is beneficial considering switching cost; only include final check if gas turbine
    # is currently not active and might be potentially switched on; if gas turbine is currently active and accumulated
    # benefit drops significantly, this case is already handled above, otherwise keep gas turbine running
    if ((acc_benefit[0] == 0) and (acc_benefit[dt-1] > cost)):
        switch = True

    return switch, dt-1, cost


def get_available_heat_capacities(heat_sources, min_supply, gas_props, market_prices, timestep):
    """
    Returns sorted DataFrame (by ascending expected heat unit cost) of all available heat capacities by provided list
    of 'heat_sources' for given time step; prioritizes GT when in 'heat_sources' (only applicable for mode /w GT)

    :param list heat_sources: list of heat generator/sourcing objects (HeatBoiler, GasTurbine, SourcingContract)
    :param float min_supply: minimum heat supply required to stabilise grid (due to temp. spread and min circulation)
    :param GasProperties gas_props: gas properties object describing the gas used by heat generators
    :param MarketPrices market_prices: market price object
    :param int timestep: time step in optimization period to derive available heat capacity for
    
    :returns pd.DataFrame: sorted DataFrame of all available heat capacities at time 'timestep'
    """

    # extract list of all heat generators capable of providing minimum heat supply
    # --> ALL HKW operated heat generators (all boilers, GTs) are suited
    min_heat_suppliers = [source for source in heat_sources if isinstance(source, (HeatBoiler, GasTurbine))]

    # initialize output DataFrame with [source name, source capacity, priority, unit price, specific gas demand (MWh gas
    #                                   per MWh heat (wrt lower calorific value), specific electricity generation (MWh
    #                                   electricity per MWh heat), source object]
    # priorities to be set as follows:
    #   1 - Minimum heat supply required to keep grid stable
    #   2 - GT (to be considered before any other capacity; only available & relevant in mode w/ GT)
    #   3 - any other available capacity
    capacities = pd.DataFrame(columns=['source', 'capacity', 'priority', 'unit_price', 'gas_cons', 'el_gen',
                                       'generator_object'])

    # 2 consecutive runs through all heat sources to derive prioritized & sorted list of all capacities wrt unit_price
    # 1st loop: derive cheapest generator(s) to provide minimum supply to the grid
    # 2nd loop: derive sorted list of all remaining capacities
    for run in range(2):
        # loop over all heat sources in heat_sources list
        for source in heat_sources:

            # extract capacity and heat unit price for conventional heat boilers
            if isinstance(source, HeatBoiler):
                # only consider available capacity
                if source.available[timestep] == 1:
                    if source.name in capacities['source'].values:
                        # if boiler already in 'capacities', only assess variable cost for incremental capacity
                        # derive already used capacity of boiler
                        used = capacities['capacity'][capacities['source'] == source.name].sum()
                        # opex for additional capacity
                        opex, gas_demand, el_gen = opex_per_MWh(source, gas_props, market_prices, timestep,
                                                                quant_old=used, quant_new=(source.capacity - used))
                        new_row = {'source': source.name, 'capacity': (source.capacity-used), 'priority': 3,
                                   'unit_price': opex, 'gas_cons': gas_demand, 'el_gen': el_gen,
                                   'generator_object': source}
                        capacities = pd.concat([capacities, pd.DataFrame([new_row])], ignore_index=True)
                    else:
                        # if boiler not yet in 'capacities', calculate heat unit price for maximum hourly generation
                        opex1 = opex_per_h(source, timestep) / source.capacity
                        opex2, gas_demand, el_gen = opex_per_MWh(source, gas_props, market_prices, timestep,
                                                                 quant_new=source.capacity)
                        # add capacity with priority 3
                        new_row = {'source': source.name, 'capacity': source.capacity, 'priority': 3,
                                   'unit_price': opex1 + opex2, 'gas_cons': gas_demand, 'el_gen': el_gen,
                                   'generator_object': source}
                        capacities = pd.concat([capacities, pd.DataFrame([new_row])], ignore_index=True)

            # extract heat capacity and heat unit price for gas turbine
            elif isinstance(source, GasTurbine):
                # only consider available capacity
                if source.available[timestep] == 1:
                    if source.name in capacities['source'].values:
                        # if GT already in 'capacities', only assess variable cost for incremental capacity
                        used = capacities['capacity'][capacities['source'] == source.name].sum()
                        # derive already used capacity of GT
                        opex, gas_demand, el_gen = opex_per_MWh(source, gas_props, market_prices, timestep,
                                                                quant_old=used, quant_new=(source.power_q - used))
                        new_row = {'source': source.name, 'capacity': (source.power_q-used), 'priority': 2,
                                   'unit_price': opex, 'gas_cons': gas_demand, 'el_gen': el_gen,
                                   'generator_object': source}
                        capacities = pd.concat([capacities, pd.DataFrame([new_row])], ignore_index=True)
                    else:
                        # if GT not yet in 'capacities', calculate heat unit price for maximum hourly generation
                        opex1 = opex_per_h(source, timestep) / source.power_q
                        opex2, gas_demand, el_gen = opex_per_MWh(source, gas_props, market_prices, timestep,
                                                                 quant_new=source.power_q)
                        # add capacity with priority 2
                        new_row = {'source': source.name, 'capacity': source.power_q,
                                   'priority': 2, 'unit_price': opex1 + opex2, 'gas_cons': gas_demand,
                                   'el_gen': el_gen, 'generator_object': source}
                        capacities = pd.concat([capacities, pd.DataFrame([new_row])], ignore_index=True)

            # extract heat supply capacity and heat unit price for external sourcing contracts
            elif isinstance(source, SourcingContract):
                # only consider available capacity
                if source.available[timestep] == 1:
                    # add full technical capacity of heat sourcing contract with priority 3
                    new_row = {'source': source.name, 'capacity': source.qmax[timestep],
                               'priority': 3, 'unit_price': source.current_price, 'gas_cons': 0.0,
                               'el_gen': 0.0, 'generator_object': source}
                    capacities = pd.concat([capacities, pd.DataFrame([new_row])], ignore_index=True)

        # incorporate minimum heat supply requirement, by prioritising cheapest capable capacities
        if run == 0:
            # extract only subset of generators capable of providing minimum heat for grid stability
            capa_sorted = capacities[capacities['generator_object'].isin(min_heat_suppliers)].copy()
            # sort DataFrame by descending capacity (1st), ascending unit price (2nd), and ascending priority (3rd)
            capa_sorted.sort_values(by=['priority', 'unit_price', 'capacity'], ascending=[True, True, False],
                                    inplace=True)
            # reset index
            capa_sorted = capa_sorted.reset_index(drop=True)

            # evaluate required capacity for each (subsequent) heat generator required
            req = [min_supply]
            for c in capa_sorted['capacity']:
                req.append(req[-1] - c)
            # derive sub-DataFrame of required generators and capacities to satisfy minimum heat demand
            min_heat_capas = capa_sorted.copy()
            min_heat_capas['required'] = req[:-1]
            # drop all unnecessary heat generators
            min_heat_capas = min_heat_capas[min_heat_capas['required'] >= 0]
            # assign required capacity for each heat generator: either capacity or remaining amount until min supply
            min_heat_capas['capacity'] = np.where(min_heat_capas['capacity'] <= min_heat_capas['required'],
                                                  min_heat_capas['capacity'], min_heat_capas['required'])

            # reset capa_sorted to be filled with required capacities to provide minimum heat supply
            capa_sorted = pd.DataFrame(columns=['source', 'capacity', 'priority', 'unit_price', 'gas_cons', 'el_gen',
                                                'generator_object'])
            # loop through all heat generators required to provide minimum heat
            for row in range(len(min_heat_capas)):
                # if full generator capacity is needed
                if row < len(min_heat_capas)-1:
                    # simply re-assign previously derived capacity and unit_price with priority 1
                    min_supplier = min_heat_capas.loc[row, capa_sorted.columns].copy()
                    min_supplier['priority'] = 1
                    capa_sorted = pd.concat([capa_sorted, min_supplier.to_frame().transpose()])
                else:
                    # if capacity is only needed partially (i.e. row is last entry)
                    min_supplier = min_heat_capas.loc[row, capa_sorted.columns].copy()
                    min_supplier['priority'] = 1
                    source = min_supplier['generator_object']
                    # re-evaluate unit_price for partial load (full hourly opex, variable opex depending on load)
                    opex1 = opex_per_h(source, timestep) / min_supplier['capacity']
                    opex2, gas_demand, el_gen = opex_per_MWh(source, gas_props, market_prices, timestep,
                                                             quant_new=min_supplier['capacity'])
                    min_supplier['unit_price'] = opex1 + opex2
                    min_supplier['gas_cons'] = gas_demand
                    min_supplier['el_gen'] = el_gen
                    capa_sorted = pd.concat([capa_sorted, min_supplier.to_frame().transpose()])

            # re-initialise 'capacities' DataFrame for 2nd run, now with minimum heat supply already satisfied
            capacities = capa_sorted

    # create final output DataFrame
    capa_sorted = capacities.copy()
    # sort DataFrame by descending capacity (first), ascending unit price (second), and ascending priority (third)
    capa_sorted.sort_values(by=['priority', 'unit_price', 'capacity'], ascending=[True, True, False], inplace=True)
    # reset index
    capa_sorted = capa_sorted.reset_index(drop=True)

    return capa_sorted


def opex_per_h(generator, timestep):
    """
    Returns non-load-dependent OPEX per operating hour, €/h

    :param HeatBoiler|GasTurbine generator: heat generator object (HeatBoiler or GasTurbine)
    :param int timestep: time step in optimization period to evaluate opex for
    
    :returns float: OPEX per operating hour
    """

    # hourly OPEX for CHP gas turbine (wear dependent on operation duration)
    if isinstance(generator, GasTurbine):
        # time-dependent wear and labour cost (shift surcharges, etc.)
        opex = generator.cost_wear[timestep] + generator.cost_labour[timestep]

    # hourly OPEX for conventional heat boilers (wear dependent on MWh produced)
    elif isinstance(generator, HeatBoiler):
        # only time-dependent labour cost (shift surcharges, etc.)
        opex = generator.cost_labour[timestep]

    else:
        opex = 0.0

    return opex


def opex_per_MWh(generator, gas_props, market_prices, timestep, quant_old=0.0, quant_new=1.0):
    """
    Returns load-dependent (average) OPEX per generated MWh of heat (€/MWh_q) as well as 
    specific (average) gas consumption (MWh_g(wrt Hu)/MWh_q)
        - for gas turbines also returns specific (average) electricity generation (MWh_el/MWh_q)
        - for gas turbines: if heat load < minimum heat load, returns np.nans

    :param HeatBoiler|GasTurbine generator: heat generator object (HeatBoiler or GasTurbine)
    :param GasProperties gas_props: gas properties object (containing calorific values and CO2 factor)
    :param MarketPrices market_prices: market price object (containing fuel and electricity prices)
    :param int timestep: time step in optimization period to evaluate opex for
    :param float quant_old: previously generated heat quantity on generator (in SAME timestep), MWh
    :param float quant_new: (additional) heat quantity to be generated, MWh
    
    :returns float: (average) OPEX per generated MWh of heat (already includes electricity revenue for GT)
    :returns float: (average) amount of gas per generated MWh of heat (wrt lower calorific value)
    :returns float: (average) amount of electricity co-generated per generated MWh of heat
    """

    # return 0 for 'inactive' heat generators
    if quant_new <= 0:
        return 0.0, 0.0, 0.0

    # (average) OPEX and gas consumption per MWh heat for conventional heat boilers
    if isinstance(generator, HeatBoiler):
        # MWhs of gas required to generate (additional) heat (wrt lower calorific value)
        # based on gas demand model ( gas consumption = f (heat generation) )
        if quant_old == 0:
            # consider potentially higher initial gas consumption, i.e. non-zero gas demand for ~0 heat generation
            # reshaping necessary to satisfy sklearn model requirement
            heat_amount = np.array(quant_new).reshape(-1, 1)
            # extract predicted value from np.array
            gas_amount = generator.gas_demand.predict(heat_amount)[0]
        else:
            # only consider incremental gas amount required to generate additional heat quantity
            heat_amount_old = np.array(quant_old).reshape(-1, 1)
            heat_amount_new = np.array(quant_old + quant_new).reshape(-1, 1)
            gas_amount_old = generator.gas_demand.predict(heat_amount_old)[0]
            gas_amount_new = generator.gas_demand.predict(heat_amount_new)[0]
            gas_amount = gas_amount_new - gas_amount_old
            
        # multiply with gas & co2 cost per MWh of gas (gas price refers to ho, while CO2 factor refers to hu)
        opex = gas_amount * ((gas_props.ho / gas_props.hu) * market_prices.gas_q[timestep] +
                             gas_props.co2_factor * market_prices.co2[timestep])
        # add wear cost (€/MWh_q)
        opex += (generator.cost_wear[timestep] * quant_new)
        # define specific return variables
        spec_opex = opex / quant_new
        spec_gas_cons = gas_amount / quant_new
        spec_el_gen = 0.0

    # (average) OPEX, gas consumption, and electricity generation per MWh heat for GTs
    elif isinstance(generator, GasTurbine):
        if (quant_old + quant_new) < generator.min_load:
            # set return variables to NaN for heat load below minimum load
            spec_opex = np.nan
            spec_gas_cons = np.nan
            spec_el_gen = np.nan
        else:
            # MWhs of gas required to generate heat (wrt lower calorific value)
            # based on gas demand and electricity output models
            if quant_old == 0:
                # consider initial 'offsets' of gas consumption and electricity generation, i.e. non-zero gas demand
                # and electricity generation for ~0 heat generation
                # reshaping necessary to satisfy sklearn model requirement
                heat_amount = np.array(quant_new).reshape(-1, 1)
                # extract predicted values from np.array
                gas_amount = generator.gas_demand.predict(heat_amount)[0]
                el_amount = generator.el_output.predict(heat_amount)[0]
            else:
                # only consider incremental gas and electricity amounts associated with additional heat quantity
                heat_amount_old = np.array(quant_old).reshape(-1, 1)
                heat_amount_new = np.array(quant_old + quant_new).reshape(-1, 1)
                gas_amount_old = generator.gas_demand.predict(heat_amount_old)[0]
                gas_amount_new = generator.gas_demand.predict(heat_amount_new)[0]
                gas_amount = gas_amount_new - max(0.0, gas_amount_old)
                el_amount_old = generator.el_output.predict(heat_amount_old)[0]
                el_amount_new = generator.el_output.predict(heat_amount_new)[0]
                el_amount = el_amount_new - max(0.0, el_amount_old)
                
            # multiply with gas & co2 cost per MWh of gas (gas price refers to ho, while CO2 factor refers to hu)
            opex = gas_amount * ((gas_props.ho / gas_props.hu) * market_prices.gas_gt[timestep] +
                                 gas_props.co2_factor * market_prices.co2[timestep])
            # subtract revenue for co-generated electricity
            opex -= (el_amount * market_prices.get_el_remun(timestep))
            # define specific return variables
            spec_opex = opex / quant_new
            spec_gas_cons = gas_amount / quant_new
            spec_el_gen = el_amount / quant_new

    else:
        spec_opex = 0.0
        spec_gas_cons = 0.0
        spec_el_gen = 0.0

    return spec_opex, spec_gas_cons, spec_el_gen


def get_min_cost_for_timestep(q_demand, sourcing_capacities, gas_props, market_prices, timestep):
    """
    Returns dictionary comprised of minimal cost (both sourcing and generation) to satisfy heat demand, how heat
    generation shall be distributed across available sources, and list of required heat generation objects

    :param float q_demand: expected heat demand for current optimization time step
    :param pd.DataFrame sourcing_capacities: sorted DataFrame of all available heat capacities at time 'timestep'
                                             (as returned by function 'get_available_heat_capacities')
    :param GasProperties gas_props: gas properties object (containing calorific values and CO2 factor)
    :param MarketPrices market_prices: market price object
    :param int timestep: time step in optimization period to derive minimum sourcing/generation cost for
    
    :returns dict: dict of heat generation per available source, associated minimal required cost, and list of
                   required heat generator objects
    """

    ###   verify suitability of sourcing capacities (only relevant for mode w/ GT)   ###

    # retrieve all potential GT entries in sourcing_capacities DataFrame
    gt = [isinstance(gt, GasTurbine) for gt in sourcing_capacities['generator_object']]
    gt_capas = sourcing_capacities[gt]
    index = gt_capas.index
    # in case sourcing_capacities contains GT entries
    if len(index) > 0:
        # retrieve GT object
        gt = sourcing_capacities[gt]['generator_object'].unique()[0]
        if q_demand >= gt.min_load:
            # if total heat demand is large enough to allow for GT operation --> summarise individual gt capacities
            sourcing_capacities = sourcing_capacities.drop(index[1:])
            sourcing_capacities.loc[index[0], 'capacity'] = gt_capas['capacity'].sum()
            # re-assess unit price, specific gas consumption, and specific electricity generation for cumulative load
            opex1 = opex_per_h(gt, timestep) / gt.power_q
            opex2, gas_demand, cogen = opex_per_MWh(gt, gas_props, market_prices, timestep,
                                                   quant_new=gt_capas['capacity'].sum())
            sourcing_capacities.loc[index[0], 'unit_price'] = opex1 + opex2
            sourcing_capacities.loc[index[0], 'gas_cons'] = gas_demand
            sourcing_capacities.loc[index[0], 'el_gen'] = cogen
            # reset index
            sourcing_capacities = sourcing_capacities.reset_index(drop=True)
        else:
            # if total heat demand is too small to allow for GT operation --> re-create sourcing_capacities with
            # all current non-GT heat generators
            non_gts = sourcing_capacities['generator_object'].unique()
            non_gts = [gen for gen in non_gts if not isinstance(gen, GasTurbine)]
            min_supply = sourcing_capacities[sourcing_capacities['priority'] == 1]['capacity'].sum()
            sourcing_capacities = get_available_heat_capacities(non_gts, min_supply, gas_props, market_prices, timestep)

    ###   evaluate optimal generation and minimal cost for interval   ###

    # initialise generation cost, gas consumption (wrt Hu), and electricity generation 'accumulation' variable
    cost_min = 0
    gas_cons = 0
    el_gen = 0

    # initialize dict to track optimised operational settings (amount of heat per source)
    active = dict()
    active['Q_demand'] = q_demand
    active['Active_generator_objects'] = []

    # add OPEX for required heat amounts by looping through sorted DataFrame of available heat sourcing_capacities
    for index, row in sourcing_capacities.iterrows():
        # if demand not satisfied
        if q_demand > 0:
            # update pending demand
            q_demand -= row['capacity']
            # if full capacity of current heat source is needed
            if q_demand >= 0:
                cost_min += row['capacity'] * row['unit_price']
                gas_cons += row['capacity'] * row['gas_cons']
                el_gen += row['capacity'] * row['el_gen']
                # if heat source already in active heat sources, only increment needed capacity
                if row['source']+'_q' in active.keys():
                    active[row['source']+'_q'] += row['capacity']
                    if isinstance(row['generator_object'], (HeatBoiler, GasTurbine)):
                        active[row['source']+'_gas'] += row['capacity'] * row['gas_cons']
                else:
                    active[row['source']+'_q'] = row['capacity']
                    if isinstance(row['generator_object'], (HeatBoiler, GasTurbine)):
                        active[row['source']+'_gas'] = row['capacity'] * row['gas_cons']
                    active['Active_generator_objects'].append(row['generator_object'])
            # if current heat source is only needed partially
            else:
                # conventional heat boilers are fully flexible (no minimum load)
                if isinstance(row['generator_object'], HeatBoiler):
                    # re-evaluate OPEX for heat generation below maximum capacity
                    if row['source']+'_q' in active.keys():
                        # if boiler already in active heat sources, only increment needed capacity and only
                        # consider marginal variable cost driven by additional heat quantity
                        used = active[row['source']+'_q']    # already used quantity on heat boiler
                        opex, gas_demand, _ = opex_per_MWh(row['generator_object'], gas_props, market_prices, timestep,
                                                           quant_old=used, quant_new=(row['capacity'] + q_demand))
                        cost_min += opex * (row['capacity'] + q_demand)
                        gas_cons += gas_demand * (row['capacity'] + q_demand)
                        active[row['source']+'_q'] += row['capacity'] + q_demand
                        active[row['source']+'_gas'] += gas_demand * (row['capacity'] + q_demand)
                    else:
                        # if boiler not yet in active heat sources, consider full hourly cost, despite only
                        # partial use of maximum generation capacity
                        opex1 = opex_per_h(row['generator_object'], timestep)
                        opex2, gas_demand, _ = opex_per_MWh(row['generator_object'], gas_props, market_prices, timestep,
                                                         quant_new=(row['capacity'] + q_demand))
                        cost_min += opex1 + (opex2 * (row['capacity'] + q_demand))
                        gas_cons += gas_demand * (row['capacity'] + q_demand)
                        active[row['source']+'_q'] = row['capacity'] + q_demand
                        active[row['source']+'_gas'] = gas_demand * (row['capacity'] + q_demand)
                        active['Active_generator_objects'].append(row['generator_object'])
                    # store used and idle capacity on 'swing' boiler (most expensive current heat source)
                    active['Used_capacity'] = active[row['source']+'_q']
                    active['Idle_capacity'] = row['generator_object'].capacity - active[row['source']+'_q']

                # gas turbine can only be operated above certain minimal load
                elif isinstance(row['generator_object'], GasTurbine):
                    # only consider GT if required capacity on GT >= minimum heat load (MW)
                    if (row['capacity'] + q_demand) >= row['generator_object'].min_load:
                        # re-evaluate OPEX for heat generation below maximum capacity (full hourly cost,
                        # despite only partial use of maximum generation potential)
                        opex1 = opex_per_h(row['generator_object'], timestep)
                        opex2, gas_demand, cogen = opex_per_MWh(row['generator_object'], gas_props, market_prices,
                                                                timestep, quant_new=(row['capacity'] + q_demand))
                        cost_min += opex1 + (opex2 * (row['capacity'] + q_demand))
                        gas_cons += gas_demand * (row['capacity'] + q_demand)
                        el_gen += cogen * (row['capacity'] + q_demand)
                        active[row['source']+'_q'] = row['capacity'] + q_demand
                        active[row['source']+'_gas'] = gas_demand * (row['capacity'] + q_demand)
                        active['Active_generator_objects'].append(row['generator_object'])
                        # store used and idle capacity on 'swing' GT (most expensive current heat source)
                        active['Used_capacity'] = active[row['source']+'_q']
                        active['Idle_capacity'] = row['generator_object'].power_q - active[row['source']+'_q']
                    else:
                        # 'reset' remaining heat demand if below minimum heat load of gas turbine
                        q_demand += row['capacity']

                # heat sourcing from waste incineration plant only feasible above min technical limit (min. pump speed)
                elif isinstance(row['generator_object'], SourcingContract):
                    # only consider MHKW if required capacity from MHKW >= minimal technical limit (MW)
                    if (row['capacity'] + q_demand) >= row['generator_object'].qmin[timestep]:
                        active[row['source']+'_q'] = row['capacity'] + q_demand
                        cost_min += row['unit_price'] * (row['capacity'] + q_demand)
                        active['Active_generator_objects'].append(row['generator_object'])
                        # store used and idle capacity on 'swing' sourcing contract (most expensive current heat source)
                        active['Used_capacity'] = (row['capacity'] + q_demand)
                        active['Idle_capacity'] = -q_demand
                    else:
                        # 'reset' remaining heat demand if below minimum technical limit
                        q_demand += row['capacity']

    # set cumulative gas consumption and electricity generation for this time step
    active['Gas_consumption'] = gas_cons
    active['Electricity_generation'] = el_gen

    # set cost to np.inf if sum of all available sources still not enough to satisfy heat demand
    if q_demand > 0:
        active['Min_cost'] = np.inf
    else:
        active['Min_cost'] = cost_min

    return active


def switching_cost(old_set_up, old_timestep, new_set_up, new_timestep):
    """
    Returns switching cost to move from 'old_set_up' to 'new_set_up' of heat generators

    :param list old_set_up: list of current heat generator/sourcing objects (HeatBoiler, GasTurbine, SourcingContract)
                            before potentially switching
    :param int old_timestep: time step in optimization period associated with old_set_up
    :param list new_set_up: list of heat generator/sourcing objects (HeatBoiler, GasTurbine, SourcingContract)
                            after potentially switching
    :param int new_timestep: time step in optimization period associated with new_set_up
    
    :returns float: switching cost
    """

    # initialize switching cost
    cost = 0.0

    # remove all Sourcing contracts from set-ups (do not incur start-up/shut-down cost)
    old = list(filter(lambda x: isinstance(x, SourcingContract) == False, old_set_up))
    new = list(filter(lambda x: isinstance(x, SourcingContract) == False, new_set_up))

    # derive heat generators to be shut-down
    for gen in old:
        if gen.name not in [n.name for n in new]:
            # add shut-down cost to overall switching cost
            if isinstance(gen, HeatBoiler):
                # time-independent heat boiler shut-down cost
                cost += gen.cost_shut
            elif isinstance(gen, GasTurbine):
                # time-dependent gas turbine shut-down cost (depending on MarketPrices)
                cost += gen.cost_shut[old_timestep]

    # derive heat generators to be started up
    for gen in new:
        if gen.name not in [o.name for o in old]:
            # add start up cost to overall switching cost
            if isinstance(gen, HeatBoiler):
                # time-independent heat boiler start-up cost
                cost += gen.cost_start
            elif isinstance(gen, GasTurbine):
                # time-dependent gas turbine start-up cost (depending on MarketPrices)
                cost += gen.cost_start[new_timestep]

    return cost


def minimum_supply(grid_operator, timestep):
    """
    Returns minimum amount of heat to be supplied to the district heating grid as a
    result of the temperature spread between flow and return temperature and minimum
    required flow rate (due to minimum pump speed)

    Arguments:
        grid_operator {MunicipalUtility} -- district heating grid operator object (i.e., 
                                            municipal utility) with associted grid entry
                                            points and respective properties
        timestep {str} -- time step in optimisation period for which to evaluate
                          minimal heat supply
    
    Returns:
        minimum required heat supply {float}
    """

    # Initialize fall-back parameters if provided grid parameters are erroneous, 
    # i.e. negative temperature spread
    t_flow_fallback = 94.6           # °C, median of historical HKW time series
    t_return_fallback = 74.1         # °C, median of historical HKW time series


    # 1) Derive average water properties (for p and T ranges)
    # convert grid p and T to SI units
    p = 1e5 * grid_operator.network.entry_points[grid_operator.name]['pressure'][timestep]              # bar to Pa
    t_flow = 273.15 + grid_operator.network.entry_points[grid_operator.name]['temp_flow'][timestep]     # °C to K
    t_return = 273.15 + grid_operator.network.entry_points[grid_operator.name]['temp_return'][timestep] # °C to K
    # use fall-back temperature values in case flow temperature < return temperature
    if t_flow < t_return:
        t_flow = t_flow_fallback + 273.15
        t_return = t_return_fallback + 273.15
    # average heat capacity cp
    cp1 = PropsSI('C', 'P', p, 'T', t_flow, 'Water')/1000       # kJ/kg/K
    cp2 = PropsSI('C', 'P', p, 'T', t_return, 'Water')/1000
    cp = (cp1 + cp2) / 2
    # average density rho
    rho1 = PropsSI('D', 'T', t_flow, 'P', p, 'Water')           # kg/m³
    rho2 = PropsSI('D', 'T', t_return, 'P', p, 'Water')
    rho = (rho1 + rho2) / 2

    # 2) Evaluate minimum heat requirement
    # temperature difference between inflow and return flow of heating grid, K
    delta_temp = t_flow - t_return
    # minimum required heat supply to district heating grid (associated with minimum pump speed), MWh/h
    min_stability_demand = (grid_operator.network.entry_points[grid_operator.name]['min_circulation'] / 3600) * rho * cp * delta_temp / 1000
    min_stability_demand = round(min_stability_demand, 1)

    return min_stability_demand


def extract_output_timeseries(optimised:pd.DataFrame, rdftype:str,
                              provider:(HeatBoiler, GasTurbine, SourcingContract)):
    """
    Extracts optimisation output time series data for given heat provider (object,
    and rdftype) from overall optimisation outputs DataFrame

    Arguments:
        optimised {pd.DataFrame} -- DataFrame with optimised generation time series
                                    as returned by 'generation_optimization'
        rdftype {str} -- rdf type of heat generator/provider 
        provider {HeatBoiler, GasTurbine, SourcingContract} -- corresponding heat
                                                        generator/provider object
    
    Returns:
        dictionary with optimisation output rdf types as keys and time series 
        data as lists as values
    """

    # Extract optimised output time series data based on rdf type
    if rdftype == OHN_INCINERATIONPLANT:
        provided_heat = list(optimised[provider.name+'_q'].values)
        generated_heat, consumed_gas, cogen_electricity = None, None, None
    elif rdftype == OHN_HEATBOILER:
        generated_heat = list(optimised[provider.name+'_q'].values)
        consumed_gas = list(optimised[provider.name+'_gas'].values)
        provided_heat, cogen_electricity = None, None
    elif rdftype == OHN_GASTURBINE:
        generated_heat = list(optimised[provider.name+'_q'].values)
        consumed_gas = list(optimised[provider.name+'_gas'].values)
        cogen_electricity = list(optimised['Electricity_generation'].values)
        provided_heat = None

    # Cast availability to default boolean datatype to avoid issues
    # with TSClient and e.g. numpy booleans (as per .astype(bool))
    ts_data = {
        OHN_PROVIDED_HEAT_AMOUNT: provided_heat,
        OHN_GENERATED_HEAT_AMOUNT: generated_heat,
        OHN_CONSUMED_GAS_AMOUNT: consumed_gas,
        OHN_COGEN_ELECTRICITY_AMOUNT: cogen_electricity,
        OHN_AVAILABILITY: list(map(bool, provider.available))
    }
    
    return ts_data
