################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 25 Sep 2023                            #
################################################

# The purpose of this module is to provide the main optimisation logic and
# methods for the DHOptimisationAgent

import numpy as np
import pandas as pd
from CoolProp.CoolProp import PropsSI

from dhoptimisation.agent.config import TIME_FORMAT
from dhoptimisation.agent.optimisation_setup import HeatBoiler, GasTurbine, SourcingContract


def generation_optimization(municipal_utility, market_prices, datetime_index, opti_horizon,
                            out_file_gt, out_file_opt, previous_state, histeval=False, live_updates=False):
    """
    Run heat generation/sourcing cost optimization as model-predictive control implementation

    :param MunicipalUtility municipal_utility: municipal utility object (with heat demand, connected grid, etc.)
    :param MarketPrices market_prices: market prices object with electricity, co2, gas, etc. prices as time series
    :param DateTimeIndex datetime_index: original datetime indices to integer-based indices of objects
    :param int mpc_horizon: time steps to be optimized in each optimization run (mpc horizon)
    :param str out_file_gt: path to .txt file for continuous gas turbine planning output (update per mpc loop)
    :param str out_file_opt: path to .txt file for continuous heat generation mix output (update per mpc loop)
    :param boolean histeval: flag whether evaluation is based on historical values or time series need to be forecasted
    :param boolean live_updates: flag whether to show continuous updates to log files while running
    :returns pd.DataFrames: holistically optimised heat generation across modes w/ and w/o GT ('mpc_results'),
                            as well as internally optimised modes w/ GT ('wgt') and w/o GT ('wogt')
    :returns pd.DataFrame: ('mpc_horizon' ahead) forecasts of heat load and grid temperatures for 'evaluation_period'
    """
    
    # Whether GT has been active in previous time step
    gt_active = previous_state.gt_active
    gt_benefit = previous_state.gt_benefit
    ops1_last_setup = previous_state.ops1_last_setup
    ops2_last_setup = previous_state.ops2_last_setup  
    # Whether GT start up cost are already included in optimized mode w/ GT
    startup_cost_included = False  # default: add (additional) start-up cost

    # Incorporate GT idle time requirement (i.e., potentially mark GT as unavailable)
    gts = [gt.name for gt in municipal_utility.gas_turbines]
    for gt in municipal_utility.gas_turbines:
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
    ops1 = minimize_generation_cost(municipal_utility, sources_mode1, market_prices, 
                                    opti_horizon, preceding_setup=ops1_last_setup)
    ops2 = minimize_generation_cost(municipal_utility, sources_mode2, market_prices, 
                                    opti_horizon, preceding_setup=ops2_last_setup)
    ops1_last_setup = ops1['Active_generator_objects'][0]
    ops2_last_setup = ops2['Active_generator_objects'][0]

    # check whether GT start-up cost need to be added in case of switching
    if (t > 0) and (ops2['SWPS_GT'][0] > 0.0) and (previous == 0.0):
        # start-up cost could potentially be already included in optimized mode w/GT in case of "inner-mode"
        # switching due to lower q_demand than GT min load
        startup_cost_included = True
    previous = ops2['SWPS_GT'][0]

    # optimize operating modes
    opt = optimize_operating_modes(ops1, ops2, gt_active=gt_active, startup_cost_included=startup_cost_included,
                                    prev_gt_benefit=gt_benefit, mpc_eval=True)

    # extract very first optimisation result for current time step and attach to overall mpc results
    if t == 0:
        # initialise mpc results DataFrame
        columns = opt.iloc[[0], :].columns
        index = pd.RangeIndex(start=0, stop=evaluation_period, step=1)
        mpc_results = pd.DataFrame(index=index, columns=columns)
        # include results for "internally" optimised modes w/o and w/ GT
        wogt = mpc_results.copy()
        wgt = wogt.copy()
    # attach results for very first optimisation time step as mpc results for current time step
    mpc_results.iloc[t, :] = opt.iloc[0, :]
    # exclude last column with active heat generator objects in ops1 and ops2 DataFrames
    wogt.iloc[t, :] = ops1.iloc[0, :-1]
    wgt.iloc[t, :] = ops2.iloc[0, :-1]

    

    #####---------------    UPDATE OPTIMISATION OBJECTS   ---------------#####

    # implement current optimisation results in overall municipal utility object and update q_hist of boilers
    for boiler in municipal_utility.boilers:
        boiler.q_hist[t] = mpc_results[boiler.name][t]
    # update q_hist of gas turbine
    for gt in municipal_utility.gas_turbines:
        gt.q_hist[t] = mpc_results[gt.name][t]
        if mpc_results[gt.name][t] > 0:
            gt_active = True
            if (t > 0) and (mpc_results[gt.name][t-1] == 0):
                # in case GT has just been activated, initialise max and cumulative benefit from current operation
                gt_benefit = [0.0, 0.0]
                gt_benefit[1] += wogt['Min_cost'][t] - mpc_results['Min_cost'][t] + switching_cost([], 0,
                            [gt for gt in ops2.iloc[0]['Active_generator_objects'] if isinstance(gt, GasTurbine)], 0)
            else:
                # update cumulative GT benefit from current operation
                gt_benefit[1] += wogt['Min_cost'][t] - mpc_results['Min_cost'][t]
            # update max benefit from current operation
            gt_benefit[0] = max(gt_benefit[0], gt_benefit[1])
        else:
            if gt_active:
                # 'correct' GT activity for current time step in 'wgt' if GT has just been switched off
                wgt.iloc[t, :] = ops1.iloc[0, :-1]
            gt_active = False
            gt_benefit = None
    for contract in municipal_utility.contracts:
        # update q_hist of sourcing contract and update unit price (to include potential volume discount)
        contract.q_hist[t] = mpc_results[contract.name][t]
        contract.update_price()


    # round results to 2 decimal places
    mpc_results = mpc_results.astype(float).round(2)
    wogt = wogt.astype(float).round(2)
    wgt = wgt.astype(float).round(2)
    forecasts_out = forecasts_out.astype(float).round(2)
    
    # TODO: to be implemented
    opti_start_dt = datetime_index[0].strftime(TIME_FORMAT)
    #previous_state.update_system_state(opti_start_dt, )

    # return mpc_results, wogt, wgt, forecasts_out
    
    
def minimize_generation_cost(municipal_utility, sourcing_mode, market_prices, optimization_period,
                             preceding_setup=[]):
    """
    Returns DataFrame with operating conditions (generation capacity per heat source) for 'optimization_period'
    timesteps, as well as associated cost

    :param MunicipalUtility municipal_utility: municipal utility object (with heat demand, connected grid, etc.)
    :param list sourcing_mode: list of heat generator/sourcing objects (HeatBoiler, GasTurbine, SourcingContract)
    :param MarketPrices market_prices: market price object
    :param int optimization_period: number of time steps in optimization period
    :param boolean hist_eval: specifies whether optimisation is run in 'forecasting' or 'historical_evaluation' mode
    :returns pd.DataFrame: DataFrame with operating conditions (heat generation per source) for each time step in
                          'optimization_period', along with associated minimal cost and active heat generator objects
    """

    # create empty DataFrame with required columns
    columns = ['Q_demand', 'Min_cost', 'SWPS_GT', 'MHKW_ToP', 'Kessel4', 'Kessel5', 'Kessel6', 'Gas_consumption',
               'Electricity_generation', 'Used_capacity', 'Idle_capacity', 'Active_generator_objects']
    ops_overview = pd.DataFrame(columns=columns)

    # initialise generator setup from preceding time step
    old = preceding_setup

    # loop over all time steps in optimization_period
    for t in range(optimization_period):

        # Retrieve minimum amount of heat to be supplied by SWPS to ensure grid stability
        # (due to min circulation volume by SWPS), MWh
        min_supply = minimum_supply(municipal_utility, t)
        # ensure that heat demand is ALWAYS equal / greater than required minimum supply
        demand = max(min_supply, municipal_utility.q_demand[t])

        # get sorted DataFrame of available heat capacities for respective heat generation mode and timestep
        capas = get_available_heat_capacities(sourcing_mode, min_supply, municipal_utility.fuel, market_prices, t)

        # calculate minimum cost required to satisfy heat demand
        ops_data = get_min_cost_for_interval(demand, capas, municipal_utility.fuel, market_prices, t)

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
        ops_overview = ops_overview.append(ops_data, ignore_index=True)

    # fill all empty cells with 0's
    ops_overview.fillna(0, inplace=True)
    # replace generic integer index with first 'optimization_period' elements of Q_demand index
    ops_overview.index = municipal_utility.q_demand.index[:optimization_period]

    return ops_overview


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
                        capacities = capacities.append(
                            {'source': source.name, 'capacity': (source.capacity-used), 'priority': 3,
                             'unit_price': opex, 'gas_cons': gas_demand, 'el_gen': el_gen,
                             'generator_object': source}, ignore_index=True)
                    else:
                        # if boiler not yet in 'capacities', calculate heat unit price for maximum hourly generation
                        opex1 = opex_per_h(source, timestep) / source.capacity
                        opex2, gas_demand, el_gen = opex_per_MWh(source, gas_props, market_prices, timestep,
                                                                 quant_new=source.capacity)
                        # add capacity with priority 3
                        capacities = capacities.append({'source': source.name, 'capacity': source.capacity,
                                                        'priority': 3, 'unit_price': opex1 + opex2, 'gas_cons': gas_demand,
                                                        'el_gen': el_gen, 'generator_object': source}, ignore_index=True)

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
                        capacities = capacities.append(
                            {'source': source.name, 'capacity': (source.power_q-used), 'priority': 2,
                             'unit_price': opex, 'gas_cons': gas_demand, 'el_gen': el_gen,
                             'generator_object': source}, ignore_index=True)
                    else:
                        # if GT not yet in 'capacities', calculate heat unit price for maximum hourly generation
                        opex1 = opex_per_h(source, timestep) / source.power_q
                        opex2, gas_demand, el_gen = opex_per_MWh(source, gas_props, market_prices, timestep,
                                                                 quant_new=source.power_q)
                        # add capacity with priority 2
                        capacities = capacities.append({'source': source.name, 'capacity': source.power_q,
                                                        'priority': 2, 'unit_price': opex1 + opex2, 'gas_cons': gas_demand,
                                                        'el_gen': el_gen, 'generator_object': source}, ignore_index=True)

            # extract heat supply capacity and heat unit price for external sourcing contracts
            elif isinstance(source, SourcingContract):
                # only consider available capacity
                if source.available[timestep] == 1:
                    # add full technical capacity of heat sourcing contract with priority 3
                    capacities = capacities.append({'source': source.name, 'capacity': source.qmax[timestep],
                                                    'priority': 3, 'unit_price': source.current_price, 'gas_cons': 0.0,
                                                    'el_gen': 0.0, 'generator_object': source}, ignore_index=True)

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
                    capa_sorted = capa_sorted.append(min_supplier)
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
                    capa_sorted = capa_sorted.append(min_supplier)

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
    Returns load-dependent (average) OPEX per generated MWh of heat (€/MWh_q) as well as specific (average) gas
    consumption (MWh_g(wrt Hu)/MWh_q)
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
