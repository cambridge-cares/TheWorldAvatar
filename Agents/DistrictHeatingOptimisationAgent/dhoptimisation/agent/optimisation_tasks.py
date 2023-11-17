################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 25 Sep 2023                            #
################################################

# The purpose of this module is to provide the main optimisation logic and
# methods for the DHOptimisationAgent

from CoolProp.CoolProp import PropsSI

from dhoptimisation.agent.config import TIME_FORMAT


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


def generation_optimization(municipal_utility, market_prices, datetime_index, evaluation_period, mpc_horizon,
                            out_file_gt, out_file_opt, previous_state, histeval=False, live_updates=False):
    """
    Run heat generation/sourcing cost optimization as model-predictive control implementation

    :param MunicipalUtility municipal_utility: municipal utility object (with heat demand, connected grid, etc.)
    :param MarketPrices market_prices: market prices object with electricity, co2, gas, etc. prices as time series
    :param DateTimeIndex datetime_index: original datetime indices to integer-based indices of objects
    :param int evaluation_period: total number of time steps to be evaluated/forecasted
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

    # create profit lines for both heat generation modes
    ops1 = minimize_generation_cost(municipal_utility, sources_mode1, market_prices, mpc_horizon, preceding_setup=ops1_last_setup)
    ops2 = minimize_generation_cost(municipal_utility, sources_mode2, market_prices, mpc_horizon, preceding_setup=ops2_last_setup)
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

