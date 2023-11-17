################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 25 Sep 2023                            #
################################################

# The purpose of this module is to provide the main optimisation logic and
# methods for the DHOptimisationAgent

from CoolProp.CoolProp import PropsSI


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


