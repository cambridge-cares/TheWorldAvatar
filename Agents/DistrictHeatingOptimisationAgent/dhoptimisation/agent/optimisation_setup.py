################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 15 Nov 2023                            #
################################################

# The purpose of this module is to provide functionality to create the required
# optimisation setup and model objects based on data queried from the KG

import numpy as np
import pandas as pd
from datetime import datetime, timedelta

from dhoptimisation.utils import *
from dhoptimisation.datamodel.iris import *
from dhoptimisation.datamodel.unit_mapping import UNITS
from dhoptimisation.agent.config import HIST_LENGTH, TIME_FORMAT
from dhoptimisation.kgutils.kgclient import KGClient
from dhoptimisation.kgutils.tsclient import TSClient


####################     CLASSES     ####################


class MarketPrices:
    # Class comprising all market prices
    def __init__(self, gas_q, gas_gt, el_spot, co2, chp_bonus, grid_save):
        # Time series attributes (covering optimisation period)
        self.gas_q = gas_q          # gas price for boilers (contracts), €/MWh_gas (wrt higher calorific value)
        self.gas_gt = gas_gt        # gas price for gas turbine (spot), €/MWh_gas (wrt higher calorific value)
        self.el_spot = el_spot      # electricity spot price, €/MWh_el
        self.co2 = co2              # cost of co2 emission certificates (EUAs), €/t_co2
        self.chp_bonus = chp_bonus  # chp subsidies per MWh of co-generated electricity, €/MWh_el
        self.grid_save = grid_save  # saved grid charges due to own electricity generation, €/MWh_el


    def __repr__(self):
        # Override default naming method by assigning class name 'MarketPrices' as printable representation of object
        return self.__class__.__name__


    def get_el_remun(self, timestep):
        # Total remuneration for electricity at particular time step, €/MWh_el
        return self.el_spot[timestep] + self.chp_bonus[timestep] + self.grid_save[timestep]


class GasProperties:
    # Class defining gas properties used by boilers and gas turbine
    def __init__(self, ho, hu, co2_factor):
        # Constant (time-independent) attributes
        self.ho = ho                    # higher calorific value, kWh/m³
        self.hu = hu                    # lower calorific value, kWh/m³
        self.co2_factor = co2_factor    # specific CO2 generation per MWh gas burned, t_CO2/MWh_g (wrt Hu)


    def __repr__(self):
        # Override default naming method by assigning class name 'GasProperties' as printable representation of object
        return self.__class__.__name__


class SourcingContract:
    # Class defining external heat sourcing by time-dependent take-or-pay contract
    def __init__(self, iri, name, qlimits_pa, prices, current_price, entry_point, 
                 availability, qmin, qmax, q_hist=[]):
        # Constant (time-independent) attributes
        self.iri = iri                      # IRI of fulfilling heating plant
        self.name = name                    # name/ID of heat sourcing contract
        self.qlimits_pa = qlimits_pa        # contractually agreed sourcing/supply limits [min_pa, max_pa], MWh/a
        # min_pa: contractually agreed minimum quantity to be sourced from MHKW p.a. (take-or-pay)
        # max_pa: contractually agreed maximum quantity to be supplied by MHKW p.a.
        self.prices = prices                # heat unit prices (incl. potential volume discount) [p_reg, p_red], €/MWh
        # p_reg: regular base price per heat unit for quantities up to 'min_pa' (contractually fixed for each year)
        # p_red: reduced base price for volumes exceeding 'min_pa' quantity
        self.current_price = current_price  # current heat unit price, €/MWh
        self.grid_entry_point = entry_point # associated district heating grid entry point (needs to match a key
                                            # in DistrictHeatingGrid entry_points dict)

        # Time series attributes (covering optimisation period)
        self.available = availability       # availability of contracted heat: [1, 0] ... yes,no
        self.qmin = qmin                    # technical hourly supply minimum (defined by minimum pump speed), MWh/h
        self.qmax = qmax                    # technical hourly supply maximum (constrained by grid hydraulics), MWh/h
        self.q_hist = q_hist                # annual heat sourcing history, MWh/h


    def __repr__(self):
        # Override generic default naming method by assigning 'name' as printable representation of object
        return '{}_{}'.format(self.__class__.__name__, self.name)


    def update_internal_price(self):
        # Updates current heat unit price (depending on total annual heat sourcing volume)
        # NOTE: Current heat unit price queried from KG, but updated here, as instantiated
        #       price does not necessarily reflect state as of previous time step
        #       (as unrelated periods can be optimised in subsequent request)
        total = self.q_hist.sum(skipna=True)
        if total <= self.qlimits_pa[0]:
            self.current_price = self.prices[0]
        elif total <= self.qlimits_pa[1]:
            self.current_price = self.prices[1]
        else:
            # Suppress heat sourcing beyond annual maximum supply limit
            self.current_price = np.inf


class HeatBoiler:
    # Class defining a conventional heat boiler
    def __init__(self, iri, name, capacity, gas_demand, start_up_cost, shut_down_cost, 
                 wear_cost, availability, labour_cost, q_hist=[]):
        # Constant (time-independent) attributes
        self.iri = iri                      # IRI of boiler instance
        self.name = name                    # name/ID of heat boiler
        self.capacity = capacity            # maximum heat generation capacity, MW
        self.gas_demand = gas_demand        # gas demand model, calculating gas consumption based on generated heat
                                            # (wrt lower calorific value)
        self.cost_start = start_up_cost     # start-up cost, €/start-up
        self.cost_shut = shut_down_cost     # shut-down cost, €/shut-down
        # Time series attributes (covering optimisation period)
        self.cost_wear = wear_cost          # wear cost per operating hour, €/MWh_q
        self.available = availability       # availability of boiler: [1, 0] ... yes,no
        self.cost_labour = labour_cost      # personnel cost per operating hour (shift surcharges, etc.), €/h
        self.q_hist = q_hist                # recent heat generation history, MWh/h


    def __repr__(self):
        # Override generic default naming method by assigning 'name' as printable representation of object
        return '{}_{}'.format(self.__class__.__name__, self.name)


class GasTurbine:
    # Class defining a CHP gas turbine
    def __init__(self, iri, name, power_el, power_q, min_load, gas_demand, el_output, 
                 idle_period, wear_cost, availability, labour_cost, start_up_cost=None, 
                 shut_down_cost=None, gas=None, prices=None, q_hist=[]):
        # Constant (time-independent) attributes
        self.iri = iri                      # IRI of gas turbine instance
        self.name = name                    # name/ID of gas turbine
        self.power_el = power_el            # maximum electricity generation capacity, MW
        self.power_q = power_q              # maximum heat generation capacity, MW
        self.min_load = min_load            # minimum required heat load (corresponds to 70% electrical load), MW
        self.gas_demand = gas_demand        # gas demand model, calculating gas consumption based on generated heat
                                            # (wrt lower calorific value)
        self.el_output = el_output          # electricity output model, calculating el. power based on generated heat
        self.idle_period = idle_period      # minimum duration between 2 subsequent GT operations, h
        # Time series attributes (covering optimisation period)
        self.cost_wear = wear_cost          # wear cost per operating hour, €/h
        self.available = availability       # availability of GT: [1, 0] ... yes,no
        self.cost_labour = labour_cost      # personnel cost per operating hour (shift surcharges, etc.), €/h
        self.q_hist = q_hist                # recent heat generation history, MWh/h        
        self.cost_start = start_up_cost     # start-up cost, €/start-up
        self.cost_shut = shut_down_cost     # shut-down cost, €/shut-down
        # Initialise start-up and shut-down cost as functions of labour, fuel, 
        # and wear cost in case not provided as ts (e.g., when creating copies)
        if not self.cost_start:
            self.set_startup_cost(gas, prices)
        if not self.cost_shut:
            self.set_shutdown_cost()        

        # Total power of gas turbine, MW
        self.power = self.power_q + self.power_el


    def __repr__(self):
        # Override generic default naming method by assigning 'name' as printable representation of object
        return '{}_{}'.format(self.__class__.__name__, self.name)


    def set_shutdown_cost(self):
        # Assessment logic: gas turbine's heat recovery boiler needs to be supervised 
        # during shut down until temperature decreases below uncritical level, which 
        # requires approx. 2h of personnel
        self.cost_shut = 2 * self.cost_labour.copy()


    def set_startup_cost(self, gas=None, prices=None):
        # Assessment logic: every start-up counts as 15 operation hours wrt to wear
        # (according to OEM and inspection schedule) + consumes approx. 6 MWh of gas
        # (wrt ho) with unusable heat production
        if isinstance(gas, GasProperties) & isinstance(prices, MarketPrices):
            if self.cost_labour.index.equals(prices.co2.index) & self.cost_labour.index.equals(prices.gas_gt.index):
                # Use labour cost pd.Series as template (to keep index, etc.)
                cost = self.cost_labour.copy()
                # Calculate start-up cost
                cost[:] = 15 * self.cost_wear                                   # wear cost of 15h
                cost += 6 * prices.gas_gt                                       # gas cost for 6 MWh_g (wrt Ho)
                cost += 6 * gas.hu / gas.ho * gas.co2_factor * prices.co2       # CO2 cost for 6 MWh_g (wrt Ho)
                cost = cost.round(1)
                # Assign as object attribute
                self.cost_start = cost
            else:
                raise_error(IndexError, "Index of provided 'gas' and 'prices' objects does not " + 
                            "match gas turbine index")
        elif (gas is None) | (prices is None):
            # Assign constant start-up cost of approx. 800 €/start-up
            self.cost_start = self.cost_labour.replace(self.cost_labour.values, 800)
        else:
            raise_error(TypeError, "Provided arguments 'gas' and 'prices' need to be GasProperties " +
                        "and MarketPrices objects, respectively")


    def get_idle_time(self):
        # Returns GT's idle time (count of inactive time steps since last operation)
        # Create copy of recent generation history and replace potential NaN with 0
        q_hist = self.q_hist.copy()
        q_hist.fillna(0, inplace=True)
        # Reverse order to count backwards from optimisation start datetime
        reversed_q_hist = self.q_hist[::-1]
        reversed_q_hist = reversed_q_hist.cumsum()
        # Keep only consecutively inactive time steps
        inactive = reversed_q_hist[reversed_q_hist == 0]
        
        return len(inactive)


class MunicipalUtility:
    # Class defining a municipal utility company providing district heating
    def __init__(self, name, conv_boilers, gas_turbines, contracts, fuel, network, q_demand):
        # Constant (time-independent) attributes / set-up
        self.name = name                        # name/ID of municipal utility
        self.boilers = conv_boilers             # list of conventional boiler objects operated by the utility
        self.gas_turbines = gas_turbines        # list of gas turbine objects operated by the utility
        self.contracts = contracts              # list of (external) heat sourcing contracts available to the utility
        self.fuel = fuel                        # gas property object describing the fuel available to the utility
        self.network = network                  # district heating grid object to which the utility is connected
        # Time series attributes (covering optimisation period)
        self.q_demand = q_demand                # total district heating demand (supply to the grid), MWh

    def __repr__(self):
        # Override generic default naming method by assigning 'name' as printable representation of object
        return '{}_{}'.format(self.__class__.__name__, self.name)


    def add_boiler(self, conv_boiler):
        self.boilers.append(conv_boiler)

    def add_gas_turbine(self, gas_turbine):
        self.gas_turbines.append(gas_turbine)

    def add_contract(self, contract):
        self.contracts.append(contract)

    def set_fuel(self, fuel):
        self.fuel = fuel

    def connect_network(self, network):
        self.network = network


class DistrictHeatingGrid:
    # Class defining the district heating network
    def __init__(self, name, entry_points):
        # Constant (time-independent) attributes
        self.name = name                        # name/ID of district heating grid
        self.entry_points = entry_points        # heat entry points to district heating grid for heat provision


    def __repr__(self):
        # Override generic default naming method by assigning 'name' as printable representation of object
        return '{}_{}'.format(self.__class__.__name__, self.name)


    def add_entry_point(self, name, min_circulation, pressure, temp_flow, temp_return):
        # Add heat entry/supply point to district heating grid
        if isinstance(name, str):
            characteristics = {
                'min_circulation': min_circulation,     # minimum water circulation volume (due to pump specs), m³/h
                                                        # NOTE: None if not applicable
                'pressure': pressure,                   # operating pressure, bara
                'temp_flow': temp_flow,                 # water supply temperature (Vorlauftemperatur), °C
                'temp_return': temp_return              # water return temperature (Ruecklauftemperatur), °C
            }
            # Add heat entry/supply point to DistrictHeatingGrid dictionary with 'name' as key
            self.entry_points[name] = characteristics
        else:
            raise_error(ValueError, "Entry point's 'name' needs to be a string")
    

class PreviousSystemState:
    # The purpose of this class is to provide a link between subsequent optimisation runs:
    # The current design sends each optimisation interval as a separate request to the
    # agent (i.e., via DIF); hence, this class ensures that relevant outputs from the
    # previous time step are available for the current optimisation run
    def __init__(self, start_dt=None, gt_active=False, gt_benefit=None, setup_woGT=[], setup_wGT=[]):
        self.start_dt = start_dt            # datetime string of first timestep of previous
                                            # optimisation interval
        self.gt_active = gt_active          # Boolean flag whether to start with/without GT
        self.gt_benefit = gt_benefit        # Cumulative profit from previous GT operation
        self.setup_woGT = setup_woGT        # Heat generation setups in preceding time step
        self.setup_wGT = setup_wGT


    def update_system_state(self, new_start_dt, new_gt_active, new_gt_benefit, 
                            new_setup_woGT, new_setup_wGT):
        # Update operational state and gas turbine conditions
        self.start_dt = new_start_dt
        self.gt_active = new_gt_active
        self.gt_benefit = new_gt_benefit
        self.setup_woGT = new_setup_woGT
        self.setup_wGT = new_setup_wGT
            
    def reset_system_state(self):
        # Reset to default optimisation start conditions
        self.start_dt = None
        self.gt_active = False
        self.gt_benefit = None
        self.setup_woGT = []
        self.setup_wGT = []
    
    
####################     METHODS     ####################


def define_optimisation_setup(kg_client: KGClient, ts_client: TSClient,
                              consumption_models: dict, cogen_models: dict,
                              optimisation_input_iris: dict, 
                              opti_start_dt: str, opti_end_dt: str, 
                              time_format=TIME_FORMAT):
    """
    Returns a dictionary describing the full SWPS optimization setup by querying 
    relevant input data from KG, with first level keys referring to object instances, 
    i.e., ['market_prices', 'gas_properties', 'sourcing_contracts', 'heat_boilers', 
    'gas_turbines', 'district_heating_grid', 'municipal_utility'] and values describing
    all parameters to create the respective objects
    
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
        time_format {str} -- Python compliant time format to parse time values
            
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
            'sc0':        # IRI of fulfilling heating plant (i.e., EfW plant)
            'sc1':        # name
            'sc2':        # annual sourcing limits [min, max], MWh/a
            'sc3':        # base unit price [p_reg, p_red], €/MWh
            'sc4':        # current heat unit price, €/MWh
            'sc5':        # associated heating grid entry point
            'sc6':        # availability
            'sc7':        # technical min. supply, MWh/h
            'sc8':        # technical max. supply, MWh/h
            'sc9':        # heat sourcing history, MWh/h
                            (for current year prior to optimisation interval)
        # (List of) conventional heat boiler(s)
            'hb0':        # IRI of heat boiler
            'hb1':        # name
            'hb2':        # capacity, MW
            'hb4':        # gas consumption/demand models (wrt hu)
            'hb5':        # start up cost, €/startup
            'hb6':        # shut down cost, €/shut-down
            'hb7':        # wear cost, €/MWh
            'hb8':        # availability
            'hb9':        # labour cost
            'hb10':       # heat generation history, MWh/h
                            (for x hours prior to optimisation interval)
        # (List of) gas turbine(s)
            'gt0':        # IRI of gas turbine
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
            'gt14':       # heat generation history, MWh/h
                            (for x hours prior to optimisation interval)
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
        # Assign EfW plant instance IRI
        sc = {'sc0': provider}
        # Get static contract details 
        # (include instance IRIs for which to retrieve ts data subsequently)
        sc.update(kg_client.get_sourcing_contract_properties(provider))
        # Get tiered unit price structure
        sc.update(kg_client.get_tiered_unit_prices(sc.pop('tiered_price')))        
        # Add to overall optimisation detup
        params = extend_setup_dictionary(params, sc)  
            
    # Add gas boilers
    for boiler in heat_providers.get('boilers', []):
        # Assign boiler instance IRI
        hb = {'hb0': boiler}
        # Get static boiler details 
        # (include instance IRIs for which to retrieve ts data subsequently)
        hb.update(kg_client.get_heat_boiler_properties(boiler))
        # Set gas consumption model
        hb.update({'hb4': consumption_models[boiler]})
        # Set start-up and shut-down cost to zero
        # NOTE: Negligible/NA for conventional gas boilers
        hb.update({'hb5': 0.0, 'hb6': 0.0})
        # Add to overall optimisation detup
        params = extend_setup_dictionary(params, hb)  
            
    # Add gas turbine
    for turbine in heat_providers.get('gt', []):
        # Assign turbine instance IRI
        gt = {'gt0': turbine}
        # Get static gas turbine details
        # (include instance IRIs for which to retrieve ts data subsequently)
        gt.update(kg_client.get_gas_turbine_properties(turbine))
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
    
    # Initialise consolidated time series DataFrames with 'time' column to merge data on
    df = pd.DataFrame()
    df.index.name = 'time'    
    q_generated = df.copy()
    q_provided = df.copy()
    forecasts = df.copy()
    
    # Map time bounds and DataFrames to corresponding data types
    ts_bounds = {
        OHN_GENERATED_HEAT_AMOUNT: (
            (datetime.strptime(opti_start_dt, time_format)-timedelta(hours=HIST_LENGTH)).strftime(time_format),
            (datetime.strptime(opti_start_dt, time_format)-timedelta(hours=1)).strftime(time_format)),
        OHN_PROVIDED_HEAT_AMOUNT: (
            datetime(datetime.strptime(opti_start_dt, TIME_FORMAT).year, 1, 1).strftime(time_format),
            (datetime.strptime(opti_start_dt, time_format)-timedelta(hours=1)).strftime(time_format))
    }

    # Query ts data for all placeholder IRIs (NOTE: still includes generator IRIs)
    ts_data_iris = extract_iris_from_setup_dict(params)
    for iri in ts_data_iris:
        # NOTE: Optimisation assumes data in certain units; hence, query ts 
        #       data for expected units!
        rdf_type = kg_client.get_rdftype(iri)
        # Only retrieve ts data for meaningsful properties (i.e., not for generator IRIs)
        if rdf_type in UNITS.keys():
            unit = UNITS[rdf_type]
            # Retrieve ts data for corresponding time bounds; default: opti interval
            t1, t2 = ts_bounds.get(rdf_type, (opti_start_dt, opti_end_dt))        
            df = retrieve_consolidated_timeseries_as_dataframe(kg_client, ts_client,
                            instance_iri=iri, unit=unit, lowerbound=t1, 
                            upperbound=t2, column_name=iri)
            if rdf_type == OHN_GENERATED_HEAT_AMOUNT:
                q_generated = q_generated.merge(df, on='time', how='outer')
            elif rdf_type == OHN_PROVIDED_HEAT_AMOUNT:
                q_provided = q_provided.merge(df, on='time', how='outer')
            else:
                forecasts = forecasts.merge(df, on='time', how='outer')
    
    # Condition forecasts collectively (to ensure availability of all required optimisation inputs)
    check_interval_spacing(forecasts)
    # Check for missing data
    if forecasts.isna().sum().sum() != 0:
        logger.warn('Some time series data is missing and will be forward filled.')
        logger.info(forecasts.isna().sum())
        forecasts.fillna(method='ffill', inplace=True)
    # Verify data availability for entire optimisation interval
    if not pd.Timestamp(opti_start_dt).tz_localize(None) == forecasts.index.min() or \
       not pd.Timestamp(opti_end_dt).tz_localize(None) == forecasts.index.max():
           raise_error(ValueError, 'Not all time series data available for optimisation interval.')
    # Convert boolean availabilities to one-hot encoded data
    bool_columns = forecasts.select_dtypes(include=['bool']).columns
    forecasts[bool_columns] = forecasts[bool_columns].astype(int) 
        
    # Extract DataTime index (for later use); then drop to ensure internal consistency
    index = pd.to_datetime(forecasts.index)
    forecasts.reset_index(drop=True, inplace=True)
    
    logger.info('Queried and validated time series data.')
    
    ###########################################################################
    # 3) Replace placeholder instance IRIs with validated time series data
    #
    logger.info('Replacing placeholder IRIs with time series data...')
    
    replacements = forecasts.to_dict(orient='list')
    replacements.update(q_generated.to_dict(orient='list'))
    replacements.update(q_provided.to_dict(orient='list'))
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
        setup['sourcing_contracts'].append({'iri': params['sc0'][i], 'name': params['sc1'][i], 
                                            'qlimits_pa': params['sc2'][i], 'prices': params['sc3'][i],
                                            'current_price': params['sc4'][i], 'entry_point': params['sc5'][i],
                                            'availability': pd.Series(params['sc6'][i], name='availability'),
                                            'qmin': pd.Series(params['sc7'][i], name='qmin'),
                                            'qmax': pd.Series(params['sc8'][i], name='qmax'),
                                            'q_hist': pd.Series(params['sc9'][i], name='q_hist')})
    # Add boiler(s) to setup
    for i in range(len(params['hb1'])):
        setup['heat_boilers'].append({'iri': params['hb0'][i], 'name': params['hb1'][i], 
                                      'capacity': params['hb2'][i], 'gas_demand': params['hb4'][i], 
                                      'start_up_cost': params['hb5'][i], 'shut_down_cost': params['hb6'][i], 
                                      'wear_cost': pd.Series(params['hb7'][i], name='wear_cost'),
                                      'availability': pd.Series(params['hb8'][i], name='availability'),
                                      'labour_cost': pd.Series(params['hb9'][i], name='labour_cost'),
                                      'q_hist': pd.Series(params['hb10'][i], name='q_hist')})
    # Add gas turbine(s) to setup
    for i in range(len(params['gt1'])):
        setup['gas_turbines'].append({'iri': params['gt0'][i], 'name': params['gt1'][i], 
                                      'power_el': params['gt2'][i], 'power_q': params['gt3'][i], 
                                      'min_load': params['gt4'][i], 'gas_demand': params['gt6'][i], 
                                      'el_output': params['gt7'][i], 'idle_period': params['gt8'][i],
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
    

def create_optimisation_setup(setup_dict):
    """
    Creates optimisation setup comprised of 'market_prices', 'gas_properties', 
    'sourcing_contracts', 'heat_boilers', 'gas_turbines', 'district_heating_grid',
    and 'municipal_utility' objects as described in 'setup_dict'

    Arguments:
        setup_dict {dict} -- dictionary providing all information to create 
                             optimization setup objects (as provided by 
                             'define_optimisation_setup')
    Returns:
        MarketPrices and MunicipalUtility  objects
    """

    # Initialise lists for contracts, boilers, and turbines
    mu_contracts = []
    mu_boilers = []
    mu_turbines = []

    # Loop over main keys in setup dictionary to create respective objects
    for item in setup_dict.keys():

        # create MarketPrices object
        if item == 'market_prices':
            market_prices = MarketPrices(**setup_dict[item])

        # create GasProperties object
        elif item == 'gas_properties':
            gas = GasProperties(**setup_dict[item])

        # create GasProperties object
        elif item == 'district_heating_grid':
            grid = DistrictHeatingGrid(**setup_dict[item])

        # create municipal utility company object
        elif item == 'municipal_utility':
            utility = MunicipalUtility(**setup_dict[item])

        # create heat sourcing contract objects
        elif item == 'sourcing_contracts':
            for i in range(len(setup_dict[item])):
                mu_contracts.append(SourcingContract(**setup_dict[item][i]))

        # create conventional heat boiler objects
        elif item == 'heat_boilers':
            for i in range(len(setup_dict[item])):
                mu_boilers.append(HeatBoiler(**setup_dict[item][i]))

        # create gas turbine objects
        elif item == 'gas_turbines':
            for i in range(len(setup_dict[item])):
                mu_turbines.append(GasTurbine(**setup_dict[item][i]))

        # extract district heating grid entry points
        elif item == 'grid_entry_points':
            grid_entries = setup_dict[item]

    # Add grid entry points to district heating grid
    for entry in grid_entries:
        grid.add_entry_point(**entry)

    # Add heat sourcing contracts, boilers, and turbines to municipal utility
    for mu_c in mu_contracts:
        # initialise current price
        mu_c.update_internal_price()
        # add to municipal utility
        utility.add_contract(mu_c)
    for mu_b in mu_boilers:
        utility.add_boiler(mu_b)
    for mu_t in mu_turbines:
        # initialise start-up and shut-down cost
        mu_t.set_startup_cost(gas=gas, prices=market_prices)
        mu_t.set_shutdown_cost()
        # add to municipal utility
        utility.add_gas_turbine(mu_t)

    # Assign fuel/gas and district heating network objects
    utility.set_fuel(gas)
    utility.connect_network(grid)

    return market_prices, utility


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
