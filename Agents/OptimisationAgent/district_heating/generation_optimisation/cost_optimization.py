"""
SWPS heat generation optimisation model with MINIMAL TOTAL HEAT GENERATION COST as objective function.

Current assumptions/limitations:
    - hierarchical optimisation with 2 primary heat generation modes: 1) heat generation with available GT, and
                                                                      2) heat generation without gas turbine
    - if GT is used, GT will always run on maximum possible load (constrained by grid capacity/heat demand)
    - hourly time intervals
    - neglected heat storage

@author: Markus Hofmeister
"""

import os
import copy
import time
import numpy as np
import pandas as pd
import datetime as dt
from pathlib import Path
from tabulate import tabulate

import webbrowser
import matplotlib.pyplot as plt
from CoolProp.CoolProp import PropsSI
from statsmodels.tsa.arima.model import ARIMA
from pmdarima.preprocessing import FourierFeaturizer

from district_heating.generation_optimisation import cost_optimization_postprocessing as postprocessing
from district_heating.timeseries_forecasting import create_SARIMAX_model


####################     CLASSES     ####################


class MarketPrices:
    # class comprising all market prices
    def __init__(self, gas_q, gas_gt, el_spot, co2, chp_bonus, grid_save):
        # dynamic (time-dependent) attributes
        self.gas_q = gas_q          # gas price for boilers (contracts), €/MWh_gas (wrt higher calorific value)
        self.gas_gt = gas_gt        # gas price for gas turbine (spot), €/MWh_gas (wrt higher calorific value)
        self.el_spot = el_spot      # electricity spot price, €/MWh_el
        self.co2 = co2              # cost of co2 emission certificates (EUAs), €/t_co2
        self.chp_bonus = chp_bonus  # chp subsidies per MWh of co-generated electricity, €/MWh_el
        self.grid_save = grid_save  # saved grid charges due to own electricity generation, €/MWh_el

    def __repr__(self):
        # override default naming method by assigning class name 'MarketPrices' as printable representation of object
        return self.__class__.__name__

    @classmethod
    def dummies(cls, length):
        # dummy constructor as class method:
        # creates MarketPrices object with dummy data; dynamic attributes with 'length' time steps

        # dictionary of all 'dummy' data
        dyn = {
            'gas_q': [21.00 for i in range(length)],
            'gas_gt': [24.00 for i in range(length)],
            'el_spot': [43.00 for i in range(length)],
            'co2': [25.00 for i in range(length)],
            'chp_bonus': [0.00 for i in range(length)],
            'grid_save': [4.70 for i in range(length)]
        }

        # create DataFrame of all dynamic data (to have aligned time index)
        df = pd.DataFrame.from_dict(dyn)

        # call class constructor with dummy data (as pd.Series)
        return cls(df['gas_q'], df['gas_gt'], df['el_spot'], df['co2'], df['chp_bonus'], df['grid_save'])

    def get_el_remun(self, timestep):
        # total remuneration for electricity at particular time step, €/MWh_el
        return self.el_spot[timestep] + self.chp_bonus[timestep] + self.grid_save[timestep]

    def create_copy(self, start=0, stop=None):
        # create copy of MarketPrices object for period of integer-location indices [start, stop)
        # 'start' is included as first index
        # 'stop' is excluded as first index, if 'None' full time series will be copied

        # extract current index (from any time series attribute)
        index = self.gas_q.index

        # convert None to respective integer index to copy object in full length if 'stop' is None
        if stop is None:
            stop = len(index)

        # raise errors for erroneous indices
        if not (isinstance(start, int) & isinstance(stop, int)):
            raise IndexError("Indices 'start' and 'stop' need to be integers for .iloc based indexing")
        if start < 0:
            raise ValueError ("'Start' index out of bounds (smaller than zero)")
        if stop > len(index):
            raise ValueError("'Stop' index out of bounds (larger than maximum index)")
        else:
            # extract time series for specified integer indices (.iloc)
            gas_q = self.gas_q.iloc[start: stop].reset_index(drop=True)
            gas_gt = self.gas_gt.iloc[start: stop].reset_index(drop=True)
            el_spot = self.el_spot.iloc[start: stop].reset_index(drop=True)
            co2 = self.co2.iloc[start: stop].reset_index(drop=True)
            chp_bonus = self.chp_bonus.iloc[start: stop].reset_index(drop=True)
            grid_save = self.grid_save.iloc[start: stop].reset_index(drop=True)

            # return new MarketPrices object with copied data for specified period/indices
            return MarketPrices(gas_q=gas_q, gas_gt=gas_gt, el_spot=el_spot, co2=co2, chp_bonus=chp_bonus,
                                grid_save=grid_save)


class GasProperties:
    # class defining gas properties used by boilers and gas turbine
    def __init__(self, ho, hu, co2_factor):
        # constant (time-independent) attributes
        self.ho = ho                    # higher calorific value, kWh/m³
        self.hu = hu                    # lower calorific value, kWh/m³
        self.co2_factor = co2_factor    # specific CO2 generation per MWh gas burned, t_CO2/MWh_g (wrt Hu)

    def __repr__(self):
        # override default naming method by assigning class name 'GasProperties' as printable representation of object
        return self.__class__.__name__

    @classmethod
    def dummies(cls):
        # dummy constructor as class method: creates GasProperties object with dummy data

        # dictionaries of all 'dummy' data
        const = {
            'ho': 11.38,
            'hu': 10.272,
            'co2_factor': 0.056*3.6    # wrt hu
            # established CO2 factor for natural gas: 0.056 t_CO2/GJ wrt Hu
            # 1 MWh = 3.6 GJ
        }

        # call class constructor with dummy data
        return cls(**const)


class SourcingContract:
    # class defining external heat sourcing by time-dependent take-or-pay contract
    def __init__(self, name, qlimits_pa, prices, current_price, entry_point, availability, qmin, qmax, q_hist=[]):
        # constant (time-independent) attributes
        self.name = name                    # name/ID of heat sourcing contract
        self.qlimits_pa = qlimits_pa        # contractually agreed sourcing/supply limits [min_pa, max_pa], MWh/a
        # min_pa: contractually agreed minimum quantity to be sourced from MHKW p.a. (take-or-pay)
        # max_pa: contractually agreed maximum quantity to be supplied by MHKW p.a.
        self.prices = prices                # heat unit prices (incl. potential volume discount) [p_reg, p_red], €/MWh
        # p_reg: regular base price per heat unit for quantities up to 'min_pa' (contractually fixed for each year)
        # p_red: reduced base price for volumes exceeding 'min_pa' quantity
        self.current_price = current_price  # current heat unit price, €/MWh
        self.grid_entry_point = entry_point # associated district heating grid entry point (needs to match an entry
                                            # point in DistrictHeatingGrid entry_points dict)


        # dynamic (time-dependent) attributes
        self.available = availability       # availability of contracted heat: [1, 0] ... yes,no
        self.qmin = qmin                    # technical hourly supply minimum (defined by minimum pump speed), MWh/h
        self.qmax = qmax                    # technical hourly supply maximum (constrained by grid hydraulics), MWh/h
        self.q_hist = q_hist                # (annual) heat sourcing history, MWh/h

    def __repr__(self):
        # override generic default naming method by assigning 'name' as printable representation of object
        return '{}_{}'.format(self.__class__.__name__, self.name)

    @classmethod
    def dummies(cls, length):
        # dummy constructor as class method:
        # creates SourcingContract object with dummy data; dynamic attributes with 'length' time steps

        # dictionary of all 'dummy' data
        const = {
            'name': 'MHKW_ToP',
            'qlimits_pa': [26000.0, 65000.0],                       # constant for each year (e.g., 2021 values)
            'prices': [12.67, 0.00],                                # 2020 values
            'current_price': np.nan,                                # initialize with NaNs
            'entry_point': 'MHKW'
        }
        dyn = {
            'availability': [1 for i in range(length)],
            'qmin': [1.0 for i in range(length)],
            'qmax': [11.0 for i in range(length)],
            'q_hist': [np.nan for i in range(length)]               # initialise sourcing history with NaNs
        }

        # create DataFrame of all dynamic data (to have aligned time index)
        df = pd.DataFrame.from_dict(dyn)

        # call class constructor with dummy data
        contract = cls(**const, availability=df['availability'], qmin=df['qmin'], qmax=df['qmax'], q_hist=df['q_hist'])

        # update current price with actual heat unit price (depending on previous sourcing history)
        contract.update_price()

        return contract

    def update_price(self):
        # updates current heat unit price (depending on total annual heat sourcing volume)
        total = self.q_hist.sum(skipna=True)
        if total <= self.qlimits_pa[0]:
            self.current_price = self.prices[0]
        elif total <= self.qlimits_pa[1]:
            self.current_price = self.prices[1]
        else:
            # suppress heat sourcing beyond annual maximum supply limit
            self.current_price = np.inf

    def create_copy(self, start=0, stop=None):
        # create copy of SourcingContract object for period of integer-location indices [start, stop)
        # 'start' is included as first index
        # 'stop' is excluded as first index, if 'None' full time series will be copied

        # extract current index (from any time series attribute)
        index = self.available.index

        # convert None to respective integer index to copy object in full length if 'stop' is None
        if stop is None:
            stop = len(index)

        # raise errors for erroneous indices
        if not (isinstance(start, int) & isinstance(stop, int)):
            raise IndexError("Indices 'start' and 'stop' need to be integers for .iloc based indexing")
        if start < 0:
            raise ValueError ("'Start' index out of bounds (smaller than zero)")
        if stop > len(index):
            raise ValueError("'Stop' index out of bounds (larger than maximum index)")
        else:
            # extract time series for specified integer indices (.iloc)
            available = self.available.iloc[start: stop].reset_index(drop=True)
            qmin = self.qmin.iloc[start: stop].reset_index(drop=True)
            qmax = self.qmax.iloc[start: stop].reset_index(drop=True)
            q_hist = self.q_hist.iloc[start: stop].reset_index(drop=True)

            # return new SourcingContract object with copied data for specified period/indices
            return SourcingContract(name=self.name, qlimits_pa=self.qlimits_pa, prices=self.prices,
                                    current_price=self.current_price, entry_point=self.grid_entry_point,
                                    availability=available, qmin=qmin, qmax=qmax, q_hist=q_hist)


class HeatBoiler:
    # class defining a conventional heat boiler
    def __init__(self, name, capacity, efficiency, gas_demand, start_up_cost, shut_down_cost, wear_cost,
                 availability, labour_cost, q_hist=[]):
        # constant (time-independent) attributes
        self.name = name                    # name/ID of heat boiler
        self.capacity = capacity            # maximum heat generation capacity, MW
        self.efficiency = efficiency        # heat generation efficiency, MWh_q/MWh_g (wrt lower calorific value)
        self.gas_demand = gas_demand        # gas demand model, calculating gas consumption based on generated heat
                                            # (wrt lower calorific value)
        self.cost_start = start_up_cost     # start-up cost, €/start-up
        self.cost_shut = shut_down_cost     # shut-down cost, €/shut-down
        self.cost_wear = wear_cost          # wear cost per operating hour, €/MWh_q
        # dynamic (time-dependent) attributes
        self.available = availability       # availability of boiler: [1, 0] ... yes,no
        self.cost_labour = labour_cost      # personnel cost per operating hour (shift surcharges, etc.), €/h
        self.q_hist = q_hist                # (annual) heat generation history, MWh/h

    def __repr__(self):
        # override generic default naming method by assigning 'name' as printable representation of object
        return '{}_{}'.format(self.__class__.__name__, self.name)

    @classmethod
    def dummies(cls, name, length):
        # dummy constructor as class method:
        # creates HeatBoiler object with dummy data; dynamic attributes with 'length' time steps

        # dictionaries of all 'dummy' data
        const = {
            'name': name,
            'capacity': 7.5,
            'efficiency': 0.90,                                 # wrt lower calorific value
            'gas_demand': None,
            'start_up_cost': 0.00,
            'shut_down_cost': 0.00,
            'wear_cost': 1.50
        }
        dyn = {
            'availability': [1 for i in range(length)],
            'labour_cost': [0.00 for i in range(length)],
            'q_hist': [np.nan for i in range(length)]           # initialise generation history with NaNs
        }

        # create DataFrame of all dynamic data (to have aligned time index)
        df = pd.DataFrame.from_dict(dyn)

        # call class constructor with dummy data (as pd.Series)
        return cls(**const, availability=df['availability'], labour_cost=df['labour_cost'], q_hist=df['q_hist'])

    def create_copy(self, start=0, stop=None):
        # create copy of HeatBoiler object for period of integer-location indices [start, stop)
        # 'start' is included as first index
        # 'stop' is excluded as first index, if 'None' full time series will be copied

        # extract current index (from any time series attribute)
        index = self.available.index

        # convert None to respective integer index to copy object in full length if 'stop' is None
        if stop is None:
            stop = len(index)

        # raise errors for erroneous indices
        if not (isinstance(start, int) & isinstance(stop, int)):
            raise IndexError("Indices 'start' and 'stop' need to be integers for .iloc based indexing")
        if start < 0:
            raise ValueError ("'Start' index out of bounds (smaller than zero)")
        if stop > len(index):
            raise ValueError("'Stop' index out of bounds (larger than maximum index)")
        else:
            # extract time series for specified integer indices (.iloc)
            available = self.available.iloc[start: stop].reset_index(drop=True)
            cost_labour = self.cost_labour.iloc[start: stop].reset_index(drop=True)
            q_hist = self.q_hist.iloc[start: stop].reset_index(drop=True)

            # return new HeatBoiler object with copied data for specified period/indices
            return HeatBoiler(name=self.name, capacity=self.capacity, efficiency=self.efficiency, gas_demand=self.gas_demand,
                              start_up_cost=self.cost_start, shut_down_cost=self.cost_shut, wear_cost=self.cost_wear,
                              availability=available, labour_cost=cost_labour, q_hist=q_hist)


class GasTurbine:
    # class defining a CHP gas turbine
    def __init__(self, name, power_el, power_q, min_load, efficiency, gas_demand, el_output, idle_period,
                 wear_cost, availability, labour_cost, start_up_cost, shut_down_cost, q_hist=[]):
        # constant (time-independent) attributes
        self.name = name                    # name/ID of gas turbine
        self.power_el = power_el            # maximum electricity generation capacity, MW
        self.power_q = power_q              # maximum heat generation capacity, MW
        self.min_load = min_load            # minimum required heat load (corresponds to 70% electrical load), MW
        self.efficiency = efficiency        # total energy efficiency, (MWh_q+MWh_el)/MWh_g (wrt lower calorific value)
        self.gas_demand = gas_demand        # gas demand model, calculating gas consumption based on generated heat
                                            # (wrt lower calorific value)
        self.el_output = el_output          # electricity output model, calculating el. power based on generated heat
        self.idle_period = idle_period      # minimum duration between 2 subsequent GT operations, h
        self.cost_wear = wear_cost          # wear cost per operating hour, €/h
        # dynamic (time-dependent) attributes
        self.available = availability       # availability of GT: [1, 0] ... yes,no
        self.cost_labour = labour_cost      # personnel cost per operating hour (shift surcharges, etc.), €/h
        self.cost_start = start_up_cost     # start-up cost, €/start-up
        self.cost_shut = shut_down_cost     # shut-down cost, €/shut-down
        self.q_hist = q_hist                # (annual) heat generation history, MWh/h

        # total power of gas turbine, MW
        self.power = self.power_q + self.power_el

    def __repr__(self):
        # override generic default naming method by assigning 'name' as printable representation of object
        return '{}_{}'.format(self.__class__.__name__, self.name)

    @classmethod
    def dummies(cls, name, length, gas=None, prices=None):
        # dummy constructor as class method:
        # creates GasTurbine object with dummy data; dynamic attributes with 'length' time steps

        # dictionaries of all 'dummy' data
        const = {
            'name': name,
            'power_el': 6.5,
            'power_q': 11.7,
            'min_load': 8.2,
            'efficiency': 0.88,                                   # wrt lower calorific value
            'gas_demand': None,
            'el_output': None,
            'idle_period': 5,                                     # ~4-6h required for GT to cool down before next start
            'wear_cost': 53.00
        }
        dyn = {
            'availability': [1 for i in range(length)],
            'labour_cost': [88.27 for i in range(length)],        # 2020 values
            'start_up_cost': [0.0 for i in range(length)],        # initialise; to be populated later
            'shut_down_cost': [0.0 for i in range(length)],       # initialise; to be populated later
            'q_hist': [np.nan for i in range(length)]             # initialise generation history with NaNs
        }

        # create DataFrame of all dynamic data (to have aligned time index)
        df = pd.DataFrame.from_dict(dyn)

        # call class constructor with dummy data (as pd.Series)
        gt = cls(**const, availability=df['availability'], labour_cost=df['labour_cost'],
                 start_up_cost=df['start_up_cost'], shut_down_cost=df['shut_down_cost'], q_hist=df['q_hist'])

        # populate start-up and shut-down cost with actual values
        gt.set_shutdown_cost()
        gt.set_startup_cost(gas, prices)

        return gt

    def set_shutdown_cost(self):
        # populates previously initialised shut-down cost with actual values --> underlying logic:
        # gas turbine's heat recovery boiler needs to be supervised during shut down until temperature decreases below
        # uncritical level, which requires approx. 2h of personnel
        self.cost_shut = 2 * self.cost_labour

    def set_startup_cost(self, gas=None, prices=None):
        # populates previously initialised start-up cost with actual values --> underlying logic:
        # every start-up counts as 15 operation hours wrt to wear (according to OEM and inspection schedule) +
        # consumes approx. 6 MWh of gas (wrt ho) with unusable heat production
        if isinstance(gas, GasProperties) & isinstance(prices, MarketPrices):
            if self.cost_start.index.equals(prices.co2.index) & self.cost_start.index.equals(prices.gas_gt.index):
                # extract current start-up cost pd.Series (to keep index, name, etc.)
                cost = self.cost_start.copy()
                # calculate start-up cost
                cost[:] = 15 * self.cost_wear                                   # wear cost of 15h
                cost += 6 * prices.gas_gt                                       # gas cost for 6 MWh_g (wrt Ho)
                cost += 6 * gas.hu / gas.ho * gas.co2_factor * prices.co2       # CO2 cost for 6 MWh_g (wrt Ho)
                cost = cost.round(1)
                # reassign as object property
                self.cost_start = cost
            else:
                raise IndexError("Index of provided 'gas' and 'prices' objects does not match gas turbine index")
        elif (gas is None) | (prices is None):
            # assign constant start-up cost of approx. 800 €/start-up
            self.cost_start.replace(self.cost_start.values, 800, inplace=True)
        else:
            raise TypeError("Provided arguments 'gas' and 'prices' need to be GasProperties and MarketPrices "
                            "objects, respectively")

    def get_idle_time(self):
        # returns gas turbine's idle time (count of inactive time steps since last operation)
        if (len(self.q_hist[self.q_hist.notna()]) < self.idle_period) & (self.q_hist.sum(skipna=True) == 0.0):
            # if generation history has less than 'idle_period' entries and all are zero
            idle = self.idle_period
        else:
            if self.q_hist.sum(skipna=True, min_count=1) == 0.0:
                # if all non-NaN entries are zeros, hence gas turbine has not been active (yet)
                idle = self.q_hist[self.q_hist.notna()].index[-1] + 1
            else:
                # derive index of last active operation
                last_active = self.q_hist[self.q_hist > 0.0].index[-1]
                # derive difference between last non-NaN entry and last active entry
                idle = self.q_hist[self.q_hist.notna()].index[-1] - last_active
        return idle

    def create_copy(self, start=0, stop=None):
        # create copy of GasTurbine object for period of integer-location indices [start, stop)
        # 'start' is included as first index
        # 'stop' is excluded as first index, if 'None' full time series will be copied

        # extract current index (from any time series attribute)
        index = self.available.index

        # convert None to respective integer index to copy object in full length if 'stop' is None
        if stop is None:
            stop = len(index)

        # raise errors for erroneous indices
        if not (isinstance(start, int) & isinstance(stop, int)):
            raise IndexError("Indices 'start' and 'stop' need to be integers for .iloc based indexing")
        if start < 0:
            raise ValueError ("'Start' index out of bounds (smaller than zero)")
        if stop > len(index):
            raise ValueError("'Stop' index out of bounds (larger than maximum index)")
        else:
            # extract time series for specified integer indices (.iloc)
            available = self.available.iloc[start: stop].reset_index(drop=True)
            cost_labour = self.cost_labour.iloc[start: stop].reset_index(drop=True)
            cost_start = self.cost_start.iloc[start: stop].reset_index(drop=True)
            cost_shut = self.cost_shut.iloc[start: stop].reset_index(drop=True)
            q_hist = self.q_hist.iloc[start: stop].reset_index(drop=True)

            # return new GasTurbine object with copied data for specified period/indices
            return GasTurbine(name=self.name, power_el=self.power_el, power_q=self.power_q, min_load=self.min_load,
                              efficiency=self.efficiency, gas_demand=self.gas_demand, el_output=self.el_output,
                              idle_period=self.idle_period, wear_cost=self.cost_wear, availability=available,
                              labour_cost=cost_labour, start_up_cost=cost_start, shut_down_cost=cost_shut, q_hist=q_hist)


class MunicipalUtility:
    # class defining a municipal utility company providing district heating
    def __init__(self, name, conv_boilers, gas_turbines, contracts, fuel, network, q_demand):
        # constant (time-independent) attributes / set-up
        self.name = name                        # name/ID of municipal utility
        self.boilers = conv_boilers             # list of conventional boiler objects operated by the utility
        self.gas_turbines = gas_turbines        # list of gas turbine objects operated by the utility
        self.contracts = contracts              # list of (external) heat sourcing contracts available to the utility
        self.fuel = fuel                        # gas property object describing the fuel available to the utility
        self.network = network                  # district heating grid object to which the utility is connected
        # dynamic (time-dependent) attributes
        self.q_demand = q_demand                # total district heating demand (supply to the grid), MWh

    def __repr__(self):
        # override generic default naming method by assigning 'name' as printable representation of object
        return '{}_{}'.format(self.__class__.__name__, self.name)

    @classmethod
    def dummies(cls, length):
        # dummy constructor as class method: creates a MunicipalUtility object
        # without boilers and gas turbines, but dynamic attributes of 'length' time steps

        # dictionaries of all 'dummy' data
        const = {
            'name': 'SWPS',
            'conv_boilers': [],
            'gas_turbines': [],
            'contracts': [],
            'fuel': None,
            'network': None
        }
        dyn = {
            'q_demand': [20.0 for i in range(length)],
        }

        # create DataFrame of all dynamic data (to have aligned time index)
        df = pd.DataFrame.from_dict(dyn)

        # call class constructor with dummy data (as pd.Series)
        return cls(**const, q_demand=df['q_demand'])

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

    def create_copy(self, start=0, stop=None):
        # create copy of MunicipalUtility and subordinated objects for period of integer-location indices [start, stop)
        # 'start' is included as first index
        # 'stop' is excluded as first index, if 'None' full time series will be copied

        # extract current index (from any time series attribute)
        index = self.q_demand.index

        # convert None to respective integer index to copy object in full length if 'stop' is None
        if stop is None:
            stop = len(index)

        # raise errors for erroneous indices
        if not (isinstance(start, int) & isinstance(stop, int)):
            raise IndexError("Indices 'start' and 'stop' need to be integers for .iloc based indexing")
        if start < 0:
            raise ValueError("'Start' index out of bounds (smaller than zero)")
        if stop > len(index):
            raise ValueError("'Stop' index out of bounds (larger than maximum index)")
        else:
            # extract time series for specified integer indices (.iloc)
            q_demand = self.q_demand.iloc[start: stop].reset_index(drop=True)

            # create new MunicipalUtility object with copied data for specified period/indices
            mu = MunicipalUtility(name=self.name, conv_boilers=[], gas_turbines=[], contracts=[], fuel=self.fuel,
                                  network=None, q_demand=q_demand)

            # add copies of boilers, contracts, and gas turbines
            for b in self.boilers:
                mu.add_boiler(b.create_copy(start, stop))
            for gt in self.gas_turbines:
                mu.add_gas_turbine(gt.create_copy(start, stop))
            for c in self.contracts:
                mu.add_contract(c.create_copy(start, stop))

            # add copy of heating grid
            mu.connect_network(self.network.create_copy(start, stop))

            return mu


class DistrictHeatingGrid:
    # class defining the district heating network
    def __init__(self, name, entry_points):
        # constant (time-independent) attributes
        self.name = name                        # name/ID of district heating grid
        self.entry_points = entry_points        # heat entry points to district heating grid for heat provision

    def __repr__(self):
        # override generic default naming method by assigning 'name' as printable representation of object
        return '{}_{}'.format(self.__class__.__name__, self.name)

    @classmethod
    def dummies(cls):
        # dummy constructor as class method: creates DistrictHeatingGrid object with dummy data

        # dictionaries of all 'dummy' data
        const = {
            'name': 'Pirmasens',
            'entry_points': {}
        }

        # call class constructor with dummy data
        return cls(**const)

    def add_entry_point(self, name, min_circulation, pressure, temp_flow, temp_return):
        # add heat entry/supply point to district heating grid
        if isinstance(name, str):
            characteristics = {
                'min_circulation': min_circulation,     # minimum water circulation volume (due to pump specs), m³/h
                'pressure': pressure,                   # operating pressure, bara
                'temp_flow': temp_flow,                 # water supply temperature (Vorlauftemperatur), °C
                'temp_return': temp_return              # water return temperature (Ruecklauftemperatur), °C
            }
            # add heat entry/supply point to DistrictHeatingGrid dictionary with 'name' as key
            self.entry_points[name] = characteristics
        else:
            raise ValueError("Entry point's 'name' needs to be a string")

    def add_entry_point_dummy(self, name, length):
        # add dummy entry point with ~average HKW properties
        mc = 80                                                    # HKW: 80 m³/h, MHKW: 30 m³/h
        dyn = {'pressure': [8 for i in range(length)],             # 6-8 bar as average
               'temp_flow': [100 for i in range(length)],          # average of summer and winter
               'temp_return': [70 for i in range(length)] }        # average of summer and winter

        # create DataFrame of all dynamic data (to have aligned time index)
        df = pd.DataFrame.from_dict(dyn)
        # call add_entry_point with conditioned dummy data
        self.add_entry_point(name, min_circulation=mc, pressure=df['pressure'], temp_flow=df['temp_flow'],
                             temp_return=df['temp_return'])

    def create_copy(self, start=0, stop=None):
        # create copy of DistrictHeatingGrid with entry_points dict for period of integer-location indices [start, stop)
        # 'start' is included as first index
        # 'stop' is excluded as first index, if 'None' full time series will be copied

        if len(self.entry_points.keys()) == 0:
            # in case original DistrictHeatingGrid does not have any entry points
            entry_points = {}
        else:
            # retrieve names of entry points
            names = list(self.entry_points.keys())

            # extract current index (from any time series attribute)
            index = self.entry_points[names[0]]['pressure'].index

            # convert None to respective integer index to copy object in full length if 'stop' is None
            if stop is None:
                stop = len(index)

            # raise errors for erroneous indices
            if not (isinstance(start, int) & isinstance(stop, int)):
                raise IndexError("Indices 'start' and 'stop' need to be integers for .iloc based indexing")
            if start < 0:
                raise ValueError("'Start' index out of bounds (smaller than zero)")
            if stop > len(index):
                raise ValueError("'Stop' index out of bounds (larger than maximum index)")
            else:
                # create deep copy of original entry_points dict (to fully break binding between copy and original)
                entry_points = copy.deepcopy(self.entry_points)
                for name in names:
                    entry_points[name]['min_circulation'] = self.entry_points[name]['min_circulation']
                    # extract entry point time series for specified integer indices (.iloc)
                    entry_points[name]['pressure'] = self.entry_points[name]['pressure'].iloc[start: stop].\
                                                     reset_index(drop=True)
                    entry_points[name]['temp_flow'] = self.entry_points[name]['temp_flow'].iloc[start: stop].\
                                                      reset_index(drop=True)
                    entry_points[name]['temp_return'] = self.entry_points[name]['temp_return'].iloc[start: stop].\
                                                        reset_index(drop=True)

        return DistrictHeatingGrid(name=self.name, entry_points=entry_points)


####################     FUNCTIONS     ####################


def define_optimization_setup(ts_data, opt_period,
                              mp1=None, mp2=None, mp3=None, mp4=None, mp5=None, mp6=None,
                              gp1=None, gp2=None, gp3=None,
                              sc1=None, sc2=None, sc3=None, sc4=None, sc5=None, sc6=None, sc7=None, sc8=None, sc9=None,
                              hb1=None, hb2=None, hb3=None, hb4=None, hb5=None, hb6=None, hb7=None, hb8=None, hb9=None,
                              hb10=None,
                              gt1=None, gt2=None, gt3=None, gt4=None, gt5=None, gt6=None, gt7=None, gt8=None, gt9=None,
                              gt10=None, gt11=None, gt12=None, gt13=None, gt14=None,
                              dh1=None, dh2=None, dh3=None, dh4=None, dh5=None, dh6=None, dh7=None,
                              mu1=None, mu2=None, mu3=None, mu4=None, mu5=None, mu6=None, mu7=None):
    """
    Returns a dictionary describing the full optimization setup, with first level keys referring to object instances,
    i.e., ['market_prices', 'gas_properties', 'sourcing_contracts', 'heat_boilers', 'gas_turbines', 'district_heating_
    grid','municipal_utility'] and values describing all parameters to create the respective objects
        - in case no explicit parameters are provided to specify certain optimization setup inputs [mp1 ... mu7],
          default values based on 2018 will be used

    :returns dict: dictionary describing the full optimization setup
    """
    # keep only data up to length 'opt_period' time steps ('1h')
    ts_data = ts_data.iloc[:opt_period]
    # extract DataTime index (for potential later use)
    index = ts_data.index
    # drop DateTime index in 'data' DataFrame to ensure internal consistency
    ts_data.reset_index(drop=True, inplace=True)

    ###   default variable values for all not specified parameters based on 2018 data   ###
    params = {
        # define market prices
        'mp1': mp1 if mp1 is not None else ts_data['Gaspreis Kessel'],                  # gas_q, €/MWh (wrt ho)
        'mp2': mp2 if mp2 is not None else ts_data['Gaspreis GT'],                      # gas_gt, €/MWh (wrt ho)
        'mp3': mp3 if mp3 is not None else ts_data['Spotpreis Strom'],                  # el_spot, €/MWh
        'mp4': mp4 if mp4 is not None else ts_data['CO2 Preis'],                        # co2, €/t
        'mp5': mp5 if mp5 is not None else [15.00 for i in range(opt_period)],          # chp_bonus, €/MWh
        'mp6': mp6 if mp6 is not None else [4.70 for i in range(opt_period)],           # grid_savings, €/MWh
        # define gas properties
        'gp1': gp1 if gp1 is not None else 11.38,                                       # ho, kWh/m³
        'gp2': gp2 if gp2 is not None else 10.272,                                      # hu, kWh/m³
        'gp3': gp3 if gp3 is not None else 0.056 * 3.6,                                 # CO2_factor, t_CO2/MWh_g (wrt hu)
        # define (list of) heat sourcing contract(s)
        'sc1': sc1 if sc1 is not None else ['MHKW_ToP'],                                # names
        'sc2': sc2 if sc2 is not None else [[15000.0, 65000.0]],                        # annual limits [min, max], MWh/a
        'sc3': sc3 if sc3 is not None else [[11.84, 11.84]],                            # base unit price [p_reg, p_red], €/MWh
        'sc4': sc4 if sc4 is not None else [np.nan],                                    # current heat unit price, €/MWh
        'sc5': sc5 if sc5 is not None else ['MHKW'],                                    # associated heating grid entry point
        'sc6': sc6 if sc6 is not None else [[1 for i in range(opt_period)]],            # availability
        'sc7': sc7 if sc7 is not None else [[1.0 for i in range(opt_period)]],          # technical min. supply, MWh/h
        'sc8': sc8 if sc8 is not None else [[11.0 for i in range(opt_period)]],         # technical max. supply, MWh/h
        'sc9': sc9 if sc9 is not None else [[np.nan for i in range(opt_period)]],       # heat sourcing history, MWh/h
        # define (list of) conventional heat boiler(s)
        'hb1': hb1 if hb1 is not None else ['Kessel4', 'Kessel5', 'Kessel6'],           # names
        'hb2': hb2 if hb2 is not None else [7.5, 7.5, 4.5],                             # capacity, MW
        'hb3': hb3 if hb3 is not None else [0.9, 0.9, 0.95],                            # efficiency, MWh_q/MWh_g (wrt hu)
        'hb4': hb4 if hb4 is not None else [None] * 3,                                  # gas consumption/demand models (wrt hu)
        'hb5': hb5 if hb5 is not None else [0.00] * 3,                                  # start_up_cost, €/startup
        'hb6': hb6 if hb6 is not None else [0.00] * 3,                                  # shut_down_cost, €/shut-down
        'hb7': hb7 if hb7 is not None else [1.50] * 3,                                  # wear_cost, €/MWh
        'hb8': hb8 if hb8 is not None else [[1 for i in range(opt_period)]] * 3,        # availability
        'hb9': hb9 if hb9 is not None else [[0.00 for i in range(opt_period)]] * 3,     # labour_cost, 2018 average
        'hb10': hb10 if hb10 is not None else [[np.nan for i in range(opt_period)]] *3, # heat generation history, MWh/h
        # define (list of) gas turbine(s)
        'gt1': gt1 if gt1 is not None else ['SWPS_GT'],                                 # name
        'gt2': gt2 if gt2 is not None else [6.5],                                       # (max.) power_el, MW
        'gt3': gt3 if gt3 is not None else [11.7],                                      # (max.) power_q, MW
        'gt4': gt4 if gt4 is not None else [8.2],                                       # minimum heat load, MW
        'gt5': gt5 if gt5 is not None else [0.88],                                      # efficiency, (MWh_q+MWh_el)/MWh_g (wrt hu)
        'gt6': gt6 if gt6 is not None else [None],                                      # gas consumption/demand model
        'gt7': gt7 if gt7 is not None else [None],                                      # electricity output model
        'gt8': gt8 if gt8 is not None else [5],                                         # minimum idle time, h
        'gt9': gt9 if gt9 is not None else [53.00],                                     # wear_cost, €/h
        'gt10': gt10 if gt10 is not None else [[1 for i in range(opt_period)]],         # availability
        'gt11': gt11 if gt11 is not None else [[53.00 for i in range(opt_period)]],     # labour_cost, €/h
        'gt12': gt12 if gt12 is not None else [[np.nan for i in range(opt_period)]],    # start_up_cost, €/startup
        'gt13': gt13 if gt13 is not None else [[np.nan for i in range(opt_period)]],    # shut_down_cost, €/shut-down
        'gt14': gt14 if gt14 is not None else [[np.nan for i in range(opt_period)]],    # heat generation history, MWh/h
        # define district heating grid
        'dh1': dh1 if dh1 is not None else 'Pirmasens',                                 # name
        'dh2': dh2 if dh2 is not None else {},                                          # dict of heat entry points
        'dh3': dh3 if dh3 is not None else ['HKW', 'MHKW'],                             # heat entry point names
        'dh4': dh4 if dh4 is not None else [80, 30],                                    # minimum circulation, m³/h
        'dh5': dh5 if dh5 is not None else [[8.0 for i in range(opt_period)]] * 2,      # network pressure, bar
        'dh6': dh6 if dh6 is not None else [[100.0 for i in range(opt_period)]] * 2,    # flow temperature (Vorlauf), °C
        'dh7': dh7 if dh7 is not None else [[70.0 for i in range(opt_period)]] * 2,     # return temperature (Ruecklauf), °C
        # define municipal utility company (contracts, boilers, and gas turbines not yet assigned)
        'mu1': mu1 if mu1 is not None else 'SWPS',                                      # name
        'mu2': mu2 if mu2 is not None else [],                                          # list of conventional boilers
        'mu3': mu3 if mu3 is not None else [],                                          # list of gas turbines
        'mu4': mu4 if mu4 is not None else [],                                          # list of sourcing contracts
        'mu5': mu5 if mu5 is not None else None,                                        # fuel/gas properties object
        'mu6': mu6 if mu6 is not None else None,                                        # attached district heating grid
        'mu7': mu7 if mu7 is not None else ts_data['Waermemenge_MHKW(MW)'] + \
                                           ts_data['Waermemenge_Innenstadt(MW)']        # q_demand
    }

    ###   create overarching dictionary describing optimization setup   ###
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


def create_optimization_setup(setup_dict):
    """
    Creates optimization setup comprised of 'market_prices', 'gas_properties', 'sourcing_contracts', 'heat_boilers',
    'gas_turbines', 'district_heating_grid', and 'municipal_utility' objects as described in 'setup_dict' and
    returns MarketPrices and MunicipalUtility object

    :param dict setup_dict: dictionary providing all information to create optimization setup objects
                            (as provided by 'define_optimization_setup')
    :returns MarketPrices and MunicipalUtility  objects
    """

    # initialize lists for contracts, boilers, and turbines
    mu_contracts = []
    mu_boilers = []
    mu_turbines = []

    # loop over main keys in setup-dictionary: ['market_prices', 'gas_properties', 'district_heating_grid',
    # 'municipal_utility', 'sourcing_contracts', 'heat_boilers', 'gas_turbines']
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

    # add grid entry points to district heating grid
    for entry in grid_entries:
        grid.add_entry_point(**entry)

    # add heat sourcing contracts, boilers, and turbines to municipal utility
    for mu_c in mu_contracts:
        # initialise current price
        mu_c.update_price()
        # initialise qmin based on flow and return temperature @ entry point
        index = mu_c.qmin.index.to_series()
        mu_c.qmin = index.apply(lambda i: minimum_supply(grid, mu_c.grid_entry_point, i))
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

    # assign fuel/gas and district heating network objects
    utility.set_fuel(gas)
    utility.connect_network(grid)

    return market_prices, utility


def opex_per_h(generator, timestep):
    """
    Returns non-load-dependent OPEX per operating hour, €/h

    :param HeatBoiler|GasTurbine generator: heat generator object (HeatBoiler or GasTurbine)
    :param int timestep: time step in optimization period to evaluate opex for
    :returns float: OPEX per operating hour
    """

    # hourly OPEX for CHP gas turbine (wear dependent on operation duration)
    if isinstance(generator, GasTurbine):
        # const. wear cost + time-dependent labour cost (shift surcharges, etc.)
        opex = generator.cost_wear + generator.cost_labour[timestep]

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

    # return 0 for 'inactive' heat generators - relevant when called for cost evaluation of historic heat generation
    if quant_new <= 0:
        return 0.0, 0.0, 0.0

    # (average) OPEX and gas consumption per MWh heat for conventional heat boilers
    if isinstance(generator, HeatBoiler):
        # MWhs of gas required to generate (additional) heat (wrt lower calorific value)
        if generator.gas_demand:
            # use gas demand model ( gas consumption = f (heat generation) ) if available ...
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
        else:
            # ... otherwise use simpler efficiency model
            # 'efficiency' model is linear function with intercept==0 --> prior generation quantity not relevant
            gas_amount = (quant_new / generator.efficiency)
        # multiply with gas & co2 cost per MWh of gas (gas price refers to ho, while CO2 factor refers to hu)
        opex = gas_amount * ((gas_props.ho / gas_props.hu) * market_prices.gas_q[timestep] +
                             gas_props.co2_factor * market_prices.co2[timestep])
        # add wear cost (€/MWh_q)
        opex += (generator.cost_wear * quant_new)
        # define specific return variables
        spec_opex = opex / quant_new
        spec_gas_cons = gas_amount / quant_new
        spec_el_gen = 0.0

    # (average) OPEX, gas consumption, and electricity generation per MWh heat for gas turbines
    elif isinstance(generator, GasTurbine):
        if (quant_old + quant_new) < generator.min_load:
            # set return variables to NaN for heat load below minimum load
            spec_opex = np.nan
            spec_gas_cons = np.nan
            spec_el_gen = np.nan
        else:
            # MWhs of gas required to generate heat (wrt lower calorific value)
            if generator.gas_demand and generator.el_output:
                # use gas demand and electricity output models if available ...
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
            else:
                # ... otherwise use simpler efficiency models
                # 'efficiency' models are linear functions with intercept==0 --> prior generation quantity not relevant
                gas_amount = (quant_new / generator.power_q) * (generator.power / generator.efficiency)
                el_amount = quant_new * (generator.power_el / generator.power_q)
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

    # extract list of all heat generators capable of providing minimum heat supply --> current understanding:
    # ALL HKW operated heat generators (all boilers, GTs) are suited
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


def get_min_cost_for_interval(q_demand, sourcing_capacities, gas_props, market_prices, timestep):
    """
    Returns dictionary comprised of minimal cost (both sourcing and generation) to satisfy heat demand, how heat
    generation shall be distributed across available sources, and list of required heat generation objects

    :param float q_demand: expected heat demand for current optimization interval
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
                if row['source'] in active.keys():
                    active[row['source']] += row['capacity']
                else:
                    active[row['source']] = row['capacity']
                    active['Active_generator_objects'].append(row['generator_object'])
            # if current heat source is only needed partially
            else:
                # conventional heat boilers are fully flexible (no minimum load)
                if isinstance(row['generator_object'], HeatBoiler):
                    # re-evaluate OPEX for heat generation below maximum capacity
                    if row['source'] in active.keys():
                        # if boiler already in active heat sources, only increment needed capacity and only
                        # consider marginal variable cost driven by additional heat quantity
                        used = active[row['source']]    # already used quantity on heat boiler
                        active[row['source']] += row['capacity'] + q_demand
                        opex, gas_demand, _ = opex_per_MWh(row['generator_object'], gas_props, market_prices, timestep,
                                                           quant_old=used, quant_new=(row['capacity'] + q_demand))
                        cost_min += opex * (row['capacity'] + q_demand)
                        gas_cons += gas_demand * (row['capacity'] + q_demand)
                    else:
                        # if boiler not yet in active heat sources, consider full hourly cost, despite only
                        # partial use of maximum generation capacity
                        active[row['source']] = row['capacity'] + q_demand
                        opex1 = opex_per_h(row['generator_object'], timestep)
                        opex2, gas_demand, _ = opex_per_MWh(row['generator_object'], gas_props, market_prices, timestep,
                                                         quant_new=(row['capacity'] + q_demand))
                        cost_min += opex1 + (opex2 * (row['capacity'] + q_demand))
                        gas_cons += gas_demand * (row['capacity'] + q_demand)
                        active['Active_generator_objects'].append(row['generator_object'])
                    # store used and idle capacity on 'swing' boiler (most expensive current heat source)
                    active['Used_capacity'] = active[row['source']]
                    active['Idle_capacity'] = row['generator_object'].capacity - active[row['source']]

                # gas turbine can only be operated above certain minimal load
                elif isinstance(row['generator_object'], GasTurbine):
                    # only consider GT if required capacity on GT >= minimum heat load (MW)
                    if (row['capacity'] + q_demand) >= row['generator_object'].min_load:
                        # re-evaluate OPEX for heat generation below maximum capacity (full hourly cost,
                        # despite only partial use of maximum generation potential)
                        active[row['source']] = row['capacity'] + q_demand
                        opex1 = opex_per_h(row['generator_object'], timestep)
                        opex2, gas_demand, cogen = opex_per_MWh(row['generator_object'], gas_props, market_prices,
                                                                timestep, quant_new=(row['capacity'] + q_demand))
                        cost_min += opex1 + (opex2 * (row['capacity'] + q_demand))
                        gas_cons += gas_demand * (row['capacity'] + q_demand)
                        el_gen += cogen * (row['capacity'] + q_demand)
                        active['Active_generator_objects'].append(row['generator_object'])
                        # store used and idle capacity on 'swing' GT (most expensive current heat source)
                        active['Used_capacity'] = active[row['source']]
                        active['Idle_capacity'] = row['generator_object'].power_q - active[row['source']]
                    else:
                        # 'reset' remaining heat demand if below minimum heat load of gas turbine
                        q_demand += row['capacity']

                # heat sourcing from waste incineration plant only feasible above min technical limit (min. pump speed)
                elif isinstance(row['generator_object'], SourcingContract):
                    # only consider MHKW if required capacity from MHKW >= minimal technical limit (MW)
                    if (row['capacity'] + q_demand) >= row['generator_object'].qmin[timestep]:
                        active[row['source']] = row['capacity'] + q_demand
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


def minimize_generation_cost(municipal_utility, sourcing_mode, market_prices, optimization_period,
                             preceding_setup=[], hist_eval=False):
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

        # retrieve minimum amount of heat to be supplied by SWPS to ensure grid stability
        # (due to min circulation volume by SWPS), MWh
        if hist_eval:
            min_supply = minimum_supply(municipal_utility.network, 'HKW', t, municipal_utility.q_demand[t])
        else:
            min_supply = minimum_supply(municipal_utility.network, 'HKW', t)
        # ensure that heat demand is ALWAYS equal / greater than required minimum supply
        demand = max(min_supply, municipal_utility.q_demand[t])

        # get sorted DataFrame of available heat capacities for respective heat generation mode and timestep
        capas = get_available_heat_capacities(sourcing_mode, min_supply, municipal_utility.fuel, market_prices, t)

        # calculate minimum cost required to satisfy heat demand
        ops_data = get_min_cost_for_interval(demand, capas, municipal_utility.fuel, market_prices, t)

        ###   include switching cost WITHIN same operating mode   ###
        # i.e., switching cost incurred by demand-driven shut-down/start-up of heat generators and NOT cost associated
        # with switching from mode w/o GT to mode w/ GT or vice versa

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


def assess_switching_period(current_cost_line, alternative_cost_line, current_set_up, alternative_set_up,
                            prev_gt_benefit=None, mpc_eval=True):
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
    :param boolean mpc_eval: indicates whether this evaluation is used in a MPC optimization or not
    :returns boolean switch: flag whether to switch profitably or not
    :returns int timesteps: number of time steps for which switching is/is not profitable
    :returns float cost: switching cost incurred by switching between modes
    """

    # initialize parameters
    switch = False              # switching flag
    first_neg = True            # flag to indicate when accumulated benefit trend reverses, i.e., acc. benefit decreases
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
                if mpc_eval:
                    # return only initial switching cost for MPC optimizations, as switching back will be included in
                    # evaluation at later time step (MPC only extracts very first time step at each iteration)
                    return switch, tmax, cost1
                else:
                    # for non-MPC optimizations, return sum of switching cost (as 'tmax' time steps will be implemented
                    # in final optimization output without explicit evaluation of switching back at a later stage)
                    return switch, tmax, (cost1 + cost2)

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


def optimize_operating_modes(operating_mode_woGT, operating_mode_wGT, gt_active=False, startup_cost_included=False,
                             prev_gt_benefit=None, mpc_eval=True):
    """
    Returns a DataFrame of optimized heat generation modes and associated cost

    :param pd.DataFrame operating_mode_woGT: optimized heat generation for each time step based on mode w/o GT
    :param pd.DataFrame operating_mode_wGT: optimized heat generation for each time step based on mode w/ GT
    :param boolean gt_active: flag whether gas turbine is currently active
    :param boolean startup_cost_included: flag whether gas turbine start-up cost are already included in minimized cost
                                          for mode w/ GT
    :param float prev_gt_benefit: cumulative previous benefit from current gas turbine operation
    :param boolean mpc_eval: indicates whether this evaluation is used in a MPC optimization or not
    :returns pd.DataFrame: DataFrame with optimized heat generation conditions (based on mode 1 and 2) for each time step
    """

    # initialize DataFrame with optimized operating conditions
    optimized = pd.DataFrame(columns=operating_mode_woGT.columns[:-1])

    # initialize current best AND alternative operating mode for initial time step
    if gt_active:
        current_best = operating_mode_wGT.copy()
        alternative = operating_mode_woGT.copy()
    else:
        current_best = operating_mode_woGT.copy()
        alternative = operating_mode_wGT.copy()
        # re-adjusted 'Min_cost' for optimized mode w/ GT if start up cost are already included in first time step
        if startup_cost_included:
            alternative.loc[0, 'Min_cost'] -= switching_cost([], 0, [gt for gt in alternative['Active_generator_objects'][0]
                                                             if isinstance(gt, GasTurbine)], 0)

    # initialize previous GT benefit parameters
    gt_benefit = prev_gt_benefit if prev_gt_benefit else [0.0, 0.0]

    t = 0
    # loop over all time steps in operating_modes
    while t < len(current_best.index):
        # check whether current operating mode is still more profitable
        if current_best['Min_cost'][t] <= alternative['Min_cost'][t]:
            # append current_best operating conditions to optimized conditions for respective time step
            optimized = optimized.append(current_best.iloc[t, :-1], ignore_index=True)
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
                                                               alternative_setup, prev_gt_benefit=gt_benefit,
                                                               mpc_eval=mpc_eval)
            else:
                switch, period, cost = assess_switching_period(current_cost, alternative_cost, current_setup,
                                                               alternative_setup, mpc_eval=mpc_eval)
            # loop over length of evaluated 'switching' period
            for dt in range(period+1):
                if switch:
                    if dt == 0:
                        # reset previously accumulated gt benefit
                        gt_benefit = [0, 0]
                        # add switching cost to first interval in switching period
                        switching_interval = alternative.iloc[t, :-1]
                        switching_interval['Min_cost'] += cost
                        optimized = optimized.append(switching_interval, ignore_index=True)
                    else:
                        optimized = optimized.append(alternative.iloc[t, :-1], ignore_index=True)
                else:
                    optimized = optimized.append(current_best.iloc[t, :-1], ignore_index=True)
                    if gt_active:
                        # update cumulative and max benefit from CURRENT GT operation
                        gt_benefit[1] += alternative['Min_cost'][t] - current_best['Min_cost'][t]
                        gt_benefit[0] = max(gt_benefit[0], gt_benefit[1])
                t += 1

    return optimized


def minimum_supply(dh_grid, entry_point, timestep, q_demand=None):
    """
    Returns minimum amount of heat to be supplied to the district heating grid as a result of the temperature spread
    between flow and return temperature and minimum required flow rate (due to minimum pump speed)

    :param DistrictHeatingGrid dh_grid: district heating grid object with flow + return temperature and minimum flow rate
                                        for several heat entry/supply points
    :param str entry_point: specification of relevant heat entry point
    :param int timestep: time step in optimization period to evaluate minimal heat supply to the grid for
    :param float q_demand: historical heat supply for the respective time step
    :returns float: minimum heat supply requirement
    """

    # initialize fall-back parameters if provided grid parameters are erroneous, i.e. negative temperature spread
    if entry_point == 'HKW':
        t_flow_fallback = 94.6           # °C, median of historical HKW time series
        t_return_fallback = 74.1         # °C, median of historical HKW time series
    else:
        t_flow_fallback = 89.2           # °C, median of historical MHKW time series
        t_return_fallback = 66.6         # °C, median of historical MHKW time series

    ###   derive average 'constant' water properties (for p and T ranges)   ###
    # convert grid p and T to SI units
    p = 1e5 * dh_grid.entry_points[entry_point]['pressure'][timestep]                      # bar to Pa
    t_flow = 273.15 + dh_grid.entry_points[entry_point]['temp_flow'][timestep]             # °C to K
    t_return = 273.15 + dh_grid.entry_points[entry_point]['temp_return'][timestep]         # °C to K
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

    ###   evaluate minimum heat requirement   ###
    # temperature difference between inflow and return flow of heating grid, K
    delta_temp = t_flow - t_return
    # minimum required heat supply to district heating grid (associated with minimum pump speed), MWh/h
    min_stability_demand = (dh_grid.entry_points[entry_point]['min_circulation'] / 3600) * rho * cp * delta_temp / 1000
    min_stability_demand = round(min_stability_demand, 1)

    if q_demand:
        # return minimum of historical heat supply to the grid AND 'minimum_supply' to account for occasions where
        # lower supply than predicted by 'minimum_supply' was sufficient (only relevant for comparisons with
        # historical data and not applicable in forecasting mode), MWh/h
        return min(q_demand, min_stability_demand)
    else:
        return min_stability_demand


def mpc_optimization(municipal_utility, market_prices, fc_input, datetime_index, evaluation_period, mpc_horizon,
                     out_file_gt, out_file_opt, histeval=False, live_updates=False):
    """
    Run heat generation/sourcing cost optimization as model-predictive control implementation

    :param MunicipalUtility municipal_utility: municipal utility object (with heat demand, connected grid, etc.)
    :param MarketPrices market_prices: market prices object with electricity, co2, gas, etc. prices as time series
    :param pd.DataFrame fc_input: DataFrame of required forecast(s) input, i.e. historical time series, temperature etc.
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

    ########################################   PREPARATION   ########################################

    # test (exemplary) whether length of provided MarketPrices and MunicipalUtility time series are long
    # enough to accommodate evaluation_period + mpc_horizon (to account for receding mpc horizon)
    req_length = evaluation_period + mpc_horizon
    if (len(market_prices.el_spot) < req_length) or (len(municipal_utility.q_demand) < req_length):
        raise IndexError("Evaluation period + optimisation period exceed length of provided time series data")

    # create continuous output/log files (to be updated in every mpc loop)
    # 1) for gas turbine operations
    gt_log_columns = ['Datum', 'GT Start', 'GT Ende', 'Preis, EUR/MWh', 'Gewinn, EUR', 'Gesamtgewinn, EUR']
    gt_log_template = '{:^20}|{:^20}|{:^20}|{:^20}|{:^20}|{:^20}|\n'
    gt_log_announcement = False
    gt_log_profit = 0
    with open(out_file_gt, 'w') as f:
        f.write('####################   LOGFILE EINSATZPLANUNG GAS TURBINE   ####################\n\n')
        f.write(gt_log_template.replace(':', ':-').replace('|', '+').format('', '', '', '', '', ''))
        f.write(gt_log_template.format(*gt_log_columns))
        f.write(gt_log_template.replace(':', ':-').replace('|', '+').format('', '', '', '', '', ''))
    # 2) for optimised heat generation outlook
    with open(out_file_opt, 'w') as f:
        f.write('####################   LOGFILE OPTIMIERUNG GESAMT-WAERMEERZEUGUNG   ####################\n')
    # open log files to follow optimisation progress in 'real' time
    if live_updates:
        webbrowser.open(out_file_gt)
        webbrowser.open(out_file_opt)

    ########################################   FORECAST INITIATION   ########################################

    if histeval:
        # create empty DataFrame
        forecasts_out = pd.DataFrame()
    else:
        # initialise DataFrame for ('mpc_horizon' ahead) forecasts
        forecasts_out = pd.DataFrame(index=datetime_index, columns=fc_input.columns.drop(['Aussentemperatur']))

        # specify SARIMA(X) forecast models
        season = 24                  # periods (hours) per seasonal cycle
        refit_interval = 24 * 60     # periods after which models will be refitted
        models = {'Waermeeinspeisung (MW)': {
            'model_params': {'arima': {'order': (1, 0, 2), 'seasonal_order': (1, 1, 1, 24)}, 'fourier': (8766, 1)},
            'regr': ['Aussentemperatur (degC)'],
            'seasons_to_use': 365},
            'Temp Vorlauf (degC)': {
                'model_params': {'arima': {'order': (1, 0, 0), 'seasonal_order': (2, 0, 1, 24)}, 'fourier': (0, 0)},
                'regr': ['Waermeeinspeisung (MW)', 'Aussentemperatur (degC)'],
                'seasons_to_use': 21},
            'Temp Ruecklauf (degC)': {
                'model_params': {'arima': {'order': (2, 1, 1), 'seasonal_order': (1, 0, 1, 24)}, 'fourier': (0, 0)},
                'regr': [],
                'seasons_to_use': 21},
            'MHKW Temp Vorlauf (degC)': {
                'model_params': {'arima': {'order': (1, 0, 1), 'seasonal_order': (1, 1, 1, 24)}, 'fourier': (0, 0)},
                'regr': ['Waermeeinspeisung (MW)', 'Aussentemperatur (degC)'],
                'seasons_to_use': 14},
            'MHKW Temp Ruecklauf (degC)': {
                'model_params': {'arima': {'order': (2, 1, 1), 'seasonal_order': (1, 1, 1, 24)}, 'fourier': (0, 0)},
                'regr': [],
                'seasons_to_use': 14}}
        fitting_params = {'method': 'statespace', 'low_memory': True, 'cov_type': 'none',
                          'method_kwargs': {'minimize_kwargs': {'method': 'powell', 'options': {'maxiter': 250}}}}

        # (potentially re-)set frequency of DateTimeIndex of time series data (needed for SARIMA(X) models)
        ts = fc_input.copy()
        ts.index.freq = ts.index.inferred_freq

        # initialize SARIMA(X) models
        start = datetime_index[0]
        for var in models.keys():
            if var == 'Waermeeinspeisung (MW)':
                # extract and condition full data history for var and regr
                hl_hist = ts[[var]].copy()
                hl_regr = ts[models[var]['regr']].copy()
                hl_regr = create_SARIMAX_model.add_daycharacteristics(hl_regr)
                _, exog_data = FourierFeaturizer(*models[var]['model_params']['fourier']).fit_transform(hl_hist)
                exog_data.set_index(hl_hist.index, inplace=True)
                hl_regr = pd.concat([hl_regr, exog_data], axis=1)
                # extract only relevant history (assumes hourly spacing of DataFrames)
                s = hl_hist.index.get_loc(start) - models[var]['seasons_to_use'] * season
                e = hl_hist.index.get_loc(start)
                hl_model = ARIMA(hl_hist[s: e], exog=hl_regr[s: e], **models[var]['model_params']['arima'])
                hl_model = hl_model.fit(**copy.deepcopy(fitting_params))
            if var == 'Temp Vorlauf (degC)':
                # extract and condition full data history for var and regr
                ft_hist = ts[[var]].copy()
                ft_regr = ts[models[var]['regr']].copy()
                # extract only relevant history (assumes hourly spacing of DataFrames)
                s = ft_hist.index.get_loc(start) - models[var]['seasons_to_use'] * season
                e = ft_hist.index.get_loc(start)
                ft_model = ARIMA(ft_hist[s: e], exog=ft_regr[s: e], **models[var]['model_params']['arima'])
                ft_model = ft_model.fit(**copy.deepcopy(fitting_params))
            if var == 'Temp Ruecklauf (degC)':
                # extract and condition full data history for var and regr
                rt_hist = ts[[var]].copy()
                # extract only relevant history (assumes hourly spacing of DataFrames)
                s = rt_hist.index.get_loc(start) - models[var]['seasons_to_use'] * season
                e = rt_hist.index.get_loc(start)
                rt_model = ARIMA(rt_hist[s: e], **models[var]['model_params']['arima'])
                rt_model = rt_model.fit(**copy.deepcopy(fitting_params))
            if var == 'MHKW Temp Vorlauf (degC)':
                # extract and condition full data history for var and regr
                mhkw_ft_hist = ts[[var]].copy()
                mhkw_ft_regr = ts[models[var]['regr']].copy()
                # extract only relevant history (assumes hourly spacing of DataFrames)
                s = mhkw_ft_hist.index.get_loc(start) - models[var]['seasons_to_use'] * season
                e = mhkw_ft_hist.index.get_loc(start)
                mhkw_ft_model = ARIMA(mhkw_ft_hist[s: e], exog=mhkw_ft_regr[s: e],
                                      **models[var]['model_params']['arima'])
                mhkw_ft_model = mhkw_ft_model.fit(**copy.deepcopy(fitting_params))
            if var == 'MHKW Temp Ruecklauf (degC)':
                # extract and condition full data history for var and regr
                mhkw_rt_hist = ts[[var]].copy()
                # extract only relevant history (assumes hourly spacing of DataFrames)
                s = mhkw_rt_hist.index.get_loc(start) - models[var]['seasons_to_use'] * season
                e = mhkw_rt_hist.index.get_loc(start)
                mhkw_rt_model = ARIMA(mhkw_rt_hist[s: e], **models[var]['model_params']['arima'])
                mhkw_rt_model = mhkw_rt_model.fit(**copy.deepcopy(fitting_params))


    ########################################   MPC OPTIMISATION   ########################################

    # whether gas turbine has been active in previous time step
    gt_active = False           # start w/o active GT
    gt_benefit = None           # previous profit from current GT operation
    ops1_last_setup = []        # initialise heat generation setups from preceding time step
    ops2_last_setup = []

    # loop over all time steps to optimise for
    for t in range(evaluation_period):

        # create "updated" copies of municipal utility and market prices objects for mpc horizon (optimisation period)
        prices = market_prices.create_copy(start=t, stop=t + mpc_horizon)
        mu = municipal_utility.create_copy(start=t, stop=t + mpc_horizon)
        # whether gas turbine start up cost are already included in optimized mode w/ GT
        startup_cost_included = False  # default: add (additional) start-up cost

        # offset forecasts
        #offset = 0.25
        #mu.q_demand[:] *= 1 + offset
        # # incorporate grid temperature forecasts
        # mu.network.entry_points['HKW']['temp_flow'][:] *= 1 + offset
        # mu.network.entry_points['HKW']['temp_return'][:] *= 1 + offset
        # # incorporate MHKW grid temperature forecasts
        # mu.network.entry_points['MHKW']['temp_flow'][:] *= 1 + offset
        # mu.network.entry_points['MHKW']['temp_return'][:] *= 1 + offset

        # incorporate forecasts (if not in histeval mode)
        if not histeval:
            # create SARIMA(X) forecasts
            hl = hl_model.forecast(mpc_horizon, exog=hl_regr.loc[datetime_index[t : t + mpc_horizon]])
            # incorporate heat load forecast in flow temperature regressor data
            ft_regr.loc[hl.index, 'Waermeeinspeisung (MW)'] = hl
            mhkw_ft_regr.loc[hl.index, 'Waermeeinspeisung (MW)'] = hl
            ft = ft_model.forecast(mpc_horizon, exog=ft_regr.loc[datetime_index[t : t + mpc_horizon]])
            mhkw_ft = mhkw_ft_model.forecast(mpc_horizon, exog=mhkw_ft_regr.loc[datetime_index[t: t + mpc_horizon]])
            rt = rt_model.forecast(mpc_horizon)
            mhkw_rt = mhkw_rt_model.forecast(mpc_horizon)
            # incorporate heat load forecast
            mu.q_demand[:] = hl.values
            # incorporate grid temperature forecasts
            mu.network.entry_points['HKW']['temp_flow'][:] = ft.values
            mu.network.entry_points['HKW']['temp_return'][:] = rt.values
            # incorporate MHKW grid temperature forecasts
            mu.network.entry_points['MHKW']['temp_flow'][:] = mhkw_ft.values
            mu.network.entry_points['MHKW']['temp_return'][:] = mhkw_rt.values
            # add forecasts to output DataFrame (every mpc_horizon time steps)
            if t % mpc_horizon == 0:
                forecasts_out.loc[datetime_index[t : t + mpc_horizon]] = \
                    np.array([hl.values, ft.values, rt.values, mhkw_ft.values, mhkw_rt.values]).T

        # incorporate gas turbine idle time requirement and update start-up/shut-down cost
        gts = [gt.name for gt in municipal_utility.gas_turbines]
        for gt in municipal_utility.gas_turbines:
            if (not gt_active) and (gt.get_idle_time() < gt.idle_period):
                # only for currently not active gas turbines: if current idle time since last active operation is
                # shorter than required idle_period, flag gas turbine as unavailable for remaining time steps
                remaining = gt.idle_period - gt.get_idle_time()
                mu.gas_turbines[gts.index(gt.name)].available[0: remaining] = 0
            # update gas turbine start-up and shut-down cost
            mu.gas_turbines[gts.index(gt.name)].set_startup_cost(municipal_utility.fuel, prices)
            mu.gas_turbines[gts.index(gt.name)].set_shutdown_cost()

        # incorporate technical supply minimum for external sourcing contracts (depend on flow and return temperature
        # at respective grid entry point); supply maximum is static and does not need to be updated
        cs = [c.name for c in municipal_utility.contracts]
        for contract in municipal_utility.contracts:
            index = mu.contracts[cs.index(contract.name)].qmin.index.to_series()
            mu.contracts[cs.index(contract.name)].qmin = index.apply(lambda i: minimum_supply(mu.network,
                                                         mu.contracts[cs.index(contract.name)].grid_entry_point, i))

        # define heat sources for both heat generation modes: mode1 w/o GT, mode2 w/ GT
        sources_mode1 = []
        sources_mode1.extend(mu.boilers)
        sources_mode1.extend(mu.contracts)
        sources_mode2 = sources_mode1.copy()
        sources_mode2.extend(mu.gas_turbines)

        # create profit lines for both heat generation modes
        ops1 = minimize_generation_cost(mu, sources_mode1, prices, mpc_horizon, preceding_setup=ops1_last_setup)
        ops2 = minimize_generation_cost(mu, sources_mode2, prices, mpc_horizon, preceding_setup=ops2_last_setup)
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

        #####---------------    CONTINUOUS OUTPUT   ---------------#####

        # populate continuous output/log files every 'mpc_horizon' time steps
        if (t % mpc_horizon == 0) or (t == (evaluation_period - 1)):
            # 1) for gas turbine operations
            with open(out_file_gt, 'a') as f:
                if gt_log_announcement:
                    # reprint table header if structure was 'disrupted' by potential GT activation announcement
                    f.write(gt_log_template.replace(':', ':-').replace('|', '+').format('', '', '', '', '', ''))
                    f.write(gt_log_template.format(*gt_log_columns))
                    f.write(gt_log_template.replace(':', ':-').replace('|', '+').format('', '', '', '', '', ''))
                    gt_log_announcement = False
                if t > 0:
                    # print GT summary from previous optimisation period
                    if t < (evaluation_period - 1):
                        gt_log_last_period = mpc_results['SWPS_GT'][t - mpc_horizon: t]
                    else:
                        gt_log_last_period = mpc_results['SWPS_GT'][-mpc_horizon: ]
                    if gt_log_last_period.sum() > 0:
                        gt_log_last_period = gt_log_last_period[gt_log_last_period > 0]
                        gt_log_start = datetime_index[gt_log_last_period.index[0]].strftime('%H:%M')
                        gt_log_end = datetime_index[gt_log_last_period.index[-1] + 1].strftime('%H:%M')
                        gt_log_price = sum(market_prices.el_spot[gt_log_last_period.index] * \
                                       mpc_results['Electricity_generation'][gt_log_last_period.index]) / \
                                       mpc_results['Electricity_generation'][gt_log_last_period.index].sum()
                        gt_log_inc_profit = sum(market_prices.get_el_remun(gt_log_last_period.index) * \
                                            mpc_results['Electricity_generation'][gt_log_last_period.index])
                        gt_log_profit += gt_log_inc_profit
                        f.write(gt_log_template.format(datetime_index[gt_log_last_period.index[0]].strftime('%d.%m.%Y'),
                                                       gt_log_start, gt_log_end, round(gt_log_price, 2),
                                                       round(gt_log_inc_profit, 2), round(gt_log_profit, 2)))
                    else:
                        f.write(gt_log_template.format(datetime_index[gt_log_last_period.index[0]].strftime('%d.%m.%Y'),
                                                       '-', '-', '-', '-', round(gt_log_profit, 2)))
                if (opt['SWPS_GT'].sum() > 0) and (t < (evaluation_period - 1)):
                    # announce potentially profitable GT operation and required break even price
                    gt_log_active_indices = opt[opt['SWPS_GT'] > 0].index
                    # derive difference between optimized state (incl. initial switching cost) and optimized w/o GT
                    gt_log_delta_cost = ops1['Min_cost'][gt_log_active_indices].sum() - \
                                        opt['Min_cost'][gt_log_active_indices].sum()
                    # subtract GT shut-down cost (if not still active in last time step)
                    if gt_log_active_indices[-1] < opt.index[-1]:
                        sc = switching_cost([sources_mode2[-1]], gt_log_active_indices[-1], [], gt_log_active_indices[-1])
                        if sc < gt_log_delta_cost:
                            gt_log_delta_cost -= sc
                    gt_log_ave_price = sum(market_prices.el_spot[t + gt_log_active_indices].values * \
                                       opt['Electricity_generation'][gt_log_active_indices].values) / \
                                       opt['Electricity_generation'][gt_log_active_indices].sum()
                    gt_log_delta_price = gt_log_delta_cost / opt['Electricity_generation'][gt_log_active_indices].sum()
                    gt_log_break_even = gt_log_ave_price - gt_log_delta_price

                    start = datetime_index[t + gt_log_active_indices[0]].strftime('%d.%m.%Y-%H:%M')
                    end = datetime_index[t + gt_log_active_indices[-1] + 1].strftime('%d.%m.%Y-%H:%M')
                    duration = gt_log_active_indices[-1] - gt_log_active_indices[0] + 1
                    f.write('\n###---------- Potentiell profitabler GT Einsatz: {} - {} ----------###\n\n'.format(
                            start, end))
                    f.write('Dauer: {:.0f} h\nGrenzpreis: {:.1f} EUR/MWh\n'.format(duration, gt_log_break_even))
                    f.write('\n###---------------------------------------------------------------------------------'
                            '-----------###\n\n')
                    gt_log_announcement = True

            # 2) for optimised heat generation outlook
            if t != (evaluation_period - 1):
                # summarise all relevant data for printing in DataFrame
                df = pd.DataFrame({'Datum, Uhrzeit': [d.strftime('%d.%m.%Y-%H:%M') for d in
                                                      datetime_index[t: t + mpc_horizon]],
                                   'Strompreis, EUR/MWh': prices.el_spot.values,
                                   'Waermebedarf, MWh': mu.q_demand.values,
                                   'MHKW Bezug, MWh': opt['MHKW_ToP'].values,
                                   'Kessel 4, MWh': opt['Kessel4'].values,
                                   'Kessel 5, MWh': opt['Kessel5'].values,
                                   'Kessel 6, MWh': opt['Kessel6'].values,
                                   'Gasturbine, MWh': opt['SWPS_GT'].values}).set_index('Datum, Uhrzeit')
                with open(out_file_opt, 'a') as f:
                    f.write('\n\n####################   {:d}h Ausblick von {:s} Uhr   ####################\n'.format(
                            mpc_horizon, datetime_index[t].strftime('%d.%m.%Y-%H:%M')))
                    f.write('\nBisheriger Gasverbrauch: {:,.1f} MWh (Heizwert)\n\n'.format(mpc_results['Gas_consumption'][:t].sum()))
                    f.write(tabulate(df, headers='keys', floatfmt=".1f", tablefmt='psql'))

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

        ####---------------    UPDATE SARIMA(X) MODELS   ---------------#####

        # incorporate actual values in forecast models (if not in histeval mode)
        if not histeval:
            if (t > 0) and (t % refit_interval == 0):
                # re-initialize SARIMA(X) models (to avoid increasing length of historical fitting period)
                start = datetime_index[t+1]
                for var in models.keys():
                    if var == 'Waermeeinspeisung (MW)':
                        s = hl_hist.index.get_loc(start) - models[var]['seasons_to_use'] * season
                        e = hl_hist.index.get_loc(start)
                        hl_model = ARIMA(hl_hist[s: e], exog=hl_regr[s: e], **models[var]['model_params']['arima'])
                        hl_model = hl_model.fit(**copy.deepcopy(fitting_params))
                    if var == 'Temp Vorlauf (degC)':
                        s = ft_hist.index.get_loc(start) - models[var]['seasons_to_use'] * season
                        e = ft_hist.index.get_loc(start)
                        ft_model = ARIMA(ft_hist[s: e], exog=ft_regr[s: e], **models[var]['model_params']['arima'])
                        ft_model = ft_model.fit(**copy.deepcopy(fitting_params))
                    if var == 'Temp Ruecklauf (degC)':
                        s = rt_hist.index.get_loc(start) - models[var]['seasons_to_use'] * season
                        e = rt_hist.index.get_loc(start)
                        rt_model = ARIMA(rt_hist[s: e], **models[var]['model_params']['arima'])
                        rt_model = rt_model.fit(**copy.deepcopy(fitting_params))
                    if var == 'MHKW Temp Vorlauf (degC)':
                        s = mhkw_ft_hist.index.get_loc(start) - models[var]['seasons_to_use'] * season
                        e = mhkw_ft_hist.index.get_loc(start)
                        mhkw_ft_model = ARIMA(mhkw_ft_hist[s: e], exog=mhkw_ft_regr[s: e],
                                              **models[var]['model_params']['arima'])
                        mhkw_ft_model = mhkw_ft_model.fit(**copy.deepcopy(fitting_params))
                    if var == 'MHKW Temp Ruecklauf (degC)':
                        s = mhkw_rt_hist.index.get_loc(start) - models[var]['seasons_to_use'] * season
                        e = mhkw_rt_hist.index.get_loc(start)
                        mhkw_rt_model = ARIMA(mhkw_rt_hist[s: e], **models[var]['model_params']['arima'])
                        mhkw_rt_model = mhkw_rt_model.fit(**copy.deepcopy(fitting_params))
            else:
                # append time series history to existing SARIMA(X) models
                hl_model = hl_model.append(endog=ts[['Waermeeinspeisung (MW)']].loc[[datetime_index[t]]],
                                           exog=hl_regr.loc[[datetime_index[t]]])
                ft_regr.loc[datetime_index[t], 'Waermeeinspeisung (MW)'] = ts.loc[datetime_index[t],
                                                                                  'Waermeeinspeisung (MW)']
                mhkw_ft_regr.loc[datetime_index[t], 'Waermeeinspeisung (MW)'] = ts.loc[datetime_index[t],
                                                                                       'Waermeeinspeisung (MW)']
                ft_model = ft_model.append(endog=ts[['Temp Vorlauf (degC)']].loc[[datetime_index[t]]],
                                           exog=ft_regr.loc[[datetime_index[t]]])
                rt_model = rt_model.append(endog=ts[['Temp Ruecklauf (degC)']].loc[[datetime_index[t]]])
                mhkw_ft_model = mhkw_ft_model.append(endog=ts[['MHKW Temp Vorlauf (degC)']].loc[[datetime_index[t]]],
                                                     exog=mhkw_ft_regr.loc[[datetime_index[t]]])
                mhkw_rt_model = mhkw_rt_model.append(endog=ts[['MHKW Temp Ruecklauf (degC)']].loc[[datetime_index[t]]])

    # round results to 2 decimal places
    mpc_results = mpc_results.astype(float).round(2)
    wogt = wogt.astype(float).round(2)
    wgt = wgt.astype(float).round(2)
    forecasts_out = forecasts_out.astype(float).round(2)

    return mpc_results, wogt, wgt, forecasts_out


def simple_optimization(municipal_utility, market_prices, evaluation_period, histeval=False):
    """
    Run heat generation/sourcing cost optimization as non-MPC implementation - very useful for faster screening of
    longer (historical) evaluation_periods

    This faster method has some shortcomings, however its optimisation result is rather conservative and offers
    potentially more upside potential (to be 'confirmed' with MPC optimization)
        - GT idle time violations are not proactively suppressed; however, violations are summarized at the end
        - GT start-up/shut-down cost and MHKW supply minimum are not updated during optimization; however, as this
          optimization method only applies to historical time series this is no issue (all data known at beginning)
        - MHKW sourcing price is not updated based on total sourced volume; hence potential volume discounts are not
          considered and will only take effect in MPC optimization

    :param MunicipalUtility municipal_utility: municipal utility object (with heat demand, connected grid, etc.)
    :param MarketPrices market_prices: market prices object with electricity, co2, gas, etc. prices as time series
    :param int evaluation_period: total number of time steps to be evaluated/optimized
    :param boolean histeval: flag whether evaluation is based on historical values
    :returns pd.DataFrames: holistically optimised heat generation across modes w/ and w/o GT ('mpc_results'),
                            as well as internally optimised modes w/ GT ('wgt') and w/o GT ('wogt')
    """

    # test (exemplary) whether length of provided MarketPrices and MunicipalUtility time series are long
    # enough to accommodate evaluation_period
    if (len(market_prices.el_spot) < evaluation_period) | (len(municipal_utility.q_demand) < evaluation_period):
        raise IndexError("Evaluation period exceed length of provided time series data")


    # define heat sources for both heat generation modes: mode1 w/o GT, mode2 w/ GT
    sources_mode1 = []
    sources_mode1.extend(municipal_utility.boilers)
    sources_mode1.extend(municipal_utility.contracts)
    sources_mode2 = sources_mode1.copy()
    sources_mode2.extend(municipal_utility.gas_turbines)

    # create profit lines for both heat generation modes
    wogt = minimize_generation_cost(municipal_utility, sources_mode1, market_prices, evaluation_period, hist_eval=histeval)
    wgt = minimize_generation_cost(municipal_utility, sources_mode2, market_prices, evaluation_period, hist_eval=histeval)

    # optimize operating modes
    opt = optimize_operating_modes(wogt, wgt, mpc_eval=False)

    # check whether external heat sourcing is compliant with ALL external obligations and limitations
    sourcing_compliance = postprocessing.check_annual_sourcing_compliance(opt, municipal_utility.contracts)

    # check whether ALL idle times between subsequent GT deployments are compliant with GT requirements
    idle_time_violations = postprocessing.check_gt_idle_time_compliance(opt, municipal_utility.gas_turbines)

    return opt, wogt, wgt, sourcing_compliance, idle_time_violations


####################     OPTIMISATION TEST     ####################

if __name__ == '__main__':

    # define length of overall and individual optimisation periods
    opt_period = 24 * 5      # number of time steps to run/evaluate (max 360)
    mpc_horizon = 24         # number of time steps to optimise per MPC optimisation run (MPC horizon)
    start = '2018-02-04'     # define specific start date (only relevant for test case 2 with 'historical' test data)

    # define which optimization method to run
    simple_opt = True
    mpc_opt = True

    # define continuous output/log files for mpc optimisation
    root = Path(__file__).parent
    out_file_gt = os.path.join(root, '..\\..\\data\\output\\optimization\\GasTurbine_planning_' + str(mpc_horizon)
                               + 'h_' + dt.datetime.now().strftime("%Y%m%d-%H%M") + '.txt')
    out_file_opt = os.path.join(root, '..\\..\\data\\output\\optimization\\HeatGeneration_optimisation_'
                                + str(mpc_horizon) + 'h_' + dt.datetime.now().strftime("%Y%m%d-%H%M") + '.txt')

    # define test case: 1) with all dummy objects,
    #                   2) with 'define_optimization_setup' and 'test_data.csv' data for heat load and prices
    test_case = 2   # 1 or 2

    if test_case == 1:

        # create market price object (dummy)
        prices = MarketPrices.dummies(opt_period + mpc_horizon)
        # arbitrarily increase electricity price to trigger GT operation
        prices.el_spot[10:20] = 3 * prices.el_spot[10: 20]

        # create gas properties object (dummy)
        gas = GasProperties.dummies()

        # create district heating grid object (dummy) with HKW and MHKW heat entry points
        grid = DistrictHeatingGrid.dummies()
        grid.add_entry_point_dummy('HKW', opt_period + mpc_horizon)
        grid.add_entry_point_dummy('MHKW', opt_period + mpc_horizon)
        grid.entry_points['MHKW']['min_circulation'] = 30.0

        ##########   define swps object   ##########

        # create municipal utility company object (dummy)
        swps = MunicipalUtility.dummies(opt_period + mpc_horizon)
        # define swps boiler set-up
        boilers = {'Kessel4': 7.5, 'Kessel5': 7.5, 'Kessel6': 4.5}
        # add conventional heat boilers
        for i in boilers:
            boiler = HeatBoiler.dummies(i, opt_period + mpc_horizon)     # create HeatBoiler object
            boiler.capacity = boilers[i]                      # overwrite dummy capacity
            swps.add_boiler(boiler)

        # add gas turbine
        swps.add_gas_turbine(GasTurbine.dummies('SWPS_GT', opt_period + mpc_horizon, gas=gas, prices=prices))

        # add MHKW heat sourcing contract
        swps.add_contract(SourcingContract.dummies(opt_period + mpc_horizon))

        # set available natural gas as fuel
        swps.set_fuel(gas)

        # connect to district heating network
        swps.connect_network(grid)

    elif test_case == 2:

        # get file location
        root = Path(__file__).parent
        file = os.path.join(root, '..\\..\\data\\input\\processed\\test_data.csv')

        data = pd.read_csv(file, index_col=0, parse_dates=True, dayfirst=True)
        data = data.loc[start:]
        setup, index = define_optimization_setup(data, opt_period + mpc_horizon)
        prices, swps = create_optimization_setup(setup)

    # load external forecast(s) required for heat load forecasting which are not explicit parts of municipal utility
    # or market prices objects, i.e. ambient temperature
    ts_ext = '..\\..\\data\\input\\processed\\fully_conditioned_timeseries.csv'
    ts_ext = pd.read_csv(os.path.join(root, ts_ext), index_col=0, parse_dates=True, dayfirst=True,
                         usecols=['Date', 'Aussentemperatur (degC)'])
    try:
        ts_ext = ts_ext.loc[index]
    except:
        # derive 'arbitrary' external forecast(s) and DateTimeIndex for dummy data
        ts_ext = ts_ext[:opt_period + mpc_horizon]
        ts_ext[:] = 10.0
        index = ts_ext.index

    ##########   run optimizations   ##########

    optimizations = []
    results = []
    results_wogt = []
    results_wgt = []

    if simple_opt:
        t1 = time.time()
        optimizations.append('Non-MPC optimization')
        res, res_wogt, res_wgt, _, _ = simple_optimization(swps, prices, opt_period, histeval=True)
        print('\nDuration: {:.2f} s'.format(time.time()-t1))
        results.append(res)
        results_wogt.append(res_wogt)
        results_wgt.append(res_wgt)
    if mpc_opt:
        t1 = time.time()
        optimizations.append('MPC optimization')
        res, res_wogt, res_wgt, _ = mpc_optimization(swps, prices, ts_ext, index, opt_period, mpc_horizon, out_file_gt,
                                                     out_file_opt, histeval=True, live_updates=True)
        print('\nDuration: {:.2f} s'.format(time.time() - t1))
        results.append(res)
        results_wogt.append(res_wogt)
        results_wgt.append(res_wgt)

    for opt in range(len(optimizations)):

        # print results
        print('###   {:s}   ###'.format(optimizations[opt]))
        print('Total amount of heat produced/sourced: %.2f MWh.' % results[opt]['Q_demand'].sum())
        print('Minimum cost to produce/source heat: %.2f €.' % results[opt]['Min_cost'].sum())
        print('Average generation/sourcing cost: %.2f €/MWh\n' % (results[opt]['Min_cost'].sum() /
                                                                  results[opt]['Q_demand'].sum()))

        # test plots
        plt.rcParams['font.size'] = 18
        fig, ax = plt.subplots(figsize=(16, 9))
        # subfigure of optimized modes
        ax.grid('both')
        ax.plot(results_wogt[opt]['Min_cost'], '.-', color='tab:gray')
        ax.plot(results_wgt[opt]['Min_cost'], '.-', color='tab:blue')
        ax.plot(results[opt]['Min_cost'], '--', color='tab:red', lw=2.0)
        ax.legend(['Optimised mode excl. GT', 'Optimised mode incl. GT', 'Overall optimised heat generation'],
                     loc='upper center')
        ax.set_ylabel('Generation cost (€/h)')
        ax.set_title(optimizations[opt])
        plt.tight_layout()
        plt.show()
