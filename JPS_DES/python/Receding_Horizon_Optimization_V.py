#!/usr/bin/env python
# coding: utf-8
# Old python method, no longer used. 
# In[58]:


import numpy as np
from scipy.optimize import *
import matplotlib.pyplot as plt
import math
import xlrd
import time
import pandas as pd
import os
from datetime import datetime

with open('timekeep.txt','w') as outfile:
    outfile.write(str(datetime.now()) + ' started \n')


# ### Renewable

# In[59]:


class RenewableGeneration:
    
    def __init__(self, t):
        self.t = t
    
    # power generation for one solar panel
    def solar_energy(self, G, Tc):
        # Solar Panel
        # PV module: SunPower SPR-230-WHT
        # Type of cell: Mono-c-Si
        
        #File Import to read the parameters for 

        # PV module coefficients
        
        DF_PV = pd.read_csv("PV_parameters.csv", header=None )
        
        Isc_ref   = DF_PV.iloc[0,1]     # Short circuit current at SRC (standard rating condition) [A]
        Voc_ref   = DF_PV.iloc[1,1]     # Open circuit voltage at SRC [V]
        Imp_ref   = DF_PV.iloc[2,1]     # Current at maximum power point [A]
        Vmp_ref   = DF_PV.iloc[3,1]     # Voltage at maximum power point [V]
        alpha_sc  = DF_PV.iloc[4,1]     # Temperature coefficient for short circuit current [A/K]
        a_ref     = DF_PV.iloc[5,1]     # ideality factor parameter at SRC [eV]
        Il_ref    = DF_PV.iloc[6,1]     # Light current at SRC [A]
        Io_ref    = DF_PV.iloc[7,1]     # Diode reverse saturation current at SRC [A]
        Rs_ref    = DF_PV.iloc[8,1]     # Series resistance at SRC [Ohm]
        Rsh_ref   = DF_PV.iloc[9,1]     # Shunt resistance at SRC [Ohm]
        Tc_ref    = DF_PV.iloc[10,1]    # Cell temperature at SRC [K]
        G_ref     = DF_PV.iloc[11,1]    # Radiation at SRC conditions [W/m^2]
        Eg_ref    = DF_PV.iloc[12,1]    # Material band gap at SRC [eV]

        
     

        # Physical constant
        k = 8.6173324e-5      # Boltzmann constant [eV/K]
        
        # Convert Tc from [C] to [K]
        Tc = Tc + 273.15
        # Ideality factor for operating conditons [eV]
        a = a_ref*Tc/Tc_ref
        # Material band gat for operating conditons [eV]
        Eg = Eg_ref*(1 - 0.0002677*(Tc - Tc_ref))
        # Diode reverse saturation current for operating conditions [A]
        Io = Io_ref*(Tc/Tc_ref)**3*np.exp((Eg_ref/Tc_ref - Eg/Tc)/k)
        # Series resistance at operating conditions [Ohm] 
        Rs = Rs_ref*np.ones(self.t)
        # Light current at operating conditions [A]
        Il = G/G_ref*(Il_ref + alpha_sc*(Tc - Tc_ref))
        # Shunt resistant at operating conditions [Ohm]
        Rsh = G_ref*Rsh_ref/(G + 1e-10)
        
        Imp = np.zeros(self.t)
        Vmp = np.zeros(self.t)
        
        for i in range(0, self.t):
            # x = V + IRs
            P = lambda x: -(x - (Il[i] - Io[i]*(math.exp(x/a[i]) - 1) - x/Rsh[i])*Rs[i])*(Il[i] - Io[i]*(math.exp(x/a[i]) - 1) - x/Rsh[i])
            xopt = fminbound(P, 0, 50)
            # Current at maximum power point (MPP) for operating condition
            Imp[i] = max(0, Il[i] - Io[i]*(math.exp(xopt/a[i]) - 1) - xopt/Rsh[i])
            # Voltage at MPP for operating condition
            Vmp[i] = max(0, xopt - Imp[i]*Rs[i])
        
        # Power at MPP for operating condition
        return Imp*Vmp
    
    
    # power generation for one wind turbine
    def wind_energy(self, v, T):
        # Wind Turbine
        # In this model a Xzeres442SR wind turbine (rated Power 10kW) is used
        
        # Turbine parameters
        P = 101325                         # Station pressure [Pa]
        R = 287.058                        # Specific gas constant for air [J/(kg*K)]
        roh_standard = 1.2041              # Density of air at standard conditions, 20 degreeC [kg/m^3] 
        v_cutin = 0.2                      # Cut in wind speed [m/s]
        v_cutout = 24                      # Cut out wind speed [m/s]
        v_rated = 11                       # Rated wind speed [m/s]
        P_rated = 10000                    # Rated Power at rated speed [W]
        d_blade = 7.2                      # Blade diameter [m] 
        A_blades = math.pi*(d_blade/2)**2  # Area swept by rotor blades [m^2]
        
        # Convert T from [C] to [K]
        T = T + 273.15
        
        roh_wind = np.zeros(self.t)
        P_wind = np.zeros(self.t)

        for i in range(0, self.t):
            # Air density from ideal gas [kg/m^3]
            roh_wind[i] = P/(R*T[i])
            # Calculate Cp, the power coefficient, by using the data given for the rated wind speed constant
            Cp = 2*P_rated/(roh_standard*A_blades*v_rated**3)
            # Calculate Wind Power
            if v[i] <= v_cutin:
                P_wind[i] = 0
            elif v[i] > v_cutin and v[i] <= v_rated:
                P_wind[i] = 0.5*roh_wind[i]*A_blades*v[i]**3*Cp
            elif v[i] > v_rated and v[i] < v_cutout:
                P_wind[i] = 0.5*roh_wind[i]*A_blades*v_rated**3*Cp
            elif v[i] >= v_cutout:
                P_wind[i] = 0
        
        return P_wind
    
    
    
    


# ### Residential

# In[60]:


class ResidentialHousehold:
    
    # scaling factor
    NUMBER = 60
    
    # electricity price
    def electricity_bill_structure(self, c0, c1):
        self.c0 = c0
        self.c1 = c1
      
    # discomfort cost scaling factor
    def discomfort_factor(self, cd):
        self.cd = cd
    
    # cutoff point for renewable generation and grid purchase [kWh]
    def cutoff_load(self, cutoff):
        self.cutoff = cutoff
    
    # The following data should be provided to fully define a resident:
    #
    # t: time span
    # app: no. of appliances
    # bat: no. of batteries
    #
    # flex[t, app]: a binary matrix defining the feasible period to schedule each appliance
    # 
    # high[app], low[app]: the maximum and minimum power for each appliance during the feasible period
    # bcap: battery capacity
    # 
    # sche[t, app]: original schedule for each appliance
    #
    # unwill[app]: unwillingness to shift the load for each appliance
    # 
    # aggr[t]: aggregate load of other users for each time slot. This will be changed during the game theory iterations
    #
    # q0[bat]: initial energy of batteries
   
    def __init__(self, t, app, bat):
        self.t = t
        self.app = app
        self.bat = bat
    
    def set_flexibility(self, flex):
        self.flex = flex.transpose().flatten()
        
    def set_power_limit(self, high, low):
        self.Pmin = np.kron(low, np.ones(self.t))
        self.Pmax = np.kron(high, np.ones(self.t))
    
    def set_bat_cap(self, bcap):
        self.bcap = bcap
    
    def set_schedule(self, sche):
        self.sche = sche.transpose().flatten()
        
    def set_unwilling(self, unwill):
        self.w = np.kron(unwill, np.ones(self.t))
    
    def get_aggregate_load(self, aggr):
        self.aggr = aggr
    
    def set_q0(self, q0):
        self.q0 = q0

    # lower and upper bounds for decision variables
    def set_var_bounds(self):
        self.xlow = np.concatenate([self.Pmin*self.flex, np.zeros((self.t+1)*self.bat)])
        self.xlow[self.t*self.app::(self.t+1)] = self.q0
        self.xup = np.concatenate([self.Pmax*self.flex, self.bcap*np.ones((self.t+1)*self.bat)])
        self.xup[self.t*self.app::(self.t+1)] = self.q0

    # aggregation matrices
    def define_aggregator(self):
        # slicing point for appliances and batteries
        self.sp = self.app*self.t
        # difference matrix to convert SOC to load
        pos = np.hstack([np.zeros((self.t, 1)), np.identity(self.t)])
        neg = np.hstack([-np.identity(self.t), np.zeros((self.t, 1))])
        self.diff = pos + neg
        # aggregate load on time
        self.Ta = np.hstack([np.identity(self.t) for i in range(0, self.app)]) # for appliances only
        self.Tb = np.hstack([self.diff for i in range(0, self.bat)])           # for batteries only
        self.Tx = np.hstack([self.Ta, self.Tb])                                # for all devices
        # aggregate load on appliance
        self.Aa = np.kron(np.identity(self.app), np.ones(self.t))                 # Aa * x[:sp]
        self.Ax = np.hstack([self.Aa, np.zeros([self.app, self.bat*(self.t+1)])]) # Ax * x
    
    # calculate hourly load in [kWh] with scaling
    def compute_load(self, x):
        load = self.NUMBER*np.dot(self.Tx, x)
        dload_dx = self.NUMBER*self.Tx
        return {'load': load, 'dload_dx': dload_dx}
    
    def compute_electricity_bill_discomfort_cost(self, x):
        res = self.compute_load(x)
        load = res['load']
        dload_dx = res['dload_dx']
        
        fraction = np.sum(load)/(np.sum(load) + np.sum(self.aggr))
        
        # when total load is below max. renewable generation
        cost_below = self.c0*(load + self.aggr)**2
        dcost_below_dx = (self.c0*2*(load + self.aggr)).reshape((self.t, 1))*dload_dx
        
        bill_below = (load + self.aggr <= self.cutoff)*fraction*cost_below
        dbill_below_dx = (load + self.aggr <= self.cutoff).reshape((self.t, 1))*fraction*dcost_below_dx
        
        # when total load is above max. renewable generation
        cost_above = self.c0*self.cutoff**2 + self.c1*(load + self.aggr - self.cutoff)**2
        dcost_above_dx = (self.c1*2*(load + self.aggr - self.cutoff)).reshape((self.t, 1))*dload_dx
        
        bill_above = (load + self.aggr >  self.cutoff)*fraction*cost_above
        dbill_above_dx = (load + self.aggr >  self.cutoff).reshape((self.t, 1))*fraction*dcost_above_dx
        
        # total discomfort cost with scaling
        discomfort = self.NUMBER*self.cd*np.sum(self.w*(x[:self.sp] - self.sche)**2)
        ddiscomfort_dx = self.NUMBER*np.concatenate([self.cd*self.w*2*(x[:self.sp] - self.sche), np.zeros(self.bat*(self.t+1))])
        
        return {'bill': np.sum(bill_below + bill_above),
                'dbill_dx': np.dot(np.ones(self.t), dbill_below_dx + dbill_above_dx),
                'discomfort': discomfort,
                'ddiscomfort_dx': ddiscomfort_dx}
    
    # objective function
    def objfunction(self, x):
        res = self.compute_electricity_bill_discomfort_cost(x)
        return res['bill'] + res['discomfort']
    
    # gradient of objective function
    def obj_der(self, x):
        res = self.compute_electricity_bill_discomfort_cost(x)
        return res['dbill_dx'] + res['ddiscomfort_dx']
    
    
    # set starting point of game theory iteration
    def set_starting_point(self):
        self.prev = np.concatenate([self.sche, np.kron(self.q0, np.ones(self.t+1))])
    
    def residential_optimize(self):           
        # variable bounds
        var_bounds = Bounds(self.xlow, self.xup)
                   
        # for SLSQP:
        # inequality constraint: positive total load (appliances + batteries) for each hour
        ineq_cons = {'type': 'ineq',
                     'fun': lambda x: np.dot(self.Tx, x),
                     'jac': lambda x: self.Tx}
        # equality constraint: daily load = scheduled for each appliance
        eq_cons = {'type': 'eq',
                   'fun': lambda x: np.dot(self.Ax, x) - np.dot(self.Aa, self.sche),
                   'jac': lambda x: self.Ax}
        
        # initial guess
        x0 = self.prev

        # minimize total cost
        self.res = minimize(self.objfunction, x0, method = 'SLSQP', 
                            jac = self.obj_der, 
                            bounds = var_bounds, constraints = [eq_cons, ineq_cons], 
                            options={'ftol': 0.01, 'maxiter': 200, 'disp': True})
        
        # check whether x has changed from previous iteration
        if all(abs((self.res.x - self.prev)/(self.prev + 1e-10)*100) < 0.1):
            self.changed = False
        else:
            self.changed = True
        
        # update solution
        self.prev = self.res.x

    # report the total load for each time slot after optimization
    def report_load(self):
        return self.compute_load(self.res.x)['load']
    
    # report whether x has changed from previous iteration
    def report_change(self):
        return self.changed
    
    # rolling the value of q0
    def rolling_q0(self):
        self.q0 = self.res.x[self.sp+1::self.t+1]
    
    # rolling the solution for receding horizon optmization
    def rolling_prev_solution(self):
        self.next = np.zeros_like(self.prev, dtype=float)
        for i in range(0, self.app):
            start = self.t*i
            self.next[start:start+self.t-1] = self.prev[start+1:start+self.t]
            self.next[start+self.t-1] = self.prev[start]
        for i in range(0, self.bat):
            start = self.sp+(self.t+1)*i
            self.next[start:start+self.t] = self.prev[start+1:start+self.t+1]
            self.next[start+self.t] = self.prev[start]
        self.prev = self.next
    
    # rolling the parameters
    def rolling_para_values(self):
        flex_new = np.zeros_like(self.flex, dtype=bool)
        sche_new = np.zeros_like(self.sche, dtype=float)
        for i in range(0, self.app):
            start = self.t*i        
            flex_new[start:start+self.t-1] = self.flex[start+1:start+self.t]
            flex_new[start+self.t-1] = self.flex[start]
            sche_new[start:start+self.t-1] = self.sche[start+1:start+self.t]
            sche_new[start+self.t-1] = self.sche[start]
        self.flex = flex_new
        self.sche = sche_new
    


# ### Commercial

# In[61]:


class Building_Temperature_Dynamics:
    
    # for a commercial building with 3600[m^2] floor area
    
    # model parameters:
    C1 = 9.356e5   #[kJ/C]
    C2 = 2.970e6   #[kJ/C]
    C3 = 6.695e5   #[kJ/C]
    K1 = 16.48     #[kW/C]
    K2 = 108.5     #[kW/C]
    K3 = 5         #[kW/C]
    K4 = 30.5      #[kW/C]
    K5 = 23.04     #[kW/C]
    
    # model dynamics:
    A = np.array([[-1/C1*(K1+K2+K3+K5),  1/C1*(K1+K2),  1/C1*K5     ],
                  [ 1/C2*(K1+K2),       -1/C2*(K1+K2),  0           ],
                  [ 1/C3*K5,             0,            -1/C3*(K4+K5)]])
    B = np.array([[-1/C1],
                  [ 0   ],
                  [ 0   ]])
    C = np.array([[1/C1*K3, 1/C1, 1/C1],
                  [0,       1/C2, 0   ],
                  [1/C3*K4, 0,    0   ]])
    
    # model representation:
    # x = [T1, T2, T3].T
    # x0, X = [x0, ..., xN-1, xN]
    
    # u = [u].T
    # U = [u0, ..., uN-1]
    
    # w = [delta1, delta2, delta3].T
    # W = [w0, ..., wN-1]
    
    
    # tspan: total time horizon [s]
    # dt: time interval [s]
    # N: no. of time intervals
    
    def __init__(self, tspan, dt):
        self.tspan = tspan
        self.dt = dt
        self.N = int(tspan/dt)
    
    
    # linear dynamical system
    # continuous: dxdt = Ax + Bu + Cw
    # discrete:   x = Ad*xi + Bd*ui + Cd*wi
    
    def N_time_steps(self, x0, U, W):
        # discretize
        self.Ad = self.A*self.dt + np.identity(3)
        self.Bd = self.B*self.dt
        self.Cd = self.C*self.dt

        U = U.reshape((1, self.N))
        W = W.reshape((3, self.N))
        
        X = np.zeros((3, self.N))
        dXdXi = np.zeros((3*self.N, 3*self.N))
        dXdU = np.zeros((3*self.N, self.N))
        dXdW = np.zeros((3*self.N, 3*self.N))
        
        xi = x0
        for i in range(0, self.N):
            x = np.dot(self.Ad, xi) + np.dot(self.Bd, U[:,i]) + np.dot(self.Cd, W[:,i])
            X[:,i] = x
            
            dXdXi[3*i:3*(i+1),3*i:3*(i+1)] = self.Ad
            dXdU[3*i:3*(i+1),i:i+1] = self.Bd
            dXdW[3*i:3*(i+1),3*i:3*(i+1)] = self.Cd
            
            for j in range(i-1, -1, -1):
                dXdXi[3*i:3*(i+1),3*j:3*(j+1)] = np.dot(dXdXi[3*i:3*(i+1),3*(j+1):3*(j+2)], self.Ad)
                dXdU[3*i:3*(i+1),j:j+1] = np.dot(dXdXi[3*i:3*(i+1),3*(j+1):3*(j+2)], self.Bd)
                dXdW[3*i:3*(i+1),3*j:3*(j+1)] = np.dot(dXdXi[3*i:3*(i+1),3*(j+1):3*(j+2)], self.Cd)
                
            xi = x
        
        return {'X': X, 'dXdXi': dXdXi, 'dXdU': dXdU, 'dXdW': dXdW,
                'T1': X[0,:],  'dT1dU': dXdU[::3,:], 'dT1dx0': dXdXi[::3,:3], 
                'xn': X[:,-1], 'dxndU': dXdU[-3:,:], 'dxndx0': dXdXi[-3:,:3]}
    


# In[62]:


class CommercialBuilding:
    
    # scaling factor
    NUMBER = 1
    
    # electricity price
    def electricity_bill_structure(self, c0, c1):
        self.c0 = c0
        self.c1 = c1
    
    # cutoff point for renewable generation and grid purchase [kWh]
    def cutoff_load(self, cutoff):
        self.cutoff = cutoff
    
    
    # t_horizon: time horizon [s]
    # dt_d: time interval for dynamics evaluation [s]
    # dt_c: time interval for control action [s]
    # dt_i: time interval for disturbance information [s]
    # dt_f: time interval for objective function calculation [s]
    def __init__(self, t_horizon, dt_d, dt_c, dt_i, dt_f):
        
        self.t_horizon = t_horizon
        self.dt_d = dt_d
        self.dt_c = dt_c
        self.dt_i = dt_i
        self.dt_f = dt_f
        
        self.Nd = int(t_horizon/dt_d)
        self.Nc = int(t_horizon/dt_c)
        self.Ni = int(t_horizon/dt_i)
        self.Nf = int(t_horizon/dt_f)
        
        self.mc = int(dt_c/dt_d)
        self.mi = int(dt_i/dt_d)
        self.mf = int(dt_f/dt_d)
        
        # when calculating objevtive function, left multiply by S to convert from Nd to Nf
        self.S = np.kron(np.identity(self.Nf), 1/self.mf*np.ones(self.mf))
        # when calculating derivative wrt control actions, right multiply by A to convert from Nd to Nc
        self.A = np.kron(np.identity(self.Nc), np.ones((self.mc, 1)))
        
        self.bld = Building_Temperature_Dynamics(t_horizon, dt_d)
    
    
    # aggregate load of other agents
    def get_aggregate_load(self, aggr):
        self.aggr = aggr
    
    # initial temperature vector of building
    def get_x0(self, x0):
        self.x0 = x0
    
    # disturbance information is provided every dt_i seconds (in total Ni values)
    def get_disturbance(self, W):
        self.W = np.kron(W, np.ones(self.mi))
    
    # control actions (electrolyzer current and fuel cell current) are decided every dt_c seconds (in total Nc values)
    # decision variable Uc is an Nc-vector of cooling duties
    
    # electricity load is calculated every dt_f seconds (in total Nf values)
    # load is scaled and converted from [kJ] to [kWh] 
    def compute_load(self, Uc):
        Ud = np.kron(Uc, np.ones(self.mc))
        load = self.NUMBER*np.dot(self.S, Ud)*self.dt_f/3600
        dload_dUc = self.NUMBER*np.dot(self.S, self.A)*self.dt_f/3600
        return {'load': load, 'dload_dUc': dload_dUc}
    
    # electricity bill is calculated every dt_f seconds (in total Nf values)
    # but only return the total bill and its derivative
    def compute_electricity_bill(self, Uc):
        res = self.compute_load(Uc)
        load = res['load']
        dload_dUc = res['dload_dUc']
        
        fraction = np.sum(load)/(np.sum(load) + np.sum(self.aggr))
        dfraction_dUc = np.sum(self.aggr)/(np.sum(load) + np.sum(self.aggr))**2*np.dot(np.ones(self.Nf), dload_dUc)
        
        # when total load is below max. renewable generation
        cost_below = self.c0*(load + self.aggr)**2
        dcost_below_dUc = (self.c0*2*(load + self.aggr)).reshape((self.Nf, 1))*dload_dUc
        
        bill_below = (load + self.aggr <= self.cutoff)*fraction*cost_below
        dbill_below_dUc = (load + self.aggr <= self.cutoff).reshape((self.Nf, 1))*                           (np.dot(cost_below.reshape((self.Nf, 1)), dfraction_dUc.reshape((1, self.Nc))) +                            fraction*dcost_below_dUc)
        
        # when total load is above max. renewable generation
        cost_above = self.c0*self.cutoff**2 + self.c1*(load + self.aggr - self.cutoff)**2
        dcost_above_dUc = (self.c1*2*(load + self.aggr - self.cutoff)).reshape((self.Nf, 1))*dload_dUc
        
        bill_above = (load + self.aggr >  self.cutoff)*fraction*cost_above
        dbill_above_dUc = (load + self.aggr >  self.cutoff).reshape((self.Nf, 1))*                           (np.dot(cost_above.reshape((self.Nf, 1)), dfraction_dUc.reshape((1, self.Nc))) +                            fraction*dcost_above_dUc)
        
        return {'bill': np.sum(bill_below + bill_above),
                'dbill_dUc': np.dot(np.ones(self.Nf), dbill_below_dUc + dbill_above_dUc)}
        
    
    def objfunction(self, Uc):
        res = self.compute_electricity_bill(Uc)
        return res['bill']
    
    def obj_der(self, Uc):
        res = self.compute_electricity_bill(Uc)
        return res['dbill_dUc']
    
    
    # comfortable range for room temperature [C]
    # lower and upper bounds for T1 is provided every t_length seconds
    def get_T1_range(self, t_length, T1_low, T1_high):
        self.Tlow = np.kron(T1_low, np.ones(int(t_length/self.dt_d)))
        self.Thigh = np.kron(T1_high, np.ones(int(t_length/self.dt_d)))
    
    def ineq_cons_fun(self, Uc):
        Ud = np.kron(Uc, np.ones(self.mc))
        res = self.bld.N_time_steps(self.x0, Ud, self.W)
        lower = res['T1'] - self.Tlow
        upper = self.Thigh - res['T1']
        return np.concatenate([lower, upper])
    
    def ineq_cons_jac(self, Uc):
        Ud = np.kron(Uc, np.ones(self.mc))
        res = self.bld.N_time_steps(self.x0, Ud, self.W)
        return np.vstack([np.dot(res['dT1dU'], self.A), -np.dot(res['dT1dU'], self.A)])
    
    
    # set starting point of game theory iteration
    # average cooling duty for buildings in Singapore = 74[W/m^2]
    def set_starting_point(self):
        self.prev = 74*3600/1000*np.ones(self.Nc)
    
    def commercial_optimize(self):
        # variable bounds
        var_bounds = Bounds(np.zeros(self.Nc), 500*np.ones(self.Nc))
        
        # for SLSQP:
        ineq_cons = {'type': 'ineq',
                     'fun': lambda Uc: self.ineq_cons_fun(Uc),
                     'jac': lambda Uc: self.ineq_cons_jac(Uc)}
        
        # initial guess
        Uc_0 = self.prev

        # minimize total cost
        self.res = minimize(self.objfunction, Uc_0, method = 'SLSQP', 
                            jac = self.obj_der, 
                            bounds = var_bounds, constraints = ineq_cons, 
                            options={'ftol': 1.14e-3, 'maxiter': 300, 'disp': True})
        
        # check whether x has changed from previous iteration
        if all(abs((self.res.x - self.prev)/(self.prev + 1e-10)*100) < 0.1):
            self.changed = False
        else:
            self.changed = True
        # update solution
        self.prev = self.res.x

    # report the total load for each time slot after optimization
    def report_load(self):
        return self.compute_load(self.res.x)['load']
    
    # report whether x has changed from previous iteration
    def report_change(self):
        return self.changed
    
    # rolling the value of x0
    def rolling_x0(self, W_actual):
        Ud = np.kron(self.res.x, np.ones(self.mc))
        W_actual = np.kron(W_actual, np.ones(self.mi))
        self.x0 = self.bld.N_time_steps(self.x0, Ud, W_actual)['X'][:,self.mc-1]
    
    # rolling the solution for receding horizon optmization
    def rolling_prev_solution(self):
        self.next = np.zeros_like(self.prev, dtype=float)
        self.next[0:self.Nc-1] = self.prev[1:self.Nc]
        self.next[self.Nc-1] = self.prev[0]
        self.prev = self.next
    


# ### Industrial

# In[63]:


class Electrolyzer_Temperature_Dynamics:
    
    # chlor-alkali process:
    # 2Na+(aq) + 2Cl-(aq) + 2H2O -> 2Na+(aq) + 2OH-(aq) + Cl2(g) + H2(g)
    
    # physical constants:
    F = 96485.34           #[A*s/mol]
    z = 2                  # no. of electrons transferred per reaction
    R = 8.315              #[J/K/mol]
    Vstd = 0.0224136       #[m^3/mol]
    
    # reaction information: (25 degreeC and 1 bar @ standard conditions)  
    dG0 = 237e3            #[J/mol]
    dH = 286e3             #[J/mol]
    
    # I-U curve parameters:
    r1 = 8.05e-5           #[Ohm*m^2]
    r2 = -2.5e-7           #[Ohm*m^2/C]
    s = 0.185              #[V]
    t1 = -0.1002           #[m^2/A]
    t2 = 8.424             #[m^2*C/A]
    t3 = 247.3             #[m^2*C^2/A]
    
    # Faraday efficiency parameters: (80 degreeC @ HYSOLAR) 
    f1 = 250e2             #[A^2/m^4]
    f2 = 0.98              #[1]
    
    # UA_HX parameters:
    h_cond = 7             #[W/C]
    h_conv = 0.02          #[W/C/A]
    
    # operation parameters:
    A = 0.25               #[m^2]
    nc = 21                # no. of cells in series
    Ct = 625e3             #[J/C]
    Rt = 0.167             #[C/W]
    taut = Ct*Rt           #[s]
    Qcw = 0.6/3600         #[m^3/s] flow rate of cooling water
    Tcwi = 14.5            #[C]
    Ccw = 4.18e3*1e3*Qcw   #[W/C]
    
    
    # tspan: total time horizon [s]
    # dt: time interval [s]
    # N: no. of time intervals
    
    def __init__(self, tspan, dt):
        self.tspan = tspan
        self.dt = dt
        self.N = int(tspan/dt)
    
    
    # Independent variables:
    # I: current [A]
    # Ti: initial temperature [C]
    
    # Disturbance:
    # Ta: ambient temperature [C]
    
    # Dependent variables:
    # n: molar flow rate of H2 (also Cl2) [mol/s]
    # P: power of electrolyzer [W]
    # T: final temperature [C]
    
    def one_time_step(self, I, Ti, Ta):
        # res is a dictionary containing:
        # n, dn_dI, dn_dTi
        # P, dP_dI, dP_dTi
        # T, dT_dI, dT_dTi
        # all derivatives in res are total derivatives
        res = dict()
        
        DI_DTi = 0
        DI_DI = 1
        
        DTi_DI = 0
        DTi_DTi = 1
        
        # eq.(9)
        etaF = (I/self.A)**2/(self.f1 + (I/self.A)**2)*self.f2
        detaF_dI = self.f1*self.f2*1/(self.f1 + (I/self.A)**2)**2*2*(I/self.A)*1/self.A
        # etaF
        DetaF_DI = detaF_dI*DI_DI
        DetaF_DTi = detaF_dI*DI_DTi
        
        # eq.(10)
        n = etaF*self.nc*I/(self.z*self.F)
        dn_dI = etaF*self.nc/(self.z*self.F)
        dn_detaF = self.nc*I/(self.z*self.F)
        # n
        Dn_DI = dn_dI*DI_DI + dn_detaF*DetaF_DI
        Dn_DTi = dn_dI*DI_DTi + dn_detaF*DetaF_DTi
        
        res['n'] = n
        res['dn_dI'] = Dn_DI
        res['dn_dTi'] = Dn_DTi
        
        # Assume linear dependency of dG on T
        dG = self.dG0 + (228477-self.dG0)*(Ti-25)/(80-25)
        ddG_dTi = (228477-self.dG0)/(80-25)
        # dG
        DdG_DI = ddG_dTi*DTi_DI
        DdG_DTi = ddG_dTi*DTi_DTi
        
        # eq.(5)
        Urev = dG/(self.z*self.F)
        dUrev_ddG = 1/(self.z*self.F)
        # Urev
        DUrev_DI = dUrev_ddG*DdG_DI
        DUrev_DTi = dUrev_ddG*DdG_DTi
        
        # eq.(8)
        U = Urev + (self.r1 + self.r2*Ti)*I/self.A + self.s*math.log((self.t1 + self.t2/Ti + self.t3/Ti**2)*I/self.A + 1)
        dU_dUrev = 1
        dU_dI = (self.r1 + self.r2*Ti)/self.A + self.s*((self.t1 + self.t2/Ti + self.t3/Ti**2)/self.A)/((self.t1 + self.t2/Ti + self.t3/Ti**2)*I/self.A + 1)
        dU_dTi = self.r2/self.A*I + self.s*(-self.t2/Ti**2*(I/self.A)-2*self.t3/Ti**3*(I/self.A))/((self.t1 + self.t2/Ti + self.t3/Ti**2)*I/self.A + 1)
        # U
        DU_DI = dU_dUrev*DUrev_DI + dU_dI*DI_DI + dU_dTi*DTi_DI
        DU_DTi = dU_dUrev*DUrev_DTi + dU_dI*DI_DTi + dU_dTi*DTi_DTi
        
        # P = nc*UI
        P = self.nc*U*I
        dP_dI = self.nc*U
        dP_dU = self.nc*I
        # P
        DP_DI = dP_dI*DI_DI + dP_dU*DU_DI
        DP_DTi = dP_dI*DI_DTi + dP_dU*DU_DTi
        
        res['P'] = P
        res['dP_dI'] = DP_DI
        res['dP_dTi'] = DP_DTi
        
        # eq.(24)
        UA = self.h_cond + self.h_conv*I
        dUA_dI = self.h_conv
        # UA
        DUA_DI = dUA_dI*DI_DI
        DUA_DTi = dUA_dI*DI_DTi
        
        # eq.(22)
        a = 1/self.taut + self.Ccw/self.Ct*(1 - math.exp(-UA/self.Ccw))
        da_dUA = self.Ccw/self.Ct*(-math.exp(-UA/self.Ccw))*(-1/self.Ccw)
        # a
        Da_DI = da_dUA*DUA_DI
        Da_DTi = da_dUA*DUA_DTi
        
        # eq.(12)
        Utn = self.dH/(self.z*self.F) # eq.(6)
        etae = Utn/U
        detae_dU = -Utn/U**2
        # etae
        Detae_DI = detae_dU*DU_DI
        Detae_DTi = detae_dU*DU_DTi
        
        # eq.(23)
        b = self.nc*U*I*(1-etae)/self.Ct + Ta/self.taut + self.Ccw*self.Tcwi/self.Ct*(1 - math.exp(-UA/self.Ccw))
        db_dUA = self.Ccw*self.Tcwi/self.Ct*(-math.exp(-UA/self.Ccw))*(-1/self.Ccw)
        db_dI = self.nc*U*(1-etae)/self.Ct
        db_dU = self.nc*I*(1-etae)/self.Ct
        db_detae = -self.nc*U*I/self.Ct
        # b
        Db_DI = db_dUA*DUA_DI + db_dI*DI_DI + db_dU*DU_DI + db_detae*Detae_DI
        Db_DTi = db_dUA*DUA_DTi + db_dI*DI_DTi + db_dU*DU_DTi + db_detae*Detae_DTi
        
        # T = Ti + (-a*Ti + b)dt
        T = Ti + (-a*Ti+b)*self.dt
        dT_da = -Ti*self.dt
        dT_db = self.dt
        dT_dTi = 1 - a*self.dt
        # T
        DT_DI = dT_da*Da_DI + dT_db*Db_DI + dT_dTi*DTi_DI
        DT_DTi = dT_da*Da_DTi + dT_db*Db_DTi + dT_dTi*DTi_DTi
        
        res['T'] = T
        res['dT_dI'] = DT_DI
        res['dT_dTi'] = DT_DTi
        
        return res
    
    
    # Independent variable:
    # I: an N-vector containing the currents at each time step [A]
    
    # Initial condition:
    # T0: starting temperature [C]
    
    # Disturbance:
    # Ta: an N-vector containing the ambient temperatures at each time step [C]
    
    # Dependent variable:
    # n: an N-vector containing the H2 molar flow rates at each time step [mol/s]
    # P: an N-vector containing the powers at each time step [W]
    # T: an N-vector containing the temperatures at each time step [C]
    
    def N_time_steps(self, I, T0, Ta):
        n = np.zeros(self.N)
        P = np.zeros(self.N)
        T = np.zeros(self.N)
        
        dndI = np.zeros((self.N, self.N))
        dPdI = np.zeros((self.N, self.N))
        dTdI = np.zeros((self.N, self.N))
        
        dndTi = np.zeros((self.N, self.N))
        dPdTi = np.zeros((self.N, self.N))
        dTdTi = np.zeros((self.N, self.N))
        
        Ti = T0
        for i in range(0, self.N):
            res = self.one_time_step(I[i], Ti, Ta[i])
            n[i] = res['n']
            P[i] = res['P']
            T[i] = res['T']
            dndI[i,i] = res['dn_dI']
            dPdI[i,i] = res['dP_dI']
            dTdI[i,i] = res['dT_dI']
            dndTi[i,i] = res['dn_dTi']
            dPdTi[i,i] = res['dP_dTi']
            dTdTi[i,i] = res['dT_dTi'] 
            
            for j in range(i-1, -1, -1):
                dndI[i,j] = dndTi[i,j+1]*dTdI[j,j]
                dPdI[i,j] = dPdTi[i,j+1]*dTdI[j,j]
                dTdI[i,j] = dTdTi[i,j+1]*dTdI[j,j]
                dndTi[i,j] = dndTi[i,j+1]*dTdTi[j,j]
                dPdTi[i,j] = dPdTi[i,j+1]*dTdTi[j,j]
                dTdTi[i,j] = dTdTi[i,j+1]*dTdTi[j,j]
            
            Ti = T[i]
            
        return {'n': n, 'P': P, 'T': T,
                'dndI': dndI, 'dPdI': dPdI, 'dTdI': dTdI,
                'dndT0': dndTi[:,0], 'dPdT0': dPdTi[:,0], 'dTdT0': dTdTi[:,0]}
        


# In[64]:


class Fuel_Cell:
    
    # hydrogen fuel cell
    # reaction based on 1 mol of H2
    
    DF_FC = pd.read_csv("FuelCell.csv", header=None )
    
    # physical constants:
    F = 96485.34           #[A*s/mol]
    z = DF_FC.iloc[0,1]           # no. of electrons transferred per reaction
    
    # polarization curve parameters:
    U0 = 33.18             #[V]
    E1 = -0.013            #[V/C]
    E2 = -1.57             #[1]
    I0 = 8.798             #[A]
    R = -2.04              #[Ohm*C]
    
    # operation parameters:
    nc     = DF_FC.iloc[1,1]              # no. of cells
    eta    = DF_FC.iloc[2,1]              # fuel utilization factor
    Tlow   = DF_FC.iloc[3,1]              #[C]
    Thigh  = DF_FC.iloc[4,1]              #[C]
    
    # track optimal operating temperature if possible:
    Ilow = Tlow**2*E1/R    #[A]
    Ihigh = Thigh**2*E1/R  #[A]
    
    
    # tspan: total time horizon [s]
    # dt: time interval [s]
    # N: no. of time intervals
    
    def __init__(self, tspan, dt):
        self.tspan = tspan
        self.dt = dt
        self.N = int(tspan/dt)
        
    
    # Simplifed Dynamic Model:
    # fuel cell operated at steady state for every time step
    
    def steady_state_operation(self, I):
        
        T = (I <= self.Ilow)*self.Tlow + (I > self.Ilow and I < self.Ihigh)*(math.sqrt(self.R*I/self.E1)) + (I >= self.Ihigh)*self.Thigh
        dT_dI = (I > self.Ilow and I < self.Ihigh)*(1/2*(self.R*I/self.E1)**(-1/2)*self.R/self.E1)
        
        U = self.U0 + self.E1*T + self.E2*math.log(I/self.I0) + self.R/T*I
        dU_dI = self.E1*dT_dI + self.E2*1/(I/self.I0)*1/self.I0 + self.R/T - self.R/T**2*dT_dI*I
        
        P = U*I
        dP_dI = U + dU_dI*I
        
        n = self.nc*I/(self.z*self.F)/self.eta
        dn_dI = self.nc/(self.z*self.F)/self.eta
        
        return {'n': n, 'dn_dI': dn_dI,
                'P': P, 'dP_dI': dP_dI,
                'T': T, 'dT_dI': dT_dI}
    
    
    # Independent variable:
    # I: an N-vector containing fuel cell currents at each time point [A]
    
    # Dependent variable:
    # n: an N-vector containing required H2 molar flow rates at each time point [mol/s]
    # P: an N-vector containing fuel cell powers at each time point [W]
    # T: an N-vector containing fuel cell temperatures at each time point [C]
    
    def N_time_steps(self, I):
        n = np.zeros(self.N)
        P = np.zeros(self.N)
        T = np.zeros(self.N)
        
        dndI = np.zeros((self.N, self.N))
        dPdI = np.zeros((self.N, self.N))
        dTdI = np.zeros((self.N, self.N))
        
        for i in range(0, self.N):     
            res = self.steady_state_operation(I[i])
            n[i] = res['n']
            P[i] = res['P']
            T[i] = res['T']
            dndI[i,i] = res['dn_dI']
            dPdI[i,i] = res['dP_dI']
            dTdI[i,i] = res['dT_dI']
        
        return {'n': n, 'dndI': dndI,
                'P': P, 'dPdI': dPdI,
                'T': T, 'dTdI': dTdI}
    


# In[65]:


class IndustrialPlant:
    
    # scaling factor
    NUMBER = 30
    
    # electricity price
    def electricity_bill_structure(self, c0, c1):
        self.c0 = c0
        self.c1 = c1
    
    # fuel cost
    def fuel_cost_factor(self, cf):
        self.cf = cf
    
    # cutoff point for renewable generation and grid purchase [kWh]
    def cutoff_load(self, cutoff):
        self.cutoff = cutoff
    
    
    # t_horizon: time horizon [s]
    # dt_d: time interval for dynamics evaluation [s]
    # dt_c: time interval for control action [s]
    # dt_i: time interval for disturbance information [s]
    # dt_f: time interval for objective function calculation [s]
    def __init__(self, t_horizon, dt_d, dt_c, dt_i, dt_f):
        
        self.t_horizon = t_horizon
        self.dt_d = dt_d
        self.dt_c = dt_c
        self.dt_i = dt_i
        self.dt_f = dt_f
        
        self.Nd = int(t_horizon/dt_d)
        self.Nc = int(t_horizon/dt_c)
        self.Ni = int(t_horizon/dt_i)
        self.Nf = int(t_horizon/dt_f)
        
        self.mc = int(dt_c/dt_d)
        self.mi = int(dt_i/dt_d)
        self.mf = int(dt_f/dt_d)
        
        # when calculating objevtive function, left multiply by S to convert from Nd to Nf
        self.S = np.kron(np.identity(self.Nf), 1/self.mf*np.ones(self.mf))
        # when calculating derivative wrt control actions, right multiply by A to convert from Nd to Nc
        self.A = np.kron(np.identity(self.Nc), np.ones((self.mc, 1)))
        
        self.el = Electrolyzer_Temperature_Dynamics(t_horizon, dt_d)
        self.fc = Fuel_Cell(t_horizon, dt_d)
    
    
    # aggregate load of other agents
    def get_aggregate_load(self, aggr):
        self.aggr = aggr
    
    # initial temperature of electrolyzer
    def get_T0(self, T0):
        self.T0 = T0
        
    # ambient temperature is provided every dt_i seconds (in total Ni values)
    def get_ambient_temperature(self, Ta):
        self.Ta = np.kron(Ta, np.ones(self.mi))
    
    # control actions (electrolyzer current and fuel cell current) are decided every dt_c seconds (in total Nc values)
    # decision variable IIc is thus a 2*Nc-vector:
    # first Nc elements correspond to electrolyzer currents, second Nc elements correspond to fuel cell currents
        
    # electricity load and H2 purchase quantity are calculated every dt_f seconds (in total Nf values)
    # load in [kWh]
    # fuel in [mol]
    def compute_load_fuel(self, IIc):
        
        # electrolyzer
        Id = np.kron(IIc[:self.Nc], np.ones(self.mc))
        res_el = self.el.N_time_steps(Id, self.T0, self.Ta)
        
        conEl = np.dot(self.S, res_el['P'])
        dconEl_dIIc = np.hstack([np.dot(np.dot(self.S, res_el['dPdI']), self.A), np.zeros((self.Nf, self.Nc))])
        
        genH2 = np.dot(self.S, res_el['n'])
        dgenH2_dIIc = np.hstack([np.dot(np.dot(self.S, res_el['dndI']), self.A), np.zeros((self.Nf, self.Nc))])
        
        # fuel cell
        Ig = np.kron(IIc[self.Nc:], np.ones(self.mc))
        res_fc = self.fc.N_time_steps(Ig)
        
        genEl = np.dot(self.S, res_fc['P'])
        dgenEl_dIIc = np.hstack([np.zeros((self.Nf, self.Nc)), np.dot(np.dot(self.S, res_fc['dPdI']), self.A)])
        
        conH2 = np.dot(self.S, res_fc['n'])
        dconH2_dIIc = np.hstack([np.zeros((self.Nf, self.Nc)), np.dot(np.dot(self.S, res_fc['dndI']), self.A)])
        
        # overall with scaling and unit conversion
        load = self.NUMBER*(conEl - genEl)*self.dt_f/1000/3600
        dload_dIIc = self.NUMBER*(dconEl_dIIc - dgenEl_dIIc)*self.dt_f/1000/3600
        
        fuel = self.NUMBER*(conH2 - genH2)*self.dt_f
        dfuel_dIIc = self.NUMBER*(dconH2_dIIc - dgenH2_dIIc)*self.dt_f
        
        return {'load': load, 'dload_dIIc': dload_dIIc,
                'fuel': fuel, 'dfuel_dIIc': dfuel_dIIc}
    
    # electricity bill and fuel (H2) cost are calculated every dt_f seconds (in total Nf values)
    # but only return the total bill and H2 cost, and their derivatives
    def compute_electricity_bill_fuel_cost(self,IIc):
        res = self.compute_load_fuel(IIc)
        load = res['load']
        dload_dIIc = res['dload_dIIc']
        fuel = res['fuel']
        dfuel_dIIc = res['dfuel_dIIc']
        
        fraction = np.sum(load)/(np.sum(load) + np.sum(self.aggr))
        dfraction_dIIc = np.sum(self.aggr)/(np.sum(load) + np.sum(self.aggr))**2*np.dot(np.ones(self.Nf), dload_dIIc)
        
        # when total load is below max. renewable generation
        cost_below = self.c0*(load + self.aggr)**2
        dcost_below_dIIc = (self.c0*2*(load + self.aggr)).reshape((self.Nf, 1))*dload_dIIc
        
        bill_below = (load + self.aggr <= self.cutoff)*fraction*cost_below
        dbill_below_dIIc = (load + self.aggr <= self.cutoff).reshape((self.Nf, 1))*                            (np.dot(cost_below.reshape((self.Nf, 1)), dfraction_dIIc.reshape((1, 2*self.Nc))) +                             fraction*dcost_below_dIIc)
        
        # when total load is above max. renewable generation
        cost_above = self.c0*self.cutoff**2 + self.c1*(load + self.aggr - self.cutoff)**2
        dcost_above_dIIc = (self.c1*2*(load + self.aggr - self.cutoff)).reshape((self.Nf, 1))*dload_dIIc
        
        bill_above = (load + self.aggr >  self.cutoff)*fraction*cost_above
        dbill_above_dIIc = (load + self.aggr >  self.cutoff).reshape((self.Nf, 1))*                           (np.dot(cost_above.reshape((self.Nf, 1)), dfraction_dIIc.reshape((1, 2*self.Nc))) +                            fraction*dcost_above_dIIc)
        
        return {'bill': np.sum(bill_below + bill_above),
                'dbill_dIIc': np.dot(np.ones(self.Nf), dbill_below_dIIc + dbill_above_dIIc),
                'H2cost': np.sum(self.cf*fuel),
                'dH2cost_dIIc': np.dot(np.ones(self.Nf), self.cf.reshape((self.Nf, 1))*dfuel_dIIc)}
    
    
    # objective function: total cost = electricity cost + fuel (H2) cost
    def objfunction(self, IIc):
        res = self.compute_electricity_bill_fuel_cost(IIc)
        return res['bill'] + res['H2cost']
    
    def obj_der(self, IIc):
        res = self.compute_electricity_bill_fuel_cost(IIc)
        return res['dbill_dIIc'] + res['dH2cost_dIIc']
    
    
    # inequality constraint 1: temperature of electropyzer within operating range ALL the time
    def ineq_cons1_fun(self, IIc):
        Id = np.kron(IIc[:self.Nc], np.ones(self.mc))
        T = self.el.N_time_steps(Id, self.T0, self.Ta)['T']
        return np.concatenate([T - 70*np.ones(self.Nd), 99.8*np.ones(self.Nd) - T])
        
    def ineq_cons1_jac(self, IIc):
        Id = np.kron(IIc[:self.Nc], np.ones(self.mc))
        jac1 = np.dot(self.el.N_time_steps(Id, self.T0, self.Ta)['dTdI'], self.A)
        jac2 = np.zeros((self.Nd, self.Nc))
        return np.vstack([np.hstack([jac1, jac2]), np.hstack([-jac1, jac2])])
    
    
    # inequality constraint 2: positive consumption of load and fuel when computing total cost (objfunction)
    def ineq_cons2_fun(self, IIc):
        res = self.compute_load_fuel(IIc)
        load = res['load']
        fuel = res['fuel']
        return np.concatenate([load, fuel])
        
    def ineq_cons2_jac(self, IIc):
        res = self.compute_load_fuel(IIc)
        dload_dIIc = res['dload_dIIc']
        dfuel_dIIc = res['dfuel_dIIc']
        return np.vstack([dload_dIIc, dfuel_dIIc])
    
    
    # equality constraint: production requirement of Cl2 at the end of t_horizon
    # production quantity
    quantity = 2500 #[mol]
    
    def eq_cons_fun(self, IIc):
        Id = np.kron(IIc[:self.Nc], np.ones(self.mc))
        nCl2 = np.sum(self.el.N_time_steps(Id, self.T0, self.Ta)['n']*self.dt_d)
        return nCl2 - self.quantity
    
    def eq_cons_jac(self, IIc):
        Id = np.kron(IIc[:self.Nc], np.ones(self.mc))
        jac1 = np.sum(np.dot(self.el.N_time_steps(Id, self.T0, self.Ta)['dndI'], self.A), axis=0)*self.dt_d
        jac2 = np.zeros(self.Nc)
        return np.concatenate([jac1, jac2])
    
    
    # set starting point of game theory iteration
    def set_starting_point(self):
        self.prev = self.quantity/self.t_horizon*2*96485.34/21*np.ones(2*self.Nc)
    
    def industrial_optimize(self):
        # variable bounds
        xlow = np.concatenate([220*np.ones(self.Nc), np.ones(self.Nc)])
        xhigh = np.concatenate([330*np.ones(self.Nc), np.inf*np.ones(self.Nc)])
        var_bounds = Bounds(xlow, xhigh)
        
        # for SLSQP:
        ineq_cons1 = {'type': 'ineq',
                      'fun': lambda IIc: self.ineq_cons1_fun(IIc),
                      'jac': lambda IIc: self.ineq_cons1_jac(IIc)}
        ineq_cons2 = {'type': 'ineq',
                      'fun': lambda IIc: self.ineq_cons2_fun(IIc),
                      'jac': lambda IIc: self.ineq_cons2_jac(IIc)}
        eq_cons = {'type': 'eq',
                   'fun': lambda IIc: self.eq_cons_fun(IIc),
                   'jac': lambda IIc: self.eq_cons_jac(IIc)}
        
        # initial guess
        IIc_0 = self.prev

        # minimize total cost
        self.res = minimize(self.objfunction, IIc_0, method = 'SLSQP', 
                            jac = self.obj_der, 
                            bounds = var_bounds, constraints = [ineq_cons1, ineq_cons2, eq_cons], 
                            options={'ftol': 0.77e-3, 'maxiter': 300, 'disp': True})
        
        # check whether x has changed from previous iteration
        if all(abs((self.res.x - self.prev)/(self.prev + 1e-10)*100) < 0.1):
            self.changed = False
        else:
            self.changed = True
        # update solution
        self.prev = self.res.x

    # report the total load for each time slot after optimization
    def report_load(self):
        return self.compute_load_fuel(self.res.x)['load']
    
    # report whether x has changed from previous iteration
    def report_change(self):
        return self.changed
    
    # rolling the value of T0
    def rolling_T0(self, Ta_actual):
        Id = np.kron(self.res.x[:self.Nc], np.ones(self.mc))
        Ta_actual = np.kron(Ta_actual, np.ones(self.mi))
        self.T0 = self.el.N_time_steps(Id, self.T0, Ta_actual)['T'][self.mc-1]
    
    # rolling the solution for receding horizon optmization
    def rolling_prev_solution(self):
        self.next = np.zeros_like(self.prev, dtype=float)
        self.next[0:self.Nc-1] = self.prev[1:self.Nc]
        self.next[self.Nc-1] = self.prev[0]
        self.next[self.Nc:2*self.Nc-1] = self.prev[self.Nc+1:2*self.Nc]
        self.next[2*self.Nc-1] = self.prev[self.Nc]
        self.prev = self.next
        


# ### Game Organizer

# In[66]:


class Organizer:
    
    def __init__(self):
        self.agent_loads = dict()
        self.agent_changes = dict()
    
    def collect_load(self, agent, load, changed):
        self.agent_loads[agent] = load
        self.agent_changes[agent] = changed
    
    def provide_load(self, agent):
        total = 0
        for a,l in self.agent_loads.items():
            if a == agent:
                continue
            total = total + l
        return total
    
    def check_termination(self):
        termination = True
        for a,c in self.agent_changes.items():
            if c:
                termination = False
                break
        return termination
    
    def rolling_load_change(self):
        for agent,load in self.agent_loads.items():
            load_next = np.zeros_like(load, dtype=float)
            load_next[0:-1] = load[1:]
            load_next[-1] = load[0]
            self.agent_loads[agent] = load_next
        for agent,change in self.agent_changes.items():
            self.agent_changes[agent] = True
    


# ### Optimization

# In[67]:


# electricity bill structure
household_below = 1/50*0.01*np.ones(24)
household_above = 1/50*0.02*np.ones(24)
business_below = 1.14*1/50*0.01*np.ones(24)
business_above = 1.14*1/50*0.02*np.ones(24)
industry_below = 0.77*1/50*0.01*np.ones(24)
industry_above = 0.77*1/50*0.02*np.ones(24)


# In[68]:



# In[69]:


# renewable generations
rg = RenewableGeneration(24)


# In[70]:


df0 = pd.read_csv('ApplianceScheduleLoad1.csv', header= None )
sche1 = np.asarray( [ [df0.iloc[t, 2+a] for a in range(11)] for t in range(72) ] )

lst0 = []
for x in range(sche1.shape[0]):
    lst1 = []
    for y in range(sche1.shape[1]):
        if (sche1[x,y] != 0):
            lst1.append('yes')
        else:
            lst1.append('0')
    lst0.append(lst1)

    
tb0 = np.asarray(lst0)
tb0 = pd.DataFrame(tb0)


# In[71]:


# 3 residential households
# 24 time slots, 11 appliances, 1 battery
rh1 = ResidentialHousehold(24, 11, 1)
rh2 = ResidentialHousehold(24, 11, 1)
rh3 = ResidentialHousehold(24, 11, 1)
rh_list = list([rh1, rh2, rh3])

#df = pd.read_csv('ApplianceScheduleLoad1.csv', header= None )
df1 = pd.read_csv('Pmin.csv', header = None)
df2 = pd.read_csv('Pmax.csv', header = None)
df3 = pd.read_csv('unwill.csv', header = None)
df4 = pd.read_csv('bcap.csv', header = None)



for r in range(0, 3):
    tb = np.asarray([ [tb0.iloc[24*r+t, a] for a in range(11)] for t in range(24) ])
    flex = (np.array(tb) == 'yes')
    rh_list[r].set_flexibility(flex)
    
    sche = np.asarray( [ [df0.iloc[24*r+t, 2+a] for a in range(11)] for t in range(24) ] )
    rh_list[r].set_schedule(sche)

    
for r in range(0, 3):
    low = np.asarray([df1.iloc[r, 1+a] for a in range(11)])
    high = np.asarray([df2.iloc[r, 1+a] for a in range(11)])
    rh_list[r].set_power_limit(high,low)
    
    unwill = np.asarray([df3.iloc[r, 1+a] for a in range(11)])
    rh_list[r].set_unwilling(unwill)
    
    bcap = df4.iloc[r, 1]
    rh_list[r].set_bat_cap(bcap)

cd = 0.003
for rh in rh_list:
    rh.electricity_bill_structure(household_below, household_above)
    rh.discomfort_factor(cd)
    rh.define_aggregator()
    rh.set_q0(0*rh.bcap)
    rh.set_var_bounds()
    rh.set_starting_point()


# In[73]:


# 1 commercial building
# plan for 24 hr with:
# temperature dynamics evaluated every 10 min
# control actions taken every hour
# disturbance information updated every hour
# total cost computed on an hourly basis
cb = CommercialBuilding(24*3600, 600, 3600, 3600, 3600)
cb_test = CommercialBuilding(24*3600, 600, 3600, 3600, 3600)

HeatSource = np.concatenate([5*np.ones(7), 25*np.ones(11), 5*np.ones(6), 5*np.ones(7), 25*np.ones(11), 5*np.ones(6)])
RoomTempLow = np.concatenate([19*np.ones(7), 21*np.ones(11), 19*np.ones(6), 19*np.ones(7), 21*np.ones(11), 19*np.ones(6)])
RoomTempHigh = np.concatenate([30*np.ones(7), 25.8*np.ones(11), 30*np.ones(6), 30*np.ones(7), 25.8*np.ones(11), 30*np.ones(6)])

cb.electricity_bill_structure(business_below, business_above)
cb.get_x0(np.array([28.3, 28.2, 28.6]))
cb.set_starting_point()


# In[74]:


# 1 industrial plant
# plan for 24 hr with:
# electrolyzer and fuel cell dynamics evaluated every 15 min
# control actions taken every hour
# ambient temperature updated every hour
# total cost computed on an hourly basis
ip = IndustrialPlant(24*3600, 900, 3600, 3600, 3600)
ip_test = IndustrialPlant(24*3600, 900, 3600, 3600, 3600)

cf = 2.31*0.002*np.ones(24)
ip.electricity_bill_structure(industry_below, industry_above)
ip.fuel_cost_factor(cf)
ip.get_T0(99)
ip.set_starting_point()


# In[75]:


# game organizer
org = Organizer()
# initialize the game
agent_list = list([rh1, rh2, rh3, cb, ip])
for a in agent_list:
    org.collect_load(a, np.zeros(24), True)


# In[76]:


# receding horizon optimization
opt_res = list()
#init Values for reading
data = pd.read_csv("WeatherInitialize.csv", header=None)
#current hour in [0 to 23]
h = 0
# weather forecast for next 24 hours
AirTempForecast = data[4].to_numpy()
WindSpeedForecast = data[6].to_numpy()
RadiationForecast = data[8].to_numpy()

# solar generation in [kWh]
sg = 30*100*rg.solar_energy(RadiationForecast, AirTempForecast)/1000
# wind generation in [kWh]
wg = 0*rg.wind_energy(WindSpeedForecast, AirTempForecast)/1000
totGen = sg + wg

for a in agent_list:
    a.cutoff_load(totGen)

# disturbance for commercial
delta1 = AirTempForecast
delta2 = 1/1000*1/2*1200*0.5*RadiationForecast
delta3 = 1/1000*3600*HeatSource[h:h+24]
W = np.vstack([delta1, delta2, delta3])
T1_low = RoomTempLow[h:h+24]
T1_high = RoomTempHigh[h:h+24]
cb.get_disturbance(W)
cb.get_T1_range(3600, T1_low, T1_high)

# disturbance for industrial
Ta = AirTempForecast
ip.get_ambient_temperature(Ta)

# sequential game
start_time = time.time()
i = 0
while not org.check_termination():
    # residential
    for rh in rh_list:
        rh.get_aggregate_load(org.provide_load(rh))
        rh.residential_optimize()
        org.collect_load(rh, rh.report_load(), rh.report_change())
    # commercial
    cb.get_aggregate_load(org.provide_load(cb))
    cb.commercial_optimize()
    org.collect_load(cb, cb.report_load(), cb.report_change())
    # industrial
    ip.get_aggregate_load(org.provide_load(ip))
    ip.industrial_optimize()
    org.collect_load(ip, ip.report_load(), ip.report_change())
    i = i + 1
    print('##### Finish Iteration: ' + str(i) + ' #####')
    if i >= 10:
        print('##### Forced Stop After 10 Iterations #####')
        break
print('--- %s seconds ---' % (time.time() - start_time))

AirTempCorrected = np.copy(AirTempForecast)
WindSpeedCorrected = np.copy(WindSpeedForecast)
RadiationCorrected = np.copy(RadiationForecast)



# ### Updated Hourly

# In[19]:


# weather forecast for next 24 hours
fore_data = pd.read_csv("WeatherForecast.csv", header=None)
# actual weather for the last hour
hist_data = pd.read_csv("WeatherActual.csv", header=None)

# current hour in [0 to 23]
h = (h + 1)%24

# weather forecast for next 24 hours
AirTempForecast = fore_data[4].to_numpy()
WindSpeedForecast = fore_data[6].to_numpy()
RadiationForecast = fore_data[8].to_numpy()

# actual weather for the last hour
AirTemp = hist_data[4].to_numpy()
WindSpeed = hist_data[6].to_numpy()
Radiation = hist_data[8].to_numpy()

# corrected weather forecast
AirTempCorrected[0] = AirTemp
WindSpeedCorrected[0] = WindSpeed
RadiationCorrected[0] = Radiation

# rolling
for rh in rh_list:
    rh.rolling_q0()
    rh.rolling_para_values()
    rh.set_var_bounds()
    rh.set_starting_point()

delta1 = AirTempCorrected
delta2 = 1/1000*1/2*1200*0.5*RadiationCorrected
delta3 = 1/1000*3600*HeatSource[h:h+24]
W_actual = np.vstack([delta1, delta2, delta3])
cb.rolling_x0(W_actual)
cb.rolling_prev_solution()

ip.rolling_T0(AirTempCorrected)
ip.rolling_prev_solution()

org.rolling_load_change()

# solar generation in [kWh]
sg = 30*100*rg.solar_energy(RadiationForecast, AirTempForecast)/1000
# wind generation in [kWh]
wg = 0*rg.wind_energy(WindSpeedForecast, AirTempForecast)/1000
totGen = sg + wg

for a in agent_list:
    a.cutoff_load(totGen)

# disturbance for commercial
delta1 = AirTempForecast
delta2 = 1/1000*1/2*1200*0.5*RadiationForecast
delta3 = 1/1000*3600*HeatSource[h:h+24]
W = np.vstack([delta1, delta2, delta3])
T1_low = RoomTempLow[h:h+24]
T1_high = RoomTempHigh[h:h+24]
cb.get_disturbance(W)
cb.get_T1_range(3600, T1_low, T1_high)

# disturbance for industrial
Ta = AirTempForecast
ip.get_ambient_temperature(Ta)
#save prelim results (aka based on initial model from OWL files and those two sets of data. )
l1 = rh1.compute_load(rh1.res.x)['load']
l2 = rh2.compute_load(rh2.res.x)['load']
l3 = rh3.compute_load(rh3.res.x)['load']
l4 = cb.compute_load(cb.res.x)['load']
l5 = ip.compute_load_fuel(ip.res.x)['load']

residential = l1+l2+l3
commercial = l4
industrial = l5
gridGen = l1+l2+l3+l4+l5 - totGen
renewableGen = totGen
np.savetxt("prelimresult.csv",[residential, commercial, industrial, renewableGen, gridGen], delimiter=",")

# residential
x1 = rh1.res.x
ref1 = np.dot(rh1.Ta, rh1.sche)
opt1 = np.dot(rh1.Ta, x1[:rh1.sp])
load1 = np.dot(rh1.Tx, x1)

x2 = rh2.res.x
ref2 = np.dot(rh2.Ta, rh2.sche)
opt2 = np.dot(rh2.Ta, x2[:rh2.sp])
load2 = np.dot(rh2.Tx, x2)

x3 = rh3.res.x
ref3 = np.dot(rh3.Ta, rh3.sche)
opt3 = np.dot(rh3.Ta, x3[:rh3.sp])
load3 = np.dot(rh3.Tx, x3)
np.savetxt("prelimrh1.csv",[ref1, opt1, load1], delimiter=",") 
np.savetxt("prelimrh2.csv",[ref2, opt2, load2], delimiter=",") 
np.savetxt("prelimrh3.csv",[ref3, opt3, load3], delimiter=",") 
# sequential game
start_time = time.time()
i = 0
while not org.check_termination():
    # residential
    for rh in rh_list:
        rh.get_aggregate_load(org.provide_load(rh))
        rh.residential_optimize()
        org.collect_load(rh, rh.report_load(), rh.report_change())
    # commercial
    cb.get_aggregate_load(org.provide_load(cb))
    cb.commercial_optimize()
    org.collect_load(cb, cb.report_load(), cb.report_change())
    # industrial
    ip.get_aggregate_load(org.provide_load(ip))
    ip.industrial_optimize()
    org.collect_load(ip, ip.report_load(), ip.report_change())
    i = i + 1
    print('##### Finish Iteration: ' + str(i) + ' #####')
    if i >= 10:
        print('##### Forced Stop After 10 Iterations #####')
        break
    with open('timekeep.txt','a') as outfile:
    	outfile.write(str(datetime.now()) + ' Finish Iteration:' + str(i) +'\n')
print('--- %s seconds ---' % (time.time() - start_time))
with open('timekeep.txt','a') as outfile:
    outfile.write(str(datetime.now()) + ' finished iteration \n')
# save result
opt_res.append({rh1: rh1.res.x,
                rh2: rh2.res.x,
                rh3: rh3.res.x,
                cb: cb.res.x,
                ip: ip.res.x})

AirTempCorrected = np.copy(AirTempForecast)
WindSpeedCorrected = np.copy(WindSpeedForecast)
RadiationCorrected = np.copy(RadiationForecast)


# ### Results

# In[20]:


# individual load
l1 = rh1.compute_load(rh1.res.x)['load']
l2 = rh2.compute_load(rh2.res.x)['load']
l3 = rh3.compute_load(rh3.res.x)['load']
l4 = cb.compute_load(cb.res.x)['load']
l5 = ip.compute_load_fuel(ip.res.x)['load']

# residential
x1 = rh1.res.x
ref1 = np.dot(rh1.Ta, rh1.sche)
opt1 = np.dot(rh1.Ta, x1[:rh1.sp])
load1 = np.dot(rh1.Tx, x1)

x2 = rh2.res.x
ref2 = np.dot(rh2.Ta, rh2.sche)
opt2 = np.dot(rh2.Ta, x2[:rh2.sp])
load2 = np.dot(rh2.Tx, x2)

x3 = rh3.res.x
ref3 = np.dot(rh3.Ta, rh3.sche)
opt3 = np.dot(rh3.Ta, x3[:rh3.sp])
load3 = np.dot(rh3.Tx, x3)
np.savetxt("rh1.csv",[ref1, opt1, load1,ref2, opt2, load2 ,ref3, opt3, load3], delimiter=",") 
# commercial
Ud = np.kron(cb.res.x, np.ones(cb.mc))
RoomTemp = cb.bld.N_time_steps(cb.x0, Ud, cb.W)['T1']

# industrial
Id = np.kron(ip.res.x[:ip.Nc], np.ones(ip.mc))
res_el = ip.el.N_time_steps(Id, ip.T0, ip.Ta)
conEl = res_el['P']/1000
genH2 = res_el['n']*3.6

Ig = np.kron(ip.res.x[ip.Nc:], np.ones(ip.mc))
res_fc = ip.fc.N_time_steps(Ig)
conH2 = res_fc['n']*3.6
genEl = res_fc['P']/1000



residential = l1+l2+l3
commercial = l4
industrial = l5
gridGen = l1+l2+l3+l4+l5 - totGen
renewableGen = totGen

np.savetxt("totgen.csv",[residential, commercial, industrial, renewableGen, gridGen], delimiter=",")


with open('timekeep.txt','a') as outfile:
    outfile.write(str(datetime.now()) + ' ended \n')
