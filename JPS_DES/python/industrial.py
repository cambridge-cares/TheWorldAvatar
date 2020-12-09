
import numpy as np
from scipy.optimize import *
import math  
import pandas as pd

def industrial(totGen, aggrLoad, AirTemp, industry_below, industry_above, cf, T0, F,
 z1, dG0, dH, r1, r2, s, t1, t2, t3, f1, f2, h_cond, h_conv, Ar, nc1, Ct, Rt, taut,
  Qcw, Tcwi, Ccw, z2, U0, E1, E2, I0, R, nc2, eta, Tlow, Thigh, Ilow, Ihigh, quantity,
   Ni, penetration=np.array([])):
    
    class Electrolyzer_Temperature_Dynamics:

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
    
    class Fuel_Cell:

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
    
    class IndustrialPlant:
    
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
            dbill_below_dIIc = (load + self.aggr <= self.cutoff).reshape((self.Nf, 1))* \
                               (np.dot(cost_below.reshape((self.Nf, 1)), dfraction_dIIc.reshape((1, 2*self.Nc))) + \
                                fraction*dcost_below_dIIc)

            # when total load is above max. renewable generation
            cost_above = self.c0*self.cutoff**2 + self.c1*(load + self.aggr - self.cutoff)**2
            dcost_above_dIIc = (self.c1*2*(load + self.aggr - self.cutoff)).reshape((self.Nf, 1))*dload_dIIc

            bill_above = (load + self.aggr >  self.cutoff)*fraction*cost_above
            dbill_above_dIIc = (load + self.aggr >  self.cutoff).reshape((self.Nf, 1))* \
                              (np.dot(cost_above.reshape((self.Nf, 1)), dfraction_dIIc.reshape((1, 2*self.Nc))) + \
                               fraction*dcost_above_dIIc)

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
                                options={'ftol': 0.01, 'maxiter': 200, 'disp': False})

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
    
    ip = IndustrialPlant(24*3600, 900, 3600, 3600, 3600)
    ip.NUMBER = Ni
    ip.quantity = quantity
    ip.el.F = F
    ip.el.z = z1
    ip.el.dG0 = dG0
    ip.el.dH = dH
    ip.el.r1 = r1
    ip.el.r2 = r2
    ip.el.s = s
    ip.el.t1 = t1
    ip.el.t2 = t2
    ip.el.t3 = t3
    ip.el.f1 = f1
    ip.el.f2 = f2
    ip.el.h_cond = h_cond
    ip.el.h_conv = h_conv
    ip.el.A = Ar
    ip.el.nc = nc1
    ip.el.Ct = Ct
    ip.el.Rt = Rt
    ip.el.taut = taut
    ip.el.Qcw = Qcw
    ip.el.Tcwi = Tcwi
    ip.el.Ccw = Ccw
    ip.fc.F = F
    ip.fc.z = z2
    ip.fc.U0 = U0
    ip.fc.E1 = E1
    ip.fc.E2 = E2
    ip.fc.I0 = I0
    ip.fc.R = R
    ip.fc.nc = nc2
    ip.fc.eta = eta
    ip.fc.Tlow = Tlow
    ip.fc.Thigh = Thigh
    ip.fc.Ilow = Ilow
    ip.fc.Ihigh = Ihigh
    ip.electricity_bill_structure(industry_below, industry_above)
    ip.fuel_cost_factor(cf)
    ip.get_T0(T0)
    ip.set_starting_point()
    ip.cutoff_load(totGen)
    ip.get_ambient_temperature(AirTemp)
    
    ip.get_aggregate_load(aggrLoad)
    if penetration.size > 0:
        ip.prev = penetration
    ip.industrial_optimize()
    
    # [x, cost, load, chlorine production profile, hydrogen purchase profile, total energy consumption profile]
    Id = np.kron(ip.res.x[:ip.Nc], np.ones(ip.mc))
    res_el = ip.el.N_time_steps(Id, ip.T0, ip.Ta)
    conEl = res_el['P']/1000
    genH2 = res_el['n']*3.6
    Ig = np.kron(ip.res.x[ip.Nc:], np.ones(ip.mc))
    res_fc = ip.fc.N_time_steps(Ig)
    conH2 = res_fc['n']*3.6
    genEl = res_fc['P']/1000
    return [ip.res.x, ip.objfunction(ip.res.x), ip.report_load(),
     np.dot(np.kron(np.identity(24), 1/ip.mc*np.ones(ip.mc)), genH2), np.dot(np.kron(np.identity(24),
      1/ip.mc*np.ones(ip.mc)), conH2-genH2), np.dot(np.kron(np.identity(24), 1/ip.mc*np.ones(ip.mc)), conEl-genEl)]

if __name__ == "__main__":
    AirTemp = np.array([28.45, 28.3 , 28.11, 27.9 , 27.74, 27.68, 27.59,
     27.54, 26.59, 23.82, 26.7 , 28.23, 28.92, 29.15, 30.56, 30.45, 30.65,
      31.06, 30.97, 30.76, 30.03, 29.76, 29.48, 28.99])
    industry_below = np.array([0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154])
    industry_above = np.array([0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308])
   
    #Electrolyzer
    # physical constants:
    F = 96485.34 #[A*s/mol]
    z1 = 2 # no. of electrons transferred per reaction
    # reaction information: (25 degreeC and 1 bar @ standard conditions) 
    dG0 = 237e3 #[J/mol]
    dH = 286e3 #[J/mol]
    # I-U curve parameters:
    r1 = 8.05e-5 #[Ohm*m^2]
    r2 = -2.5e-7 #[Ohm*m^2/C]
    s = 0.185 #[V]
    t1 = -0.1002 #[m^2/A]
    t2 = 8.424 #[m^2*C/A]
    t3 = 247.3 #[m^2*C^2/A]
    # Faraday efficiency parameters: (80 degreeC @ HYSOLAR) 
    f1 = 250e2             #[A^2/m^4]
    f2 = 0.98              #[1]
    # UA_HX parameters:
    h_cond = 7             #[W/C]
    h_conv = 0.02          #[W/C/A]
    # operation parameters:
    Ar = 0.25   #[m^2]
    nc1 = 21 # no. of cells in series
    Ct = 625e3 #[J/C]
    Rt = 0.167 #[C/W]
    taut = Ct*Rt #[s]
    Qcw = 0.6/3600 #[m^3/s] flow rate of cooling water
    Tcwi = 14.5 #[C]
    Ccw = 4.18e3*1e3*Qcw #[W/C]
    z2 = 2

    #FuelCell
    # polarization curve parameters:
    U0 = 33.18             #[V]
    E1 = -0.013            #[V/C]
    E2 = -1.57             #[1]
    I0 = 8.798             #[A]
    R = -2.04              #[Ohm*C]
    DF_FC = pd.read_csv("FuelCell.csv", header=None )
    nc2 = DF_FC.iloc[0,1] # no. of cells
    eta = DF_FC.iloc[1,1] # fuel utilization factor
    Tlow = DF_FC.iloc[2,1] #[C]
    Thigh = DF_FC.iloc[3,1] #[C]
    # track optimal operating temperature if possible:
    Ilow = Tlow**2*E1/R #[A]
    Ihigh = Thigh**2*E1/R #[A]
    quantity = 2500
    Ni = 30 
    cf = np.array([0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462])
    T0 = 99

    industrial(np.zeros(24), np.zeros(24), AirTemp, industry_below, industry_above,
     cf, T0,  F, z1, dG0, dH, r1, r2, s, t1, t2, t3, f1, f2, h_cond, h_conv, Ar,
      nc1, Ct, Rt, taut, Qcw, Tcwi, Ccw, z2, U0, E1, E2, I0, R, nc2, eta, Tlow,
       Thigh, Ilow, Ihigh, quantity, Ni)[3:]