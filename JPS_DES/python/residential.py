import numpy as np
from scipy.optimize import *
import math  
def residential(totGen, aggrLoad, household_below, household_above, flex, sche, low, high, unwill, bcap, cd, Nr, penetration=np.array([])):
    
    class ResidentialHousehold:

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
    
    rh = ResidentialHousehold(24, 11, 1)
    rh.NUMBER = Nr
    rh.set_flexibility(flex)
    rh.set_schedule(sche)
    rh.set_power_limit(high, low)
    rh.set_unwilling(unwill)
    rh.set_bat_cap(bcap)
    rh.electricity_bill_structure(household_below, household_above)
    rh.discomfort_factor(cd)
    rh.define_aggregator()
    rh.set_q0(0*rh.bcap)
    rh.set_var_bounds()
    rh.set_starting_point()
    rh.cutoff_load(totGen)
    
    rh.get_aggregate_load(aggrLoad)
    if penetration.size > 0:
        rh.prev = penetration
    rh.residential_optimize()
    
    # [x, cost, load, appliance consumption profile, battery charging profile, total energy consumption profile]
    opt = np.dot(rh.Ta, rh.res.x[:rh.sp])
    load = np.dot(rh.Tx, rh.res.x)
    return [rh.res.x, rh.objfunction(rh.res.x), rh.report_load(), opt, load-opt, load]

if __name__ == "__main__":
    #automatically assigned
    household_below = 1/50*0.01*np.ones(24)
    household_above = 1/50*0.02*np.ones(24)
    flex1 = np.array([[False, False, True, True, True, False, False, False, False, False, False],
     [False, False, True, True, True, False, False, False, False, False, False],
      [False, False, True, True, True, False, False, False, False, False, False],
       [False, False, True, True, True, False, False, False, False, False, False],
        [False, False, True, True, True, False, False, False, False, False, False],
         [False, False, True, True, True, False, False, False, False, False, False],
          [False, False, False, True, True, False, False, True, True, False, False],
           [False, False, False, True, True, False, True, False, False, False, False],
            [False, False, False, True, True, False, True, False, False, False, False],
             [False, False, False, True, True, False, True, False, False, False, False],
              [False, False, False, True, True, False, True, False, False, False, False],
               [False, False, False, True, True, False, True, False, False, False, False],
                [False, False, False, True, True, False, True, False, False, False, True],
                 [False, False, False, True, True, True, True, False, False, False, True],
                  [True, False, False, True, True, True, True, False, False, False, False],
                   [True, False, False, True, True, False, True, False, False, False, False],
                    [True, False, False, True, True, False, True, False, False, False, False],
                     [True, False, False, True, True, True, True, False, False, False, False],
                      [True, False, False, True, True, True, True, True, False, True, False],
                       [False, True, False, True, True, True, True, False, False, False, False],
                        [False, True, True, True, True, True, True, False, False, False, False],
                         [False, True, True, True, True, False, True, False, False, False, False],
                          [False, False, True, True, True, False, True, False, False, False, False],
                           [False, False, True, True, True, False, False, False, False, False, False]])
    sche1 = np.array([[0.0, 0.0, 0.1, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.1, 0.0, 0.0, 0.65, 0.45, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.1, 0.0, 0.32, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.1, 0.0, 0.32, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.01, 0.0, 0.32, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.1, 0.0, 0.32, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.01, 0.0, 0.32, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.1, 0.0, 0.32, 0.0, 0.0, 0.0, 0.95], [0.0, 0.0, 0.0, 0.2, 0.01, 0.17, 0.32, 0.0, 0.0, 0.0, 0.95], [0.6, 0.0, 0.0, 0.2, 0.1, 0.17, 0.32, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.01, 0.0, 0.32, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.1, 0.0, 0.32, 0.0, 0.0, 0.0, 0.0], [3.1, 0.0, 0.0, 0.2, 0.1, 0.17, 0.32, 0.0, 0.0, 0.0, 0.0], [3.1, 0.0, 0.0, 0.2, 0.1, 0.17, 0.32, 0.65, 0.0, 0.725, 0.0], [0.0, 2.4, 0.0, 0.2, 0.1, 0.17, 0.32, 0.0, 0.0, 0.0, 0.0], [0.0, 0.2, 1.6, 0.2, 0.1, 0.17, 0.32, 0.0, 0.0, 0.0, 0.0], [0.0, 0.2, 0.1, 0.2, 0.1, 0.0, 0.32, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.1, 0.0, 0.32, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]])
    low1  = np.array([0.6, 0.2, 0.1, 0.2, 0.01, 0.17, 0.32, 0.65, 0.45, 0.725, 0.95])
    high1 = np.array([3.9, 3.0, 3.1, 0.2, 0.1, 0.17, 0.32, 0.65, 0.45, 0.725, 0.95])

    unwill1 = np.array([0.5, 0.5, 0.6, 1.0, 0.7, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0])
    #read bcap from csv
    bcap1 = pd.read_csv('bcap.csv', header = None).iloc[0, 1]
    cd = 0.003
    Nr = 60
    print(residential(np.zeros(24), np.zeros(24), household_below, household_above, flex1,
     sche1, low1, high1, unwill1, bcap1, cd, Nr)[3:])