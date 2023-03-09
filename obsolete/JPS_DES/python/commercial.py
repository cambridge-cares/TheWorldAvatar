import numpy as np
from scipy.optimize import *
import math    
def commercial(totGen, aggrLoad, AirTemp,Radiation, business_below, business_above, HeatSource, RoomTempLow, RoomTempHigh, InitialTemp, C1, C2, C3, K1, K2, K3, K4, K5, A, B, C, Nc, penetration=np.array([])):
	
	class Building_Temperature_Dynamics:
	
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
	
	class CommercialBuilding:

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
			dbill_below_dUc = (load + self.aggr <= self.cutoff).reshape((self.Nf, 1))* \
							  (np.dot(cost_below.reshape((self.Nf, 1)), dfraction_dUc.reshape((1, self.Nc))) + \
							   fraction*dcost_below_dUc)

			# when total load is above max. renewable generation
			cost_above = self.c0*self.cutoff**2 + self.c1*(load + self.aggr - self.cutoff)**2
			dcost_above_dUc = (self.c1*2*(load + self.aggr - self.cutoff)).reshape((self.Nf, 1))*dload_dUc

			bill_above = (load + self.aggr >  self.cutoff)*fraction*cost_above
			dbill_above_dUc = (load + self.aggr >  self.cutoff).reshape((self.Nf, 1))* \
							  (np.dot(cost_above.reshape((self.Nf, 1)), dfraction_dUc.reshape((1, self.Nc))) + \
							   fraction*dcost_above_dUc)

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
	
	cb = CommercialBuilding(24*3600, 600, 3600, 3600, 3600)
	cb.NUMBER = Nc
	cb.bld.C1 = C1
	cb.bld.C2 = C2
	cb.bld.C3 = C3
	cb.bld.K1 = K1
	cb.bld.K2 = K2
	cb.bld.K3 = K3
	cb.bld.K4 = K4
	cb.bld.K5 = K5
	cb.bld.A = A
	cb.bld.B = B
	cb.bld.C = C
	cb.electricity_bill_structure(business_below, business_above)
	cb.get_x0(InitialTemp)
	cb.set_starting_point()
	cb.cutoff_load(totGen)
	cb.get_disturbance(np.vstack([AirTemp, 0.3*Radiation, 3.6*HeatSource]))
	cb.get_T1_range(3600, RoomTempLow, RoomTempHigh)
	
	cb.get_aggregate_load(aggrLoad)
	if penetration.size > 0:
		cb.prev = penetration
	cb.commercial_optimize()
	
	# [x, cost, load, room temperature profile, total energy consumption profile]
	Ud = np.kron(cb.res.x, np.ones(cb.mc))
	RoomTemp = cb.bld.N_time_steps(cb.x0, Ud, cb.W)['T1']
	return [cb.res.x, cb.objfunction(cb.res.x), cb.report_load(),
	 np.dot(np.kron(np.identity(24), 1/cb.mc*np.ones(cb.mc)), RoomTemp), cb.res.x]

if __name__ == "__main__":
	import pandas as pd
	import sys
	from caresjpsutil import returnExceptionToJava, returnResultsToJava
	from caresjpsutil import PythonLogger

	pythonLogger = PythonLogger('commercial.py')
	pythonLogger.postInfoToLogServer('start of commercial.py')
	folder = sys.argv[1]
	dfWeather=pd.read_csv(folder + '/WeatherForecast.csv', sep=',', header=None)
	dfConst=pd.read_csv(folder + '/constant.csv', sep=',', header=None)
	AirTemp = dfWeather[0].to_numpy()
	Radiation = dfWeather[1].to_numpy()
	C1 = dfConst.iloc[0,0]
	C2 = dfConst.iloc[0,1]
	C3 = dfConst.iloc[0,2]
	K1 = dfConst.iloc[0,3]
	K2 = dfConst.iloc[0,4]
	K3 = dfConst.iloc[0,5]
	K4 = dfConst.iloc[0,6]
	K5 = dfConst.iloc[0,7]

	#Default Values; we don't have a OWL for them since they're arbitrary
	business_below = 1.14*1/50*0.01*np.ones(24)
	business_above = 1.14*1/50*0.02*np.ones(24)
	HeatSource = np.array([5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 25.0, 25.0, 25.0, 25.0, 25.0,
	 25.0, 25.0, 25.0, 25.0, 25.0, 25.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0])
	RoomTempLow = np.array([19.0, 19.0, 19.0, 19.0, 19.0, 19.0, 19.0, 21.0, 21.0, 21.0, 21.0,
	 21.0, 21.0, 21.0, 21.0, 21.0, 21.0, 21.0, 19.0, 19.0, 19.0, 19.0, 19.0, 19.0])
	RoomTempHigh = np.array([30.0, 30.0, 30.0, 30.0, 30.0, 30.0, 30.0, 25.8, 25.8, 25.8, 25.8,
	 25.8, 25.8, 25.8, 25.8, 25.8, 25.8, 25.8, 30.0, 30.0, 30.0, 30.0, 30.0, 30.0])
	InitialTemp = np.array([28.3, 28.2, 28.6])

	
	A = np.array([[-1/C1*(K1+K2+K3+K5), 1/C1*(K1+K2), 1/C1*K5], [1/C2*(K1+K2), -1/C2*(K1+K2), 0], [1/C3*K5, 0, -1/C3*(K4+K5)]])
	B = np.array([[-1/C1], [0], [0]])
	C = np.array([[1/C1*K3, 1/C1, 1/C1], [0, 1/C2, 0], [1/C3*K4, 0, 0]])
	Nc = 1
	f = open(folder + "/commercial.csv", 'ab')
	
	result = commercial(np.zeros(24), np.zeros(24), AirTemp,Radiation, business_below,
	 business_above, HeatSource, RoomTempLow, RoomTempHigh, InitialTemp,
	  C1, C2, C3, K1, K2, K3, K4, K5, A, B, C, Nc)[3:]
	np.savetxt(f,result, delimiter=",")
	returnResultsToJava(result)
	pythonLogger.postInfoToLogServer('end of commercial.py')