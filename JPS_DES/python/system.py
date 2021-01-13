import numpy as np
from scipy.optimize import *
import math
from commercial import commercial
from residential import residential 
from industrial import industrial
from solarRadiation import solar 
def rotate(lst, h):
	return (lst[h:] + lst[:h])
def system(AirTemp, Radiation, alpha_sc, a_ref, Il_ref, Io_ref,
 Rs_ref, Rsh_ref, Tc_ref, G_ref, Eg_ref, k, Ns, household_below,
  household_above, flex1, sche1, low1, high1, un1, bcap1, flex2,
   sche2, low2, high2, un2, bcap2, flex3, sche3, low3, high3,
    un3, bcap3, cd, Nr, business_below, business_above, HeatSource,
     RoomTempLow, RoomTempHigh, InitialTemp, C1, C2, C3, K1,
      K2, K3, K4, K5, A, B, C, Nc, industry_below, industry_above,
       cf, T0,  F, z1, dG0, dH, r1, r2, s, t1, t2, t3, f1, f2,
        h_cond, h_conv, Ar, nc1, Ct, Rt, taut, Qcw, Tcwi, Ccw,
         z2, U0, E1, E2, I0, R, nc2, eta, Tlow, Thigh, Ilow,
          Ihigh, quantity, Ni):
	def compute_aggregate(idx, load):
		arr = np.zeros(24)
		for i in range(len(load)):
			if i == idx:
				continue
			arr += load[i]
		return arr
	
	def check_termination(x0, x1):
		for i in range(len(x0)):
			if any(abs((x1[i] - x0[i])/(x0[i] + 1e-10))) > 1e-3:
				return False
		return True
	
	totGen = solar(AirTemp, Radiation, alpha_sc, a_ref, Il_ref,
	 Io_ref, Rs_ref, Rsh_ref, Tc_ref, G_ref, Eg_ref, k, Ns)
	load = [np.zeros(24), np.zeros(24), np.zeros(24), np.zeros(24), np.zeros(24)]
	while True:
		try:
			out0 = residential(totGen, compute_aggregate(0, load),
			 household_below, household_above, flex1, sche1, low1,
			  high1, un1, bcap1, cd, Nr, penetration[0])
		except:
			out0 = residential(totGen, compute_aggregate(0, load),
			 household_below, household_above, flex1, sche1, low1,
			  high1, un1, bcap1, cd, Nr)
		load[0] = out0[2] 

		try:
			out1 = residential(totGen, compute_aggregate(1, load),
			 household_below, household_above, flex2, sche2, low2,
			  high2, un2, bcap2, cd, Nr, penetration[1])
		except:
			out1 = residential(totGen, compute_aggregate(1, load),
			 household_below, household_above, flex2, sche2, low2,
			  high2, un2, bcap2, cd, Nr)
		load[1] = out1[2]

		try:
			out2 = residential(totGen, compute_aggregate(2, load),
			 household_below, household_above, flex3, sche3, low3,
			  high3, un3, bcap3, cd, Nr, penetration[2])
		except:
			out2 = residential(totGen, compute_aggregate(2, load),
			 household_below, household_above, flex3, sche3, low3,
			  high3, un3, bcap3, cd, Nr)
		load[2] = out2[2]

		try:
			out3 = commercial(totGen, compute_aggregate(3, load),
			 AirTemp,Radiation, business_below, business_above,
			  HeatSource, RoomTempLow, RoomTempHigh, InitialTemp,
			   C1, C2, C3, K1, K2, K3, K4, K5, A, B, C, Nc, penetration[3])
		except:
			out3 = commercial(totGen, compute_aggregate(3, load),
			 AirTemp,Radiation, business_below, business_above,
			  HeatSource, RoomTempLow, RoomTempHigh, InitialTemp,
			   C1, C2, C3, K1, K2, K3, K4, K5, A, B, C, Nc)
		load[3] = out3[2]

		try:
			out4 = industrial(totGen, compute_aggregate(4, load),
			 AirTemp, industry_below, industry_above, cf, T0,
			   F, z1, dG0, dH, r1, r2, s, t1, t2, t3, f1, f2,
				h_cond, h_conv, Ar, nc1, Ct, Rt, taut, Qcw,
				 Tcwi, Ccw, z2, U0, E1, E2, I0, R, nc2, eta,
				  Tlow, Thigh, Ilow, Ihigh, quantity, Ni, penetration[4])
		except:
			out4 = industrial(totGen, compute_aggregate(4, load),
			 AirTemp, industry_below, industry_above, cf, T0,
			   F, z1, dG0, dH, r1, r2, s, t1, t2, t3, f1, f2,
				h_cond, h_conv, Ar, nc1, Ct, Rt, taut, Qcw,
				 Tcwi, Ccw, z2, U0, E1, E2, I0, R, nc2, eta,
				  Tlow, Thigh, Ilow, Ihigh, quantity, Ni)
		load[4] = out4[2]
		now = datetime.now()
		current_time = now.strftime("%H:%M:%S")
		print(" Current Iteration " + current_time)
		try:
			if check_termination([out0[0], out1[0], out2[0], out3[0], out4[0]],
			 penetration):
				break
		except: pass

		penetration = [out0[0], out1[0], out2[0], out3[0], out4[0], totGen]
		
	# [residential cost 1, residential cost 2, residential cost 3, commerical cost,
	# return [out0[1], out1[1], out2[1], out3[1],
	#  out4[1], totGen, out0[2], out1[2], out2[2], out3[2], out4[2]]
	#  industrial cost, renewable generation, residential load 1, residential load 2, residential load 3, commerical load, industrial load]
	return [out0, out1, out2, out3, out4,totGen]
if __name__ == "__main__":
	import pandas as pd
	import sys
	from caresjpsutil import returnExceptionToJava, returnResultsToJava
	from caresjpsutil import PythonLogger
	from datetime import datetime

	now = datetime.now()
	current_time = now.strftime("%H:%M:%S")
	# pythonLogger = PythonLogger('system.py')
	# pythonLogger.postInfoToLogServer('start of system.py, Current Time = '+current_time)
	folder = sys.argv[1]
	#forecast Weather
	dfWeather=pd.read_csv(folder + '/WeatherForecast.csv', sep=',', header=None)
	AirTemp = dfWeather[0].to_numpy()
	Radiation = dfWeather[1].to_numpy()
	# Residential Agent
	# electricity bill structure
	household_below = 1/50*0.01*np.ones(24)
	household_above = 1/50*0.02*np.ones(24)
	cd = 0.003
	Nr = 60 #No of Households? 
	low = np.asarray(pd.read_csv(folder + '/Pmin.csv', header = None))
	high = np.asarray(pd.read_csv(folder + '/Pmax.csv', header = None))
	unwill = np.asarray(pd.read_csv(folder + '/unwill.csv', header = None))
	bcap = np.asarray(pd.read_csv(folder + '/bcap.csv', header = None))
	appl1 = pd.read_csv(folder + '/ApplianceScheduleLoad1.csv', sep=',',header=None)
	seq = appl1.iloc[0]
	sche1 = np.array( [seq[i:i+11] for i in range(0,len(seq),11)] )
	flex1 = np.array(sche1, dtype=bool)
	seq = appl1.iloc[1]
	sche2 = np.array( [seq[i:i+11] for i in range(0,len(seq),11)] )
	flex2 = np.array(sche2, dtype=bool)
	seq1 = appl1.iloc[2]
	sche3 = np.array( [seq[i:i+11] for i in range(0,len(seq),11)] )
	flex3 = np.array(sche3, dtype=bool)

	#From Industrial:
	industry_below = 0.154*np.ones(24)#np.array([0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154])
	industry_above = 0.000308 *np.ones(24)#np.array([0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308])

	#Electrolyzer
	DF_E= pd.read_csv(folder + "/ElectrolyzerConstant.csv", header=None )
	# physical constants:
	F = DF_E.iloc[2,1] #96485.34 #[A*s/mol] #Faraday's Constant
	z1 = DF_E.iloc[6,1] #2 # no. of electrons transferred per reaction #This is standard for the reaction
	# reaction information: (25 degreeC and 1 bar @ standard conditions) 
	dG0 = DF_E.iloc[8,1] #237e3 #[J/mol]
	dH = DF_E.iloc[9,1] #286e3 #[J/mol]
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
	Ar = DF_E.iloc[0,1] #0.25   #[m^2]
	nc1 = 21 # no. of cells in series
	Ct = 625e3 #[J/C]
	Rt = DF_E.iloc[4,1] #0.167 #[C/W] #Heat Resistance
	taut = Ct*Rt #[s]
	Qcw =  DF_E.iloc[5,1] #[m^3/s] flow rate of cooling water
	Tcwi = DF_E.iloc[1,1] # 14.5 #[C]
	Ccw = 4.18e3*1e3*Qcw #[W/C]
	z2 =  2 #no of Electrons


	DF_FC = pd.read_csv(folder + "/FuelCell.csv", header=None )
	nc2 = DF_FC.iloc[0,0] # no. of cells
	eta = DF_FC.iloc[0,1] # fuel utilization factor
	Tlow = DF_FC.iloc[0,2] #[C]
	Thigh = DF_FC.iloc[0,3] #[C]
	#FuelCell
	# polarization curve parameters:
	U0 = DF_FC.iloc[0,4]   #[V]
	E1 = -0.013            #[V/C]
	E2 = -1.57             #[1]
	I0 = DF_FC.iloc[0,5]   #[A]
	R = -2.04              #[Ohm*C]
	# variables that are not taken from OWL file taken from here: https://doi.org/10.1016/j.jpowsour.2004.08.019 
	# Apparently they're taken from another paper? There's no basis? Extract from properties file? 
	# track optimal operating temperature if possible:
	Ilow = Tlow**2*E1/R #[A]
	Ihigh = Thigh**2*E1/R #[A]
	quantity = 2500
	Ni = 30 
	cf = np.array([0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462])
	T0 = 99
   #From Commercial
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
	#Rotation of the hour
	hour = now.hour
	HeatSource = np.array(rotate([5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 25.0, 25.0, 25.0, 25.0, 25.0,
	 25.0, 25.0, 25.0, 25.0, 25.0, 25.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0], hour))
	RoomTempLow = np.array(rotate([19.0, 19.0, 19.0, 19.0, 19.0, 19.0, 19.0, 21.0, 21.0, 21.0, 21.0,
	 21.0, 21.0, 21.0, 21.0, 21.0, 21.0, 21.0, 19.0, 19.0, 19.0, 19.0, 19.0, 19.0], hour))
	RoomTempHigh = np.array(rotate([30.0, 30.0, 30.0, 30.0, 30.0, 30.0, 30.0, 25.8, 25.8, 25.8, 25.8,
	 25.8, 25.8, 25.8, 25.8, 25.8, 25.8, 25.8, 30.0, 30.0, 30.0, 30.0, 30.0, 30.0], hour))
	#Supposed to be the ambient temperature of the building. 
	#get previous hour's temperature
	previ_hourTemp = RoomTempHigh[hour-1]    
	InitialTemp = previ_hourTemp*np.ones(3)


	A = np.array([[-1/C1*(K1+K2+K3+K5), 1/C1*(K1+K2), 1/C1*K5], [1/C2*(K1+K2), -1/C2*(K1+K2), 0], [1/C3*K5, 0, -1/C3*(K4+K5)]])
	B = np.array([[-1/C1], [0], [0]])
	C = np.array([[1/C1*K3, 1/C1, 1/C1], [0, 1/C2, 0], [1/C3*K4, 0, 0]])
	Nc = 1

	#From Solar
	dfSolar=pd.read_csv(folder + '/PVGenerator.csv', sep=',', header=None)
	alpha_sc = dfSolar.iloc[0,0] #0.002132 #<http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/PVPanel-001.owl#V_TempCoeffSCC_PVPanel-001>
	a_ref =  dfSolar.iloc[0,1] #1.9136 #<http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/PVPanel-001.owl#V_IdealityFactor_PVPanel-001>
	Il_ref = dfSolar.iloc[0,2] #5.995 #<http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/PVPanel-001.owl#V_Il_PVPanel-001>
	Io_ref = dfSolar.iloc[0,3] #5.194e-11 #<http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/PVPanel-001.owl#V_Io_PVPanel-001>
	Rs_ref = dfSolar.iloc[0,4] #0.321 #<http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/PVPanel-001.owl#V_Rs_PVPanel-001>
	Rsh_ref = dfSolar.iloc[0,5] #367.11 #<http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/PVPanel-001.owl#V_Rsh_PVPanel-001>
	Tc_ref = dfSolar.iloc[0,6] + 273.15 #25 #<http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/PVPanel-001.owl#V_Tcref_PVPanel-001>
	G_ref =dfSolar.iloc[0,7]  # 1000 #<http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/PVPanel-001.owl#V_Gref_PVPanel-001>
	Eg_ref = dfSolar.iloc[0,8] #1.121 #<http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/PVPanel-001.owl#V_Eg_PVPanel-001>
	k = 8.6173324e-5 #Boltzman Constant
	Ns = 3000
	result = system(AirTemp, Radiation, alpha_sc, a_ref,
     Il_ref, Io_ref, Rs_ref, Rsh_ref, Tc_ref,
      G_ref, Eg_ref, k, Ns, household_below, household_above,
       flex1, sche1, low[0], high[0], unwill[0], bcap[0], flex2, sche2,
        low[1], high[1], unwill[1], bcap[1], flex3, sche3,
       low[2], high[2], unwill[2], bcap[2], cd, Nr, business_below, business_above,
          HeatSource, RoomTempLow, RoomTempHigh, InitialTemp,
           C1, C2, C3, K1, K2, K3, K4, K5, A, B, C, Nc, industry_below,
            industry_above, cf, T0,  F, z1, dG0, dH, r1, r2, s, t1, t2,
             t3, f1, f2, h_cond, h_conv, Ar, nc1, Ct, Rt, taut, Qcw, Tcwi,
              Ccw, z2, U0, E1, E2, I0, R, nc2, eta, Tlow, Thigh, Ilow,
               Ihigh, quantity, Ni)

	out0 = result[0]
	out1 = result[1]
	out2 = result[2]
	residential = out0[2]+out1[2]+out2[2]
	commercial = result[3][2]
	industrial = result[4][2]
	renewableGen = result[5]
	gridGen = residential + commercial + industrial - renewableGen
	#clear to zero
	gridGen = gridGen.clip(min=0)

	np.savetxt(folder +"/totgen.csv",[residential, commercial, industrial, renewableGen, gridGen], delimiter=",")
	np.savetxt(folder +"/rh1.csv",[out0[3],out0[4], out0[5],out1[3],out1[4],out1[5],out2[3],out2[4], out2[5]], delimiter="," )
	returnResultsToJava(result)
	now = datetime.now()
	current_time = now.strftime("%H:%M:%S")
	# pythonLogger.postInfoToLogServer('end of system.py, Current time = ' +current_time)