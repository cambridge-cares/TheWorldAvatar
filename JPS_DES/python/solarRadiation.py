import numpy as np
from scipy.optimize import *
import math    
def solar(AirTemp, Radiation, alpha_sc, a_ref, Il_ref, Io_ref, Rs_ref, Rsh_ref, Tc_ref, G_ref, Eg_ref, k, Ns):
	
	class RenewableGeneration:

		def __init__(self, t):
			self.t = t

		# power generation for one solar panel
		def solar_energy(self, G, Tc):
			# Solar Panel
			# PV module: SunPower SPR-230-WHT
			# Type of cell: Mono-c-Si

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
	
	rg = RenewableGeneration(24)
	rg.alpha_sc = alpha_sc
	rg.a_ref = a_ref
	rg.Il_ref = Il_ref
	rg.Io_ref = Io_ref
	rg.Rs_ref = Rs_ref
	rg.Rsh_ref = Rsh_ref
	rg.Tc_ref = Tc_ref
	rg.G_ref = G_ref
	rg.Eg_ref = Eg_ref
	rg.k = k
	
	# solar generation profile
	return Ns*rg.solar_energy(Radiation, AirTemp)/1000


if __name__ == "__main__":
	import pandas as pd
	import sys
	from caresjpsutil import returnExceptionToJava, returnResultsToJava
	from caresjpsutil import PythonLogger

	pythonLogger = PythonLogger('solarRadiation.py')
	pythonLogger.postInfoToLogServer('start of solarRadiation.py')
	folder = sys.argv[1]
	dfWeather=pd.read_csv(folder + '/WeatherForecast.csv', sep=',', header=None)
	AirTemp = dfWeather[0].to_numpy()
	Radiation = dfWeather[1].to_numpy()
	# AirTemp = np.array([28.45, 28.3 , 28.11, 27.9 , 27.74, 27.68, 27.59,
	#  27.54, 26.59, 23.82, 26.7 , 28.23, 28.92, 29.15, 30.56, 30.45, 30.65,
	#   31.06, 30.97, 30.76, 30.03, 29.76, 29.48, 28.99])
	# Radiation = np.array([1.700e-02, 2.100e-02, 1.500e-02, 2.000e-02, 1.800e-02,
	#  1.500e-02, 2.400e-02, 3.360e-01, 2.343e+01, 1.193e+02, 4.061e+02, 5.386e+02,
	#   6.513e+02, 7.080e+02, 7.000e+02, 6.656e+02, 5.661e+02, 4.238e+02, 2.571e+02,
	#    8.630e+01, 1.071e+00, 1.900e-02, 1.800e-02, 1.700e-02])

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
	result = solar(AirTemp, Radiation, alpha_sc, a_ref, Il_ref, Io_ref, Rs_ref,
	 Rsh_ref, Tc_ref, G_ref, Eg_ref, k, Ns)

	f = open(folder + "/solar.csv", 'ab')
	np.savetxt(f,result, delimiter=",")
	returnResultsToJava(result)
	pythonLogger.postInfoToLogServer('end of solarRadiation.py')
