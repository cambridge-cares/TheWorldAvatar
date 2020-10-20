import numpy as np
from scipy.optimize import *
import math
from commercial import commercial
from residential import residential 
from industrial import industrial
from solarRadiation import solar 
import pandas as pd

def system(AirTemp, Radiation, alpha_sc, a_ref, Il_ref, Io_ref, Rs_ref, Rsh_ref, Tc_ref, G_ref, Eg_ref, k, Ns, household_below, household_above, flex1, sche1, low1, high1, un1, bcap1, flex2, sche2, low2, high2, un2, bcap2, flex3, sche3, low3, high3, un3, bcap3, cd, Nr, business_below, business_above, HeatSource, RoomTempLow, RoomTempHigh, InitialTemp, C1, C2, C3, K1, K2, K3, K4, K5, A, B, C, Nc, industry_below, industry_above, cf, T0,  F, z1, dG0, dH, r1, r2, s, t1, t2, t3, f1, f2, h_cond, h_conv, Ar, nc1, Ct, Rt, taut, Qcw, Tcwi, Ccw, z2, U0, E1, E2, I0, R, nc2, eta, Tlow, Thigh, Ilow, Ihigh, quantity, Ni):
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
    
    totGen = solar(AirTemp, Radiation, alpha_sc, a_ref, Il_ref, Io_ref, Rs_ref, Rsh_ref, Tc_ref, G_ref, Eg_ref, k, Ns)
    load = [np.zeros(24), np.zeros(24), np.zeros(24), np.zeros(24), np.zeros(24)]
    while True:
        try:
            out0 = residential(totGen, compute_aggregate(0, load), household_below, household_above, flex1, sche1, low1, high1, un1, bcap1, cd, Nr, penetration[0])
        except:
            out0 = residential(totGen, compute_aggregate(0, load), household_below, household_above, flex1, sche1, low1, high1, un1, bcap1, cd, Nr)
        load[0] = out0[2] 

        try:
            out1 = residential(totGen, compute_aggregate(1, load), household_below, household_above, flex2, sche2, low2, high2, un2, bcap2, cd, Nr, penetration[1])
        except:
            out1 = residential(totGen, compute_aggregate(1, load), household_below, household_above, flex2, sche2, low2, high2, un2, bcap2, cd, Nr)
        load[1] = out1[2]

        try:
            out2 = residential(totGen, compute_aggregate(2, load), household_below, household_above, flex3, sche3, low3, high3, un3, bcap3, cd, Nr, penetration[2])
        except:
            out2 = residential(totGen, compute_aggregate(2, load), household_below, household_above, flex3, sche3, low3, high3, un3, bcap3, cd, Nr)
        load[2] = out2[2]

        try:
            out3 = commercial(totGen, compute_aggregate(3, load), AirTemp,Radiation, business_below, business_above, HeatSource, RoomTempLow, RoomTempHigh, InitialTemp, C1, C2, C3, K1, K2, K3, K4, K5, A, B, C, Nc, penetration[3])
        except:
            out3 = commercial(totGen, compute_aggregate(3, load), AirTemp,Radiation, business_below, business_above, HeatSource, RoomTempLow, RoomTempHigh, InitialTemp, C1, C2, C3, K1, K2, K3, K4, K5, A, B, C, Nc)
        load[3] = out3[2]

        try:
            out4 = industrial(totGen, compute_aggregate(4, load), AirTemp, industry_below, industry_above, cf, T0,  F, z1, dG0, dH, r1, r2, s, t1, t2, t3, f1, f2, h_cond, h_conv, Ar, nc1, Ct, Rt, taut, Qcw, Tcwi, Ccw, z2, U0, E1, E2, I0, R, nc2, eta, Tlow, Thigh, Ilow, Ihigh, quantity, Ni, penetration[4])
        except:
            out4 = industrial(totGen, compute_aggregate(4, load), AirTemp, industry_below, industry_above, cf, T0,  F, z1, dG0, dH, r1, r2, s, t1, t2, t3, f1, f2, h_cond, h_conv, Ar, nc1, Ct, Rt, taut, Qcw, Tcwi, Ccw, z2, U0, E1, E2, I0, R, nc2, eta, Tlow, Thigh, Ilow, Ihigh, quantity, Ni)
        load[4] = out4[2]

        try:
            if check_termination([out0[0], out1[0], out2[0], out3[0], out4[0]], penetration):
                break
        except: pass

        penetration = [out0[0], out1[0], out2[0], out3[0], out4[0]]
        
    # [residential cost 1, residential cost 2, residential cost 3, commerical cost, industrial cost, renewable generation, residential load 1, residential load 2, residential load 3, commerical load, industrial load]
    return [out0[1], out1[1], out2[1], out3[1], out4[1], totGen, out0[2], out1[2], out2[2], out3[2], out4[2]]
if __name__ == "__main__":
    from datetime import datetime
    now = datetime.now()

    current_time = now.strftime("%H:%M:%S")
    print("Current Time =", current_time)
    # general input
    # forecast weather
    DF_FORECAST = pd.read_csv("WeatherForecast.csv", header = None)
    AirTemp = DF_FORECAST[0].to_numpy()
    Radiation = DF_FORECAST[1].to_numpy()
    # electricity bill structure
    household_below = np.array([0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002])
    household_above = np.array([0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004])
    # what values are these? 

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
    flex1 = np.array([[False, False, True, True, True, False, False, False, False, False, False], [False, False, True, True, True, False, False, False, False, False, False], [False, False, True, True, True, False, False, False, False, False, False], [False, False, True, True, True, False, False, False, False, False, False], [False, False, True, True, True, False, False, False, False, False, False], [False, False, True, True, True, False, False, False, False, False, False], [False, False, False, True, True, False, False, True, True, False, False], [False, False, False, True, True, False, True, False, False, False, False], [False, False, False, True, True, False, True, False, False, False, False], [False, False, False, True, True, False, True, False, False, False, False], [False, False, False, True, True, False, True, False, False, False, False], [False, False, False, True, True, False, True, False, False, False, False], [False, False, False, True, True, False, True, False, False, False, True], [False, False, False, True, True, True, True, False, False, False, True], [True, False, False, True, True, True, True, False, False, False, False], [True, False, False, True, True, False, True, False, False, False, False], [True, False, False, True, True, False, True, False, False, False, False], [True, False, False, True, True, True, True, False, False, False, False], [True, False, False, True, True, True, True, True, False, True, False], [False, True, False, True, True, True, True, False, False, False, False], [False, True, True, True, True, True, True, False, False, False, False], [False, True, True, True, True, False, True, False, False, False, False], [False, False, True, True, True, False, True, False, False, False, False], [False, False, True, True, True, False, False, False, False, False, False]])
    flex2 = np.array([[False, False, True, True, False, False, False, False, False, False, False], [False, False, True, True, False, False, False, False, False, False, False], [False, False, True, True, False, False, False, False, False, False, False], [False, False, True, True, True, False, False, False, False, False, False], [False, False, True, True, True, False, False, False, False, False, False], [False, False, True, True, True, False, True, False, False, False, False], [False, False, True, True, True, False, True, False, False, False, False], [False, False, False, True, True, True, True, True, True, False, False], [False, False, False, True, False, False, False, False, False, False, False], [True, False, False, True, False, False, False, False, False, False, False], [True, False, False, True, False, False, False, False, False, False, False], [True, False, False, True, False, False, False, False, False, False, False], [True, False, False, True, False, False, False, False, False, False, False], [True, False, False, True, False, False, False, False, False, False, False], [True, False, False, True, True, False, False, False, False, False, False], [True, False, False, True, True, False, False, False, False, False, False], [True, False, False, True, True, False, False, False, False, False, False], [False, True, False, True, True, True, True, False, False, False, False], [False, True, False, True, True, True, True, False, False, True, True], [False, True, True, True, True, False, True, False, False, False, False], [False, True, True, True, True, False, True, False, False, False, False], [False, True, True, True, True, False, True, False, False, False, False], [False, False, True, True, True, False, True, False, False, False, False], [False, False, True, True, True, False, True, False, False, False, False]])
    flex3 = np.array([[False, False, True, True, True, True, True, False, False, False, False], [False, False, True, True, False, False, False, False, False, False, False], [False, False, True, True, False, False, False, False, False, False, False], [False, False, True, True, False, False, False, False, False, False, False], [False, False, True, True, False, False, False, False, False, False, False], [False, False, True, True, True, False, False, False, False, False, False], [False, False, False, True, True, False, True, True, True, False, False], [False, False, False, True, True, False, False, False, False, False, False], [False, False, False, True, True, False, False, False, False, False, False], [True, False, False, True, True, False, False, False, False, False, False], [True, False, False, True, False, False, False, False, False, False, False], [True, False, False, True, False, False, False, False, False, False, False], [True, False, False, True, False, False, False, False, False, False, False], [True, False, False, True, True, False, False, False, False, False, False], [True, False, False, True, True, False, False, False, False, False, False], [True, False, False, True, True, False, False, False, False, False, False], [True, False, False, True, True, False, True, False, False, True, False], [False, True, True, True, True, False, True, True, False, False, False], [False, True, True, True, True, False, True, False, False, False, True], [False, True, True, True, True, False, True, False, False, False, False], [False, True, True, True, True, False, True, False, False, False, False], [False, True, True, True, True, False, True, False, False, False, False], [False, True, True, True, True, True, True, False, False, True, False], [False, True, True, True, True, True, True, False, False, False, False]])
    sche1 = np.array([[0.0, 0.0, 0.1, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.1, 0.0, 0.0, 0.65, 0.45, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.1, 0.0, 0.32, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.1, 0.0, 0.32, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.01, 0.0, 0.32, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.1, 0.0, 0.32, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.01, 0.0, 0.32, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.1, 0.0, 0.32, 0.0, 0.0, 0.0, 0.95], [0.0, 0.0, 0.0, 0.2, 0.01, 0.17, 0.32, 0.0, 0.0, 0.0, 0.95], [0.6, 0.0, 0.0, 0.2, 0.1, 0.17, 0.32, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.01, 0.0, 0.32, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.1, 0.0, 0.32, 0.0, 0.0, 0.0, 0.0], [3.1, 0.0, 0.0, 0.2, 0.1, 0.17, 0.32, 0.0, 0.0, 0.0, 0.0], [3.1, 0.0, 0.0, 0.2, 0.1, 0.17, 0.32, 0.65, 0.0, 0.725, 0.0], [0.0, 2.4, 0.0, 0.2, 0.1, 0.17, 0.32, 0.0, 0.0, 0.0, 0.0], [0.0, 0.2, 1.6, 0.2, 0.1, 0.17, 0.32, 0.0, 0.0, 0.0, 0.0], [0.0, 0.2, 0.1, 0.2, 0.1, 0.0, 0.32, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.1, 0.0, 0.32, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]])
    sche2 = np.array([[0.0, 0.0, 0.1, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.1, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.1, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.1, 0.17, 0.2, 0.65, 0.45, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.2, 0.0, 0.2, 0.1, 0.17, 0.2, 0.0, 0.0, 0.0, 0.0], [0.0, 2.4, 0.0, 0.2, 0.1, 0.17, 0.2, 0.0, 0.0, 0.725, 0.95], [0.0, 0.2, 1.6, 0.2, 0.1, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0], [0.0, 0.2, 0.1, 0.2, 0.1, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0], [0.0, 0.2, 0.1, 0.2, 0.1, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.1, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.01, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0]])
    sche3 = np.array([[0.0, 0.0, 0.1, 0.2, 0.1, 0.17, 0.24, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.01, 0.0, 0.24, 0.65, 0.45, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.1, 0.0, 0.24, 0.0, 0.0, 0.725, 0.0], [0.0, 0.2, 1.6, 0.2, 0.1, 0.0, 0.24, 0.65, 0.0, 0.0, 0.0], [0.0, 0.2, 0.1, 0.2, 0.1, 0.0, 0.24, 0.0, 0.0, 0.0, 0.95], [0.0, 0.2, 0.1, 0.2, 0.1, 0.0, 0.24, 0.0, 0.0, 0.0, 0.0], [0.0, 2.4, 0.1, 0.2, 0.1, 0.0, 0.24, 0.0, 0.0, 0.0, 0.0], [0.0, 0.2, 0.1, 0.2, 0.1, 0.0, 0.24, 0.0, 0.0, 0.0, 0.0], [0.0, 0.2, 0.1, 0.2, 0.1, 0.17, 0.24, 0.0, 0.0, 0.725, 0.0], [0.0, 0.2, 0.1, 0.2, 0.1, 0.17, 0.24, 0.0, 0.0, 0.0, 0.0]])
    DF_PMIN = pd.read_csv("Pmin.csv", header = None)
    DF_PMAX = pd.read_csv("Pmax.csv", header = None)
    low1  = np.asarray([DF_PMIN.iloc[0, 1+a] for a in range(11)])
    high1 = np.asarray([DF_PMAX.iloc[0, 1+a] for a in range(11)])
    low2  = np.asarray([DF_PMIN.iloc[1, 1+a] for a in range(11)])
    high2 = np.asarray([DF_PMAX.iloc[1, 1+a] for a in range(11)])
    low3  = np.asarray([DF_PMIN.iloc[2, 1+a] for a in range(11)])
    high3 = np.asarray([DF_PMAX.iloc[2, 1+a] for a in range(11)])
    un1 = np.array([0.5, 0.5, 0.6, 1.0, 0.7, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0])
    un2 = np.array([0.7, 0.7, 0.6, 1.0, 0.9, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0])
    un3 = np.array([0.2, 0.2, 0.3, 1.0, 0.5, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0])
    DF_BCAP = pd.read_csv('bcap.csv', header = None)
    bcap1 = DF_BCAP.iloc[0,1]
    bcap2 = DF_BCAP.iloc[1,1]
    bcap3 = DF_BCAP.iloc[2,1]
    cd = 0.003
	# electricity bill structure
    business_below = np.array([0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228])
    business_above = np.array([0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456])
    # 1 commercial building
	# plan for 24 hr with:
	# temperature dynamics evaluated every 10 min
	# control actions taken every hour
	# disturbance information updated every hour
	# total cost computed on an hourly basis
    HeatSource = np.array([5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 25.0, 25.0, 25.0, 25.0, 25.0, 25.0, 25.0, 25.0, 25.0, 25.0, 25.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0])
    RoomTempLow = np.array([19.0, 19.0, 19.0, 19.0, 19.0, 19.0, 19.0, 21.0, 21.0, 21.0, 21.0, 21.0, 21.0, 21.0, 21.0, 21.0, 21.0, 21.0, 19.0, 19.0, 19.0, 19.0, 19.0, 19.0])
    RoomTempHigh = np.array([30.0, 30.0, 30.0, 30.0, 30.0, 30.0, 30.0, 25.8, 25.8, 25.8, 25.8, 25.8, 25.8, 25.8, 25.8, 25.8, 25.8, 25.8, 30.0, 30.0, 30.0, 30.0, 30.0, 30.0])
    InitialTemp = np.array([28.3, 28.2, 28.6])
	# electricity bill structure
    industry_below = np.array([0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154])
    industry_above = np.array([0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308])
    # 1 industrial plant
	# plan for 24 hr with:
	# electrolyzer and fuel cell dynamics evaluated every 15 min
	# control actions taken every hour
	# ambient temperature updated every hour
	# total cost computed on an hourly basis
    cf = np.array([0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462])
    T0 = 99
    
    DF_PV = pd.read_csv("PV_parameters.csv", header=None )
    # change the following parameters only if you understand their physical meanings
    Il_ref = DF_PV.iloc[0,1] # Short circuit current at SRC (standard rating condition) [A]
    alpha_sc = DF_PV.iloc[1,1] # Temperature coefficient for short circuit current [A/K]
    a_ref = DF_PV.iloc[2,1] # ideality factor parameter at SRC [eV]
    Io_ref = DF_PV.iloc[3,1] # Diode reverse saturation current at SRC [A]
    Rs_ref = DF_PV.iloc[4,1] # Series resistance at SRC [Ohm]
    Rsh_ref = DF_PV.iloc[5,1] # Shunt resistance at SRC [Ohm]
    Tc_ref = DF_PV.iloc[6,1] # Cell temperature at SRC [K]
    G_ref = DF_PV.iloc[7,1] # Radiation at SRC conditions [W/m^2]
    Eg_ref = DF_PV.iloc[8,1] # Material band gap at SRC [eV]
    k = 8.6173324e-5 # Boltzmann constant [eV/K]
    Ns = 3000

    Nr = 60

    C1 = 9.356e5   #[kJ/C]
    C2 = 2.970e6   #[kJ/C]
    C3 = 6.695e5   #[kJ/C]
    K1 = 16.48     #[kW/C]
    K2 = 108.5     #[kW/C]
    K3 = 5         #[kW/C]
    K4 = 30.5      #[kW/C]
    K5 = 23.04     #[kW/C]
    A = np.array([[-1/C1*(K1+K2+K3+K5), 1/C1*(K1+K2), 1/C1*K5], [1/C2*(K1+K2), -1/C2*(K1+K2), 0], [1/C3*K5, 0, -1/C3*(K4+K5)]])
    B = np.array([[-1/C1], [0], [0]])
    C = np.array([[1/C1*K3, 1/C1, 1/C1], [0, 1/C2, 0], [1/C3*K4, 0, 0]])
    Nc = 1
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
    # polarization curve parameters:
    U0 = 33.18             #[V]
    E1 = -0.013            #[V/C]
    E2 = -1.57             #[1]
    I0 = 8.798             #[A]
    R = -2.04              #[Ohm*C]
    # operation parameters:
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
    print(system(AirTemp, Radiation, alpha_sc, a_ref,
     Il_ref, Io_ref, Rs_ref, Rsh_ref, Tc_ref,
      G_ref, Eg_ref, k, Ns, household_below, household_above,
       flex1, sche1, low1, high1, un1, bcap1, flex2, sche2,
        low2, high2, un2, bcap2, flex3, sche3, low3, high3,
         un3, bcap3, cd, Nr, business_below, business_above,
          HeatSource, RoomTempLow, RoomTempHigh, InitialTemp,
           C1, C2, C3, K1, K2, K3, K4, K5, A, B, C, Nc, industry_below,
            industry_above, cf, T0,  F, z1, dG0, dH, r1, r2, s, t1, t2,
             t3, f1, f2, h_cond, h_conv, Ar, nc1, Ct, Rt, taut, Qcw, Tcwi,
              Ccw, z2, U0, E1, E2, I0, R, nc2, eta, Tlow, Thigh, Ilow,
               Ihigh, quantity, Ni))

    now = datetime.now()

    current_time = now.strftime("%H:%M:%S")
    print("Current Time =", current_time)