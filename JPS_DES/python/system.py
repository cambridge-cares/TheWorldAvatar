import numpy as np
from scipy.optimize import *
import math
from commercial import commercial
from residential import residential 
from industrial import industrial
from solarRadiation import solar 


def system(AirTemp, Radiation, alpha_sc, a_ref, Il_ref, Io_ref, Rs_ref, Rsh_ref, Tc_ref, G_ref, Eg_ref, k, Ns, household_below, household_above, flex1, sche1, low1, high1, unwill1, bcap1, flex2, sche2, low2, high2, unwill2, bcap2, flex3, sche3, low3, high3, unwill3, bcap3, cd, Nr, business_below, business_above, HeatSource, RoomTempLow, RoomTempHigh, InitialTemp, C1, C2, C3, K1, K2, K3, K4, K5, A, B, C, Nc, industry_below, industry_above, cf, T0,  F, z1, dG0, dH, r1, r2, s, t1, t2, t3, f1, f2, h_cond, h_conv, Ar, nc1, Ct, Rt, taut, Qcw, Tcwi, Ccw, z2, U0, E1, E2, I0, R, nc2, eta, Tlow, Thigh, Ilow, Ihigh, quantity, Ni):
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
            out0 = residential(totGen, compute_aggregate(0, load), household_below, household_above, flex1, sche1, low1, high1, unwill1, bcap1, cd, Nr, penetration[0])
        except:
            out0 = residential(totGen, compute_aggregate(0, load), household_below, household_above, flex1, sche1, low1, high1, unwill1, bcap1, cd, Nr)
        load[0] = out0[2] 

        try:
            out1 = residential(totGen, compute_aggregate(1, load), household_below, household_above, flex2, sche2, low2, high2, unwill2, bcap2, cd, Nr, penetration[1])
        except:
            out1 = residential(totGen, compute_aggregate(1, load), household_below, household_above, flex2, sche2, low2, high2, unwill2, bcap2, cd, Nr)
        load[1] = out1[2]

        try:
            out2 = residential(totGen, compute_aggregate(2, load), household_below, household_above, flex3, sche3, low3, high3, unwill3, bcap3, cd, Nr, penetration[2])
        except:
            out2 = residential(totGen, compute_aggregate(2, load), household_below, household_above, flex3, sche3, low3, high3, unwill3, bcap3, cd, Nr)
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
    AirTemp = np.array([28.45, 28.3 , 28.11, 27.9 , 27.74, 27.68, 27.59, 27.54, 26.59, 23.82, 26.7 , 28.23, 28.92, 29.15, 30.56, 30.45, 30.65, 31.06, 30.97, 30.76, 30.03, 29.76, 29.48, 28.99])
    Radiation = np.array([1.700e-02, 2.100e-02, 1.500e-02, 2.000e-02, 1.800e-02, 1.500e-02, 2.400e-02, 3.360e-01, 2.343e+01, 1.193e+02, 4.061e+02, 5.386e+02, 6.513e+02, 7.080e+02, 7.000e+02, 6.656e+02, 5.661e+02, 4.238e+02, 2.571e+02, 8.630e+01, 1.071e+00, 1.900e-02, 1.800e-02, 1.700e-02])

    household_below = np.array([0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002, 0.0002])
    household_above = np.array([0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004])
    flex1 = np.array([[False, False, True, True, True, False, False, False, False, False, False], [False, False, True, True, True, False, False, False, False, False, False], [False, False, True, True, True, False, False, False, False, False, False], [False, False, True, True, True, False, False, False, False, False, False], [False, False, True, True, True, False, False, False, False, False, False], [False, False, True, True, True, False, False, False, False, False, False], [False, False, False, True, True, False, False, True, True, False, False], [False, False, False, True, True, False, True, False, False, False, False], [False, False, False, True, True, False, True, False, False, False, False], [False, False, False, True, True, False, True, False, False, False, False], [False, False, False, True, True, False, True, False, False, False, False], [False, False, False, True, True, False, True, False, False, False, False], [False, False, False, True, True, False, True, False, False, False, True], [False, False, False, True, True, True, True, False, False, False, True], [True, False, False, True, True, True, True, False, False, False, False], [True, False, False, True, True, False, True, False, False, False, False], [True, False, False, True, True, False, True, False, False, False, False], [True, False, False, True, True, True, True, False, False, False, False], [True, False, False, True, True, True, True, True, False, True, False], [False, True, False, True, True, True, True, False, False, False, False], [False, True, True, True, True, True, True, False, False, False, False], [False, True, True, True, True, False, True, False, False, False, False], [False, False, True, True, True, False, True, False, False, False, False], [False, False, True, True, True, False, False, False, False, False, False]])
    flex2 = np.array([[False, False, True, True, False, False, False, False, False, False, False], [False, False, True, True, False, False, False, False, False, False, False], [False, False, True, True, False, False, False, False, False, False, False], [False, False, True, True, True, False, False, False, False, False, False], [False, False, True, True, True, False, False, False, False, False, False], [False, False, True, True, True, False, True, False, False, False, False], [False, False, True, True, True, False, True, False, False, False, False], [False, False, False, True, True, True, True, True, True, False, False], [False, False, False, True, False, False, False, False, False, False, False], [True, False, False, True, False, False, False, False, False, False, False], [True, False, False, True, False, False, False, False, False, False, False], [True, False, False, True, False, False, False, False, False, False, False], [True, False, False, True, False, False, False, False, False, False, False], [True, False, False, True, False, False, False, False, False, False, False], [True, False, False, True, True, False, False, False, False, False, False], [True, False, False, True, True, False, False, False, False, False, False], [True, False, False, True, True, False, False, False, False, False, False], [False, True, False, True, True, True, True, False, False, False, False], [False, True, False, True, True, True, True, False, False, True, True], [False, True, True, True, True, False, True, False, False, False, False], [False, True, True, True, True, False, True, False, False, False, False], [False, True, True, True, True, False, True, False, False, False, False], [False, False, True, True, True, False, True, False, False, False, False], [False, False, True, True, True, False, True, False, False, False, False]])
    flex3 = np.array([[False, False, True, True, True, True, True, False, False, False, False], [False, False, True, True, False, False, False, False, False, False, False], [False, False, True, True, False, False, False, False, False, False, False], [False, False, True, True, False, False, False, False, False, False, False], [False, False, True, True, False, False, False, False, False, False, False], [False, False, True, True, True, False, False, False, False, False, False], [False, False, False, True, True, False, True, True, True, False, False], [False, False, False, True, True, False, False, False, False, False, False], [False, False, False, True, True, False, False, False, False, False, False], [True, False, False, True, True, False, False, False, False, False, False], [True, False, False, True, False, False, False, False, False, False, False], [True, False, False, True, False, False, False, False, False, False, False], [True, False, False, True, False, False, False, False, False, False, False], [True, False, False, True, True, False, False, False, False, False, False], [True, False, False, True, True, False, False, False, False, False, False], [True, False, False, True, True, False, False, False, False, False, False], [True, False, False, True, True, False, True, False, False, True, False], [False, True, True, True, True, False, True, True, False, False, False], [False, True, True, True, True, False, True, False, False, False, True], [False, True, True, True, True, False, True, False, False, False, False], [False, True, True, True, True, False, True, False, False, False, False], [False, True, True, True, True, False, True, False, False, False, False], [False, True, True, True, True, True, True, False, False, True, False], [False, True, True, True, True, True, True, False, False, False, False]])
    sche1 = np.array([[0.0, 0.0, 0.1, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.1, 0.0, 0.0, 0.65, 0.45, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.1, 0.0, 0.32, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.1, 0.0, 0.32, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.01, 0.0, 0.32, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.1, 0.0, 0.32, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.01, 0.0, 0.32, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.1, 0.0, 0.32, 0.0, 0.0, 0.0, 0.95], [0.0, 0.0, 0.0, 0.2, 0.01, 0.17, 0.32, 0.0, 0.0, 0.0, 0.95], [0.6, 0.0, 0.0, 0.2, 0.1, 0.17, 0.32, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.01, 0.0, 0.32, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.1, 0.0, 0.32, 0.0, 0.0, 0.0, 0.0], [3.1, 0.0, 0.0, 0.2, 0.1, 0.17, 0.32, 0.0, 0.0, 0.0, 0.0], [3.1, 0.0, 0.0, 0.2, 0.1, 0.17, 0.32, 0.65, 0.0, 0.725, 0.0], [0.0, 2.4, 0.0, 0.2, 0.1, 0.17, 0.32, 0.0, 0.0, 0.0, 0.0], [0.0, 0.2, 1.6, 0.2, 0.1, 0.17, 0.32, 0.0, 0.0, 0.0, 0.0], [0.0, 0.2, 0.1, 0.2, 0.1, 0.0, 0.32, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.1, 0.0, 0.32, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]])
    sche2 = np.array([[0.0, 0.0, 0.1, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.1, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.1, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.1, 0.17, 0.2, 0.65, 0.45, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.2, 0.0, 0.2, 0.1, 0.17, 0.2, 0.0, 0.0, 0.0, 0.0], [0.0, 2.4, 0.0, 0.2, 0.1, 0.17, 0.2, 0.0, 0.0, 0.725, 0.95], [0.0, 0.2, 1.6, 0.2, 0.1, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0], [0.0, 0.2, 0.1, 0.2, 0.1, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0], [0.0, 0.2, 0.1, 0.2, 0.1, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.1, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.01, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0]])
    sche3 = np.array([[0.0, 0.0, 0.1, 0.2, 0.1, 0.17, 0.24, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.1, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.01, 0.0, 0.24, 0.65, 0.45, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.2, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.01, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], [0.6, 0.0, 0.0, 0.2, 0.1, 0.0, 0.24, 0.0, 0.0, 0.725, 0.0], [0.0, 0.2, 1.6, 0.2, 0.1, 0.0, 0.24, 0.65, 0.0, 0.0, 0.0], [0.0, 0.2, 0.1, 0.2, 0.1, 0.0, 0.24, 0.0, 0.0, 0.0, 0.95], [0.0, 0.2, 0.1, 0.2, 0.1, 0.0, 0.24, 0.0, 0.0, 0.0, 0.0], [0.0, 2.4, 0.1, 0.2, 0.1, 0.0, 0.24, 0.0, 0.0, 0.0, 0.0], [0.0, 0.2, 0.1, 0.2, 0.1, 0.0, 0.24, 0.0, 0.0, 0.0, 0.0], [0.0, 0.2, 0.1, 0.2, 0.1, 0.17, 0.24, 0.0, 0.0, 0.725, 0.0], [0.0, 0.2, 0.1, 0.2, 0.1, 0.17, 0.24, 0.0, 0.0, 0.0, 0.0]])
    low1  = np.array([0.6, 0.2, 0.1, 0.2, 0.01, 0.17, 0.32, 0.65, 0.45, 0.725, 0.95])
    high1 = np.array([3.9, 3.0, 3.1, 0.2, 0.1, 0.17, 0.32, 0.65, 0.45, 0.725, 0.95])
    low2  = np.array([0.6, 0.2, 0.1, 0.2, 0.01, 0.17, 0.2, 0.65, 0.45, 0.725, 0.95])
    high2 = np.array([3.9, 3.0, 3.1, 0.2, 0.1, 0.17, 0.2, 0.65, 0.45, 0.725, 0.95])
    low3  = np.array([0.6, 0.2, 0.1, 0.2, 0.01, 0.17, 0.24, 0.65, 0.45, 0.725, 0.95])
    high3 = np.array([3.9, 3.0, 3.1, 0.2, 0.1, 0.17, 0.24, 0.65, 0.45, 0.725, 0.95])
    unwill1 = np.array([0.5, 0.5, 0.6, 1.0, 0.7, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0])
    unwill2 = np.array([0.7, 0.7, 0.6, 1.0, 0.9, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0])
    unwill3 = np.array([0.2, 0.2, 0.3, 1.0, 0.5, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0])
    bcap1 = 6
    bcap2 = 4
    bcap3 = 2
    cd = 0.003

    business_below = np.array([0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228, 0.000228])
    business_above = np.array([0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456, 0.000456])
    HeatSource = np.array([5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 25.0, 25.0, 25.0, 25.0, 25.0, 25.0, 25.0, 25.0, 25.0, 25.0, 25.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0])
    RoomTempLow = np.array([19.0, 19.0, 19.0, 19.0, 19.0, 19.0, 19.0, 21.0, 21.0, 21.0, 21.0, 21.0, 21.0, 21.0, 21.0, 21.0, 21.0, 21.0, 19.0, 19.0, 19.0, 19.0, 19.0, 19.0])
    RoomTempHigh = np.array([30.0, 30.0, 30.0, 30.0, 30.0, 30.0, 30.0, 25.8, 25.8, 25.8, 25.8, 25.8, 25.8, 25.8, 25.8, 25.8, 25.8, 25.8, 30.0, 30.0, 30.0, 30.0, 30.0, 30.0])
    InitialTemp = np.array([28.3, 28.2, 28.6])

    industry_below = np.array([0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154, 0.000154])
    industry_above = np.array([0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308, 0.000308])
    cf = np.array([0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462, 0.00462])
    T0 = 99
    # change the following parameters only if you understand their physical meanings
    alpha_sc = 0.002132
    a_ref = 1.9136
    Il_ref = 5.995
    Io_ref = 5.194e-11
    Rs_ref = 0.321
    Rsh_ref = 367.11
    Tc_ref = 25 + 273.15
    G_ref = 1000
    Eg_ref = 1.121
    k = 8.6173324e-5
    Ns = 3000

    Nr = 60

    C1 = 9.356e5
    C2 = 2.970e6
    C3 = 6.695e5
    K1 = 16.48
    K2 = 108.5
    K3 = 5
    K4 = 30.5
    K5 = 23.04
    A = np.array([[-1/C1*(K1+K2+K3+K5), 1/C1*(K1+K2), 1/C1*K5], [1/C2*(K1+K2), -1/C2*(K1+K2), 0], [1/C3*K5, 0, -1/C3*(K4+K5)]])
    B = np.array([[-1/C1], [0], [0]])
    C = np.array([[1/C1*K3, 1/C1, 1/C1], [0, 1/C2, 0], [1/C3*K4, 0, 0]])
    Nc = 1

    F = 96485.34
    z1 = 2
    dG0 = 237e3
    dH = 286e3
    r1 = 8.05e-5
    r2 = -2.5e-7
    s = 0.185
    t1 = -0.1002
    t2 = 8.424
    t3 = 247.3
    f1 = 250e2
    f2 = 0.98
    h_cond = 7
    h_conv = 0.02
    Ar = 0.25 
    nc1 = 21
    Ct = 625e3
    Rt = 0.167
    taut = Ct*Rt
    Qcw = 0.6/3600
    Tcwi = 14.5
    Ccw = 4.18e3*1e3*Qcw
    z2 = 2
    U0 = 33.18
    E1 = -0.013
    E2 = -1.57
    I0 = 8.798
    R = -2.04
    nc2 = 35
    eta = 0.7
    Tlow = 24
    Thigh = 72
    Ilow = Tlow**2*E1/R
    Ihigh = Thigh**2*E1/R
    quantity = 2500
    Ni = 30
    print(system(AirTemp, Radiation, alpha_sc, a_ref,
     Il_ref, Io_ref, Rs_ref, Rsh_ref, Tc_ref,
      G_ref, Eg_ref, k, Ns, household_below, household_above,
       flex1, sche1, low1, high1, unwill1, bcap1, flex2, sche2,
        low2, high2, unwill2, bcap2, flex3, sche3, low3, high3,
         unwill3, bcap3, cd, Nr, business_below, business_above,
          HeatSource, RoomTempLow, RoomTempHigh, InitialTemp,
           C1, C2, C3, K1, K2, K3, K4, K5, A, B, C, Nc, industry_below,
            industry_above, cf, T0,  F, z1, dG0, dH, r1, r2, s, t1, t2,
             t3, f1, f2, h_cond, h_conv, Ar, nc1, Ct, Rt, taut, Qcw, Tcwi,
              Ccw, z2, U0, E1, E2, I0, R, nc2, eta, Tlow, Thigh, Ilow,
               Ihigh, quantity, Ni))

    now = datetime.now()

    current_time = now.strftime("%H:%M:%S")
    print("Current Time =", current_time)