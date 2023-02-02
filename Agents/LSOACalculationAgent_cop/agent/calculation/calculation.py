################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

# The purpose of this module is to define functions to calculate COP based on Temperature
# And another way around.

import numpy as np

def COP_degreeC(temp, hp_efficiency:float = 0.35, T_H: float = 45 +273.15):
    '''
    Based on a given temperature to calculate the COP
    Note: COP = (hp_efficiency * T_H) / (T_H - T_C), where the input temperature is represented as T_C
    T_H, hp_efficiency are hypothesd as constant, which have default value as 318.15 and 0.35
    respectfully. I suggest to check if that default value is up to date or if that hypothesis is 
    valid in your case
    '''
    COP = hp_efficiency * T_H / (T_H -273.15 - temp)
    
    COP = np.round(COP,3)
    
    return COP

def COP_kelvin(temp, hp_efficiency:float = 0.35, T_H: float = 45 +273.15):
    celsius_temps = np.subtract(temp, 273.15)
    COP = COP_degreeC(celsius_temps,hp_efficiency,T_H)
    return COP

def COP_Fahrenheit(temp, hp_efficiency:float = 0.35, T_H: float = 45 +273.15):
    celsius_temps = (np.subtract(temp, 32) * 5/9)
    COP = COP_degreeC(celsius_temps,hp_efficiency,T_H)
    return COP

def T_from_COP(COP):
    '''
    Return temperature based on a given COP
    '''
    T = 45 - ((45+273.15)/(COP/0.35))

    return T

