# ### 1.5 function that post process the ccs results

import numpy as np
import pandas as pd

# define a function that could calculate the overall annual emission and lock emission

def bau_ccs_post (df):
    
    coal_annual_existing_emission = df.loc[:,('coal_power_annual_emission_existing')].values
    coal_annual_new_emission = df.loc[:,('coal_power_annual_emission_new')].values
    gas_annual_existing_emission = df.loc[:,('gas_power_annual_emission_existing')].values
    gas_annual_new_emission = df.loc[:,('gas_power_annual_emission_new')].values
    oil_annual_existing_emission = df.loc[:,('oil_power_annual_emission_existing')].values
    oil_annual_new_emission = df.loc[:,('oil_power_annual_emission_new')].values
        
    coal_lock_existing_emission = df.loc[:,('coal_power_lock_emission_existing')].values
    coal_lock_new_emission = df.loc[:,('coal_power_lock_emission_new')].values
    gas_lock_existing_emission = df.loc[:,('gas_power_lock_emission_existing')].values
    gas_lock_new_emission = df.loc[:,('gas_power_lock_emission_new')].values
    oil_lock_existing_emission = df.loc[:,('oil_power_lock_emission_existing')].values
    oil_lock_new_emission = df.loc[:,('oil_power_lock_emission_new')].values
    
    coal_overall_lock_emission = np.zeros(shape=(36))
    gas_overall_lock_emission = np.zeros(shape=(36))
    oil_overall_lock_emission = np.zeros(shape=(36))
        
    coal_overall_annual_emission = np.zeros(shape=(36))
    gas_overall_annual_emission = np.zeros(shape=(36))
    oil_overall_annual_emission = np.zeros(shape=(36))
    
    for i in range(36):
    
        coal_annual_exisitng = coal_annual_existing_emission[i]  
        gas_annual_exisitng = gas_annual_existing_emission[i] 
        oil_annual_exisitng = oil_annual_existing_emission[i] 
    
        coal_annual_added = 0
        gas_annual_added = 0
        oil_annual_added = 0
    
        for j in range(i+1):
    
            coal_annual_new = coal_annual_new_emission[j]
            coal_annual_added = coal_annual_added + coal_annual_new
            
            gas_annual_new = gas_annual_new_emission[j]
            gas_annual_added = gas_annual_added + gas_annual_new
            
            oil_annual_new = oil_annual_new_emission[j]
            oil_annual_added = oil_annual_added + oil_annual_new
        
    
        coal_overall_annual_emission[i] = coal_annual_exisitng  + coal_annual_added
        df.loc[:,('coal_annual_emission')] = coal_overall_annual_emission
        
        gas_overall_annual_emission[i] = gas_annual_exisitng  + gas_annual_added
        df.loc[:,('gas_annual_emission')] = gas_overall_annual_emission
        
        oil_overall_annual_emission[i] = oil_annual_exisitng  + oil_annual_added
        df.loc[:,('oil_annual_emission')] = oil_overall_annual_emission
    
    
    for i in range(36):
    
        coal_lock_exisitng = coal_lock_existing_emission[i]  
        gas_lock_exisitng = gas_lock_existing_emission[i] 
        oil_lock_exisitng = oil_lock_existing_emission[i] 
    
        coal_lock_added = 0
        gas_lock_added = 0
        oil_lock_added = 0
    
        for j in range(i+1):
    
            coal_lock_new = coal_lock_new_emission[j]* (1-0.025*(i-j))
            coal_lock_added = coal_lock_added + coal_lock_new
            
            gas_lock_new = gas_lock_new_emission[j]* (1-0.025*(i-j))
            gas_lock_added = gas_lock_added + gas_lock_new
            
            oil_lock_new = oil_lock_new_emission[j]* (1-0.025*(i-j))
            oil_lock_added = oil_lock_added + oil_lock_new
        
    
        coal_overall_lock_emission[i] = coal_lock_exisitng  + coal_lock_added
        df.loc[:,('coal_lock_emission')] = coal_overall_lock_emission
        
        gas_overall_lock_emission[i] = gas_lock_exisitng  + gas_lock_added
        df.loc[:,('gas_lock_emission')] = gas_overall_lock_emission
        
        oil_overall_lock_emission[i] = oil_lock_exisitng  + oil_lock_added
        df.loc[:,('oil_lock_emission')] = oil_overall_lock_emission
    
    return df
    

# define a function that could select the useful columns from the table

def ccs_results (ccs):

    ccs_cols = ['year',
         'coal_power_capacity_GW','coal_power_capacity_existing','coal_power_capacity_new',
         'coal_annual_emission','coal_power_annual_emission_existing','coal_power_annual_emission_new',
         'coal_lock_emission','coal_power_lock_emission_existing','coal_power_lock_emission_new',
         'gas_power_capacity_GW','gas_power_capacity_existing','gas_power_capacity_new',
         'gas_annual_emission','gas_power_annual_emission_existing','gas_power_annual_emission_new',
         'gas_lock_emission','gas_power_lock_emission_existing','gas_power_lock_emission_new',
         'oil_power_capacity_GW','oil_power_capacity_existing','oil_power_capacity_new',
         'oil_annual_emission','oil_power_annual_emission_existing','oil_power_annual_emission_new',
         'oil_lock_emission','oil_power_lock_emission_existing','oil_power_lock_emission_new',
         'coal_power_capacity_new1','coal_power_annual_emission_new1','coal_power_annual_emission_new1',
         'coal_power_capacity_new2','coal_power_annual_emission_new2','coal_power_annual_emission_new2',
         'gas_power_capacity_new1','gas_power_annual_emission_new1','gas_power_annual_emission_new1',
         'gas_power_capacity_new2','gas_power_annual_emission_new2','gas_power_annual_emission_new2',
         'oil_power_capacity_new1','oil_power_annual_emission_new1','oil_power_annual_emission_new1',
         'oil_power_capacity_new2','oil_power_annual_emission_new2','oil_power_annual_emission_new2']

    ccs = ccs[ccs_cols]

    return ccs