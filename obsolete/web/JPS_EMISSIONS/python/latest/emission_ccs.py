# ### 1.4 define function that could calculate the fleet emission with ccs

import numpy as np
import pandas as pd

# function that calculate emission from ccs possible newly plant


def  emission_aggregation_ccs_1 (m, n, i): 
    
    if m.shape[0] == 0:
       # if there is empty dataframe input, set all values to zero then
      return n
         
    else:
      # power_capacity = m.fuel_used.unique()[0] + '_' + 'power_capacity_GW'
      plant_capacity = m.fuel_used.unique()[0] + '_' + 'plant_capacity_new1'
      plant_annual_emission = m.fuel_used.unique()[0] + '_' + 'plant_annual_emission_new1'
      plant_lock_emission = m.fuel_used.unique()[0] + '_' + 'plant_lock_emission_new1'
      # power_annual_emission = m.fuel_used.unique()[0] + '_' + 'power_annual_emission_Gt'
      # power_lock_emission = m.fuel_used.unique()[0] + '_' + 'power_lock_emission_Gt'

      # get the overall capacity and write it to corresponding cell
      n.at[i,plant_capacity] = m['capacity_MW'].sum()/1000
  
      # get the overall annual emission and write it to corresponding cell
      n.at[i,plant_annual_emission] = m['annual_emission'].sum()/1000000000

      # plant younger than 40 years, serve till 40; plant overall 40 years, serve 5 more years
      remain_age = (m['age']//40 != 0).astype(int) * (5-0)  + (40 - m['age'] ) *  (m['age']//40 == 0).astype(int)
      m['remain_age'] = remain_age

      # lock in emission 
      m['lock_emission'] = m['remain_age'] * m['annual_emission']
      lock_emission = m['lock_emission'].sum()/1000000000
      n.at[i,plant_lock_emission] = lock_emission

      return n


# function that calculate emission from ccs not possible newly plants


def  emission_aggregation_ccs_2 (m, n, i): 
    
   if m.shape[0] == 0:
       # if there is empty dataframe input, set all values to zero then
       return n
  
   else:
       # power_capacity = m.fuel_used.unique()[0] + '_' + 'power_capacity_GW'
       plant_capacity = m.fuel_used.unique()[0] + '_' + 'plant_capacity_new2'
       plant_annual_emission = m.fuel_used.unique()[0] + '_' + 'plant_annual_emission_new2'
       plant_lock_emission = m.fuel_used.unique()[0] + '_' + 'plant_lock_emission_new2'
       # power_annual_emission = m.fuel_used.unique()[0] + '_' + 'power_annual_emission_Gt'
       # power_lock_emission = m.fuel_used.unique()[0] + '_' + 'power_lock_emission_Gt'

       # get the overall capacity and write it to corresponding cell
       n.at[i,plant_capacity] = m['capacity_MW'].sum()/1000
  
       # get the overall annual emission and write it to corresponding cell
       n.at[i,plant_annual_emission] = m['annual_emission'].sum()/1000000000

       # plant younger than 40 years, serve till 40; plant overall 40 years, serve 5 more years
       remain_age = (m['age']//40 != 0).astype(int) * (5-0)  + (40 - m['age'] ) *  (m['age']//40 == 0).astype(int)
       m['remain_age'] = remain_age

       # lock in emission 
       m['lock_emission'] = m['remain_age'] * m['annual_emission']
       lock_emission = m['lock_emission'].sum()/1000000000
       n.at[i,plant_lock_emission] = lock_emission

       return n  



# define function that can calculate the fleet emission in bau scenario based on existing plant and new plant
# new plants are further divided into ccs possible and ccs not possible parts
# m is the ccs possible newly built fleet at year i, n is the ccs not possible newly built fleet from retired plants at year i, 
# p is the result csv

def  emission_aggregation_ccs (m, n, p, i): 
    
    if  m.shape[0] == 0 and n.shape[0] == 0:
        # if there is empty dataframe input, do not change the result csv
        # calculate emission from remaining parts
        return p
    
    elif m.shape[0] != 0 and n.shape[0] == 0:
    
        power_capacity_new = m.fuel_used.unique()[0] + '_' + 'power_capacity_new'
        power_annual_emission_new = m.fuel_used.unique()[0] + '_' + 'power_annual_emission_new'
        power_lock_emission_new = m.fuel_used.unique()[0] + '_' + 'power_lock_emission_new'
        plant_capacity_new = m.fuel_used.unique()[0] + '_' + 'plant_capacity_new'
        plant_capacity_new1 = m.fuel_used.unique()[0] + '_' + 'plant_capacity_new1'
        plant_capacity_new2 = m.fuel_used.unique()[0] + '_' + 'plant_capacity_new2'
        plant_annual_emission_new = m.fuel_used.unique()[0] + '_' + 'plant_annual_emission_new'
        plant_annual_emission_new1 = m.fuel_used.unique()[0] + '_' + 'plant_annual_emission_new1'
        plant_annual_emission_new2 = m.fuel_used.unique()[0] + '_' + 'plant_annual_emission_new2'
        plant_lock_emission_new = m.fuel_used.unique()[0] + '_' + 'plant_lock_emission_new'
        plant_lock_emission_new1 = m.fuel_used.unique()[0] + '_' + 'plant_lock_emission_new1'
        plant_lock_emission_new2 = m.fuel_used.unique()[0] + '_' + 'plant_lock_emission_new2'
        
        # get the overall and existing annual emission and lock emission notation
        existing_annual_emission = m.fuel_used.unique()[0] + '_' + 'power_annual_emission_existing'
        existing_lock_emission = m.fuel_used.unique()[0] + '_' + 'power_lock_emission_existing'
        power_capacity_new1 = m.fuel_used.unique()[0] + '_' + 'power_capacity_new1'
        power_capacity_new2 = m.fuel_used.unique()[0] + '_' + 'power_capacity_new2'
        power_annual_emission_new1 = m.fuel_used.unique()[0] + '_' + 'power_annual_emission_new1'
        power_annual_emission_new2 = m.fuel_used.unique()[0] + '_' + 'power_annual_emission_new2'
        power_lock_emission_new1 = m.fuel_used.unique()[0] + '_' + 'power_lock_emission_new1'
        power_lock_emission_new2 = m.fuel_used.unique()[0] + '_' + 'power_lock_emission_new2'


    elif m.shape[0] == 0 and n.shape[0] != 0:
    
        power_capacity_new = n.fuel_used.unique()[0] + '_' + 'power_capacity_new'
        power_annual_emission_new = n.fuel_used.unique()[0] + '_' + 'power_annual_emission_new'
        power_lock_emission_new = n.fuel_used.unique()[0] + '_' + 'power_lock_emission_new'
        plant_capacity_new = n.fuel_used.unique()[0] + '_' + 'plant_capacity_new'
        plant_capacity_new1 = n.fuel_used.unique()[0] + '_' + 'plant_capacity_new1'
        plant_capacity_new2 = n.fuel_used.unique()[0] + '_' + 'plant_capacity_new2'
        plant_annual_emission_new = n.fuel_used.unique()[0] + '_' + 'plant_annual_emission_new'
        plant_annual_emission_new1 = n.fuel_used.unique()[0] + '_' + 'plant_annual_emission_new1'
        plant_annual_emission_new2 = n.fuel_used.unique()[0] + '_' + 'plant_annual_emission_new2'
        plant_lock_emission_new = n.fuel_used.unique()[0] + '_' + 'plant_lock_emission_new'
        plant_lock_emission_new1 = n.fuel_used.unique()[0] + '_' + 'plant_lock_emission_new1'
        plant_lock_emission_new2 = n.fuel_used.unique()[0] + '_' + 'plant_lock_emission_new2'
        
        # get the overall annual emission and lock emission
        existing_annual_emission = n.fuel_used.unique()[0] + '_' + 'power_annual_emission_existing'
        existing_lock_emission = n.fuel_used.unique()[0] + '_' + 'power_lock_emission_existing'
        power_capacity_new1 = n.fuel_used.unique()[0] + '_' + 'power_capacity_new1'
        power_capacity_new2 = n.fuel_used.unique()[0] + '_' + 'power_capacity_new2'
        power_annual_emission_new1 = n.fuel_used.unique()[0] + '_' + 'power_annual_emission_new1'
        power_annual_emission_new2 = n.fuel_used.unique()[0] + '_' + 'power_annual_emission_new2'
        power_lock_emission_new1 = n.fuel_used.unique()[0] + '_' + 'power_lock_emission_new1'
        power_lock_emission_new2 = n.fuel_used.unique()[0] + '_' + 'power_lock_emission_new2'
        
        
    else: 
        
        power_capacity_new = m.fuel_used.unique()[0] + '_' + 'power_capacity_new'
        power_annual_emission_new = m.fuel_used.unique()[0] + '_' + 'power_annual_emission_new'
        power_lock_emission_new = m.fuel_used.unique()[0] + '_' + 'power_lock_emission_new'
        plant_capacity_new = m.fuel_used.unique()[0] + '_' + 'plant_capacity_new'
        plant_capacity_new1 = m.fuel_used.unique()[0] + '_' + 'plant_capacity_new1'
        plant_capacity_new2 = m.fuel_used.unique()[0] + '_' + 'plant_capacity_new2'
        plant_annual_emission_new = m.fuel_used.unique()[0] + '_' + 'plant_annual_emission_new'
        plant_annual_emission_new1 = m.fuel_used.unique()[0] + '_' + 'plant_annual_emission_new1'
        plant_annual_emission_new2 = m.fuel_used.unique()[0] + '_' + 'plant_annual_emission_new2'
        plant_lock_emission_new = m.fuel_used.unique()[0] + '_' + 'plant_lock_emission_new'
        plant_lock_emission_new1 = m.fuel_used.unique()[0] + '_' + 'plant_lock_emission_new1'
        plant_lock_emission_new2 = m.fuel_used.unique()[0] + '_' + 'plant_lock_emission_new2'
        
        # get the overall annual emission and lock emission
        existing_annual_emission = m.fuel_used.unique()[0] + '_' + 'power_annual_emission_existing'
        existing_lock_emission = m.fuel_used.unique()[0] + '_' + 'power_lock_emission_existing'
        power_capacity_new1 = m.fuel_used.unique()[0] + '_' + 'power_capacity_new1'
        power_capacity_new2 = m.fuel_used.unique()[0] + '_' + 'power_capacity_new2'
        power_annual_emission_new1 = m.fuel_used.unique()[0] + '_' + 'power_annual_emission_new1'
        power_annual_emission_new2 = m.fuel_used.unique()[0] + '_' + 'power_annual_emission_new2'
        power_lock_emission_new1 = m.fuel_used.unique()[0] + '_' + 'power_lock_emission_new1'
        power_lock_emission_new2 = m.fuel_used.unique()[0] + '_' + 'power_lock_emission_new2'
            
            
    # calculate emission from remaining parts
    df =  emission_aggregation_ccs_1 (m, p, i)
    
    # calculate emission from ccs part
    df =  emission_aggregation_ccs_2 (n, df, i)
    
    # get the overall capacity, emission by adding ccs possible and impossible new plants
    df.at[i,plant_capacity_new] = df.at[i,plant_capacity_new1]  + df.at[i,plant_capacity_new2] 
    df.at[i,plant_annual_emission_new] = df.at[i,plant_annual_emission_new1] + df.at[i,plant_annual_emission_new2]
    df.at[i,plant_lock_emission_new] = df.at[i,plant_lock_emission_new1] + df.at[i,plant_lock_emission_new2] 
    
    # power plant ratio calculation for new plants
    plant_power_ratio = df.at[i,power_capacity_new] / df.at[i,plant_capacity_new]   
    
    # calculate the power annual emission and lock emission for new plants
    df.at[i,power_annual_emission_new] = df.at[i,plant_annual_emission_new] * plant_power_ratio
    df.at[i,power_lock_emission_new] = df.at[i,plant_lock_emission_new] * plant_power_ratio
    df.at[i,power_annual_emission_new1] = df.at[i,plant_annual_emission_new1] * plant_power_ratio
    df.at[i,power_annual_emission_new2] = df.at[i,plant_annual_emission_new2] * plant_power_ratio
    df.at[i,power_lock_emission_new1] = df.at[i,plant_lock_emission_new1] * plant_power_ratio
    df.at[i,power_lock_emission_new2] = df.at[i,plant_lock_emission_new2] * plant_power_ratio
    df.at[i,power_capacity_new1] = df.at[i,plant_capacity_new1] * plant_power_ratio
    df.at[i,power_capacity_new2] = df.at[i,plant_capacity_new2] * plant_power_ratio

    return df  