# define function that can screen capture possible plants from nearly retired plants
# p is power plant database, q is the emission inventory, cost is the reference cost, j is year

import numpy as np
import pandas as pd
from cost_emission_calculation_w_ccs import cost_emission_calculation_w_ccs
from cost_emission_calculation_wo_ccs import cost_emission_calculation_wo_ccs

def  ccs_screen (p, q, cost, j): 
  
   if  p.shape[0] == 0:
       # if there is empty dataframe input, set all values to zero then
       p.assign(LCOE = " ", annual_emission = " ")
       df1 = p
       df2 = p    
       return [df1, df2]
        
   else:  
    
    
      # get column of power plant database
      cols = list(p)
    
      # calculate the cost and emission with and without ccs
      m = cost_emission_calculation_w_ccs (p, q)
      n = cost_emission_calculation_wo_ccs (p, q)

      # merge m and n into one dataframe
      df = pd.merge(m, n, on=cols)

      # define the reference cost to decide whether ccs is feasible
      reference_cost = cost

      # creat LCOE and annual emission column    
      df['LCOE'] = np.zeros(df.shape[0])
      df['annual_emission'] = np.zeros(df.shape[0])
    
      # set the corresponding LCOE and emission according to reference cost: if capture cost larger than reference, no capture
      for i in range(df.shape[0]):
        if df.loc[i,'w_ccs_cost'] <= reference_cost:
          df.loc[i,'LCOE'] = df.loc[i,'w_ccs_cost']
          df.loc[i,'annual_emission'] = df.loc[i,'w_ccs_emission']
        else:
          df.loc[i,'LCOE'] = df.loc[i,'wo_ccs_cost']
          df.loc[i,'annual_emission'] = df.loc[i,'wo_ccs_emission']

      # choose the feasible ones and store their information
      df1 = df[df['w_ccs_cost'] <= reference_cost]
      df1 = df1.drop(['w_ccs_cost', 'w_ccs_emission', 'wo_ccs_cost', 'wo_ccs_emission'],1)
      df1['age'] = 36 - j
        
      # choose the infeasible ones and store their information
      df2 = df[df['w_ccs_cost'] > reference_cost]
      df2 = df2.drop(['w_ccs_cost', 'w_ccs_emission', 'wo_ccs_cost', 'wo_ccs_emission'],1)
      df2['age'] = 36 - j  
    
      return [df1, df2]