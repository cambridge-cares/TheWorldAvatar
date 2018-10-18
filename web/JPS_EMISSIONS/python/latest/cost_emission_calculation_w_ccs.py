# define a function here that can intepolate the emission and cost surface, and calculate the corresponding power plant emission 
# with potential CCS in CCS scenario  

import numpy as np
from scipy import interpolate
from scipy.interpolate import LinearNDInterpolator

def  cost_emission_calculation_w_ccs (m, n): 
    
    if m.shape[0] == 0:
       # if there is empty dataframe input, set all values to zero then
       pass
        
    else:  
       
        # read age and capacity information from ultrasupercritical PC database
        x_1 = m.loc[:,('age')].values
        y_1 = m.loc[:,('capacity_MW')].values
        y_11 = m.loc[:,('capacity_MW')].values
        
        # use cubic spline interpolation to construct surrogate model
        x1 = n.loc[:,('age')].values.reshape((int(n.shape[0]/n.capacity_MW.nunique()), n.capacity_MW.nunique()))[:,0]
        y1 = n.loc[:,('capacity_MW')].values.reshape((int(n.shape[0]/n.capacity_MW.nunique()), n.capacity_MW.nunique()))[0,:]
        x1x1, y1y1 = np.meshgrid(x1, y1, indexing = 'ij')
        x1y1=np.column_stack([x1x1.ravel(),y1y1.ravel()])
        # x1y1.shape
        y11= n.loc[:,('capacity_MW')].values.reshape((int(n.shape[0]/n.capacity_MW.nunique()), n.capacity_MW.nunique()))[0,:]
        # y11.shape
        z1 = n.loc[:,('capture_LCOE_MWh')].values
        # z1.shape
        z11 = n.loc[:,('capture_annual_emission_ton')].values.reshape((int(n.shape[0]/n.capacity_MW.nunique()), n.capacity_MW.nunique()))[0,:]
        # z11.shape
        
        # f1 is the functional relationship between electricity cost and age, capacity
        f1 = LinearNDInterpolator(x1y1, z1)
        # f1(30,1865)
        
        # f11 is the functional relationship between annual emission and capacity
        f11 = interpolate.interp1d(y11, z11, kind='slinear')
        # f11(1865)
        # vectorized function
        f_11 = np.vectorize(f11)

        # maximum x_1 and y_1 are 30 and 2500 repsectively
        x_new_1 = np.clip(x_1, 1, 30)
        y_new_1 = np.clip(y_1, 100, 2500)
        xnew1ynew1 = np.column_stack((x_new_1,y_new_1))
        
        # cost estimiation by age and capacity, clip at 30 and 2500
        cost = f1(xnew1ynew1)
        
        # emission estimation by capacity
        emission = f_11(np.clip(y_11%2500, 100, 2500)) + f_11(2500)*(y_11//2500) 
        
        m = m.assign(w_ccs_cost = cost)
        m = m.assign(w_ccs_emission = emission)
        
        # m.rename(columns={'w_ccs_cost': 'w_ccs_cost_$_MWh', 'w_ccs_emission': 'w_ccs_annual_emission_ton'}, inplace=True)
        k = m.loc[:,:]
    
        return k