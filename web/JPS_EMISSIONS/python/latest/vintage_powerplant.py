# ### 1.2 define functions that change the vintage of power plant stock

import numpy as np
import pandas as pd

# define function that split the power plant database into two parts: younger than 40, older than 40

def  plant_split (m): 

   part1 = m[m['age'] < 40]
   part2 = m[m['age'] >= 40]
    
   return [part1, part2] 

# define function that change the vintage for both kinds of power plants and get the corresponding two parts

# def  vintage_change_ccs (part1, part2): 
    
#     # for plants less than 40, serve till 40
#     part1 = part1.copy()
#     part1.loc[:,'age'] = part1['age'] + 1
#     part1_r = part1[part1.age == 40]
#     part1 = part1.drop(part1[part1.age == 40].index)
    
    
#     # for plants more than 40, serve 5 more years, then retire averagely over the next 30 years
#     part2 = part2.sort_values(by=['age'], ascending=[False])
#     # split the part 2 index into 30 segements
#     indxs = np.array_split(part2.index, 36-i)
#     # initialize part2_r to avoid the local variable problem 
#     part2_r = part2[part2.age == 100]

#     if i < 5:
#       part_2 = part2.copy()
#       # part2_r is set to empty by this sentence
#       part2_r = part2[part2.age == 100]
#     if i == 5:
#       # part2_r is the newly retired plants this year
#       part2_r = part2.loc[indxs[0]]
#       # delete the first segement
#       part2 = part2.drop(indxs[0])
#     if i > 5 and i < 36:
#       # part2_r is the newly retired plants this year
#       part2_r = part2.loc[indxs[0]]
#       # delete the second to final segement
#       part2 = part2.drop(indxs[0])  
    
#     k = pd.concat([part1, part2], ignore_index=True)
#     k_r = pd.concat([part1_r, part2_r], ignore_index=True)
    
#     return [k, k_r]