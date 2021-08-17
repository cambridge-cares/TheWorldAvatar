# INCREASED BOILER EFFICIENCY
# alpha = 0.9
# nb    = 0.9

# REDUCED BOILER EFFICIENCY
# alpha = 0.9
# nb    = 0.7

# INCREASED USAGE FOR HEATING
# alpha = 1
# nb    = 0.8

# REDUCED USAGE FOR HEATING
alpha = 0.8
nb    = 0.8

# BASE CASE
# alpha = 0.9
# nb    = 0.8

import os 

try:
	os.mkdir('figure_output/a_'+str(alpha)+'n_'+str(nb))
except FileExistsError:
	print('output folder already exists')

