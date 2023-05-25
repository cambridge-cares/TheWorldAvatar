#!/usr/bin/python

import cantera as ct
import pandas as pd
import argparse
import os
ct.suppress_thermo_warnings()

from timeit import default_timer as timer                                ###### @added
class interrupter(object):                                ###### @added
    def __init__(self, start, tmax):                                ###### @added
        self.start = start                                ###### @added
        self.tmax = tmax                                ###### @added
    def __call__(self, _):                                ###### @added
        tnow = timer()                                ###### @added
        if tnow -self.start > self.tmax:                                ###### @added
            raise Exception('Maximum simulation wall time exceeded.')                                ###### @added
        return 0                                ###### @added

##################################################################################
# Define the available arguments to the script
##################################################################################
parser = argparse.ArgumentParser(description='Simulate laminar flame speed')
parser.add_argument('-d', '--dataPath', type=str, metavar='', required=True, help='File path for simulation settings')
parser.add_argument('-t', '--tranModel', type=str, metavar='', required=True, help='Transport model for simulation, mix or multi')
parser.add_argument('-v', '--verbose', type=int, metavar='', help='Verbose level of solution, from 0 to 5')
args = parser.parse_args()

##################################################################################
# Get the simulation setup
##################################################################################
# Read argument
dataPath = args.dataPath
tranModel = args.tranModel
if args.verbose:
    verbose = args.verbose
else:
    verbose = 0

# Read flame speed data
flameData = pd.read_csv(dataPath)

# Direct to path of mechanism
mechanismPath = flameData['Mechanism'][0]
# Setup fuel and oxidizer
fuel = flameData['Fuel'][0]
oxidizer = flameData['Oxidizer'][0]
if oxidizer == 'Air':
    oxidizer = "O2:21, N2:79"
# Get temperature
temperature = flameData['Temperature'][0]
unitTemp = flameData['UnitTemp'][0]
if unitTemp == 'K':
    temperature = temperature
elif unitTemp == 'C':
    temperature = temperature + 273.15
# Get phi
phi = flameData['Phi'][0]
# Get pressure
pressure = flameData['Pressure'][0]
unitPressure = flameData['UnitPres'][0]
if unitPressure == 'atm':
    pressure = 101325*pressure
elif unitPressure == 'bar':
    pressure = 100000*pressure
elif unitPressure == 'kpa':
    pressure = 1000*pressure


##################################################################################
# Setup simulation
##################################################################################
# Set caseName
caseName = fuel + "_" + str(temperature) + "K" + str(pressure) + "Pa" + "_" + str(phi)

# Setup the width of the solution grid in m (internal points selected by solver)
width = 0.01                                ###### @modified

# Import mechanism
gas = ct.Solution(mechanismPath)

# Setup initial state
gas.TP = temperature, pressure
gas.set_equivalence_ratio(phi=phi, fuel=fuel, oxidizer=oxidizer)

# Create the free laminar premixed flame
flame = ct.FreeFlame(gas, width=width)
flame = ct.FreeFlame(gas, width=width)                                ###### @modified
flame.inlet.mdot = gas.DP[0]*50*0.01

# Start solving with solving energy equations disabled
flame.energy_enabled = False
# Setup the tolerance
tol_ss = [1.0e-5, 1.0e-13] # [rtol atol] for steady-state problem
tol_ts = [1.0e-4, 1.0e-13] # [rtol atol] for time stepping                                ###### @modified
flame.flame.set_steady_tolerances(default=tol_ss)
flame.flame.set_transient_tolerances(default=tol_ts)
# Setup the initial guess of temperature profile                                ###### @modified
flame.set_initial_guess(locs=[0.0, 0.7, 0.8, 1.0])                                ###### @modified
# Setup the refinement and grid properties
flame.set_refine_criteria(ratio = 7.0, slope = 1, curve = 1)
#flame.set_max_jac_age(50, 50)                                ###### @commented
flame.set_time_step(5.e-06, [10, 20, 80]) #s
flame.max_time_step_count = 4000
# Setup the interrupter                                ###### @added
flame.set_interrupt(interrupter(timer(), 600))                                ###### @added, maybe further parameterise the time limit
# Solve the first flame
flame.solve(verbose)

# Solve the second flame and so on with energy equations enabled
flame.energy_enabled = True
flame.solve(verbose)

##################################################################################
# Loop the solver over a set of refinement criteria
##################################################################################
refCrit = []
refCrit.append([5.0, 1, 1])
refCrit.append([5.0, 0.995, 1])
refCrit.append([5.0, 0.99, 1])
refCrit.append([5.0, 0.985, 1])
refCrit.append([5.0, 0.98, 1])
refCrit.append([5.0, 0.97, 1])
refCrit.append([5.0, 0.95, 1])
refCrit.append([5.0, 0.92, 1])
refCrit.append([5.0, 0.90, 1])
refCrit.append([5.0, 0.85, 0.90])
refCrit.append([5.0, 0.80, 0.85])
refCrit.append([5.0, 0.70, 0.80])
refCrit.append([5.0, 0.50, 0.75])
refCrit.append([5.0, 0.50, 0.50])
refCrit.append([5.0, 0.30, 0.30])
refCrit.append([3.0, 0.10, 0.10])

for ref in refCrit:
    flame.set_refine_criteria(ratio = ref[0], slope = ref[1], curve = ref[2])
    flame.solve(verbose)

#flame.set_refine_criteria(ratio = 5.0, slope = 1, curve = 1)
#flame.solve(verbose)
#print("success:{}".format(flame.u[0]))

#flame.set_refine_criteria(ratio = 5.0, slope = 0.995, curve = 1)
#flame.solve(verbose)
#print("success:{}".format(flame.u[0]))

#flame.set_refine_criteria(ratio = 5.0, slope = 0.99, curve = 1)
#flame.solve(verbose)
#print("success:{}".format(flame.u[0]))

#flame.set_refine_criteria(ratio = 5.0, slope = 0.98, curve = 1)
#flame.solve(verbose)
#print("success:{}".format(flame.u[0]))

#flame.set_refine_criteria(ratio = 5.0, slope = 0.96, curve = 1)
#flame.solve(verbose)
#print("success:{}".format(flame.u[0]))

#flame.set_refine_criteria(ratio = 5.0, slope = 0.94, curve = 1)
#flame.solve(verbose)
#print("success:{}".format(flame.u[0]))

#flame.set_refine_criteria(ratio = 5.0, slope = 0.90, curve = 1)
#flame.solve(verbose)
#print("success:{}".format(flame.u[0]))

#flame.set_refine_criteria(ratio = 7.0, slope = 0.75, curve = 0.75)
#flame.solve(verbose)
#print("success:{}".format(flame.u[0]))
#flame.set_refine_criteria(ratio = 6.0, slope = 0.75, curve = 0.75)
#flame.solve(verbose)
#print("success:{}".format(flame.u[0]))
#flame.set_refine_criteria(ratio = 5.0, slope = 0.5, curve = 0.5)
#flame.solve(verbose)
#print("success:{}".format(flame.u[0]))
## Third flame and so on ...:
#flame.set_refine_criteria(ratio = 5.0, slope = 0.3, curve = 0.3)
#flame.solve(verbose)
#print("success:{}".format(flame.u[0]))
#flame.set_refine_criteria(ratio = 3.0, slope = 0.1, curve = 0.1)
#flame.solve(verbose)
#print("success:{}".format(flame.u[0]))


##################################################################################
# Refine solution with prune
##################################################################################
flame.set_refine_criteria(ratio = 2.0, slope = 0.05, curve = 0.05, prune = 0.01)
flame.solve(verbose)
#print("success:{}".format(flame.u[0]))
#flame.set_refine_criteria(ratio = 2.0, slope = 0.03, curve = 0.03, prune = 0.005)
#flame.solve(verbose)
#print("success:{}".format(flame.u[0]))
#flame.set_refine_criteria(ratio = 2.0, slope = 0.02, curve = 0.02, prune = 0.001)
#flame.solve(verbose)
#print("success:{}".format(flame.u[0]))

##################################################################################
# Solve with multi-component model if specified
##################################################################################
if ('multi' in tranModel) or ('Multi' in tranModel):
    flame.transport_model = 'Multi'
    flame.solve(verbose)

## Simulate laminar flame speed
#gas = ct.Solution(mechanismPath)
#gas.TP = temperature, pressure
#gas.set_equivalence_ratio(phi=phi, fuel=fuel, oxidizer=oxidizer)
#flame = ct.FreeFlame(gas, width=width)
#flame.set_refine_criteria(ratio=ratio, slope=slope, curve=curve)
#
## Set the name of solution file
#solFile = 'LaminarFlameSpeedSimulationResults.xml'
#solId = 'multi'
#
## Carry out the simulation and output results to solution file
#if (os.path.isfile(solFile)):
#    print("Loading solution {} from {}".format(solId,solFile))
#    flame.restore(solFile,solId)
#    print("\n")
#else:
#    #flame.show_solution()
#    flame.solve(loglevel=0, auto=True)
#    print('\nmixture-averaged flamespeed = {:7f} cm/s'.format(flame.u[0]*100))
##    flame.save(solFile, 'mix', 'solution with mixture-averaged transport')
#    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
#    
#    flame.transport_model = 'Multi'
#    flame.solve(loglevel=0)
#    print('\nmulticomponent flamespeed = {0:7f} cm/s'.format(flame.u[0]*100))
##    flame.save(solFile,'multi', 'solution with multicomponent transport')
#    print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")


##################################################################################
# Generate output file
##################################################################################
# Collect data for dfLfs
lfsDict = {}
lfsDict['CaseName'] = caseName
lfsDict['Mechanism'] = mechanismPath
lfsDict['Fuel'] = fuel
lfsDict['Oxidizer'] = oxidizer.replace(",", "")
lfsDict['Temperature'] = temperature
lfsDict['Pressure'] = pressure
lfsDict['Phi'] = phi
lfsDict['Laminar flame speed [cm/s]'] = flame.u[0]*100

# Create dfLfs and output to csv file
dfLfs = pd.Series(lfsDict)
dfLfs = pd.DataFrame(dfLfs).T
dfLfs.to_csv('OutputCase00001Lfs0001Info.csv', encoding='utf-8', index=False)
