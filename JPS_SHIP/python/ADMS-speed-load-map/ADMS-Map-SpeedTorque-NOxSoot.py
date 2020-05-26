from math import exp, log, sqrt, pi, erf
import json

import torch
import gpytorch
import numpy
import pickle

## Global inputs.
NO_frac = 0.35
#NumParSizeClasses = 9
#NumParSizeClasses = 4
NumParSizeClasses = 3
LogNorm_mu = 200.0
LogNorm_sigma = 1.0

def LogNormal(ax, amu, asigma):
    return exp(-((log(ax)-log(amu))/asigma)**2/2)/(ax*asigma*sqrt(2*pi))
    
def LogNormalCDF(ax, amu, asigma):
    if ax<1e-20:
        cd = 0.0
    else:
        cd = (1.0+erf((log(ax)-log(amu))/(asigma*sqrt(2.0))))/2
    return cd

def ValueUnitDict(aValue, aUnit):
    return {'value': aValue, 'unit': aUnit}

def PollutantDict(aName, aValue, aUnit, aPseudo, aMolmass, aMolmassUnit):
    return {'name': aName, 'value': aValue, 'unit': aUnit,
        'pseudocomponent': aPseudo,
        'molmass': ValueUnitDict(aMolmass, aMolmassUnit)}

def ParSizeClassDict(aDiam, aDiamUnit, aDens, aDensUnit, aMassflowrate, aMassflowrateUnit):
    return {'diameter': ValueUnitDict(aDiam, aDiamUnit),
        'density': ValueUnitDict(aDens, aDensUnit),
        'emission_rate': ValueUnitDict(aMassflowrate, aMassflowrateUnit)}

def MixtureDict():
    return {'molmass': ValueUnitDict(2.8638811393753E-2, 'kg/mol'),
        'cp': ValueUnitDict(1334.86958348868, 'J/kg/K'),
        'cv': ValueUnitDict(1043.71109135798, 'J/kg/K'),
        'density': ValueUnitDict(0.577544330505469, 'kg/m3'),
        'temperature': ValueUnitDict(889.27736479669, 'K'),
        'pressure': ValueUnitDict(153934.915475353, 'Pa'),
        'massflux': ValueUnitDict(1.92143028723584E-2, 'kg/s'),
        'massH2O': ValueUnitDict(6.4456929510343E-2, '-'),
        'viscosity': ValueUnitDict(3.9067870880755E-5, 'Pa s'),
        'compressibilityfactor': ValueUnitDict(1, '-')}

def PollutantsList(aNOx):
    NOx_mfr = aNOx*1e-6*0.05 #ppmv to kg/s, with rough guesses of density and mass flow rate
    NO_mfr = NOx_mfr*NO_frac
    NO2_mfr = NOx_mfr*(1.0-NO_frac)
    return [PollutantDict('CO', 4.38018592976711E-4, 'kg/s', 'no', 2.80104E-2, 'kg/mol'),
        PollutantDict('CO2', 1.48566682568053E-2, 'kg/s', 'no', 4.40098E-2, 'kg/mol'),
        PollutantDict('HC', 1.23609160023103E-3, 'kg/s', 'yes', 4.58822245497081E-2, 'kg/mol'),
        PollutantDict('NO', NO_mfr, 'kg/s', 'no', 3.0006E-2, 'kg/mol'),
        PollutantDict('NO2', NO2_mfr, 'kg/s', 'no', 4.6006E-2, 'kg/mol'),
        PollutantDict('SO2', 8.69367E-6, 'kg/s', 'no', 6.4066E-2, 'kg/mol'),
        PollutantDict('SO3', 3.56659152E-4, 'kg/s', 'no', 8.0066E-2, 'kg/mol'),
        PollutantDict('O3', 3.0319795918367E-8, 'kg/s', 'no', 4.7997E-2, 'kg/mol')]

def ParticleList(aSoot):
    the_list = []
    prev_cdf = 0.0
    for i in range(1,NumParSizeClasses+1):
        the_size = exp(log(1e4)*i/3)
		#the_size = exp(log(1e4)*i/4)
		#the_size = exp(log(1e3)*i/7)
        cdf = LogNormalCDF(the_size, LogNorm_mu, LogNorm_sigma)
        the_mfr = aSoot*(cdf-prev_cdf)/1000.0/3600.0 #g/h to kg/s
        the_list.append(ParSizeClassDict(the_size, 'nm', 1800, 'kg/m3', the_mfr, 'kg/s'))
        prev_cdf = cdf
    return the_list

def ADMSDict(aNOx, aSoot):
    return {'mixture': MixtureDict(),
        'pollutants': PollutantsList(aNOx),
        'particle': ParticleList(aSoot)}

# NB Need to have activated the MoDS Python_Env for this to work!
# For some reason it needs to find the module named after the original file
# in which the surrogate was pickled...

def deserialise_surrogate(aFilename):
    with open(aFilename, 'br') as f:
        f.seek(8) # NB First 64 bits are the length of the binary stream!
        the_surr = pickle.load(f)
    return the_surr

def evaluate_surrogate(the_surr, the_xs):
    #print('python: xs: (for evaluation)')
    #print(the_xs)
    #print(type(the_xs))
    #print(the_xs.shape)
    # Set model to evaluate mode
    the_surr.eval()
    the_surr.m_likelihood.eval()
    with torch.no_grad(), gpytorch.settings.use_toeplitz(False), gpytorch.settings.debug(False):
        y_pred = the_surr(torch.tensor(the_xs, dtype=torch.float32))
    #print('python: y: (result of evaluation)')
    #print(y_pred)
    return y_pred.mean().data.numpy().astype(numpy.float64)

def evaluate_map(aSpeed, aTorque):
    # Transform inputs.
    speed_transf = 2*(aSpeed-800.0)/1600.0-1.0
    torque_transf = 2*(aTorque-30.0)/470.0-1.0
    xs = numpy.array([[speed_transf, torque_transf, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]])
    #print(xs)
    # Evaluate surrogates.
    rNOx = evaluate_surrogate(NOx_surr, xs)[0]
    rSoot = evaluate_surrogate(Soot_surr, xs)[0]
    rSoot = exp(rSoot*log(10.0))
    return rNOx, rSoot

######################
## Executable part.
######################
print('This is a speed-load map.')

# Load inputs (speed and load), i.e. deserialise JSON from file.
with open('in.json', 'r') as file:
    speed_load_dict = json.load(file)
speed = speed_load_dict['speed']['value']
torque = speed_load_dict['torque']['value']
#print(speed_load_dict)
print('Requested speed (assumed unit RPM):', speed, '(should be in range 600-2500)')
print('Requested torque (assumed unit Nm):', torque, '(should be in range 50-550)')

# Load surrogates.
NOx_surr = deserialise_surrogate('NOx_surr.bin')
Soot_surr = deserialise_surrogate('Soot_surr.bin')

# Evaluate map.
NOx_resp, Soot_resp = evaluate_map(speed, torque)
print('NOx response [ppmv]:', NOx_resp)
print('Soot response [g/h]:', Soot_resp)

# Write outputs, i.e. serialise JSON to file.
with open('out.json', 'w') as file:
    json.dump(ADMSDict(NOx_resp, Soot_resp), file, indent=4)
