import chemspecies as chs
import numpy as np
import lmfit as lmf
import utilities as utl
import params as p

# Fit Nasa polynomials
def fitThermoDataNASA(Spec,Trange=[]):
    # By default the Trange is subdivided into 20 temperatures
    nT = 20
    if len(Trange)!=0:
       Spec.FitTrangeNasa = Trange

    Trange1 = np.linspace(Spec.FitTrangeNasa[0],Spec.FitTrangeNasa[1],nT)
    Trange2 = np.linspace(Spec.FitTrangeNasa[1],Spec.FitTrangeNasa[2],nT)
    
    resultLow = findNASACoeffs(Spec,Trange1)
    resultHigh = findNASACoeffs(Spec,Trange2)

    Spec.FitLowNasa = resultLow
    Spec.FitHighNasa = resultHigh    
    return Spec

def f(params,x,y):
    a=[]
    a.append(params['a0'])
    a.append(params['a1'])
    a.append(params['a2'])
    a.append(params['a3'])
    a.append(params['a4'])
    a.append(params['a5'])
    a.append(params['a6'])

    Cp_eq = a[0]           + a[1]*x     + a[2]*x**2     + a[3]*x**3     + a[4]*x**4
    H_eq  = a[0]           + a[1]/2.0*x + a[2]/3.0*x**2 + a[3]/4.0*x**3 + a[4]/5.0*x**4 + a[5]/x
    S_eq  = a[0]*np.log(x) + a[1]*x     + a[2]/2.0*x**2 + a[3]/3.0*x**3 + a[4]/4.0*x**4 + a[6]

    residual = np.zeros([3,len(x)])
    residual[0][:] = y[0][:]-Cp_eq
    residual[1][:] = y[1][:]-H_eq
    residual[2][:] = y[2][:]-S_eq
    return residual

def findNASACoeffs(Spec,xdata):
    x0= [1.0e+00, 1.0e-03, 1.0e-07, 1.0e-10, 1.0e-14, 1.0e+03, 1.0e+01]
    #x0= [1.0e+00, 1.0e+00, 1.0e+00, 1.0e+00, 1.0e+00, 1.0e+00, 1.0e+00]
    params = lmf.Parameters()
    params.add('a0',x0[0])
    params.add('a1',x0[1])
    params.add('a2',x0[2])
    params.add('a3',x0[3])
    params.add('a4',x0[4])
    params.add('a5',x0[5])
    params.add('a6',x0[6])
    ydata = np.zeros([3,len(xdata)])
    for i,xi in enumerate(xdata):
        Cpi = Spec.getSpHeatCapacityCpSTHD(xi)/p.R
        Hi = Spec.getSpEnthalpySTHD(xi)/p.R/xi
        Si = Spec.getSpEntropySTHD(xi)/p.R
        ydata[0][i] = Cpi
        ydata[1][i] = Hi
        ydata[2][i] = Si
    result = lmf.minimize(f, params, args=(xdata, ydata))
    aa= result.residual
    fitParams = []
    fitParams.append(result.params['a0'].value)
    fitParams.append(result.params['a1'].value)
    fitParams.append(result.params['a2'].value)
    fitParams.append(result.params['a3'].value)
    fitParams.append(result.params['a4'].value)
    fitParams.append(result.params['a5'].value)
    fitParams.append(result.params['a6'].value)
    return fitParams