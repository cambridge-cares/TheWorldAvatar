import numpy as np
import lmfit as lmf
import stdc.utils.params as p

def _nasaResidualFunc(params,x,y):
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

def fitNASACoeffs(T, Cp, H, S):
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
    ydata = np.zeros([3,len(T)])
    for i, (Cpi,Hi,Si,Ti) in enumerate(zip(Cp, H, S, T)):
        ydata[0][i] = Cpi/p.R
        ydata[1][i] = Hi/p.R/Ti
        ydata[2][i] = Si/p.R
    result = lmf.minimize(_nasaResidualFunc, params, args=(T, ydata))
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