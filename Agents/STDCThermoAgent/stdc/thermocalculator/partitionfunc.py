import numpy as np
from stdc.utils import params as p
from stdc.utils.geomtypes import GeomTypes

def molecular_partfunc(Mwt,GeomType,SymNr,RotTemp,VibTemp,ElecLvL,T,P=p.Pref):
    qt = translational_partfunc(Mwt,T,P)
    qr = rotational_partfunc(GeomType,SymNr,RotTemp,T)
    qv = vibrational_partfunc(VibTemp,T)
    qe = electronic_partfunc(ElecLvL,T)
    q = qt*qr*qv*qe
    return q

def translational_partfunc(Mwt,T,P):
    if T>0.0:
        L = p.h/np.sqrt(2.0*p.pi*Mwt*p.kB*T)
        V = p.kB*T/P #this at a single molecule level
        qt= V/(L*L*L)
    else:
        qt = 1.0
    return qt

def rotational_partfunc(GeomType,SymNr,RotTemp,T):
    if T>0.0:
        if GeomType == GeomTypes.ATOMIC:
            qr = 1.0
        elif GeomType == GeomTypes.LINEAR:
            qr = 1.0/SymNr*T/RotTemp[0]
        else:
           qr = 1.0/SymNr*np.sqrt(p.pi*T*T*T/(RotTemp[0]* \
                RotTemp[1]*RotTemp[2]))
    else:
        qr = 1.0
    return qr

def vibrational_partfunc(VibTemp,T):
    # T = 0 K case ?
    if T>0.0:
        for vT in VibTemp:
            if vT>0.0:
                qv = qv * 1.0/(1.0-np.exp(-vT/T))
    else:
        qv = 1.0
    return qv

def electronic_partfunc(ElecLvL,T):
    qe = 0.0
    if T>0.0:
        for lvls in ElecLvL:
            qe = qe + lvls[0]*np.exp(-lvls[1]/(p.kB*T))
    else:
        for lvls in ElecLvL:
            qe = qe + lvls[0]
    return qe
