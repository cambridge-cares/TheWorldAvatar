import stdc.utils.params as p
import stdc.thermocalculator.partitionfunc as pf
from stdc.utils.geomtypes import GeomTypes
import numpy as np

#======================================================
#    Thermochemistry via statistical thermodynamics
#======================================================

#------------------------------------------------------
# Overall Entropy S
#------------------------------------------------------
def getEntropy(Mwt,GeomType,SymNr,RotTemp,VibTemp,ElecLvL,T,P=p.Pref):
    St = getEntropyTrans(Mwt,T,P)
    Sr = getEntropyRot(GeomType,SymNr,RotTemp,T)
    Sv = getEntropyVib(VibTemp,T)
    Se = getEntropyElec(ElecLvL,T)
    S = St+Sr+Sv+Se
    return S

# Entropy Contribution from 
# translational motion St
#---------------------------------
def getEntropyTrans(Mwt,T,P=p.patm):
    qt = pf.translational_partfunc(Mwt,T,P)
    St = p.R*(np.log(qt)+1.0+1.5)
    return St

# Entropy Contribution from
# rotational motion Sr
#---------------------------------
def getEntropyRot(GeomType,SymNr,RotTemp,T):    
    Sr = 0.0
    if GeomType != GeomTypes.ATOMIC:
        qr = pf.rotational_partfunc(GeomType,SymNr,RotTemp,T)
        if GeomType == GeomTypes.LINEAR:
            Sr = p.R*(np.log(qr)+1.0)
        else:
            Sr = p.R*(np.log(qr)+1.5)
    return Sr

# Entropy Contribution from
# vibrational motion Sv
#---------------------------------
def getEntropyVib(VibTemp,T):
    Sv = 0.0
    if T>0.0:
        for vT in VibTemp:
            if vT>0.0:
                Sv = Sv + ((vT/T)/(np.exp(vT/T)-1.0)-\
                    np.log(1.0-np.exp(-vT/T)))
        Sv = Sv*p.R
    return Sv

# Entropy Contribution from
# electronic levels Se
#---------------------------------
def getEntropyElec(ElecLvL,T):
    qe = pf.electronic_partfunc(ElecLvL,T)
    Se = p.R*np.log(qe)
    if T>0.0 and len(ElecLvL)>1:
        sum = 0.0
        for lvls in ElecLvL:
            sum = sum + lvls[0]*(lvls[1]/p.kB/T)* \
                np.exp(-lvls[1]/p.kB/T)
        sum = sum/qe
        Se = Se+p.R*sum
    return Se

#------------------------------------------------------
# Overall internal Energy U
#------------------------------------------------------
def getInternalEnergy(GeomType,VibTemp,ElecLvL,T):
    Ut = getInternalEnergyTrans(T)
    Ur = getInternalEnergyRot(GeomType,T)
    Uv = getInternalEnergyVib(VibTemp,T)
    Ue = getInternalEnergyElec(ElecLvL,T)
    U = Ut+Ur+Uv+Ue
    return U

# Internal Energy Contribution
# from translational motion Ut
#---------------------------------
def getInternalEnergyTrans(T):        
    Ut = 1.5*p.R*T
    return Ut

# Internal Energy Contribution
# from rotational motion Ur
#---------------------------------
def getInternalEnergyRot(GeomType,T):
    Ur = 0.0
    if GeomType == GeomTypes.LINEAR:
        Ur = p.R*T
    elif GeomType == GeomTypes.NONLINEAR:
        Ur = 1.5*p.R*T    
    return Ur

# Internal Energy Contribution
# from vibrational motion Uv
#---------------------------------
def getInternalEnergyVib(VibTemp,T):
    Uv = 0.0
    if T>0.0:
        for vT in VibTemp:
            Uv = Uv + vT*(0.5+1.0/(np.exp(vT/T)-1.0))
    Uv = Uv*p.R
    return Uv

# Internal Energy Contribution
# from electronic levels Ue
#--------------------------------- 
def getInternalEnergyElec(ElecLvL,T):    
    Ue = 0.0
    if T>0.0:
        sum = 0.0
        qe = pf.electronic_partfunc(ElecLvL,T)
        for lvls in ElecLvL:
            sum = sum + lvls[0]*(lvls[1]/p.kB/T)* \
                np.exp(-lvls[1]/p.kB/T)
        sum = sum/qe
        Ue = p.R*T*sum
    return Ue

#------------------------------------------------------
# Overall Heat Capacity at constant Volume Cv
#------------------------------------------------------
def getHeatCapacityCv(GeomType,VibTemp,ElecLvL,T):
    Cvt = getHeatCapacityCvTrans()
    Cvr = getHeatCapacityCvRot(GeomType)
    Cvv = getHeatCapacityCvVib(VibTemp,T)
    Cve = getHeatCapacityCvElec(ElecLvL,T)
    Cv = Cvt+Cvr+Cvv+Cve
    return Cv

# Heat Capacity Cv Contribution
# from translational motion Cvt
#---------------------------------
def getHeatCapacityCvTrans():
    Cvt = 1.5*p.R
    return Cvt

# Heat Capacity Cv Contribution
# from rotational motion Cvr
#---------------------------------
def getHeatCapacityCvRot(GeomType):
    Cvr = 0.0
    if GeomType == GeomTypes.LINEAR:
        Cvr = p.R
    elif GeomType == GeomTypes.NONLINEAR:
        Cvr = 1.5*p.R
    return Cvr

# Heat Capacity Cv Contribution
# from vibrational motion Cvv
#---------------------------------        
def getHeatCapacityCvVib(VibTemp,T):
    Cvv = 0.0
    if T>0.0:        
        for vT in VibTemp:
            if vT>0.0:
                Cvv = Cvv + (vT/T)**2* \
                    np.exp(-vT/T)/(1-np.exp(-vT/T))**2
        Cvv = Cvv*p.R
    return Cvv

# Heat Capacity Cv Contribution
# from electronic levels Cve
#--------------------------------- 
def getHeatCapacityCvElec(ElecLvL,T):
    Cve = 0.0
    if T>0.0:
        sum1 = 0.0
        sum2 = 0.0
        qe = pf.electronic_partfunc(ElecLvL,T)
        for lvls in ElecLvL:
            sum1 = sum1 + lvls[0]*(lvls[1]/p.kB/T)* \
                np.exp(-lvls[1]/p.kB/T)
            sum2 = sum2 + lvls[0]*(lvls[1]/p.kB/T)**2* \
                np.exp(-lvls[1]/p.kB/T)
        sum1 = (sum1/qe)**2
        sum2 = sum2/qe
        Cve = p.R*(sum2-sum1)
    return Cve

#------------------------------------------------------
# Overall Heat Capacity at constant Pressure Cp
#------------------------------------------------------
def getHeatCapacityCp(GeomType,VibTemp,ElecLvL,T):    
    Cpt = getHeatCapacityCpTrans()
    Cpr = getHeatCapacityCpRot(GeomType)
    Cpv = getHeatCapacityCpVib(VibTemp,T)
    Cpe = getHeatCapacityCpElec(ElecLvL,T)
    Cp = Cpt+Cpr+Cpv+Cpe
    return Cp

# Heat Capacity Cp Contribution
# from translational motion Cpt
#---------------------------------
def getHeatCapacityCpTrans():
    #extra R term comes from translational motion
    Cpt = p.R+getHeatCapacityCvTrans()
    return Cpt

# Heat Capacity Cp Contribution
# from rotational motion Cpr
#---------------------------------
def getHeatCapacityCpRot(GeomType):
    Cpr = getHeatCapacityCvRot(GeomType)
    return Cpr

# Heat Capacity Cv Contribution
# from vibrational motion Cpv
#---------------------------------
def getHeatCapacityCpVib(VibTemp,T):
    Cpv = getHeatCapacityCvVib(VibTemp,T)
    return Cpv

# Heat Capacity C Contribution
# from electronic levels Cpe
#---------------------------------
def getHeatCapacityCpElec(ElecLvL,T):
    Cpe = getHeatCapacityCvElec(ElecLvL,T)
    return Cpe

#------------------------------------------------------
# Overall Enthalpy H
#------------------------------------------------------
def getEnthalpy(GeomType,VibTemp,ElecLvL,T):
    Ht = getEnthalpyTrans(T)
    Hr = getEnthalpyRot(GeomType,T)
    Hv = getEnthalpyVib(VibTemp,T)
    He = getEnthalpyElec(ElecLvL,T)
    H = Ht+Hr+Hv+He
    return H

# Enthalpy Contribution
# from translational motion Ht
#---------------------------------
def getEnthalpyTrans(T):
    # extra RT term comes from translational
    # motion
    Ht = p.R*T + getInternalEnergyTrans(T)
    return Ht

# Enthalpy Contribution
# from rotational motion Hr
#---------------------------------
def getEnthalpyRot(GeomType,T):
    Hr = getInternalEnergyRot(GeomType,T)
    return Hr

# Enthalpy Contribution
# from vibrational motion Hv
#---------------------------------
def getEnthalpyVib(VibTemp,T):
    Hv = getInternalEnergyVib(VibTemp,T)
    return Hv

# Enthalpy Contribution
# from elecrtonic levels He
#---------------------------------
def getEnthalpyElec(ElecLvL,T):
    He = getInternalEnergyElec(ElecLvL,T)
    return He

#------------------------------------------------------
# Overall Gibbs Energy G
#------------------------------------------------------
def getGibbsEnergy(Mwt,GeomType,SymNr,RotTemp,VibTemp,ElecLvL,T):
    Gt = getGibbsEnergyTrans(Mwt,T)
    Gr = getGibbsEnergyRot(GeomType,SymNr,RotTemp,T)
    Gv = getGibbsEnergyVib(VibTemp,T)
    Ge = getGibbsEnergyElec(ElecLvL,T)
    G = Gt+Gr+Gv+Ge
    return G

# Gibbs Energy Contribution
# from translational motion Gt
#---------------------------------
def getGibbsEnergyTrans(Mwt,T,P=p.Pref):
    Ht = getEnthalpyTrans(T)
    St = getEntropyTrans(Mwt,T,P)
    Gt = Ht-T*St
    return Gt

# Gibbs Energy Contribution
# from rotational motion Gr
#---------------------------------
def getGibbsEnergyRot(GeomType,SymNr,RotTemp,T):
    Hr = getEnthalpyRot(GeomType,T)
    Sr = getEntropyRot(GeomType,SymNr,RotTemp,T)
    Gr = Hr-T*Sr
    return Gr

# Gibbs Energy Contribution
# from vibrational motion Gv
#---------------------------------
def getGibbsEnergyVib(VibTemp,T):
    Hv = getEnthalpyVib(VibTemp,T)
    Sv = getEntropyVib(VibTemp,T)
    Gv = Hv-T*Sv
    return Gv

# Gibbs Energy Contribution
# from electronic levels Ge
#---------------------------------
def getGibbsEnergyElec(ElecLvL,T):
    He = getEnthalpyElec(ElecLvL,T)
    Se = getEntropyElec(ElecLvL,T)
    Ge = He-T*Se
    return Ge