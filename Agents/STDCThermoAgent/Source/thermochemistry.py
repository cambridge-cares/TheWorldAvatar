import params as p
import numpy as np

#======================================================
#                 Partition functions
#======================================================

# determines vibrational partition function reference point
# globref = 0 - bottome of the well, use it if you want to add ZPE
#               from somwhere else
# globref = 1 - first vibrational level, use it if ZPE is already
#               included in electronic energy
# globref value can be overwritten when making a function call
globref = 1
#------------------------------------------------------
# Overall Partition function (for a single molecule) q
#------------------------------------------------------
def getPartFunc(Mwt,GeomType,SymNr,RotTemp,VibTemp,ElecLvL,T,ref=globref,P=p.Pref):
    qt = getTransPartFunc(Mwt,T,P)
    qr = getRotPartFunc(GeomType,SymNr,RotTemp,T)
    qv = getVibPartFunc(VibTemp,T,ref)
    qe = getElecPartFunc(ElecLvL,T)
    q = qt*qr*qv*qe
    return q

# Translational Contribution qt
#---------------------------------
def getTransPartFunc(Mwt,T,P=p.Pref):
    if T>0.0:
        L = p.h/np.sqrt(2.0*p.pi*Mwt*p.kB*T)
        V = p.kB*T/P #this at a single molecule level
        qt= V/(L*L*L)
    else:
        qt = 1.0
    return qt

# Rotational Contribution qr
#---------------------------------
def getRotPartFunc(GeomType,SymNr,RotTemp,T):
    if T>0.0:
        if GeomType == 0:
            qr = 1.0
        elif GeomType == 1:
            qr = 1.0/SymNr*T/RotTemp[0]
        else:
           qr = 1.0/SymNr*np.sqrt(p.pi*T*T*T/(RotTemp[0]* \
                RotTemp[1]*RotTemp[2]))
    else:
        qr = 1.0
    return qr

# Vibrational Contribution qv
#---------------------------------
def getVibPartFunc(VibTemp,T,ref=globref):
    # T = 0 K case ?
    if T>0.0:
        for vT in VibTemp:
            if vT>0.0:
                if ref==0:
                    # Eref at the bottom of the well.
                    qv = qv * ( np.exp(-vT/2.0/T)/(1.0- \
                        np.exp(-vT/T)) )
                else:
                    #Eref at the first vibrational level.
                    qv = qv * 1.0/(1.0-np.exp(-vT/T))
    else:
        qv = 1.0
    return qv

# Electronic Contribution qe
#---------------------------------
def getElecPartFunc(ElecLvL,T):
    qe = 0.0
    if T>0.0:
        for lvls in ElecLvL:
            qe = qe + lvls[0]*np.exp(-lvls[1]/(p.kB*T))
    else:
        for lvls in ElecLvL:
            qe = qe + lvls[0]
    return qe


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
    qt = getTransPartFunc(Mwt,T,P)
    St = p.R*(np.log(qt)+1.0+1.5)
    return St

# Entropy Contribution from
# rotational motion Sr
#---------------------------------
def getEntropyRot(GeomType,SymNr,RotTemp,T):    
    if GeomType == 0:
        Sr = 0.0
    else:
        qr = getRotPartFunc(GeomType,SymNr,RotTemp,T)
        if GeomType == 1:
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
    qe = getElecPartFunc(ElecLvL,T)
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
def getInternalEnergy(GeomType,VibTemp,ElecLvL,T,ref=globref):
    Ut = getInternalEnergyTrans(T)
    Ur = getInternalEnergyRot(GeomType,T)
    Uv = getInternalEnergyVib(VibTemp,T,ref)
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
    if GeomType == 0:
        Ur = 0.0
    elif GeomType == 1:
        Ur = p.R*T
    else:
        Ur = 1.5*p.R*T    
    return Ur

# Internal Energy Contribution
# from vibrational motion Uv
#---------------------------------
def getInternalEnergyVib(VibTemp,T,ref=globref):
    Uv = 0.0
    ZPEsum = 0.0
    if T>0.0:
        for vT in VibTemp:
            Uv = Uv + vT*(1.0/(np.exp(vT/T)-1.0))
    if ref==0:
        for vT in VibTemp:
            ZPEsum = ZPEsum + 0.5*vT
    Uv = (Uv+ZPEsum)*p.R
    return Uv

# Internal Energy Contribution
# from electronic levels Ue
#--------------------------------- 
def getInternalEnergyElec(ElecLvL,T):    
    Ue = 0.0
    if T>0.0:
        sum = 0.0
        qe = getElecPartFunc(ElecLvL,T)
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
    if GeomType == 1:
        Cvr = p.R
    elif GeomType == 2:
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
        qe = getElecPartFunc(ElecLvL,T)
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
def getEnthalpy(GeomType,VibTemp,ElecLvL,T,ref=globref):
    Ht = getEnthalpyTrans(T)
    Hr = getEnthalpyRot(GeomType,T)
    Hv = getEnthalpyVib(VibTemp,T,ref)
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
def getEnthalpyVib(VibTemp,T,ref=globref):
    Hv = getInternalEnergyVib(VibTemp,T,ref)
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
def getGibbsEnergy(Mwt,GeomType,SymNr,RotTemp,VibTemp,ElecLvL,T,ref=globref):
    Gt = getGibbsEnergyTrans(Mwt,T)
    Gr = getGibbsEnergyRot(GeomType,SymNr,RotTemp,T)
    Gv = getGibbsEnergyVib(VibTemp,T,ref)
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
def getGibbsEnergyVib(VibTemp,T,ref=globref):
    Hv = getEnthalpyVib(VibTemp,T,ref)
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