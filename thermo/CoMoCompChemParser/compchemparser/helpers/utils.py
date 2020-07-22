import sys
import numpy as np
import compchemparser.helpers.params as p

def wait():
	input("Press Enter to continue...")

def codexit():
    wait()
    sys.exit()

def dienicely(errmsg):
    print(errmsg)
    codexit()

# intertia tensor
# -----------------------------
def getInertiaTensor(aElMolWt,aXYZ):
    # ===================================
    # helper functions to populate 
    # inertia tensor
    #
    # diagonal elements    
    def getDiagMoments(a,b,m):
        MolWt = sum(m)
        sum1 = 0.0
        sum2 = 0.0
        sum3 = 0.0
        for i,rows in enumerate(m):
            sum1 = sum1 + m[i]*(a[i]*a[i]+b[i]*b[i])
            sum2 = sum2 + m[i]*a[i]
            sum3 = sum3 + m[i]*b[i]
        sum2 = sum2*sum2 * 1.0/MolWt
        sum3 = sum3*sum3 * 1.0/MolWt
        Iaa = sum1 - sum2 - sum3
        return Iaa
    # off-diagonal elements
    # -----------------------------
    def getOffDiagMoments(a,b,m):
        MolWt = sum(m)
        sum1 = 0.0
        sum2 = 0.0
        sum3 = 0.0
        for i,rows in enumerate(m):
            sum1 = sum1 + m[i]*a[i]*b[i]
            sum2 = sum2 + m[i]*a[i]
            sum3 = sum3 + m[i]*b[i]
        Iab = -sum1 + 1.0/MolWt*sum2*sum3
        return Iab
    # ===================================

    # init inertia tensor
    IT = np.empty([3,3])
    # get mass vector and X,Y,Z coordiantes
    m = aElMolWt
    X = aXYZ[:,0]
    Y = aXYZ[:,1]
    Z = aXYZ[:,2]
    # get diagonal and off-diagonal elements
    Ixx = getDiagMoments(Y,Z,m)
    Iyy = getDiagMoments(X,Z,m)
    Izz = getDiagMoments(X,Y,m)
    Ixy = getOffDiagMoments(X,Y,m)
    Iyx = Ixy
    Ixz = getOffDiagMoments(X,Z,m)
    Izx = Ixz
    Iyz = getOffDiagMoments(Y,Z,m)
    Izy = Iyz
    # put everything together
    IT = [[Ixx,Ixy,Ixz],[Iyx,Iyy,Iyz],[Izx,Izy,Izz]]
    return IT

# Get moments of Inertia 
# of a molecule
#---------------------------------
def getRotConst(aElMolWt,aXYZ):
    InertiaMom = [] # I in kg*m^2

    #construct the mass and XYZ np arrays
    if len(aElMolWt)>0 and len(aXYZ)>0:
        IT = getInertiaTensor(aElMolWt,aXYZ)
        eigen,V = np.linalg.eig(IT)
        InertiaMom = [eigen[0],eigen[1],eigen[2]] # in amu*A^2
        # sort the moments
        InertiaMom = sorted(InertiaMom,reverse=True)
        # convert from atomic to SI units (amu*A^2 -> kg*m^2)
        InertiaMom =[I*p.amu*p.Angs*p.Angs for I in InertiaMom]

        RotConst = getRotConstFromImom(InertiaMom)
    return RotConst

def getRotConstFromImom(aImom):
    RotConst = [] # B in GHz
    B_THRESHOLD = 1e-3
    if len(aImom)>0:
        for I in aImom:
            append_B = True
            if I>0.0: # I in kg*m^2
                B = p.BI_pref/I*p.c/1e9
                for B_stored in RotConst:
                    if abs(B - B_stored) < B_THRESHOLD:
                        append_B = False

                if append_B: RotConst.append(B)
                        
    return RotConst