from openbabel import openbabel
from openbabel import pybel

def obGetMolBonds(xyzString):
    mol = pybel.readstring("xyz",xyzString)
    bond_list = []
    numBonds = mol.OBMol.NumBonds()
    for bondId in range(numBonds):
        bond = mol.OBMol.GetBond(bondId)
        bond_list.append(str(bond.GetBeginAtom().GetIdx()) + ' '  +  str(bond.GetEndAtom().GetIdx()) + ' ' + str(bond.GetBondOrder()))
    return ' '.join(bond_list)

