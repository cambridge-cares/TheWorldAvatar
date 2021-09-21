import openbabel.pybel as pybel


def obGetMolBonds(xyzString):
    mol = pybel.readstring("xyz",xyzString)
    bonds_info = []
    numBonds = mol.OBMol.NumBonds()
    for bondId in range(numBonds):
        bond = mol.OBMol.GetBond(bondId)
        beginAtom = bond.GetBeginAtom()
        endAtom = bond.GetEndAtom()
        bonds_info.append(
            {#bond
                'order': bond.GetBondOrder(),
                'length': bond.GetLength(),
                'isAromatic': bond.IsAromatic(),
                'isAmide': bond.IsAmide(),
                'isCarbonyl': bond.IsCarbonyl,
                'isCisOrTrans': bond.IsCisOrTrans,
                'isEster': bond.IsEster(),
                'beginAtom': {'atomId': beginAtom.GetIdx(), 'atomType': beginAtom.GetType(), 'atomicNum': beginAtom.GetAtomicNum(),
                    'atomicMass': beginAtom.GetAtomicMass(), 'coordinatesXYZ': [beginAtom.GetX(), beginAtom.GetY(),
                    beginAtom.GetZ()]},
                'endAtom': {'atomId': endAtom.GetIdx(), 'atomType': endAtom.GetType(), 'atomicNum': endAtom.GetAtomicNum(),
                'atomicMass': endAtom.GetAtomicMass(), 'coordinatesXYZ': [endAtom.GetX(), endAtom.GetY(),
                endAtom.GetZ()]}
            }
        )
    return bonds_info

def obGetMolMolWt(xyzString):
    mol = pybel.readstring("xyz",xyzString)
    return mol.OBMol.GetMolWt()

def obToInertialFrame(xyzString):
    mol = pybel.readstring("xyz",xyzString)
    xyzString = xyzString.split('\n')[2:]
    xyzAtoms = [xyzLine.split(' ')[0] for xyzLine in xyzString]
    mol.OBMol.ToInertialFrame()
    xyzInertialFrame = [str(len(xyzAtoms)), '']
    for i in range(1,mol.OBMol.NumAtoms()+1):
        atom = mol.OBMol.GetAtom(i)
        atomType = xyzAtoms[i-1]
        x = atom.GetX()
        y = atom.GetY()
        z = atom.GetZ()
        xyzLine = f'{atomType} {x} {y} {z}'
        xyzInertialFrame.append(xyzLine)
    return '\n'.join(xyzInertialFrame)
