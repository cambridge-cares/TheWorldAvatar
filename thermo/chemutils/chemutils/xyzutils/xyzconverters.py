from chemutils.rdkitutils import molBlockToRdkitMol, \
                                 inchiToRdkitMol, \
                                 pdbBlockToRdkitMol, \
                                 rdkitMolToInchi
from chemutils.rdkitutils import rdkitMolToMolFrags
from chemutils.obabelutils import obConvert


def xyzToMolToRdkitMol(inputMolXYZ, *args, **kwargs):
    molBlock = obConvert(inputMolXYZ, 'xyz', 'mol')
    return molBlockToRdkitMol(molBlock, *args, **kwargs)

def xyzToInchiToRdkitMol(inputMolXYZ, *args, **kwargs):
    inchi = obConvert(inputMolXYZ, 'xyz', 'inchi')
    return inchiToRdkitMol(inchi, *args, **kwargs)

def xyzToPdbToRdkitMol(inputMolXYZ, *args, **kwargs):
    pdbBlock = obConvert(inputMolXYZ, 'xyz', 'pdb')
    return pdbBlockToRdkitMol(pdbBlock, *args, **kwargs)

def xyzFragsToRdkitMolFrags(inputMolXYZ, removeHs=False):
    rdkitMolFromMol = xyzToMolToRdkitMol(inputMolXYZ, removeHs)
    rdkitMolFrags = rdkitMolToMolFrags(rdkitMolFromMol, asMols=True)
    return rdkitMolFrags

def xyzFragsToInchiFrags(inputMolXYZ):
    rdkitMolFrags = xyzFragsToRdkitMolFrags(inputMolXYZ)
    return [rdkitMolToInchi(mol) for mol in rdkitMolFrags]