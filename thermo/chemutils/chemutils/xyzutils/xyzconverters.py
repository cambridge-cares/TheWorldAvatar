from chemutils.rdkitutils import molBlockToRdkitMol, \
                                 inchiToRdkitMol, \
                                 pdbBlockToRdkitMol, \
                                 rdkitMolToInchi
from chemutils.rdkitutils import rdkitMolToMolFrags
from chemutils.obabelutils import obConvert
from chemutils.ioutils import fileExists, readFile


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


def xyzToGaussianInput(xyz_file_path, job_route, charge, spin_multiplicity,
                       memory, num_cpus):
    if fileExists(xyz_file_path): inputMol= readFile(xyz_file_path)
    inputMol =\
    """3\n
       \n
       C  0.0 0.0 0.0\n
       O  0.0 0.0 0.0\n
       O  0.0 0.0 0.0\n
    """
    inputMol = inputMol.split('\n')[2:]
    while not inputMol[-1]: del inputMol[-1]
    inputMol = [ 'C  0.0 0.0 0.0',
                 'C  0.0 0.0 0.0.',
                 'C  0.0 0.0 0.0'
                ]
