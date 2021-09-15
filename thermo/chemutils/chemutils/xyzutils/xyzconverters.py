import chemutils.rdkitutils.rdkitconverters as rdkitconverters
import chemutils.rdkitutils.rdkitmolutils as rdkitmolutils
import chemutils.obabelutils.obconverter as obconverter
import chemutils.ioutils.ioutils as ioutils


def xyzToMolToRdkitMol(inputMolXYZ, *args, **kwargs):
    molBlock = obconverter.obConvert(inputMolXYZ, 'xyz', 'mol')
    return rdkitconverters.molBlockToRdkitMol(molBlock, *args, **kwargs)

def xyzToInchiToRdkitMol(inputMolXYZ, *args, **kwargs):
    inchi = obconverter.obConvert(inputMolXYZ, 'xyz', 'inchi')
    return rdkitconverters.inchiToRdkitMol(inchi, *args, **kwargs)

def xyzToPdbToRdkitMol(inputMolXYZ, *args, **kwargs):
    pdbBlock = obconverter.obConvert(inputMolXYZ, 'xyz', 'pdb')
    return rdkitconverters.pdbBlockToRdkitMol(pdbBlock, *args, **kwargs)

def xyzFragsToRdkitMolFrags(inputMolXYZ, removeHs=False):
    rdkitMolFromMol = xyzToMolToRdkitMol(inputMolXYZ, removeHs)
    rdkitMolFrags = rdkitmolutils.rdkitMolToMolFrags(rdkitMolFromMol, asMols=True)
    return rdkitMolFrags

def xyzFragsToInchiFrags(inputMolXYZ):
    rdkitMolFrags = xyzFragsToRdkitMolFrags(inputMolXYZ)
    return [rdkitconverters.rdkitMolToInchi(mol) for mol in rdkitMolFrags]


def xyzToGaussianInput(xyz_file_path, job_route, charge, spin_multiplicity,
                       memory, num_cpus):
    if ioutils.fileExists(xyz_file_path):
        inputMol= ioutils.readFile(xyz_file_path)

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
