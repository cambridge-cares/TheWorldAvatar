import chemutils.rdkitutils.rdkitconverters as rdkitconverters
import chemutils.rdkitutils.rdkitmolutils as rdkitmolutils
import chemutils.obabelutils.obconverter as obconverter
import chemutils.ioutils.ioutils as ioutils
import os


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
        filename = xyz_file_path.replace(".xyz",".com")

    inputMol = inputMol.split('\n')[2:]
    while not inputMol[-1]: 
        del inputMol[-1]
                 
    atoms = []
    coordinates = []
    for input in inputMol:
       atom,x,y,z = input.split()
       atoms.append(atom)
       coordinates.append(["{:.5f}".format(float(x)), "{:.5f}".format(float(y)), "{:.5f}".format(float(z))])
    gauss_out = ""
    gauss_out += "%NProcShared=" + str(num_cpus) + "\n"
    gauss_out += "%mem=" + str(memory) + "GB" + "\n"
    gauss_out += "%Chk="+os.path.basename(filename).replace(".com",'') +".chk" + "\n"
    gauss_out += str(job_route) + "\n"
    gauss_out += "\n"
    gauss_out += os.path.basename(filename).replace(".com",'')+"\n"
    gauss_out += "\n"
    gauss_out += str(charge) + " " + str(spin_multiplicity) + "\n"
    for i in range(len(atoms)):
       gauss_out += atoms[i] + "          " + coordinates[i][0] + "        " + coordinates[i][1] + "        " + coordinates[i][2] + "\n"
    gauss_out += "\n"
    return gauss_out    
