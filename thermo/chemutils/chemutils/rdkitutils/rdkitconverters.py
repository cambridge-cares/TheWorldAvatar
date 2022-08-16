import rdkit
import rdkit.Chem
import rdkit.Chem.inchi
import chemutils.rdkitutils.rdkitmolutils as rdkitmolutils


def molBlockToRdkitMol(molBlock, *args, **kwargs):
    return rdkit.Chem.MolFromMolBlock(molBlock, *args, **kwargs)

def pdbBlockToRdkitMol(pdbBlock, *args, **kwargs):
    return rdkit.Chem.MolFromPDBBlock(pdbBlock, *args, **kwargs)

def inchiToRdkitMol(inchi, *args, **kwargs):
    mol_from_inchi = rdkit.Chem.inchi.MolFromInchi(inchi, *args, **kwargs)
    # this is needed, apparently the MolFromInchi with RemoveHs=False
    # still does not add all required hydrogens...
    removeHs = kwargs.get('removeHs', False)
    if not removeHs:
        mol_from_inchi = rdkit.Chem.rdmolops.AddHs(mol_from_inchi)
    return mol_from_inchi

def rdkitMolToInchi(rdkitMol, *args, **kwargs):
    return rdkit.Chem.inchi.MolToInchi(rdkitMol, *args, **kwargs)

def rdkitMolToXYZ(rdkitMol, confId=0, numDigits=5):
    xyz = []
    xyz.append(str(rdkitMol.GetNumAtoms()))
    xyz.append("")
    fspec=f".{numDigits}f"
    for atom in rdkitMol.GetAtoms():
        atompos = rdkitmolutils.getRdkitAtomXYZbyId(rdkitMol, atom.GetIdx(), confId)
        xyzLine = f"{atom.GetSymbol()} {atompos[0]:{fspec}} {atompos[1]:{fspec}} {atompos[2]:{fspec}}"
        xyz.append(xyzLine)
    return "\n".join(xyz)