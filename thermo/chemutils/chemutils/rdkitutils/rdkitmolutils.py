import rdkit
import rdkit.Chem
import rdkit.Chem.rdMolAlign
import rdkit.Chem.rdMolTransforms
import numpy as np
from chemutils.mathutils import getXYZPointsDistance
import copy


def rdkitSubstrMatch(targetRdkitMol, refRdkitMol, *args, **kwargs):
    """
    Matches two rdkit mol object graphs with each other.
    Outputs a single tuple of corresponding atoms indices.

    Arguments:
    ----------
    targetRdkitMol : object
        target rdkit molecule object
    refRdkitMol : object
        reference rdkit molecule object
    *args : various
        any allowed positional arguments to rdkit
        GetSubstructMatch method
    **kwargs : dictionary
        any allowed key=value arguments to rdkit
        GetSubstructMatch method

    Returns:
    ----------
    match: int list
        a list containing reference molecule atoms
        indices matching the target molecule atoms
        e.g. match = [4,3,2,0,1] means:
        target atom   ref atom
             0      ->     4
             1      ->     3
             2      ->     2
             3      ->     0
             4      ->     1
    """
    match = refRdkitMol.GetSubstructMatch(targetRdkitMol, *args, **kwargs)
    return match

def rdkitSubstrMatches(targetRdkitMol, refRdkitMol, *args, **kwargs):
    """
    Matches two rdkit mol object graphs with each other.
    Outputs a list of tuples of corresponding atoms indices.

    Arguments:
    ----------
    targetRdkitMol : object
        target rdkit molecule object
    refRdkitMol : object
        reference rdkit molecule object
    *args : various
        any allowed positional arguments to rdkit
        GetSubstructMatches method
    **kwargs : dictionary
        any allowed key=value arguments to rdkit
        GetSubstructMatches method

    Returns:
    ----------
    match: int list
        a list containing reference molecule atoms
        indices matching the target molecule atoms
        e.g. match = [4,3,2,0,1] means:
        target atom   ref atom
             0      ->     4
             1      ->     3
             2      ->     2
             3      ->     0
             4      ->     1
    """
    matches = refRdkitMol.GetSubstructMatches(targetRdkitMol, *args, **kwargs)
    return matches

def rdkitSubstrAlignMatch(targetRdkitMol, refRdkitMol,
        uniquify=False, filterByDist=True):
    """
    Matches two rdkit mol object graphs with each other.
    Outputs the best match based on rmsd score.

    Arguments:
    ----------
    targetRdkitMol : object
        target rdkit molecule object
    refRdkitMol : object
        reference rdkit molecule object
    uniquify : bool, optional, default False
        uniquify (by atom index) match results - if True
        it collapses the same atoms matches to one
    filterByDist : bool, optional, default True
        in case of multiple best matches it filters them
        based on atoms xyz distances in target and ref
        molecules

    Returns:
    ----------
    bestMatch: int list
        a list containing reference molecule atoms
        indices matching the target molecule atoms
    bestRmsd: float
        the best rmsd match score
    """
    matches = rdkitSubstrMatches(targetRdkitMol, refRdkitMol, uniquify)
    bestRmsd = 1e9
    bestMatch = []
    rmsds = []
    for i, match in enumerate(matches):
        atomMap = [[i,matchedAtomId] for i,matchedAtomId in enumerate(match)]
        # use a copy of the target molecule as not to modify the atoms order
        # in the original target molecule!
        targetRdkitMolLocal = copy.deepcopy(targetRdkitMol)
        rmsd = rdkitAlignMols(targetRdkitMolLocal, refRdkitMol, atomMap=atomMap)
        rmsds.append(rmsd)
        if rmsd < bestRmsd:
            bestRmsd = rmsd
            bestMatch = match

    if filterByDist:
        bestMatchByDist, bestRmsdByDist = rdkitFilterAlignMatchesByDist(matches, rmsds, targetRdkitMol, refRdkitMol)
        if bestMatchByDist and bestRmsdByDist is not None:
           bestMatch, bestRmsd = bestMatchByDist, bestRmsdByDist
    return bestMatch, bestRmsd

def rdkitFilterAlignMatchesByDist(matches, rmsds, targetRdkitMol, refRdkitMol):
    # check rmsds list for any duplicate min rmsd values - this would indicate that there
    # are multiple equivalent ways to match heavy atoms in two molecules
    duplMinRmsds = [(rmsd, match) for rmsd, match in zip(rmsds,matches) if not rmsd > min(rmsds) ]
    bestMatch = []
    bestRmsd = None
    if duplMinRmsds:
        bestRmsd = duplMinRmsds[0][0]
        # if there are multiple ways for matching the heavy atoms (each having the same min rmsd)
        # then use the knowledge of the heavy atoms spatial positions in target and ref mols
        # and select the match that gives the lowest sum of target - ref atoms distances
        atomsAbsDistSumBest = 1e9
        for _, match in duplMinRmsds:
            atomsAbsDistSum = 0.0
            for tarAtomId, refAtomId in enumerate(match):
                targetAtomPos = getRdkitAtomXYZbyId(targetRdkitMol,tarAtomId)
                refAtomPos = getRdkitAtomXYZbyId(refRdkitMol,refAtomId)
                dist = getXYZPointsDistance(targetAtomPos,refAtomPos)
                atomsAbsDistSum = atomsAbsDistSum + dist
            if atomsAbsDistSum < atomsAbsDistSumBest:
                atomsAbsDistSumBest = atomsAbsDistSum
                bestMatch = match
    return bestMatch, bestRmsd

def rdkitAlignMols(targetRdkitMol, refRdkitMol, *args, **kwargs):
    """
    Matches two rdkit mol objects based on their xyz coordinates.
    Outputs the best rmsd score and modified targetRdkitMol that
    is now aligned with the ref molecule.

    Arguments:
    ----------
    targetRdkitMol : object
        target rdkit molecule object
    refRdkitMol : object
        reference rdkit molecule object
    *args : various
        any allowed positional arguments to rdkit
        AlignMol method
    **kwargs : dictionary
        any allowed key=value arguments to rdkit
        AlignMol method

    Returns:
    ----------
    rmsd: float
        the best rmsd score
    """
    rmsd = rdkit.Chem.rdMolAlign.AlignMol(targetRdkitMol, refRdkitMol, *args, **kwargs)
    return rmsd

def rdkitAlignTransform(targetRdkitMol, refRdkitMol, *args, **kwargs):
    """Matches two rdkit mol object graphs with each other.
       Outputs corresponding atom indices."""
    rmsd, transf = rdkit.Chem.rdMolAlign.GetAlignmentTransform(targetRdkitMol, refRdkitMol, *args, **kwargs)
    return rmsd, transf

def findRdkitMolRadicalAtomIds(rdkitMol):
    """Returns a list of atom ids with radical sites."""
    radicalAtomIds = []
    for atom in rdkitMol.GetAtoms():
        if atom.GetNumRadicalElectrons() > 0:
            radicalAtomIds.append(atom.GetIdx())
    return radicalAtomIds

def rdkitMolToMolFrags(rdkitMol, *args, **kwargs):
    """Given a single rdkit mol object, the function detects if the mol
       contains separate mol fragments. If the fragments are present,
       they are returned as a list of rdkit mol objects. If fragments
       are not present an empty list is returned insread."""
    rdkitMolFrags = rdkit.Chem.GetMolFrags(rdkitMol, *args, **kwargs)
    if len(rdkitMolFrags) <= 1: rdkitMolFrags = []
    return rdkitMolFrags

#def combineRdkitMolFrags(rdkitMolFrags, *args, **kwargs):
#    """Given a list of two rdkit mol fragments, the function tries to
#       recombine them into a single molecule by detecting and bonding
#       the most suitable atoms in the two fragments."""
#    if rdkitMolFrags == 1:
#        return rdkitMolFrags[0]
#    elif len(rdkitMolFrags) > 2:
#        return -1
#    else:
#        radicalAtomList1 = findRdkitMolRadicalAtomIds(rdkitMolFrags[0])
#        radicalAtomList2 = findRdkitMolRadicalAtomIds(rdkitMolFrags[1])
#
#        AtomsXYZ1 = [getRdkitMolAtomXYZbyId(rdkitMolFrags[0],idx) for idx in radicalAtomList1]
#        AtomsXYZ2 = [getRdkitMolAtomXYZbyId(rdkitMolFrags[1],idx) for idx in radicalAtomList2]
#
#        closestAtoms = findClosestPoints(AtomsXYZ1,AtomsXYZ2)
#        closestAtoms[1] = closestAtoms[1] + rdkitMolFrags[0].GetNumAtoms()
#        m12 = rdkit.Chem.rdmolops.CombineMols(molMolFragList[0],rdkitMolFrags[1])
#        m12 = rdkit.Chem.RWMol(m12)
#        m12.AddBond(closestAtoms[0],closestAtoms[1],order=rdkit.Chem.rdchem.BondType.SINGLE)
#    return rdkitMolToInchi(m12)

def getRdkitAtomXYZbyId(rdkitMol,atomId):
    """Returns an xyz atom position given atom's id."""
    conf = rdkitMol.GetConformer()
    return np.array(list(conf.GetAtomPosition(atomId)))

def getRdkitHeavyAtomsIds(rdkitMol):
    return [atom.GetIdx() for atom in rdkitMol.GetAtoms() if atom.GetAtomicNum() > 1]

def getRdkitMolHydrogenAtomsIds(rdkitMol):
    return [atom.GetIdx() for atom in rdkitMol.GetAtoms() if atom.GetAtomicNum() == 1]

def getRdkitAllAtomsIds(rdkitMol):
    return [atom.GetIdx() for atom in rdkitMol.GetAtoms()]

def rdkitRemoveAllHydrogens(rdkitMol):
    """Removes all hydrogens in a molecule."""
    # this removes all hydrogens bonded to heavy atoms
    rdkitMolNoHs = rdkit.Chem.RWMol(rdkit.Chem.rdmolops.RemoveHs(rdkitMol))

    # the rest of the code removes any other hydrogens
    heavyAtomIds = getRdkitHeavyAtomsIds(rdkitMolNoHs)
    allAtomIds = getRdkitAllAtomsIds(rdkitMolNoHs)

    for atomId in sorted(allAtomIds, reverse=True):
        if atomId not in heavyAtomIds: rdkitMolNoHs.RemoveAtom(atomId)
    return rdkit.Chem.Mol(rdkitMolNoHs)

def rdkitMatchHeavyAtoms(rdkitTargetMol, rdkitRefMol, uniquify=False, filterByDist=True):
    """
    Matches heavy atoms from one molecule to the other.
    Outputs best match dictionary containing atoms
    indices between target (keys) and reference (values)
    molecules and corresponding rmsd match score.

    Arguments:
    ----------
    xyzTargetFileOrStr : str
        target molecule (either file path or xyz string)
    xyzRefFileOrStr : str
        reference molecule (either file path or xyz string)
    uniquify : bool, optional, default False
        uniquify (by atom index) match results - if True
        it collapses the same atoms matches to one
    filterByDist : bool, optional, default True
        in case of multiple best matches it filters them
        based on atoms xyz distances in target and ref
        molecules

    Returns:
    ----------
    heavyAtomsMatch: dict
        dictionary whose keys correspond to target atoms
        and values to the matched reference atoms
    rmsd: float
        the best rmsd match score
    """
    rdkitTargetMolNoHs = rdkitRemoveAllHydrogens(rdkitTargetMol)
    rdkitRefMolNoHs = rdkitRemoveAllHydrogens(rdkitRefMol)

    heavyAtomsMatch = {}
    rmsd = None
    if getRdkitHeavyAtomsIds(rdkitTargetMolNoHs):
        if rdkitTargetMolNoHs.GetNumAtoms()> 0:
            match, rmsd = rdkitSubstrAlignMatch(rdkitTargetMolNoHs, rdkitRefMolNoHs, uniquify=uniquify, filterByDist=filterByDist)
            if match:
                # add the best heavy atoms match to the final dict
                # work out heavy atoms indices after adding hydrogens
                refHeavyAtomIds = getRdkitHeavyAtomsIds(rdkitRefMol)
                targetHeavyAtomIds = getRdkitHeavyAtomsIds(rdkitTargetMol)

                for i,id in enumerate(match):
                    heavyAtomsMatch[targetHeavyAtomIds[i]] = refHeavyAtomIds[id]
    return heavyAtomsMatch, rmsd

def setRdkitMolBondLength(rdkitMol, atomId1, atomId2, bondLength):
    """Returns an xyz atom position given atom's id."""
    conf = rdkitMol.GetConformer()
    rdkit.Chem.rdMolTransforms.SetBondLength(conf, atomId1, atomId2, bondLength)

def getRdkitMolBondLength(rdkitMol, atomId1, atomId2):
    """Returns an xyz atom position given atom's id."""
    conf = rdkitMol.GetConformer()
    return rdkit.Chem.rdMolTransforms.GetBondLength(conf, atomId1, atomId2)