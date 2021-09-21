import chemutils.xyzutils.xyzconverters as xyzconverters
import chemutils.rdkitutils.rdkitmolutils as rdkitmolutils
import chemutils.rdkitutils.rdkitconverters as rdkitconverters
import chemutils.mathutils.linalg as linalg
import chemutils.ioutils.ioutils as ioutils
import chemutils.obabelutils.obconverter as obconverter
import chemutils.obabelutils.obutils as obutils
import random
import re
import numpy as np
import copy

def xyzToAtomsPositions(xyzFileOrStr):
    """
    Returns atom positions (order) given a molecule in an xyz format.
    Inchi-based algorithm.

    Note that the assigned positions are NOT UNIQUE and depend on the
    order of atoms in xyz file. This is true for both, heavy atoms
    and hydrogens. Heavy atoms positons are based on inchi, so they
    can only change due to presence of equivalent atoms with respect
    to connectivity. Positions of hydrogen atoms are assigned after
    the heavy atoms, taking any non-connected hydrogens first
    followed by hydrogens connected to heavy atoms.

    Use this function to set the atoms positions in a reference
    molecule. The idea is to assign the positions once and to never
    change them again.

    Arguments:
    ----------
    xyzFileOrStr : str
        input xyz molecule (either file path or xyz string)

    Returns:
    ----------
    atomsPositions: dict
        dictionary whose keys correspond to atoms positions in xyz
        file and values to the newly assigned positions
    """
    # get inchi with extra auxiliary log
    if ioutils.fileExists(xyzFileOrStr): xyzFileOrStr= ioutils.readFile(xyzFileOrStr)

    xyzFileOrStr = xyzToIntertialFrame(xyzFileOrStr)
    xyzFileOrStr = '\n'.join([xyz_line.replace('H','Cl') for xyz_line in xyzFileOrStr.split('\n')])

    inchiWithAux = obconverter.obConvert(inputMol=xyzFileOrStr,inputMolFormat='xyz',
                            outputMolFormat='inchi', options=['-xa'])
    inchi, inchiAux = inchiWithAux.split('\n')
    # find connectivity info in the inchi string - used to detect the
    # presence of heavy atoms.
    atomsInchiConnectivity = re.search(r'/c(\d+?\*)?(.*?/?)',inchi)
    # read the mapping between heavy atoms (+ lone hydrogens) in xyz and inchi
    # from the auxiliary log
    atomsInchiAuxMap = re.search(r'/N:(.*?/)',inchiAux)
    atomsInchiAuxEquivMap = re.search(r'/E:(.*?/)',inchiAux)

    # create the rdkit mol object which simply serves as the molecule container
    rdkitMolFromMol = xyzconverters.xyzToMolToRdkitMol(xyzFileOrStr, removeHs=False)
    numAtoms = rdkitMolFromMol.GetNumAtoms()

    # initialise the atoms position dict
    atomsPositions = {k:None for k in range(numAtoms)}
    nextAtomId = 0

    mol_frags = rdkitmolutils.rdkitMolToMolFrags(rdkitMolFromMol)
    if mol_frags:
        print(f'Warning: Provided xyz file contains {len(mol_frags)} molecular fragments.')
        #return atomsPositions

    # get the positions for heavy atoms and lone hydrogens
    if atomsInchiConnectivity is not None:
        # process the atomsInchiAuxMap and extract the atoms mapping
        atomsInchiAuxMap= atomsInchiAuxMap.groups()[0] \
                        .replace('/','').replace(';',',').split(',')
        atomsInchiMatch = {int(atomId)-1: i
                           for i, atomId in enumerate(atomsInchiAuxMap)}
        atomsInchiMatchList = list(map(lambda x: int(x)-1, atomsInchiAuxMap))

        if atomsInchiMatch:
            # now disambiguate any equivalent atoms
            if atomsInchiAuxEquivMap:
                atomsInchiAuxEquivMap= atomsInchiAuxEquivMap.groups()[0] \
                                    .replace('/','').replace(')(','#').replace(')','') \
                                    .replace('(','').split('#')

                for i in range(len(atomsInchiAuxEquivMap)):
                    atomsInchiAuxEquivMap[i] = list(map(lambda x: int(x)-1, atomsInchiAuxEquivMap[i].split(',')))
                    atomsInchiAuxEquivMap[i] = list(map(lambda x: atomsInchiMatchList[x], atomsInchiAuxEquivMap[i]))

                for equivAtomsList in atomsInchiAuxEquivMap:
                    atomsXYZ = rdkitmolutils.getAtomsXYZs(rdkitMolFromMol, equivAtomsList)
                    atomsXYZ = (atomsXYZ * 1e7).astype(int)
                    atomsX = atomsXYZ[:,0].tolist()
                    atomsY = atomsXYZ[:,1].tolist()
                    atomsZ = atomsXYZ[:,2].tolist()
                    _atomsDist = rdkitmolutils.rdkitSumAllAtomsDistFromAtoms(rdkitMolFromMol, equivAtomsList)
                    _atomsDist = [int(dist * 1e7) for dist in _atomsDist]
                    equivAtomsOrder = np.lexsort((atomsZ,atomsY,atomsX,_atomsDist)).tolist()

                    currentAtomsOrder = sorted([atomsInchiMatch[equivAtomId] for equivAtomId in equivAtomsList])
                    for equivAtomPos in equivAtomsOrder:
                        atomsInchiMatch[equivAtomsList[equivAtomPos]] = currentAtomsOrder.pop(0)


            # add the atoms positions to the overall atomsPosition dictionary
            atomsPositions = {**atomsPositions, **atomsInchiMatch}
            nextAtomId = len(atomsInchiMatch)

    # assign posititions to any atoms that are left
    if nextAtomId < numAtoms:
        loneAtomsIds = [atomId
                        for atomId, refId in atomsPositions.items()
                        if refId is None]
        loneAtomsMap = {}

        atomsXYZ = rdkitmolutils.getAtomsXYZs(rdkitMolFromMol, loneAtomsIds)
        atomsXYZ = (atomsXYZ * 1e7).astype(int)
        atomsX = atomsXYZ[:,0].tolist()
        atomsY = atomsXYZ[:,1].tolist()
        atomsZ = atomsXYZ[:,2].tolist()
        _atomsDist = rdkitmolutils.rdkitSumAllAtomsDistFromAtoms(rdkitMolFromMol, loneAtomsIds)
        _atomsDist = [int(dist * 1e7) for dist in _atomsDist]
        loneAtomsOrder = np.lexsort((atomsZ,atomsY,atomsX,_atomsDist)).tolist()

        for loneAtomPos in loneAtomsOrder:
            loneAtomsMap[loneAtomsIds[loneAtomPos]] = nextAtomId
            nextAtomId += 1

        # add the remaining positions to the overall atoms positions
        atomsPositions = {**atomsPositions, **loneAtomsMap}

    # check for duplicate and None values at the end
    hasDuplicates = len(atomsPositions.values()) > len(set(atomsPositions.values()))
    hasNones = None in atomsPositions.values()
    if hasDuplicates or hasNones:
        print('Error: atom canoncial positions algorithm has failed.')
        atomsPositions= {}
    return atomsPositions

def xyzMatch(xyzTargetFileOrStr, xyzRefFileOrStr):
    """
    Matches atom indices from one xyz molecule to the other.
    Molecules must have the same topology (the same inchis).
    Outputs match dictionary containing corresponding atoms
    indices between target (keys) and reference (values)
    molecules

    Arguments:
    ----------
    xyzTargetFileOrStr : str
        target molecule (either file path or xyz string)
    xyzRefFileOrStr : str
        reference molecule (either file path or xyz string)

    Returns:
    ----------
    match: dict
        dictionary whose keys correspond to target atoms
        and values to the matched reference atoms
    """
    # create ref and target molecules with hydrogens included
    rdkitRefMol = xyzconverters.xyzToMolToRdkitMol(xyzRefFileOrStr, removeHs=False)
    rdkitTargetMol = xyzconverters.xyzToMolToRdkitMol(xyzTargetFileOrStr, removeHs=False)

    # init the final atoms match dictionary to be populated
    # keys - target Mol atom ids, values - ref Mol atom ids
    # e.g. match = [
    #  0, 5  - target atom 3 matches ref atom 5
    #  1, 3  - target atom 1 matches ref atom 3
    #  ...
    # ]
    match = {k:None for k in range(rdkitTargetMol.GetNumAtoms())}

    # check if the two mols are the same
    rdkitRefMol_inchi = rdkitconverters.rdkitMolToInchi(rdkitRefMol)
    rdkitTargetMol_inchi = rdkitconverters.rdkitMolToInchi(rdkitTargetMol)
    if rdkitRefMol_inchi == rdkitTargetMol_inchi:
        # find the best heavy atoms match first, e.g.
        # heavyAtomsMatch = {
        #  4: 2, - target heavy atom 4 matched ref heavy atom 2
        #  5: 6, - target heavy atom 5 matched ref heavy atom 6
        # }
        # I use dict here, because we only match a subset of atoms,
        # so we can not start from 0
        heavyAtomsMatch, _ = rdkitmolutils.rdkitMatchHeavyAtoms(rdkitTargetMol, rdkitRefMol)

        # match any hydrogens attached to heavy atoms
        if heavyAtomsMatch:
            match = {**match, **heavyAtomsMatch}
            # now loop over heavy atoms in the target and ref mols,
            # get hydrogens bonded to them and match them based on their
            # distance with each other
            for targetAtomId, refAtomId in heavyAtomsMatch.items():
                targetAtom = rdkitTargetMol.GetAtomWithIdx(targetAtomId)
                targetAtomHsIds = [atom.GetIdx()
                                   for atom in targetAtom.GetNeighbors()
                                   if atom.GetAtomicNum() == 1]
                targetAtomHsPos = [rdkitmolutils.getRdkitAtomXYZbyId(rdkitTargetMol,atomId)
                                   for atomId in targetAtomHsIds]

                refAtom = rdkitRefMol.GetAtomWithIdx(refAtomId)
                refAtomHsIds = [atom.GetIdx()
                                for atom in refAtom.GetNeighbors()
                                if atom.GetAtomicNum() == 1]
                refAtomHsPos = [rdkitmolutils.getRdkitAtomXYZbyId(rdkitRefMol,atomId)
                                for atomId in refAtomHsIds]

                closestHydrogens = linalg.findClosestXYZPoints(targetAtomHsPos, refAtomHsPos)

                # add matched hydrogen indices to the overall match dict
                for i, refId in enumerate(closestHydrogens):
                    # targetAtomHsIds[i] actual hydrogen id in the target molecule
                    # refAtomHsIds[refId] actual hydrogen id in the ref molecule
                    match[targetAtomHsIds[i]] = refAtomHsIds[refId]

        # check for any unmatched yet atoms
        # e.g any target atom ids whose ref ids are None
        unmatchedTargetAtomIds = [targetId
                                  for targetId, refId in match.items()
                                  if refId is None]
        if unmatchedTargetAtomIds:
            # get ref Ids not yet assigned to the target atoms
            unmatchedRefAtomIds = [refId
                                   for refId in range(rdkitTargetMol.GetNumAtoms())
                                   if refId not in match.values()]

            unmatchedTargetAtomPos = [rdkitmolutils.getRdkitAtomXYZbyId(rdkitTargetMol,atomId)
                                      for atomId in unmatchedTargetAtomIds]
            unmatchedRefAtomPos = [rdkitmolutils.getRdkitAtomXYZbyId(rdkitRefMol,atomId)
                                   for atomId in unmatchedRefAtomIds]

            fragsMatch = linalg.findClosestXYZPoints(unmatchedTargetAtomPos, unmatchedRefAtomPos)
            # add matched hydrogen indices to the overall list
            for i, refId in enumerate(fragsMatch):
                match[unmatchedTargetAtomIds[i]] = unmatchedRefAtomIds[refId]
    else:
        print('Error: Provided xyz files contain two different molecules.')

    return match

def xyzReshuffle(xyzFileOrStr, seed):
    """
    Reshuffles the atoms order in xyz molecule string.

    Arguments:
    ----------
    xyzFileOrStr : str
        molecule (either file path or xyz string)

    Returns:
    ----------
    xyzStr: str
        xyz string containing reshuffled atoms
    """
    if ioutils.fileExists(xyzFileOrStr): xyzFileOrStr= ioutils.readFile(xyzFileOrStr)

    xyzFileOrStr = xyzFileOrStr.split('\n')
    # fix for xyz files containing extra empty lines at the end
    while not xyzFileOrStr[-1]: del xyzFileOrStr[-1]
    xyzHeader, xyzFileOrStr = xyzFileOrStr[:2], xyzFileOrStr[2:]
    random.Random(seed).shuffle(xyzFileOrStr)
    xyzFileOrStr = xyzHeader + xyzFileOrStr
    return "\n".join(xyzFileOrStr)

def xyzReorderOnAtomsMatch(xyzFileOrStr, atomsMatch):
    """
    Reorders the atoms xyz molecule string given
    the atoms match dictionary.

    Arguments:
    ----------
    xyzFileOrStr : str
        molecule (either file path or xyz string)

    Returns:
    ----------
    xyzStr: str
        xyz string containing re-ordered atoms
    """
    if ioutils.fileExists(xyzFileOrStr): xyzFileOrStr= ioutils.readFile(xyzFileOrStr)
    # take care of any trailing empty lines
    xyzFileOrStr = ioutils.removeBlankTrailingLines(xyzFileOrStr)

    xyzFileOrStr = xyzFileOrStr.split('\n')
    xyzHeader, xyzFileOrStr = xyzFileOrStr[:2], xyzFileOrStr[2:]
    xyzReordered = [None]*len(xyzFileOrStr)
    for targetAtomId,refAtomId in atomsMatch.items():
        xyzReordered[refAtomId] = xyzFileOrStr[targetAtomId]
    xyzFileOrStr = xyzHeader + xyzReordered
    return "\n".join(xyzFileOrStr)

def xyzMatchWithBondAdjustment(xyzTargetFileOrStr, xyzRefFileOrStr, refAtomId1, refAtomId2, incrChange=2.5, nSteps=10):
    #
    # WORK IN PROGRESS, FEATURE NOT FINISHED
    #
    # create ref and target molecules with hydrogens included
    rdkitRefMol =xyzconverters.xyzToMolToRdkitMol(xyzRefFileOrStr, removeHs=False)
    rdkitTargetMol = xyzconverters.xyzToMolToRdkitMol(xyzTargetFileOrStr, removeHs=False)
    refInchi = rdkitconverters.rdkitMolToInchi(rdkitRefMol)
    tarInchi = rdkitconverters.rdkitMolToInchi(rdkitTargetMol)
    match = {}

    # get the chemical formula from inchi - for fragments detection
    refChemFormula = re.search(r'InChI=1S/(.*?/|\d*?H$)',refInchi).groups()[0].replace('/','')
    # detect if the molecule contains any separate fragments
    refMolFragsDetect = re.search(r'(^\d+?[A-Z]|.+?\..+?)',refChemFormula)

    rdkitRefMolLocal = copy.deepcopy(rdkitRefMol)
    rdkitTarMolLocal = copy.deepcopy(rdkitTargetMol)
    startBondLength = rdkitmolutils.getRdkitMolBondLength(rdkitRefMolLocal, refAtomId1, refAtomId2)
    bestRmsd = 1e9
    bestBondLength = startBondLength
    for step in range(nSteps):
        refInchiLocal = rdkitconverters.rdkitMolToInchi(rdkitRefMolLocal)
        refChemFormulaLocal = re.search(r'InChI=1S/(.*?/|\d*?H$)',refInchiLocal).groups()[0].replace('/','')
        refMolFragsDetectLocal = re.search(r'(^\d+?[A-Z]|.+?\..+?)',refChemFormulaLocal)
        if not refMolFragsDetectLocal: break

        newBondLength = startBondLength-incrChange*(step+1)
        rdkitmolutils.setRdkitMolBondLength(rdkitRefMolLocal, refAtomId1, refAtomId2, newBondLength)
        rmsd = rdkitmolutils.rdkitAlignMols(rdkitRefMolLocal, rdkitTargetMol)
        if rmsd < bestRmsd:
            bestRmsd = rmsd
            bestBondLength = newBondLength

    for step in range(nSteps):
        newBondLength = startBondLength+incrChange*(step+1)
        rdkitmolutils.setRdkitMolBondLength(rdkitRefMolLocal, refAtomId1, refAtomId2, newBondLength)

        refInchi2 = rdkitconverters.rdkitMolToInchi(rdkitRefMolLocal)
        tarInchi = rdkitconverters.rdkitMolToInchi(rdkitTargetMol)

        bb = rdkitRefMolLocal.GetNumAtoms()
        aa = rdkitTargetMol.GetNumAtoms()
        #rmsd = rdkitAlignMols(rdkitTarMolLocal, rdkitRefMolLocal)
        match = rdkitmolutils.rdkitSubstrAlignMatch(rdkitTargetMol, rdkitRefMolLocal)
        match = rdkitmolutils.rdkitMatchHeavyAtoms(rdkitTargetMol, rdkitRefMolLocal)
        if rmsd < bestRmsd:
            bestRmsd = rmsd
            bestBondLength = newBondLength

    match= xyzMatch(xyzTargetFileOrStr, xyzRefFileOrStr)

def getConformersXYZ(moleculeFileOrStr, inputFormat, retNumConfs, genNumConfs,
                      maxIters=1000, mmffVariant="MMFF94"):

    if ioutils.fileExists(moleculeFileOrStr): inputMol= ioutils.readFile(moleculeFileOrStr)

    if inputFormat=='xyz':
        mol = xyzconverters.xyzToMolToRdkitMol(inputMol)
    elif inputFormat=='mol':
        mol = rdkitconverters.molBlockToRdkitMol(inputMol)
    elif inputFormat=='pdb':
        mol = rdkitconverters.pdbBlockToRdkitMol(inputMol)
    else:
        inchi = obconverter.obConvert(inputMol=moleculeFileOrStr,inputMolFormat=inputFormat,
                            outputMolFormat='inchi')
        mol = rdkitconverters.inchiToRdkitMol(inchi)

    conformers = rdkitmolutils.getRdkitMolOptConformers(mol,
                        retNumConfs=retNumConfs, numConfs=genNumConfs,
                        maxIters=maxIters, mmffVariant=mmffVariant)

    conformers_with_xyz = []
    for i, (energy, confMolBlock) in enumerate(conformers):
        confXYZ = obconverter.obConvert(confMolBlock,'mol','xyz')
        confXYZ = confXYZ.split('\n')
        confXYZ[1] = f'Conformer: {i}, {mmffVariant} energy: {energy}'
        confXYZ = '\n'.join(confXYZ)
        conformers_with_xyz.append((energy,confXYZ))
    return conformers_with_xyz

#def canonicalizeXYZorientation(xyzFileOrStr):
#    if ioutils.fileExists(xyzFileOrStr): xyzFileOrStr= ioutils.readFile(xyzFileOrStr)
#    rdkitMol= xyzconverters.xyzToMolToRdkitMol(xyzFileOrStr, removeHs=False)
#    rdkitmolutils.canonicalizeMolOrientation(rdkitMol, ignoreHs=False, normalizeCovar=True)
#    return rdkitconverters.rdkitMolToXYZ(rdkitMol)

def xyzToIntertialFrame(xyzString):
    return obutils.obToInertialFrame(xyzString)


def compareXYZs(tarXYZ, refXYZ, rtol=1e-5, atol=1e-8, equal_nan=False):
    tarXYZ = ioutils.removeBlankTrailingLines(tarXYZ)
    refXYZ = ioutils.removeBlankTrailingLines(refXYZ)

    tarXYZ = tarXYZ.split('\n')
    refXYZ = refXYZ.split('\n')

    theSame = tarXYZ[0]==tarXYZ[0]
    if not theSame: return False

    tarXYZ = tarXYZ[2:]
    refXYZ = refXYZ[2:]

    for tarXYZline, refXYZline in zip(tarXYZ,refXYZ):
        tarXYZline = tarXYZline.split()
        refXYZline = refXYZline.split()
        theSame= tarXYZline[0]==refXYZline[0]
        if not theSame: return False
        for tarCoord, refCoord in zip(tarXYZline[1:],refXYZline[1:]):
            theSame= np.isclose(float(tarCoord), float(refCoord), rtol=rtol,
                atol=atol, equal_nan=equal_nan)
            if not theSame:
                return (False, tarCoord, refCoord)
    return True

def roundXYZ(xyzString, numDigits=5):
    xyzString = ioutils.removeBlankTrailingLines(xyzString)
    xyzString = xyzString.split('\n')
    xyzNumAtoms = xyzString[0]
    xyzTitle = xyzString[1]
    xyzAtoms = [xyzLine.split(' ')[0] for xyzLine in xyzString[2:]]
    fspec = f'.{numDigits}f'
    xyzRounded = [xyzNumAtoms, xyzTitle]
    for i in range(int(xyzNumAtoms)):
        atomType = xyzAtoms[i]
        x = float(xyzString[i+2].split()[1])
        y = float(xyzString[i+2].split()[2])
        z = float(xyzString[i+2].split()[3])
        xyzLine = f'{atomType} {x:{fspec}} {y:{fspec}} {z:{fspec}}'
        xyzRounded.append(xyzLine)
    return '\n'.join(xyzRounded)
