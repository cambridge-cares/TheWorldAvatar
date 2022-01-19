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
    # setting the main xyzToAtomsPositions function.
    # at the moment it is the inchi-based algorithm with
    # hydrogen atoms replacement. Once the inchi-based
    # algorithm without the hydrogen replacement is mature
    # enough I will use it instead.
    return xyzToAtomsPositionsNoInChi(xyzFileOrStr)

def xyzToAtomsPositionsNoInChi(xyzFileOrStr):
    """
    Returns atom positions (order) given a molecule in an xyz format.
    The atoms order is determined using four invariants, in the
    following order of precedence:
        1) Largest sum of distances from all other atoms
        2) Largest X coordinate
        3) Largest Y coordinate
        4) Largest Z coordinate

    Pros:
    This is the possible simplest algorithm to canonicalize
    atoms order in molecules.
    The algorithm works with single molecules and disconnected
    molecular fragments out of the box.

    Cons:
    Since the algorithm doesnt use any topological information
    (atoms connectivity) it is sensitive to the quality of the
    xyz coordinates. This means that atoms canonical order may
    change if their positions are distorted too much with
    respoect to each other.

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

    # create the rdkit mol object
    rdkitMolFromMol = xyzconverters.xyzToMolToRdkitMol(xyzFileOrStr, removeHs=False)
    numAtoms = rdkitMolFromMol.GetNumAtoms()

    # initialise the atoms position dict
    atomsPositions = {k:None for k in range(numAtoms)}
    atomsIds = list(atomsPositions.keys())
    nextAtomId = 0

    atomsXYZ = rdkitmolutils.getAtomsXYZs(rdkitMolFromMol, atomsIds)
    atomsXYZ = (atomsXYZ * 1e7).astype(int)
    atomsX = atomsXYZ[:,0].tolist()
    atomsY = atomsXYZ[:,1].tolist()
    atomsZ = atomsXYZ[:,2].tolist()
    _atomsDist = rdkitmolutils.rdkitSumAllAtomsDistFromAtoms(rdkitMolFromMol, atomsIds)
    _atomsDist = [int(dist * 1e7) for dist in _atomsDist]
    atomsOrder = np.lexsort((atomsZ,atomsY,atomsX,_atomsDist)).tolist()

    for atomPos in atomsOrder:
        atomsPositions[atomsIds[atomPos]] = nextAtomId
        nextAtomId += 1

    # check for duplicate and None values at the end
    hasDuplicates = len(atomsPositions.values()) > len(set(atomsPositions.values()))
    hasNones = None in atomsPositions.values()
    if hasDuplicates or hasNones:
        print('Error: atom canoncial positions algorithm has failed.')
        atomsPositions= {}
    return atomsPositions


def xyzToAtomsPositionsWithInChi(xyzFileOrStr):
    """
    Returns atom positions (order) given a molecule in an xyz format.
    Inchi-based algorithm.
    The heavy atoms order is the inchi order. Any topologically
    equivalent atoms indicated by inchi are disammbiguated using
    four invariants, in the following order of precedence:
        1) Largest sum of distances from all other atoms
        2) Largest X coordinate
        3) Largest Y coordinate
        4) Largest Z coordinate

    Hydrogen atoms order is assigned by looping over heavy atoms
    in their found order of precendce. If more than one hydrogen
    is attached to a heavy atom, then all of them are treated
    as topologically equivalent and their order is disambiguated
    using four invariants, in the following order of precedence:
        1) Largest sum of distances from all other atoms
        2) Largest X coordinate
        3) Largest Y coordinate
        4) Largest Z coordinate

    Pros:
    Gives atom orders that are closest to the inchi algorithm
    atoms ordering. The atoms order is much less affected by the
    quality of the xyz coordinates. No hydrogen atoms swapping.

    Cons:
    The most difficult algorithm to write. At the moment, it
    doesnt work with disconnected molecular fragments.

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

    inchiWithAux = obconverter.obConvert(inputMol=xyzFileOrStr,inputMolFormat='xyz',
                            outputMolFormat='inchi', options=['-xa'])
    inchi, inchiAux = inchiWithAux.split('\n')

    atomsInchiAuxMap = re.search(r'/N:(?P<atomsAuxMap>.*?)(?=/|$)',inchiAux)
    atomsInchiAuxEquivMap = re.search(r'/E:(?P<equivMap>.*?)(?=/|$)',inchiAux)
    chemFormula = re.search(r'1S/(?P<chemFormula>.*?)(?=/|$)',inchi)

    # process all regex, if match found
    if atomsInchiAuxMap:atomsInchiAuxMap = atomsInchiAuxMap.group('atomsAuxMap')
    if atomsInchiAuxEquivMap: atomsInchiAuxEquivMap = atomsInchiAuxEquivMap.group('equivMap')
    if chemFormula: chemFormula = chemFormula.group('chemFormula')

    # create the rdkit mol object
    rdkitMolFromMol = xyzconverters.xyzToMolToRdkitMol(xyzFileOrStr, removeHs=False)
    numAtoms = rdkitMolFromMol.GetNumAtoms()

    # initialise the atoms position dict
    atomsPositions = {k:None for k in range(numAtoms)}
    nextAtomId = 0
    posToUse = [i for i in range(numAtoms)]

    # this removes any h2 or h indices from the inchi aux info
    # instead, it creates the equivH2_and_H array which contains
    # H2 and H atom indices with atoms equivalency included
    equivH2_and_H, atomsInchiAuxMap, atomsInchiAuxEquivMap= _preprocessInchiAuxInfo(
            rdkitMolFromMol,
            chemFormula,
            atomsInchiAuxMap,
            atomsInchiAuxEquivMap)

    # get the atoms based on the inchi connectivity info
    if atomsInchiAuxMap:
        # process the atomsInchiAuxMap and extract the atoms mapping
        atomsInchiAuxMapFrags = atomsInchiAuxMap

        atomsInchiAuxMap= atomsInchiAuxMap.replace('/','') \
                            .replace(';',',').split(',')

        atomsInchiMatch = {int(atomId)-1: i
                           for i, atomId in enumerate(atomsInchiAuxMap)}

        if atomsInchiMatch:
            equivMap = []
            # now disambiguate any equi-valent atoms
            if atomsInchiAuxEquivMap:
                # this processes the atoms equivalency map produced by inchi
                equivMap = _getAtomsInchiAuxEquivMap(
                    atomsInchiAuxEquivMap,
                    atomsInchiAuxMapFrags)

                # firstly sort the order of any heavy equivalent atoms
                for equivAtomsList in equivMap:
                    atomsXYZ = rdkitmolutils.getAtomsXYZs(rdkitMolFromMol, equivAtomsList)
                    atomsXYZ = (atomsXYZ * 1e7).astype(int)
                    atomsX = atomsXYZ[:,0].tolist()
                    atomsY = atomsXYZ[:,1].tolist()
                    atomsZ = atomsXYZ[:,2].tolist()
                    _atomsDist = rdkitmolutils.rdkitSumAllAtomsDistFromAtoms(rdkitMolFromMol, equivAtomsList)
                    _atomsDist = [int(dist * 1e7) for dist in _atomsDist]
                    # use four invariants to disambiguate atoms
                    equivAtomsOrder = np.lexsort((atomsZ,atomsY,atomsX,_atomsDist)).tolist()

                    currentAtomsOrder = sorted([atomsInchiMatch[equivAtomId] for equivAtomId in equivAtomsList])
                    for equivAtomPos in equivAtomsOrder:
                        atomsInchiMatch[equivAtomsList[equivAtomPos]] = currentAtomsOrder.pop(0)

            # add the atoms positions to the overall atomsPosition dictionary
            atomsPositions = {**atomsPositions, **atomsInchiMatch}
            nextAtomId = len(atomsInchiMatch)

    atomsPosNoNones = {k: v for k, v in atomsPositions.items() if v is not None}
    atomsSorted = {k: v for k, v in sorted(atomsPosNoNones.items(), key=lambda item: item[1])}
    posToUse = [i for i in range(numAtoms) if i not in atomsPositions.values()]

    # now, sort the order of any H2 and H fragments (equivalent or not)
    for equivAtomsList in equivH2_and_H:
        atomsXYZ = rdkitmolutils.getAtomsXYZs(rdkitMolFromMol, equivAtomsList)
        atomsXYZ = (atomsXYZ * 1e7).astype(int)
        atomsX = atomsXYZ[:,0].tolist()
        atomsY = atomsXYZ[:,1].tolist()
        atomsZ = atomsXYZ[:,2].tolist()
        _atomsDist = rdkitmolutils.rdkitSumAllAtomsDistFromAtoms(rdkitMolFromMol, equivAtomsList)
        _atomsDist = [int(dist * 1e7) for dist in _atomsDist]
        # use four invariants to disambiguate atoms
        equivAtomsOrder = np.lexsort((atomsZ,atomsY,atomsX,_atomsDist)).tolist()
        currentAtomsOrder = [i for i in posToUse[:len(equivAtomsList)]]
        for equivAtomPos in equivAtomsOrder:
            atomsPositions[equivAtomsList[equivAtomPos]] = currentAtomsOrder[0]
            posToUse.remove(currentAtomsOrder.pop(0))
            nextAtomId +=1

    # now find all hydrogens attached to already matched heavy atoms
    # group hydrogens together if they belong to equivalent heavy
    # atoms
    hydrogenMap = {}
    for atomId in atomsSorted:
        atom_i = rdkitMolFromMol.GetAtomWithIdx(atomId)
        if atom_i.GetAtomicNum() > 1:
            min_atom_i = atomId
            for equivLists in equivMap:
                if atomId in equivLists:
                    min_atom_i = min(equivLists)

            hydrogenIds = [atom.GetIdx()
                        for atom in atom_i.GetNeighbors()
                        if atom.GetAtomicNum() == 1]
            if hydrogenIds:
                if min_atom_i not in hydrogenMap.keys():
                    hydrogenMap[min_atom_i] = hydrogenIds
                else:
                    hydrogenMap[min_atom_i].extend(hydrogenIds)

    # sort the order of any hydrogens attached to already
    # matched heavy atoms
    for _ , equivAtomsList in hydrogenMap.items():
        if len(equivAtomsList)>1:
            atomsXYZ = rdkitmolutils.getAtomsXYZs(rdkitMolFromMol, equivAtomsList)
            atomsXYZ = (atomsXYZ * 1e7).astype(int)
            atomsX = atomsXYZ[:,0].tolist()
            atomsY = atomsXYZ[:,1].tolist()
            atomsZ = atomsXYZ[:,2].tolist()
            _atomsDist = rdkitmolutils.rdkitSumAllAtomsDistFromAtoms(rdkitMolFromMol, equivAtomsList)
            _atomsDist = [int(dist * 1e7) for dist in _atomsDist]
            # use four invariants to disambiguate atoms
            equivAtomsOrder = np.lexsort((atomsZ,atomsY,atomsX,_atomsDist)).tolist()
        else:
            equivAtomsOrder = [0]
        currentAtomsOrder = [i for i in posToUse[:len(equivAtomsList)]]
        for equivAtomPos in equivAtomsOrder:
            atomsPositions[equivAtomsList[equivAtomPos]] = currentAtomsOrder[0]
            posToUse.remove(currentAtomsOrder.pop(0))
            nextAtomId +=1

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


def _getAtomsInchiAuxEquivMap(equivAtomsMapFrags, atomsInchiAuxMapFrags):
    """Creates the final equivalence atoms mapping array"""
    # atomsInchiAuxMapFrags - this a raw list of atoms pos assigned by the inchi
    equivAtomsMapFrags = equivAtomsMapFrags.split(';')
    equivAtomsMapFrags = [eqvFrag for eqvFrag in equivAtomsMapFrags if eqvFrag != '']
    #atomsInchiAuxEquivMap - this is a raw equivalence atoms string from inchi
    atomsInchiAuxMapFrags = atomsInchiAuxMapFrags.split(';')

    atomFragsId = 0

    equivAtomsMapOut = []
    for equivAtomsMap in equivAtomsMapFrags:
        equivAtomsProcessed, atomFragsId = _procesxEquivAtomsMap(equivAtomsMap, atomsInchiAuxMapFrags, atomFragsId)
        for equivAtomsIds in equivAtomsProcessed:
            equivAtomsMapOut.append(equivAtomsIds)
    return equivAtomsMapOut

def _procesxEquivAtomsMap(equivAtomsMap, atomsInchiMapFrags, atomFragsId):
    """This function processes the inchi aux equivalency info.

    1) It replaces the zero-based equiv atoms indices referring to the
    inchi aux map lists, with the actual atoms indices from those lists

    2) In case of n separate fragments, e.g. 2C2H6, 2CH3, it expands
    the equivalence aux info merging the heavy atoms indices from all
    equivalent fragments
    """
    # inchi equiv atoms multiplier match
    # e.g. for equiv atoms = 2*(1,2) it would match the first 2
    equivAtomsMultMatch = re.match(r'(?P<equivAtomsMult>\d+?)\*',equivAtomsMap)
    equivAtomsMult = 1
    equivAtomsTempStr = ''
    if equivAtomsMultMatch:
        # if multiplier found, it means that there are two the same fragments
        equivAtomsMultStr = equivAtomsMultMatch.group('equivAtomsMult')
        equivAtomsMult = int(equivAtomsMultStr)

        equivAtomsMap = equivAtomsMap.replace(equivAtomsMultStr+'*','')

        # this adds extra equivalent atoms for the same fragments, that
        # were missed by the inchi algorithm because the
        # fragments are apart
        atomsInchiMapFrags_i = atomsInchiMapFrags[atomFragsId]
        for k in range(len(atomsInchiMapFrags_i.split(','))):
            if str(k+1) not in equivAtomsMap:
                equivAtomsMap = f'{equivAtomsMap}({k+1})'

    equivAtomsMap = equivAtomsMap.replace(')(','#') \
                            .replace(')','') \
                            .replace('(','')
    i = 0
    while i< len(equivAtomsMap):
        digit_to_repl = ''

        while True:
            digit_to_repl = digit_to_repl + equivAtomsMap[i]
            i+=1
            commaFound = digit_to_repl[-1] == ','
            hashFound = digit_to_repl[-1] == '#'
            eolFound = i== len(equivAtomsMap)
            if commaFound or hashFound:
                digit_to_repl = digit_to_repl[:-1]
                break
            if eolFound: break

        digit_to_repl = int(digit_to_repl)-1
        for j in range(equivAtomsMult):
            atFrags = atomsInchiMapFrags[atomFragsId+j].split(',')
            equivAtomsTempStr = f'{equivAtomsTempStr}{int(atFrags[digit_to_repl])-1},'

        if hashFound: equivAtomsTempStr = equivAtomsTempStr[:-1]+'#'
        if eolFound: equivAtomsTempStr = equivAtomsTempStr[:-1]
    atomFragsId = atomFragsId + equivAtomsMult

    equivAtomsArray = list(map(lambda x: x.split(',') ,equivAtomsTempStr.split('#')))
    for i in range(len(equivAtomsArray)):
        equivAtomsArray[i] = list(map(lambda x: int(x) , equivAtomsArray[i]))
    return equivAtomsArray, atomFragsId

def _preprocessInchiAuxInfo(rdkitMolFromMol, chemFormula, atomsInchiAuxMap, atomsInchiAuxEquivMap):
    """This preprocesses the inchi aux info in the following way:

       1) Removes H2 and H fragments aux info.

       The problem is that inchi aux info for H2 and H fragments
       is incomplete to be processed in the same way as the aux
       info of the heavy atoms. Therefore it is simple to remove
       it from aux string and write H2 and H indices and equivalencies
       explicitly in a separate list

       2) Finds any equivalent single heavy atom fragments, e.g.
       2CH3 etc and adds them to the inchi aux string.

       The problem here is that inchi fails to recognize that such
       fragments are equivalent.
    """

    # the code is ugly and needs improvement....

    atomsInchiAuxMap =  atomsInchiAuxMap.split(';')

    # unroll atomsInchiAuxEquivMap - so that we could easier process it
    atomsInchiAuxEquivMapUnrolled = []
    if atomsInchiAuxEquivMap:
        atomsInchiAuxEquivMapUnrolled = ''
        atomsInchiAuxEquivMap = atomsInchiAuxEquivMap.split(';')

        for equivMap in atomsInchiAuxEquivMap:
            atomsInchiAuxEquivMapUnrolled = atomsInchiAuxEquivMapUnrolled + equivMap + ';'

            multMatch = re.search('(?P<mult>^\d+?)\*',equivMap)
            if multMatch:
                mult = int(multMatch.group('mult'))
                for i in range(mult-1):
                    atomsInchiAuxEquivMapUnrolled = atomsInchiAuxEquivMapUnrolled + '-1;'
        atomsInchiAuxEquivMapUnrolled = atomsInchiAuxEquivMapUnrolled[:-1].split(';')
    else:
        if atomsInchiAuxMap:
            for i in range(len(atomsInchiAuxMap)):
                atomsInchiAuxEquivMapUnrolled.append('')

    atomsInchiAuxMapList = list(map( lambda x: x.split(','), atomsInchiAuxMap))

    # use chem formula to find any molecular fragments
    chemFormulaMults = []
    chemFormula = chemFormula.split('.')
    for chemFormulaFrag in chemFormula:
        multMatch = re.search('(?P<mult>^\d+?)[a-zA-Z]',chemFormulaFrag)
        if multMatch:
            mult = int(multMatch.group('mult'))
            chemFormulaMults.append(mult)
            for i in range(mult-1):
                chemFormulaMults.append(-1)
        else:
            chemFormulaMults.append(1)

    # now remove any H2 and H fragments aux info and move it
    # into its own array
    equivH = []
    equivH2 = []
    equivH2_and_H = []
    for j, atomsMap in enumerate(atomsInchiAuxMapList):
        for atomId in atomsMap:
            atomId = int(atomId)-1
            atom_i = rdkitMolFromMol.GetAtomWithIdx(atomId)
            if atom_i.GetAtomicNum() == 1:
                hydrogenIds = [atom.GetIdx()
                        for atom in atom_i.GetNeighbors()
                        if atom.GetAtomicNum() == 1]
                atomsInchiAuxMap[j] = '-1'
                if hydrogenIds:
                    equivH2.append(atomId)
                    equivH2.extend(hydrogenIds)
                else:
                    equivH.append(atomId)
                if atomsInchiAuxEquivMapUnrolled:
                    atomsInchiAuxEquivMapUnrolled[j] = '-1'
                if chemFormulaMults[j] > 1: chemFormulaMults[j] = -1

    if equivH2:
        equivH2_and_H.append(equivH2)
    if equivH:
        equivH2_and_H.append(equivH)

    # now fix the single heavy atoms fragments equivalency info
    for j, (chemFormMult, atomsMap) in enumerate(zip(chemFormulaMults, atomsInchiAuxMapList)):
        if chemFormMult > 1 and len(atomsMap)==1:
            atomsInchiAuxEquivMapUnrolled[j] = f'{chemFormMult}*(1)'

    if atomsInchiAuxEquivMapUnrolled:
        atomsInchiAuxEquivMap = ';'.join([x for x in atomsInchiAuxEquivMapUnrolled if x != '-1'])
    atomsInchiAuxMap = ';'.join([x for x in atomsInchiAuxMap if x != '-1'])

    return equivH2_and_H, atomsInchiAuxMap, atomsInchiAuxEquivMap