from chemutils.xyzutils import xyzToAtomsPositions, \
                               xyzMatch, \
                               xyzMatchWithBondAdjustment, \
                               xyzReorderOnAtomsMatch
from chemutils.ioutils import verbosityBasedOutput

def xyzToAtomsPositionsWrapper(xyzFileOrStr, silent=False, outFile=None):
    """
    Wrapper for the xyzToAtomsPositions function.
    Returns atom positions (order) given a molecule in an xyz format.

    The heavy atoms positions are based on inchi, thus should always
    be the same, regardless of the heavy atoms order in the xyz file.

    The hydrogen positions ARE NOT UNIQUE. They will depend, to some
    extent, on their order in the xyz file.

    Use this function to set the atoms positions in a reference
    molecule. The idea is to assign the positions once and to never
    change them again.

    Arguments:
    ----------
    xyzFileOrStr : str
        input xyz molecule (either file path or xyz string)
    silent : str, optional, default = False
        prints result to stdout if not False
    outFile : str, optional, default = None
        if specified, the final result will be
        additionally written to the file

    Returns:
    ----------
    atomsPositions: list
    """
    atomsPositions = xyzToAtomsPositions(xyzFileOrStr)
    verbosityBasedOutput(atomsPositions, silent, outFile)
    return atomsPositions

def xyzReorderToxyz(xyzTargetFileOrStr, xyzRefFileOrStr, silent=False, outFile=None):
    """
    Matches atom indices from one xyz molecule to the other.
    Molecules must have the same topology (the same inchis).
    Outputs the target molecule where the atoms order is the
    same as in the reference molecule.

    Arguments:
    ----------
    xyzTargetFileOrStr : str
        target molecule (either file path or xyz string)
    xyzRefFileOrStr : str
        reference molecule (either file path or xyz string)
    silent : str, optional, default = False
        prints result to stdout if not False
    outFile : str, optional, default = None
        if specified, the final result will be
        additionally written to the file

    Returns:
    ----------
    xyzTargetStr: str
        reordered target molecule xyz
    """
    match = xyzMatch(xyzTargetFileOrStr, xyzRefFileOrStr)
    xyzTargetStr = ''
    if not match:
        print('Error: Couldnt match the molecules.')
    else:
        xyzTargetStr = xyzReorderOnAtomsMatch(xyzTargetFileOrStr, match)
        verbosityBasedOutput(xyzTargetStr, silent, outFile)
    return xyzTargetStr

def xyzReorderToxyzFlexBond(xyzTargetFileOrStr, xyzRefFileOrStr, refAtomId1, refAtomId2, silent=False, outFile=None):
    """
    Matches atom indices from one xyz molecule to the other.
    Molecules must have the same topology (the same inchis).
    Outputs the target molecule where the atoms order is the
    same as in the reference molecule.

    Arguments:
    ----------
    xyzTargetFileOrStr : str
        target molecule (either file path or xyz string)
    xyzRefFileOrStr : str
        reference molecule (either file path or xyz string)
    silent : str, optional, default = False
        prints result to stdout if not False
    outFile : str, optional, default = None
        if specified, the final result will be
        additionally written to the file

    Returns:
    ----------
    xyzTargetStr: str
        reordered target molecule xyz
    """
    match = xyzMatchWithBondAdjustment(xyzTargetFileOrStr, xyzRefFileOrStr, int(refAtomId1), int(refAtomId2))
    xyzTargetStr = ''
    if not match:
        print('Error: Couldnt match the molecules.')
    else:
        xyzTargetStr = xyzReorderOnAtomsMatch(xyzTargetFileOrStr, match)
        verbosityBasedOutput(xyzTargetStr, silent, outFile)
    return xyzTargetStr