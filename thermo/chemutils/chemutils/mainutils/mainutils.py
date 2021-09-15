import chemutils.xyzutils.xyztools as xyztools
import chemutils.xyzutils.xyzconverters as xyzconverters
import chemutils.xyzutils.xyztools as xyztools
import chemutils.ioutils.ioutils as ioutils
import chemutils.obabelutils.obconverter as obconverter
import os

def xyzToAtomsPositionsWrapper(xyzFileOrDir, outDir=None, fileExt=None, noOutFile=False):
    """
    Wrapper for the xyzToAtomsPositions function.
    Returns atom positions (order) given a molecule9s) in an xyz format.

    The heavy atoms positions are based on inchi, thus should always
    be the same, regardless of the heavy atoms order in the xyz file.

    The hydrogen positions ARE NOT UNIQUE. They will depend, to some
    extent, on their order in the xyz file.

    Use this function to set the atoms positions in a reference
    molecule. The idea is to assign the positions once and to never
    change them again.

    Arguments:
    ----------
    xyzFileOrDir : str
        input xyz molecule (either file or dir path)
    outDir : str, optional, default = cwd
        directory to write the results into
    fileExt : str, optional, default = xyz
        extension of the input files
    noOutFile : bool, optional, default False
        suppresses writing any output files

    Returns:
    ----------
    atomsPositions: list of tuples
                    also written to a file
    """

    if outDir is None: outDir= os.getcwd()
    if fileExt is None: fileExt= '.xyz'

    xyz_files = ioutils.getFilesWithExtensions(xyzFileOrDir, fileExt)
    if not xyz_files: return

    atomsPositions = []
    for xyz_file in xyz_files:
        _atomsPositions = xyztools.xyzToAtomsPositions(xyz_file)

        filebasename = ioutils.getFileBaseName(xyz_file)
        if not noOutFile:
            outPath = os.path.join(outDir,filebasename+'atomspositions.csv')
            ioutils.writeFile(path=outPath,
                 data=','.join([str(v) for v in _atomsPositions.values()]),
                 newline='')

        atomsPositions.append((filebasename,_atomsPositions))

    print(atomsPositions)
    return atomsPositions

def obConvertWrapper(inputFileOrDir, convertFrom, convertTo, convOptions=None,
                     outDir=None, fileExt=None, noOutFile=False):

    """
    Wrapper for the obConver function from open babel.
    Returns a molecule converted to a desired format. The convertFrom,
    convertTo and convOptions values are those allowed by open babel.

    Arguments:
    ----------
    inputFileOrDir : str
        input molecule file or dir path
    convertFrom : str
        format of the input molecule
    convertTo : str
        desired format of the output molecule
    convOptions : str
        any open babel conversion options
    outDir : str, optional, default = cwd
        directory to write the results into
    fileExt : str, optional, default = xyz
        extension of the input files
    noOutFile : bool, optional, default False
        suppresses writing any output files

    Returns:
    ----------
    convertedMols: list of tuples
                   also written to files
    """

    if fileExt is None: fileExt=''
    if outDir is None: outDir= os.getcwd()
    if convOptions is not None: convOptions = convOptions.split(' ')

    xyz_files = ioutils.getFilesWithExtensions(inputFileOrDir, fileExt)
    if not xyz_files: return

    convertedMols = []
    for xyz_file in xyz_files:
        _convertedMol = obconverter.obConvert(xyz_file, convertFrom, convertTo, convOptions)

        filebasename = ioutils.getFileBaseName(xyz_file)
        if not noOutFile:
            outPath = os.path.join(outDir,filebasename+'.'+convertTo)
            ioutils.writeFile(path=outPath, data=_convertedMol, newline='')

        convertedMols.append((filebasename,_convertedMol))

    print(convertedMols)
    return convertedMols


def xyzReorderToxyz(xyzTargetFile, xyzRefFile, outDir=None, noOutFile=False):
    """
    Matches atom indices from one xyz molecule to the other.
    Molecules must have the same topology (the same inchis).
    Outputs the target molecule where the atoms order is the
    same as in the reference molecule.

    Arguments:
    ----------
    xyzTargetFile : str
        target molecule file path, in xyz format
    xyzRefFileOrStr : str
        reference molecule file path in xyz format
    outDir : str, optional, default = cwd
        directory to write the results into
    noOutFile : bool, optional, default False
        suppresses writing any output files

    Returns:
    ----------
    xyzTargetReordered: str
        reordered target molecule xyz
        also written to a file
    """

    if outDir is None: outDir= os.getcwd()

    if not ioutils.fileExists(xyzTargetFile): return
    if not ioutils.fileExists(xyzRefFile): return

    atomsMatch = xyztools.xyzMatch(xyzTargetFile, xyzRefFile)
    if not atomsMatch:
        print('Error: Couldnt match the molecules.')
        return

    xyzTargetReordered = xyztools.xyzReorderOnAtomsMatch(xyzTargetFile, atomsMatch)
    filebasename = ioutils.getFileBaseName(xyzTargetFile)

    print(f'Reordered {filebasename} file: \n{xyzTargetReordered}')
    if not noOutFile:
        outPath = os.path.join(outDir,filebasename+'_reordered.xyz')
        ioutils.writeFile(path=outPath, data=xyzTargetReordered, newline='')
    return xyzTargetReordered

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
    match = xyztools.xyzMatchWithBondAdjustment(xyzTargetFileOrStr, xyzRefFileOrStr, int(refAtomId1), int(refAtomId2))
    xyzTargetStr = ''
    if not match:
        print('Error: Couldnt match the molecules.')
    else:
        xyzTargetStr = xyztools.xyzReorderOnAtomsMatch(xyzTargetFileOrStr, match)
    return xyzTargetStr

def xyzToGaussianInputWrapper(xyzFileOrDir, jobRoute, charge, spinMult, memory, \
                              numCpus, outDir, fileExt, noOutFile=False):

    if fileExt is None: fileExt='.xyz'
    if outDir is None: outDir= os.getcwd()

    xyz_files = ioutils.getFilesWithExtensions(xyzFileOrDir, fileExt)
    if not xyz_files: return

    ginputs = []
    for xyz_file in xyz_files:
        _ginput = xyzconverters.xyzToGaussianInput(xyz_file, jobRoute, charge, spinMult, memory,numCpus)

        filebasename = ioutils.getFileBaseName(xyz_file)

        if not noOutFile:
            outPath = os.path.join(outDir,xyz_file+'.gau')
            ioutils.writeFile(path=outPath, data=_ginput, newline='')

        ginputs.append((filebasename,_ginput))
    return ginputs

def getConformersXYZWrapper(moleculeFileOrDir, inputFormat, retNumConfs,
                            genNumConfs, maxIters, mmffVariant, outDir,
                            fileExt, noOutFile=False):

    if fileExt is None: fileExt=''
    if outDir is None: outDir= os.getcwd()

    mol_files = ioutils.getFilesWithExtensions(moleculeFileOrDir, fileExt)
    if not mol_files: return

    all_conformers = []
    for mol_file in mol_files:
        conformers = xyztools.getConformersXYZ(mol_file, inputFormat, retNumConfs,
                            genNumConfs, maxIters, mmffVariant)

        filebasename = ioutils.getFileBaseName(mol_file)
        all_conformers = [(filebasename, conformers)]
        for i, (energy, confXYZ) in enumerate(conformers):
            confXYZ = confXYZ.split('\n')
            confXYZ[1] = f'{mmffVariant} energy: {energy}'
            confXYZ = '\n'.join(confXYZ)
            print(f'Conformer {i}, geometry: \n {confXYZ}')
            if not noOutFile:
                outPath = os.path.join(outDir,filebasename+'_conformer_'+str(i)+'.xyz')
                ioutils.writeFile(path=outPath, data=confXYZ, newline='')
    return all_conformers