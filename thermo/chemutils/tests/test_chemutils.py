import os
import chemutils.main
import chemutils.mainutils.mainutils as mainutils
import chemutils.xyzutils.xyztools as xyztools
import chemutils.ioutils.ioutils as ioutils
import docopt
import pytest
import json

THIS_DIR = os.path.dirname(os.path.abspath(__file__))

def getRefName(testLog,jobIndex,numJobs):
    if numJobs > 1:
        refName = testLog + '_' + str(jobIndex+1)+'.json'
    else:
        refName = testLog + '.json'
    return refName

@pytest.mark.parametrize("testFilesDir, testFileName",
[
('xyzFileSamples','h.xyz'),
('xyzFileSamples','h_h.xyz'),
('xyzFileSamples','h2.xyz'),
('xyzFileSamples','c2h6.xyz'),
('xyzFileSamples','ethanol.xyz'),
('xyzFileSamples','example1_c2h5_h_L5_L10.xyz'),
('xyzFileSamples','example2_ch3sih2_f_L5_L10.xyz'),
('xyzFileSamples','example3_ch3_ch2oh_L3_L8.xyz'),
('xyzFileSamples','example4_circumpyrene-2-nyl_ic_L33_L97.xyz'),
('xyzFileSamples','example5_IS56_ic_L16_L79.xyz'),
('xyzFileSamples','example6_pyrene-2-nyl_ic_L17_L27.xyz'),
('xyzFileSamples','example7_pyrene-2-nyl_ic_mod_L17_L27.xyz'),
('xyzFileSamples','cholesterol.xyz'),
('xyzFileSamples','teos.xyz')
]
)
def test_match_xyzToxyz(testFilesDir, testFileName):
    print('========================================================')
    print('TEST DIR: ', testFilesDir)
    print('TEST LOG: ', testFileName)
    testPath = os.path.join(THIS_DIR, testFilesDir)
    refFile = os.path.join(testPath, testFileName)
    refXYZ = ioutils.removeBlankTrailingLines(ioutils.readFile(refFile))
    for i in range(10):
        tarXYZ = xyztools.xyzReshuffle(refXYZ, seed=i)
        match = xyztools.xyzMatch(tarXYZ,refXYZ)
        tarXYZ = xyztools.xyzReorderOnAtomsMatch(tarXYZ, match)
        assert tarXYZ == refXYZ
    print('========================================================')

@pytest.mark.parametrize("testFilesDir, testFileName, regenerateResults",
[
('xyzFileSamples','h_h.xyz', False),
('xyzFileSamples','h2.xyz', False),
('xyzFileSamples','c2h6.xyz', False),
('xyzFileSamples','ethanol.xyz', False),
('xyzFileSamples','example1_c2h5_h_L5_L10.xyz', False),
('xyzFileSamples','example2_ch3sih2_f_L5_L10.xyz', False),
('xyzFileSamples','example3_ch3_ch2oh_L3_L8.xyz', False),
('xyzFileSamples','example4_circumpyrene-2-nyl_ic_L33_L97.xyz', False),
('xyzFileSamples','example5_IS56_ic_L16_L79.xyz', False),
('xyzFileSamples','example6_pyrene-2-nyl_ic_L17_L27.xyz', False),
('xyzFileSamples','example7_pyrene-2-nyl_ic_mod_L17_L27.xyz', False),
('xyzFileSamples','teos.xyz', False)
]
)
def test_xyzToAtomsPositions(testFilesDir, testFileName,
            regenerateResults, regenerateAllResults=False):
    print('========================================================')
    print('TEST DIR: ', testFilesDir)
    print('TEST LOG: ', testFileName)
    testPath = os.path.join(THIS_DIR, testFilesDir)
    testFile = os.path.join(testPath, testFileName)
    refFile = testFile + '_atomspositions.json_ref'

    testAtomsPos = json.dumps(
                        mainutils.xyzToAtomsPositionsWrapper(
                            testFile,
                            noOutFile=True)[0],
                        indent=4)
    if regenerateResults or regenerateAllResults:
        # dump just parsed data as the ref data
        ioutils.writeFile(refFile,testAtomsPos)

    refAtomsPos = ioutils.readFile(refFile)
    assert testAtomsPos == refAtomsPos
    print('========================================================')


@pytest.mark.parametrize("args, exp_file_output",
[
    (['genconfs',
    'c2h6.xyz',
    '--input-format', 'xyz'],
    'c2h6.xyz_conformer_0.xyz'
    ),
    (['genconfs',
    'ethanol.xyz',
    '--input-format', 'xyz'],
    'ethanol.xyz_conformer_0.xyz'
    ),
    (['genconfs',
    'cholesterol.inchi',
    '--input-format', 'inchi',
    '--gen-num-confs', '10'],
    'cholesterol.inchi_conformer_0.xyz'
    ),
    (['atomspos',
    'c2h6.xyz',
    '--no-file-output'],
    None
    ),
    (['convert',
    'c2h6.xyz',
    'xyz',
    'inchi',
    '--no-file-outputs'],
    None
    )
]
)
def test_cli(args,exp_file_output,monkeypatch):
    args[1] = os.path.join(THIS_DIR,'xyzFileSamples', args[1])
    args.insert(0, 'test_'+args[0])
    monkeypatch.setattr("sys.argv", args)
    try:
        chemutils.main.start()
    except docopt.DocoptExit:
        assert False

    if exp_file_output is not None:
        exp_file_output = os.path.join(THIS_DIR,'xyzFileSamples', exp_file_output)
        assert os.path.isfile(exp_file_output)



#
#@pytest.mark.parametrize("testFilesDir, testFileName",
#[
#('xyzFileSamples','ethanol.xyz')
#]
#)
#def test_connectFrags(testFilesDir, testFileName):
#    pass
#
#@pytest.mark.parametrize("testFilesDir, testFileName",
#[
#('xyzFileSamples','ethanol.xyz')
#]
#)
#def test_findFrags(testFilesDir, testFileName):
#    pass