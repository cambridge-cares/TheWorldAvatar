import os
from chemutils.xyzutils import xyzMatch, \
                               xyzReshuffle, \
                               xyzReorderOnAtomsMatch, \
                               xyzToAtomsPositions
from chemutils.ioutils import readFile, writeFile, \
                              removeBlankTrailingLines
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
    refXYZ = removeBlankTrailingLines(readFile(refFile))
    for i in range(10):
        tarXYZ = xyzReshuffle(refXYZ, seed=i)
        match = xyzMatch(tarXYZ,refXYZ)
        tarXYZ = xyzReorderOnAtomsMatch(tarXYZ, match)
        assert tarXYZ == refXYZ
    print('========================================================')

@pytest.mark.parametrize("testFilesDir, testFileName, regenerateResults",
[
('xyzFileSamples','h.xyz', False),
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
def test_xyzToAtomsPositions(testFilesDir, testFileName, regenerateResults):
    print('========================================================')
    print('TEST DIR: ', testFilesDir)
    print('TEST LOG: ', testFileName)
    testPath = os.path.join(THIS_DIR, testFilesDir)
    testFile = os.path.join(testPath, testFileName)
    refFile = testFile + '_atomPosRef.json'
    testXYZ = removeBlankTrailingLines(readFile(testFile))

    testAtomsPos = json.dumps(xyzToAtomsPositions(xyzFileOrStr=testXYZ))
    if regenerateResults:
        # dump just parsed data as the ref data
        writeFile(refFile,testAtomsPos)

    refAtomsPos = readFile(refFile)
    assert testAtomsPos == refAtomsPos
    print('========================================================')

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