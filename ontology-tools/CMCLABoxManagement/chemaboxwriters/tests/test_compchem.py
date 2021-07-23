import os
from compchemparser.helpers.utils import getRefName

THIS_DIR = os.path.dirname(os.path.abspath(__file__))

QC_LOGS_DIR = os.path.join(THIS_DIR,'..','..','..','..','thermo','CoMoCompChemParser','tests','gaussian')


#@pytest.mark.parametrize("testFilesDir, testFileName, regenerateResults",
#[
#(os.path.join(QC_LOGS_DIR,'dft'),'c2h4_opt_dft_g09.log', False),
#]
#)
#def test_compchemwriter_from_qclogs(testFilesDir, testFileName):
#    print('========================================================')
#    print('TEST DIR: ', testFilesDir)
#    print('TEST LOG: ', testFileName)
#
#    print('========================================================')
