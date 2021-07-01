from compchemparser.parsers.ccgaussian_parser import CcGaussianParser
import json
import os
import sys
from compchemparser.aboxwriters import write_abox as write_compchem_abox
from compchemparser.helpers.utils import readFile
import pytest

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
parser = CcGaussianParser()

gaussianTestsLog = os.path.join(THIS_DIR, 'gaussianTests.log')

def getRefName(testLog,jobIndex,numJobs):
    if numJobs > 1:
        refName = testLog + '_' + str(jobIndex+1)+'.json'
    else:
        refName = testLog + '.json'
    return refName

@pytest.mark.parametrize("testType, testLog",
[
('g1','co2_g1_g09.log'),
('g2','co2_g2_g09.log'),
('g2','co2_g2mp2_g09.log'),
('cbs-4m','co2_cbs4m_g09.log'),
('cbs-apno','co2_cbsapno_g09.log'),
#linked
('linked','co2_linked_g09.log'),
#cas
('cas','co2_cas_g09.log'),
('cas','co2_casmp2.log'),
#dft
('dft','co2_freq_g09.log'),
('dft','co2_sp_dft_g09.log'),
('dft','co2_opt_freq_dft_p_g09.log'),
('dft','co2_opt_freq_dft_t_g09.log'),
('dft','h_sp_n_g09.log'),
#ccsd/ci
('ccsd','co2_ccsd_g09.log'),
('ccsd','co2_qci_g09.log'),
('ci','co2_cis.log'),
#mpn
('mpn','oh_sp_mp4_g03.log'),
#extra
('extra','Cl.g09'),
('extra','Ti.g09'),
('extra','TiCl4.g09'),
('extra','O_3let.g09'),
('extra','71-41-0.g09'),
('extra','71-43-2.g09'),
('extra','A2R5H.log'),
('extra','1b.log'),
#failed
('failed_jobs','c2h4_opt_dft_g09_failed.log'),
('failed_jobs','c2h4_opt_dft_g09_powercut.log'),
('failed_jobs','co2_linked_g09.log'),
#scans
#('scans','ethane_scan_short.log')
]
)
def test_parser(testType, testLog, regenerateResults=False):
    print('========================================================')
    print('TEST TYPE: ', testType)
    print('TEST LOG: ', testLog)
    print()
    print()
    testPath = os.path.join(THIS_DIR, 'gaussian', testType)
    testLogFilePath = os.path.join(testPath, testLog)
    parsedJobsList = parser.parse(testLogFilePath)

    for jobIndex, jobDataJson in enumerate(parsedJobsList):
        #This converts test_data into a dictionary from JSON
        #For easy comparison and readability
        jobDataDict = json.loads(jobDataJson)

        refDataFileName = getRefName(testLog,jobIndex=jobIndex,numJobs=len(parsedJobsList))
        refDataFilePath = os.path.join(testPath, refDataFileName)

        if regenerateResults:
            # dump just parsed data as the ref data
            with open(refDataFilePath, 'w') as outFile:
                    json.dump(jobDataDict, outFile, indent = 4)

        # read the red data
        with open(refDataFilePath) as refFile:
            refDataDict = json.load(refFile)

        assert len(jobDataDict.keys()) == len(refDataDict.keys())
        for key in jobDataDict.keys():
            assert key in refDataDict.keys()
            assert jobDataDict[key] == refDataDict[key]

        print('========================================================')
        print()
        print()

#class TestGaussianParser(unittest.TestCase):
#    @parameterized.expand([
#        #composite
#        # type, log, regenerate results flag
#        # e.g:
#        #['g1','co2_g1_g09.log', True],  <- True here sets the regenerateResults flag to True
#        # no need to set it to False as that is the default
#        ['g1','co2_g1_g09.log'],
#        ['g2','co2_g2_g09.log'],
#        ['g2','co2_g2mp2_g09.log'],
#        ['cbs-4m','co2_cbs4m_g09.log'],
#        ['cbs-apno','co2_cbsapno_g09.log'],
#        #linked
#        ['linked','co2_linked_g09.log'],
#        #cas
#        ['cas','co2_cas_g09.log'],
#        ['cas','co2_casmp2.log'],
#        #dft
#        ['dft','co2_freq_g09.log'],
#        ['dft','co2_sp_dft_g09.log'],
#        ['dft','co2_opt_freq_dft_p_g09.log'],
#        ['dft','co2_opt_freq_dft_t_g09.log'],
#        ['dft','h_sp_n_g09.log'],
#        #ccsd/ci
#        ['ccsd','co2_ccsd_g09.log'],
#        ['ccsd','co2_qci_g09.log'],
#        ['ci','co2_cis.log'],
#        #mpn
#        ['mpn','oh_sp_mp4_g03.log'],
#        #extra
#        ['extra','Cl.g09'],
#        ['extra','Ti.g09'],
#        ['extra','TiCl4.g09'],
#        ['extra','O_3let.g09'],
#        ['extra','71-41-0.g09'],
#        ['extra','71-43-2.g09'],
#        ['extra','A2R5H.log'],
#        ['extra','1b.log'],
#        #failed
#        ['failed_jobs','c2h4_opt_dft_g09_failed.log'],
#        ['failed_jobs','c2h4_opt_dft_g09_powercut.log'],
#        ['failed_jobs','co2_linked_g09.log'],
#        #scans
#        #['scans','ethane_scan_short.log']
#    ])#

#    @parameterized.expand([
#        #composite
#        # type, log, regenerate results flag
#        # e.g:
#        #['g1','co2_g1_g09.log', True],  <- True here sets the regenerateResults flag to True
#        # no need to set it to False as that is the default
#        #dft
#        ['dft','co2_freq_g09.log.json'],#

#    ])
#    def test_ontocompchem_abox_writer(self, testType, testJSON, regenerateResults=False):
#        print('========================================================')
#        print('TEST TYPE: ', testType)
#        print('TEST JSON: ', testJSON)
#        print()
#        print()
#        testPath = os.path.join(THIS_DIR, 'gaussian', testType)
#        testJSONFilePath = os.path.join(testPath, testJSON)
#        init_num = '1111.2222'
#        calc_id = 'aabb-2021-test'
#        if regenerateResults:
#             write_compchem_abox(testJSONFilePath,init_num=init_num,calc_id=calc_id)#

#        test_abox_csv = write_compchem_abox(testJSONFilePath,os.path.join(testPath,'test_csv.csv'),init_num,calc_id)
#        ref_abox_csv = os.path.join(testPath,'ABox_' + testJSON.replace('.json','') + '.csv')#

#        test_abox_data = readFile(test_abox_csv)
#        ref_abox_data = readFile(ref_abox_csv)
#        self.assertEqual(test_abox_data, ref_abox_data)#

#        print('========================================================')
#        print()
#        print()#
#
#

#def main(out = sys.stderr, verbosity = 2):
#    loader = unittest.TestLoader()#

#    suite = loader.loadTestsFromModule(sys.modules[__name__])
#    unittest.TextTestRunner(out, verbosity = verbosity).run(suite)#

#if __name__ == '__main__':
#    with open(gaussianTestsLog, 'w') as f:
#        main(f)
