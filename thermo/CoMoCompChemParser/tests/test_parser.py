import json
import os
from compchemparser.parsers.ccgaussian_parser import CcGaussianParser
from compchemparser.app import runParser
from compchemparser.helpers.utils import getRefName, readJsonToDict, writeDictToJson
import pytest

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
parser = CcGaussianParser()

@pytest.mark.parametrize("testType, testLog, outputSize, regenerateResults",
[
('g1','co2_g1_g09.log', 1, False),
('g2','co2_g2_g09.log', 1, False),
('g2','co2_g2mp2_g09.log', 1, False),
('cbs-4m','co2_cbs4m_g09.log', 1, False),
('cbs-apno','co2_cbsapno_g09.log', 1, False),
#linked
('linked','co2_linked_g09.log', 2, False),
#cas
('cas','co2_cas_g09.log', 1, False),
('cas','co2_casmp2.log', 1, False),
#dft
('dft','co2_freq_g09.log', 1, False),
('dft','co2_sp_dft_g09.log', 1, False),
('dft','co2_opt_freq_dft_p_g09.log', 1, False),
('dft','co2_opt_freq_dft_t_g09.log', 1, False),
('dft','h_sp_n_g09.log', 1, False),
#ccsd/ci
('ccsd','co2_ccsd_g09.log', 1, False),
('ccsd','co2_qci_g09.log', 1, False),
('ci','co2_cis.log', 1, False),
#mpn
('mpn','oh_sp_mp4_g03.log', 1, False),
#extra
('extra','Cl.g09', 1, False),
('extra','Ti.g09', 1, False),
('extra','TiCl4.g09', 1, False),
('extra','O_3let.g09', 1, False),
('extra','71-41-0.g09', 1, False),
('extra','71-43-2.g09', 1, False),
('extra','A2R5H.log', 1, False),
('extra','1b.log', 1, False),
#failed
('failed_jobs','c2h4_opt_dft_g09_failed.log', 0, False),
('failed_jobs','c2h4_opt_dft_g09_powercut.log', 0, False),
('failed_jobs','co2_linked_g09.log', 1, False),
#scans
#  singlelog, relaxed
('scans\\singlelogscans\\relaxed','ethane_scan_short.g09', 3, False),
('scans\\singlelogscans\\relaxed','ethane_scan_cas.log', 3, False),
('scans\\singlelogscans\\relaxed','ethane_scan_mp2.log', 3, False),
('scans\\singlelogscans\\relaxed','nbut.log', 7, False),
#  singlelog, rigid
('scans\\singlelogscans\\rigid','ethane_angle_rigid.log', 4, False),
('scans\\singlelogscans\\rigid','ethane_scan_rigid.g09', 4, False),
('scans\\singlelogscans\\rigid','nbut_rigid.log', 7, False),
#  multilog, relaxed
('scans\\multilogscans\\relaxed','ethane_1.log', 1, False),
('scans\\multilogscans\\relaxed','ethane_2.log', 1, False),
('scans\\multilogscans\\relaxed','ethane_3.log', 1, False),
#  multilog, rigid
('scans\\multilogscans\\rigid','ethane_1_rigid.log', 1, False),
('scans\\multilogscans\\rigid','ethane_2_rigid.log', 1, False),
('scans\\multilogscans\\rigid','ethane_3_rigid.log', 1, False)
]
)
def test_parser(testType, testLog, outputSize, regenerateResults, regenerateAllResults=False):
    print('========================================================')
    print('TEST TYPE: ', testType)
    print('TEST LOG: ', testLog)
    print()
    print()
    testPath = os.path.join(THIS_DIR, 'gaussian', testType)
    testLogFilePath = os.path.join(testPath, testLog)
    parsedJobsList = parser.parse(testLogFilePath)

    assert outputSize == len(parsedJobsList)
    if outputSize>0:
        for jobIndex, jobDataJson in enumerate(parsedJobsList):
            #This converts test_data into a dictionary from JSON
            #For easy comparison and readability
            jobDataDict = json.loads(jobDataJson)

            jobDataFileName = getRefName(testLog,jobIndex=jobIndex,numJobs=len(parsedJobsList),extension='.json')
            jobDataFilePath = os.path.join(testPath, jobDataFileName)
            refDataFilePath=jobDataFilePath+'_ref'

            #if os.path.isfile(jobDataFilePath): os.remove(jobDataFilePath)
            #writeDictToJson(jobDataFilePath, jobDataDict)

            if regenerateResults or regenerateAllResults:
                # dump just parsed data as the ref data
                writeDictToJson(refDataFilePath, jobDataDict)

            # read the red data
            refDataDict = readJsonToDict(refDataFilePath)

            assert len(jobDataDict.keys()) == len(refDataDict.keys())
            for key in jobDataDict.keys():
                assert key in refDataDict.keys()
                assert jobDataDict[key] == refDataDict[key]

        print('========================================================')
        print()
        print()


@pytest.mark.parametrize("logFileOrDir, logExt, outputSize",
[
('scans\\singlelogscans\\rigid', '.g09', 4),
('scans\\singlelogscans\\relaxed', '.log, .g09', 17),
('scans\\multilogscans\\relaxed', '.log', 3),
('scans\\multilogscans\\rigid', '.log', 3)
]
)
def test_parser_app(logFileOrDir, logExt, outputSize):
    print('========================================================')
    print('TEST FILE/DIR: ', logFileOrDir)
    print()
    print()

    testPath = os.path.join(THIS_DIR, 'gaussian', logFileOrDir)
    output= runParser(testPath, logExt, suppressOutput=True)

    assert outputSize == len(output)