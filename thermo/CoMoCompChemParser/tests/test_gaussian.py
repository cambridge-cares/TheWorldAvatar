import unittest
from compchemparser.parsers.ccgaussian_parser import CcGaussianParser
import json
import os

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
parser = CcGaussianParser()

Test_suite1 = {'g1': ['co2_g1_g09.log'],
               'g2': ['co2_g2_g09.log', 'co2_g2mp2_g09.log'],
               'linked': ['co2_linked_g09.log'],
               'cas': ['co2_cas_g09.log', 'co2_casmp2.log'],
               'dft': ['TEOS_ub971_6311+gdp.log'],
}



class TestGaussianParser(unittest.TestCase):

    def test_suite1(self):
        test_path = os.path.join(THIS_DIR, 'gaussian')

        for method, logs in Test_suite1.items():
            for log_file in logs:
                test_name = 'Gaussian '+method
                file_path = os.path.join(test_path, method, log_file)
                print("Test " + test_name+", File: "+log_file)

                test_data_list = parser.parse(file_path)
                for i, test_data in enumerate(test_data_list):
                    test_data = json.loads(test_data)

                    if len(test_data_list) > 1:
                        ref_name = log_file.replace('.log','#'+str(i+1)+'.json')
                    else:
                        ref_name = log_file.replace('.log','.json')
                    ref_path = os.path.join(test_path, method, ref_name)
                    # uncomment to generate ref json file
                    #---------------------------------------------------
                    #with open(ref_path, 'w') as outfile:
                    #    json.dump(test_data, outfile, indent = 4)
                    #---------------------------------------------------
                    
                    with open(ref_path) as ref_file:
                        ref_data = json.load(ref_file)
                    
                    self.assertEqual(len(test_data.keys()), len(ref_data.keys()))
                    for key in test_data.keys():
                        self.assertEqual(key in ref_data.keys(), True)
                        self.assertEqual(test_data[key], ref_data[key])

if __name__ == '__main__':
    unittest.main()