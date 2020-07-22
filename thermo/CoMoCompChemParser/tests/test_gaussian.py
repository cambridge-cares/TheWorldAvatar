import unittest
from compchemparser.parsers.ccgaussian_parser import CcGaussianParser
import json

parser = CcGaussianParser()

class TestGaussianParser(unittest.TestCase):

    def test_g1(self):
        test_path = './tests/gaussian/g1/'
        test_name = 'Gaussian G1 method'
        filename = 'co2_g1_g09.log'
        json_ref_name = filename.replace('.log','.json')

        print("Test " + test_name+", File: "+filename)
        json_data = parser.parse(test_path+filename)[0]

        #with open(test_path+json_ref_name, 'w') as outfile:
        #    json.dump(json_data, outfile)
        
        with open(test_path+json_ref_name) as ref_file:
            ref_data = json.load(ref_file)
        self.assertEqual(json_data, ref_data)

    def test_cas(self):
        test_path = './tests/gaussian/cas/'
        test_name = 'Gaussian CASSCF method'
        filename = 'co2_cas_g09.log'
        json_ref_name = filename.replace('.log','.json')

        print("Test " + test_name+", File: "+filename)
        json_data = parser.parse(test_path+filename)[0]

        #with open(test_path+json_ref_name, 'w') as outfile:
        #    json.dump(json_data, outfile)
        
        with open(test_path+json_ref_name) as ref_file:
            ref_data = json.load(ref_file)
        self.assertEqual(json_data, ref_data)

if __name__ == '__main__':
    unittest.main()