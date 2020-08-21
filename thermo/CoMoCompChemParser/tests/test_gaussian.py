import unittest
from compchemparser.parsers.ccgaussian_parser import CcGaussianParser
import json
import os
from tkinter.tix import DialogShell

#Added by Nenad Krdzavac
from compchemparser.ontocompchemdata.ontocompchemdata import OntoCompChemData
import random
from rdflib import Graph
from pathlib import Path


THIS_DIR = os.path.dirname(os.path.abspath(__file__))
parser = CcGaussianParser()

# create OntoCompChemData object
CompChemObj = OntoCompChemData()

Test_suite1 = {'g1': ['co2_g1_g09.log'],
               'g2': ['co2_g2_g09.log', 'co2_g2mp2_g09.log'],
               'linked': ['co2_linked_g09.log'],
               'cas': ['co2_cas_g09.log', 'co2_casmp2.log'],
               'dft': ['TEOS_ub971_6311+gdp.log','co2_freq_g09.log','co2_sp_dft_g09.log','co2_opt_freq_dft_p_g09.log','co2_opt_freq_dft_t_g09.log','h_sp_n_g09.log'],
               'ccsd': ['co2_ccsd_g09.log','co2_qci_g09.log'],
               'ci': ['co2_cis.log'],
               'mpn': ['oh_sp_mp4_g03.log'],
               "extra":['Cl.g09', 'Ti.g09','TiCl4.g09','O_3let.g09','71-41-0.g09','71-43-2.g09','A2R5H.log'],
               "failed_jobs":['c2h4_opt_dft_g09_failed.log', 'c2h4_opt_dft_g09_powercut.log','co2_linked_g09.log']
}

class TestGaussianParser(unittest.TestCase):

    def test_suite1(self):

        test_path = os.path.join(THIS_DIR, 'gaussian')
        
        print("test_path: ", test_path)

        for method, logs in Test_suite1.items():
            for log_file in logs:
                test_name = 'Gaussian '+ method
                file_path = os.path.join(test_path, method, log_file)

                print("Test " + test_name+", File: "+ log_file)

                test_data_list = parser.parse(file_path)

                for i, test_data in enumerate(test_data_list):
                    #This converts test_data into a dictionary from JSON
                    #For easy comparison and readability
                    test_data = json.loads(test_data)

                    if len(test_data_list) > 1:
                    #   ref_name = log_file.replace('.log','#'+str(i+1)+'.json')
                         ref_name = log_file + '#' + str(i+1)+'.json'
                         #print("ref_name (json): ", ref_name)

                    else:
                        #ref_name = log_file.replace('.log','.json')
                        ref_name = log_file + '.json'
                        #print("ref_name (json): ", ref_name)


                        
                    ref_path = os.path.join(test_path, method, ref_name)

                    # uncomment to generate ref json file
                    #---------------------------------------------------
                    #with open(ref_path, 'w') as outfile:
                    #     json.dump(test_data, outfile, indent = 4)
                    #---------------------------------------------------

                    print("ref_path: ", ref_path)

                    with open(ref_path) as ref_file:
                        ref_data = json.load(ref_file)

                    self.assertEqual(len(test_data.keys()), len(ref_data.keys()))
                    for key in test_data.keys():
                        self.assertEqual(key in ref_data.keys(), True)
                        self.assertEqual(test_data[key], ref_data[key])



if __name__ == '__main__':
    unittest.main()