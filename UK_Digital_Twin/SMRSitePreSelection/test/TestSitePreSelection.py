##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 25 August 2022       #
##########################################

import os, sys
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
import SitePreSelection as sp
import unittest

class TestSitePreSelection(unittest.TestCase):

    ## initialise the site selection objec

    def setUp(self):
        print("This is the unittest for SitePreSelection class.")
        pass

    ##-- pure replacement, non-clustered scenario --##
    ## Scenario 1: same location of different types of generators with the same capacity
    ####genDiffType_s1: same operation hours, same decommissioning factors and different emission factors
    def test_pureReplacement_CO2EmissionFactor(self):
        print("This is the unittest for test_pureReplacement_CO2EmissionFactor")
        genDiffType_s1 =  [{'Capacity': '470', 'LatLon': [51.38731, -3.4049], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal', 'annualOperatingHours': 7000, 'CO2EmissionFactor': 0.319}, 
                {'Capacity': '470', 'LatLon': [51.38731, -3.4049], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Oil', 'annualOperatingHours': 7000, 'CO2EmissionFactor': 0.281},
                {'Capacity': '470', 'LatLon': [51.38731, -3.4049], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGas', 'annualOperatingHours': 7000, 'CO2EmissionFactor': 0.181},
                {'Capacity': '470', 'LatLon': [51.38731, -3.4049], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'annualOperatingHours': 7000, 'CO2EmissionFactor': 0.0},
                {'Capacity': '470', 'LatLon': [51.38731, -3.4049], 'fuelOrGenType': 'http://www.theworldavatar.com/kb/ontoeip/WindOnshore', 'annualOperatingHours': 7000, 'CO2EmissionFactor': 0.0}]

        replacementResult = [['y_0_0', 'y_1_0', 'y_2_0', 'y_3_0', 'y_4_0'], ['y_0_1', 'y_1_0', 'y_2_0', 'y_3_0', 'y_4_0'], ['y_0_1', 'y_1_1', 'y_2_0', 'y_3_0', 'y_4_0'], ['y_0_1', 'y_1_1', 'y_2_1', 'y_3_0', 'y_4_0'], ['y_0_1', 'y_1_1', 'y_2_1', 'y_3_0', 'y_4_0']]
        carbonTax = [0, 60, 80, 100, 200]
        for i in range(len(replacementResult)):
            self.sps = sp.SitePreSelection('UKPopulationData', genDiffType_s1, 0.02, 40, 1800000000, 2400000, 200, 0.002985, 470, 0.7, 0.0125, carbonTax[i], False, True, False, 4, 0)
            self.assertEqual(self.sps.SMRSitePreSelector(), replacementResult[i])
    
    ####genDiffType_s2: same decommissioning factors and same emission factors and different operating hours
    def test_pureReplacement_OperatingHour(self):
        print("This is the unittest for test_pureReplacement_OperatingHour")
        genDiffType_s2 =  [{'Capacity': '470', 'LatLon': [51.38731, -3.4049], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal', 'annualOperatingHours': 7000, 'CO2EmissionFactor': 0.319}, 
                {'Capacity': '470', 'LatLon': [51.38731, -3.4049], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Oil', 'annualOperatingHours': 6000, 'CO2EmissionFactor': 0.319},
                {'Capacity': '470', 'LatLon': [51.38731, -3.4049], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGas', 'annualOperatingHours': 5000, 'CO2EmissionFactor': 0.319}]

        replacementResult = [['y_0_0', 'y_1_0', 'y_2_0'], ['y_0_1', 'y_1_0', 'y_2_0'], ['y_0_1', 'y_1_1', 'y_2_0'], ['y_0_1', 'y_1_1', 'y_2_1']]
        carbonTax = [0, 60, 70, 80]
        for i in range(len(replacementResult)):
            self.sps = sp.SitePreSelection('UKPopulationData', genDiffType_s2, 0.02, 40, 1800000000, 2400000, 200, 0.002985, 470, 0.7, 0.0125, carbonTax[i], False, True, False, 4, 0)
            self.assertEqual(self.sps.SMRSitePreSelector(), replacementResult[i])
    
    ####genDiffType_s3: same emission factors, same operating hours and different decommissioning factors
    # Coal > Oil > NaturalGas 
    def test_pureReplacement_DecommissioningCost(self):
        print("This is the unittest for test_pureReplacement_DecommissioningCost")
        genDiffType_s3 =  [{'Capacity': '470', 'LatLon': [51.38731, -3.4049], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal', 'annualOperatingHours': 7000, 'CO2EmissionFactor': 0.319}, 
                {'Capacity': '470', 'LatLon': [51.38731, -3.4049], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Oil', 'annualOperatingHours': 7000, 'CO2EmissionFactor': 0.319},
                {'Capacity': '470', 'LatLon': [51.38731, -3.4049], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGas', 'annualOperatingHours': 7000, 'CO2EmissionFactor': 0.319}]

        replacementResult = [['y_0_0', 'y_1_0', 'y_2_0'], ['y_0_0', 'y_1_0', 'y_2_1'], ['y_0_0', 'y_1_1', 'y_2_1'], ['y_0_1', 'y_1_1', 'y_2_1']]
        carbonTax = [0, 54.09, 54.2, 55]
        for i in range(len(replacementResult)):
            self.sps = sp.SitePreSelection('UKPopulationData', genDiffType_s3, 0.02, 40, 1800000000, 2400000, 200, 0.002985, 470, 0.7, 0.0125, carbonTax[i], False, True, False, 4, 1)
            self.assertEqual(self.sps.SMRSitePreSelector(), replacementResult[i])

    ## Scenario 2: same types of generators located at the same location of different capacity
    ####genDiffType_s4: same operation hours, same emission factors and same decommissioning factors
    def test_pureReplacement_Capacity(self):
        print("This is the unittest for test_pureReplacement_Capacity")
        genDiffType_s4 =  [{'Capacity': '400', 'LatLon': [51.38731, -3.4049], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal', 'annualOperatingHours': 7000, 'CO2EmissionFactor': 0.319}, 
                {'Capacity': '600', 'LatLon': [51.38731, -3.4049], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal', 'annualOperatingHours': 7000, 'CO2EmissionFactor': 0.319},
                {'Capacity': '1400', 'LatLon': [51.38731, -3.4049], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal', 'annualOperatingHours': 7000, 'CO2EmissionFactor': 0.319}]

        replacementResult = [['y_0_0', 'y_1_0', 'y_2_0'], ['y_0_0', 'y_1_0', 'y_2_1'], ['y_0_0', 'y_1_1', 'y_2_1'], ['y_0_1', 'y_1_1', 'y_2_1']]
        carbonTax = [0, 54.09, 54.2, 55]
        # for i in range(len(replacementResult)):
        #     self.sps = sp.SitePreSelection('UKPopulationData', genDiffType_s4, 0.02, 40, 1800000000, 2400000, 200, 0.002985, 470, 0.7, 0.0125, carbonTax[i], False, True, False, 4, 0)
        #     self.assertEqual(self.sps.SMRSitePreSelector(), replacementResult[i])

        self.sps = sp.SitePreSelection('UKPopulationData', genDiffType_s4, 0.02, 40, 1800000000, 2400000, 200, 0.002985, 470, 0.7, 0.0125, 42, False, True, False, 4, 0)
        self.assertEqual(self.sps.SMRSitePreSelector(), replacementResult[0])

    ## Scenario 3: same types of generators located at the different locations of different capacity
    ####genDiffType_s5: same operation hours, same emission factors and same decommissioning factors
    def test_pureReplacement_Location(self):
        print("This is the unittest for test_pureReplacement_Location")
        genDiffType_s4 =  [{'Capacity': '470', 'LatLon': [51.38731, -3.4049], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal', 'annualOperatingHours': 7000, 'CO2EmissionFactor': 0.319}, 
                {'Capacity': '470', 'LatLon': [52.064288, -1.341546], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal', 'annualOperatingHours': 7000, 'CO2EmissionFactor': 0.319},
                {'Capacity': '470', 'LatLon': [52.205276, 0.11916], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal', 'annualOperatingHours': 7000, 'CO2EmissionFactor': 0.319}]

        replacementResult = [['y_0_0', 'y_1_0', 'y_2_0'], ['y_0_1', 'y_1_0', 'y_2_0'], ['y_0_1', 'y_1_1', 'y_2_0'], ['y_0_1', 'y_1_1', 'y_2_1']]
        carbonTax = [0, 60, 70, 90]
        for i in range(len(replacementResult)):
            self.sps = sp.SitePreSelection('UKPopulationData', genDiffType_s4, 0.02, 40, 1800000000, 2400000, 200, 0.002985, 470, 0.7, 0.0125, carbonTax[i], False, True, False, 4, 0)
            self.assertEqual(self.sps.SMRSitePreSelector(), replacementResult[i])

    
    def tearDown(self):
        print("This is the end of the test of site selection class.")


if __name__ == '__main__':
    suite = unittest.TestSuite()
    suite.addTest(TestSitePreSelection("test_pureReplacement_CO2EmissionFactor"))
    suite.addTest(TestSitePreSelection("test_pureReplacement_OperatingHour"))
    suite.addTest(TestSitePreSelection("test_pureReplacement_DecommissioningCost"))
    suite.addTest(TestSitePreSelection("test_pureReplacement_Location"))

    ## suite.addTest(TestSitePreSelection("test_pureReplacement_Capacity"))

    runner = unittest.TextTestRunner()
    runner.run(suite)


