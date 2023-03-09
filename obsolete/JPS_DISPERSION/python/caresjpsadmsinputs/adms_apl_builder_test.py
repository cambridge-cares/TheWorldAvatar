import unittest

from adms_apl_builder import *
from adms_apl_test import AdmsAplTestHelper as helper
from config import Constants


class AplDirectorTest(unittest.TestCase):

    def test_init(self):
        ad = AplDirector()
        self.assertIsNone(ad._AplDirector__builder)

    def test_set_builder(self):
        ad = AplDirector()
        ab = AplBuilder({})
        ad.set_builder(ab)
        self.assertIsInstance(ad._AplDirector__builder, AplBuilder)

    def test_get_apl(self):
        ad = AplDirector()
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        ad.set_builder(ab)
        apl = ad.get_apl()
        # Values for pollutants tested in builder tests. Repeating tests is not necessary here.
        apl.set_pollutants([])
        self.assertEqual(apl.specification(), helper.get_default_apl_builder_specification(helper))
        asb = AdmsAplShipBuilder(helper.get_default_apl_builder_data(helper))
        ad.set_builder(asb)
        apl = ad.get_apl()
        # Values for pollutants tested in builder tests. Repeating tests is not necessary here.
        apl.set_pollutants([])
        self.assertEqual(apl.specification(), helper.get_default_apl_ship_builder_specification(helper))
        apb = AdmsAplPlantBuilder(helper.get_default_apl_builder_data(helper))
        ad.set_builder(apb)
        apl = ad.get_apl()
        # Values for pollutants tested in builder tests. Repeating tests is not necessary here.
        apl.set_pollutants([])
        self.maxDiff = None
        self.assertEqual(apl.specification(), helper.get_default_apl_plant_builder_specification(helper))


class AplBuilderTest(unittest.TestCase):

    def test_init(self):
        ab = AplBuilder({})
        self.assertEqual(ab.data, {})
        self.assertEqual(ab.pollutant_names, helper.get_default_apl_pollutant_names())

    def test_get_header(self):
        self.assertEqual(AplBuilder.get_header().to_string(), AdmsHeader().to_string())

    def test_get_sup(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_sup().to_string(), AdmsSup.to_string(AdmsSup()))

    def test_get_met(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_met().to_string(), AdmsMet().to_string())

    def test_get_bld(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_bld()._name, "&ADMS_PARAMETERS_BLD")
        self.assertEqual(ab.get_bld().BldName, helper.get_default_apl_bld_data().BldName)
        self.assertEqual(ab.get_bld().BldNumBuildings, helper.get_default_apl_bld_data().BldNumBuildings)
        self.assertEqual(ab.get_bld().BldType, helper.get_default_apl_bld_data().BldType)
        self.assertEqual(ab.get_bld().BldX, helper.get_default_apl_bld_data().BldX)
        self.assertEqual(ab.get_bld().BldY, helper.get_default_apl_bld_data().BldY)
        self.assertEqual(ab.get_bld().BldHeight, helper.get_default_apl_bld_data().BldHeight)
        self.assertEqual(ab.get_bld().BldLength, helper.get_default_apl_bld_data().BldLength)
        self.assertEqual(ab.get_bld().BldWidth, helper.get_default_apl_bld_data().BldWidth)
        self.assertEqual(ab.get_bld().BldAngle, helper.get_default_apl_bld_data().BldAngle)

    def test_get_hil(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_hil().to_string(), AdmsHil().to_string())

    def test_get_cst(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_cst().to_string(), AdmsCst().to_string())

    def test_get_flc(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_flc().to_string(), AdmsFlc().to_string())

    def test_get_grd(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_grd()._name, "&ADMS_PARAMETERS_GRD")
        self.assertEqual(ab.get_grd().GrdRegularMin[0], 0)
        self.assertEqual(ab.get_grd().GrdRegularMin[1], 1)
        self.assertEqual(ab.get_grd().GrdRegularMax[0], 2)
        self.assertEqual(ab.get_grd().GrdRegularMax[1], 3)
        self.assertEqual(ab.get_grd().GrdRegularNumPoints[0], 4)
        self.assertEqual(ab.get_grd().GrdRegularNumPoints[1], 5)

    def test_get_puf(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_puf().to_string(), AdmsPuf().to_string())

    def test_get_gam(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_gam().to_string(), AdmsGam().to_string())

    def test_get_opt(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_opt().to_string(), AdmsOpt().to_string())

    def test_get_bkg(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_bkg().to_string(), AdmsBkg().to_string())

    def test_get_chm(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_chm().to_string(), AdmsChm().to_string())

    def test_get_etc(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_etc().to_string(), AdmsEtc().to_string())

    def test_get_coordsys(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_coordsys().to_string(), AdmsCoordSys().to_string())

    def test_get_mapper(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_mapper().to_string(), AdmsMapper().to_string())

    def test_get_pollutants(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertIsNotNone(ab.get_pollutants())
        self.assertEqual(len(ab.get_pollutants()), 18)
        self.assertEqual(ab.get_pollutants()[0].PolName, Constants.POL_CO2)
        self.assertEqual(ab.get_pollutants()[1].PolName, Constants.POL_NOX)
        self.assertEqual(ab.get_pollutants()[2].PolName, Constants.POL_NO2)
        self.assertEqual(ab.get_pollutants()[3].PolName, Constants.POL_NO)
        self.assertEqual(ab.get_pollutants()[4].PolName, Constants.POL_PART_O3)
        self.assertEqual(ab.get_pollutants()[5].PolName, Constants.POL_VOC)
        self.assertEqual(ab.get_pollutants()[6].PolName, Constants.POL_PART_SO2)
        self.assertEqual(ab.get_pollutants()[7].PolName, Constants.POL_PM10)
        self.assertEqual(ab.get_pollutants()[8].PolName, Constants.POL_PM25)
        self.assertEqual(ab.get_pollutants()[9].PolName, Constants.POL_CO)
        self.assertEqual(ab.get_pollutants()[10].PolName, Constants.POL_BENZENE)
        self.assertEqual(ab.get_pollutants()[11].PolName, Constants.POL_BUTADIENE)
        self.assertEqual(ab.get_pollutants()[12].PolName, Constants.POL_HCl)
        self.assertEqual(ab.get_pollutants()[13].PolName, Constants.POL_Cl2)
        self.assertEqual(ab.get_pollutants()[14].PolName, Constants.POL_CH3Cl)
        self.assertEqual(ab.get_pollutants()[15].PolName, Constants.POL_ISOBUTYLENE)
        self.assertEqual(ab.get_pollutants()[16].PolName, Constants.POL_NH3)
        self.assertEqual(ab.get_pollutants()[17].PolName, Constants.POL_HC)
        self.assertEqual(ab.get_pollutants()[0].PolPollutantType, 0)
        self.assertEqual(ab.get_pollutants()[1].PolPollutantType, 0)
        self.assertEqual(ab.get_pollutants()[2].PolPollutantType, 0)
        self.assertEqual(ab.get_pollutants()[3].PolPollutantType, 0)
        self.assertEqual(ab.get_pollutants()[4].PolPollutantType, 0)
        self.assertEqual(ab.get_pollutants()[5].PolPollutantType, 0)
        self.assertEqual(ab.get_pollutants()[6].PolPollutantType, 0)
        self.assertEqual(ab.get_pollutants()[7].PolPollutantType, 1)
        self.assertEqual(ab.get_pollutants()[8].PolPollutantType, 1)
        self.assertEqual(ab.get_pollutants()[9].PolPollutantType, 0)
        self.assertEqual(ab.get_pollutants()[10].PolPollutantType, 0)
        self.assertEqual(ab.get_pollutants()[11].PolPollutantType, 0)
        self.assertEqual(ab.get_pollutants()[12].PolPollutantType, 0)
        self.assertEqual(ab.get_pollutants()[13].PolPollutantType, 0)
        self.assertEqual(ab.get_pollutants()[14].PolPollutantType, 0)
        self.assertEqual(ab.get_pollutants()[15].PolPollutantType, 0)
        self.assertEqual(ab.get_pollutants()[16].PolPollutantType, 0)
        self.assertEqual(ab.get_pollutants()[17].PolPollutantType, 0)
        self.assertEqual(ab.get_pollutants()[0].PolGasDepVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[1].PolGasDepVelocityKnown, 0)
        self.assertEqual(ab.get_pollutants()[2].PolGasDepVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[3].PolGasDepVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[4].PolGasDepVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[5].PolGasDepVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[6].PolGasDepVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[7].PolGasDepVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[8].PolGasDepVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[9].PolGasDepVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[10].PolGasDepVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[11].PolGasDepVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[12].PolGasDepVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[13].PolGasDepVelocityKnown, 0)
        self.assertEqual(ab.get_pollutants()[14].PolGasDepVelocityKnown, 0)
        self.assertEqual(ab.get_pollutants()[15].PolGasDepVelocityKnown, 0)
        self.assertEqual(ab.get_pollutants()[16].PolGasDepVelocityKnown, 0)
        self.assertEqual(ab.get_pollutants()[17].PolGasDepVelocityKnown, 0)
        self.assertEqual(ab.get_pollutants()[0].PolGasDepositionVelocity, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[1].PolGasDepositionVelocity, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[2].PolGasDepositionVelocity, 1.5e-3)
        self.assertEqual(ab.get_pollutants()[3].PolGasDepositionVelocity, 1.5e-3)
        self.assertEqual(ab.get_pollutants()[4].PolGasDepositionVelocity, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[5].PolGasDepositionVelocity, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[6].PolGasDepositionVelocity, 1.2e-2)
        self.assertEqual(ab.get_pollutants()[7].PolGasDepositionVelocity, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[8].PolGasDepositionVelocity, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[9].PolGasDepositionVelocity, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[10].PolGasDepositionVelocity, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[11].PolGasDepositionVelocity, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[12].PolGasDepositionVelocity, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[13].PolGasDepositionVelocity, 5.0e+0)
        self.assertEqual(ab.get_pollutants()[14].PolGasDepositionVelocity, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[15].PolGasDepositionVelocity, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[16].PolGasDepositionVelocity, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[17].PolGasDepositionVelocity, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[0].PolGasType, 1)
        self.assertEqual(ab.get_pollutants()[1].PolGasType, 1)
        self.assertEqual(ab.get_pollutants()[2].PolGasType, 1)
        self.assertEqual(ab.get_pollutants()[3].PolGasType, 1)
        self.assertEqual(ab.get_pollutants()[4].PolGasType, 1)
        self.assertEqual(ab.get_pollutants()[5].PolGasType, 1)
        self.assertEqual(ab.get_pollutants()[6].PolGasType, 1)
        self.assertEqual(ab.get_pollutants()[7].PolGasType, 1)
        self.assertEqual(ab.get_pollutants()[8].PolGasType, 1)
        self.assertEqual(ab.get_pollutants()[9].PolGasType, 1)
        self.assertEqual(ab.get_pollutants()[10].PolGasType, 1)
        self.assertEqual(ab.get_pollutants()[11].PolGasType, 1)
        self.assertEqual(ab.get_pollutants()[12].PolGasType, 0)
        self.assertEqual(ab.get_pollutants()[13].PolGasType, 0)
        self.assertEqual(ab.get_pollutants()[14].PolGasType, 0)
        self.assertEqual(ab.get_pollutants()[15].PolGasType, 0)
        self.assertEqual(ab.get_pollutants()[16].PolGasType, 0)
        self.assertEqual(ab.get_pollutants()[17].PolGasType, 0)
        self.assertEqual(ab.get_pollutants()[0].PolParDepVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[1].PolParDepVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[2].PolParDepVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[3].PolParDepVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[4].PolParDepVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[5].PolParDepVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[6].PolParDepVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[7].PolParDepVelocityKnown, 0)
        self.assertEqual(ab.get_pollutants()[8].PolParDepVelocityKnown, 0)
        self.assertEqual(ab.get_pollutants()[9].PolParDepVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[10].PolParDepVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[11].PolParDepVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[12].PolParDepVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[13].PolParDepVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[14].PolParDepVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[15].PolParDepVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[16].PolParDepVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[17].PolParDepVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[0].PolParTermVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[1].PolParTermVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[2].PolParTermVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[3].PolParTermVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[4].PolParTermVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[5].PolParTermVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[6].PolParTermVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[7].PolParTermVelocityKnown, 0)
        self.assertEqual(ab.get_pollutants()[8].PolParTermVelocityKnown, 0)
        self.assertEqual(ab.get_pollutants()[9].PolParTermVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[10].PolParTermVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[11].PolParTermVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[12].PolParTermVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[13].PolParTermVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[14].PolParTermVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[15].PolParTermVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[16].PolParTermVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[17].PolParTermVelocityKnown, 1)
        self.assertEqual(ab.get_pollutants()[0].PolParDiameter, 1.0e-6)
        self.assertEqual(ab.get_pollutants()[1].PolParDiameter, 1.0e-6)
        self.assertEqual(ab.get_pollutants()[2].PolParDiameter, 1.0e-6)
        self.assertEqual(ab.get_pollutants()[3].PolParDiameter, 1.0e-6)
        self.assertEqual(ab.get_pollutants()[4].PolParDiameter, 1.0e-6)
        self.assertEqual(ab.get_pollutants()[5].PolParDiameter, 1.0e-6)
        self.assertEqual(ab.get_pollutants()[6].PolParDiameter, 1.0e-6)
        self.assertEqual(ab.get_pollutants()[7].PolParDiameter, 1.0e-5)
        self.assertEqual(ab.get_pollutants()[8].PolParDiameter, 2.5e-6)
        self.assertEqual(ab.get_pollutants()[9].PolParDiameter, 1.0e-6)
        self.assertEqual(ab.get_pollutants()[10].PolParDiameter, 1.0e-6)
        self.assertEqual(ab.get_pollutants()[11].PolParDiameter, 1.0e-6)
        self.assertEqual(ab.get_pollutants()[12].PolParDiameter, 1.0e-6)
        self.assertEqual(ab.get_pollutants()[13].PolParDiameter, 1.0e-6)
        self.assertEqual(ab.get_pollutants()[14].PolParDiameter, 1.0e-6)
        self.assertEqual(ab.get_pollutants()[15].PolParDiameter, 1.0e-6)
        self.assertEqual(ab.get_pollutants()[16].PolParDiameter, 1.0e-6)
        self.assertEqual(ab.get_pollutants()[17].PolParDiameter, 1.0e-6)
        self.assertEqual(ab.get_pollutants()[0].PolWetWashoutKnown, 0)
        self.assertEqual(ab.get_pollutants()[1].PolWetWashoutKnown, 0)
        self.assertEqual(ab.get_pollutants()[2].PolWetWashoutKnown, 0)
        self.assertEqual(ab.get_pollutants()[3].PolWetWashoutKnown, 1)
        self.assertEqual(ab.get_pollutants()[4].PolWetWashoutKnown, 1)
        self.assertEqual(ab.get_pollutants()[5].PolWetWashoutKnown, 0)
        self.assertEqual(ab.get_pollutants()[6].PolWetWashoutKnown, 1)
        self.assertEqual(ab.get_pollutants()[7].PolWetWashoutKnown, 1)
        self.assertEqual(ab.get_pollutants()[8].PolWetWashoutKnown, 0)
        self.assertEqual(ab.get_pollutants()[9].PolWetWashoutKnown, 0)
        self.assertEqual(ab.get_pollutants()[10].PolWetWashoutKnown, 1)
        self.assertEqual(ab.get_pollutants()[11].PolWetWashoutKnown, 1)
        self.assertEqual(ab.get_pollutants()[12].PolWetWashoutKnown, 0)
        self.assertEqual(ab.get_pollutants()[13].PolWetWashoutKnown, 1)
        self.assertEqual(ab.get_pollutants()[14].PolWetWashoutKnown, 1)
        self.assertEqual(ab.get_pollutants()[15].PolWetWashoutKnown, 1)
        self.assertEqual(ab.get_pollutants()[16].PolWetWashoutKnown, 0)
        self.assertEqual(ab.get_pollutants()[17].PolWetWashoutKnown, 1)
        self.assertEqual(ab.get_pollutants()[0].PolWetWashout, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[1].PolWetWashout, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[2].PolWetWashout, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[3].PolWetWashout, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[4].PolWetWashout, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[5].PolWetWashout, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[6].PolWetWashout, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[7].PolWetWashout, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[8].PolWetWashout, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[9].PolWetWashout, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[10].PolWetWashout, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[11].PolWetWashout, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[12].PolWetWashout, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[13].PolWetWashout, 1.0e-4)
        self.assertEqual(ab.get_pollutants()[14].PolWetWashout, 1.0e-4)
        self.assertEqual(ab.get_pollutants()[15].PolWetWashout, 1.0e-4)
        self.assertEqual(ab.get_pollutants()[16].PolWetWashout, 1.0e-4)
        self.assertEqual(ab.get_pollutants()[17].PolWetWashout, 1.0e-4)
        self.assertEqual(ab.get_pollutants()[0].PolWetWashoutA, 1.0e-4)
        self.assertEqual(ab.get_pollutants()[1].PolWetWashoutA, 1.0e-4)
        self.assertEqual(ab.get_pollutants()[2].PolWetWashoutA, 1.0e-4)
        self.assertEqual(ab.get_pollutants()[3].PolWetWashoutA, 1.0e-4)
        self.assertEqual(ab.get_pollutants()[4].PolWetWashoutA, 1.0e-4)
        self.assertEqual(ab.get_pollutants()[5].PolWetWashoutA, 1.0e-4)
        self.assertEqual(ab.get_pollutants()[6].PolWetWashoutA, 1.0e-4)
        self.assertEqual(ab.get_pollutants()[7].PolWetWashoutA, 1.0e-4)
        self.assertEqual(ab.get_pollutants()[8].PolWetWashoutA, 3.552e-1)
        self.assertEqual(ab.get_pollutants()[9].PolWetWashoutA, 1.0e-4)
        self.assertEqual(ab.get_pollutants()[10].PolWetWashoutA, 1.0e-4)
        self.assertEqual(ab.get_pollutants()[11].PolWetWashoutA, 1.0e-4)
        self.assertEqual(ab.get_pollutants()[12].PolWetWashoutA, 3.0e-4)
        self.assertEqual(ab.get_pollutants()[13].PolWetWashoutA, 1.0e-4)
        self.assertEqual(ab.get_pollutants()[14].PolWetWashoutA, 1.0e-4)
        self.assertEqual(ab.get_pollutants()[15].PolWetWashoutA, 1.0e-4)
        self.assertEqual(ab.get_pollutants()[16].PolWetWashoutA, 5.0e-3)
        self.assertEqual(ab.get_pollutants()[17].PolWetWashoutA, 1.0e-4)
        self.assertEqual(ab.get_pollutants()[0].PolWetWashoutB, 6.4e-1)
        self.assertEqual(ab.get_pollutants()[1].PolWetWashoutB, 6.4e-1)
        self.assertEqual(ab.get_pollutants()[2].PolWetWashoutB, 6.4e-1)
        self.assertEqual(ab.get_pollutants()[3].PolWetWashoutB, 6.4e-1)
        self.assertEqual(ab.get_pollutants()[4].PolWetWashoutB, 6.4e-1)
        self.assertEqual(ab.get_pollutants()[5].PolWetWashoutB, 6.4e-1)
        self.assertEqual(ab.get_pollutants()[6].PolWetWashoutB, 6.4e-1)
        self.assertEqual(ab.get_pollutants()[7].PolWetWashoutB, 6.4e-1)
        self.assertEqual(ab.get_pollutants()[8].PolWetWashoutB, 5.394e-1)
        self.assertEqual(ab.get_pollutants()[9].PolWetWashoutB, 6.4e-1)
        self.assertEqual(ab.get_pollutants()[10].PolWetWashoutB, 6.4e-1)
        self.assertEqual(ab.get_pollutants()[11].PolWetWashoutB, 6.4e-1)
        self.assertEqual(ab.get_pollutants()[12].PolWetWashoutB, 6.6e-1)
        self.assertEqual(ab.get_pollutants()[13].PolWetWashoutB, 6.4e-1)
        self.assertEqual(ab.get_pollutants()[14].PolWetWashoutB, 6.4e-1)
        self.assertEqual(ab.get_pollutants()[15].PolWetWashoutB, 6.4e-1)
        self.assertEqual(ab.get_pollutants()[16].PolWetWashoutB, 6.4e-1)
        self.assertEqual(ab.get_pollutants()[17].PolWetWashoutB, 6.4e-1)
        self.assertEqual(ab.get_pollutants()[0].PolConvFactor, 5.47e-1)
        self.assertEqual(ab.get_pollutants()[1].PolConvFactor, 5.2e-1)
        self.assertEqual(ab.get_pollutants()[2].PolConvFactor, 5.2e-1)
        self.assertEqual(ab.get_pollutants()[3].PolConvFactor, 8.0e-1)
        self.assertEqual(ab.get_pollutants()[4].PolConvFactor, 5.0e-1)
        self.assertEqual(ab.get_pollutants()[5].PolConvFactor, 3.1e-1)
        self.assertEqual(ab.get_pollutants()[6].PolConvFactor, 3.7e-1)
        self.assertEqual(ab.get_pollutants()[7].PolConvFactor, 1.0e+0)
        self.assertEqual(ab.get_pollutants()[8].PolConvFactor, 1.0e+0)
        self.assertEqual(ab.get_pollutants()[9].PolConvFactor, 8.6e-1)
        self.assertEqual(ab.get_pollutants()[10].PolConvFactor, 3.1e-1)
        self.assertEqual(ab.get_pollutants()[11].PolConvFactor, 4.5e-1)
        self.assertEqual(ab.get_pollutants()[12].PolConvFactor, 6.589e-1)
        self.assertEqual(ab.get_pollutants()[13].PolConvFactor, 3.5e-1)
        self.assertEqual(ab.get_pollutants()[14].PolConvFactor, 4.922e-1)
        self.assertEqual(ab.get_pollutants()[15].PolConvFactor, 4.43e-1)
        self.assertEqual(ab.get_pollutants()[16].PolConvFactor, 1.462e+0)
        self.assertEqual(ab.get_pollutants()[17].PolConvFactor, 0.802e+0)
        self.assertEqual(ab.get_pollutants()[0].PolBkgLevel, 4.14e+5)
        self.assertEqual(ab.get_pollutants()[1].PolBkgLevel, 6.0e+1)
        self.assertEqual(ab.get_pollutants()[2].PolBkgLevel, 4.41e+1)
        self.assertEqual(ab.get_pollutants()[3].PolBkgLevel, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[4].PolBkgLevel, 6.899e+1)
        self.assertEqual(ab.get_pollutants()[5].PolBkgLevel, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[6].PolBkgLevel, 1.513e+1)
        self.assertEqual(ab.get_pollutants()[7].PolBkgLevel, 5.63e+1)
        self.assertEqual(ab.get_pollutants()[8].PolBkgLevel, 8.0e+0)
        self.assertEqual(ab.get_pollutants()[9].PolBkgLevel, 1.222e+3)
        self.assertEqual(ab.get_pollutants()[10].PolBkgLevel, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[11].PolBkgLevel, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[12].PolBkgLevel, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[13].PolBkgLevel, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[14].PolBkgLevel, 6.0e-1)
        self.assertEqual(ab.get_pollutants()[15].PolBkgLevel, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[16].PolBkgLevel, 6.0e+0)
        self.assertEqual(ab.get_pollutants()[17].PolBkgLevel, 0.0e+0)
        self.assertEqual(ab.get_pollutants()[0].PolBkgUnits, Constants.UNIT_PPB)
        self.assertEqual(ab.get_pollutants()[1].PolBkgUnits, Constants.UNIT_PPB)
        self.assertEqual(ab.get_pollutants()[2].PolBkgUnits, Constants.UNIT_PPB)
        self.assertEqual(ab.get_pollutants()[3].PolBkgUnits, Constants.UNIT_PPB)
        self.assertEqual(ab.get_pollutants()[4].PolBkgUnits, Constants.UNIT_PPB)
        self.assertEqual(ab.get_pollutants()[5].PolBkgUnits, Constants.UNIT_PPB)
        self.assertEqual(ab.get_pollutants()[6].PolBkgUnits, Constants.UNIT_PPB)
        self.assertEqual(ab.get_pollutants()[7].PolBkgUnits, Constants.UNIT_UGM3)
        self.assertEqual(ab.get_pollutants()[8].PolBkgUnits, Constants.UNIT_UGM3)
        self.assertEqual(ab.get_pollutants()[9].PolBkgUnits, Constants.UNIT_PPB)
        self.assertEqual(ab.get_pollutants()[10].PolBkgUnits, Constants.UNIT_PPB)
        self.assertEqual(ab.get_pollutants()[11].PolBkgUnits, Constants.UNIT_PPB)
        self.assertEqual(ab.get_pollutants()[12].PolBkgUnits, Constants.UNIT_PPB)
        self.assertEqual(ab.get_pollutants()[13].PolBkgUnits, Constants.UNIT_PPB)
        self.assertEqual(ab.get_pollutants()[14].PolBkgUnits, Constants.UNIT_PPB)
        self.assertEqual(ab.get_pollutants()[15].PolBkgUnits, Constants.UNIT_PPB)
        self.assertEqual(ab.get_pollutants()[16].PolBkgUnits, Constants.UNIT_PPB)
        self.assertEqual(ab.get_pollutants()[17].PolBkgUnits, Constants.UNIT_PPB)

    def test_get_sources(self):
        ab = AplBuilder({Constants.KEY_SRC: [AdmsSrc()]})
        self.assertIsNotNone(ab.get_sources())
        self.assertEqual(len(ab.get_sources()), 1)
        self.assertEqual(ab.get_sources()[0].to_string(), AdmsSrc().to_string())

    def test_get_pol_type(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_pol_type("test"), 0)
        self.assertEqual(ab.get_pol_type(Constants.POL_PM10), 1)
        self.assertEqual(ab.get_pol_type(Constants.POL_PM25), 1)

    def test_get_pol_gas_dep_velocity_known(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_pol_gas_dep_velocity_known("test"), 1)
        type_0 = [Constants.POL_Cl2, Constants.POL_CH3Cl, Constants.POL_ISOBUTYLENE, Constants.POL_NH3,
                  Constants.POL_HC, Constants.POL_NOX]
        for name in type_0:
            self.assertEqual(ab.get_pol_gas_dep_velocity_known(name), 0)

    def test_get_pol_gas_dep_velocity(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_pol_gas_dep_velocity("test"), 0.0e+0)
        self.assertEqual(ab.get_pol_gas_dep_velocity(Constants.POL_NO2), 1.5e-3)
        self.assertEqual(ab.get_pol_gas_dep_velocity(Constants.POL_NO), 1.5e-3)
        self.assertEqual(ab.get_pol_gas_dep_velocity(Constants.POL_PART_SO2), 1.2e-2)
        self.assertEqual(ab.get_pol_gas_dep_velocity(Constants.POL_Cl2), 5.0e+0)

    def test_get_pol_gas_type(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_pol_gas_type("test"), 1)
        type_0 = [Constants.POL_HCl, Constants.POL_Cl2, Constants.POL_CH3Cl, Constants.POL_ISOBUTYLENE,
                  Constants.POL_NH3, Constants.POL_HC]
        for name in type_0:
            self.assertEqual(ab.get_pol_gas_type(name), 0)

    def test_get_pol_par_dep_velocity_known(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_pol_par_dep_velocity_known("test"), 1)
        self.assertEqual(ab.get_pol_par_dep_velocity_known(Constants.POL_PM10), 0)
        self.assertEqual(ab.get_pol_par_dep_velocity_known(Constants.POL_PM25), 0)

    def test_get_pol_par_term_velocity_known(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_pol_par_term_velocity_known("test"), 1)
        self.assertEqual(ab.get_pol_par_term_velocity_known(Constants.POL_PM10), 0)
        self.assertEqual(ab.get_pol_par_term_velocity_known(Constants.POL_PM25), 0)

    def test_get_pol_par_diameter(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_pol_par_diameter("test"), 1.0e-6)
        self.assertEqual(ab.get_pol_par_diameter(Constants.POL_PM10), 1.0e-5)
        self.assertEqual(ab.get_pol_par_diameter(Constants.POL_PM25), 2.5e-6)

    def test_get_pol_wet_washout_known(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_pol_wet_washout_known("test"), 0)
        type_1 = [Constants.POL_NO, Constants.POL_PART_O3, Constants.POL_PART_SO2, Constants.POL_PM10,
                  Constants.POL_BENZENE, Constants.POL_BUTADIENE, Constants.POL_Cl2, Constants.POL_CH3Cl,
                  Constants.POL_ISOBUTYLENE, Constants.POL_HC]
        for name in type_1:
            self.assertEqual(ab.get_pol_wet_washout_known(name), 1)

    def test_get_pol_wet_washout(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_pol_wet_washout("test"), 0.0e+0)
        type_1 = [Constants.POL_Cl2, Constants.POL_CH3Cl, Constants.POL_ISOBUTYLENE, Constants.POL_NH3,
                  Constants.POL_HC]
        for name in type_1:
            self.assertEqual(ab.get_pol_wet_washout(name), 1.0e-4)

    def test_get_pol_wet_washout_a(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_pol_wet_washout_a("test"), 1.0e-4)
        self.assertEqual(ab.get_pol_wet_washout_a(Constants.POL_PM25), 3.552e-1)
        self.assertEqual(ab.get_pol_wet_washout_a(Constants.POL_HCl), 3.0e-4)
        self.assertEqual(ab.get_pol_wet_washout_a(Constants.POL_NH3), 5.0e-3)

    def test_get_pol_wet_washout_b(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_pol_wet_washout_b("test"), 6.4e-1)
        self.assertEqual(ab.get_pol_wet_washout_b(Constants.POL_PM25), 5.394e-1)
        self.assertEqual(ab.get_pol_wet_washout_b(Constants.POL_HCl), 6.6e-1)

    def test_get_pol_conv_factor(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_pol_conv_factor(Constants.POL_CO2), 5.47e-1)
        self.assertEqual(ab.get_pol_conv_factor(Constants.POL_NOX), 5.2e-1)
        self.assertEqual(ab.get_pol_conv_factor(Constants.POL_NO2), 5.2e-1)
        self.assertEqual(ab.get_pol_conv_factor(Constants.POL_NO), 8.0e-1)
        self.assertEqual(ab.get_pol_conv_factor(Constants.POL_PART_O3), 5.0e-1)
        self.assertEqual(ab.get_pol_conv_factor(Constants.POL_VOC), 3.1e-1)
        self.assertEqual(ab.get_pol_conv_factor(Constants.POL_PART_SO2), 3.7e-1)
        self.assertEqual(ab.get_pol_conv_factor(Constants.POL_PM10), 1.0e+0)
        self.assertEqual(ab.get_pol_conv_factor(Constants.POL_PM25), 1.0e+0)
        self.assertEqual(ab.get_pol_conv_factor(Constants.POL_CO), 8.6e-1)
        self.assertEqual(ab.get_pol_conv_factor(Constants.POL_BENZENE), 3.1e-1)
        self.assertEqual(ab.get_pol_conv_factor(Constants.POL_BUTADIENE), 4.5e-1)
        self.assertEqual(ab.get_pol_conv_factor(Constants.POL_HCl), 6.589e-1)
        self.assertEqual(ab.get_pol_conv_factor(Constants.POL_Cl2), 3.5e-1)
        self.assertEqual(ab.get_pol_conv_factor(Constants.POL_CH3Cl), 4.922e-1)
        self.assertEqual(ab.get_pol_conv_factor(Constants.POL_ISOBUTYLENE), 4.43e-1)
        self.assertEqual(ab.get_pol_conv_factor(Constants.POL_NH3), 1.462e+0)
        self.assertEqual(ab.get_pol_conv_factor(Constants.POL_HC), 0.802e+0)

    def test_get_pol_bkg_level(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_pol_bkg_level(Constants.POL_CO2), 4.14e+5)
        self.assertEqual(ab.get_pol_bkg_level(Constants.POL_NOX), 6.0e+1)
        self.assertEqual(ab.get_pol_bkg_level(Constants.POL_NO2), 4.41e+1)
        self.assertEqual(ab.get_pol_bkg_level(Constants.POL_NO), 0.0e+0)
        self.assertEqual(ab.get_pol_bkg_level(Constants.POL_PART_O3), 6.899e+1)
        self.assertEqual(ab.get_pol_bkg_level(Constants.POL_VOC), 0.0e+0)
        self.assertEqual(ab.get_pol_bkg_level(Constants.POL_PART_SO2), 1.513e+1)
        self.assertEqual(ab.get_pol_bkg_level(Constants.POL_PM10), 5.63e+1)
        self.assertEqual(ab.get_pol_bkg_level(Constants.POL_PM25), 8.0e+0)
        self.assertEqual(ab.get_pol_bkg_level(Constants.POL_CO), 1.222e+3)
        self.assertEqual(ab.get_pol_bkg_level(Constants.POL_BENZENE), 0.0e+0)
        self.assertEqual(ab.get_pol_bkg_level(Constants.POL_BUTADIENE), 0.0e+0)
        self.assertEqual(ab.get_pol_bkg_level(Constants.POL_HCl), 0.0e+0)
        self.assertEqual(ab.get_pol_bkg_level(Constants.POL_Cl2), 0.0e+0)
        self.assertEqual(ab.get_pol_bkg_level(Constants.POL_CH3Cl), 6.0e-1)
        self.assertEqual(ab.get_pol_bkg_level(Constants.POL_ISOBUTYLENE), 0.0e+0)
        self.assertEqual(ab.get_pol_bkg_level(Constants.POL_NH3), 6.0e+0)
        self.assertEqual(ab.get_pol_bkg_level(Constants.POL_HC), 0.0e+0)

    def test_get_pol_bkg_units(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_pol_bkg_units("test"), Constants.UNIT_PPB)
        self.assertEqual(ab.get_pol_bkg_units(Constants.POL_PM10), Constants.UNIT_UGM3)
        self.assertEqual(ab.get_pol_bkg_units(Constants.POL_PM25), Constants.UNIT_UGM3)


class AplShipBuilderTest(unittest.TestCase):

    def test_get_sup(self):
        asb = AdmsAplShipBuilder(helper.get_default_apl_builder_data(helper))
        tst_sup = AdmsSup()
        tst_sup.SupModelComplexTerrain = 0
        tst_sup.SupCalcChm = 0
        tst_sup.SupUseAddInput = 0
        tst_sup.SupAddInputPath = "test"
        tst_sup.SupCalcWetDep = 0
        self.assertEqual(asb.get_sup().to_string(), tst_sup.to_string())

    def test_get_met(self):
        asb = AdmsAplShipBuilder(helper.get_default_apl_builder_data(helper))
        tst_met = AdmsMet()
        tst_met.MetDataFileWellFormedPath = "test"
        tst_met.MetLatitude = 1
        self.assertEqual(asb.get_met().to_string(), tst_met.to_string())

    def test_get_hil(self):
        asb = AdmsAplShipBuilder(helper.get_default_apl_builder_data(helper))
        tst_hil = AdmsHil()
        tst_hil.HilTerrainPath = Constants.FILEPATH_HIL_HK
        self.assertEqual(asb.get_hil().to_string(), tst_hil.to_string())

    def test_get_bkg(self):
        asb = AdmsAplShipBuilder(helper.get_default_apl_builder_data(helper))
        tst_bkg = AdmsBkg()
        tst_bkg.BkgFilePath = "test"
        self.assertEqual(asb.get_bkg().to_string(), tst_bkg.to_string())

    def test_get_bkg(self):
        asb = AdmsAplShipBuilder(helper.get_default_apl_builder_data(helper))
        tst_etc = AdmsEtc()
        tst_etc.SrcNumSources = len(helper.get_default_apl_builder_data(helper)[Constants.KEY_SRC])
        self.assertEqual(asb.get_etc().to_string(), tst_etc.to_string())

    def test_get_pol_wet_washout(self):
        asb = AdmsAplShipBuilder(helper.get_default_apl_builder_data(helper))
        type_1 = [Constants.POL_Cl2, Constants.POL_CH3Cl, Constants.POL_ISOBUTYLENE, Constants.POL_NH3,
                  Constants.POL_HC]
        for name in type_1:
            self.assertEqual(asb.get_pol_wet_washout(name), 1.0e-4)
        self.assertEqual(asb.get_pol_wet_washout("test"), 0.0e+0)
        self.assertEqual(asb.get_pol_wet_washout(Constants.POL_PART_SO2), 2.0e-4)
        self.assertEqual(asb.get_pol_wet_washout(Constants.POL_PM10), 3.0e-4)

    def test_get_pollutants(self):
        asb = AdmsAplShipBuilder(helper.get_default_apl_builder_data(helper))
        polls = asb.get_pollutants()
        self.assertIsNotNone(polls)
        self.assertEqual(len(polls), 16)
        self.assertEqual(polls[0].PolName, Constants.POL_CO2)
        self.assertEqual(polls[1].PolName, Constants.POL_NOX)
        self.assertEqual(polls[2].PolName, Constants.POL_NO2)
        self.assertEqual(polls[3].PolName, Constants.POL_NO)
        self.assertEqual(polls[4].PolName, Constants.POL_PART_O3)
        self.assertEqual(polls[5].PolName, Constants.POL_VOC)
        self.assertEqual(polls[6].PolName, Constants.POL_PART_SO2)
        self.assertEqual(polls[7].PolName, Constants.POL_CO)
        self.assertEqual(polls[8].PolName, Constants.POL_BENZENE)
        self.assertEqual(polls[9].PolName, Constants.POL_BUTADIENE)
        self.assertEqual(polls[10].PolName, Constants.POL_HCl)
        self.assertEqual(polls[11].PolName, Constants.POL_Cl2)
        self.assertEqual(polls[12].PolName, Constants.POL_CH3Cl)
        self.assertEqual(polls[13].PolName, Constants.POL_ISOBUTYLENE)
        self.assertEqual(polls[14].PolName, Constants.POL_NH3)
        self.assertEqual(polls[15].PolName, Constants.POL_HC)
        # Values for pollutants same as in superclass. Repeating tests is not necessary here.


class AplPlantBuilderTest(unittest.TestCase):

    def test_get_sup(self):
        apb = AdmsAplPlantBuilder(helper.get_default_apl_builder_data(helper))
        tst_sup = AdmsSup()
        tst_sup.SupModelComplexTerrain = 0
        tst_sup.SupCalcChm = 0
        tst_sup.SupCalcWetDep = 0
        tst_sup.SupCalcPlumeVisibility = 0
        self.assertEqual(apb.get_sup().to_string(), tst_sup.to_string())

    def test_get_met(self):
        apb = AdmsAplPlantBuilder(helper.get_default_apl_builder_data(helper))
        tst_met = AdmsMet()
        tst_met.Met_DS_Roughness = 1.5e+0
        tst_met.MetDataFileWellFormedPath = "test"
        tst_met.MetLatitude = 1.09e+0
        self.assertEqual(apb.get_met().to_string(), tst_met.to_string())

    def test_get_hil(self):
        apb = AdmsAplPlantBuilder(helper.get_default_apl_builder_data(helper))
        tst_hil = AdmsHil()
        tst_hil.HilTerrainPath = Constants.FILEPATH_HIL_SG
        self.assertEqual(apb.get_hil().to_string(), tst_hil.to_string())

    def test_get_bkg(self):
        apb = AdmsAplPlantBuilder(helper.get_default_apl_builder_data(helper))
        tst_bkg = AdmsBkg()
        tst_bkg.BkgFilePath = Constants.FILEPATH_HIL_BGD
        tst_bkg.BkgFixedLevels = 1
        self.assertEqual(apb.get_bkg().to_string(), tst_bkg.to_string())

    def test_get_etc(self):
        apb = AdmsAplPlantBuilder(helper.get_default_apl_builder_data(helper))
        tst_etc = AdmsEtc()
        tst_etc.SrcNumSources = 1
        tst_etc.PolNumPollutants = 18
        self.assertEqual(apb.get_etc().to_string(), tst_etc.to_string())

    def test_get_pol_wet_washout_known(self):
        apb = AdmsAplPlantBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(apb.get_pol_wet_washout_known("test"), 0)
        type_1 = [Constants.POL_NO, Constants.POL_PART_O3, Constants.POL_PART_SO2, Constants.POL_PM10,
                  Constants.POL_BENZENE, Constants.POL_BUTADIENE, Constants.POL_Cl2, Constants.POL_CH3Cl,
                  Constants.POL_ISOBUTYLENE, Constants.POL_HC, Constants.POL_NH3, Constants.POL_HCl, Constants.POL_CO,
                  Constants.POL_PM25, Constants.POL_NO2, Constants.POL_NOX, Constants.POL_CO2]
        for name in type_1:
            self.assertEqual(apb.get_pol_wet_washout_known(name), 1)

    def test_get_pol_bkg_level(self):
        apb = AdmsAplPlantBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(apb.get_pol_bkg_level(Constants.POL_CO2), 0.0e+0)
        self.assertEqual(apb.get_pol_bkg_level(Constants.POL_NOX), 0.0e+0)
        self.assertEqual(apb.get_pol_bkg_level(Constants.POL_NO2), 0.0e+0)
        self.assertEqual(apb.get_pol_bkg_level(Constants.POL_NO), 0.0e+0)
        self.assertEqual(apb.get_pol_bkg_level(Constants.POL_PART_O3), 0.0e+0)
        self.assertEqual(apb.get_pol_bkg_level(Constants.POL_VOC), 0.0e+0)
        self.assertEqual(apb.get_pol_bkg_level(Constants.POL_PART_SO2), 0.0e+0)
        self.assertEqual(apb.get_pol_bkg_level(Constants.POL_PM10), 0.0e+0)
        self.assertEqual(apb.get_pol_bkg_level(Constants.POL_PM25), 0.0e+0)
        self.assertEqual(apb.get_pol_bkg_level(Constants.POL_CO), 0.0e+0)
        self.assertEqual(apb.get_pol_bkg_level(Constants.POL_BENZENE), 0.0e+0)
        self.assertEqual(apb.get_pol_bkg_level(Constants.POL_BUTADIENE), 0.0e+0)
        self.assertEqual(apb.get_pol_bkg_level(Constants.POL_HCl), 0.0e+0)
        self.assertEqual(apb.get_pol_bkg_level(Constants.POL_Cl2), 0.0e+0)
        self.assertEqual(apb.get_pol_bkg_level(Constants.POL_CH3Cl), 6.0e-1)
        self.assertEqual(apb.get_pol_bkg_level(Constants.POL_ISOBUTYLENE), 0.0e+0)
        self.assertEqual(apb.get_pol_bkg_level(Constants.POL_NH3), 6.0e+0)
        self.assertEqual(apb.get_pol_bkg_level(Constants.POL_HC), 0.0e+0)

    def test_get_pol_gas_dep_velocity_known(self):
        apb = AdmsAplPlantBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(apb.get_pol_gas_dep_velocity_known("test"), 1)
        type_0 = [Constants.POL_Cl2, Constants.POL_CH3Cl, Constants.POL_ISOBUTYLENE, Constants.POL_NH3,
                  Constants.POL_HC]
        for name in type_0:
            self.assertEqual(apb.get_pol_gas_dep_velocity_known(name), 0)

    def test_get_pol_gas_dep_velocity(self):
        apb = AdmsAplPlantBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(apb.get_pol_gas_dep_velocity("test"), 0.0e+0)
        self.assertEqual(apb.get_pol_gas_dep_velocity(Constants.POL_Cl2), 5.0e+0)

    def test_get_pol_par_dep_velocity_known(self):
        apb = AdmsAplPlantBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(apb.get_pol_par_dep_velocity_known("test"), 1)

    def test_get_pol_par_term_velocity_known(self):
        apb = AdmsAplPlantBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(apb.get_pol_par_term_velocity_known("test"), 1)

    def test_get_pol_wet_washout_a(self):
        apb = AdmsAplPlantBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(apb.get_pol_wet_washout_a("test"), 1.0e-4)

    def test_get_pol_wet_washout_b(self):
        apb = AdmsAplPlantBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(apb.get_pol_wet_washout_b("test"), 6.4e-1)
