import unittest

from adms_apl import *
from adms_apl_builder import AplBuilder
from adms_apl_test import AdmsAplTestHelper as helper
from config import Constants


class AplDirectorTest(unittest.TestCase):
    pass

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
        #@TODO [AC] - implementation
        pass

    def test_get_sources(self):
        #@TODO [AC] - implementation
        pass

    def test_get_pol_type(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_pol_type("test"), 0)
        self.assertEqual(ab.get_pol_type(Constants.POL_PM10), 1)

    def test_get_pol_gas_dep_velocity_known(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_pol_gas_dep_velocity_known("test"), 1)
        self.assertEqual(ab.get_pol_gas_dep_velocity_known(Constants.POL_Cl2), 0)

    def test_get_pol_gas_dep_velocity(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_pol_gas_dep_velocity("test"), 0.0e+0)
        self.assertEqual(ab.get_pol_gas_dep_velocity(Constants.POL_NO2), 1.5e-3)
        self.assertEqual(ab.get_pol_gas_dep_velocity(Constants.POL_PART_SO2), 1.2e-2)
        self.assertEqual(ab.get_pol_gas_dep_velocity(Constants.POL_Cl2), 5.0e+0)

    def test_get_pol_gas_type(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_pol_gas_type("test"), 1)
        self.assertEqual(ab.get_pol_gas_type(Constants.POL_HCl), 0)

    def test_get_pol_par_dep_velocity_known(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_pol_par_dep_velocity_known("test"), 1)
        self.assertEqual(ab.get_pol_par_dep_velocity_known(Constants.POL_PM10), 0)

    def test_get_pol_par_term_velocity_known(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_pol_par_term_velocity_known("test"), 1)
        self.assertEqual(ab.get_pol_par_term_velocity_known(Constants.POL_PM10), 0)

    def test_get_pol_par_diameter(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_pol_par_diameter("test"), 1.0e-6)
        self.assertEqual(ab.get_pol_par_diameter(Constants.POL_PM10), 1.0e-5)
        self.assertEqual(ab.get_pol_par_diameter(Constants.POL_PM25), 2.5e-6)

    def test_get_pol_wet_washout_known(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_pol_wet_washout_known("test"), 0)
        self.assertEqual(ab.get_pol_wet_washout_known(Constants.POL_NO), 1)

    def test_get_pol_wet_washout(self):
        ab = AplBuilder(helper.get_default_apl_builder_data(helper))
        self.assertEqual(ab.get_pol_wet_washout("test"), 0.0e+0)
        self.assertEqual(ab.get_pol_wet_washout(Constants.POL_Cl2), 1.0e-4)

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


class AplShipBuilderTest(unittest.TestCase):
    pass

class AplplantBuilderTest(unittest.TestCase):
    pass
