import unittest

from adms_apl import *


class AplTest(unittest.TestCase):

    def test_init(self):
        apl = Apl()
        self.assertIsNone(apl._Apl__header)
        self.assertIsNone(apl._Apl__sup)
        self.assertIsNone(apl._Apl__met)
        self.assertIsNone(apl._Apl__bld)
        self.assertIsNone(apl._Apl__hil)
        self.assertIsNone(apl._Apl__cst)
        self.assertIsNone(apl._Apl__flc)
        self.assertIsNone(apl._Apl__grd)
        self.assertIsNone(apl._Apl__puf)
        self.assertIsNone(apl._Apl__gam)
        self.assertIsNone(apl._Apl__opt)
        self.assertIsNone(apl._Apl__bkg)
        self.assertIsNone(apl._Apl__etc)
        self.assertIsNone(apl._Apl__chm)
        self.assertIsNone(apl._Apl__coordsys)
        self.assertIsNone(apl._Apl__mapper)
        self.assertIsNone(apl._Apl__pollutants)
        self.assertIsNone(apl._Apl__sources)

    def test_set_header(self):
        apl = Apl()
        apl.set_header(AdmsHeader())
        self.assertIsNotNone(apl._Apl__header)

    def test_set_sup(self):
        apl = Apl()
        apl.set_sup(AdmsSup())
        self.assertIsNotNone(apl._Apl__sup)

    def test_set_met(self):
        apl = Apl()
        apl.set_met(AdmsMet())
        self.assertIsNotNone(apl._Apl__met)

    def test_set_bld(self):
        apl = Apl()
        apl.set_bld(AdmsBld())
        self.assertIsNotNone(apl._Apl__bld)

    def test_set_hil(self):
        apl = Apl()
        apl.set_hil(AdmsHil())
        self.assertIsNotNone(apl._Apl__hil)

    def test_set_cst(self):
        apl = Apl()
        apl.set_cst(AdmsCst())
        self.assertIsNotNone(apl._Apl__cst)

    def test_set_flc(self):
        apl = Apl()
        apl.set_flc(AdmsFlc())
        self.assertIsNotNone(apl._Apl__flc)

    def test_set_grd(self):
        apl = Apl()
        apl.set_grd(AdmsGrd())
        self.assertIsNotNone(apl._Apl__grd)

    def test_set_puf(self):
        apl = Apl()
        apl.set_puf(AdmsPuf())
        self.assertIsNotNone(apl._Apl__puf)

    def test_set_gam(self):
        apl = Apl()
        apl.set_gam(AdmsGam())
        self.assertIsNotNone(apl._Apl__gam)

    def test_set_opt(self):
        apl = Apl()
        apl.set_opt(AdmsOpt())
        self.assertIsNotNone(apl._Apl__opt)

    def test_set_bkg(self):
        apl = Apl()
        apl.set_bkg(AdmsBkg())
        self.assertIsNotNone(apl._Apl__bkg)

    def test_set_chm(self):
        apl = Apl()
        apl.set_chm(AdmsChm())
        self.assertIsNotNone(apl._Apl__chm)

    def test_set_etc(self):
        apl = Apl()
        apl.set_etc(AdmsEtc())
        self.assertIsNotNone(apl._Apl__etc)

    def test_set_coordsys(self):
        apl = Apl()
        apl.set_coordsys(AdmsCoordSys())
        self.assertIsNotNone(apl._Apl__coordsys)

    def test_set_mapper(self):
        apl = Apl()
        apl.set_mapper(AdmsMapper())
        self.assertIsNotNone(apl._Apl__mapper)

    def test_set_pollutants(self):
        apl = Apl()
        apl.set_pollutants([AdmsPold()])
        self.assertIsNotNone(apl._Apl__pollutants)

    def test_set_sources(self):
        apl = Apl()
        apl.set_sources([AdmsSrc()])
        self.assertIsNotNone(apl._Apl__sources)

    def test_specification(self):
        apl = Apl()
        apl.set_header(AdmsHeader())
        apl.set_sup(AdmsSup())
        apl.set_met(AdmsMet())
        apl.set_bld(AdmsBld())
        apl.set_hil(AdmsHil())
        apl.set_cst(AdmsCst())
        apl.set_flc(AdmsFlc())
        apl.set_grd(AdmsGrd())
        apl.set_puf(AdmsPuf())
        apl.set_gam(AdmsGam())
        apl.set_opt(AdmsOpt())
        apl.set_bkg(AdmsBkg())
        apl.set_chm(AdmsChm())
        apl.set_etc(AdmsEtc())
        apl.set_coordsys(AdmsCoordSys())
        apl.set_mapper(AdmsMapper())
        apl.set_pollutants([AdmsPold()])
        apl.set_sources([AdmsSrc()])
        self.assertEqual(AdmsAplTestHelper.get_default_specification(AdmsAplTestHelper), apl.specification())

class AplPartTest(unittest.TestCase):

    def test_init(self):
        aplp = AplPart()
        self.assertEqual(aplp._AplPart__name, '&')
        self.assertEqual(aplp._AplPart__end, '/')

    def test_to_string(self):
        aplp = AplPart()
        aplp._name = aplp._AplPart__name
        self.assertEqual(aplp.to_string(), AdmsAplTestHelper.get_default_aplp_str())

class AdmsHeaderTest(unittest.TestCase):

    def test_init(self):
        ah = AdmsHeader()
        self.assertEqual(ah._name, '&ADMS_HEADER')
        self.assertEqual(ah.Comment, 'This is an ADMS parameter file')
        self.assertEqual(ah.Model, 'ADMS')
        self.assertEqual(ah.Version, 5.2)
        self.assertEqual(ah.FileVersion, 8)
        self.assertEqual(ah.Complete, 1)

    def test_to_string(self):
        ah = AdmsHeader()
        self.assertEqual(ah.to_string(), AdmsAplTestHelper.get_default_adms_header_str())

class AdmsSupTest(unittest.TestCase):

    def test_init(self):
        asp = AdmsSup()
        self.assertEqual(asp._name, '&ADMS_PARAMETERS_SUP')
        self.assertEqual(asp.SupSiteName, 'terrain dispersion site')
        self.assertEqual(asp.SupProjectName, 'chlorine leakage tank dispersion')
        self.assertEqual(asp.SupUseAddInput, 0)
        self.assertEqual(asp.SupAddInputPath, ' ')
        self.assertEqual(asp.SupReleaseType, 0)
        self.assertEqual(asp.SupModelBuildings, 1)
        self.assertEqual(asp.SupModelComplexTerrain, 1)
        self.assertEqual(asp.SupModelCoastline, 0)
        self.assertEqual(asp.SupPufType, 0)
        self.assertEqual(asp.SupCalcChm, 1)
        self.assertEqual(asp.SupCalcDryDep, 0)
        self.assertEqual(asp.SupCalcWetDep, 1)
        self.assertEqual(asp.SupCalcPlumeVisibility, 1)
        self.assertEqual(asp.SupModelFluctuations, 0)
        self.assertEqual(asp.SupModelRadioactivity, 0)
        self.assertEqual(asp.SupModelOdours, 0)
        self.assertEqual(asp.SupOdourUnits, 'ou_e')
        self.assertEqual(asp.SupPaletteType, 1)
        self.assertEqual(asp.SupUseTimeVaryingEmissions, 0)
        self.assertEqual(asp.SupTimeVaryingEmissionsType, 0)
        self.assertEqual(asp.SupTimeVaryingVARPath, ' ')
        self.assertEqual(asp.SupTimeVaryingFACPath, ' ')
        self.assertEqual(asp.SupTimeVaryingEmissionFactorsWeekday, [1.0e+0, 1.0e+0, 1.0e+0, 1.0e+0,
                                                                    1.0e+0, 1.0e+0, 1.0e+0, 1.0e+0,
                                                                    1.0e+0, 1.0e+0, 1.0e+0, 1.0e+0,
                                                                    1.0e+0, 1.0e+0, 1.0e+0, 1.0e+0,
                                                                    1.0e+0, 1.0e+0, 1.0e+0, 1.0e+0,
                                                                    1.0e+0, 1.0e+0, 1.0e+0, 1.0e+0])
        self.assertEqual(asp.SupTimeVaryingEmissionFactorsSaturday, [1.0e+0, 1.0e+0, 1.0e+0, 1.0e+0,
                                                                     1.0e+0, 1.0e+0, 1.0e+0, 1.0e+0,
                                                                     1.0e+0, 1.0e+0, 1.0e+0, 1.0e+0,
                                                                     1.0e+0, 1.0e+0, 1.0e+0, 1.0e+0,
                                                                     1.0e+0, 1.0e+0, 1.0e+0, 1.0e+0,
                                                                     1.0e+0, 1.0e+0, 1.0e+0, 1.0e+0])
        self.assertEqual(asp.SupTimeVaryingEmissionFactorsSunday, [1.0e+0, 1.0e+0, 1.0e+0, 1.0e+0,
                                                                   1.0e+0, 1.0e+0, 1.0e+0, 1.0e+0,
                                                                   1.0e+0, 1.0e+0, 1.0e+0, 1.0e+0,
                                                                   1.0e+0, 1.0e+0, 1.0e+0, 1.0e+0,
                                                                   1.0e+0, 1.0e+0, 1.0e+0, 1.0e+0,
                                                                   1.0e+0, 1.0e+0, 1.0e+0, 1.0e+0])

    def test_to_string(self):
        asp = AdmsSup()
        self.assertEqual(asp.to_string(), AdmsAplTestHelper.get_default_adms_sup_str())

class AdmsMetTest(unittest.TestCase):

    def test_init(self):
        am = AdmsMet()
        self.assertEqual(am._name, '&ADMS_PARAMETERS_MET')
        self.assertEqual(am.MetLatitude, 0)
        self.assertEqual(am.MetDataSource, 0)
        self.assertEqual(am.MetDataFileWellFormedPath, ' ')
        self.assertEqual(am.MetWindHeight, 1.0e+1)
        self.assertEqual(am.MetWindInSectors, 0)
        self.assertEqual(am.MetWindSectorSizeDegrees, 1.0e+1)
        self.assertEqual(am.MetDataIsSequential, 0)
        self.assertEqual(am.MetUseSubset, 0)
        self.assertEqual(am.MetSubsetHourStart, 1)
        self.assertEqual(am.MetSubsetDayStart, 1)
        self.assertEqual(am.MetSubsetMonthStart, 1)
        self.assertEqual(am.MetSubsetYearStart, 2016)
        self.assertEqual(am.MetSubsetHourEnd, 0)
        self.assertEqual(am.MetSubsetDayEnd, 1)
        self.assertEqual(am.MetSubsetMonthEnd, 1)
        self.assertEqual(am.MetSubsetYearEnd, 2017)
        self.assertEqual(am.MetUseVerticalProfile, 0)
        self.assertEqual(am.MetVerticalProfilePath, ' ')
        self.assertEqual(am.Met_DS_RoughnessMode, 1)
        self.assertEqual(am.Met_DS_Roughness, 1.0e+0)
        self.assertEqual(am.Met_DS_UseAdvancedMet, 0)
        self.assertEqual(am.Met_DS_SurfaceAlbedoMode, 1)
        self.assertEqual(am.Met_DS_SurfaceAlbedo, 2.3e-1)
        self.assertEqual(am.Met_DS_PriestlyTaylorMode, 1)
        self.assertEqual(am.Met_DS_PriestlyTaylor, 1.0e+0)
        self.assertEqual(am.Met_DS_MinLmoMode, 1)
        self.assertEqual(am.Met_DS_MinLmo, 3.45e+1)
        self.assertEqual(am.Met_DS_PrecipFactorMode, 1)
        self.assertEqual(am.Met_DS_PrecipFactor, 4.5e-1)
        self.assertEqual(am.Met_MS_RoughnessMode, 3)
        self.assertEqual(am.Met_MS_Roughness, 1.0e-1)
        self.assertEqual(am.Met_MS_UseAdvancedMet, 0)
        self.assertEqual(am.Met_MS_SurfaceAlbedoMode, 3)
        self.assertEqual(am.Met_MS_SurfaceAlbedo, 2.3e-1)
        self.assertEqual(am.Met_MS_PriestlyTaylorMode, 3)
        self.assertEqual(am.Met_MS_PriestlyTaylor, 1.0e+0)
        self.assertEqual(am.Met_MS_MinLmoMode, 3)
        self.assertEqual(am.Met_MS_MinLmo, 1.0e+0)
        self.assertEqual(am.MetHeatFluxType, 0)
        self.assertEqual(am.MetInclBoundaryLyrHt, 0)
        self.assertEqual(am.MetInclSurfaceTemp, 1)
        self.assertEqual(am.MetInclLateralSpread, 0)
        self.assertEqual(am.MetInclRelHumidity, 0)
        self.assertEqual(am.MetHandNumEntries, 1)
        self.assertEqual(am.MetWindSpeed, [3.06e+0])
        self.assertEqual(am.MetWindDirection, [6.0e+1])
        self.assertEqual(am.MetJulianDayNum, [2.47e+2])
        self.assertEqual(am.MetLocalTime, [5.0e+0])
        self.assertEqual(am.MetCloudAmount, [5.0e+0])
        self.assertEqual(am.MetSurfaceHeatFlux, [0.0e+0])
        self.assertEqual(am.MetBoundaryLayerHeight, [8.00e+2])
        self.assertEqual(am.MetSurfaceTemp, [2.8e+1])
        self.assertEqual(am.MetLateralSpread, [7.5e+0])
        self.assertEqual(am.MetYear, [2017])
        self.assertEqual(am.MetRelHumidity, [7.4e+1])

    def test_to_string(self):
        am = AdmsMet()
        self.assertEqual(am.to_string(), AdmsAplTestHelper.get_default_adms_met_str())

class AdmsHilTest(unittest.TestCase):

    def test_init(self):
        ah = AdmsHil()
        self.assertEqual(ah._name, '&ADMS_PARAMETERS_HIL')
        self.assertEqual(ah.HilGridSize, 2)
        self.assertEqual(ah.HilUseTerFile, 1)
        self.assertEqual(ah.HilUseRoughFile, 0)
        self.assertEqual(ah.HilTerrainPath, ' ')
        self.assertEqual(ah.HilRoughPath, ' ')
        self.assertEqual(ah.HilCreateFlowField, 1)

    def test_to_string(self):
        ah = AdmsHil()
        self.assertEqual(ah.to_string(), AdmsAplTestHelper.get_default_adms_hil_str())

class AdmsCstTest(unittest.TestCase):

    def test_init(self):
        ac = AdmsCst()
        self.assertEqual(ac._name, '&ADMS_PARAMETERS_CST')
        self.assertEqual(ac.CstPoint1X, 0.0e+0)
        self.assertEqual(ac.CstPoint1Y, 0.0e+0)
        self.assertEqual(ac.CstPoint2X, -1.000e+3)
        self.assertEqual(ac.CstPoint2Y, 1.000e+3)
        self.assertEqual(ac.CstLandPointX, 5.00e+2)
        self.assertEqual(ac.CstLandPointY, 5.00e+2)

    def test_to_string(self):
        ac = AdmsCst()
        self.assertEqual(ac.to_string(), AdmsAplTestHelper.get_default_adms_cst_str())

class AdmsFlcTest(unittest.TestCase):

    def test_init(self):
        af = AdmsFlc()
        self.assertEqual(af._name, '&ADMS_PARAMETERS_FLC')
        self.assertEqual(af.FlcAvgTime, 9.00e+2)
        self.assertEqual(af.FlcUnitsPollutants, 'ug/m3')
        self.assertEqual(af.FlcUnitsIsotopes, 'Bq/m3')
        self.assertEqual(af.FlcCalcToxicResponse, 0)
        self.assertEqual(af.FlcToxicExp, 1.0e+0)
        self.assertEqual(af.FlcCalcPercentiles, 0)
        self.assertEqual(af.FlcNumPercentiles, 0)
        self.assertEqual(af.FlcCalcPDF, 0)
        self.assertEqual(af.FlcNumPDF, 0)

    def test_to_string(self):
        af = AdmsFlc()
        self.assertEqual(af.to_string(), AdmsAplTestHelper.get_default_adms_flc_str())

class AdmsGrdTest(unittest.TestCase):

    def test_init(self):
        ag = AdmsGrd()
        self.assertEqual(ag._name, '&ADMS_PARAMETERS_GRD')
        self.assertEqual(ag.GrdType, 0)
        self.assertEqual(ag.GrdCoordSysType, 0)
        self.assertEqual(ag.GrdSpacingType, 0)
        self.assertEqual(ag.GrdRegularMin, [0.00e+0, 0.00e+0, 0.00e+0, 1.0e+1, 0.0e+0, 0.0e+0])
        self.assertEqual(ag.GrdRegularMax, [0.00e+0, 0.00e+0, 3.00e+1, 1.000e+3, 3.30e+2, 0.0e+0])
        self.assertEqual(ag.GrdRegularNumPoints, [80, 80, 4, 10, 12, 1])
        self.assertEqual(ag.GrdVarSpaceNumPointsX, 0)
        self.assertEqual(ag.GrdVarSpaceNumPointsY, 0)
        self.assertEqual(ag.GrdVarSpaceNumPointsZ, 0)
        self.assertEqual(ag.GrdVarSpaceNumPointsR, 0)
        self.assertEqual(ag.GrdVarSpaceNumPointsTh, 0)
        self.assertEqual(ag.GrdVarSpaceNumPointsZp, 0)
        self.assertEqual(ag.GrdPtsNumPoints, [0, 0])
        self.assertEqual(ag.GrdPolarCentreX, 0.0e+0)
        self.assertEqual(ag.GrdPolarCentreY, 0.0e+0)
        self.assertEqual(ag.GrdPtsUsePointsFile, 1)
        self.assertEqual(ag.GrdPtsPointsFilePath, ' ')

    def test_to_string(self):
        ag = AdmsGrd()
        self.assertEqual(ag.to_string(), AdmsAplTestHelper.get_default_adms_grd_str())

class AdmsPufTest(unittest.TestCase):

    def test_init(self):
        ap = AdmsPuf()
        self.assertEqual(ap._name, '&ADMS_PARAMETERS_PUF')
        self.assertEqual(ap.PufStart, 1.00e+2)
        self.assertEqual(ap.PufStep, 1.00e+2)
        self.assertEqual(ap.PufNumSteps, 10)

    def test_to_string(self):
        ap = AdmsPuf()
        self.assertEqual(ap.to_string(), AdmsAplTestHelper.get_default_adms_puf_str())

class AdmsGamTest(unittest.TestCase):

    def test_init(self):
        ag = AdmsGam()
        self.assertEqual(ag._name, '&ADMS_PARAMETERS_GAM')
        self.assertEqual(ag.GamCalcDose, 0)

    def test_to_string(self):
        ag = AdmsGam()
        self.assertEqual(ag.to_string(), AdmsAplTestHelper.get_default_adms_gam_str())

class AdmsBkgTest(unittest.TestCase):

    def test_init(self):
        ab = AdmsBkg()
        self.assertEqual(ab._name, '&ADMS_PARAMETERS_BKG')
        self.assertEqual(ab.BkgFilePath, ' ')
        self.assertEqual(ab.BkgFixedLevels, 0)

    def test_to_string(self):
        ab = AdmsBkg()
        self.assertEqual(ab.to_string(), AdmsAplTestHelper.get_default_adms_bkg_str())

class AdmsEtcTest(unittest.TestCase):
    pass


class AdmsChmTest(unittest.TestCase):
    pass


class AdmsCoordSysTest(unittest.TestCase):
    pass


class AdmsMapperTest(unittest.TestCase):
    pass


class AdmsBldTest(unittest.TestCase):
    pass


class AdmsOptTest(unittest.TestCase):
    pass


class AdmsPoldTest(unittest.TestCase):
    pass


class AdmsSrcTest(unittest.TestCase):
    pass


class AdmsAplTestHelper(object):

    @staticmethod
    def get_default_specification(self):
        return self.get_default_adms_header_str() + \
               self.get_default_adms_sup_str() + \
               self.get_default_adms_met_str() + \
               self.get_default_adms_bld_str() + \
               self.get_default_adms_hil_str() + \
               self.get_default_adms_cst_str() + \
               self.get_default_adms_flc_str() + \
               self.get_default_adms_grd_str() + \
               self.get_default_adms_puf_str() + \
               self.get_default_adms_gam_str() + \
               self.get_default_adms_chm_str() + \
               self.get_default_adms_bkg_str() + \
               self.get_default_adms_etc_str() + \
               self.get_default_adms_coords_str() + \
               self.get_default_adms_map_str() + \
               self.get_default_adms_pold_str() + \
               self.get_default_adms_src_str()

    @staticmethod
    def get_default_aplp_str():
        return '&\n/\n\n'

    @staticmethod
    def get_default_adms_header_str():
        return '&ADMS_HEADER\n' \
               'Comment = "This is an ADMS parameter file"\n' \
               'Model = "ADMS"\n' \
               'Version = 5.2\n' \
               'FileVersion = 8\n' \
               'Complete = 1\n' \
               '/\n\n'

    @staticmethod
    def get_default_adms_sup_str():
        return '&ADMS_PARAMETERS_SUP\n' \
               'SupSiteName = "terrain dispersion site"\n' \
               'SupProjectName = "chlorine leakage tank dispersion"\n' \
               'SupUseAddInput = 0\n' \
               'SupAddInputPath = " "\n' \
               'SupReleaseType = 0\n' \
               'SupModelBuildings = 1\n' \
               'SupModelComplexTerrain = 1\n' \
               'SupModelCoastline = 0\n' \
               'SupPufType = 0\n' \
               'SupCalcChm = 1\n' \
               'SupCalcDryDep = 0\n' \
               'SupCalcWetDep = 1\n' \
               'SupCalcPlumeVisibility = 1\n' \
               'SupModelFluctuations = 0\n' \
               'SupModelRadioactivity = 0\n' \
               'SupModelOdours = 0\n' \
               'SupOdourUnits = "ou_e"\n' \
               'SupPaletteType = 1\n' \
               'SupUseTimeVaryingEmissions = 0\n' \
               'SupTimeVaryingEmissionsType = 0\n' \
               'SupTimeVaryingVARPath = " "\n' \
               'SupTimeVaryingFACPath = " "\n' \
               'SupTimeVaryingEmissionFactorsWeekday = \n' \
               '  1.0e+0 1.0e+0 1.0e+0 1.0e+0 \n' \
               '  1.0e+0 1.0e+0 1.0e+0 1.0e+0 \n' \
               '  1.0e+0 1.0e+0 1.0e+0 1.0e+0 \n' \
               '  1.0e+0 1.0e+0 1.0e+0 1.0e+0 \n' \
               '  1.0e+0 1.0e+0 1.0e+0 1.0e+0 \n' \
               '  1.0e+0 1.0e+0 1.0e+0 1.0e+0 \n' \
               'SupTimeVaryingEmissionFactorsSaturday = \n' \
               '  1.0e+0 1.0e+0 1.0e+0 1.0e+0 \n' \
               '  1.0e+0 1.0e+0 1.0e+0 1.0e+0 \n' \
               '  1.0e+0 1.0e+0 1.0e+0 1.0e+0 \n' \
               '  1.0e+0 1.0e+0 1.0e+0 1.0e+0 \n' \
               '  1.0e+0 1.0e+0 1.0e+0 1.0e+0 \n' \
               '  1.0e+0 1.0e+0 1.0e+0 1.0e+0 \n' \
               'SupTimeVaryingEmissionFactorsSunday = \n' \
               '  1.0e+0 1.0e+0 1.0e+0 1.0e+0 \n' \
               '  1.0e+0 1.0e+0 1.0e+0 1.0e+0 \n' \
               '  1.0e+0 1.0e+0 1.0e+0 1.0e+0 \n' \
               '  1.0e+0 1.0e+0 1.0e+0 1.0e+0 \n' \
               '  1.0e+0 1.0e+0 1.0e+0 1.0e+0 \n' \
               '  1.0e+0 1.0e+0 1.0e+0 1.0e+0 \n' \
               '/\n\n'

    @staticmethod
    def get_default_adms_met_str():
        return '&ADMS_PARAMETERS_MET\n' \
               'MetLatitude = 0\n' \
               'MetDataSource = 0\n' \
               'MetDataFileWellFormedPath = " "\n' \
               'MetWindHeight = 10.0\n' \
               'MetWindInSectors = 0\n' \
               'MetWindSectorSizeDegrees = 10.0\n' \
               'MetDataIsSequential = 0\n' \
               'MetUseSubset = 0\n' \
               'MetSubsetHourStart = 1\n' \
               'MetSubsetDayStart = 1\n' \
               'MetSubsetMonthStart = 1\n' \
               'MetSubsetYearStart = 2016\n' \
               'MetSubsetHourEnd = 0\n' \
               'MetSubsetDayEnd = 1\n' \
               'MetSubsetMonthEnd = 1\n' \
               'MetSubsetYearEnd = 2017\n' \
               'MetUseVerticalProfile = 0\n' \
               'MetVerticalProfilePath = " "\n' \
               'Met_DS_RoughnessMode = 1\n' \
               'Met_DS_Roughness = 1.0\n' \
               'Met_DS_UseAdvancedMet = 0\n' \
               'Met_DS_SurfaceAlbedoMode = 1\n' \
               'Met_DS_SurfaceAlbedo = 0.23\n' \
               'Met_DS_PriestlyTaylorMode = 1\n' \
               'Met_DS_PriestlyTaylor = 1.0\n' \
               'Met_DS_MinLmoMode = 1\n' \
               'Met_DS_MinLmo = 34.5\n' \
               'Met_DS_PrecipFactorMode = 1\n' \
               'Met_DS_PrecipFactor = 0.45\n' \
               'Met_MS_RoughnessMode = 3\n' \
               'Met_MS_Roughness = 0.1\n' \
               'Met_MS_UseAdvancedMet = 0\n' \
               'Met_MS_SurfaceAlbedoMode = 3\n' \
               'Met_MS_SurfaceAlbedo = 0.23\n' \
               'Met_MS_PriestlyTaylorMode = 3\n' \
               'Met_MS_PriestlyTaylor = 1.0\n' \
               'Met_MS_MinLmoMode = 3\n' \
               'Met_MS_MinLmo = 1.0\n' \
               'MetHeatFluxType = 0\n' \
               'MetInclBoundaryLyrHt = 0\n' \
               'MetInclSurfaceTemp = 1\n' \
               'MetInclLateralSpread = 0\n' \
               'MetInclRelHumidity = 0\n' \
               'MetHandNumEntries = 1\n' \
               'MetWindSpeed = \n' \
               '  3.060e+0 \n' \
               'MetWindDirection = \n' \
               '  6.0e+1 \n' \
               'MetJulianDayNum = \n' \
               '  2.470e+2 \n' \
               'MetLocalTime = \n' \
               '  5.0e+0 \n' \
               'MetCloudAmount = \n' \
               '  5.0e+0 \n' \
               'MetSurfaceHeatFlux = \n' \
               '  0.0e+0 \n' \
               'MetBoundaryLayerHeight = \n' \
               '  8.0e+2 \n' \
               'MetSurfaceTemp = \n' \
               '  2.80e+1 \n' \
               'MetLateralSpread = \n' \
               '  7.50e+0 \n' \
               'MetYear = \n' \
               '  2017 \n' \
               'MetRelHumidity = \n' \
               '  7.40e+1 \n' \
               '/\n\n'

    @staticmethod
    def get_default_adms_bld_str():
        return '&ADMS_PARAMETERS_BLD\n' \
               'BldNumBuildings = 0\n' \
               'BldName = \n  \n' \
               'BldType = \n  \n' \
               'BldX = \n  \n' \
               'BldY = \n  \n' \
               'BldHeight = \n  \n' \
               'BldLength = \n  \n' \
               'BldWidth = \n  \n' \
               'BldAngle = \n  \n' \
               '/\n\n'

    @staticmethod
    def get_default_adms_hil_str():
        return '&ADMS_PARAMETERS_HIL\n' \
               'HilGridSize = 2\n' \
               'HilUseTerFile = 1\n' \
               'HilUseRoughFile = 0\n' \
               'HilTerrainPath = " "\n' \
               'HilRoughPath = " "\n' \
               'HilCreateFlowField = 1\n' \
               '/\n\n'

    @staticmethod
    def get_default_adms_cst_str():
        return '&ADMS_PARAMETERS_CST\n' \
               'CstPoint1X = 0.0\n' \
               'CstPoint1Y = 0.0\n' \
               'CstPoint2X = -1000.0\n' \
               'CstPoint2Y = 1000.0\n' \
               'CstLandPointX = 500.0\n' \
               'CstLandPointY = 500.0\n' \
               '/\n\n'

    @staticmethod
    def get_default_adms_flc_str():
        return '&ADMS_PARAMETERS_FLC\n' \
               'FlcAvgTime = 900.0\n' \
               'FlcUnitsPollutants = "ug/m3"\n' \
               'FlcUnitsIsotopes = "Bq/m3"\n' \
               'FlcCalcToxicResponse = 0\n' \
               'FlcToxicExp = 1.0\n' \
               'FlcCalcPercentiles = 0\n' \
               'FlcNumPercentiles = 0\n' \
               'FlcCalcPDF = 0\n' \
               'FlcPDFMode = 0\n' \
               'FlcNumPDF = 0\n' \
               '/\n\n'

    @staticmethod
    def get_default_adms_grd_str():
        return '&ADMS_PARAMETERS_GRD\n' \
               'GrdType = 0\n' \
               'GrdCoordSysType = 0\n' \
               'GrdSpacingType = 0\n' \
               'GrdRegularMin = \n' \
               '  0.0e+0 0.0e+0 0.0e+0 1.0e+1 \n' \
               '  0.0e+0 0.0e+0 \n' \
               'GrdRegularMax = \n' \
               '  0.0e+0 0.0e+0 3.0e+1 1.0e+3 \n' \
               '  3.30e+2 0.0e+0 \n' \
               'GrdRegularNumPoints = \n' \
               '  80 80 4 10 \n' \
               '  12 1 \n' \
               'GrdVarSpaceNumPointsX = 0\n' \
               'GrdVarSpaceNumPointsY = 0\n' \
               'GrdVarSpaceNumPointsZ = 0\n' \
               'GrdVarSpaceNumPointsR = 0\n' \
               'GrdVarSpaceNumPointsTh = 0\n' \
               'GrdVarSpaceNumPointsZp = 0\n' \
               'GrdPtsNumPoints = \n' \
               '  0 0 \n' \
               'GrdPolarCentreX = 0.0\n' \
               'GrdPolarCentreY = 0.0\n' \
               'GrdPtsUsePointsFile = 1\n' \
               'GrdPtsPointsFilePath = " "\n' \
               '/\n\n'

    @staticmethod
    def get_default_adms_puf_str():
        return '&ADMS_PARAMETERS_PUF\n' \
               'PufStart = 100.0\n' \
               'PufStep = 100.0\n' \
               'PufNumSteps = 10\n' \
               '/\n\n'

    @staticmethod
    def get_default_adms_gam_str():
        return '&ADMS_PARAMETERS_GAM\n' \
               'GamCalcDose = 0\n' \
               '/\n\n' \

    @staticmethod
    def get_default_adms_opt_str():
        return '&ADMS_PARAMETERS_OPT\n' \
               'OptNumOutputs = 8\n' \
               'OptPolName = \n  \n' \
               'OptInclude = \n  \n' \
               'OptShortOrLong = \n  \n' \
               'OptSamplingTime = \n  \n' \
               'OptSamplingTimeUnits = \n  \n' \
               'OptCondition = \n  \n' \
               'OptNumPercentiles = \n  \n' \
               'OptNumExceedences = \n  \n' \
               'OptPercentiles = \n  \n' \
               'OptExceedences = \n  \n' \
               'OptUnits = \n  \n' \
               'OptGroupsOrSource = 0\n' \
               'OptAllSources = 0\n' \
               'OptNumGroups = 1\n' \
               'OptIncludedGroups = \n  \n' \
               'OptIncludedSource = ""\n' \
               'OptCreateComprehensiveFile = 0\n' \
               '/\n\n'

    @staticmethod
    def get_default_adms_chm_str():
        return '&ADMS_PARAMETERS_CHM\n' \
               'ChmScheme = 2\n' \
               '/\n\n'

    @staticmethod
    def get_default_adms_bkg_str():
        return '&ADMS_PARAMETERS_BKG\n' \
               'BkgFilePath = " "\n' \
               'BkgFixedLevels = 0\n' \
               '/\n\n'

    @staticmethod
    def get_default_adms_etc_str():
        return '&ADMS_PARAMETERS_ETC\n' \
               'SrcNumSources = 1\n' \
               'PolNumPollutants = 19\n' \
               'PolNumIsotopes = 0\n' \
               '/\n\n'

    @staticmethod
    def get_default_adms_coords_str():
        return '&ADMS_COORDINATESYSTEM\n' \
               'ProjectedEPSG = 2\n' \
               '/\n\n'

    @staticmethod
    def get_default_adms_map_str():
        return '&ADMS_MAPPERPROJECT\n' \
               'ProjectFilePath = " "\n' \
               '/\n\n'

    @staticmethod
    def get_default_adms_pold_str():
        return '&ADMS_POLLUTANT_DETAILS\n' \
               'PolName = ""\n' \
               'PolPollutantType = 0\n' \
               'PolGasDepVelocityKnown = 0\n' \
               'PolGasDepositionVelocity = 0\n' \
               'PolGasType = 0\n' \
               'PolParDepVelocityKnown = 1\n' \
               'PolParTermVelocityKnown = 1\n' \
               'PolParNumDepositionData = 1\n' \
               'PolParDepositionVelocity = \n' \
               '  0.0e+0 \n' \
               'PolParTerminalVelocity = \n' \
               '  0.0e+0 \n' \
               'PolParDiameter = \n  \n' \
               'PolParDensity = \n' \
               '  1.0e+3 \n' \
               'PolParMassFraction = \n' \
               '  1.0e+0 \n' \
               'PolWetWashoutKnown = 0\n' \
               'PolWetWashout = 0\n' \
               'PolWetWashoutA = 0.0001\n' \
               'PolWetWashoutB = 0.64\n' \
               'PolConvFactor = 0\n' \
               'PolBkgLevel = 0\n' \
               'PolBkgUnits = ""\n' \
               '/\n\n'

    @staticmethod
    def get_default_adms_src_str():
        return '&ADMS_SOURCE_DETAILS\n' \
               'SrcName = ""\n' \
               'SrcMainBuilding = ""\n' \
               'SrcHeight = 0\n' \
               'SrcDiameter = 0\n' \
               'SrcVolFlowRate = 0\n' \
               'SrcVertVeloc = 0\n' \
               'SrcTemperature = 0\n' \
               'SrcMolWeight = 0\n' \
               'SrcDensity = 0\n' \
               'SrcSpecHeatCap = 0\n' \
               'SrcSourceType = 0\n' \
               'SrcReleaseAtNTP = 0\n' \
               'SrcEffluxType = 0\n' \
               'SrcBuoyancyType = 0\n' \
               'SrcPercentNOxAsNO2 = 0\n' \
               'SrcX1 = 0\n' \
               'SrcY1 = 0\n' \
               'SrcL1 = 0\n' \
               'SrcL2 = 0\n' \
               'SrcFm = 0\n' \
               'SrcFb = 0\n' \
               'SrcMassFlux = 0\n' \
               'SrcAngle1 = 0\n' \
               'SrcAngle2 = 0\n' \
               'SrcMassH2O = 0\n' \
               'SrcUseVARFile = 0\n' \
               'SrcNumGroups = 0\n' \
               'SrcGroup = \n  \n' \
               'SrcNumVertices = 0\n' \
               'SrcTraNumTrafficFlows = 0\n' \
               'SrcNumPollutants = 0\n' \
               'SrcPollutants = \n  \n' \
               'SrcPolEmissionRate = \n  \n' \
               'SrcPolTotalemission = \n  \n' \
               'SrcPolStartTime = \n  \n' \
               'SrcPolDuration = \n  \n' \
               'SrcNumIsotopes = 0\n' \
               '/\n\n'
