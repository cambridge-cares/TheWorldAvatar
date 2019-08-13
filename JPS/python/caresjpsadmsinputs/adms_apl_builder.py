from adms_apl import *
from config import Constants


class AplDirector(object):
    __builder = None

    def set_builder(self, builder):
        self.__builder = builder

    def get_apl(self):
        apl = Apl()

        header = self.__builder.get_header()
        sup = self.__builder.get_sup()
        met = self.__builder.get_met()
        bld = self.__builder.get_bld()
        hil = self.__builder.get_hil()
        cst = self.__builder.get_cst()
        flc = self.__builder.get_flc()
        grd = self.__builder.get_grd()
        puf = self.__builder.get_puf()
        gam = self.__builder.get_gam()
        opt = self.__builder.get_opt()
        bkg = self.__builder.get_bkg()
        chm = self.__builder.get_chm()
        etc = self.__builder.get_etc()
        coordsys = self.__builder.get_coordsys()
        mapper = self.__builder.get_mapper()
        pollutants = self.__builder.get_pollutants()
        sources = self.__builder.get_sources()

        apl.set_header(header)
        apl.set_sup(sup)
        apl.set_met(met)
        apl.set_bld(bld)
        apl.set_hil(hil)
        apl.set_cst(cst)
        apl.set_flc(flc)
        apl.set_grd(grd)
        apl.set_puf(puf)
        apl.set_gam(gam)
        apl.set_opt(opt)
        apl.set_bkg(bkg)
        apl.set_chm(chm)
        apl.set_etc(etc)
        apl.set_coordsys(coordsys)
        apl.set_mapper(mapper)
        apl.set_pollutants(pollutants)
        apl.set_sources(sources)

        return apl


class AplBuilder(object):
    def __init__(self, data):
        self.data = data
        self.pollutant_names = [Constants.POL_CO2, Constants.POL_NOX, Constants.POL_NO2,
                                Constants.POL_NO, Constants.POL_PART_O3, Constants.POL_VOC, Constants.POL_PART_SO2,
                                Constants.POL_PM10, Constants.POL_PM25, Constants.POL_CO, Constants.POL_BENZENE,
                                Constants.POL_BUTADIENE, Constants.POL_HCl, Constants.POL_Cl2, Constants.POL_CH3Cl,
                                Constants.POL_ISOBUTYLENE, Constants.POL_NH3, Constants.POL_HC]

    @staticmethod
    def get_header():
        header = AdmsHeader()
        return header

    def get_sup(self):
        sup = AdmsSup()
        return sup

    def get_met(self):
        met = AdmsMet()
        return met

    def get_bld(self):
        bld = AdmsBld()
        bld_data = self.data[Constants.KEY_BDN]
        for field in bld_data._fields:
            setattr(bld, field, getattr(bld_data, field))
        return bld

    def get_hil(self):
        hil = AdmsHil()
        return hil

    @staticmethod
    def get_cst():
        cst = AdmsCst()
        return cst

    @staticmethod
    def get_flc():
        flc = AdmsFlc()
        return flc

    def get_grd(self):
        grd = AdmsGrd()
        data_grd = self.data[Constants.KEY_GRD]
        grd.GrdRegularMin[0] = data_grd[0]
        grd.GrdRegularMin[1] = data_grd[1]
        grd.GrdRegularMax[0] = data_grd[2]
        grd.GrdRegularMax[1] = data_grd[3]
        grd.GrdRegularNumPoints[0] = self.data[Constants.GRD_X]
        grd.GrdRegularNumPoints[1] = self.data[Constants.GRD_Y]

        return grd

    @staticmethod
    def get_puf():
        puf = AdmsPuf()
        return puf

    @staticmethod
    def get_gam():
        gam = AdmsGam()
        return gam

    def get_opt(self):
        opt = AdmsOpt()
        opt_data = self.data[Constants.KEY_OPT]
        for field in opt_data._fields:
            setattr(opt, field, getattr(opt_data, field))
        return opt

    def get_bkg(self):
        bkg = AdmsBkg()
        return bkg

    @staticmethod
    def get_chm():
        chm = AdmsChm()
        return chm

    def get_etc(self):
        etc = AdmsEtc()
        return etc

    def get_coordsys(self):
        coordsys = AdmsCoordSys()
        coordsys.ProjectedEPSG = self.data[Constants.KEY_COORD_SYS]
        return coordsys

    @staticmethod
    def get_mapper():
        mapper = AdmsMapper()
        return mapper

    def get_pollutants(self):
        pollutants = []
        for pname in self.pollutant_names:
            pollutant = AdmsPold()
            pollutant.PolName = pname
            pollutant.PolPollutantType = self.get_pol_type(pname)
            pollutant.PolGasDepVelocityKnown = self.get_pol_gas_dep_velocity_known(pname)
            pollutant.PolGasDepositionVelocity = self.get_pol_gas_dep_velocity(pname)
            pollutant.PolGasType = self.get_pol_gas_type(pname)
            pollutant.PolParDepVelocityKnown = self.get_pol_par_dep_velocity_known(pname)
            pollutant.PolParTermVelocityKnown = self.get_pol_par_term_velocity_known(pname)
            pollutant.PolParDiameter = self.get_pol_par_diameter(pname)
            pollutant.PolWetWashoutKnown = self.get_pol_wet_washout_known(pname)
            pollutant.PolWetWashout = self.get_pol_wet_washout(pname)
            pollutant.PolWetWashoutA = self.get_pol_wet_washout_a(pname)
            pollutant.PolWetWashoutB = self.get_pol_wet_washout_b(pname)
            pollutant.PolConvFactor = self.get_pol_conv_factor(pname)
            pollutant.PolBkgLevel = self.get_pol_bkg_level(pname)
            pollutant.PolBkgUnits = self.get_pol_bkg_units(pname)

            pollutants.append(pollutant)

        return pollutants

    def get_sources(self):
        sources = []
        src_data = self.data[Constants.KEY_SRC]
        for s in src_data:
            source = AdmsSrc()
            for field in dir(s):
                if not field.startswith('_') and field not in [Constants.KEY_INDEX, Constants.KEY_COUNT]:
                    setattr(source, field, getattr(s, field))
            sources.append(source)

        return sources

    @staticmethod
    def get_pol_type(name):
        ty = 0
        ty_1 = [Constants.POL_PM10, Constants.POL_PM25]
        if name in ty_1:
            ty = 1

        return ty

    @staticmethod
    def get_pol_gas_dep_velocity_known(name):
        flag = 1
        type_0 = [Constants.POL_Cl2, Constants.POL_CH3Cl, Constants.POL_ISOBUTYLENE, Constants.POL_NH3,
                  Constants.POL_HC, Constants.POL_NOX]
        if name in type_0:
            flag = 0

        return flag

    @staticmethod
    def get_pol_gas_dep_velocity(name):
        value = 0.0e+0
        type_15 = [Constants.POL_NO2, Constants.POL_NO]
        type_12 = [Constants.POL_PART_SO2]
        type_50 = [Constants.POL_Cl2]
        if name in type_15:
            value = 1.5e-3
        elif name in type_12:
            value = 1.2e-2
        elif name in type_50:
            value = 5.0e+0

        return value

    @staticmethod
    def get_pol_gas_type(name):
        ty = 1
        ty_0 = [Constants.POL_HCl, Constants.POL_Cl2, Constants.POL_CH3Cl, Constants.POL_ISOBUTYLENE,
                Constants.POL_NH3, Constants.POL_HC]
        if name in ty_0:
            ty = 0

        return ty

    @staticmethod
    def get_pol_par_dep_velocity_known(name):
        flag = 1
        type_0 = [Constants.POL_PM10, Constants.POL_PM25]
        if name in type_0:
            flag = 0

        return flag

    @staticmethod
    def get_pol_par_term_velocity_known(name):
        flag = 1
        type_0 = [Constants.POL_PM10, Constants.POL_PM25]
        if name in type_0:
            flag = 0

        return flag

    @staticmethod
    def get_pol_par_diameter(name):
        value = 1.0e-6
        if name == Constants.POL_PM10:
            value = 1.0e-5
        elif name == Constants.POL_PM25:
            value = 2.5e-6

        return value

    @staticmethod
    def get_pol_wet_washout_known(name):
        flag = 0
        type_1 = [Constants.POL_NO, Constants.POL_PART_O3, Constants.POL_PART_SO2, Constants.POL_PM10,
                  Constants.POL_BENZENE, Constants.POL_BUTADIENE, Constants.POL_Cl2, Constants.POL_CH3Cl,
                  Constants.POL_ISOBUTYLENE, Constants.POL_HC]
        if name in type_1:
            flag = 1

        return flag

    @staticmethod
    def get_pol_wet_washout(name):
        value = 0.0e+0
        type_1 = [Constants.POL_Cl2, Constants.POL_CH3Cl, Constants.POL_ISOBUTYLENE, Constants.POL_NH3,
                  Constants.POL_HC]
        if name in type_1:
            value = 1.0e-4

        return value

    @staticmethod
    def get_pol_wet_washout_a(name):
        value = 1.0e-4
        if name == Constants.POL_PM25:
            value = 3.552e-1
        elif name == Constants.POL_HCl:
            value = 3.0e-4
        elif name == Constants.POL_NH3:
            value = 5.0e-3

        return value

    @staticmethod
    def get_pol_wet_washout_b(name):
        value = 6.4e-1
        if name == Constants.POL_PM25:
            value = 5.394e-1
        elif name == Constants.POL_HCl:
            value = 6.6e-1

        return value

    @staticmethod
    def get_pol_conv_factor(name):
        values = {Constants.POL_CO2: 5.47e-1, Constants.POL_NOX: 5.2e-1, Constants.POL_NO2: 5.2e-1,
                  Constants.POL_NO: 8.0e-1, Constants.POL_PART_O3: 5.0e-1, Constants.POL_VOC: 3.1e-1,
                  Constants.POL_PART_SO2: 3.7e-1, Constants.POL_PM10: 1.0e+0, Constants.POL_PM25: 1.0e+0,
                  Constants.POL_CO: 8.6e-1, Constants.POL_BENZENE: 3.1e-1, Constants.POL_BUTADIENE: 4.5e-1,
                  Constants.POL_HCl: 6.589e-1, Constants.POL_Cl2: 3.5e-1, Constants.POL_CH3Cl: 4.922e-1,
                  Constants.POL_ISOBUTYLENE: 4.43e-1, Constants.POL_NH3: 1.462e+0, Constants.POL_HC: 0.802e+0}
        value = values[name]

        return value

    @staticmethod
    def get_pol_bkg_level(name):
        values = {Constants.POL_CO2: 4.14e+5, Constants.POL_NOX: 6.0e+1, Constants.POL_NO2: 4.41e+1,
                  Constants.POL_NO: 0.0e+0, Constants.POL_PART_O3: 6.899e+1, Constants.POL_VOC: 0.0e+0,
                  Constants.POL_PART_SO2: 1.513e+1, Constants.POL_PM10: 5.63e+1, Constants.POL_PM25: 8.0e+0,
                  Constants.POL_CO: 1.222e+3, Constants.POL_BENZENE: 0.0e+0, Constants.POL_BUTADIENE: 0.0e+0,
                  Constants.POL_HCl: 0.0e+0, Constants.POL_Cl2: 0.0e+0, Constants.POL_CH3Cl: 6.0e-1,
                  Constants.POL_ISOBUTYLENE: 0.0e+0, Constants.POL_NH3: 6.0e+0, Constants.POL_HC: 0.0e+0}
        value = values[name]

        return value

    @staticmethod
    def get_pol_bkg_units(name):
        value = Constants.UNIT_PPB
        ugm3 = [Constants.POL_PM10, Constants.POL_PM25]
        if name in ugm3:
            value = Constants.UNIT_UGM3

        return value


class AdmsAplShipBuilder(AplBuilder):
    def get_sup(self):
        sup = AdmsSup()
        sup.SupModelComplexTerrain = self.data[Constants.KEY_INDICATOR_TERR]
        sup.SupCalcChm = self.data[Constants.KEY_INDICATOR_CHEM]
        sup.SupUseAddInput = self.data[Constants.KEY_NIGHT]
        sup.SupAddInputPath = self.data[Constants.KEY_DIR_NIGHT]
        sup.SupCalcWetDep = self.data[Constants.KEY_INDICATOR_WET]
        return sup

    def get_met(self):
        met = AdmsMet()
        met.MetDataFileWellFormedPath = self.data[Constants.KEY_MET]
        met.MetLatitude = self.data[Constants.KEY_LAT.title()]
        return met

    def get_hil(self):
        hil = AdmsHil()
        hil.HilTerrainPath = Constants.FILEPATH_HIL_HK
        return hil

    def get_bkg(self):
        bkg = AdmsBkg()
        bkg.BkgFilePath = self.data[Constants.KEY_BKG]
        return bkg

    def get_etc(self):
        etc = AdmsEtc()
        etc.SrcNumSources = len(self.data[Constants.KEY_SRC])
        return etc

    def get_pol_wet_washout(self, name):
        value = 0.0e+0
        type_1 = [Constants.POL_Cl2, Constants.POL_CH3Cl, Constants.POL_ISOBUTYLENE, Constants.POL_NH3,
                  Constants.POL_HC]
        if name in type_1:
            value = 1.0e-4
        elif name == Constants.POL_PART_SO2:
            value = self.data[Constants.KEY_WASHOUT_SO2]
        elif name == Constants.POL_PM10:
            value = self.data[Constants.KEY_WASHOUT_PM10]

        return value

    def get_pollutants(self):
        self.pollutant_names.remove(Constants.POL_PM10)
        self.pollutant_names.remove(Constants.POL_PM25)
        pollutants = super().get_pollutants()

        polls = self.data[Constants.KEY_POL]
        for pold_data in polls:
            pold = AdmsPold()
            for field in dir(pold_data):
                if not field.startswith('_') and field not in [Constants.KEY_INDEX, Constants.KEY_COUNT]:
                    setattr(pold, field, getattr(pold_data, field))
            pollutants.append(pold)

        return pollutants


class AdmsAplPlantBuilder(AplBuilder):
    def get_sup(self):
        sup = AdmsSup()
        sup.SupModelComplexTerrain = 0
        sup.SupCalcChm = 0
        sup.SupCalcWetDep = 0
        sup.SupCalcPlumeVisibility = 0
        return sup

    def get_met(self):
        met = AdmsMet()
        met.Met_DS_Roughness = 1.5e+0
        met.MetDataFileWellFormedPath = self.data[Constants.KEY_MET]
        met.MetLatitude = 1.09e+0
        return met

    def get_hil(self):
        hil = AdmsHil()
        hil.HilTerrainPath = Constants.FILEPATH_HIL_SG
        return hil

    def get_bkg(self):
        bkg = AdmsBkg()
        bkg.BkgFilePath = Constants.FILEPATH_HIL_BGD
        bkg.BkgFixedLevels = 1
        return bkg

    def get_etc(self):
        etc = AdmsEtc()
        etc.SrcNumSources = 1
        etc.PolNumPollutants = 18
        return etc

    def get_pol_wet_washout_known(self, name):
        flag = 0
        type_1 = [Constants.POL_NO, Constants.POL_PART_O3, Constants.POL_PART_SO2, Constants.POL_PM10,
                  Constants.POL_BENZENE, Constants.POL_BUTADIENE, Constants.POL_Cl2, Constants.POL_CH3Cl,
                  Constants.POL_ISOBUTYLENE, Constants.POL_HC, Constants.POL_NH3, Constants.POL_HCl, Constants.POL_CO,
                  Constants.POL_PM25, Constants.POL_NO2, Constants.POL_NOX, Constants.POL_CO2]
        if name in type_1:
            flag = 1

        return flag

    def get_pol_bkg_level(self, name):
        values = {Constants.POL_CO2: 0.0e+0, Constants.POL_NOX: 0.0e+0, Constants.POL_NO2: 0.0e+0,
                  Constants.POL_NO: 0.0e+0, Constants.POL_PART_O3: 0.0e+0, Constants.POL_VOC: 0.0e+0,
                  Constants.POL_PART_SO2: 0.0e+0, Constants.POL_PM10: 0.0e+0, Constants.POL_PM25: 0.0e+0,
                  Constants.POL_CO: 0.0e+0, Constants.POL_BENZENE: 0.0e+0, Constants.POL_BUTADIENE: 0.0e+0,
                  Constants.POL_HCl: 0.0e+0, Constants.POL_Cl2: 0.0e+0, Constants.POL_CH3Cl: 6.0e-1,
                  Constants.POL_ISOBUTYLENE: 0.0e+0, Constants.POL_NH3: 6.0e+0, Constants.POL_HC: 0.0e+0}
        value = values[name]

        return value

    def get_pol_gas_dep_velocity_known(self, name):
        flag = 1
        type_0 = [Constants.POL_Cl2, Constants.POL_CH3Cl, Constants.POL_ISOBUTYLENE, Constants.POL_NH3,
                  Constants.POL_HC]
        if name in type_0:
            flag = 0

        return flag

    def get_pol_gas_dep_velocity(self, name):
        value = 0.0e+0
        type_50 = [Constants.POL_Cl2]

        if name in type_50:
            value = 5.0e+0

        return value

    def get_pol_par_dep_velocity_known(self, name):
        return 1

    def get_pol_par_term_velocity_known(self, name):
        return 1

    def get_pol_wet_washout_a(self, name):
        return 1.0e-4

    def get_pol_wet_washout_b(self, name):
        return 6.4e-1
