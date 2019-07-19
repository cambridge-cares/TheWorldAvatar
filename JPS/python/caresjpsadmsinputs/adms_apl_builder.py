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

        return apl


class AplBuilder(object):
    def __init__(self, data):
        self.data = data
        self.pollutant_names = [Constants.POL_CO2,  Constants.POL_CO2, Constants.POL_NOX, Constants.POL_NO2,
                                Constants.POL_NO, Constants.POL_PART_O3, Constants.POL_VOC, Constants.POL_PART_SO2,
                                Constants.POL_PM10, Constants.POL_PM25, Constants.POL_CO, Constants.POL_BENZENE,
                                Constants.POL_BUTADIENE, Constants.POL_HCl, Constants.POL_Cl2, Constants.POL_CH3Cl,
                                Constants.POL_ISOBUTYLENE, Constants.POL_NH3, Constants.POL_HC]

    def get_header(self): pass

    def get_sup(self): pass

    def get_met(self): pass

    def get_bld(self): pass

    def get_hil(self): pass

    def get_cst(self): pass

    def get_flc(self): pass

    def get_grd(self): pass

    def get_puf(self): pass

    def get_gam(self): pass

    def get_opt(self): pass

    def get_bkg(self): pass

    def get_chm(self): pass

    def get_etc(self): pass

    def get_coordsys(self): pass

    def get_mapper(self): pass

    def get_pollutants(self):
        pollutants = []
        for pname in self.pollutant_names:
            pollutant = AdmsPold()
            pollutant.PolName = pname
            pollutant.PolPollutantType = self.get_pollutant_type(pname)
            pollutant.PolGasDepVelocityKnown = self.get_pollutant_gas_dep_velocity_known(pname)
            pollutant.PolGasDepositionVelocity = self.get_pollutant_gas_dep_velocity(pname)
            pollutant.PolGasType = self.get_pollutant_gas_type(pname)
            pollutant.PolParDepVelocityKnown = self.get_pol_par_dep_velocity_known(pname)
            pollutant.PolParTermVelocityKnown = self.get_pol_par_term_velocity_known(pname)
            pollutant.PolParDiameter = self.get_pol_par_diameter(pname)
            pollutant.PolWetWashoutKnown = self.get_pol_wet_washout_known(pname)
            pollutant.PolWetWashout = self.get_pol_wet_washout(pname)

            pollutants.append(pollutant)

        return pollutants

    def get_pollutant_type(self, name):
        type = 0
        type_1 = [Constants.POL_PM10, Constants.POL_PM25]
        if name in type_1:
            type = 1

        return type

    def get_pollutant_gas_dep_velocity_known(self, name):
        flag = 1
        type_0 = [Constants.POL_Cl2, Constants.POL_CH3Cl, Constants.POL_ISOBUTYLENE, Constants.POL_NH3,
                     Constants.POL_HC, Constants.POL_NOx]
        if name in type_0:
            flag = 0

        return flag

    def get_pollutant_gas_dep_velocity(self, name):
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

    def get_pollutant_gas_type(self, name):
        type = 1
        type_0 = [Constants.POL_HCl, Constants.POL_Cl2, Constants.POL_CH3Cl, Constants.POL_ISOBUTYLENE,
                  Constants.POL_NH3, Constants.POL_HC]
        if name in type_0:
            type = 0

        return type

    def get_pol_par_dep_velocity_known(self, name):
        flag = 1
        type_0 = [Constants.POL_PM10, Constants.POL_PM25]
        if name in type_0:
            flag = 0

        return flag

    def get_pol_par_term_velocity_known(self, name):
        flag = 1
        type_0 = [Constants.POL_PM10, Constants.POL_PM25]
        if name in type_0:
            flag = 0

        return flag

    def get_pol_par_diameter(self, name):
        value = 1.0e-6
        if name == Constants.POL_PM10:
            value = 1.0e-5
        elif name == Constants.POL_PM25:
            value = 2.5e-6

        return value

    def get_pol_wet_washout_known(self, name):
        flag = 0
        type_1 = [Constants.NO, Constants.O3, Constants.SO2, Constants.PM10, Constants.BENZENE, Constants.BUTADIENE,
                  Constants.Cl2, Constants.CH3Cl, Constants.ISOBUTYLENE, Constants.HC]
        if name in type_1:
            flag = 1

        return flag

    def get_pol_wet_washout(self, name):
        value = 0.0e+0
        type_1 = [Constants.Cl2, Constants.CH3Cl, Constants.ISOBUTYLENE, Constants.NH3, Constants.HC]
        if name in type_1:
            value = 1.0e-4

        return value



class AdmsAplShipBuilder(AplBuilder):
    def get_header(self):
        header = AdmsHeader()
        return header

    def get_sup(self):
        sup = AdmsSup()
        sup.SupModelComplexTerrain = str(self.data[Constants.KEY_INDICATOR_TERR])
        sup.SupCalcChm = str(self.data[Constants.KEY_INDICATOR_CHEM])
        sup.SupUseAddInput = str(self.data[Constants.KEY_NIGHT])
        sup.SupAddInputPath = self.data[Constants.KEY_DIR_NIGHT]
        sup.SupCalcWetDep = str(self.data[Constants.KEY_INDICATOR_WET])
        return sup

    def get_met(self):
        met = AdmsMet()
        met.MetDataFileWellFormedPath = self.data[Constants.KEY_MET]
        met.MetLatitude = self.data[Constants.KEY_LAT.title()]
        return met

    def get_bld(self):
        bld = AmdsBld()
        bld_data = self.data[Constants.KEY_BDN]
        for field in bld_data._fields:
            setattr(bld, field, getattr(bld_data, field))
        return bld

    def get_hil(self):
        hil = AdmsHil()
        hil.HilTerrainPath = Constants.FILEPATH_HIL
        return hil

    def get_cst(self):
        cst = AdmsCst()
        return cst

    def get_flc(self):
        flc = AdmsFlc()
        return flc

    def get_grd(self):
        grd = AdmsGrd()
        data_grd = self.data[Constants.KEY_GRD]
        grd.GrdRegularMin[0] = data_grd[0]
        grd.GrdRegularMin[1] = data_grd[1]
        grd.GrdRegularMax[0] = data_grd[2]
        grd.GrdRegularMax[1] = data_grd[3]
        return grd

    def get_puf(self):
        puf = AdmsPuf()
        return puf

    def get_gam(self):
        gam = AdmsGam()
        return gam

    def get_opt(self):
        opt = AmdsOpt()
        opt_data = self.data[Constants.KEY_OPT]
        for field in opt_data._fields:
            setattr(opt, field, getattr(opt_data, field))
        return opt

    def get_bkg(self):
        bkg = AdmsBkg()
        bkg.BkgFilePath = self.data[Constants.KEY_BKG]
        return bkg

    def get_chm(self):
        chm = AdmsChm()
        return chm

    def get_etc(self):
        etc = AdmsEtc()
        etc.SrcNumSources = len(self.data[Constants.KEY_SRC])
        return etc

    def get_coordsys(self):
        coordsys = AdmsCoordSys()
        coordsys.ProjectedEPSG = self.data[Constants.KEY_COORD_SYS]
        return coordsys

    def get_mapper(self):
        mapper = AdmsMapper()
        return mapper


class AdmsAplPlantBuilder(AplBuilder):
    def get_header(self):
        header = AdmsHeader()
        return header

    def get_sup(self):
        sup = AdmsSup()
        return sup

    def get_met(self):
        met = AdmsMet()
        return met

    def get_bld(self):
        bld = AmdsBld()
        return bld

    def get_hil(self):
        hil = AdmsHil()
        return hil

    def get_cst(self):
        cst = AdmsCst()
        return cst

    def get_flc(self):
        flc = AdmsFlc()
        return flc

    def get_grd(self):
        grd = AdmsGrd()
        return grd

    def get_puf(self):
        puf = AdmsPuf()
        return puf

    def get_gam(self):
        gam = AdmsGam()
        return gam

    def get_bkg(self):
        bkg = AdmsBkg()
        return bkg

    def get_chm(self):
        chm = AdmsChm()
        return chm

    def get_etc(self):
        etc = AdmsEtc()
        return etc

    def get_mapper(self):
        mapper = AdmsMapper()
        return mapper



'''
    PolWetWashout      = 0.0e+0
'''

'''
    PolName                  = "CO2"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 1
    PolGasDepositionVelocity = 0.0e+0
    PolGasType               = 1
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 0
    PolWetWashout      = 0.0e+0
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 5.47e-1
    PolBkgLevel        = 4.14e+5
    PolBkgUnits        = "ppb"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "NOx"
    PolGasDepVelocityKnown   = 0
    PolGasDepositionVelocity = 0.0e+0
    PolGasType               = 1
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 0
    PolWetWashout      = 0.0e+0
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 5.2e-1
    PolBkgLevel        = 6.0e+1
    PolBkgUnits        = "ppb"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "NO2"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 1
    PolGasDepositionVelocity = 1.5e-3
    PolGasType               = 1
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 0
    PolWetWashout      = 0.0e+0
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 5.2e-1
    PolBkgLevel        = 4.41e+1
    PolBkgUnits        = "ppb"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "NO"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 1
    PolGasDepositionVelocity = 1.5e-3
    PolGasType               = 1
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 1
    PolWetWashout      = 0.0e+0
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 8.0e-1
    PolBkgLevel        = 0.0e+0
    PolBkgUnits        = "ppb"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "O3"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 1
    PolGasDepositionVelocity = 0.0e+0
    PolGasType               = 1
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 1
    PolWetWashout      = 0.0e+0
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 5.0e-1
    PolBkgLevel        = 6.899e+1
    PolBkgUnits        = "ppb"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "VOC"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 1
    PolGasDepositionVelocity = 0.0e+0
    PolGasType               = 1
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 0
    PolWetWashout      = 0.0e+0
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 3.1e-1
    PolBkgLevel        = 0.0e+0
    PolBkgUnits        = "ppb"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "SO2"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 1
    PolGasDepositionVelocity = 1.2e-2
    PolGasType               = 1
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 1
    PolWetWashout      = {0}
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 3.7e-1
    PolBkgLevel        = 1.513e+1
    PolBkgUnits        = "ppb"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "PM10"
    PolPollutantType         = 1
    PolGasDepVelocityKnown   = 1
    PolGasDepositionVelocity = 0.0e+0
    PolGasType               = 1
    PolParDepVelocityKnown   = 0
    PolParTermVelocityKnown  = 0
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-5
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 1
    PolWetWashout      = {1}
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 1.0e+0
    PolBkgLevel        = 5.63e+1
    PolBkgUnits        = "ug/m3"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "PM2.5"
    PolPollutantType         = 1
    PolGasDepVelocityKnown   = 1
    PolGasDepositionVelocity = 0.0e+0
    PolGasType               = 1
    PolParDepVelocityKnown   = 0
    PolParTermVelocityKnown  = 0
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      2.5e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 0
    PolWetWashout      = 0.0e+0
    PolWetWashoutA     = 3.552e-1
    PolWetWashoutB     = 5.394e-1
    PolConvFactor      = 1.0e+0
    PolBkgLevel        = 8.0e+0
    PolBkgUnits        = "ug/m3"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "CO"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 1
    PolGasDepositionVelocity = 0.0e+0
    PolGasType               = 1
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 0
    PolWetWashout      = 0.0e+0
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 8.6e-1
    PolBkgLevel        = 1.222e+3
    PolBkgUnits        = "ppb"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "BENZENE"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 1
    PolGasDepositionVelocity = 0.0e+0
    PolGasType               = 1
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 1
    PolWetWashout      = 0.0e+0
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 3.1e-1
    PolBkgLevel        = 0.0e+0
    PolBkgUnits        = "ppb"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "BUTADIENE"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 1
    PolGasDepositionVelocity = 0.0e+0
    PolGasType               = 1
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 1
    PolWetWashout      = 0.0e+0
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 4.5e-1
    PolBkgLevel        = 0.0e+0
    PolBkgUnits        = "ppb"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "HCl"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 1
    PolGasDepositionVelocity = 0.0e+0
    PolGasType               = 0
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 0
    PolWetWashout      = 0.0e+0
    PolWetWashoutA     = 3.0e-4
    PolWetWashoutB     = 6.6e-1
    PolConvFactor      = 6.589e-1
    PolBkgLevel        = 0.0e+0
    PolBkgUnits        = "ppb"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "Cl2"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 0
    PolGasDepositionVelocity = 5.0e+0
    PolGasType               = 0
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 1
    PolWetWashout      = 1.0e-4
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 3.5e-1
    PolBkgLevel        = 0.0e+0
    PolBkgUnits        = "ppb"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "CH3Cl"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 0
    PolGasDepositionVelocity = 0.0e+0
    PolGasType               = 0
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 1
    PolWetWashout      = 1.0e-4
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 4.922e-1
    PolBkgLevel        = 6.0e-1
    PolBkgUnits        = "ppb"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "ISOBUTYLENE"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 0
    PolGasDepositionVelocity = 0.0e+0
    PolGasType               = 0
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 1
    PolWetWashout      = 1.0e-4
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 4.43e-1
    PolBkgLevel        = 0.0e+0
    PolBkgUnits        = "ppb"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "NH3"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 0
    PolGasDepositionVelocity = 0.0e+0
    PolGasType               = 0
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 0
    PolWetWashout      = 1.0e-4
    PolWetWashoutA     = 5.0e-3
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 1.462e+0
    PolBkgLevel        = 6.0e+0
    PolBkgUnits        = "ppb"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "HC"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 0
    PolGasDepositionVelocity = 0.0e+0
    PolGasType               = 0
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 1
    PolWetWashout      = 1.0e-4
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 0.802e+0
    PolBkgLevel        = 0.0e+0
    PolBkgUnits        = "ppb"
    /
'''











































'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "CO2"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 1
    PolGasDepositionVelocity = 0.0e+0
    PolGasType               = 1
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 0
    PolWetWashout      = 0.0e+0
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 5.47e-1
    PolBkgLevel        = 4.14e+5
    PolBkgUnits        = "ppb"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "NOx"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 0
    PolGasDepositionVelocity = 0.0e+0
    PolGasType               = 1
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 0
    PolWetWashout      = 0.0e+0
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 5.2e-1
    PolBkgLevel        = 6.0e+1
    PolBkgUnits        = "ppb"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "NO2"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 1
    PolGasDepositionVelocity = 1.5e-3
    PolGasType               = 1
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 0
    PolWetWashout      = 0.0e+0
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 5.2e-1
    PolBkgLevel        = 4.41e+1
    PolBkgUnits        = "ppb"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "NO"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 1
    PolGasDepositionVelocity = 1.5e-3
    PolGasType               = 1
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 1
    PolWetWashout      = 0.0e+0
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 8.0e-1
    PolBkgLevel        = 0.0e+0
    PolBkgUnits        = "ppb"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "O3"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 1
    PolGasDepositionVelocity = 0.0e+0
    PolGasType               = 1
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 1
    PolWetWashout      = 0.0e+0
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 5.0e-1
    PolBkgLevel        = 6.899e+1
    PolBkgUnits        = "ppb"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "VOC"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 1
    PolGasDepositionVelocity = 0.0e+0
    PolGasType               = 1
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 0
    PolWetWashout      = 0.0e+0
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 3.1e-1
    PolBkgLevel        = 0.0e+0
    PolBkgUnits        = "ppb"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "SO2"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 1
    PolGasDepositionVelocity = 1.2e-2
    PolGasType               = 1
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 1
    PolWetWashout      = {0}
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 3.7e-1
    PolBkgLevel        = 1.513e+1
    PolBkgUnits        = "ppb"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "PM10"
    PolPollutantType         = 1
    PolGasDepVelocityKnown   = 1
    PolGasDepositionVelocity = 0.0e+0
    PolGasType               = 1
    PolParDepVelocityKnown   = 0
    PolParTermVelocityKnown  = 0
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-5
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 1
    PolWetWashout      = {1}
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 1.0e+0
    PolBkgLevel        = 5.63e+1
    PolBkgUnits        = "ug/m3"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "PM2.5"
    PolPollutantType         = 1
    PolGasDepVelocityKnown   = 1
    PolGasDepositionVelocity = 0.0e+0
    PolGasType               = 1
    PolParDepVelocityKnown   = 0
    PolParTermVelocityKnown  = 0
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      2.5e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 0
    PolWetWashout      = 0.0e+0
    PolWetWashoutA     = 3.552e-1
    PolWetWashoutB     = 5.394e-1
    PolConvFactor      = 1.0e+0
    PolBkgLevel        = 8.0e+0
    PolBkgUnits        = "ug/m3"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "CO"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 1
    PolGasDepositionVelocity = 0.0e+0
    PolGasType               = 1
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 0
    PolWetWashout      = 0.0e+0
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 8.6e-1
    PolBkgLevel        = 1.222e+3
    PolBkgUnits        = "ppb"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "BENZENE"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 1
    PolGasDepositionVelocity = 0.0e+0
    PolGasType               = 1
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 1
    PolWetWashout      = 0.0e+0
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 3.1e-1
    PolBkgLevel        = 0.0e+0
    PolBkgUnits        = "ppb"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "BUTADIENE"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 1
    PolGasDepositionVelocity = 0.0e+0
    PolGasType               = 1
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 1
    PolWetWashout      = 0.0e+0
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 4.5e-1
    PolBkgLevel        = 0.0e+0
    PolBkgUnits        = "ppb"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "HCl"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 1
    PolGasDepositionVelocity = 0.0e+0
    PolGasType               = 0
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 0
    PolWetWashout      = 0.0e+0
    PolWetWashoutA     = 3.0e-4
    PolWetWashoutB     = 6.6e-1
    PolConvFactor      = 6.589e-1
    PolBkgLevel        = 0.0e+0
    PolBkgUnits        = "ppb"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "Cl2"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 0
    PolGasDepositionVelocity = 5.0e+0
    PolGasType               = 0
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 1
    PolWetWashout      = 1.0e-4
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 3.5e-1
    PolBkgLevel        = 0.0e+0
    PolBkgUnits        = "ppb"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "CH3Cl"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 0
    PolGasDepositionVelocity = 0.0e+0
    PolGasType               = 0
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 1
    PolWetWashout      = 1.0e-4
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 4.922e-1
    PolBkgLevel        = 6.0e-1
    PolBkgUnits        = "ppb"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "ISOBUTYLENE"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 0
    PolGasDepositionVelocity = 0.0e+0
    PolGasType               = 0
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 1
    PolWetWashout      = 1.0e-4
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 4.43e-1
    PolBkgLevel        = 0.0e+0
    PolBkgUnits        = "ppb"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "NH3"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 0
    PolGasDepositionVelocity = 0.0e+0
    PolGasType               = 0
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 0
    PolWetWashout      = 1.0e-4
    PolWetWashoutA     = 5.0e-3
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 1.462e+0
    PolBkgLevel        = 6.0e+0
    PolBkgUnits        = "ppb"
    /
'''
'''
    &ADMS_POLLUTANT_DETAILS
    PolName                  = "HC"
    PolPollutantType         = 0
    PolGasDepVelocityKnown   = 0
    PolGasDepositionVelocity = 0.0e+0
    PolGasType               = 0
    PolParDepVelocityKnown   = 1
    PolParTermVelocityKnown  = 1
    PolParNumDepositionData  = 1
    PolParDepositionVelocity =
      0.0e+0
    PolParTerminalVelocity =
      0.0e+0
    PolParDiameter =
      1.0e-6
    PolParDensity =
      1.000e+3
    PolParMassFraction =
      1.0e+0
    PolWetWashoutKnown = 1
    PolWetWashout      = 1.0e-4
    PolWetWashoutA     = 1.0e-4
    PolWetWashoutB     = 6.4e-1
    PolConvFactor      = 0.802e+0
    PolBkgLevel        = 0.0e+0
    PolBkgUnits        = "ppb"
    /
'''