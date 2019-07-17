from adms_apl import *


class AplDirector(object):
    __builder = None

    def set_builder(self, builder):
        self.__builder = builder

    def get_apl(self):
        apl = Apl()

        header = self.__builder.get_header()
        sup = self.__builder.get_sup()
        met = self.__builder.get_met()
        hil = self.__builder.get_hil()
        cst = self.__builder.get_cst()
        flc = self.__builder.get_flc()
        grd = self.__builder.get_grd()
        puf = self.__builder.get_puf()
        gam = self.__builder.get_gam()
        bkg = self.__builder.get_bkg()
        chm = self.__builder.get_chm()
        etc = self.__builder.get_etc()
        mapper = self.__builder.get_mapper()

        apl.set_header(header)
        apl.set_sup(sup)
        apl.set_met(met)
        apl.set_hil(hil)
        apl.set_cst(cst)
        apl.set_flc(flc)
        apl.set_grd(grd)
        apl.set_puf(puf)
        apl.set_gam(gam)
        apl.set_bkg(bkg)
        apl.set_chm(chm)
        apl.set_etc(etc)
        apl.set_mapper(mapper)

        return apl


class AplBuilder(object):
    def get_header(self): pass

    def get_sup(self): pass

    def get_met(self): pass

    def get_hil(self): pass

    def get_cst(self): pass

    def get_flc(self): pass

    def get_grd(self): pass

    def get_puf(self): pass

    def get_gam(self): pass

    def get_bkg(self): pass

    def get_chm(self): pass

    def get_etc(self): pass

    def get_mapper(self): pass


class AdmsAplShipBuilder(AplBuilder):
    def get_header(self):
        header = AdmsHeader()
        return header

    def get_sup(self):
        sup = AdmsSup()
        return sup

    def get_met(self):
        met = AdmsMet()
        return met

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
