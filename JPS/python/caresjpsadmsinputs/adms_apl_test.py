import unittest

from adms_apl import Apl


class AplTest(unittest.TestCase):

    def test_init(self):
        self.apl = Apl()
        self.assertIsNone(self.apl._Apl__header)
        self.assertIsNone(self.apl._Apl__sup)
        self.assertIsNone(self.apl._Apl__met)
        self.assertIsNone(self.apl._Apl__bld)
        self.assertIsNone(self.apl._Apl__hil)
        self.assertIsNone(self.apl._Apl__cst)
        self.assertIsNone(self.apl._Apl__flc)
        self.assertIsNone(self.apl._Apl__grd)
        self.assertIsNone(self.apl._Apl__puf)
        self.assertIsNone(self.apl._Apl__gam)
        self.assertIsNone(self.apl._Apl__opt)
        self.assertIsNone(self.apl._Apl__bkg)
        self.assertIsNone(self.apl._Apl__etc)
        self.assertIsNone(self.apl._Apl__chm)
        self.assertIsNone(self.apl._Apl__coordsys)
        self.assertIsNone(self.apl._Apl__mapper)
        self.assertIsNone(self.apl._Apl__pollutants)
        self.assertIsNone(self.apl._Apl__sources)


class AplPartTest(unittest.TestCase):
    pass

class AdmsHeaderTest(unittest.TestCase):
    pass

class AdmsSupTest(unittest.TestCase):
    pass

class AdmsMetTest(unittest.TestCase):
    pass

class AdmsHilTest(unittest.TestCase):
    pass

class AdmsCstTest(unittest.TestCase):
    pass

class AdmsFlcTest(unittest.TestCase):
    pass

class AdmsGrdTest(unittest.TestCase):
    pass

class AdmsPufTest(unittest.TestCase):
    pass

class AdmsGamTest(unittest.TestCase):
    pass

class AdmsBkgTest(unittest.TestCase):
    pass

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