
import unittest
import os

# from crystalinfo import *
import crystalinfo
import crystaldata
import tools

ontoCrystPrefix = "http://www.theworldavatar.com/kg/ontocrystal/"


class TestCrystalData(unittest.TestCase):
    def setUp(self):
        self.uuidDB = tools.UuidDB(filename="test_crystdata.csv")

    def test_split_cif_file(self):
        cd = crystaldata.CrystalData("ValAndErr", uuidDB=self.uuidDB,
                                     abox_prefix="abox")

        #file_in = os.path.join("ccdcfiles", "10.1021_ja9634841_Rb.cif")
        file_in = os.path.join("ccdcfiles", "10.1007_BF00624446_Rb.cif")
        file_in = os.path.join("ccdcfiles", "10.1107_S0108270194003537.cif")


        parts = cd._split_cif_to_parts(file_in)

        #print("parts of ", file_in, ":", parts)
        for line in parts["lines"]:
            #print(line)
            cd._parse_cif_line(line)
            #cd.
            pass
        for loop in parts["loops"]:
            #print(loop)
            pass

        self.fail("not implemented test")
        # === end of TestCrystalData.test_load_cif_data()

    def test_read_cif_data(self):

        cd = crystaldata.CrystalData("ValAndErr", uuidDB=self.uuidDB,
                                     abox_prefix="abox")

        #file_in = os.path.join("ccdcfiles", "10.1021_ja9634841_Rb.cif")
        #file_in = os.path.join("ccdcfiles", "10.1007_BF00624446_Rb.cif")
        file_in = os.path.join("ccdcfiles", "10.1107_S0108270194003537.cif")
        print("Starting", file_in)

        #print("Before read_cif:", len(cd.listAtomRaw))
        cd.read_cif_data(file_in)
        #print("tmp =", tmp)


        #for i in range(len(cd.listAtomRaw)):
        #    print("after read_cif_data:", cd.listAtomRaw[i].frac)

        #print(cd.unitCellLengths.comp_dict)
        self.assertEqual(cd.unitCellLengths.comp_dict["a"]["value"], "10.281")
        self.assertEqual(cd.unitCellLengths.comp_dict["a"]["error"], "0.002")
        self.assertEqual(cd.unitCellAngles.comp_dict["gamma"]["value"], "90")
        #self.assertEqual(cd.unitCellAngles.comp_dict["gamma"]["error"], "0.002")
        self.assertEqual(cd.unitCellVolume.value, "2428.72")  #.comp_dict["a"]["error"], "0.002")
        self.assertEqual(cd.symmITNumber, 61)  #.comp_dict["a"]["error"], "0.002")
        
        #print(">>>>>>>>>>>>> n frac coord", len(cd.listAtomRaw))
        a = cd.listAtomRaw[0]
        #print(">>>>>>>>>>>>> frac coord", cd.listAtomRaw[0].frac)
        self.assertEqual(a.frac,     ["0.3676", "-0.74672", "0.20387"])
        #self.assertEqual(a.frac_err, ["0.0001",  "0.00009", "0.00007"])
        self.assertEqual(float(a.frac_err[0]), float("0.0001"))
        self.assertEqual(float(a.frac_err[1]), float("0.00009"))
        self.assertEqual(float(a.frac_err[2]), float("0.00007"))
        self.assertEqual(a.element, "P")
        self.assertEqual(a.cif_label, "P1")

        a = cd.listAtomRaw[-1]
        self.assertEqual(a.frac,     ["0.311", "-0.2545", "0.0131"])
        #self.assertEqual(a.frac_err, ["0.001",  "0.0007", "0.0006"])
        self.assertEqual(float(a.frac_err[0]), float("0.001"))
        self.assertEqual(float(a.frac_err[1]), float("0.0007"))
        self.assertEqual(float(a.frac_err[2]), float("0.0006"))
        self.assertEqual(a.element, "H")
        self.assertEqual(a.cif_label, "H6")

        #print(cd.listAtomSymm)
        self.assertEqual(cd.listAtomSymm[0], "x,y,z")
        self.assertEqual(cd.listAtomSymm[-2], "1/2-x,1/2+y,-z")
        self.assertEqual(cd.listAtomSymm[-1], "x,1/2+y,1/2-z")

        #self.fail("not implemented test")
        # === end of TestCrystalData.()

    def test_atom_struct(self):

        cd = crystaldata.CrystalData("ValAndErr", uuidDB=self.uuidDB,
                                     abox_prefix="abox")

        #file_in = os.path.join("ccdcfiles", "10.1021_ja9634841_Rb.cif")
        #file_in = os.path.join("ccdcfiles", "10.1007_BF00624446_Rb.cif")
        #file_in = os.path.join("ccdcfiles", "10.1107_S0108270194003537.cif")
        #file_in = os.path.join("ccdcfiles", "10.1107_S0108270194000326.cif")
        #file_in = os.path.join("ccdcfiles", "279367.cif")
        #file_in = os.path.join("CIF", "ITV.cif")
        #file_in = os.path.join("CIF", "ABW.cif")
        #file_in = os.path.join("LI-CIF", "Li-ABW.cif")
        #file_in = os.path.join("LI-CIF", "Li-ABW-test.cif")
        #file_in = os.path.join("LI-CIF", "Li-BOG.cif")
        #file_in = os.path.join("LI-CIF", "Li-BPH.cif")
        file_in = os.path.join("LI-CIF", "Li-CON.cif")
        #file_in = os.path.join("LI-CIF", "Li-DON.cif")
        #file_in = os.path.join("LI-CIF", "Li-FAR.cif")
        file_in = os.path.join("LI-CIF", "Li-FRA.cif")
        file_in = os.path.join("LI-CIF", "Li-GIU.cif")
        #file_in = os.path.join("LI-CIF", "Li-JRY.cif")
        file_in = os.path.join("LI-CIF", "Li-JSR.cif")
        file_in = os.path.join("LI-CIF", "Li-MAR.cif")
        file_in = os.path.join("LI-CIF", "Li-MRT.cif")
        #file_in = os.path.join("LI-CIF", "Li-MSO.cif")
        file_in = os.path.join("LI-CIF", "Li-MWF.cif")
        #file_in = os.path.join("LI-CIF", "Li-OBW.cif")
        #file_in = os.path.join("LI-CIF", "Li-PON.cif")
        #file_in = os.path.join("LI-CIF", "Li-RSN.cif")
        file_in = os.path.join("LI-CIF", "Li-SFE.cif")
        file_in = os.path.join("LI-CIF", "Li-SFW.cif")
        file_in = os.path.join("LI-CIF", "Li-SOF.cif")
        #file_in = os.path.join("LI-CIF", "Li-STW.cif")
        #file_in = os.path.join("LI-CIF", "Li-TUN.cif")
        file_in = os.path.join("LI-CIF", "Li-UWY.cif")
        file_in = os.path.join("LI-CIF", "Li-VNI.cif")
        file_in = os.path.join("LI-CIF", "Li-YUG.cif")
        #file_in = os.path.join("LI-CIF", "Li-ZON.cif")
        #file_in = os.path.join("LI-CIF", "Li-ITV.cif")
        #file_in = os.path.join("LI-CIF", "Li-SVR.cif")
        #file_in = os.path.join("LI-CIF", "Li-BIK.cif")
        #file_in = os.path.join("LI-CIF", "Li-RWY.cif")

        file_in = os.path.join("LI-CIF", "Li-OSO.cif")
        #file_in = os.path.join("LI-CIF", "Li-UOV.cif")
        #file_in = os.path.join("LI-CIF", "Li-VSV.cif")
        #file_in = os.path.join("LI-CIF", "Li-LTA.cif")

        file_in = "ccdcfiles\\10.1107_S0108768192009005-CrAPO-1.cif"
        #file_in = "ccdcfiles\\10.1107_S0108768192009005-CrAPO-2.cif"

        print("Starting", file_in)

        cd.read_cif_data(file_in)
        #cd.
        cd.cifName = "new"
        cd.evalValAndErrUnitCell()
        cd.evalValAndErrAtom()
        print("n orig in new:", len(cd.listAtomRaw), len(cd.listAtomSymm),
                          "=>", len(cd.listAtomRaw)* len(cd.listAtomSymm))
        print("n atom in new:", len(cd.listAtomAll))

        #print("tmp =", tmp)
        ref = crystaldata.CrystalData("ValAndErr", uuidDB=self.uuidDB,
                                      abox_prefix="abox")

        ref.loadPyMatGen(file_in, "cifname")
        print("============")
        ref.evalPyMatGenAtom()

        print("n atom in ref:", len(ref.listAtomAll))
        for atom in ref.listAtomAll:
            #print(atom)
            pass



        pass
        # === end of TestCrystalData.()

    # === end of class TestCrystalData

if __name__ == "__main__":
    unittest.main()
