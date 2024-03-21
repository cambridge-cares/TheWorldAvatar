
import os
import re
import time
import json
import requests

from bs4 import BeautifulSoup

# from pymatgen.core.structure import Structure, Lattice
# import pymatgen
import crystaldata
import crystalinfo

import zeolist
import tools

IZADATA = "izadata"

IZASITE = "https://asia.iza-structure.org/IZA-SC/"


class IzaFramework:
    __slots__ = ["framework", "iname", "tmp"]  # , "", "", "", "", "",  ]

    def __init__(self, name, i_name=None):
        self.prepare()
        self.framework = name
        self.iname = i_name

        pass  # IzaFramework.__init__()

    def prepare(self):
        if not os.path.isdir(IZADATA):
            os.makedirs(IZADATA)

        for theme in ["coord", "volume", "frame", "tatom", "refmat", "mater"]:
            path = os.path.join(IZADATA, theme)
            if not os.path.isdir(path):
                os.makedirs(path)

        # === end of IzaFramework.prepare()

    def get_framework_data(self):
        name = self.framework
        i_name = self.iname

        name2 = zeolist.zeoCodeToCode3(name)
        if i_name is not None:
            # print("Starting ", name, i_name)
            pass
        else:
            print("Starting ", name)

        # Function returns a dict() with all data
        # Format of the dict is not clear yet, will be adjusted over time:
        output = {}

        # Get the Coord System html files (for t-atoms):
        html = self._get_html_as_text("coord", name2)

        # Extract T atoms (the main table):
        table = self.getTAtomTable(html, name)

        atoms = self.getTAtoms(table)
        # print("atoms =", atoms, type(atoms))

        output["Framework"] = name
        output["TAtoms"] = atoms["TAtoms"]

        # Get the Framework html page:
        html = self._get_html_as_text("frame", name2)

        # Extract framework data (the main table):
        tables = self.get_framework_tables(html, name)

        value = self.getCellParam(tables)
        if "CellParameters" in value:
            output["CellParameters"] = value["CellParameters"]
        else:
            print("No data for CellParameter for framework", name)

        value = self.getFWDensity(tables)
        if "FrameworkDensity" in value:
            output["FrameworkDensity"] = value["FrameworkDensity"]
        else:
            print("No data for FrameworkDensity for framework", name)

        value = self.getTopoDensity(tables)
        if "TopologicalDensity" in value:
            output["TopologicalDensity"] = value["TopologicalDensity"]
        else:
            print("No data for TopologicalDensity for framework", name)

        value = self.getRingSize(tables)
        if "RingSizes" in value:
            output["RingSizes"] = value["RingSizes"]
        else:
            print("No data for RingSizes for framework", name)

        value = self.getChannelDim(tables)
        if "ChannelDimensions" in value:
            output["ChannelDimensions"] = value["ChannelDimensions"]
        else:
            print("No data for ChannelDimensions for framework", name)

        value = self.getSphereDiam(tables)
        if "SphereDiameter" in value:
            output["SphereDiameter"] = value["SphereDiameter"]
        else:
            print("No data for SphereDiameter for framework", name)

        # value = self.getAccessVolume(tables)
        # if "AccessVolume" in value:
        #    output["AccessVolume"] = value["AccessVolume"]
        # else:
        #    print("No data for AccessVolume for framework", name)

        value = self.getABCSequence(tables, name)
        if "ABCSequence" in value:
            output["ABCSequence"] = value["ABCSequence"]
        else:
            # Hide the warning, since not all frameworks have SBU
            # print("No data for ABCSequence for framework", name)
            pass

        value = self.getSecondaryBU(tables, name)
        if "SecondaryBU" in value:
            output["SecondaryBU"] = value["SecondaryBU"]
        else:
            # Hide the warning, since not all frameworks have SBU
            # print("No data for SecondaryBU for framework", name)
            pass

        value = self.getCompositeBU(tables, code=name)
        if "CompositeBU" in value:
            output["CompositeBU"] = value["CompositeBU"]
        else:
            print("No data for CompositeBU for framework", name)

        value = self.getNaturalTile(tables)
        if "NaturalTiles" in value:
            output["NaturalTiles"] = value["NaturalTiles"]
        else:
            print("No data for NaturalTiles for framework", name)

        # Get the Coord System html files (for t-atoms):
        html = self._get_html_as_text("volume", name2)
        # print(html)

        # Extract T atoms (the main table):
        table = self.getVolumeTable(html, name)

        value = self.getVolumeAndArea(table, name)
        if "AccessibleVolume" in value:
            output["AccessibleVolume"] = value["AccessibleVolume"]
        else:
            print("No data for AccessibleVolume for framework", name)

        if "OccupiableVolume" in value:
            output["OccupiableVolume"] = value["OccupiableVolume"]
        else:
            print("No data for OccupiableVolume for framework", name)

        if "AccessibleArea" in value:
            output["AccessibleArea"] = value["AccessibleArea"]
        else:
            print("No data for AccessibleArea for framework", name)

        if "OccupiableArea" in value:
            output["OccupiableArea"] = value["OccupiableArea"]
        else:
            print("No data for OccupiableArea for framework", name)

        if "SpecificAccessibleArea" in value:
            output["SpecificAccessibleArea"] = value["SpecificAccessibleArea"]
        else:
            print("No data for SpecificAccessibleArea for framework", name)

        if "SpecificOccupiableArea" in value:
            output["SpecificOccupiableArea"] = value["SpecificOccupiableArea"]
        else:
            print("No data for SpecificOccupiableArea for framework", name)

        # print("atoms =", atoms, type(atoms))

        # output["TAtoms"   ] = atoms["TAtoms"]

        return output
        # === end of IzaFramework.get_framework_data()

    def get_refmat_data(self):

        name = self.framework
        i_name = self.iname

        name2 = zeolist.zeoCodeToCode3(name)
        if i_name is not None:
            # print("Starting ", name, i_name)
            pass
        else:
            print("Starting ", name)
            pass

        output = {}

        html = self._get_html_as_text("refmat", name2)

        # print(html)

        # Extract data (the main table):
        tables = self.get_refmat_tables(html, name)

        """

        table, iCell = self._get_table_by_title(tables, "unit cell")
        print(table, iCell)

        """

        # print("In reference_data n_tables =", len(tables))

        table, iName = self._get_table_by_title(tables, "material name")
        # print(table, iName)

        output |= self._get_refmat_name(table)
        output |= self._get_refmat_formula(table)
        output["UnitCell"] = self._get_refmat_unitcell(table)
        output |= self._get_refmat_fwdens(table)

        table, iChann = self._get_table_by_title(tables, "channels")
        # print(table, iChann)

        output |= self._get_refmat_channels(table, name)

        table, iRef = self._get_table_by_title(tables, "reference")
        # print(table, iRef)

        output["Refs"] = self._get_refmat_references(table, code=name)

        table, iDeriv = self._get_table_by_title(tables,
                                                 "name and code derivation")
        # print(table, iDeriv)

        output["NameDerivation"] = self._get_refmat_nameorig(tables[3])

        return {"ReferenceMaterial": output}
        # output["ReferenceMaterial"] = {}
        # return output
        # === end of IzaFramework.get_refmat_data()

    def _get_oneline_by_keyword(self, table, keyword):
        output = None
        if table is None:
            print("Invalid input data in _get_refmat_name:", table)
            return output

        got_data = False
        tr_els = table.find_all('tr')
        # if len(tr_els) > 1:
        #    print("Too many rows for oneline_by_keyword for '",
        #          keyword, "'.", sep="")
        for tr in tr_els:
            td_els = tr.find_all('td')
            # print( td_els)
            for itd, td in enumerate(td_els):
                text = td.text.strip().lower()
                if text.startswith(keyword.lower()):
                    if itd < len(td_els) - 1:
                        output = td_els[itd+1].text.strip()
                        got_data = True
                    else:
                        print("Found name '", keyword,
                              "' but not found value", sep="")
                    break

        if not got_data:
            print("Warning! Not found data for keyword '", keyword,
                  "'.", sep="")

        return output
        # === end of IzaFramework._get_oneline_by_keyword()

    def _get_refmat_name(self, table):
        output = {}

        value = self._get_oneline_by_keyword(table, "material name")
        if value is not None:
            output["MaterialName"] = value

        return output
        # === end of IzaFramework._get_refmat_name()

    def _get_refmat_formula(self, table):
        output = {}

        value = self._get_oneline_by_keyword(table, "chemical formula")
        if value is not None:
            value = value.replace("\u00a0", " ").replace("  ", " ")
            value = value.replace("  ", " ")
            output["ChemicalFormula"] = value

        return output
        # === end of IzaFramework._get_refmat_formula()

    def _get_refmat_unitcell(self, table):
        output = {}
        if table is None:
            print("Invalid input data in _get_refmat_formula:", table)
            return output

        return output
        # === end of IzaFramework._get_refmat_formula()

    def _get_refmat_fwdens(self, table):
        output = {}

        value = self._get_oneline_by_keyword(table, "framework density")
        if value is not None:
            value = value.replace("\u00c5", "A")  # Angstrom special character
            pos = value.find("T/1000 A3")
            if pos >= 0:
                value = value[:pos].strip()
                output["FrameworkDensity"] = {"value": value,
                                              "unit": "T/1000A^3"}
            else:
                print("Framework Density (ref mat) should contain T/1000 A^3")

        return output
        # === end of IzaFramework._get_refmat_fwdens()

    def _get_refmat_channels(self, table, code=""):
        output = {}
        if table is None:
            print("Invalid input data in _get_refmat_channels:", table)
            return output

        if code == "CGF":
            output["Channels"] = []
            output["Channels"].append({"dir": '[100]', "tring": 10, "sizeMin": 2.5, "sizeMax": 5.2, "dim": 1})
            output["Channels"].append({"dir": '[100]', "tring":  8, "sizeMin": 2.1, "sizeMax": 6.7, "dim": 1})
            output["Channels"].append({"dir": '[001]', "tring":  8, "sizeMin": 2.4, "sizeMax": 4.8, "dim": 1})
            # output["Channels"].append({"dir": , "tring": , "sizeMin": , "sizeMax": , "dim": }
            return output
        elif code == "EON":
            # {<->  [010]  {<->    }**
            output["Channels"] = []
            output["Channels"].append({"dir": "[100]", "tring": 12, "sizeMin": 6.7, "sizeMax":  6.8, "dim": 1})
            output["Channels"].append({"dir": "[001]", "tring": 8, "sizeMin": 3.4, "sizeMax": 4.9, "dim": 1})
            output["Channels"].append({"dir": "[100]", "tring": 8, "sizeMin": 2.8, "sizeMax": 2.8, "dim": 1})
            return output
        elif code == "EUO":
            # EUO with large side pockets
            output["Channels"] = []
            output["Channels"].append({"dir": "[100]", "tring": 10, "sizeMin": 4.1, "sizeMax":  5.4, "dim": 1})
            return output
        elif code == "HEU":
            # HEU {+ } <->
            output["Channels"] = []
            output["Channels"].append({"dir": "[001]", "tring": 10, "sizeMin": 3.1, "sizeMax": 5.5, "dim": 1})
            output["Channels"].append({"dir": "[001]", "tring": 8, "sizeMin": 4.1, "sizeMax": 4.1, "dim": 1})
            output["Channels"].append({"dir": "[100]", "tring": 8, "sizeMin": 2.8, "sizeMax": 3.4, "dim": 1})
            return output
        elif code == "-HOS":
            # -HOS (↔  ↔  )***
            output["Channels"] = []
            output["Channels"].append({"dir": "[100]", "tring": 12, "sizeMin": 6.0, "sizeMax": 9.1, "dim": 1})
            output["Channels"].append({"dir": "[001]", "tring": 12, "sizeMin": 7.2, "sizeMax": 8.0, "dim": 1})
            output["Channels"].append({"dir": "[100]", "tring": 10, "sizeMin": 5.2, "sizeMax": 5.2, "dim": 1})
            return output
        elif code == "LAU":
            # LAU (contracts upon dehydration)
            output["Channels"] = []
            output["Channels"].append({"dir": "[100]", "tring": 10, "sizeMin": 4.0, "sizeMax": 5.3, "dim": 1})
            return output

        elif code == "LTF":
            # LTF |  <->
            output["Channels"] = []
            output["Channels"].append({"dir": "[001]", "tring": 12, "sizeMin": 7.2, "sizeMax": 7.5, "dim": 1})
            output["Channels"].append({"dir": "[001]", "tring": 12, "sizeMin": 6.5, "sizeMax": 7.2, "dim": 1})
            output["Channels"].append({"dir": "<210>", "tring":  8, "sizeMin": 3.3, "sizeMax": 4.9, "dim": 2})
            return output
        elif code == "MRT":
            # MRT {(<->  <->  (<->   <-> )}**
            output["Channels"] = []
            output["Channels"].append({"dir": "[010]", "tring": 8, "sizeMin": 3.3, "sizeMax": 3.5, "dim": 1})
            output["Channels"].append({"dir": "[100]", "tring": 8, "sizeMin": 3.6, "sizeMax": 3.9, "dim": 1})
            output["Channels"].append({"dir": "[001]", "tring": 8, "sizeMin": 3.6, "sizeMax": 3.6, "dim": 1})
            output["Channels"].append({"dir": "[100]", "tring": 8, "sizeMin": 3.7, "sizeMax": 3.9, "dim": 1})
            output["Channels"].append({"dir": "[001]", "tring": 8, "sizeMin": 3.8, "sizeMax": 3.9, "dim": 1})
            return output
        elif code == "OBW":
            # { <->   + )    <-> }***
            output["Channels"] = []
            output["Channels"].append({"dir": "<110>", "tring": 10, "sizeMin": 5.0, "sizeMax": 5.0, "dim": 2})
            output["Channels"].append({"dir": "[001]", "tring":  8, "sizeMin": 3.4, "sizeMax": 3.4, "dim": 1})
            output["Channels"].append({"dir": "<101>", "tring":  8, "sizeMin": 2.8, "sizeMax": 4.0, "dim": 1})
            output["Channels"].append({"dir": "<100>", "tring":  8, "sizeMin": 3.3, "sizeMax": 3.4, "dim": 2})
            return output

        elif code == "POR":
            # POR    x ** <->   b  x
            output["Channels"] = [{"dir": "{100}", "tring": 8, "sizeMin": 3.3,
                                   "sizeMax": 4.4, "dim": 2},
                                  {"dir": "[001]", "tring": 8, "sizeMin": 3.1,
                                   "sizeMax": 3.1, "dim": 1}]
            return output
        elif code == "SIV":
            # { [100] 8 (3.5 x 3.9+ 8 3.7 x 3.8  <->   [110] 8 3.7 x 3.8 <->
            #   [001] 8 3.8 x 3.9 } ***
            output["Channels"] = [{"dir": "[100]", "tring": 8, "sizeMin": 3.5,
                                   "sizeMax": 3.9, "dim": 1},
                                  {"dir": "[100]", "tring": 8, "sizeMin": 3.7,
                                   "sizeMax": 3.8, "dim": 1},
                                  {"dir": "[110]", "tring": 8, "sizeMin": 3.7,
                                   "sizeMax": 3.8, "dim": 1},
                                  {"dir": "[001]", "tring": 8, "sizeMin": 3.8,
                                   "sizeMax": 3.9, "dim": 1}]
            return output
        elif code == "SOR":
            # SOR [001] 12 6.3 x.6.6* <->  [110] 8 2.6 x 4.9**
            output["Channels"] = [{"dir": "[001]", "tring": 12, "sizeMin": 6.3,
                                   "sizeMax": 6.6, "dim": 1},
                                  {"dir": "[110]", "tring": 8, "sizeMin": 2.6,
                                   "sizeMax": 4.9, "dim": 2}]
            return output

        elif code == "SOV":
            # SOV [100] 12 6.1 x 6.9 <->  [001] 12 5.9 x 6.3 <->
            #     [010] 10 5.1 x.5.5 ***
            output["Channels"] = [{"dir": "[100]", "tring": 12, "sizeMin": 6.1,
                                   "sizeMax": 6.9, "dim": 1},
                                  {"dir": "[001]", "tring": 12, "sizeMin": 5.9,
                                   "sizeMax": 6.3, "dim": 1},
                                  {"dir": "[010]", "tring": 10, "sizeMin": 5.1,
                                   "sizeMax": 5.5, "dim": 1}]
            return output
        elif code == "-SSO":
            # -SSO {[010] 11(18) 4.6 x 6.0 <->
            #   [100] 9 3.3 x 6.0 (internal connection}*
            output["Channels"] = [{"dir": "[010]", "tring": 11, "sizeMin": 4.6,
                                   "sizeMax": 6.0, "dim": 1},
                                  {"dir": "[010]", "tring": 18, "sizeMin": 4.6,
                                   "sizeMax": 6.0, "dim": 1},
                                  {"dir": "[100]", "tring": 9, "sizeMin": 3.3,
                                   "sizeMax": 6.0, "dim": 1}]
            return output

        elif code == "-SVR":
            # -SVR [100] 10 5.5 x.5.7* <->  [010] 10 5.2 x.5.9* <->
            #   [001] 10 5.2 x.5.6*
            output["Channels"] = [{"dir": "[100]", "tring": 10, "sizeMin": 5.5,
                                   "sizeMax": 5.7, "dim": 1},
                                  {"dir": "[010]", "tring": 10, "sizeMin": 5.2,
                                   "sizeMax": 5.9, "dim": 1},
                                  {"dir": "[001]", "tring": 10, "sizeMin": 5.2,
                                   "sizeMax": 5.6, "dim": 1}]
            return output
        elif code == "TUN":
            # TUN {[010] { 10 5.5 x 6.0  + 10 5.2 x 6.0} <->
            #   [10-1] 10 5.4 x 5.5 }***
            output["Channels"] = [{"dir": "[010]", "tring": 10, "sizeMin": 5.5,
                                   "sizeMax": 6.0, "dim": 1},
                                  {"dir": "[010]", "tring": 10, "sizeMin": 5.2,
                                   "sizeMax": 6.0, "dim": 1},
                                  {"dir": "[10-1]", "tring": 10,
                                   "sizeMin": 5.4, "sizeMax": 5.5, "dim": 2}]
            return output
        elif code == "UFI":
            # UFI <100> 8 3.6 x 4.4** <->   [001] 8 3.3 x 3.3(cage)
            #       i.e. ends in a cage with this window to the cage
            output["Channels"] = [{"dir": "<100>", "tring": 8, "sizeMin": 3.6,
                                   "sizeMax": 4.4, "dim": 2},
                                  {"dir": "[001]", "tring": 8, "sizeMin": 3.3,
                                   "sizeMax": 3.3, "dim": 1}]
            return output
        elif code == "UOV":
            # UOV {[100] { 12 6.0 x 7.7* + 12 5.9 x 7.1* + 8 2.9 x 3.1*} <->
            #      [001] 10 4.7 x 5.9* <->  <031> 10 4.7 x 5.9**}***
            output["Channels"] = [{"dir": "[100]", "tring": 12, "sizeMin": 6.0,
                                   "sizeMax": 7.7, "dim": 1},
                                  {"dir": "[100]", "tring": 12, "sizeMin": 5.9,
                                   "sizeMax": 7.1, "dim": 1},
                                  {"dir": "[100]", "tring":  8, "sizeMin": 2.9,
                                   "sizeMax": 3.1, "dim": 1},
                                  {"dir": "[001]", "tring": 10, "sizeMin": 4.7,
                                   "sizeMax": 5.9, "dim": 1},
                                  {"dir": "<031>", "tring": 10, "sizeMin": 4.7,
                                   "sizeMax": 5.9, "dim": 2}]
            return output

        elif code == "VNI":
            # VNI {<110> 8 3.1 x 4.0 <->  [001] 8 3.5 x .3.6}***
            output["Channels"] = [{"dir": "<110>", "tring": 8, "sizeMin": 3.1,
                                   "sizeMax": 4.0, "dim": 2},
                                  {"dir": "[001]", "tring": 8, "sizeMin": 3.5,
                                   "sizeMax": 3.6, "dim": 1}]
            return output
        elif code == "-ION":
            # -ION {[010] 10 5.1 x 5.8* <->  [100] 4.7 x 5.2 }**
            output["Channels"] = [{"dir": "[010]", "tring": 10, "sizeMin": 5.1,
                                   "sizeMax": 5.8, "dim": 1},
                                  {"dir": "[100]", "tring": 10, "sizeMin": 4.7,
                                   "sizeMax": 5.2, "dim": 1},
                                  {"dir": "[100]", "tring": 18, "sizeMin": 4.7,
                                   "sizeMax": 5.2, "dim": 1}]
            return output

            """
        elif code == "PWN":
            #
            output["Channels"] = []
            return output

        elif code == "PWN":
            #PWN <100> 8 4.0 x 4.0 *** | <100> 8 3.6 x 3.6***

        elif code == "RHO":
            #RHO <100> 8 3.6 x 3.6*** | <100> 8 3.6 x 3.6***
            output["Channels"] = [
    {"dir": '<100>', 'tring': 8, 'sizeMin': 3.6, 'sizeMax': 3.6, 'dim': 2},
    {"dir": '<100>', 'tring': 8, 'sizeMin': 3.6, 'sizeMax': 3.6, 'dim': 2}]
            return output
        """

        elif code == "RWR":
            # RWR [100] 8 2.8 x 5.0* || [010] 8 2.8 x 5.0*
            #      --- nonintersecting 1-d 8-ring channels
            output["Channels"] = [{"dir": '[100]', "tring": 8, "sizeMin": 2.8,
                                   "sizeMax": 5.0, "dim": 1},
                                  {"dir": '[010]', "tring": 8, "sizeMin": 2.8,
                                   "sizeMax": 5.0, "dim": 1}]
            return output

        elif code in ["AFG", "ANA", "AST", "DOH", "FAR", "FRA", "GIU", "LIO",
                      "LOS", "LTN", "MAR", "MEP", "MSO", "MTN", "NON", "RUT",
                      "SGT", "SOD", "SVV", "TOL", "UOZ", "-LIT", "-ITV", "AFN",
                      ]:
            output["Channels"] = []
            return output

        elif code == "CHA":
            # CHA ⊥ [001] 8 3.8 x 3.8*** (variable due to considerable
            #                             flexibility of framework)
            output["Channels"] = [{"dir": "⊥ [001]", "tring": 8,
                                   "sizeMin": 3.8, "sizeMax": 3.8, "dim": 3}]
            return output
        elif code == "AEI":
            # AEI   ['{[100]', '8', '3.8 x 3.8 <-> [110]', '8', '3.8 x 3.8 <->
            #          [001]', '8', '3.8 x 3.8}***']
            output["Channels"] = [{'dir': '[100]', 'tring': 8, 'sizeMin': 3.8,
                                   'sizeMax': 3.8, 'dim': 1},
                                  {'dir': '[110]', 'tring': 8, 'sizeMin': 3.8,
                                   'sizeMax': 3.8, 'dim': 2},
                                  {'dir': '[001]', 'tring': 8, 'sizeMin': 3.8,
                                   'sizeMax': 3.8, 'dim': 1}]
            return output

        elif code == "CGS":
            # CGS    ['{[001]', '10', '3.5 x 4.8 <->
            #           [100]', '8', '2.5 x 4.6}***']
            output["Channels"] = [{'dir': '[001]', 'tring': 10, 'sizeMin': 3.5,
                                   'sizeMax': 4.8, 'dim': 1},
                                  {'dir': ' [100]', 'tring': 8, 'sizeMin': 2.5,
                                   'sizeMax': 4.6, 'dim': 1}]
            return output
        elif code == "DFO":
            # DFO   ['{[001]', '12', '7.3 x 7.3 <->',
            #    '⊥', '[001]', '8', '3.4 x 5.6}*** <->
            #         {[001]', '12', '6.2 x 6.2 <->',
            #    '⊥', '[001]', '10', '5.4 x 6.4}***']
            output["Channels"] = [{'dir': '[001]', 'tring': 12, 'sizeMin': 7.3,
                                   'sizeMax': 7.3, 'dim': 1},
                                  {'dir': '⊥[001]', 'tring': 8, 'sizeMin': 3.4,
                                   'sizeMax': 5.6, 'dim': 2},
                                  {'dir': '[001]', 'tring': 12, 'sizeMin': 6.2,
                                   'sizeMax': 6.2, 'dim': 1},
                                  {'dir': '⊥[001]', 'tring': 10,
                                   'sizeMin': 5.4, 'sizeMax': 6.4, 'dim': 2}]
            return output
        elif code == "EPI":
            # EPI   ['{ [001]', '8', '3.7 x 4.5\xa0<->
            #           [100]', '8', '3.6 x 3.6 }**']
            output["Channels"] = [{'dir': ' [001]', 'tring': 8, 'sizeMin': 3.7,
                                   'sizeMax': 4.5, 'dim': 1},
                                  {'dir': ' [100]', 'tring': 8, 'sizeMin': 3.6,
                                   'sizeMax': 3.6, 'dim': 1}]
            return output

        elif code == "ETL":
            # ETL   ['[100]', '8', '2.7 x 5.0* <-> [101]', '8', '3.3 x 4.8 <->
            #         [100]', '8', '2.8 x 4.6*']
            output["Channels"] = [{'dir': '[100]', 'tring': 8, 'sizeMin': 2.7,
                                   'sizeMax': 5.0, 'dim': 1},
                                  {'dir': ' [101]', 'tring': 8, 'sizeMin': 3.3,
                                   'sizeMax': 4.8, 'dim': 2},
                                  {'dir': ' [100]', 'tring': 8, 'sizeMin': 2.8,
                                   'sizeMax': 4.6, 'dim': 1}]
            return output

        elif code == "ETV":
            # ETV   ['[001]', '10', '2.9 x 6.1 <-> [100]', '8', '3.2 x 5.2 <->
            #         [010]', '8', '4.2 x 4.5']
            output["Channels"] = [{'dir': '[001]', 'tring': 10, 'sizeMin': 2.9,
                                   'sizeMax': 6.1, 'dim': 1},
                                  {'dir': ' [100]', 'tring': 8, 'sizeMin': 3.2,
                                   'sizeMax': 5.2, 'dim': 1},
                                  {'dir': ' [010]', 'tring': 8, 'sizeMin': 4.2,
                                   'sizeMax': 4.5, 'dim': 1}]
            return output

        elif code == "EWF":
            # EWF   ['[001]', '11', '6.0 x 4.6 <-> [100]', '10', '5.4 x 5.2']
            output["Channels"] = [{'dir': '[001]', 'tring': 11, 'sizeMin': 6.0,
                                   'sizeMax': 4.6, 'dim': 1},
                                  {'dir': '[100]', 'tring': 10, 'sizeMin': 5.4,
                                   'sizeMax': 5.2, 'dim': 1}]
            return output

        elif code == "EWO":
            # EWO    ['[001]', '10', '4.6 x 5.4']
            output["Channels"] = [{'dir': '[001]', 'tring': 10, 'sizeMin': 4.6,
                                   'sizeMax': 5.4, 'dim': 1}]
            return output

        elif code == "GIS":
            # GIS  ['{[100]', '8', '3.1 x 4.5 <-> [010]', '8', '2.8 x 4.8}***']
            output["Channels"] = [{'dir': '[100]', 'tring': 8, 'sizeMin': 3.1,
                                   'sizeMax': 4.5, 'dim': 1},
                                  {'dir': ' [010]', 'tring': 8, 'sizeMin': 2.8,
                                   'sizeMax': 4.8, 'dim': 1}]
            return output

        elif code == "IMF":
            # IMF    ['{[001]', '10', '5.5 x 5.6 <->
            #           [100]', '10', '5.3 x 5.4}** <->
            #          {[010]', '10', '5.3 x 5.9} <->
            #          {[001]', '10', '4.8 x 5.4  <->
            #           [100]', '10', '5.1 x 5.3}**']
            output["Channels"] = [{'dir': '[001]', 'tring': 10, 'sizeMin': 5.5,
                                   'sizeMax': 5.6, 'dim': 1},
                                  {'dir': '[100]', 'tring': 10, 'sizeMin': 5.3,
                                   'sizeMax': 5.4, 'dim': 1},
                                  {'dir': '[010]', 'tring': 10, 'sizeMin': 5.3,
                                   'sizeMax': 5.9, 'dim': 1},
                                  {'dir': '[001]', 'tring': 10, 'sizeMin': 4.8,
                                   'sizeMax': 5.4, 'dim': 1},
                                  {'dir': '[100]', 'tring': 10, 'sizeMin': 5.1,
                                   'sizeMax': 5.3, 'dim': 1}]
            return output

        elif code == "-IRT":
            # -IRT    ['([001]', '28', '20.1 x 21.2 <->
            #            <100]', '12', '6.4 x 8.0', '12', '5.9 x 7.0)***']
            output["Channels"] = [{'dir': '[001]', 'tring': 28,
                                   'sizeMin': 20.1, 'sizeMax': 21.2, 'dim': 1},
                                  {'dir': '[001]', 'tring': 12, 'sizeMin': 6.4,
                                   'sizeMax': 8.0, 'dim': 1},
                                  {'dir': '[001]', 'tring': 12, 'sizeMin': 5.9,
                                   'sizeMax': 7.0, 'dim': 1}]
            return output

        elif code == "ITG":
            # ITG   ['[010]', '12', '5.9x 6.9* <-> [001]', '10', '4.8 x 6.1 <->
            #        {[010]', '10', '3.8x 6.3 <-> [100]', '10', '4.8 x 5.8}**']
            output["Channels"] = [{'dir': '[010]', 'tring': 12, 'sizeMin': 5.9,
                                   'sizeMax': 6.9, 'dim': 1},
                                  {'dir': '[001]', 'tring': 10, 'sizeMin': 4.8,
                                   'sizeMax': 6.1, 'dim': 1},
                                  {'dir': '[010]', 'tring': 10, 'sizeMin': 3.8,
                                   'sizeMax': 6.3, 'dim': 1},
                                  {'dir': '[100]', 'tring': 10, 'sizeMin': 4.8,
                                   'sizeMax': 5.8, 'dim': 1}]
            return output

        elif code == "IWR":
            # IWR    ['{[001]', '12', '5.8 x 6.8 <->
            #           [100]', '10', '4.6 x 5.3 <->
            #           [010]', '10', '4.6 x 5.3}***']
            output["Channels"] = [{'dir': '[001]', 'tring': 12, 'sizeMin': 5.8,
                                   'sizeMax': 6.8, 'dim': 1},
                                  {'dir': '[100]', 'tring': 10, 'sizeMin': 4.6,
                                   'sizeMax': 5.3, 'dim': 1},
                                  {'dir': '[010]', 'tring': 10, 'sizeMin': 4.6,
                                   'sizeMax': 5.3, 'dim': 1}]
            return output

        elif code == "IWV":
            # IWV    ['{[001]', '12', '6.2 x 6.9 <->
            #           [011]', '12', '6.2 x 6.9 }**']
            output["Channels"] = [{'dir': '[001]', 'tring': 12, 'sizeMin': 6.2,
                                   'sizeMax': 6.9, 'dim': 1},
                                  {'dir': '[011]', 'tring': 12, 'sizeMin': 6.2,
                                   'sizeMax': 6.9, 'dim': 2}]
            return output

        elif code == "JZO":
            # JZO    ['<100>', '16', '9.6 x 9.7*** <->
            #          <100>', '12', '6.6 x 6.8']
            output["Channels"] = [{'dir': '<100>', 'tring': 16, 'sizeMin': 9.6,
                                   'sizeMax': 9.7, 'dim': 2},
                                  {'dir': '<100>', 'tring': 12, 'sizeMin': 6.6,
                                   'sizeMax': 6.8, 'dim': 2}]
            return output

        elif code == "MER":
            # MER    ['[100]', '8', '3.1 x 3.5* <->
            #          [010]', '8', '2.7  x 3.6* <->
            #          [001] {', '8', '3.4 x 5.1* +', '8', '3.3 x 3.3*}']
            output["Channels"] = [{'dir': '[100]', 'tring': 8, 'sizeMin': 3.1,
                                   'sizeMax': 3.5, 'dim': 1},
                                  {'dir': '[010]', 'tring': 8, 'sizeMin': 2.7,
                                   'sizeMax': 3.6, 'dim': 1},
                                  {'dir': '[001]', 'tring': 8, 'sizeMin': 3.4,
                                   'sizeMax': 5.1, 'dim': 1},
                                  {'dir': '[001]', 'tring': 8, 'sizeMin': 3.3,
                                   'sizeMax': 3.3, 'dim': 1}]
            return output

        elif code == "MFI":
            # MFI    ['{[100]', '10', '5.1 x 5.5 <->
            #          [010]', '10', '5.3 x 5.6}***']
            output["Channels"] = [{'dir': '[100]', 'tring': 10, 'sizeMin': 5.1,
                                   'sizeMax': 5.5, 'dim': 1},
                                  {'dir': '[010]', 'tring': 10, 'sizeMin': 5.3,
                                   'sizeMax': 5.6, 'dim': 1}]
            return output

        elif code == "MOZ":
            # MOZ    ['{[001]', '12', '6.8 x 7.0 <->
            #          n[001]', '8', '3.8 x 4.8}*** |
            #          [001]', '12', '6.8 x 6.8*']
            output["Channels"] = [{'dir': '[001]', 'tring': 12, 'sizeMin': 6.8,
                                   'sizeMax': 7.0, 'dim': 1},
                                  {'dir': ' [001]', 'tring': 8, 'sizeMin': 3.8,
                                   'sizeMax': 4.8, 'dim': 1},
                                  {'dir': '[001]', 'tring': 12, 'sizeMin': 6.8,
                                   'sizeMax': 6.8, 'dim': 1}]
            return output

        elif code == "MSE":
            # MSE ['{[001]', '12', '6.4 x 6.8 <-> [100]', '10', '5.2 x 5.8 <->
            #        [110]', '10', '5.2 x 5.2 }***']
            output["Channels"] = [{'dir': '[001]', 'tring': 12, 'sizeMin': 6.4,
                                   'sizeMax': 6.8, 'dim': 1},
                                  {'dir': '[100]', 'tring': 10, 'sizeMin': 5.2,
                                   'sizeMax': 5.8, 'dim': 1},
                                  {'dir': '[110]', 'tring': 10, 'sizeMin': 5.2,
                                   'sizeMax': 5.2, 'dim': 2}]
            return output

        elif code == "PTF":
            # PTF    ['[101]', '10', '4.6 x 6.7 <-> [001]', '8', '2.3 x 6.3 <->
            #          [001]', '6', '1.3 x 6.3']
            output["Channels"] = [{'dir': '[101]', 'tring': 10, 'sizeMin': 4.6,
                                   'sizeMax': 6.7, 'dim': 1},
                                  {'dir': ' [001]', 'tring': 8, 'sizeMin': 2.3,
                                   'sizeMax': 6.3, 'dim': 1},
                                  {'dir': ' [001]', 'tring': 6, 'sizeMin': 1.3,
                                   'sizeMax': 6.3, 'dim': 1}]
            return output

        elif code == "PTY":
            # PTY    ['[100]', '10', '4.1 x 6.7 <-> [001]', '8', '3.1 x 4.3']
            output["Channels"] = [{'dir': '[100]', 'tring': 10, 'sizeMin': 4.1,
                                   'sizeMax': 6.7, 'dim': 1},
                                  {'dir': ' [001]', 'tring': 8, 'sizeMin': 3.1,
                                   'sizeMax': 4.3, 'dim': 1}]
            return output

        elif code == "SOF":
            # SOF ['{[001]', '12', '4.4 x 9.7 <-> [100]', '9', '4.3 x 4.8}***']
            output["Channels"] = [{'dir': '[001]', 'tring': 12, 'sizeMin': 4.4,
                                   'sizeMax': 9.7, 'dim': 1},
                                  {'dir': ' [100]', 'tring': 9, 'sizeMin': 4.3,
                                   'sizeMax': 4.8, 'dim': 1}]
            return output
        elif code == "SOS":
            # SOS  ['[100]', '12', '3.9 x 9.1 <-> [010]', '8', '3.3 x 3.3}***']
            output["Channels"] = [{'dir': '[100]', 'tring': 12, 'sizeMin': 3.9,
                                   'sizeMax': 9.1, 'dim': 1},
                                  {'dir': ' [010]', 'tring': 8, 'sizeMin': 3.3,
                                   'sizeMax': 3.3, 'dim': 1}]
            return output

        elif code == "-SYT":
            # -SYT    ['[001]', '24', '4.8 x 7.4 <-> [110]', '8', '3.8 x 4.8 |
            #           [001]', '8', '3.8 x 3.8']
            output["Channels"] = [{'dir': '[001]', 'tring': 24, 'sizeMin': 4.8,
                                   'sizeMax': 7.4, 'dim': 1},
                                  {'dir': ' [110]', 'tring': 8, 'sizeMin': 3.8,
                                   'sizeMax': 4.8, 'dim': 2},
                                  {'dir': ' [001]', 'tring': 8, 'sizeMin': 3.8,
                                   'sizeMax': 3.8, 'dim': 1}]
            return output

        elif code == "SZR":
            # SZR  ['{[001]', '10', '4.1 x 5.2 <-> [010]', '8', '3.2 x 4.8 <->
            #         [110]', '8', '3.0 x 4.8 }***']
            output["Channels"] = [{'dir': '[001]', 'tring': 10, 'sizeMin': 4.1,
                                   'sizeMax': 5.2, 'dim': 1},
                                  {'dir': ' [010]', 'tring': 8, 'sizeMin': 3.2,
                                   'sizeMax': 4.8, 'dim': 1},
                                  {'dir': ' [110]', 'tring': 8, 'sizeMin': 3.0,
                                   'sizeMax': 4.8, 'dim': 2}]
            return output

        elif code == "UEI":
            # UEI   ['{[010]', '8', '3.5 x 4.6 <-> [001]', '8', '2.5 x 3.6}**']
            output["Channels"] = [{'dir': '[010]', 'tring': 8, 'sizeMin': 3.5,
                                   'sizeMax': 4.6, 'dim': 1},
                                  {'dir': '[001]', 'tring': 8, 'sizeMin': 2.5,
                                   'sizeMax': 3.6, 'dim': 1}]
            return output

        elif code == "YFI":
            # YFI   ['[100]', '12', '6.0 x 7.8 <-> [010]', '12', '6.0 x 7.2 <->
            #         [001]', '8', '3.4 x 4.5 | [001]', '8', '3.8 x 3.8*']
            output["Channels"] = [{'dir': '[100]', 'tring': 12, 'sizeMin': 6.0,
                                   'sizeMax': 7.8, 'dim': 1},
                                  {'dir': '[010]', 'tring': 12, 'sizeMin': 6.0,
                                   'sizeMax': 7.2, 'dim': 1},
                                  {'dir': ' [001]', 'tring': 8, 'sizeMin': 3.4,
                                   'sizeMax': 4.5, 'dim': 1},
                                  {'dir': ' [001]', 'tring': 8, 'sizeMin': 3.8,
                                   'sizeMax': 3.8, 'dim': 1}]
            return output

        """
        elif code == "":
            #
            output["Channels"] = [

            return output

        elif code == "":
            #
            output["Channels"] = [

            return output

        elif code == "":
            #
            output["Channels"] = [

            return output

        elif code == "":
            #
            output["Channels"] = [

            return output

        elif code == "":
            #
            output["Channels"] = [

            return output

        elif code == "":
            #
            output["Channels"] = [

            return output

        """

        got_data = False

        tr_els = table.find_all("tr")
        tr = tr_els[0]
        td_els = tr.find_all("td")
        for itd, td in enumerate(td_els):
            # print(td)
            if got_data:
                break
            if td.text.strip().lower().find("channels") >= 0:
                # print("found channels: '", td.text, "'", sep="")
                val = td_els[itd+1]
                # ttt = val.find_all("table")
                # print( val )
                td_content = val.contents
                out = ""
                out_list = []
                for content in td_content:
                    if content.name == "table":
                        # print(">>> found table", content)
                        got_data = True
                        break
                    if content.name == "img":
                        img_name = content.get("src")
                        if img_name == "images/vertical_sign.gif":
                            # out_list.append("perp")
                            out_list.append(u"\u22A5")
                        else:
                            print("Unknown image:", img_name, "in", code)

                    short = content.text.strip()
                    short = short.replace("(highly distorted 12-ring)", "")
                    short = short.replace("", "")
                    short = short.replace("", "")
                    short = short.replace("", "")
                    short = short.replace("", "")

                    out += short
                    if short != "":
                        out_list.append(short)
                    # print(content.text.strip())
                    origList = list(out_list)
                # print("channels = '", out, "'", sep="")
                # print("channels = '", out_list, "'", sep="")
                # print("sssssssss")

                new_list = []
                for elem in out_list:
                    elem = elem.replace("{", "").replace("}", "")
                    elem = elem.replace("↔", "<->")
                    words = elem.split("<->")
                    if len(words) == 2:
                        new_list += [words[0] + "<->", words[1]]
                    else:
                        new_list += words
                out_list = list(new_list)

                # print("bbb", out_list)
                new_list = []
                for elem in out_list:
                    words = elem.split("|")
                    if len(words) == 2:
                        new_list += [words[0] + "|", words[1]]
                    else:
                        new_list += words

                out_list = list(new_list)

                # print("aaa", out_list)
                new_list = []
                for elem in out_list:
                    if isinstance(elem, str):
                        if len(elem.strip()) > 0:
                            new_list.append(elem)
                    else:
                        new_list.append(elem)
                out_list = list(new_list)

                # n = len(out_list)
                # for i in reversed(range(n)):
                #    elem = out_list[i]
                #    if elem.text.find("<->")

                # if code == "-HOS":
                #    tmp = " ".join(out_list)
                #    for char in tmp[:]:
                #        print(char, ord(char), hex(ord(char)))
                #    print(u"\u2194")
                # if "".join(out_list).find("<") >= 0:
                #    print("detected <000> in", code, ":", " ".join(out_list))
                # if "".join(out_list).find(u"\u2194") >= 0:
                #    print("detected <> in", code, ":", " ".join(out_list))
                # if "".join(out_list).find("{") >= 0:
                #    print("detected {} in", code, ":", " ".join(out_list))
                # if "".join(out_list).find("(") >= 0:
                #    print("detected () in", code, ":", " ".join(out_list))
                # if "".join(out_list).find("|") >= 0:
                #    print("detected | in", code, ":", " ".join(out_list))
                # print(code, " ".join(out_list))
                output = self._decode_channel_list(list(out_list), code=code)
                # print(channel)
                # print("    ", output)
                for chan in output["Channels"]:
                    # print("    ", chan)
                    pass
                # print("==============================")

                errCount = 0
                for chan in output["Channels"]:
                    if len(chan) > 0 and (chan["dir"] == "" or
                       chan["tring"] is None or chan["sizeMin"] is None or
                       chan["sizeMax"] is None or chan["dim"] is None or
                       chan["dim"] == 0 or chan["dim"] > 3 or
                       chan["sizeMin"] > chan["sizeMax"] or
                       chan["dir"].count("[") > 1 or
                       chan["dir"].count("]") > 1 or
                       chan["dir"].count("<") > 1 or
                       chan["dir"].count(">") > 1):
                        errCount += 1
                if errCount > 0:
                    print(code, "  ", origList)
                    for elem in output["Channels"]:
                        print("   ", "=>", elem)

        """
        for value in out_list:
            if value == u"\u22A5":
                output["channel"]["dir"] += value
            elif value.find("[") >= 0 and value.find("]") >= 0:
                output["channel"]["dir"] += value
            elif
            else:
                print("Error: not processed channel data:")
        td_content = tr.find("td").contents

        for content in td_content:
            if content.name == "table":
                print("found internal table")
                pass
            elif isinstance(content, str):
                print("found string:", content)
            else:
                print("found something:", content)
        """

        """
        tr_els = table.find_all('tr')
        # if len(tr_els) > 1:
        #    print("Too many rows for oneline_by_keyword for '",
        #          keyword, "'.", sep="")
        #for tr in tr_els:
        tr = tr_els[0]
        td_els = tr.find_all('td')
        #print( td_els)
        for itd, td in enumerate(td_els):
                text = td.text.strip()
                print( text )
        """

        return output
        # === end of IzaFramework._get_refmat_channels()

    def _decode_channel_list(self, out_list, code=""):
        output = {}

        output["Channels"] = []

        channel = {"dir": "", "tring": None, "sizeMin": None,
                   "sizeMax": None, "dim": None}
        # print(out_list)
        while len(out_list) > 0:
            value = out_list.pop(0)
            value = value.replace("{", "").replace("}", "")
            # print(value)
            # print(channel)
            if value == u"\u22A5":
                channel["dir"] += value

            elif (value.find("[") >= 0 and value.find("]") >= 0) or \
                 (value.find("<") >= 0 and value.find(">") >= 0):
                channel["dir"] += value

                tring = out_list.pop(0)
                # print(tring)
                tring = tring.replace("{", "").replace("}", "")
                channel["tring"] = int(tring)

                size = out_list.pop(0)
                # print(size)
                size = size.replace("{", "").replace("}", "")
                sizeMin, sizeMax, dim, link = self._split_channel_range(size, code)
                channel["sizeMin"] = sizeMin
                channel["sizeMax"] = sizeMax
                channel["dim"] = dim
                if link == "|":
                    output["ChannelConnection"] = "|"
                    if channel["sizeMin"] > channel["sizeMax"]:
                        tmp = channel["sizeMin"]
                        channel["sizeMin"] = channel["sizeMax"]
                        channel["sizeMax"] = tmp
                    output["Channels"].append(dict(channel))
                    channel = {"dir": ""}
                elif link == "<->":
                    output["ChannelConnection"] = "<->"

                    if channel["sizeMin"] > channel["sizeMax"]:
                        tmp = channel["sizeMin"]
                        channel["sizeMin"] = channel["sizeMax"]
                        channel["sizeMax"] = tmp
                    output["Channels"].append(dict(channel))
                    channel = {"dir": ""}
                elif link == "+":
                    output["ChannelConnection"] = "+"

                    if channel["sizeMin"] > channel["sizeMax"]:
                        tmp = channel["sizeMin"]
                        channel["sizeMin"] = channel["sizeMax"]
                        channel["sizeMax"] = tmp
                    output["Channels"].append(dict(channel))
                    channel = {"dir": ""}
                else:
                    # print("unknown link:", link, "for code", code)
                    pass

        # print(channel)
        if channel["sizeMin"] > channel["sizeMax"]:
            tmp = channel["sizeMin"]
            channel["sizeMin"] = channel["sizeMax"]
            channel["sizeMax"] = tmp

        output["Channels"].append(dict(channel))
        return output
        # === end of IzaFramework._get_refmat_channels()

    def _split_channel_range(self, size_str, code=""):
        size_min = None
        size_max = None
        dim = None
        link = ""
        size_str = size_str.replace(u"\u2194", "<->")

        pos = size_str.find("|")
        if pos >= 0:
            size_str = size_str[:pos]
            link = "|"
        pos = size_str.find("<->")
        if pos >= 0:
            size_str = size_str[:pos]
            link = "<->"
        pos = size_str.find("+")
        if pos >= 0:
            size_str = size_str[:pos]
            link = "+"

        dim = size_str.count("*")
        size_str = size_str.replace("*", "")

        size_range = size_str.split("x")
        if isinstance(size_range, list) and len(size_range) == 2:
            size_min = float(size_range[0])
            size_max = float(size_range[1])
        else:
            print("Error in the channel size:", size_str)

        return size_min, size_max, dim, link

    def _get_refmat_references(self, table, code):
        output = []
        if table is None:
            print("Invalid input data in _get_refmat_nameorig:", table)
            return output

        # print(table)
        tr_els = table.find_all("tr")
        ref_line = 0
        ref = {}
        for tr in tr_els[:]:
            short = tr.text.replace(u"\u00a0", " ").strip()

            if short.startswith("Reference"):
                # print("Found header 'Reference'")
                continue
            # print(tr)
            if short.startswith("Material name"):
                # print("Found header 'Material name'")
                ref_line = 0
                continue
            if short.startswith("Chemical formula"):
                # print("Found header 'Chemical formula'")
                ref_line = 0
                continue

            if short.lower().find("private communication") >= 0:
                # print("Found header 'private communication'")
                ref_line = 0
                output.append(dict(ref))
                ref = {}
                continue

            if 0 == ref_line:
                td_els = tr.find_all("td")
                ref["author"] = td_els[-1].text.strip()
                ref_line += 1
                continue

            if 1 == ref_line:
                td_els = tr.find_all("td")
                ref["title"] = td_els[-1].text.strip()
                ref_line += 1
                continue

            if 2 == ref_line:
                td_els = tr.find_all("td")
                short = td_els[-1].text.strip()
                ref["data"] = short

                # print("Checking year: '", short, ", code", code)
                year = None
                for i in range(len(short)-5):
                    # print(i, "of", len(short), short[i:])
                    if short[i] == "(" and short[i+5] == ")" and \
                      (short[i+1] == "1" or short[i+1] == "2"):
                        # year = short[i+1:i+5]
                        short = short[:i]
                        break

                # print(year, ">>", short)
                # words = short.split("(")
                # short = words[0]
                # if len(words) > 0:
                if year is not None:
                    # print(words, "in code", code)
                    # year = words[1]
                    ref["year"] = year.replace(")", "").strip()
                else:
                    print("Missing year, code", code)

                words = short.split(",")
                ref["journal"] = words[0].strip()
                ref["volume"] = words[1].strip()
                ref["pages"] = words[2].strip()
                # print(td_els[-1])
                adata = td_els[-1].find_all("a")
                if len(adata) > 0:
                    href = adata[0].get("href")
                    # print(href)
                    href = href.replace("http://doi.org/", "")
                    ref["doi"] = href.strip()

                ref_line = 0
                output.append(dict(ref))
                ref = {}
                continue

            # print(td_els[-1].text.strip())

        return output
        # === end of IzaFramework._get_refmat_nameorig()

    def _get_refmat_nameorig(self, table):
        output = {}
        if table is None:
            print("Invalid input data in _get_refmat_nameorig:", table)
            return output

        return output
        # === end of IzaFramework._get_refmat_nameorig()

    def getVolumeAndArea(self, table, name):
        # print("starting getVolumeAndArea(), name =", name)
        output = {}

        value = self.getVolProperty(table, "Occupiable Volume", name)
        if not value:
            print("No data for occup. vol. for framework", name)
        else:
            output |= value

        value = self.getVolProperty(table, "Accessible Volume", name)
        if not value:
            print("No data for access. vol. for framework", name)
        else:
            output |= value

        value = self.getVolProperty(table, "Occupiable Area", name)
        if not value:
            print("No data for occup. area. for framework", name)
        else:
            output |= value

        value = self.getVolProperty(table, "Accessible Area", name)
        if not value:
            print("No data for access. area. for framework", name)
        else:
            output |= value

        value = self.getVolProperty(table, "Specific Occupiable Area", name)
        if not value:
            print("No data for spec. occup. area. for framework", name)
        else:
            output |= value

        value = self.getVolProperty(table, "Specific Accessible Area", name)
        if not value:
            print("No data for spec. access. area. for framework", name)
        else:
            output |= value

        # print(json.dumps(output,indent=4))
        """
        value = self.getVolProperty(table, "
Maximum diameter of a sphere:
    that can be included    5.42    Å
        that can diffuse along
        """
        # print(output)
        return output
        pass  # IzaFramework.getVolumeAndArea()

    def getVolProperty(self, table, title, name):
        # print("Starting getVolProperty() for row", title)
        output = {}

        key = title.strip().replace(" ", "")

        tr_els = table.find_all('tr')
        # first row is the header, need to check it. TODO
        # if not
        # print(table)
        # print("nTr = ", len(tr_els))
        # print("tr = ", tr_els)
        # 1/0
        # print("key =", key)
        for ir, tr in enumerate(tr_els[1:]):
            # WARNING! There is an error (either in webpage, or in parser),
            #          but the first row cannot be extracted separately.
            #          So I just ignore it. Anyway it is a standard header.
            #          Maybe, it is due to colspan=9, but I did not check.

            # print("ir = ", ir)
            # print(tr)

            td_els = tr.find_all('td')
            """
            if 0 == ir:
                # WARNING! There is an error (either in webpage, or in parser),
                #          but the first row cannot be extracted separately.
                #          So I just ignore it. Anyway it is a standard header.
                #          Maybe, it is due to colspan=9, but I did not check.
                continue # I.e. Ignore this case.

                if len(td_els) != 1:
                    print("Error! In Volume prop first row must be one cell")
                    #print("      ", len(td_els), ":", td_els)
                    #print(td_els)
                td = td_els[0]
                short = td.text.strip().lower()
                if not short.startswith("Volumes and areas".lower()):
                    print("In volume prop first row must start with " + \
                           "'Volumes and areas' but got", td, "in", name)
                continue
            """

            if len(td_els) < 4:
                print("Error! Possibly too short row in", name)
                continue

            td = td_els[0]
            word = td.text.strip()
            key = word.strip().replace(" ", "")
            # print("word =", word)
            # if word.lower().startswith(title.lower()) or :
            if word.lower().startswith("Occupiable Volume".lower()):
                # print("Found", title, "at", ir)
                # print("      ", output)
                output[key] = {}

                td = td_els[1]
                output[key]["value"] = td.text.strip()
                output[key]["unit"] = "A^3"

                td = td_els[3]
                value = td.text.replace("(", "").strip()
                output[key]["percent"] = value
                # output[key]["unit"] = "%"
                # print("After:", output)

            elif word.lower().startswith("Accessible Volume".lower()):
                # print("Found", title, "at", ir)
                # print("      ", output)
                output[key] = {}

                td = td_els[1]
                output[key]["value"] = td.text.strip()
                output[key]["unit"] = "A^3"

                td = td_els[3]
                value = td.text.replace("(", "").strip()
                output[key]["percent"] = value
                # output[key]["unit"] = "%"
                # print("After:", output)

            elif word.lower().startswith("Occupiable Area".lower()):
                # print("Found", title, "at", ir)
                # print("      ", output)
                output[key] = {}

                td = td_els[1]
                output[key]["value"] = td.text.strip()
                output[key]["unit"] = "A^2"
                td = td_els[3]
                value = td.text.replace("(", "").strip()
                output[key]["pergram"] = value
                output[key]["pergramunit"] = "m^2/g"
                # print("After:", output)

            elif word.lower().startswith("Accessible Area".lower()):
                # print("Found", title, "at", ir)
                # print("      ", output)
                output[key] = {}

                td = td_els[1]
                output[key]["value"] = td.text.strip()
                output[key]["unit"] = "A^2"
                td = td_els[3]
                value = td.text.replace("(", "").strip()
                output[key]["pergram"] = value
                output[key]["pergramunit"] = "m^2/g"
                # print("After:", output)

            elif word.lower().startswith("Specific Occupiable Area".lower()):
                # print("Found", title, "at", ir)
                # print("      ", output)
                output[key] = {}

                td = td_els[1]
                output[key]["value"] = td.text.strip()
                output[key]["unit"] = "m^2/cm^3"
                # print("After:", output)

            elif word.lower().startswith("Specific Accessible Area".lower()):
                # print("Found", title, "at", ir)
                # print("      ", output)
                output[key] = {}

                td = td_els[1]
                output[key]["value"] = td.text.strip()
                output[key]["unit"] = "m^2/cm^3"
                # print("After:", output)
            else:
                pass

                # word.lower().startswith("".lower()) or
                # word.lower().startswith("".lower()) or
                # word.lower().startswith("".lower()) or

        # output[key]["value"] = "AAAAAAAAAAAA"
        # output[key]["unit" ] = "AAAAAAAAAAAA"

        # print(output)
        return output
        pass  # IzaFramework.getVolProperty()

    def get_table_title(self, table):
        """ Extract the left-most cell text from the table.
        """
        # print(table)
        # print(table.find_all('tr'))
        if table is None:
            return ""

        # tr_left = table.find_all('tr')[0]
        tr_els = table.find_all('tr')

        # Sometimes the first row is empty,
        # so I check the first two rows (if they exist):
        nTrToCheck = min(len(tr_els), 2)

        for i in range(nTrToCheck):
            tr = tr_els[i]
            # print("tr_left =", tr_left)
            td_els = tr.find_all('td')
            # print("td_elem =", td_elem)
            if len(td_els) > 1:
                for td in td_els:
                    # print(td)
                    pass
                title = td_els[1].get_text()
                title = title.replace(":", "")
                return title.strip()

        return ""
        pass  # IzaFramework._get_table_title()

    def getCellParam(self, tables):
        table = tables[0]
        title = self.get_table_title(table)
        # print(title, len(table)) #, len(table[0]))

        # tr_left = table.find_all('tr')[0]
        output = {}
        if "cell parameters" == title.lower():
            output["CellParameters"] = {}
            # output["CellParameters"]["a"] =

            tr_els = table.find_all('tr')
            # print("n tr_els =", len(tr_els), "=>", tr_els[4])

            td_els = tr_els[4].find_all('td')
            # print("n td_els =", len(td_els), "=>", td_els)

            # print("Beginning = '", td_els[1].get_text(), "'.", sep="")
            if td_els[1].get_text().startswith("RDLS"):
                # print("End = '", td_els[2].get_text(), "'.", sep="")
                r_dls = td_els[2].get_text()
                output["CellParameters"]["RDLS"] = float(r_dls.strip())

        else:
            print("Failed to find 'Cell Parameters' section")
        return output
        pass  # IzaFramework.()

    def getFWDensity(self, tables):
        table = tables[1]
        title = self.get_table_title(table)
        # print(table)
        # print(title, "expecting FW Density")

        output = {}
        if title.lower().startswith("framework density"):
            output["FrameworkDensity"] = {}
            # output["FrameworkDensity"]["a"] =
            # print("   >>>> In fw density", table)
            tr_els = table.find_all('tr')
            tr_el = tr_els[0]
            td_els = tr_el.find_all('td')
            for td in td_els[1:]:
                # print(" >>> ", td)
                break
            td = td_els[2].contents
            output["FrameworkDensity"]["value"] = float(td[0].split()[0])
            output["FrameworkDensity"]["unit"] = str(td[0].split()[1]) + "A^3"

        else:
            print("Failed to find 'Framework Density' section")

        return output
        pass  # IzaFramework.getFWDensity()

    def getTopoDensity(self, tables):
        table = tables[1]
        tr_els = table.find_all('tr')
        # print(">>>>>>>>>", tr_els)
        tr_el = tr_els[1]  # << Topological

        title = tr_el.find_all('td')[1].contents[0]
        # print("In topo density", title)

        output = {}
        if title.lower().startswith("topological density"):
            output["TopologicalDensity"] = {}
            # output["TopologicalDensity"]["a"] =
            # print("   >>>> In fw density", table)
            tr_els = table.find_all('tr')
            tr_el = tr_els[1]  # << Topological

            td_els = tr_el.find_all('td')
            for td in td_els[0:]:
                # print(" >>> ", td)
                break

            # print("as text:", td_els[2].text)
            if "TD" == td_els[2].text.split("=")[0].strip():
                td = td_els[2].contents
                # print(td[0], td[1], td[2])
                # output["TopologicalDensity"]["type"]  = "TD"
                value = float(td[2].replace("=", ""))
                output["TopologicalDensity"]["valueTD"] = value

            elif "TD10" == td_els[2].text.split("=")[0].strip():
                td = td_els[2].contents
                # print("---", td, ",", td[2].split("="), ",", td[2].strip())
                if len(td[2].strip()) > 0:
                    value = float(td[2].split("=")[1])
                    output["TopologicalDensity"]["valueTD10"] = value

            if len(td_els) > 3:

                # print("as text:", td_els[3].text)

                if "TD" == td_els[3].text.split("=")[0].strip():
                    td = td_els[3].contents
                    # print(td, ",", td[0].split("="), ",", td[0].strip())
                    # print(td[0], td[1], td[2])
                    # output["TopologicalDensity"]["type"]  = "TD"
                    value = float(td[0].split("=")[1])
                    output["TopologicalDensity"]["valueTD"] = value

                elif "TD10" == td_els[3].text.split("=")[0].strip():
                    td = td_els[3].contents
                    # print(td, ",", td[0].split("="), ",", td[0].strip())
                    if len(td[0].strip()) > 0:
                        value = float(td[0].split("=")[1])
                        output["TopologicalDensity"]["valueTD10"] = value

                pass

        # Verification:
        if "valueTD10" in output["TopologicalDensity"]:
            td10 = output["TopologicalDensity"]["valueTD10"]
            if td10 < 100.:
                print("Error: td10 may be too small:", td10, type(td10))

        if "valueTD" in output["TopologicalDensity"]:
            td = output["TopologicalDensity"]["valueTD"]
            if td > 10.:
                print("Error: td may be too big:", td, type(td))

        # print(output)
        return output

        pass  # IzaFramework.getTopoDensity()

    def getRingSize(self, tables):

        table = tables[1]
        # print("Ring size:", table)
        tr_els = table.find_all('tr')
        # print(">>>>>>>>>", tr_els)
        tr_el = tr_els[2]  # << RingSize

        title = tr_el.find_all('td')[1].contents[0]

        # title = self.get_table_title(table)
        # print("In ring size:", title)

        output = {}
        if title.lower().startswith("ring sizes"):
            output["RingSizes"] = []
            # output["TopologicalDensity"]["a"] =
            # print("   >>>> In fw density", table)
            tr_els = table.find_all('tr')
            tr_el = tr_els[2]  # << Ring sizes

            td_els = tr_el.find_all('td')
            for td in td_els[0:]:
                # print(" >>> ", td)
                break
            td = td_els[2].contents
            # print(td, type(td))
            # print(td[0].split())
            rings = td[0].split()
            # output["TopologicalDensity"]["type"]  = "TD"
            for r in rings:
                output["RingSizes"].append(int(r))
                # output["RingSizes"]["a"] = int(abc[0])
                # output["RingSizes"]["b"] = int(abc[1])
                # output["RingSizes"]["c"] = int(abc[2])

        # print(output)

        return output

        pass  # IzaFramework.getRingSize   ()

    def getChannelDim(self, tables):

        table = tables[1]
        # print("Ring size:", table)
        tr_els = table.find_all('tr')
        # print(">>>>>>>>>", tr_els)
        tr_el = tr_els[3]  # << ChannelDimension

        title = tr_el.find_all('td')[1].contents[0]

        # title = self.get_table_title(table)
        # print("In channel dimension:", title)

        output = {}
        if title.lower().startswith("channel dimensionality"):
            output["ChannelDimensions"] = {}
            # output["TopologicalDensity"]["a"] =
            # print("   >>>> In fw density", table)
            tr_els = table.find_all('tr')
            tr_el = tr_els[3]  # << ChannelDimension

            td_els = tr_el.find_all('td')
            for td in td_els[0:]:
                # print(" >>> ", td)
                break
            td = td_els[2].contents
            # print(td, type(td))
            # print(td[0].split())
            value = td[0]
            # print(value)
            output["ChannelDimensions"] = value

        # print(output)

        return output

        pass  # IzaFramework.getChannelDim ()

    def getSphereDiam(self, tables):
        table = tables[2]
        title = self.get_table_title(table)
        # print("in sphere diam title:", title)

        output = {}
        if title.lower().startswith("maximum diameter of a sphere"):
            output["SphereDiameter"] = {}
            table = tables[3]
            tr_els = table.find_all('tr')
            # print(tr_els)
            tr_el = tr_els[0]
            td_els = tr_el.find_all('td')
            td = td_els[2].contents
            # print(td)
            words = td[0].split()
            output["SphereDiameter"]["unit"] = "A"
            output["SphereDiameter"]["included"] = float(words[0])

            table = tables[4]
            tr_els = table.find_all('tr')
            # print(tr_els)
            tr_el = tr_els[0]
            td_els = tr_el.find_all('td')

            td = td_els[2].contents
            # print(td)
            words = td[0].split()
            output["SphereDiameter"]["a"] = float(words[1])

            td = td_els[3].contents
            # print(td)
            words = td[0].split()
            output["SphereDiameter"]["b"] = float(words[1])

            td = td_els[4].contents
            # print(td)
            words = td[0].split()
            output["SphereDiameter"]["c"] = float(words[1])

        # print(output)
        return output

        pass  # IzaFramework.getSphereDiam ()

    def getAccessVolume(self, tables):
        table = tables[5]
        # print(table)
        title = self.get_table_title(table)
        # print(title)

        output = {}
        if title.lower().strip().startswith("accessible volume"):
            output["AccessVolume"] = {}

            tr_els = table.find_all('tr')
            # print(tr_els)
            tr_el = tr_els[0]
            td_els = tr_el.find_all('td')
            # print(td_els)
            td = td_els[2].contents[0]
            # print(td)
            words = td.split()
            output["AccessVolume"]["value"] = float(words[0])
            output["AccessVolume"]["unit"] = "%"

        1/0
        # print(output)
        return output
        pass  # IzaFramework.getAccesVolume()

    def getOccupyVolume(self, tables):
        table = tables[5]
        # print(table)
        title = self.get_table_title(table)
        # print(title)

        output = {}
        if title.lower().strip().startswith("occupiable volume"):
            output["OccupyVolume"] = {}

            tr_els = table.find_all('tr')
            # print(tr_els)
            tr_el = tr_els[0]
            td_els = tr_el.find_all('td')
            # print(td_els)
            td = td_els[2].contents[0]
            # print(td)
            words = td.split()
            output["OccupyVolume"]["value"] = float(words[0])
            output["OccupyVolume"]["unit"] = "%"

        # print(output)
        return output
        pass  # IzaFramework.getOccupyVolume()

    def getABCSequence(self, tables, code=""):

        # table = tables[6]
        table, iTitle = self._get_table_by_title(tables, "abc sequence")
        # print(table)
        title = self.get_table_title(table)
        # print("In ABCSequence title:", title)

        output = {}
        if title.lower().strip().startswith("abc sequence"):
            output["ABCSequence"] = {}

            tr_els = table.find_all('tr')
            # print(tr_els)
            tr_el = tr_els[0]
            td_els = tr_el.find_all('td')
            # print(td_els)
            td = td_els[2].contents[0]
            # print(td)
            if td.find("or") >= 0:
                print("found 'or' in ABCSequence '", td,
                      "' for code ", code, sep="")
            if td.find("OR") >= 0:
                print("found 'OR' in ABCSequence '", td,
                      "' for code ", code, sep="")

            output["ABCSequence"] = []

            words = td.split("or")
            for w in words:
                w2 = w.replace("&nbsp", "").strip()
                output["ABCSequence"].append(w2)
            # output["ABCSequence"]["unit"]  = "%"

        # print(output)

        return output

        pass  # IzaFramework.getABCSequence()

    def getSecondaryBU(self, tables, code=""):

        # table = tables[6]
        table, iTitle = self._get_table_by_title(tables,
                                                 "secondary building units")
        # print(table)
        title = self.get_table_title(table)
        # print("In Secondary BU title:", title)

        output = {}
        if title.lower().strip().startswith("secondary building units"):
            output["SecondaryBU"] = {}

            tr_els = table.find_all('tr')
            # print(tr_els)
            tr_el = tr_els[0]
            td_els = tr_el.find_all('td')
            # print(td_els)
            td = td_els[2].contents[0]
            # print(td)

            output["SecondaryBU"] = []
            if td.find("or") >= 0:
                # print("found OR in SecondaryBU for code", code)
                words = td.split("or")
                for w in words:
                    w2 = w.replace("&nbsp", "").strip()
                    output["SecondaryBU"].append(w2)

            if td.find("AND") >= 0:
                # print("found AND in SecondaryBU '", td.strip(),
                #      "' for code ", code, sep="")
                td = td[:td.find("(")]

                words = td.split("AND")
                for w in words:
                    w2 = w.replace("&nbsp", "").strip()
                    output["SecondaryBU"].append(w2)
            # output["SecondaryBU"]["unit"]  = "%"

        # print(output)

        return output

        # === end of IzaFramework.getSecondaryBU()

    def _get_table_by_title(self, tables, title, minOrder=0):
        """
        Returnd the table and its order (integer number).
        If not found, return (None,None)

        minOrder is optional argument, if the minimum order is known.
        """

        for i in range(minOrder, len(tables)):
            table = tables[i]
            value = self.get_table_title(table)
            # print("value = ", value, ", i =", i)

            if value.lower().strip().startswith(title.lower()):
                # print("Found value")
                return table, i

        return None, None
        # === end of IzaFramework._get_table_by_title()

    def getCompositeBU(self, tables, msg="", code=""):

        # Header:
        table, iTitle = self._get_table_by_title(tables,
                                                 "composite building units")
        iCBUs = None
        # Chains:
        table, iChain = self._get_table_by_title(tables, "chains")
        table, iTiles = self._get_table_by_title(tables, "natural tiling")

        if iTitle:
            table = tables[iTitle]
            pass
        else:
            # print("Failed to find the Composite BU title")
            pass

        if iChain:
            if iChain - iTitle == 2:
                iCBUs = iTitle + 1
            # table = tables[iCBUs]
        else:
            # print("Failed to find the Chains title")
            pass

        if iTiles:
            # if (iTiles - iChain) == 1:
            #    pass # Do nothing
            if (iTiles - iTitle) == 2:
                if iChain:
                    pass  # Do nothing
                else:
                    iCBUs = iTiles - 1

        # print("Table order ids: Title:", iTitle, "CBU:", iCBUs,
        #      "Chain:", iChain, "Tiles:", iTiles)

        table = tables[iTitle]
        # print(table)
        title = self.get_table_title(table)
        # print("In Composite BU title:", title)
        # print([i for i in range(1, 4, 2) ])

        output = {}
        if title.lower().strip().startswith("composite building units"):
            output["CompositeBU"] = {}
            output["CompositeBU"]["cages"] = []
            output["CompositeBU"]["t-cages"] = []
            output["CompositeBU"]["chains"] = []

            if iCBUs:
                # print("Found CBU table at position", iCBUs)
                table = tables[iCBUs]

                tr_els = table.find_all('tr')

                # if len(tr_els) > 0:
                for ir in range(1, len(tr_els), 2):
                    # print(tr_els)
                    tr_el = tr_els[ir]
                    td_els = tr_el.find_all('td')
                    # print(td_els)
                    if len(td_els) == 0:
                        continue
                    for td in td_els[1:]:
                        # print(td)
                        # print(td.text)
                        if len(td.text.strip()) == 0:
                            continue
                        td = td.contents[0]
                        # print(" td = ", td)
                        word = td.find_all("div")  # .contents
                        word = td.contents[0].strip()
                        # print(word)
                        cName, tName = self.splitBrackets(word, msg="where?")
                        if cName:
                            output["CompositeBU"]["cages"].append(cName)
                        if tName:
                            output["CompositeBU"]["t-cages"].append(tName)

            if iChain:
                # print("Found Chain table at position", iChain)
                table = tables[iChain]

                tr_els = table.find_all('tr')

                if len(tr_els) > 1:
                    # print(">>>>>>>>>>>> More than one chain for", code)
                    pass
                # for ir in range(1, len(tr_els), 2):
                for ir, tr_el in enumerate(tr_els):
                    # print(tr_el)
                    # tr_el = tr_els[ir]
                    td_els = tr_el.find_all('td')
                    # print(td_els)
                    if len(td_els) < 3:
                        continue
                    td = td_els[2]
                    # for td in td_els[2]:
                    #    if td.text
                    #    print(td)
                    #    print(td.text)
                    if len(td.text.strip()) == 0:
                        continue
                    td = td.contents[0]
                    # print(" td = ", td)
                    word = td.text.strip()
                    # word = td.find_all("div") #.contents
                    # word = td.contents[0].strip()
                    # print(word)
                    # cName, tName = self.splitBrackets(word, msg = "where?")
                    output["CompositeBU"]["chains"].append(word)

        # print(output)

        return output
        # === end of IzaFramework.getCompositeBU()

    def splitBrackets(self, word, msg=""):
        # print(word, type(word))

        pattern = re.compile(r'\s*([\w-]+)\s*(?:\(\s*([\w-]+)\s*\))?\s*')
        # pattern = re.compile(r'\s*(\w+)\s*\(\s*(\w+)\s*\)\s*')
        # pattern = re.compile(r'(\w+)\((\w+)\)')
        match = pattern.match(word)
        if match:
            # matches = pattern.findall(word)
            # print(len(match.groups()))
            if 2 != len(match.groups()):
                print("Error in match find word", word, ", in ", msg)
            c_name = match.group(1)
            t_name = match.group(2)
            # print(match.group(0), " ==>", cName, "vs", tName)
        else:
            print("Failed to find match for word '" + word + "', " + msg)
            c_name = word
            t_name = word

        return c_name, t_name
        # === end of IzaFramework.splitBrackets()

    def getNaturalTile(self, tables):
        # table = tables[0]
        # title = self.get_table_title(table)
        # print(title)
        table, iTiles = self._get_table_by_title(tables, "natural tiling")

        output = {}
        output["NaturalTiles"] = []

        if table:
            tr_els = table.find_all('tr')

            if len(tr_els) > 1:
                # print(">>>>>>>>>>>>>>>>>>>>>> More than one chain")
                # for ir in range(1, len(tr_els), 2):
                for ir, tr_el in enumerate(tr_els):
                    # print(tr_el)
                    # tr_el = tr_els[ir]
                    td_els = tr_el.find_all('td')
                    # print(td_els)
                    if len(td_els) < 3:
                        continue
                    # td = td_els[2]
                    for td in td_els[1:]:
                        # if td.text
                        # print(td)
                        # print(td.text)
                        # td = td.contents
                        # print(" td = ", td.text)
                        if len(td.text.strip()) == 0:
                            continue
                        word = td.text.strip()
                    # word = td.find_all("div") #.contents
                    # word = td.contents[0].strip()
                        if not word.lower().startswith("natural tiling"):
                            output["NaturalTiles"].append(word)

        # print(output)
        return output
        # === end of IzaFramework.getNaturalTile()

    # def (self, tables):
    #    pass # IzaFramework.()

    def getTAtoms(self, table):
        atoms = {}
        atoms["TAtoms"] = []

        # print("warning: only 2 rows read")
        rows = table.find_all("tr")[1:]
        for row in rows:
            atom = {}
            # print (row)
            cells = row.find_all("td")
            # print("cells =", cells)
            # print(type(table))
            # print(type(cells))
            # print(type(cells[0]))

            for ic, cell in enumerate(cells):
                # print(type(cell), ":", cell)
                value = self.getCellContent(cell)
                # print(value)
                if 0 == ic:
                    atom["TAtomName"] = {}
                    atom["TAtomName"]["Name"] = value["p"]
                    atom["TAtomName"]["Id"] = value["sub"]

                    atom["CoordinateSequence"] = []
                elif ic < 13:
                    # print("coord seq =", value)
                    atom["CoordinateSequence"].append(int(value["p"]))
                elif ic < 14:
                    # print("cell = ", cell, type(cell))
                    # print("vert symb =", value)
                    # atom["VertexSymbol"] = value["p"]
                    vs = self.getVertexSymbol(cell)
                    atom["VertexSymbol"] = vs
                else:
                    print(" Error! too many columns in TAtoms table", ic)

                # print(cell)
                # print("atom = ", atom)
                # atom["key"] = value
                # atom["value"] = value

            atoms["TAtoms"].append(atom)
        return atoms

        pass  # IzaFramework.getTAtoms()

    def getVertexSymbol(self, cell):
        # print("Vert Symb from cell =", cell, type(cell))
        # value = self.getCellContent(cell)
        # print(value["p"])
        # print(value["p"][0])
        # print(hex(ord(value["p"][1])))
        # print(u"\u00B7")
        # print(value["p"].split(u"\u00B7"))

        # td = cell.find("div").get_text(strip=True) #.find("div")

        td_el = cell.find("div")  # .find("div")
        # print("td = ", td_el.get_text)
        # print("td = ", td_el.text)
        content = ''.join(map(str, td_el.contents))
        # print(content.split(u"\u00B7"))
        # print("ssss:", content)
        dot = u"\u00B7"  # This is a dot symbol, similar to TeX \cdoc symbol.
        vsArr = content.split(dot)

        output = []
        for vs in vsArr:
            # print(vs, type(vs))
            tag = BeautifulSoup("<div>"+vs+"</div>", "html.parser")
            # print(tag, type(tag))

            value = self.getCellContent(tag)
            # print("vs value = ", value)
            vsValue = {}
            vsValue["ring"] = value["p"].strip()
            if "sub" in value:
                vsValue["count"] = value["sub"].strip()
            # print("   > ", vsValue)
            output.append(vsValue)

        # value = td.find('div')
        # value = td.text
        # print("value = ", value)
        # print(value.split(u"\u00B7"))

        return output
        pass  # IzaFramework.getVertexSymbol()

    def getCellContent(self, cell):
        # print(cell)
        # print("------------")
        # print(cell.prettify())
        # cp = BeautifulSoup(cell, 'html.parser')

        # print(cell)
        cp = cell.find('div')
        div_text = ''.join([str(item) for item in cp.contents
                            if item.name != 'sub'])
        # print("div = ", cell.div.text)
        # print("div = ", div_text)
        # print("div = ", cell.p)
        # print("sub = ", cell.find('sub'))

        sub_el = cell.find('sub')
        # print(div_text, cp.find('sub').get_text())

        output = {}
        output["p"] = div_text
        if sub_el:
            output["sub"] = sub_el.get_text()

        return output
        """
        div_el = cell.find('div')
        value = ""
        if div_el:
            value = div_el.get_text(strip=False)
            sub = sub_el.get_text(strip=True)
            print(value, sub)

        else:
            print("No <div> elements found in cell", cell)
        #print(value)
        key = "a"
        #value = "aa"
        return key, value
        """
        # === end of IzaFramework.getCellContent()

    def get_framework_tables(self, html, name):

        soup = BeautifulSoup(html, 'html.parser')
        # table = soup.find('table', {'id': 'table_id'})
        tables = soup.find_all('table')

        # print(len(tables))
        table = tables[0]
        # print("table = ", type(table), table)

        output = []
        if table:
            tr_left = table.find_all('tr')[0]  # The left row of the html page
            td_el = tr_left.find('td')
            # print(td_el)
            # output.append(td_el)
            tbl_el = td_el.find_all('table')

            for tbl in tbl_el:
                output.append(tbl)
                # print("   >>>>>> table =", tbl)
            # print("Prop: \n",table.prettify())
            pass
        else:
            print("Table with Framework is not found:", name)

        return output
        return table
        # === end of IzaFramework.get_tramework_table()

    def getTAtomTable(self, html, name):
        # print("Starting T atoms")

        soup = BeautifulSoup(html, 'html.parser')
        # table = soup.find('table', {'id': 'table_id'})
        tables = soup.find_all('table')

        # print(len(tables))
        table = tables[1]
        # print("tables = ", type(table))
        if table:
            # print("Prop: \n",table.prettify())
            pass
        else:
            print("Table with T-atoms is not found:", name)

        return table
        # === end of IzaFramework.getTAtomTable()

    def getVolumeTable(self, html, name):
        soup = BeautifulSoup(html, 'html.parser')
        # table = soup.find('table', {'id': 'table_id'})
        tables = soup.find_all('table')

        # print(len(tables))
        table = tables[0]
        # print("tables = ", type(table))
        # print("tables = ", table)

        return table
        # === end of IzaFramework.getVolumeTable()

    def get_refmat_tables(self, html, name):

        soup = BeautifulSoup(html, 'html.parser')
        # table = soup.find('table', {'id': 'table_id'})
        tables = soup.find_all('table')

        # print(len(tables))
        table = tables[1]
        # print("table = ", type(table), table)

        output = []
        if table:
            tr_left = table.find_all('tr')[0]  # The left row of the html page
            td_el = tr_left.find('td')
            # print(td_el)
            # output.append(td_el)
            tbl_el = td_el.find_all('table')

            for tbl in tbl_el:
                output.append(tbl)
                # print("   >>>>>> table =", tbl)
            # print("Prop: \n",table.prettify())
        else:
            print("Table with Reference Material is not found:", name)

        # return output
        return tables
        # === end of IzaFramework.get_refmat_table()

    def nameToPath(self, theme, name):
        file_path = os.path.join(IZADATA, theme, name + ".html")
        return file_path
        # === end of IzaFramework.nameToPath()

    def _get_html_as_text(self, theme, name):

        file_path = self.nameToPath(theme, name)
        if os.path.isfile(file_path):
            html = self.readHtmlFile(file_path)
        else:
            html = self._read_url(theme, name)
            self.save_html_file(theme, name, html)

        # print(html)
        return html
        # === end of IzaFramework._get_html_as_text()

    def readHtmlFile(self, file_path):
        if os.path.isfile(file_path):
            with open(file_path, encoding="utf-8") as fp:
                lines = fp.readlines()

        output = "".join(lines)
        # print(output)

        # print(type(output))
        return output
        # === end of IzaFramework.readHtmlFile()

    def _read_url(self, theme, name):
        """ Read data from the IZA web-site.
        There are several copies of the web-site, one can set IZASITE.
        """
        print("Info: Loading data from web: ", theme, name)
        time.sleep(12.)

        if "coord" == theme:
            # url = "https://example.com/sample.html"
            url = IZASITE + "framework_cs.php?STC=" + name
        elif "frame" == theme:
            url = IZASITE + "framework.php?STC=" + name
        elif "volume" == theme:
            url = IZASITE + "framework_vol.php?STC=" + name
        elif "refmat" == theme:
            url = IZASITE + "material_tm.php?STC=" + name
        elif "mater" == theme:
            url = IZASITE + "material_rm.php?STC=" + name
        elif "tatom" == theme:
            url = IZASITE + "framework_coord.php?STC=" + name
        else:
            print("Invalid theme = '" + str(theme) + "'. ")

        response = requests.get(url)
        html = response.text
        # print(type(html))

        return html
        # === end of IzaFramework._read_url()

    def save_html_file(self, theme, name, html):
        file_path = self.nameToPath(theme, name)
        with open(file_path, "w", encoding="utf-8") as fp:
            fp.write(html)

        # === end of IzaFramework.save_html_file()

    def get_framework_volume(self):
        """
        Extract from cif files:
        1) unit cell volume of the Framework
        2) unit cell volume of the Reference material
        3) unit cell parameters.
        4)
        """
        print("Starting unit cell for framework:", self.framework)

        output = {}
        output |= self._compute_framewokr_from_cif()

        return output
        # === end of IzaFramework.get_framework_volume()

    def _add_one_cif_file(self, file_path):
        output = {}

        uuidDB = tools.UuidDB()

        if not os.path.isfile(file_path):
            print(f"Missing file '{file_path}' while reading from cif")
            return output
        ci = crystalinfo.CrystalInfo(uuidDB)

        # Remove <br> from cif file
        # print(">>>>>", "starting file_path")
        file_path = ci.cleanCif(file_path)[0]
        time.sleep(0.01)
        # print(">>>>>", file_path)

        cryst = crystaldata.CrystalData("PyMatGen", uuidDB)
        cryst.loadData(file_path, self.framework)
        cryst.evalPyMatGenUnitCell()

        # print(cryst.unitCellVolume.value)
        output["volume"] = cryst.unitCellVolume.value
        output["a"] = cryst.struct.lattice.a
        output["b"] = cryst.struct.lattice.b
        output["c"] = cryst.struct.lattice.c
        output["alpha"] = cryst.struct.lattice.alpha
        output["beta" ] = cryst.struct.lattice.beta
        output["gamma"] = cryst.struct.lattice.gamma

        return output
        # === end of IzaFramework._add_one_cif_file()

    def _compute_framewokr_from_cif(self):
        output = {}

        if "-" == self.framework[0]:
            name_tmp = self.framework.upper()[1:]
        else:
            name_tmp = self.framework.upper()
        file_path = os.path.join("CIF", name_tmp + ".cif")

        # output["UnitCellFramework"] = {}
        try:
            output["UnitCellFramework"] = self._add_one_cif_file(file_path)
        except:
            with open("log.err", "a", encoding="utf-8") as fp:
                fp.write("Failed to read '" + file_path + "'" + "\n")
            print(f"Failed to read '{file_path}'.")

        if "-" == self.framework[0]:
            name_tmp = self.framework.upper()[1:]
        else:
            name_tmp = self.framework.upper()

        file_path = os.path.join("LI-CIF", "Li-" + name_tmp + ".cif")
        try:
            output["UnitCellReference"] = self._add_one_cif_file(file_path)
        except:
            with open("log.err", "a", encoding="utf-8") as fp:
                fp.write("Failed to read '" + file_path + "'" + "\n")
            print(f"Failed to read '{file_path}'.")

# Replace the URL with the actual URL of the HTML page containing the table

# Make a request to fetch the HTML content
# Parse the HTML content using BeautifulSoup

# Replace 'table_id' with the actual ID or class of the table to extract

# Print the table content

        # print(output)
        return output
        # === end of IzaFramework._compute_framewokr_from_cif()

    # === end if class IzaFramework


def save_volume(frames):
    vol = []
    for frame in frames:
        if "volume" in frame["UnitCellFramework"]:
            #vol.append(frame["UnitCellFramework"]["volume"])
            vol.append([frame["Framework"],
                        frame["UnitCellFramework"]["volume"],
                        frame["UnitCellFramework"]["a"],
                        frame["UnitCellFramework"]["b"],
                        frame["UnitCellFramework"]["c"],
                        frame["UnitCellFramework"]["alpha"],
                        frame["UnitCellFramework"]["beta"],
                        frame["UnitCellFramework"]["gamma"]])
        else:
            print("Missing volume for", frame["Framework"])

    vol.sort()

    with open("volume.dat", "w", encoding="utf-8") as fp:
        for val in vol:
            for el in val:
                fp.write(str(el) + "\t")
            #fp.write(str(val) + "\n")
            fp.write("\n")

if __name__ == "__main__":

    all_frames = []

    # zList = zeolist.getZeoList("main")
    zList = zeolist.getZeoList(["main", "new"])

    # zList = zList[:1]
    # zList = zList[150:]
    # zList = ["-EWT"]
    # zList = ["YFI"]
    # zList = ["ABW"]
    # zList = ["ABW", "YUG"]
    # zList = ["AFG"]
    # zList = ["AFN"]
    # zList = ["MWW"]
    # zList = ["ACO"]
    # zList = ["CGF"]
    # zList = ["ITV"]
    # zList = ["MAR", "ITV"]

    # Clean the log file:
    file_p = open("log.err", "w", encoding="utf-8")
    file_p.close()

    print("Number of zeolite frameworks in the list:", len(zList))
    # for name in [ "ABW", "ACO", "MTW" ]:
    for i_name, name in enumerate(zList):
        # print("Starting frame", name)

        if len(zList) > 1 and "-EWT" == name:
            # Note: -EWT for some reason it has completely different structure
            # I will do it manually.
            # continue
            pass

        x = {}

        fw = IzaFramework(name, i_name=i_name)

        # x |= fw.get_framework_data("ABW")
        # x = fw.get_framework_data("MTW")

        # print("Hidden framework_data()")
        # Extract the framework data only:
        x |= fw.get_framework_data()

        # print("Hidden refmat_data()")
        # x |= fw.get_refmat_data()
        # x |= fw.get_refmat_data(name, i_name=i_name)

        x |= fw.get_framework_volume()
        # TODO
        # Extract individual materials and references:
        # html = fw._get_html_as_text("mater", name)
        # html = fw._get_html_as_text("tatom", name)

        all_frames.append(x)

    save_volume(all_frames)

    fileOut = os.path.join("ontozeolite", "izadata", "iza-data.json")
    with open(fileOut, "w", encoding="utf-8") as fp:
        json.dump(all_frames, fp, indent=4)
