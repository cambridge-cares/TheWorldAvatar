import os
import sys
import re
import unittest

# Define the processing directory path two levels up from the current file
PROCESSING_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), os.pardir, os.pardir))
# Add the processing directory to the system path
sys.path.append(PROCESSING_DIR)

import src.updateKG.UpdateKG as KG
import test_fun
class TestMOPFormulas(unittest.TestCase):

    def setUp(self):
        """Set up the UpdateKG instance and load CSV data."""
        self.updaterCBU         = KG.UpdateKG(
            query_endpoint      = test_fun.QUERY_ENDPOINT    ,
            update_endpoint     = test_fun.UPDATE_ENDPOINT   ,
            kg_user             = test_fun.KG_USER           ,
            kg_password         = test_fun.KG_PASSWORD
        )
        self.assembly_model_map = {
            "(4-pyramidal)x6(3-planar)x8"   : "(3-planar)x8(4-pyramidal)x6" ,
            "(3-planar)x4(3-pyramidal)x4"   : "(3-pyramidal)x4(3-planar)x4" ,
            "(3-planar)x4(2-bent)x6"        : "(2-bent)x6(3-planar)x4"      ,
            "(3-planar)x8(2-bent)x12"       : "(2-bent)x12(3-planar)x8"
            }
        self.mop_formula_map = {
            '[Mo2]12[C6H3OH(CO2)2]24'                       : "[Mo2]12[(C6H3OH)(CO2)2]24"                   ,
            "[(CHC6HO3)4(C3H6OH)4]6[Cu3]8"                  : "[Cu3]8[(CHC6HO3)4(C3H6OH)4]6"                ,
            "[(CHC6HO3)4(C5H11)4]6[Co3]8"                   : "[Co3]8[(CHC6HO3)4(C5H11)4]6"                 ,
            "[(C6HO3)4(C4H8)4]6[Mg3]8"                      : "[Mg3]8[(C6HO3)4(C4H8)4]6"                    ,
            "[(C6HO3)4(C4H8)4]6[V3O3]8"                     : "[V3O3]8[(C6HO3)4(C4H8)4]6"                   ,
            "[(C6HO3)4(C4H8)4]6[Ni3]8"                      : "[Ni3]8[(C6HO3)4(C4H8)4]6"                    ,
            "[(CHC6HO3)4(C3H6OH)4]6[Ni3]8"                  : "[Ni3]8[(CHC6HO3)4(C3H6OH)4]6"                ,
            "[(C6H3)(CO2)3]4[Zr3O(OH)3(C5H5)3]4"            : "[Zr3O(OH)3(C5H5)3]4[(C6H3)(CO2)3]4"          ,
            "[(C6H3)(CO2)3]4[V6O6(OCH3)9(SO4)]4"            : "[V6O6(OCH3)9(SO4)]4[(C6H3)(CO2)3]4"          ,
            "[(C3N3)(C6H4)3(CO2)3]4[V6O6(OCH3)9(SO4)]4"     : "[V6O6(OCH3)9(SO4)]4[(C3N3)(C6H4)3(CO2)3]4"   , 
            "[(C3N3)(C6H4)3(CO2)3]4[V7O10(OCH3)9]4"         : "[V7O10(OCH3)9]4[(C3N3)(C6H4)3(CO2)3]4"       , 
            "[(C6H3)(C6H4)3(CO2)3]4[Zr3O(OH)3(C5H5)3]4"     : "[Zr3O(OH)3(C5H5)3]4[(C6H3)(C6H4)3(CO2)3]4"   , 
            "[(C6H3)(C6H4)3(CO2)3]4[Fe3O(SO4)3(C5H5N)3]4"   : "[Fe3O(SO4)3(C5H5N)3]4[(C6H3)(C6H4)3(CO2)3]4" , 
            "[(C6H3)(C6H4)3(CO2)3]4[V6O6(OCH3)9(SO4)]4"     : "[V6O6(OCH3)9(SO4)]4[(C6H3)(C6H4)3(CO2)3]4"   , 
            "[(C6H3)(CO2)3]4[Cu2(C10H6N2O2C2H6)2]6"         : "[Cu2(C10H6N2O2C2H6)2]6[(C6H3)(CO2)3]4"       ,
            "[(C6H3)(CO2)3]4[Mo2(HCN2C12H8C2H6)2]6"         : "[Mo2(HCN2C12H8C2H6)2]6[(C6H3)(CO2)3]4"       ,
            "[(C6H3)(CO2)3]4[Mo2(HCN2C12H8O2C2H6)2]6"       : "[Mo2(HCN2C12H8O2C2H6)2]6[(C6H3)(CO2)3]4"     ,
            "[(C6H3)(CO2)3]4[Rh2(HCN2C12H8O2C2H6)2]6"       : "[Rh2(HCN2C12H8O2C2H6)2]6[(C6H3)(CO2)3]4"     ,
            "[(C6H3)(CO2)3]8[Cu2(C10H10N2)2]12"             : "[Cu2(C10H10N2)2]12[(C6H3)(CO2)3]8"  
        }
        # Load the CSV data
        self.rows                       = list(test_fun.get_csv_rows(test_fun.CSV_FILE_PATH))

    def check_mop_formula(self, mop_formula_value:str) -> str:
        if mop_formula_value in self.mop_formula_map:
            mop_formula_value           = self.mop_formula_map[mop_formula_value]
        # Test MOP Formulas
        vars_mop                        =     "?MOPIRI"
        where_mop                       =  f"""?MOPIRI om:hasMOPFormula "{mop_formula_value}" ."""
        output                          = self.updaterCBU.query_triple(where_mop, vars_mop)
        self.assertGreater(len(output), 0, f"There should be more than 0 MOP(s) with the value")
        return output[0]
    
    def check_am_value_symmetrie(self, mop_iri:str, geom:str, p_group:str) -> None:
        print(mop_iri)
        print(geom,p_group)
        if geom in self.assembly_model_map:
            geom                            = self.assembly_model_map[geom]
        vars_am                             = "?AmIri"
        where_am                            =   f""" <{mop_iri}>    om:hasAssemblyModel         ?AmIri          .
                                                ?AmIri          om:hasSymmetryPointGroup    "{p_group}"     ;
                                                                os:value                    "{geom}"        .    """
        output_am                           = self.updaterCBU.query_triple(where_am, vars_am)
        self.assertGreater(len(output_am), 0, f"There should be more than 0 MOP(s) with the geometry and point group")
        return output_am[0]
    
    def check_cbu(self, mop_iri, cbu):
        vars_cbu                            = "     ?CBUIRI"
        where_cbu                           = f""" <{mop_iri}>      om:hasChemicalBuildingUnit  ?CBUIRI         .
                                                    ?CBUIRI         om:hasCBUFormula            "[{cbu}]"       ."""
        output_cbu                          = self.updaterCBU.query_triple(where_cbu, vars_cbu)
        self.assertGreater(len(output_cbu), 0, f"There should be more than 0 cbus(s) with matching formula.")

    def check_gbu(self, am_iri, gbu):
        vars_gbu                            = "     ?GBUIRI ?GBU_num_IRI"
        where_gbu                           = f""" <{am_iri}>       om:hasGenericBuildingUnit       ?GBUIRI             .
                                                    ?GBU_num_IRI    om:isNumberOf                   ?GBUIRI             ;
                                                                    os:value                        "{gbu[1]}"          .
                                                    ?GBUIRI 	    om:hasPlanarity 			    ?planarity		    ;
                                                                    om:hasModularity			    ?modularity 		.
  	                                                                BIND(CONCAT(str(?modularity), "-", str(?planarity)) AS ?combined) 
                                                                    FILTER(?combined = "{gbu[0]}") 
                                                                """
        output_gbu                          = self.updaterCBU.query_triple(where_gbu, vars_gbu)
        self.assertGreater(len(output_gbu), 0, f"There should be more than 0 gbus(s) with matching formula.")
    
    def test_all(self, mop_formula, p_group, geom, cbu1, cbu2, gbu1, gbu2):
        """Faster than loads of individual queries and might be a bit more accurate but more difficult to work out what is wrong."""
        vars                                = "     ?MOPIRI"
        where                               = f"""  ?MOPIRI         om:hasMOPFormula                "{mop_formula}"         ;
                                                                    om:hasAssemblyModel             ?AMIRI                  ;
                                                                    om:hasChemicalBuildingUnit      ?CBUIRI                 .
                                                    ?AMIRI          om:hasSymmetryPointGroup        "{p_group}"             ;
                                                                    os:value                        "{geom}"                ;    
                                                                    om:hasGenericBuildingUnit       ?GBUIRI                 .
                                                    ?CBUIRI1        om:hasCBUFormula                "[{cbu1}]"              .
                                                    ?CBUIRI2        om:hasCBUFormula                "[{cbu2}]"              .                
                                                    ?GBU_num_IRI1   om:isNumberOf                   ?GBUIRI1                ;
                                                                    os:value                        "{gbu1[1]}"             .
                                                    ?GBUIRI1 	    om:hasPlanarity 			    ?planarity1		        ;
                                                                    om:hasModularity			    ?modularity1 		    .
  	                                                                BIND(CONCAT(str(?modularity1), "-", str(?planarity1)) AS ?combined1) 
                                                                    FILTER(?combined1 = "{gbu1[0]}") 
                                                    ?GBU_num_IRI2   om:isNumberOf                   ?GBUIRI2                ;
                                                                    os:value                        "{gbu2[1]}"             .
                                                    
                                                    ?GBUIRI2 	    om:hasPlanarity 			    ?planarity2		        ;
                                                                    om:hasModularity			    ?modularity2 		    .
  	                                                                BIND(CONCAT(str(?modularity2), "-", str(?planarity2)) AS ?combined2) 
                                                                    FILTER(?combined2 = "{gbu2[0]}") 
                                                                """
        output                              = self.updaterCBU.query_triple(where, vars)
        try:
            self.assertGreater(len(output), 0, f"Something is wrong with an element.")
        except:
            print("Error in overall query. Continue with single queries to find issue.")
            mop_iri                                     = self.check_mop_formula(mop_formula)["MOPIRI"]
            am_iri                                      = self.check_am_value_symmetrie(mop_iri, geom, p_group)["AmIri"]
            self.check_cbu(mop_iri, cbu1)
            self.check_cbu(mop_iri, cbu2)
            self.check_gbu(am_iri, gbu1)
            self.check_gbu(am_iri, gbu2)
        
        
    def test_all_rows(self):
        """Run all checks for each row in the CSV."""
        for i, row in enumerate(self.rows):
            print(i+1, row)
            # check MOP formula
            mop_formula                                 = row["MOP Formula"]
            if mop_formula in self.mop_formula_map:
                mop_formula                             = self.mop_formula_map[mop_formula]
            # check assembly model
            assembly_model_value, point_group_value     = row['Assembly Model'].split('___')
            if assembly_model_value in self.assembly_model_map:
                assembly_model_value                    = self.assembly_model_map[assembly_model_value]
            # remove "("" and ")"
            point_group_value                           = point_group_value.replace("(", "").replace(")", "")
            # check the CBUs
            cbus                                        = re.findall(r'\[([^\[\]]+)\]', mop_formula)
            # check the gbu and gbu_num
            gbu1, gbu2                                  = test_fun.split_geometry(assembly_model_value)
            self.test_all(mop_formula, point_group_value, assembly_model_value, cbus[0], cbus[1], gbu1, gbu2)


if __name__ == '__main__':
    unittest.main()
