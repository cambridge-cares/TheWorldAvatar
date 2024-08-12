import os
import sys
import re
import unittest
import csv

PROCESSING_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), os.pardir))
# Add the processing directory to the system path
sys.path.append(PROCESSING_DIR)
from rework_ontomops.update_kg  import config_a_box_updates
import rework_ontomops.update_kg as KG

def get_csv_rows(csv_file_path):
    """Yield rows from the CSV file."""
    with open(csv_file_path, mode='r') as file:
        csv_reader = csv.DictReader(file)
        for row in csv_reader:
            yield row

class TestMOPFormulas(unittest.TestCase):

    def setUp(self):
        """Set up the UpdateKG instance and load CSV data."""
        script_dir                  = os.path.dirname(os.path.abspath(__file__))
        a_box_updates_config_mop    = config_a_box_updates(os.path.join(script_dir, "../OntoMOPConnection.env"))

        self.updaterCBU             = KG.UpdateKG(
            query_endpoint          = a_box_updates_config_mop.SPARQL_QUERY_ENDPOINT    ,
            update_endpoint         = a_box_updates_config_mop.SPARQL_UPDATE_ENDPOINT   ,
            kg_user                 = a_box_updates_config_mop.KG_USERNAME           ,
            kg_password             = a_box_updates_config_mop.KG_PASSWORD
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
        mop_csv                         = os.path.join(script_dir, "../Data/r2_mops.csv")
        cbu_csv                         = os.path.join(script_dir, "../Data/CBUs.csv")
        self.rows                       = list(get_csv_rows(mop_csv))
        self.rows_cbu                   = list(get_csv_rows(cbu_csv))

    def check_mop_formula(self, mop_formula_value:str) -> str:
        if mop_formula_value in self.mop_formula_map:
            mop_formula_value               = self.mop_formula_map[mop_formula_value]
        # Test MOP Formulas 
        vars_mop                            =     "?MOPIRI"
        where_mop                           =  f"""?MOPIRI om:hasMOPFormula "{mop_formula_value}" ."""
        output                              = self.updaterCBU.query_triple(where_mop, vars_mop)
        self.assertGreater(len(output), 0, f"There should be more than 0 MOP(s) with the value")
        return output[0]
    
    def check_am_value_symmetrie(self, mop_iri:str, geom:str, p_group:str) -> None:
        print(mop_iri)
        print(geom,p_group)
        if geom in self.assembly_model_map:
            geom                            = self.assembly_model_map[geom]
        vars_am                             = "?AmIri"
        where_am                            =   f"""<{mop_iri}>     om:hasAssemblyModel         ?AmIri               .
                                                ?AmIri              om:hasSymmetryPointGroup    "{p_group}"         ;
                                                                    rdfs:label                  "{geom}"            .    """
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
        print(am_iri, gbu)
        vars_gbu                            = "     ?GBUIRI ?GBU_num_IRI"
        where_gbu                           = f""" <{am_iri}>       om:hasGenericBuildingUnit       ?GBUIRI             .
                                                    ?GBU_num_IRI    om:isNumberOf                   ?GBUIRI             ;
                                                                    om:hasUnitNumberValue           {gbu[1]}          .
                                                    ?GBUIRI 	    om:hasPlanarity 			    ?planarity		    ;
                                                                    om:hasModularity			    ?modularity 		.
  	                                                                BIND(CONCAT(str(?modularity), "-", str(?planarity)) AS ?combined) 
                                                                    FILTER(?combined = "{gbu[0]}") 
                                                                """
        output_gbu                          = self.updaterCBU.query_triple(where_gbu, vars_gbu)
        self.assertGreater(len(output_gbu), 0, f"There should be more than 0 gbus(s) with matching formula.")
    
    def check_all(self, mop_formula, p_group, geom, cbu1, cbu2, gbu1, gbu2):
        print(geom, p_group)
        """Faster than loads of individual queries and might be a bit more accurate but more difficult to work out what is wrong."""
        vars                                = "     ?MOPIRI"
        where                               = f"""  ?MOPIRI         om:hasMOPFormula                "{mop_formula}"         ;
                                                                    om:hasAssemblyModel             ?AMIRI                  ;
                                                                    om:hasChemicalBuildingUnit      ?CBUIRI                 .
                                                    ?AMIRI          om:hasSymmetryPointGroup        "{p_group}"             ;
                                                                    rdfs:label                      "{geom}"                ;    
                                                                    om:hasGenericBuildingUnit       ?GBUIRI                 .
                                                    ?CBUIRI1        om:hasCBUFormula                "[{cbu1}]"              .
                                                    ?CBUIRI2        om:hasCBUFormula                "[{cbu2}]"              .                
                                                    ?GBU_num_IRI1   om:isNumberOf                   ?GBUIRI1                ;
                                                                    om:hasUnitNumberValue           {gbu1[1]}               .
                                                    ?GBUIRI1 	    om:hasPlanarity 			    ?planarity1		        ;
                                                                    om:hasModularity			    ?modularity1 		    .
  	                                                                BIND(CONCAT(str(?modularity1), "-", str(?planarity1)) AS ?combined1) 
                                                                    FILTER(?combined1 = "{gbu1[0]}") 
                                                    ?GBU_num_IRI2   om:isNumberOf                   ?GBUIRI2                ;
                                                                    om:hasUnitNumberValue           {gbu2[1]}               .
                                                    
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


    def check_all_cbu(self, cbu_formula, charge_value, mw_value):
        print(float(charge_value))
        """Faster than loads of individual queries and might be a bit more accurate but more difficult to work out what is wrong."""
        vars                                = "     ?CBUIRI ?charge ?MWvalue"
        where                               = f"""  ?CBUIRI         om:hasCBUFormula                "{cbu_formula}"                 ;
                                                                    os:hasCharge                    ?ChargeIRI                      ;
                                                                    os:hasMolecularWeight           ?MWIRI                          .
                                                    ?MWIRI          os:value                        ?MWvalue                        .
                                                    ?ChargeIRI      os:value                        ?charge                         .
                                                                """
        output                              = self.updaterCBU.query_triple(where, vars)[0]
        self.assertAlmostEqual(float(output["charge"]), charge_value,1)
        self.assertAlmostEqual(float(output["MWvalue"]), mw_value, 1)
        self.assertGreater(len(output), 0, f"Something is wrong with a cbu element.")

    def test_all_rows_cbu(self):
        """Run all checks for each row in the CSV."""
        failed_cbu                                      = []
        for i, row in enumerate(self.rows_cbu):
            print(i+1, row)
            cbu_formula                                 = row["CBU_Formula"]
            charge_value                                = row["Formal charge"]
            mw_value                                    = row["MolecularWeight"]
            try: 
                self.check_all_cbu(cbu_formula, float(charge_value), float(mw_value))
            except:
                if cbu_formula not in ['[(C28H34N2O2)Cr(CO2)2]', '[(C6H4)2C2)(CO2)2)]', '[(N2(C6H4)2)(CO2)2)]', '[(C28H34N2O2)Cr(CO2)2]', '[(C5H3N)(C6H4)2(CO2)2]', '[(C6H3)O(CH2)7CH3(CO2)2]', '[Pd2]', '[V5O10]']:
                    print("------ failed ------")
                    failed_cbu.append(cbu_formula)
                

        print("failed cbus: ", failed_cbu, len(failed_cbu))



    def test_all_rows(self):
        for i, row in enumerate(self.rows):
            print(i+1, row)
            # check MOP formula
            print(row)
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
            gbu1, gbu2                                  = self.split_geometry(assembly_model_value)
            self.check_all(mop_formula, point_group_value, assembly_model_value, cbus[0], cbus[1], gbu1, gbu2)
    # add tests for empty classes / missing connections

    def split_geometry(self, geometry) ->str:
        """
        Deduce GBU by splitting the Geometry value string into two parts.
        
        Args:
            geometry (str): The geometry value string, e.g. (4-planar)x12(2-bent)x24.
        
        Returns:
            tuple: A tuple containing two parts of the GBU, e.g. ('4-planar', '2-bent').
        """
        pattern             = re.compile(r'\(([^)]+)\)x(\d+)')
        gbu_num             = pattern.findall(geometry)
        return gbu_num[0], gbu_num[1]

if __name__ == '__main__':
    unittest.main()
