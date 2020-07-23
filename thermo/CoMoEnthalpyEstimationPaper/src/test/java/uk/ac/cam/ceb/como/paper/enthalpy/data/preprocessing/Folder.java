package uk.ac.cam.ceb.como.paper.enthalpy.data.preprocessing;

import java.io.File;

/**
 * List of folder created for using across all tests.
 * 
 * @author msff2
 *
 */
public enum Folder {
	
	COMPOUNDS_REF_TI("test_data/Gaussian/ti/"),
	COMPOUNDS_REF_HCO("test_data/Gaussian/hco/"),
	COMPOUNDS_REF_HCO_HD("test_data/Gaussian/hco_hd/"),
	REF_POOL_TI("test_data/csv/ref_scaled_kJperMols_v8.csv"),
	REF_POOL_HCO("test_data/csv/ref-enthalpy_scaled_kJperMol.csv"),
	REF_POOL_HCO_HD("test_data/csv/calc-enthalpy_scaled_kJperMol-junit-hd.csv"),
	REACTIONS_TI_ISG("test_data/test_results/ti_isg/"),
	REACTIONS_TI_ISD("test_data/test_results/ti_isd/"),
	REACTIONS_HCO_ISG("test_data/test_results/hco_isg/"),
	REACTIONS_HCO_ISD("test_data/test_results/hco_isd/"),
	REACTIONS_HCO_HD("test_data/test_results/hco_hd/"),
	CROSS_VALIDATION(System.getProperty("user.home").concat("/")),
	VALID_TEST_RESULT_ISD_TI_115("valid-test-results-isd-ti-1-1-5"),
	VALID_TEST_RESULT_ISG_TI_115("valid-test-results-isg-ti-1-1-5"),
	VALID_TEST_RESULT_ISG_HCO_110("valid-test-results-isg-hco-1-1-0"),
	VALID_TEST_RESULT_ISG_HCO_115("valid-test-results-isg-hco-1-1-5"),
	VALID_TEST_RESULT_ISD_HCO_110("valid-test-results-isd-hco-1-1-0"),
	VALID_TEST_RESULT_ISD_HCO_115("valid-test-results-isd-hco-1-1-5"),
	VALID_TEST_RESULT_HD_HCO_110("valid-test-results-hd-hco-1-1-0"),
	ISG_TI_115("isg_ti_115"),
	ISG_HCO_115("isg_hco_115"),
	ISG_HCO_110("isg_hco_110"),
	ISD_TI_115("isd_ti_115"),
	ISD_HCO_110("isd_hco_110"),
	ISD_HCO_115("isd_hco_115"),
	HD_HCO_110("hd_hco_110"),
	TEMP_FOLDER(System.getProperty("user.home").concat(File.separator));
	
	
	private String folderName;
	private Folder(String folderName){
		this.folderName = folderName;
	}
	
	public String getFolderName(){
		return folderName;
	}
	
}
