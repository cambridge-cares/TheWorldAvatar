package com.cmclinnovations.jps.upload.test;

import static org.junit.Assert.*;

import org.junit.Test;

import com.cmclinnovations.jps.upload.CompChemUpload;
/**
 * This JUnit test uploads a calculation without establishing a link to the<br>
 * corresponding OntoSpecies entry.
 * 
 * @author msff2
 *
 */
public class CompChemUploadTest2 {

	@Test
	public void test() {
		try{
		CompChemUpload compChemUpload = new CompChemUpload();
		compChemUpload.setCalculationFileName("login-skylake.hpc.cam.ac.uk_113420049902577.log");
		System.out.println(getClass().getClassLoader().getResource("login-skylake.hpc.cam.ac.uk_113420049902577.log").getPath());
		compChemUpload.setCalculationFilePath(getClass().getClassLoader().getResource("login-skylake.hpc.cam.ac.uk_113420049902577.log").getPath());
		compChemUpload.upload();
		}catch(Exception e){
			fail(e.getMessage());
		}
	}

}
