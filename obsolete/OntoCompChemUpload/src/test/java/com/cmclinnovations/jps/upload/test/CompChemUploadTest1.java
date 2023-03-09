package com.cmclinnovations.jps.upload.test;

import static org.junit.Assert.*;

import org.junit.Test;

import com.cmclinnovations.jps.upload.CompChemUpload;

/**
 * This unit test uploads a calculation with a link established to the<br>
 * corresponding OntoSpcies entry. 
 * 
 * @author msff2
 *
 */
public class CompChemUploadTest1 {

	@Test
	public void test(){
		try{
		CompChemUpload compChemUpload = new CompChemUpload();
		compChemUpload.setCalculationFileName("login-skylake.hpc.cam.ac.uk_113420049902577.log");
		System.out.println(getClass().getClassLoader().getResource("login-skylake.hpc.cam.ac.uk_113420049902577.log").getPath());
		compChemUpload.setCalculationFilePath(getClass().getClassLoader().getResource("login-skylake.hpc.cam.ac.uk_113420049902577.log").getPath());
		compChemUpload.setOntoSpeciesIRI("http://www.theworldavatar.com/kb/ontospecies/3ad49265-0d58-3827-ba0f-18201693f82b.owl#3ad49265-0d58-3827-ba0f-18201693f82b");
		String response = compChemUpload.upload();
		System.out.println("received UUID response:"+response);
		}catch(Exception e){
			fail(e.getMessage());
		}
	}

}
