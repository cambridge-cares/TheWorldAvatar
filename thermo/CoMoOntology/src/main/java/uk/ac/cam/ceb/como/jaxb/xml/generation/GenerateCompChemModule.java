package uk.ac.cam.ceb.como.jaxb.xml.generation;

import uk.ac.cam.ceb.como.io.chem.file.jaxb.Module;

public class GenerateCompChemModule {
	
	public static Module generateInitialModule() {
		
		Module module = new Module();
		
		module.setDictRef("cc:initialization");
		module.setId("jobInitialization");
		
		return module;
	}
	
	public static Module generateFinalModule() {
		
		Module module = new Module();
		
		module.setDictRef("cc:finalization");
		module.setId("finalization");
		
		return module;
	}
	
	public static Module getEnvironmentModule() {
		
		Module module = new Module();
		
		module.setDictRef("cc:environment");
		module.setId("environment");
		
    	return module;
	}
}