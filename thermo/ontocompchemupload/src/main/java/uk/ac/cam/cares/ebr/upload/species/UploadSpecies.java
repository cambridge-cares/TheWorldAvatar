package uk.ac.cam.cares.ebr.upload.species;

import java.io.IOException;

import uk.ac.cam.cares.ebr.manager.FolderManager;

public class UploadSpecies {

	static String csvFilePath ="C:\\Users\\NK\\Documents\\cas_reg_id_species_2.csv";
	static String speciesFolderPath = "C:\\Users\\NK\\Documents\\species";
	
	public static void main(String[] args) throws IOException {
	
	HttpRequest.uploadSpecies(FolderManager.getOntoSpeciesCasRegIDAndIRI(csvFilePath), speciesFolderPath, ".g09");
	
	}
}