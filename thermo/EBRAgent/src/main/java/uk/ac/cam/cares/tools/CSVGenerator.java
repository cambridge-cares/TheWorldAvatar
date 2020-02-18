package uk.ac.cam.cares.tools;

import java.io.FileWriter;
import java.io.IOException;
import java.util.LinkedList;

import uk.ac.cam.ceb.como.nist.info.NISTSpeciesId;

public class CSVGenerator {

	
	private LinkedList<String> scvData = new LinkedList<String>();
	
	public static void generateCSVFile(LinkedList<NISTSpeciesId> nistSpeciesLinkedList, String csvFilePath)  {
		
		
		try {
			
		FileWriter writer = new FileWriter(csvFilePath);
		
		
		}catch(IOException e) {
			
			e.printStackTrace();
		}
		
	}
	
}