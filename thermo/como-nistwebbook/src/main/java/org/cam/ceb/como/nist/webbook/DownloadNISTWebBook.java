package org.cam.ceb.como.nist.webbook;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;

import org.cam.ceb.como.nist.webbook.info.NISTSpeciesId;
import org.cam.ceb.como.nist.webbook.parser.NISTSpeciesList;
import org.cam.ceb.como.nist.webbook.parser.NISTSpeciesListParser;

public class DownloadNISTWebBook {

	public static void main(String[] args) throws FileNotFoundException, IOException {
		
		String speciesFilePath = "C:\\Users\\NK\\Downloads\\NIST\\download\\species.txt";
		
		/**
		 * 
		 * @nk510@cam.ac.uk
		 * For given ID it downloads file with thermochemistry data.
		 * 
		 */
		
		NISTSpeciesListParser nistSpeciesListParser = new NISTSpeciesListParser();
		
		nistSpeciesListParser.setPath(speciesFilePath);
		
		nistSpeciesListParser.parse();
		
		NISTSpeciesList nistSpeciesList = nistSpeciesListParser.getNISTSpeciesList();
		
		List<NISTSpeciesId> listNistSpeciesId = nistSpeciesList.get();
		
		int count= 0;
		
		for(NISTSpeciesId nistSpeciesId : listNistSpeciesId) {
			
			if(!nistSpeciesId.getCASRegNr().matches("N/A")) {
				
			System.out.println(nistSpeciesId.getName() + " " + nistSpeciesId.getFormula()+ ", " +nistSpeciesId.getCASRegNr());
			
			count++;
			
		URLDownload.fileUrl("https://webbook.nist.gov/cgi/cbook.cgi?Str3File="+nistSpeciesId.getCASRegNr(), nistSpeciesId.getCASRegNr()+".sdf", "C:\\Users\\NK\\Downloads\\NIST\\download\\");
		
		URLDownload.fileUrl("https://webbook.nist.gov/cgi/cbook.cgi?Str2File="+nistSpeciesId.getCASRegNr(), nistSpeciesId.getCASRegNr()+".mol", "C:\\Users\\NK\\Downloads\\NIST\\download\\");
		
			}
			
		}
		
		System.out.println("- - -  - - - - -  - - - - - - ");
		
		System.out.println("Number of species: " +  count);
		
}
	
	}

