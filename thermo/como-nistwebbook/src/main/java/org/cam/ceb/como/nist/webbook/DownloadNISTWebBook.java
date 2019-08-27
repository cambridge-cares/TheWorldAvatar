package org.cam.ceb.como.nist.webbook;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.cam.ceb.como.nist.webbook.info.NISTSpeciesId;
import org.cam.ceb.como.nist.webbook.parser.NISTSpeciesList;
import org.cam.ceb.como.nist.webbook.parser.NISTSpeciesListParser;

public class DownloadNISTWebBook {

	public static void main(String[] args) throws FileNotFoundException, IOException {
		
		String speciesFilePath = "D:\\msff2\\Documents\\Data\\NIST\\input\\species.txt";
		
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
			delay(5);
			URLDownload.fileUrl("https://webbook.nist.gov/cgi/cbook.cgi?Str3File="+nistSpeciesId.getCASRegNr(), nistSpeciesId.getCASRegNr()+".sdf", "D:\\msff2\\Documents\\Data\\NIST\\download\\");
			delay(5);
			URLDownload.fileUrl("https://webbook.nist.gov/cgi/cbook.cgi?Str2File="+nistSpeciesId.getCASRegNr(), nistSpeciesId.getCASRegNr()+".mol", "D:\\msff2\\Documents\\Data\\NIST\\download\\");
			delay(5);
			URLDownload.fileUrl("https://webbook.nist.gov/cgi/cbook.cgi?ID="+nistSpeciesId.getCASRegNr(), nistSpeciesId.getCASRegNr()+".html", "D:\\msff2\\Documents\\Data\\NIST\\ChemSpecies\\html\\");
			delay(5);
			URLDownload.fileUrl("https://webbook.nist.gov/cgi/cbook.cgi?ID="+nistSpeciesId.getCASRegNr()+"&Units=SI&Mask=4#Thermo-Phase", nistSpeciesId.getCASRegNr()+".html", "D:\\msff2\\Documents\\Data\\NIST\\ChemSpecies\\html\\ThermoPhase\\");			
			}
		}
		
		System.out.println("- - -  - - - - -  - - - - - - ");
		
		System.out.println("Number of species: " +  count);
		
}
	/**
	 * Adds an amount of delay specified by user. 
	 * 
	 * @param t an integer quantity with units in second.
	 */
	private static void delay(int t){
		try{
		TimeUnit.SECONDS.sleep(t);
		}catch(InterruptedException e){
			e.printStackTrace();
		}
	}
	
	}

