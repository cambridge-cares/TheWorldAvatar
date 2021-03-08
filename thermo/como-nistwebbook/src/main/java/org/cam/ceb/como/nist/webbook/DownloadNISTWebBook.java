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
		int countNonCasSpec= 0;
		
		for(NISTSpeciesId nistSpeciesId : listNistSpeciesId) {
			if(!nistSpeciesId.getCASRegNr().matches("N/A")) {				
				System.out.println(nistSpeciesId.getName() + " " + nistSpeciesId.getFormula()+ ", " +nistSpeciesId.getCASRegNr());
				count++;
				if(count>14554){
					delay(2);
					URLDownload.fileUrl("https://webbook.nist.gov/cgi/cbook.cgi?Str3File="+nistSpeciesId.getCASRegNr(), nistSpeciesId.getCASRegNr()+".sdf", "D:\\msff2\\Documents\\Data\\NIST\\download\\");
					delay(3);
					URLDownload.fileUrl("https://webbook.nist.gov/cgi/cbook.cgi?Str2File="+nistSpeciesId.getCASRegNr(), nistSpeciesId.getCASRegNr()+".mol", "D:\\msff2\\Documents\\Data\\NIST\\download\\");
					delay(2);
					URLDownload.fileUrl("https://webbook.nist.gov/cgi/cbook.cgi?ID="+nistSpeciesId.getCASRegNr()+"&Units=SI&cTG=on&cIR=on&cTC=on&cTZ=on&cTP=on&cMS=on&cTR=on&cUV=on&cIE=on&cGC=on&cIC=on&cES=on&cDI=on&cSO=on", nistSpeciesId.getCASRegNr()+".html", "D:\\msff2\\Documents\\Data\\NIST\\ChemSpecies\\html\\");
					System.out.println("----------------------\nDownloaded: "+count+" species.\n----------------------");
//					if(count == 25000){
//						break;
//					}
				}
			}else{
				countNonCasSpec++;
			}
		}
		
		System.out.println("- - -  - - - - -  - - - - - - ");
		
		System.out.println("Number of species: " +  count);
		System.out.println("Total number of species: " +  (countNonCasSpec+count));
		
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

