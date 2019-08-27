package org.cam.ceb.como.nist.webbook.parser;

import java.util.HashMap;
import java.util.Map;

import org.cam.ceb.como.nist.webbook.DownloadHTML;
import org.cam.ceb.como.nist.webbook.info.NISTSpeciesInfo;

public class NISTWebBookParser {
	/**
	 * A map created to store the type of parsed file vs information extracted from the file. 
	 */
	Map<String, NISTSpeciesInfo> data = new HashMap<String, NISTSpeciesInfo>();
	
	static NISTWebBookParser nistWebBookParser;
	static NISTSDFParser nistSDFParser;
	/**
	 * The default constructor
	 */
	public NISTWebBookParser() {
		data = new HashMap<String, NISTSpeciesInfo>();
		nistSDFParser = new NISTSDFParser();
	}
	
	
	public static void main(String[] args) {
		nistWebBookParser = new NISTWebBookParser();
		try{
			nistWebBookParser.parseHTML("D:\\msff2\\Documents\\Data\\NIST\\ChemSpecies\\html\\ThermoPhase\\");
			nistWebBookParser.parseSDF("D:\\msff2\\Documents\\Data\\NIST\\download\\");
		}catch(Exception e){
			e.printStackTrace();
		}
		nistWebBookParser.display();

	}
	
	private void display(){
		for(String key:data.keySet()){
			NISTSpeciesInfo speciesInfo = data.get(key);
			DownloadHTML.display(speciesInfo);
			System.out.println("Energy:"+speciesInfo.getEnergy());
			System.out.println("name:"+speciesInfo.getName());
			System.out.println("Boiling point temperature:"+speciesInfo.gettBoil().getValue());
			System.out.println("Boiling point temperature units:"+speciesInfo.gettBoil().getUnits());
			System.out.println("Critical point temperature:"+speciesInfo.gettCritical().getValue());
			System.out.println("Critical point temperature units:"+speciesInfo.gettCritical().getUnits());
			if(speciesInfo.getpTriple()!=null){
				System.out.println("Triple point pressure:"+speciesInfo.getpTriple().getValue());
				System.out.println("Triple point pressure units:"+speciesInfo.getpTriple().getUnits());
			}
			if(speciesInfo.gettFusion()!=null){
				System.out.println("Fusion (or melting) temperature:"+speciesInfo.gettFusion().getValue());
				System.out.println("Fusion (or melting) temperature units:"+speciesInfo.gettFusion().getUnits());
			}
			System.out.println(" - - -  - - - -  - - - - - -  - - - - - - - -");
		}
	}
	
	/**
	 * Scans the current HTML file to extract information of interest. 
	 * 
	 * @param htmlFolderPath
	 * @throws Exception
	 */
	public void parseHTML(String htmlFolderPath) throws Exception{
		if(htmlFolderPath!=null && !htmlFolderPath.isEmpty())
		{
			DownloadHTML.parsingHTML(htmlFolderPath, data);
		}
	}
	
	/**
	 * Scans the current SDF file to extract information of interest. 
	 * 
	 * @param sdfFolderPath
	 * @throws Exception
	 */
	public void parseSDF(String sdfFolderPath) throws Exception{
		if(sdfFolderPath!=null && !sdfFolderPath.isEmpty())
		{
			nistSDFParser.parseSDF(sdfFolderPath, data);
		}
	}

}
