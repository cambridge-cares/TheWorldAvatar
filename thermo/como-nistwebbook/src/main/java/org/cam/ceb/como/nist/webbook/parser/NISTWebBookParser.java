package org.cam.ceb.como.nist.webbook.parser;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import org.cam.ceb.como.nist.webbook.DownloadHTML;
import org.cam.ceb.como.nist.webbook.info.NISTSpeciesInfo;
import org.cam.ceb.como.nist.webbook.thermochem.NISTEnthalpy;

public class NISTWebBookParser {
	/**
	 * This file contains information about valence electrons of atoms.
	 */
	private static final String FILE_NAME_ATOM_INFO = "atom.csv";
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
	
    // Gets the path of resources folder
    private String getFileFromResources(String fileName) throws IOException{
        ClassLoader classLoader = getClass().getClassLoader();

        URL resource = classLoader.getResource(fileName);
        if (resource == null) {
            throw new IllegalArgumentException("file is not found!");
        } else {
            return new File(resource.getFile()).getAbsolutePath();
        }

    }
	
    /**
     * The main method of this class.
     * 
     * @param args
     * @throws IOException
     */
	public static void main(String[] args) throws IOException{
		nistWebBookParser = new NISTWebBookParser();
		try{
			nistWebBookParser.parseHTML("D:\\msff2\\Documents\\Data\\NIST\\ChemSpecies\\html\\");
			nistWebBookParser.parseSDF("D:\\msff2\\Documents\\Data\\NIST\\download\\", nistWebBookParser.getFileFromResources(FILE_NAME_ATOM_INFO));
		}catch(Exception e){
			e.printStackTrace();
		}
		nistWebBookParser.display();
	}
	
	/**
	 * Displays all property values extracted from NIST Chemistry WebBook. 
	 * 
	 */
	private void display(){
		for(String key:data.keySet()){
			NISTSpeciesInfo speciesInfo = data.get(key);
			DownloadHTML.display(speciesInfo);
			if(speciesInfo.getEnergy()!=null){
				System.out.println("Energy:"+speciesInfo.getEnergy());
			}
			System.out.println("Paired Electrons:"+speciesInfo.getPairedElectrons());
			System.out.println("Unpaired Electrons:"+speciesInfo.getUnpairedElectrons());
			System.out.println("Total number of valence Electrons:"+speciesInfo.getElectrons());
			if(speciesInfo.getBondType()!=null){
				for(int i=0;i<speciesInfo.getBondType().size();i++){
					for(String atom:speciesInfo.getBondType().get(i).keySet()){
						System.out.println("Atom "+(i+1)+ " has the following bond type(s):"+speciesInfo.getBondType().get(i).get(atom));
					}
				}
			}
			if(speciesInfo.getName()!=null && !speciesInfo.getName().isEmpty()){
				System.out.println("name:"+speciesInfo.getName());
			}
			if(speciesInfo.gettBoil()!=null){
				System.out.println("Boiling point temperature:"+speciesInfo.gettBoil().getValue());
				System.out.println("Boiling point temperature units:"+speciesInfo.gettBoil().getUnits());
				if(speciesInfo.gettBoil().getReference()!=null && !speciesInfo.gettBoil().getReference().isEmpty()){
					System.out.println("Boiling point temperature reference:"+speciesInfo.gettBoil().getReference());
				}
			}
			if(speciesInfo.gettCritical()!=null){
				System.out.println("Critical point temperature:"+speciesInfo.gettCritical().getValue());
				System.out.println("Critical point temperature units:"+speciesInfo.gettCritical().getUnits());
				if(speciesInfo.gettCritical().getReference()!=null && !speciesInfo.gettCritical().getReference().isEmpty()){
					System.out.println("Critical point temperature reference:"+speciesInfo.gettCritical().getReference());
				}
			}
			if(speciesInfo.getpTriple()!=null){
				System.out.println("Triple point pressure:"+speciesInfo.getpTriple().getValue());
				System.out.println("Triple point pressure units:"+speciesInfo.getpTriple().getUnits());
				if(speciesInfo.getpTriple().getReference()!=null && !speciesInfo.getpTriple().getReference().isEmpty()){
					System.out.println("Triple point pressure reference:"+speciesInfo.getpTriple().getReference());
				}
			}
			if(speciesInfo.gettFusion()!=null){
				System.out.println("Fusion (or melting) temperature:"+speciesInfo.gettFusion().getValue());
				System.out.println("Fusion (or melting) temperature units:"+speciesInfo.gettFusion().getUnits());
				if(speciesInfo.gettFusion().getReference()!=null && !speciesInfo.gettFusion().getReference().isEmpty()){
					System.out.println("Fusion (or melting) temperature reference:"+speciesInfo.gettFusion().getReference());
				}
			}
			if(speciesInfo.getEnthalpy()!=null && speciesInfo.getEnthalpy().size()>0){
				for(NISTEnthalpy enthalpy: speciesInfo.getEnthalpy()){
					System.out.println("Enthalpy of formation value:"+enthalpy.getValue());
					System.out.println("Enthalpy of formation units:"+enthalpy.getUnits());
					if(enthalpy.getReference()!=null && !enthalpy.getReference().isEmpty()){
						System.out.println("Enthalpy of formation reference:"+enthalpy.getReference());
					}
				}
			}
			if(speciesInfo.getPhase()!=null && !speciesInfo.getPhase().trim().isEmpty()){
				System.out.println("Phase:"+speciesInfo.getPhase());
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
	public void parseSDF(String sdfFolderPath, String pathToAtoms) throws Exception{
		if(sdfFolderPath!=null && !sdfFolderPath.isEmpty())
		{
			nistSDFParser.setPathToAtoms(pathToAtoms);
			nistSDFParser.parseSDF(sdfFolderPath, data);
		}
	}

}
