package org.cam.ceb.como.nist.webbook.parser;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.cam.ceb.como.chem.filemgmt.filter.SDFFileFilter;
import org.cam.ceb.como.nist.webbook.info.NISTSpeciesInfo;
import java.util.HashMap;

/**
 * This class parses content of the current SDF (structured data-file) file. 
 * 
 * @author msff2
 *
 */
public class NISTSDFParser {

	protected String path;
	protected String pathToAtoms;
	NISTSpeciesInfo reader = null;
	List<String> lines;
	Map<String, Integer> atomVsElectronMap = new HashMap<String, Integer>();
	List<Map<String, Integer>> atomVsBondList = new ArrayList<Map<String, Integer>>();
	Map<String, Integer> atomVsBondMap = new HashMap<String, Integer>();
	
	/**
	 * A parser designed to extract data from all SDF files.
	 * 
	 * @param sdfFolderPath
	 * @param data
	 * @throws Exception
	 */
	public void parseSDF(String sdfFolderPath, Map<String, NISTSpeciesInfo> data) throws Exception {
		SDFFileFilter sdfFilter = new SDFFileFilter();
		// Reads the SDF files
		Collection<File> sdfFiles = sdfFilter.getValidFiles(sdfFolderPath, true);
		// Populate a list with atom vs valence electrons.   
		fillAtomVsElectronList();
		for (File f : sdfFiles) {
			if(data.containsKey(f.getName().replace(".sdf", ""))){
				reader = data.get(f.getName().replace(".sdf", ""));
			} else{
				reader = new NISTSpeciesInfo();
			}
			setPath(f.getAbsolutePath());
			parse();
			data.put(f.getName().replace(".sdf", ""), reader);
		}
	}
	
	/**
	 * The global SDF parser.
	 * 
	 * @throws IOException
	 */
	private void parse() throws IOException{
		BufferedReader br = openSourceFile(path);
		String line = "";
		lines = new ArrayList<String>();
		while((line=br.readLine())!=null){
			lines.add(line);
		}
		parseAtomicBonds(lines);
		parseEnergy(lines);
	}
	
	private void parseAtomicBonds(List<String> lines){
		parseElementalBonds(lines);
		int pairedElectrons = 0;
		for(int i=0;i<atomVsBondList.size();i++){
			Map<String, Integer> temp = atomVsBondList.get(i);
			Integer bond = new Integer(0);
			for(String str:temp.keySet()){
				bond = temp.get(str);
				pairedElectrons = pairedElectrons + bond.intValue() * 2;
			}
		}
		reader.setPairedElectrons(pairedElectrons);
	}
	
	private void parseElementalBonds(List<String> lines){
		boolean flag = false;
		int index = -2;
		atomVsBondList = new ArrayList<Map<String,Integer>>();
		for(String line:lines){
			if(line.contains("Copyright by the U.S. Sec.")){
				flag = true;
			}
			String[] tokens = line.trim().split("\\s+");
			if(flag && ++index>=1 && tokens.length>=10 && atomVsElectronMap.containsKey(tokens[3])){
				atomVsBondMap = new HashMap<String, Integer>();
				atomVsBondMap.put(tokens[3], new Integer(0));
				atomVsBondList.add(atomVsBondMap);
			} else if(index>=1 && tokens.length>=3){
				Map<String, Integer> temp = atomVsBondList.get(Integer.parseInt(tokens[0])-1);
				String key = "";
				Integer bond = new Integer(0);
				for(String str:temp.keySet()){
					key = str;
					bond = temp.get(str);
					bond = new Integer(bond.intValue()+Integer.parseInt(tokens[2]));
				}
				temp.put(key, bond);
				atomVsBondList.remove(Integer.parseInt(tokens[0])-1);
				atomVsBondList.add(Integer.parseInt(tokens[0])-1, temp);
			}
			if(line.contains("M  END")){
				break;
			}
		}
	}
	
	/**
	 * The energy parser.
	 * 
	 * @param lines
	 * @throws IOException
	 */
	private void parseEnergy(List<String> lines) throws IOException{
		boolean flag = false;
		for(String line:lines){
			if(line.startsWith(">") && line.contains("<ELECTRONIC.ENERGY>")){
				flag = true;
				continue;
			}
			if(flag && !line.isEmpty()){
				reader.setEnergy(line.trim());;
				break;
			}
		}
	}
	
	/**
	 * Returns the path of SDF files.
	 * 
	 * @return
	 */
	public String getPath() {
		return path;
	}
	
	/**
	 * Adds the path of SDF files.
	 * 
	 * @param path
	 */
	public void setPath(String path) {
		this.path = path;
	}
	
	/**
	 * Returns the path to the file that contains list of atoms.
	 * 
	 * @return
	 */
	public String getPathToAtoms() {
		return pathToAtoms;
	}
	
	/**
	 * Sets the path to the file that contains list of atoms.
	 * 
	 * @param pathAtoms
	 */
	public void setPathToAtoms(String pathToAtoms) {
		this.pathToAtoms = pathToAtoms;
	}
	
	/**
	 * Parses atoms and their valence electrons from a CSV file to fill</br>
	 * out the corresponding in-memory map.
	 * 
	 * @throws IOException
	 */
	private void fillAtomVsElectronList() throws IOException{
		if(pathToAtoms!=null && !pathToAtoms.isEmpty()){
			BufferedReader br = openSourceFile(pathToAtoms);
			String line;
			br.readLine();
			while((line=br.readLine())!=null){
				String[] tokens = line.split(",");
				if(tokens.length>=10 && isInteger(tokens[9])){
					atomVsElectronMap.put(tokens[2], new Integer(tokens[9]));
				}
			}
		}
	}
	
	/**
	 * Creates and returns an instance of the BufferedReader class.
	 * 
	 * @param absolutePath
	 *            the path plus name of the file being read
	 * @return
	 * @throws IOException
	 */
	private BufferedReader openSourceFile(String absolutePath)
			throws IOException {
		return new BufferedReader(new InputStreamReader(new FileInputStream(
				absolutePath), "UTF-8"));
	}
	
	/**
	 * Checks if the string contains an integer.
	 * 
	 * @param str
	 * @return
	 */
	public static boolean isInteger(String str) {
		  return str.matches("\\d+(\\d+)?");
	}

}
