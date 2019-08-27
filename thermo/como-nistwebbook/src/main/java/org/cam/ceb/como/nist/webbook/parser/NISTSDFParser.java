package org.cam.ceb.como.nist.webbook.parser;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Collection;
import java.util.Map;

import org.cam.ceb.como.chem.filemgmt.filter.SDFFileFilter;
import org.cam.ceb.como.nist.webbook.info.NISTSpeciesInfo;

/**
 * This class parses content of the current SDF (structured data-file) file. 
 * 
 * @author msff2
 *
 */
public class NISTSDFParser {

	protected String path = ""; 
	NISTSpeciesInfo reader = null;
	
	public void parseSDF(String sdfFolderPath, Map<String, NISTSpeciesInfo> data) throws Exception {
		SDFFileFilter sdfFilter = new SDFFileFilter();

		// read the SDF files
		Collection<File> sdfFiles = sdfFilter.getValidFiles(sdfFolderPath, true);

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

		for (Map.Entry<String, NISTSpeciesInfo> speciesData : data.entrySet()) {
		}
	}
	
	private void parse() throws IOException{
		BufferedReader br = openSourceFile(path);
		String line = "";
		while((line=br.readLine())!=null){
			if(line.startsWith(">") && line.contains("<ELECTRONIC.ENERGY>")){
				parseEnergy(br);
			}
		}
	}
	
	private void parseEnergy(BufferedReader br) throws IOException{
		String line = "";
		while((line=br.readLine())!=null){
			if(!line.isEmpty()){
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
}
