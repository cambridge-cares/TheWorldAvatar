package uk.ac.ceb.como.properties;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;

import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.nio.file.Files;
import java.nio.file.attribute.BasicFileAttributes;
import java.text.ParseException;
import java.text.SimpleDateFormat;

import java.time.LocalDate;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.TreeMap;

import org.eclipse.rdf4j.repository.RepositoryConnection;

import uk.ac.ceb.como.query.QueryManager;


public class PropertiesManager {

	/**
	 * @author NK510
	 * @param inputStream the Input Stream.
	 * @return Properties (key, value) given in .properties file for self growing knowledge graph statistics project
	 */
	public static Properties loadProperties(InputStream inputStream) {

		Properties properties = new Properties();

		try {

			if (inputStream == null) {

				return properties;
			}

			// load a properties file from class path.
			properties.load(inputStream);

		} catch (IOException ex) {

			ex.printStackTrace();

		} finally {

			if (inputStream != null) {

				try {

					inputStream.close();

				} catch (IOException e) {

					e.printStackTrace();

				}
			}
		}

		return properties;

	}
	
	/**
	 * 
	 * @param inputDate A month given by name.
	 * @return Month given by number.
	 */
	public static String getNumberFromMonthName (String inputDate) {
		
		String monthNumber = "";
		
		if(inputDate.equalsIgnoreCase("Jan")) {monthNumber ="01";return monthNumber;}
		if(inputDate.equalsIgnoreCase("Feb")) {monthNumber ="02";return monthNumber;}
		if(inputDate.equalsIgnoreCase("Mar")) {monthNumber ="03";return monthNumber;}
		if(inputDate.equalsIgnoreCase("Apr")) {monthNumber ="04";return monthNumber;}
		if(inputDate.equalsIgnoreCase("May")) {monthNumber ="05";return monthNumber;}
		if(inputDate.equalsIgnoreCase("Jun")) {monthNumber ="06";return monthNumber;}
		if(inputDate.equalsIgnoreCase("Jul")) {monthNumber ="07";return monthNumber;}		
		if(inputDate.equalsIgnoreCase("Aug")) {monthNumber ="08";return monthNumber;}
		if(inputDate.equalsIgnoreCase("Sep")) {monthNumber ="09";return monthNumber;}
		if(inputDate.equalsIgnoreCase("Oct")) {monthNumber ="10";return monthNumber;}
		if(inputDate.equalsIgnoreCase("Nov")) {monthNumber ="11";return monthNumber;}
		if(inputDate.equalsIgnoreCase("Dec")) {monthNumber ="12";return monthNumber;}
		
		return monthNumber;
	}	

	/**
	 * @author NK510
	 * @param filesList list of Files stored in given folder including sub folders. 
	 * @param level the level of a folder in folder hierarchy 
	 * @param speciesMap the species hash map that contains unique OWL file name  
	 * @throws IOException
	 */
	static void getAllFiles(File[] filesList, int level,LinkedHashMap<String,String> speciesMap) throws IOException  
    {
		
    for (File f : filesList)  
    {        
    if(f.isFile()) {
        	
    BasicFileAttributes basicAttribute = Files.readAttributes(f.getAbsoluteFile().toPath(), BasicFileAttributes.class);
			    
	String[] date = basicAttribute.creationTime().toString().split("T");
	
	speciesMap.put(f.getName(), date[0]);
              
    }else if(f.isDirectory())  
    {  
 
    getAllFiles(f.listFiles(), level + 1,speciesMap);
    
    }
    
    }
    
    }
/**
 * @author NK510
 * @param folderPath the folder that contains OWL files uploaded to JPS knowledge graph.
 * @return the map that contains unique OWL file name and created date (uploaded date)
 * @throws MalformedURLException the ex
 * @throws IOException
 * @throws InterruptedException
 */
	
public  LinkedHashMap<String,String> getFrequencyOfSpeciesPerDate(String folderPath) throws IOException{
		
		LinkedHashMap<String,String> speciesMap = new LinkedHashMap<String,String>();
		
		File maindir = new File(folderPath);
		
        if(maindir.exists() && maindir.isDirectory()) 
        { 
        	File fileList[] = maindir.listFiles();            
          
            getAllFiles(fileList, 0,speciesMap);  
       } 
        
		LinkedHashMap<String,String> speciesFrequecniesPerDate = new LinkedHashMap<String,String>();
		
		for(Map.Entry<String,String> map: speciesMap.entrySet()) {
			
			int count = Collections.frequency(new ArrayList<String>(speciesMap.values()), map.getValue());
			
			if(!speciesFrequecniesPerDate.containsKey(map.getValue())) {
				 
				speciesFrequecniesPerDate.put(map.getValue(),String.valueOf(count));
			}
		}
		
		Map<String, String> treeMap = new TreeMap<String, String>(speciesFrequecniesPerDate);
		
		LinkedHashMap<String,String> linkedHashMap = new LinkedHashMap<String,String>(treeMap);
		
		return linkedHashMap;
	}

	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * Iterates through source map and adds key value in target map if that key value does not exist in target map. The method adds "0" frequency for number of uploaded species.
	 * 
	 * @param sourceMap the source map of number of uploaded species per given date.
	 * @param targetMap the target map of number of uploaded species per given date.
	 * @return the updated target map of number of uploaded species per given date.
	 * @throws InterruptedException 
	 */
	public LinkedHashMap<String, String> updateFrequenciesMapData(Map<String,String> sourceMap, Map<String,String> targetMap) throws InterruptedException{
		
		for(Map.Entry<String, String> m: sourceMap.entrySet()) {
			
		if(!targetMap.containsKey(m.getKey())){
				
				targetMap.put(m.getKey(), "0");
		} 
		
		}
		
		/**
		 * sort target map by key values.
		 */
		Map<String, String> treeMap = new TreeMap<String, String>(targetMap);
		
		LinkedHashMap<String,String> linkedHashMap = new LinkedHashMap<String,String>(treeMap);
		
		return linkedHashMap;
		
	}
	
	/**
	 * 
	 * @param lastDate the latest date when OWL file is uploaded to RDF4J
	 * @return the date that is three months earlier than lastDate
	 * @throws ParseException
	 */
	public static LocalDate getDateThreeMonthsEarlier(LinkedList<String> labelList, int numberOfMonths) throws ParseException {
		
	     Date currentDate = new SimpleDateFormat("yyyy-MM-dd").parse(labelList.getLast().toString());
	     String formattedDate = new SimpleDateFormat("yyyy-MM-dd").format(currentDate);
	     
	     LocalDate statedDate = LocalDate.parse(formattedDate);
	     String threeMonthEarlierDate=statedDate.minusMonths(numberOfMonths-1).toString();	     
		
	     Date threeMonthsBeforeDate = new SimpleDateFormat("yyyy-MM-dd").parse(threeMonthEarlierDate);
	     String formattedThreeMonthsBeforeDate = new SimpleDateFormat("yyyy-MM-dd").format(threeMonthsBeforeDate);
	     
	     LocalDate statedThreeMonthBeforeDate = LocalDate.parse(formattedThreeMonthsBeforeDate);
	     
		return statedThreeMonthBeforeDate; 
	}
}
