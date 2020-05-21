package uk.ac.ceb.como.properties;

import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Properties;
import java.util.TreeMap;

import uk.ac.ceb.como.query.QueryManager;


public class PropertiesManager {

	/**
	 * @author NK510
	 * @param inputStream the Input Stream.
	 * @return Properties (key, value) given in .properties file of self growing knowledge graph statistics project
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
	 * 
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @return the Map of frequencies for species per date
	 * @throws MalformedURLException
	 * @throws IOException
	 * 
	 */
	public LinkedHashMap<String,String> getFrequencyOfSpeciesPerDate(String serverUrl, String queryString) throws MalformedURLException, IOException{
		
		LinkedList<String> linkedList = new LinkedList<String>();
		
		QueryManager qm = new QueryManager();
		
		/**
		 * An example of querying ontocompchem repository
		 */
//		linkedList.addAll((qm.getQueryDateStamp("http://theworldavatar.com/rdf4j-server/repositories/ontocompchem", QueryString.getSpeciesIRIOfGaussianCalculations())));
		
		linkedList.addAll(qm.getQueryDateStamp(serverUrl, queryString));

		LinkedHashMap<String,String> speciesMap = new LinkedHashMap<String,String>();		
		
		for(String list: linkedList) {
			
			String[] parts = list.split("\\#");		
			
			URLConnection connection = new URL(parts[0]).openConnection();			
			
			String lastModified = connection.getHeaderField("Last-Modified");
			
			/**
			 * Some owl files are uplaoded on Claudius but these owl files are removed from folder in Tomcat server.
			 */
			if(lastModified!=null) {
			
			String[] dateParts = lastModified.split(" ");
                    
//            System.out.println("file: " + parts[0] + "  lastModified: " + lastModified + " modified date: " + dateParts[1]+"-"+dateParts[2]+"-"+dateParts[3]);
            
            speciesMap.put(parts[0], dateParts[3]+"-"+PropertiesManager.getNumberFromMonthName(dateParts[2])+"-"+dateParts[1]);
            
			}
		}
		
		LinkedHashMap<String,String> speciesFrequecniesPerDate = new LinkedHashMap<String,String>();
		
		for(Map.Entry<String,String> map: speciesMap.entrySet()) {
			
			int count = Collections.frequency(new ArrayList<String>(speciesMap.values()), map.getValue());
			
//			System.out.println("date: " +map.getValue() + " count:" + count);
			
			if(!speciesFrequecniesPerDate.containsKey(map.getValue())) {
				 
				speciesFrequecniesPerDate.put(map.getValue(),String.valueOf(count));
			}
		}
		
		Map<String, String> treeMap = new TreeMap<String, String>(speciesFrequecniesPerDate);
		
//		System.out.println(" - - - - - - - - -  - - -");		
//		for(Map.Entry<String, String> m: treeMap.entrySet()) {			
//			System.out.println(m.getKey() + " " + m.getValue());
//		}
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
	 */
	public LinkedHashMap<String, String> updateFrequenciesMapData(Map<String,String> sourceMap, Map<String,String> targetMap){
		
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
}
