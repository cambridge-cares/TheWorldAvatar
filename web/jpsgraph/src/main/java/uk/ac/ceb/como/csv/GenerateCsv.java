package uk.ac.ceb.como.csv;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import uk.ac.ceb.como.properties.PropertiesManager;
import uk.ac.ceb.como.query.QueryManager;
import uk.ac.ceb.como.query.QueryString;

public class GenerateCsv {
	

	private static String statisticsFolderPath ="C:/TOMCAT/webapps/ROOT/data/statistics/";
//	private static String statisticsFolderPath = "C:/TOMCAT/conf/Catalina/generatecsv/";
	private static String ontocompchemkb = "http://localhost/rdf4j-server/repositories/ontocompchem";
	private static String ontokinkb = "http://localhost/rdf4j-server/repositories/ontokin";
	private static String ontospecieskb = "http://localhost/rdf4j-server/repositories/ontospecieskb";
	
	private static String generateTxtFolderPath = "C:\\TOMCAT\\conf\\Catalina\\generatecsv\\";
	
	private static int count = 0;

	/**
	 * 
	 * Data structure LinkedHashMap<String,String> used in generating data for bar chart
	 * 
	 */
	 private static LinkedHashMap<String, String> ontoCompChemMap = new LinkedHashMap<String, String>();
     private static LinkedHashMap<String, String> ontoKinMap = new LinkedHashMap<String, String>();
	 private static LinkedHashMap<String, String> updatedOntoCompChemMap = new LinkedHashMap<String, String>();
	 private static LinkedHashMap<String, String> updatedOntoKinMap = new LinkedHashMap<String, String>();

	private static LinkedList<String> labelListTemp = new LinkedList<String>();
	private static LinkedList<String> ontoCompChemDataSetListTemp = new LinkedList<String>();
	private static LinkedList<String> ontoKinDataSetListTemp = new LinkedList<String>();

	/**
	 * String(s) data used for first table
	 */
	private static String numberOfCalculationsTemp = new String();
	private static String numberOfReactionMechanismsTemp = new String();
	private static String numberOfSpeciesInOntoKinTemp = new String();
	private static String numberOfChemicalReactionsTemp = new String();
	private static String numberOfSynonymsTemp = new String();
	private static String numberOfSpeciesInOntoSpeciesTemp = new String();

	/**
	 * String(s) used for second table
	 */
	private static String numberOfReactionsHydrocarbonSpeciesTemp = new String();
	private static String numberOfReactionsThatInvolveNitrogenSpeciesTemp = new String();
	private static String numberOfReactionsThatInvolveOxygenHydrocarbonSpeciesTemp = new String();
	private static String numberOfCabronAndHydrogenSpeciesTemp = new String();
	private static String numberOfCabronAndHydrogenAndOxygenSpeciesTemp = new String();
	private static String numberOfNitrogenSpeciesInOntoKinTemp = new String();
	
	public static void main(String[] args) throws InterruptedException, IOException {
		
		System.out.println("statisticsFolderPath_1: " + statisticsFolderPath);
		
		System.out.println("ontocompchemkb_1: " + ontocompchemkb);
		
		ScheduledExecutorService ses = Executors.newScheduledThreadPool(1);
		
		Runnable generateCSVFilesTask = () -> {

			try {
				
				count++;

				System.out.println("statisticsFolderPath: " + statisticsFolderPath);
				
				GenerateCsv generateCsv = new GenerateCsv();
				generateCsv.generateFile(statisticsFolderPath, "label.csv");
				generateCsv.generateFile(statisticsFolderPath, "ontocompchem.csv");
				generateCsv.generateFile(statisticsFolderPath, "ontokin.csv");
				
				PropertiesManager propertiesManager = new PropertiesManager();
				/**
				 * Creates hash maps that are storing the results of SPARQL queries. 
				 */
				ontoCompChemMap.putAll(propertiesManager.getFrequencyOfSpeciesPerDate(ontocompchemkb,QueryString.getSpeciesIRIFromOntoCompChem(),"linked_list_ontocompchem.txt", "sparql_query_result_ontocompchem.txt"));
				ontoKinMap.putAll(propertiesManager.getFrequencyOfSpeciesPerDate(ontokinkb,QueryString.getSpeciesIRIFromOntoKin(),"linked_list_ontokin.txt", "sparql_query_result_ontokin.txt"));
				updatedOntoCompChemMap.putAll(new PropertiesManager().updateFrequenciesMapData(ontoKinMap, ontoCompChemMap));
				updatedOntoKinMap.putAll(new PropertiesManager().updateFrequenciesMapData(updatedOntoCompChemMap,ontoKinMap));
				
				/**
				 * Store the content of ontocompchem map into txt file
				 */
				
				System.out.println("ontoCompChemMap.isEmpty(): " + ontoCompChemMap.isEmpty());
				
				File queryResultOntoCompChemFile =new File(generateTxtFolderPath+"query_result_ontocompchem.txt");
				
				queryResultOntoCompChemFile.createNewFile();
				
			    FileWriter queryOntoCompChemFileWriter = new FileWriter(queryResultOntoCompChemFile, false);
			    
				for(Map.Entry<String, String> map: ontoCompChemMap.entrySet()) {
					
				try {

				    queryOntoCompChemFileWriter.write("key: " + map.getKey() + " value: " + map.getValue());
				    queryOntoCompChemFileWriter.write(System.lineSeparator());
				
				} catch (IOException e) {
				    e.printStackTrace();
				} 		    
				}
				
				queryOntoCompChemFileWriter.close();
				
				/**
				 * Store the content of ontokin map into txt file 
				 */

				System.out.println("ontoKinMap.isEmpty(): " + ontoKinMap.isEmpty());
				
				File queryResultOntoKinFile =new File(generateTxtFolderPath+"query_result_ontokin.txt");
				
				queryResultOntoKinFile.createNewFile();
				
			    FileWriter queryOntoCompKinFileWriter = new FileWriter(queryResultOntoKinFile, false);
			    
				for(Map.Entry<String, String> map: ontoKinMap.entrySet()) {
					
				try {

					queryOntoCompKinFileWriter.write("key: " + map.getKey() + " value: " + map.getValue());
					queryOntoCompKinFileWriter.write(System.lineSeparator());
				
				} catch (IOException e) {
				    e.printStackTrace();
				} 		    
				}
				
				queryOntoCompKinFileWriter.close();
				
				/**
				 * Store the content of updated ontocompchem map into txt file. 
				 */

				System.out.println("updatedOntoCompChemMap: " + updatedOntoCompChemMap.isEmpty());
				
				File queryResultUpdatedOntoCompChemFile =new File(generateTxtFolderPath+"query_result_updated_ontocompchem.txt");
				
				queryResultUpdatedOntoCompChemFile.createNewFile();
				
			    FileWriter queryUpdatedOntoCompFileWriter = new FileWriter(queryResultUpdatedOntoCompChemFile, false);
			    
				for(Map.Entry<String, String> map: updatedOntoCompChemMap.entrySet()) {
					
				try {

					queryUpdatedOntoCompFileWriter.write("key: " + map.getKey() + " value: " + map.getValue());
					queryUpdatedOntoCompFileWriter.write(System.lineSeparator());
				
				} catch (IOException e) {
				    e.printStackTrace();
				} 		    
				}
				
				queryUpdatedOntoCompFileWriter.close();
				

				/**
				 * 
				 * Store the content of updated ontokin map into txt file.
				 * 
				 */
				
				System.out.println("updatedOntoKinMap.isEmpty(): " + updatedOntoKinMap.isEmpty());
				
				File queryResultUpdatedOntoKinFile =new File(generateTxtFolderPath+"query_result_updated_ontokin.txt");
				
				queryResultUpdatedOntoKinFile.createNewFile();
				
			    FileWriter queryUpdatedOntoKinFileWriter = new FileWriter(queryResultUpdatedOntoKinFile, false);
			    
				for(Map.Entry<String, String> map: updatedOntoKinMap.entrySet()) {
					
				try {

					queryUpdatedOntoKinFileWriter.write("key: " + map.getKey() + " value: " + map.getValue());
					queryUpdatedOntoKinFileWriter.write(System.lineSeparator());
				
				} catch (IOException e) {
				    e.printStackTrace();
				} 		    
				}
				
				queryUpdatedOntoKinFileWriter.close();
				
			
				
				/**
				 * @author NK510 (caresssd@hermes.cam.ac.uk)
				 * 
				 * Updated ontocompchem data
				 * 
				 */
				
				for(Map.Entry<String, String> compChemMap: updatedOntoCompChemMap.entrySet()) {
					labelListTemp.add(compChemMap.getKey());
					ontoCompChemDataSetListTemp.add(compChemMap.getValue());
				}


				/**
				 * 
				 * @author NK510 (caresssd@hermes.cam.ac.uk)
				 * Updated ontokin data.
				 * 
				 */
				
				for(Map.Entry<String, String> m: updatedOntoKinMap.entrySet()) {			
					ontoKinDataSetListTemp.add(m.getValue());
					}

				/**
				 * Three lines are commented below. 
				 */
//				labelList.addAll(labelListTemp);
//				ontoCompChemDataSetList.addAll(ontoCompChemDataSetListTemp);
//				ontoKinDataSetList.addAll(ontoKinDataSetListTemp);

				/**
				 * write results in csv files
				 */
			  System.out.println("Data set writen in label.csv file:");
			   String labelCSVFile = statisticsFolderPath +"label.csv";
		       FileWriter labelCSVWriter = new FileWriter(labelCSVFile);
		       
			   for(String s: labelListTemp) {
					
				   System.out.println("s: " + s);
				   
		           labelCSVWriter.append(s);
				   labelCSVWriter.append(",");
					    
					    System.out.println("s: " + s);
			   }
			   
			   labelCSVWriter.append("\n");

			   labelCSVWriter.flush();
			   labelCSVWriter.close();
				
				
				System.out.println("OntoCompChem data set writen in ontocompchem.csv file:");
				String ontoCompChemCSVFile = statisticsFolderPath +"ontocompchem.csv";
		        FileWriter ontoCompChemCSVWriter = new FileWriter(ontoCompChemCSVFile);
				for(String s: ontoCompChemDataSetListTemp) {
					
					ontoCompChemCSVWriter.append(s);
				    ontoCompChemCSVWriter.append(",");
				
				    
					
				}
				ontoCompChemCSVWriter.append("\n");
				
				ontoCompChemCSVWriter.flush();
				ontoCompChemCSVWriter.close();
				
				
				
				System.out.println("OntoKin data set writen in ontokin.csv file:");	
				String ontoKinCSVFile = statisticsFolderPath +"ontokin.csv";
		      FileWriter ontoKinCSVWriter = new FileWriter(ontoKinCSVFile);
				for(String s: ontoKinDataSetListTemp) {
					
					ontoKinCSVWriter.append(s);
				    ontoKinCSVWriter.append(",");
					
				}
				ontoKinCSVWriter.append("\n");
				ontoKinCSVWriter.flush();
				ontoKinCSVWriter.close();
				
				/**
				 * 
				 * @author NK510 (caresssd@hermes.cam.ac.uk)
				 * These properties are shown in first table. 
				 * 
				 */
				
				File table1File  = new File(statisticsFolderPath+"table1.csv");
				
				if(!table1File.exists()) {
					
					table1File.createNewFile();
				}
				
			numberOfCalculationsTemp = new QueryManager().getQuery(ontocompchemkb,QueryString.getNumberOfGaussianCalculations());		
			numberOfReactionMechanismsTemp = new QueryManager().getQuery(ontokinkb, QueryString.getNumberOfReactionMechanisms());
			numberOfSpeciesInOntoKinTemp = new QueryManager().getQuery(ontokinkb, QueryString.getNumberOfSpeciesInOntoKin());
			numberOfChemicalReactionsTemp = new QueryManager().getQuery(ontokinkb,QueryString.getNumberOfChemicalReactionsInOntoKin());		
			numberOfSynonymsTemp = new QueryManager().getQuery(ontospecieskb,QueryString.getNumberOfSynonymsInOntoSpecies());		
			numberOfSpeciesInOntoSpeciesTemp = new QueryManager().getQuery(ontospecieskb,QueryString.getNumberOfSpeciesInOntoSpecies());
			
			System.out.println("data set writen in table1.csv file:");
			String table1CSVFile = statisticsFolderPath +"table1.csv";
		    FileWriter table1CSVWriter = new FileWriter(table1CSVFile);
						
			table1CSVWriter.append(numberOfCalculationsTemp);
			table1CSVWriter.append(",");
			table1CSVWriter.append(numberOfReactionMechanismsTemp);
			table1CSVWriter.append(",");
			table1CSVWriter.append(numberOfSpeciesInOntoKinTemp);
			table1CSVWriter.append(",");
			table1CSVWriter.append(numberOfChemicalReactionsTemp);
			table1CSVWriter.append(",");
			table1CSVWriter.append(numberOfSynonymsTemp);
			table1CSVWriter.append(",");
			table1CSVWriter.append(numberOfSpeciesInOntoSpeciesTemp);
			table1CSVWriter.append(",");
			
			
			table1CSVWriter.append("\n");		
			
			table1CSVWriter.flush();
			table1CSVWriter.close();
			
			
			/**
			 * 
			 * @author NK510 (caresssd@hermes.cam.ac.uk)
			 * These properties are shown in second table.
			 * 
			 */
			
		File table2File  = new File(statisticsFolderPath+"table2.csv");
			
		if(!table2File.exists()) {
				
		table2File.createNewFile();

		}

		numberOfReactionsHydrocarbonSpeciesTemp = new QueryManager().getQuery(ontokinkb, QueryString.getNumberOfReactionsThatInvolveHydrocarbonSpecies());		
		numberOfReactionsThatInvolveNitrogenSpeciesTemp=new QueryManager().getQuery(ontokinkb, QueryString.getNumberOfReactionsThatInvolveNitrogenSpecies());			
		numberOfReactionsThatInvolveOxygenHydrocarbonSpeciesTemp =new QueryManager().getQuery(ontokinkb, QueryString.getNumberOfReactionsThatInvolveOxygenHydrocarbonSpecies());		
		numberOfCabronAndHydrogenSpeciesTemp = new QueryManager().getQuery(ontokinkb, QueryString.getCabronHydrogenSpeciesInOntoKin());		
		numberOfCabronAndHydrogenAndOxygenSpeciesTemp = new QueryManager().getQuery(ontokinkb, QueryString.getCabronHydrogenOxygenSpeciesInOntoKin());		
		numberOfNitrogenSpeciesInOntoKinTemp = new QueryManager().getQuery(ontokinkb, QueryString.getNumberNitrogenSpeciesInOntoKin());

		System.out.println("data set writen in table1.csv file:");
		String table2CSVFile = statisticsFolderPath +"table2.csv";
		FileWriter table2CSVWriter = new FileWriter(table2CSVFile);
					
		table2CSVWriter.append(numberOfReactionsHydrocarbonSpeciesTemp);
		table2CSVWriter.append(",");
		table2CSVWriter.append(numberOfReactionsThatInvolveNitrogenSpeciesTemp);
		table2CSVWriter.append(",");
		table2CSVWriter.append(numberOfReactionsThatInvolveOxygenHydrocarbonSpeciesTemp);
		table2CSVWriter.append(",");
		table2CSVWriter.append(numberOfCabronAndHydrogenSpeciesTemp);
		table2CSVWriter.append(",");
		table2CSVWriter.append(numberOfCabronAndHydrogenAndOxygenSpeciesTemp);
		table2CSVWriter.append(",");
		table2CSVWriter.append(numberOfNitrogenSpeciesInOntoKinTemp);
		table2CSVWriter.append(",");

		table2CSVWriter.append("\n");		

		table2CSVWriter.flush();
		table2CSVWriter.close();			
				
				
				
			} catch (IOException e) {

				e.printStackTrace();
				
			}

			System.out.println("Running...generate CSV Files task - number of runs : " + count);

		};

		/**
		 * Repeat the task ones on every 12 hours.
		 */
		ScheduledFuture<?> scheduledFuture = ses.scheduleAtFixedRate(generateCSVFilesTask, 1, 2, TimeUnit.HOURS);

		System.out.println("scheduledFuture.isDone(): " + scheduledFuture.isDone());
		
//		while (true) {
//			System.out.println("count :" + count);
//			Thread.sleep(1000);
//			if (count == 2) {
//				System.out.println("Count is 2, cancel the scheduledFuture!");
//				scheduledFuture.cancel(true);
//				ses.shutdown();
//				break;
//			}
//		}
}

	/**
	 * Creates csv files to store statistics data.
	 * 
	 * @param statisticsFolderPath the folder that constains csv files with statistics data
	 * @param csvFileName the csv file name
	 * @throws IOException the exception 
	 */
	public void generateFile(String statisticsFolderPath, String csvFileName) throws IOException {

		File file = new File(statisticsFolderPath + csvFileName);

		
		if (file.exists() &&  file.isFile()) {

			file.delete();

			System.out.println("File: " + file.getPath() + " is deleted!");

		}
		
		file.createNewFile();
		
		System.out.println("File: " + file.toPath() + " is created!");
		
	}
	
}