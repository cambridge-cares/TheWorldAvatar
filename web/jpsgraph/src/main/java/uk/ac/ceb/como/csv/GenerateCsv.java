package uk.ac.ceb.como.csv;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.HashMap;
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

	/**
	 * Folder where stastistics data are stored in csv files.
	 */
	private static String statisticsFolderPath ="C:\\TOMCAT\\webapps\\ROOT\\data\\statistics\\";
//	private static String statisticsFolderPath = "C:\\statistics\\";

	/**
	 * Folder paths where OWL files are stored. We use them to calculate date stamp. 
	 */
	private static String ontocompchemKBFolderPath ="C:\\TOMCAT\\webapps\\ROOT\\kb\\ontocompchem";
	private static String ontokinKBFolderPath ="C:\\TOMCAT\\webapps\\ROOT\\kb\\ontokin";
	
	/**
	 * RDF4J remote repositories.
	 * Works if runs from local machine.
	 */
	private static String ontocompchemkb = "http://www.theworldavatar.com/rdf4j-server/repositories/ontocompchem";
	private static String ontokinkb = "http://www.theworldavatar.com/rdf4j-server/repositories/ontokin";
	private static String ontospecieskb = "http://www.theworldavatar.com/rdf4j-server/repositories/ontospecieskb";

	private static int count = 0;

	/**
	 * 
	 * Data structure LinkedHashMap<String,String> used in generating data for bar
	 * chart
	 * 
	 */
//	private static HashMap<String, String> ontoCompChemMap = new HashMap<String, String>();
//	private static LinkedHashMap<String, String> ontoKinMap = new LinkedHashMap<String, String>();
//	private static LinkedHashMap<String, String> updatedOntoCompChemMap = new LinkedHashMap<String, String>();
//	private static LinkedHashMap<String, String> updatedOntoKinMap = new LinkedHashMap<String, String>();
//	
//	private static LinkedList<String> labelListTemp = new LinkedList<String>();
//	private static LinkedList<String> ontoCompChemDataSetListTemp = new LinkedList<String>();
//	private static LinkedList<String> ontoKinDataSetListTemp = new LinkedList<String>();

	public static void main(String[] args) throws InterruptedException, IOException {
		
		/**
		 * Executor service to run code ones on each 30 minutes. Can be set up to run the code ones on one hour or longer.
		 */
		ScheduledExecutorService ses = Executors.newScheduledThreadPool(1);
		Runnable generateCSVFilesTask = () -> {

		try {

			count++;

			System.out.println("statistics folder path: " + statisticsFolderPath);

			PropertiesManager propertiesManagerOntoCompChem = new PropertiesManager();					
			HashMap<String, String> ontoCompChemMap = new HashMap<String, String>();
					try {						
					
					ontoCompChemMap.putAll(propertiesManagerOntoCompChem.getFrequencyOfSpeciesPerDate(ontocompchemKBFolderPath));

					} catch (IOException e) {
						// TODO Auto-generated catch block
						System.out.println("ontocompchem kb exception: " + e.getMessage());
					}
					
					System.out.println("OntoCompChem map:");
					for(Map.Entry<String, String> map : ontoCompChemMap.entrySet()) {
						
						System.out.println(map.getKey() + " " + map.getValue());
					}
					
					PropertiesManager propertiesManagerOntoKin = new PropertiesManager();
					LinkedHashMap<String, String> ontoKinMap = new LinkedHashMap<String, String>();
					try {
						
						ontoKinMap.putAll(propertiesManagerOntoKin.getFrequencyOfSpeciesPerDate(ontokinKBFolderPath));
						
					} catch (IOException e) {
						// TODO Auto-generated catch block
						System.out.println("ontokin kb exception: " + e.getMessage());
					}
					
					System.out.println("OntoKin map:");
					for(Map.Entry<String, String> map : ontoKinMap.entrySet()) {
						
						System.out.println(map.getKey() + " " + map.getValue());
					}
					
			LinkedHashMap<String, String> updatedOntoCompChemMap = new LinkedHashMap<String, String>();
					
			LinkedHashMap<String, String> updatedOntoKinMap = new LinkedHashMap<String, String>();
			
			updatedOntoCompChemMap.putAll(new PropertiesManager().updateFrequenciesMapData(ontoKinMap, ontoCompChemMap));
			
			updatedOntoKinMap.putAll(new PropertiesManager().updateFrequenciesMapData(updatedOntoCompChemMap, ontoKinMap));			
			
			/**
			 * @author NK510 (caresssd@hermes.cam.ac.uk)
			 * 
			 *         Updated ontocompchem data
			 * 
			 */
			LinkedList<String> labelListTemp = new LinkedList<String>();
			LinkedList<String> ontoCompChemDataSetListTemp = new LinkedList<String>();

			for (Map.Entry<String, String> compChemMap : updatedOntoCompChemMap.entrySet()) {
				labelListTemp.add(compChemMap.getKey());
				ontoCompChemDataSetListTemp.add(compChemMap.getValue());
			}

			/**
			 * 
			 * @author NK510 (caresssd@hermes.cam.ac.uk) Updated ontokin data.
			 * 
			 */
			LinkedList<String> ontoKinDataSetListTemp = new LinkedList<String>();
			for (Map.Entry<String, String> m : updatedOntoKinMap.entrySet()) {
				ontoKinDataSetListTemp.add(m.getValue());
			}

			/**
			 * Three lines are commented below.
			 */
             //				labelList.addAll(labelListTemp);
             //				ontoCompChemDataSetList.addAll(ontoCompChemDataSetListTemp);
             //				ontoKinDataSetList.addAll(ontoKinDataSetListTemp);

//			GenerateCsv generateCsv = new GenerateCsv();

			/**
			 * Deletes existing label.csv and creates new label.csv file
			 */
//			generateCsv.createFile(statisticsFolderPath, "label.csv");		
			

			PrintWriter pwLabel = new PrintWriter(statisticsFolderPath + "label.csv");
			pwLabel.close();
			/**
			 * Write results in csv files label, ontocompchem and ontokin
			 */
			System.out.println("Data set writen to label.csv file:");
			String labelCSVFile = statisticsFolderPath + "label.csv";
			FileWriter labelCSVWriter = new FileWriter(labelCSVFile);

			for (String s : labelListTemp) {

				labelCSVWriter.write(s);
				labelCSVWriter.write(",");		

			}

			labelCSVWriter.write("\n");

			labelCSVWriter.flush();
			labelCSVWriter.close();

			/**
			 * Deletes existing ontocompchem.csv and creates new ontocompchem.csv file
			 */
//			generateCsv.createFile(statisticsFolderPath, "ontocompchem.csv");

			PrintWriter pwOntocompchem = new PrintWriter(statisticsFolderPath + "ontocompchem.csv");
			pwOntocompchem.close();
	
			System.out.println("OntoCompChem data set writen to ontocompchem.csv file:");
			String ontoCompChemCSVFile = statisticsFolderPath + "ontocompchem.csv";
			FileWriter ontoCompChemCSVWriter = new FileWriter(ontoCompChemCSVFile);
			for (String s : ontoCompChemDataSetListTemp) {

				ontoCompChemCSVWriter.write(s);
				ontoCompChemCSVWriter.write(",");

			}
			ontoCompChemCSVWriter.write("\n");

			ontoCompChemCSVWriter.flush();
			ontoCompChemCSVWriter.close();
			
			/**
			 * Deletes existing ontokin.csv and creates new ontokin.csv file
			 */
//			generateCsv.createFile(statisticsFolderPath, "ontokin.csv");

			PrintWriter pwOntokin = new PrintWriter(statisticsFolderPath + "ontokin.csv");
			pwOntokin.close();
				

			System.out.println("OntoKin data set writen to ontokin.csv file:");

			String ontoKinCSVFile = statisticsFolderPath + "ontokin.csv";
			FileWriter ontoKinCSVWriter = new FileWriter(ontoKinCSVFile);
			for (String s : ontoKinDataSetListTemp) {

				ontoKinCSVWriter.write(s);
				ontoKinCSVWriter.write(",");

			}
			ontoKinCSVWriter.write("\n");
			ontoKinCSVWriter.flush();
			ontoKinCSVWriter.close();

			/**
			 * 
			 * @author NK510 (caresssd@hermes.cam.ac.uk) These properties are shown in first
			 *         table.
			 * 
			 */

			String numberOfCalculationsTemp = new QueryManager().getQuery(ontocompchemkb,
					QueryString.getNumberOfGaussianCalculations());

			String numberOfReactionMechanismsTemp = new QueryManager().getQuery(ontokinkb,
					QueryString.getNumberOfReactionMechanisms());

			String numberOfSpeciesInOntoKinTemp = new QueryManager().getQuery(ontokinkb,
					QueryString.getNumberOfSpeciesInOntoKin());

			String numberOfChemicalReactionsTemp = new QueryManager().getQuery(ontokinkb,
					QueryString.getNumberOfChemicalReactionsInOntoKin());

			String numberOfSynonymsTemp = new QueryManager().getQuery(ontospecieskb,
					QueryString.getNumberOfSynonymsInOntoSpecies());

			String numberOfSpeciesInOntoSpeciesTemp = new QueryManager().getQuery(ontospecieskb,
					QueryString.getNumberOfSpeciesInOntoSpecies());

			/**
			 * Loading data in first table that is located before bar chart
			 */

			PrintWriter table1Writer = new PrintWriter(statisticsFolderPath + "table1.csv");
			table1Writer.close();

			System.out.println("data set writen to table1.csv file:");
			String table1CSVFile = statisticsFolderPath + "table1.csv";
			FileWriter table1CSVWriter = new FileWriter(table1CSVFile);

			table1CSVWriter.write(numberOfCalculationsTemp);
			table1CSVWriter.write(",");
			table1CSVWriter.write(numberOfReactionMechanismsTemp);
			table1CSVWriter.write(",");
			table1CSVWriter.write(numberOfSpeciesInOntoKinTemp);
			table1CSVWriter.write(",");
			table1CSVWriter.write(numberOfChemicalReactionsTemp);
			table1CSVWriter.write(",");
			table1CSVWriter.write(numberOfSynonymsTemp);
			table1CSVWriter.write(",");
			table1CSVWriter.write(numberOfSpeciesInOntoSpeciesTemp);
			table1CSVWriter.write(",");

			table1CSVWriter.write("\n");

			table1CSVWriter.flush();
			table1CSVWriter.close();

			/**
			 * 
			 * @author NK510 (caresssd@hermes.cam.ac.uk) These properties are shown in
			 *         second table.
			 * 
			 */

			String numberOfReactionsHydrocarbonSpeciesTemp = new QueryManager().getQuery(ontokinkb,
					QueryString.getNumberOfReactionsThatInvolveHydrocarbonSpecies());

			String numberOfReactionsThatInvolveNitrogenSpeciesTemp = new QueryManager().getQuery(ontokinkb,
					QueryString.getNumberOfReactionsThatInvolveNitrogenSpecies());

			String numberOfReactionsThatInvolveOxygenHydrocarbonSpeciesTemp = new QueryManager().getQuery(ontokinkb,
					QueryString.getNumberOfReactionsThatInvolveOxygenHydrocarbonSpecies());
			;
			String numberOfCabronAndHydrogenSpeciesTemp = new QueryManager().getQuery(ontokinkb,
					QueryString.getCabronHydrogenSpeciesInOntoKin());

			String numberOfCabronAndHydrogenAndOxygenSpeciesTemp = new QueryManager().getQuery(ontokinkb,
					QueryString.getCabronHydrogenOxygenSpeciesInOntoKin());

			String numberOfNitrogenSpeciesInOntoKinTemp = new QueryManager().getQuery(ontokinkb,
					QueryString.getNumberNitrogenSpeciesInOntoKin());

			/**
			 * Loading data in second table that is located after bar chart
			 */

			PrintWriter table2Writer = new PrintWriter(statisticsFolderPath + "table2.csv");
			table2Writer.close();

			System.out.println("data set writen to table2.csv file:");
			String table2CSVFile = statisticsFolderPath + "table2.csv";
			FileWriter table2CSVWriter = new FileWriter(table2CSVFile);

			table2CSVWriter.write(numberOfReactionsHydrocarbonSpeciesTemp);
			table2CSVWriter.write(",");
			table2CSVWriter.write(numberOfReactionsThatInvolveNitrogenSpeciesTemp);
			table2CSVWriter.write(",");
			table2CSVWriter.write(numberOfReactionsThatInvolveOxygenHydrocarbonSpeciesTemp);
			table2CSVWriter.write(",");
			table2CSVWriter.write(numberOfCabronAndHydrogenSpeciesTemp);
			table2CSVWriter.write(",");
			table2CSVWriter.write(numberOfCabronAndHydrogenAndOxygenSpeciesTemp);
			table2CSVWriter.write(",");
			table2CSVWriter.write(numberOfNitrogenSpeciesInOntoKinTemp);
			table2CSVWriter.write(",");

			table2CSVWriter.write("\n");

			table2CSVWriter.flush();
			table2CSVWriter.close();

		} catch (IOException e) {

			e.printStackTrace();

		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		System.out.println("Running...generate CSV Files task - number of runs : " + count);

		};

		/**
		 * Repeat the task ones on every 2 hour.
		 */
		ScheduledFuture<?> scheduledFuture = ses.scheduleAtFixedRate(generateCSVFilesTask, 1, 30, TimeUnit.MINUTES);

		System.out.println("scheduledFuture.isDone(): " + scheduledFuture.isDone());
	}

	/**
	 * Creates csv files to store statistics data.
	 * 
	 * @param statisticsFolderPath the folder that constains csv files with
	 *                             statistics data
	 * @param csvFileName          the csv file name
	 * @throws IOException the exception
	 */
	public void createFile(String statisticsFolderPath, String csvFileName) throws IOException {

		File file = new File(statisticsFolderPath + csvFileName);

		if (file.exists() && file.isFile()) {

			file.delete();

		System.out.println("File: " + file.getPath() + " is deleted!");

		}

		file.createNewFile();

		System.out.println("File: " + file.toPath() + " is created!");

	}

}