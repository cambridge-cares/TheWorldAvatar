package uk.ac.ceb.como.csv;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;

import java.util.LinkedList;
import java.util.List;
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
//	private static String statisticsFolderPath = "C:\\TOMCAT\\webapps\\ROOT\\data\\statistics\\";	
	private static String statisticsFolderPath = "C:\\statistics\\";

	/**
	 * Folder paths where OWL files are stored. We use them to calculate date stamp.
	 */
//	private static String ontocompchemKBFolderPath = "C:\\TOMCAT\\webapps\\ROOT\\kb\\ontocompchem";
//	private static String ontokinKBFolderPath = "C:\\TOMCAT\\webapps\\ROOT\\kb\\ontokin";

	private static String ontocompchemKBFolderPath = "C:\\apache-tomcat-8.5.35\\webapps\\ROOT\\kb\\ontocompchem";
	private static String ontokinKBFolderPath = "C:\\apache-tomcat-8.5.35\\webapps\\ROOT\\kb\\ontokin";

	/**
	 * RDF4J remote repositories. Works if runs from local machine.
	 */
	private static String ontocompchemkb = "http://www.theworldavatar.com/rdf4j-server/repositories/ontocompchem";
	private static String ontokinkb = "http://www.theworldavatar.com/rdf4j-server/repositories/ontokin";
	private static String ontospecieskb = "http://www.theworldavatar.com/rdf4j-server/repositories/ontospecieskb";

	private static int count = 0;

	public static void main(String[] args) throws InterruptedException, IOException, ParseException {

		/**
		 * Executor service to run code ones on each 30 minutes. Can be set up to run
		 * the code ones on one hour or longer.
		 */
//		ScheduledExecutorService ses = Executors.newScheduledThreadPool(1);
//		Runnable generateCSVFilesTask = () -> {

		try {

			count++;

			System.out.println("statistics folder path: " + statisticsFolderPath);

			PropertiesManager propertiesManagerOntoCompChem = new PropertiesManager();

			LinkedHashMap<String, String> ontoCompChemMap = new LinkedHashMap<String, String>();

			try {

				ontoCompChemMap
						.putAll(propertiesManagerOntoCompChem.getFrequencyOfSpeciesPerDate(ontocompchemKBFolderPath));

			} catch (IOException e) {
				// TODO Auto-generated catch block
				System.out.println("ontocompchem kb exception: " + e.getMessage());
			}

			System.out.println("OntoCompChem map:");
			for (Map.Entry<String, String> map : ontoCompChemMap.entrySet()) {

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
			for (Map.Entry<String, String> map : ontoKinMap.entrySet()) {

				System.out.println(map.getKey() + " " + map.getValue());
			}

			/**
			 * Calculates today's date using format 'yyyy-MM-dd'.
			 */
			Calendar c = Calendar.getInstance();

			DateFormat todaysDateFormat = new SimpleDateFormat("yyyy-MM-dd");

//			Date d = todaysDateFormat.parse("2020-12-21");
//			Calendar c = Calendar.getInstance();
//			c.setTime(d);

			LocalDate todaysLocalDate = LocalDate.parse(todaysDateFormat.format(c.getTime()));

			System.out.println("Todays date: " + todaysLocalDate.toString());

			/**
			 * Calculates three date that is three months earlier from today's date.
			 */
			LocalDate threeMonthsEarlier = PropertiesManager.getDateThreeMonthsEarlierFromTodaysDate(
					LocalDate.parse(todaysDateFormat.format(c.getTime())).toString(), 3);

			System.out.println("Three months earlier date: " + threeMonthsEarlier.toString());

			/**
			 * List all dates between two dates on range of seven days.
			 */
			PropertiesManager pm = new PropertiesManager();
			
			LinkedHashMap<LocalDate, LocalDate> pairsOfDatesMap = new LinkedHashMap<LocalDate, LocalDate>();

			pairsOfDatesMap.putAll(
					pm.getWeeklyDatesBetweenTwoDates(todaysLocalDate.toString(), threeMonthsEarlier.toString()));

			

			/**
			 * Use HashMap<LocalDate,LocalDate> instead of LinkedList<LocalDate>
			 */
//			for (Map.Entry<LocalDate, LocalDate> m : pairsOfDatesMap.entrySet()) {
//				
//				int sumOntoCompChem = 0;
//				
//				for (Map.Entry<String, String> map : ontoCompChemMap.entrySet()) {
//					
//					LocalDate ontoCompChemDate = LocalDate.parse(map.getKey().toString());
//
//					int value = Integer.parseInt(map.getValue());
//
//					if ((ontoCompChemDate.isBefore(m.getKey()) ||  ontoCompChemDate.isEqual(m.getKey())) && (ontoCompChemDate.isAfter(m.getValue()) || ontoCompChemDate.isEqual(m.getValue()) ) ) {
//
//						System.out.println(ontoCompChemDate + " is between " + m.getKey() + " and " + m.getValue());
//
//						sumOntoCompChem = sumOntoCompChem + value;
//
//						ontoCompChemSumMap.put("[ " + m.getKey().toString() + " ; " + m.getValue().toString() + " ] ",
//								String.valueOf(sumOntoCompChem));
//					}
//				}
//			}

			LinkedHashMap<String, String> ontoCompChemSumMap = new LinkedHashMap<String, String>();
			ontoCompChemSumMap.putAll(pm.getSpeciesStatisticsWeekly(pairsOfDatesMap, ontoCompChemMap));
			System.out.println("sum onto comp chem map: ");

			for (Map.Entry<String, String> m : ontoCompChemSumMap.entrySet()) {

				System.out.println(m.getKey() + " " + m.getValue());
			}
			

			PropertiesManager pm1 = new PropertiesManager();
			LinkedHashMap<String, String> ontoKinSumMap = new LinkedHashMap<String, String>();
			ontoKinSumMap.putAll(pm1.getSpeciesStatisticsWeekly(pairsOfDatesMap, ontoKinMap));
			System.out.println("sum onto kin map: ");

			for (Map.Entry<String, String> m1 : ontoKinSumMap.entrySet()) {

				System.out.println(m1.getKey() + " " + m1.getValue());
			}
			
			LinkedHashMap<String, String> updatedOntoCompChemMap = new LinkedHashMap<String, String>();
			LinkedHashMap<String, String> updatedOntoKinMap = new LinkedHashMap<String, String>();
			updatedOntoCompChemMap
					.putAll(new PropertiesManager().updateFrequenciesMapData(ontoKinMap, ontoCompChemMap));
			updatedOntoKinMap
					.putAll(new PropertiesManager().updateFrequenciesMapData(updatedOntoCompChemMap, ontoKinMap));

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
			 * Three lines below are commented.
			 */
			// labelList.addAll(labelListTemp);
			// ontoCompChemDataSetList.addAll(ontoCompChemDataSetListTemp);
			// ontoKinDataSetList.addAll(ontoKinDataSetListTemp);

			PrintWriter pwLabel = new PrintWriter(statisticsFolderPath + "label.csv");
			pwLabel.close();
			/**
			 * Write results in CSV files label, ontocompchem and ontokin
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

//				String numberOfCalculationsTemp = new QueryManager().getQuery(ontocompchemkb, QueryString.getNumberOfGaussianCalculations());
//				String numberOfReactionMechanismsTemp = new QueryManager().getQuery(ontokinkb,QueryString.getNumberOfReactionMechanisms());
//				String numberOfSpeciesInOntoKinTemp = new QueryManager().getQuery(ontokinkb,QueryString.getNumberOfSpeciesInOntoKin());
//				String numberOfChemicalReactionsTemp = new QueryManager().getQuery(ontokinkb,QueryString.getNumberOfChemicalReactionsInOntoKin());
//				String numberOfSynonymsTemp = new QueryManager().getQuery(ontospecieskb,QueryString.getNumberOfSynonymsInOntoSpecies());
//				String numberOfSpeciesInOntoSpeciesTemp = new QueryManager().getQuery(ontospecieskb,QueryString.getNumberOfSpeciesInOntoSpecies());

			/**
			 * Loading data in first table that is located before bar chart
			 */

			PrintWriter table1Writer = new PrintWriter(statisticsFolderPath + "table1.csv");
			table1Writer.close();

			System.out.println("data set writen to table1.csv file:");
			String table1CSVFile = statisticsFolderPath + "table1.csv";
			FileWriter table1CSVWriter = new FileWriter(table1CSVFile);

//				table1CSVWriter.write(numberOfCalculationsTemp);
//				table1CSVWriter.write(",");
//				table1CSVWriter.write(numberOfReactionMechanismsTemp);
//				table1CSVWriter.write(",");
//				table1CSVWriter.write(numberOfSpeciesInOntoKinTemp);
//				table1CSVWriter.write(",");
//				table1CSVWriter.write(numberOfChemicalReactionsTemp);
//				table1CSVWriter.write(",");
//				table1CSVWriter.write(numberOfSynonymsTemp);
//				table1CSVWriter.write(",");
//				table1CSVWriter.write(numberOfSpeciesInOntoSpeciesTemp);
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

//				String numberOfReactionsHydrocarbonSpeciesTemp = new QueryManager().getQuery(ontokinkb,QueryString.getNumberOfReactionsThatInvolveHydrocarbonSpecies());
//				String numberOfReactionsThatInvolveNitrogenSpeciesTemp = new QueryManager().getQuery(ontokinkb,QueryString.getNumberOfReactionsThatInvolveNitrogenSpecies());
//				String numberOfReactionsThatInvolveOxygenHydrocarbonSpeciesTemp = new QueryManager().getQuery(ontokinkb,QueryString.getNumberOfReactionsThatInvolveOxygenHydrocarbonSpecies());
//				String numberOfCabronAndHydrogenSpeciesTemp = new QueryManager().getQuery(ontokinkb,QueryString.getCabronHydrogenSpeciesInOntoKin());
//				String numberOfCabronAndHydrogenAndOxygenSpeciesTemp = new QueryManager().getQuery(ontokinkb,QueryString.getCabronHydrogenOxygenSpeciesInOntoKin());
//				String numberOfNitrogenSpeciesInOntoKinTemp = new QueryManager().getQuery(ontokinkb,QueryString.getNumberNitrogenSpeciesInOntoKin());

			/**
			 * Loading data in second table that is located after bar chart on the
			 * statistics page.
			 */

			PrintWriter table2Writer = new PrintWriter(statisticsFolderPath + "table2.csv");
			table2Writer.close();

			System.out.println("data set writen to table2.csv file:");
			String table2CSVFile = statisticsFolderPath + "table2.csv";
			FileWriter table2CSVWriter = new FileWriter(table2CSVFile);

//				table2CSVWriter.write(numberOfReactionsHydrocarbonSpeciesTemp);
//				table2CSVWriter.write(",");
//				table2CSVWriter.write(numberOfReactionsThatInvolveNitrogenSpeciesTemp);
//				table2CSVWriter.write(",");
//				table2CSVWriter.write(numberOfReactionsThatInvolveOxygenHydrocarbonSpeciesTemp);
//				table2CSVWriter.write(",");
//				table2CSVWriter.write(numberOfCabronAndHydrogenSpeciesTemp);
//				table2CSVWriter.write(",");
//				table2CSVWriter.write(numberOfCabronAndHydrogenAndOxygenSpeciesTemp);
//				table2CSVWriter.write(",");
//				table2CSVWriter.write(numberOfNitrogenSpeciesInOntoKinTemp);
//				table2CSVWriter.write(",");

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

//		};

		/**
		 * Repeat the task ones on every 2 hour.
		 */
//		ScheduledFuture<?> scheduledFuture = ses.scheduleAtFixedRate(generateCSVFilesTask, 1, 10, TimeUnit.HOURS);
//		System.out.println("scheduledFuture.isDone(): " + scheduledFuture.isDone());
	}
}