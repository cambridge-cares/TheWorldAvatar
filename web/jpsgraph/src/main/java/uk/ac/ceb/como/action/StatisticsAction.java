package uk.ac.ceb.como.action;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.apache.log4j.Logger;

import com.opensymphony.xwork2.ActionSupport;


import uk.ac.ceb.como.properties.PropertiesManager;
import uk.ac.ceb.como.query.QueryManager;
import uk.ac.ceb.como.query.QueryString;

/**
 * 
 * @author NK
 *
 *         Show a simple statistical data about number of Gaussian calculations,
 *         number of species, number of reactions, and number of reaction
 *         mechanisms.
 *
 */
public class StatisticsAction extends ActionSupport {

	private static final long serialVersionUID = 1L;
	
	final static Logger logger = Logger.getLogger(StatisticsAction.class.getName());
	
	static Properties kbProperties = PropertiesManager.loadProperties(StatisticsAction.class.getClassLoader().getResourceAsStream("kb.properties"));	
	
	private static String ontocompchemkb = kbProperties.getProperty("ontocompchem.kb.local.rdf4j.server.url");
	private static String ontokinkb = kbProperties.getProperty("ontokin.kb.local.rdf4j.server.url");
	private static String ontospecieskb = kbProperties.getProperty("ontospecies.kb.local.rdf4j.server.url");
		
	private static String statisticsFolderPath = kbProperties.getProperty("data.folder.path");
	
	
	
	private String numberOfCalculations;

	private String numberOfSpeciesInOntoSpecies ;

	private String numberOfReactionMechanisms ;

	private String numberOfSpeciesInOntoKin ;

	private String numberOfChemicalReactions;
	
//	private String numberOfAgents;
	
	private String numberOfSynonyms;
	
	private String numberOfCabronAndHydrogenSpecies;
	
	private String numberOfCabronAndHydrogenAndOxygenSpecies;
	
	private String numberOfNitrogenSpeciesInOntoKin;
	
	private String numberOfReactionsThatInvolveOxygenHydrocarbonSpecies;
	
	private String numberOfReactionsThatInvolveNitrogenSpecies;
	
	private String numberOfReactionsHydrocarbonSpecies;
	
	private LinkedList<String> labelList = new LinkedList<String>();	
	
	private LinkedList<String> ontoCompChemDataSetList = new LinkedList<String>();
	
	private LinkedList<String> ontoKinDataSetList = new LinkedList<String>();
	
	public List<String> getOntoKinDataSetList() {
		return ontoKinDataSetList;
	}

	public void setOntoKinDataSetList(LinkedList<String> ontoKinDataSetList) {
		this.ontoKinDataSetList = ontoKinDataSetList;
	}

	public LinkedList<String> getOntoCompChemDataSetList() {
		return ontoCompChemDataSetList;
	}

	public void setOntoCompChemDataSetList(LinkedList<String> ontoCompChemDataSetList) {
		this.ontoCompChemDataSetList = ontoCompChemDataSetList;
	}

	public LinkedList<String> getLabelList() {
		return labelList;
	}

	public void setLabelList(LinkedList<String> labelList) {
		this.labelList = labelList;
	}

	public String getNumberOfReactionsHydrocarbonSpecies() {
		return numberOfReactionsHydrocarbonSpecies;
	}

	public void setNumberOfReactionsHydrocarbonSpecies(String numberOfReactionsHydrocarbonSpecies) {
		this.numberOfReactionsHydrocarbonSpecies = numberOfReactionsHydrocarbonSpecies;
	}

	public String getNumberOfReactionsThatInvolveNitrogenSpecies() {
		return numberOfReactionsThatInvolveNitrogenSpecies;
	}

	public void setNumberOfReactionsThatInvolveNitrogenSpecies(String numberOfReactionsThatInvolveNitrogenSpecies) {
		this.numberOfReactionsThatInvolveNitrogenSpecies = numberOfReactionsThatInvolveNitrogenSpecies;
	}

	public String getNumberOfReactionsThatInvolveOxygenHydrocarbonSpecies() {
		return numberOfReactionsThatInvolveOxygenHydrocarbonSpecies;
	}

	public void setNumberOfReactionsThatInvolveOxygenHydrocarbonSpecies(
			String numberOfReactionsThatInvolveOxygenHydrocarbonSpecies) {
		this.numberOfReactionsThatInvolveOxygenHydrocarbonSpecies = numberOfReactionsThatInvolveOxygenHydrocarbonSpecies;
	}
	
	public String getNumberOfNitrogenSpeciesInOntoKin() {
		return numberOfNitrogenSpeciesInOntoKin;
	}

	public void setNumberOfNitrogenSpeciesInOntoKin(String numberOfNitrogenSpeciesInOntoKin) {
		this.numberOfNitrogenSpeciesInOntoKin = numberOfNitrogenSpeciesInOntoKin;
	}

	public String getNumberOfCabronAndHydrogenAndOxygenSpecies() {
		return numberOfCabronAndHydrogenAndOxygenSpecies;
	}

	public void setNumberOfCabronAndHydrogenAndOxygenSpecies(String numberOfCabronAndHydrogenAndOxygenSpecies) {
		this.numberOfCabronAndHydrogenAndOxygenSpecies = numberOfCabronAndHydrogenAndOxygenSpecies;
	}

	public String getNumberOfCabronAndHydrogenSpecies() {
		return numberOfCabronAndHydrogenSpecies;
	}

	public void setNumberOfCabronAndHydrogenSpecies(String numberOfCabronAndHydrogenSpecies) {
		this.numberOfCabronAndHydrogenSpecies = numberOfCabronAndHydrogenSpecies;
	}

	public String getNumberOfSynonyms() {
		return numberOfSynonyms;
	}

	public void setNumberOfSynonyms(String numberOfSynonyms) {
		this.numberOfSynonyms = numberOfSynonyms;
	}

//	public String getNumberOfAgents() {
//		return numberOfAgents;
//	}
//
//	public void setNumberOfAgents(String numberOfAgents) {
//		this.numberOfAgents = numberOfAgents;
//	}

	public String getNumberOfChemicalReactions() {
		return numberOfChemicalReactions;
	}

	public void setNumberOfChemicalReactions(String numberOfChemicalReactions) {
		this.numberOfChemicalReactions = numberOfChemicalReactions;
	}

	public String getNumberOfSpeciesInOntoKin() {
		return numberOfSpeciesInOntoKin;
	}

	public void setNumberOfSpeciesInOntoKin(String numberOfSpeciesInOntoKin) {
		this.numberOfSpeciesInOntoKin = numberOfSpeciesInOntoKin;
	}

	public String getNumberOfReactionMechanisms() {
		return numberOfReactionMechanisms;
	}

	public void setNumberOfReactionMechanisms(String numberOfReactionMechanisms) {
		this.numberOfReactionMechanisms = numberOfReactionMechanisms;
	}

	public String getNumberOfSpeciesInOntoSpecies() {
		return numberOfSpeciesInOntoSpecies;
	}

	public void setNumberOfSpeciesInOntoSpecies(String numberOfSpeciesInOntoSpecies) {
		this.numberOfSpeciesInOntoSpecies = numberOfSpeciesInOntoSpecies;
	}

	public String getNumberOfCalculations() {
		return numberOfCalculations;
	}

	public void setNumberOfCalculations(String numberOfCalculations) {
		this.numberOfCalculations = numberOfCalculations;
	}

	@Override
	public String execute() throws IOException {		
		
		/**
		 * Calculates current time. 
		 */
		SimpleDateFormat dateFormat = new SimpleDateFormat("hhmmss aa");
		String formattedDate = dateFormat.format(new Date()).toString();
		System.out.println("formattedDate: " +formattedDate);
		
		String[] date = formattedDate.split("\\ ");
		
		String time = date[0];
		String am_pm_marker = date[1];
		
		

		logger.info("current time (int): " + time + " current am/pm marker: " + am_pm_marker);
		
		/**
		 * Data structure LinkedHashMap<String,String> used in generating data for bar chart
		 */
		LinkedHashMap<String,String> ontoCompChemMap = new LinkedHashMap<String,String>();
		LinkedHashMap<String,String> ontoKinMap = new LinkedHashMap<String,String>();		
		LinkedHashMap<String,String> updatedOntoCompChemMap = new LinkedHashMap<String,String>();
		LinkedHashMap<String,String> updatedOntoKinMap = new LinkedHashMap<String,String>();
		
		LinkedList<String> labelListTemp = new LinkedList<String>();
		LinkedList<String> ontoCompChemDataSetListTemp = new LinkedList<String>();
		LinkedList<String> ontoKinDataSetListTemp = new LinkedList<String>();
		
		/**
		 * String(s) data used for first table 
		 */
		String numberOfCalculationsTemp= new String();
		String numberOfReactionMechanismsTemp = new String();
		String numberOfSpeciesInOntoKinTemp = new String();
		String numberOfChemicalReactionsTemp = new String();
		String numberOfSynonymsTemp = new String();
		String numberOfSpeciesInOntoSpeciesTemp = new String();
		
		
		/**
		 * String(s) used for second table
		 */
		String numberOfReactionsHydrocarbonSpeciesTemp = new String();
		String numberOfReactionsThatInvolveNitrogenSpeciesTemp = new String();
		String numberOfReactionsThatInvolveOxygenHydrocarbonSpeciesTemp = new String();
		String numberOfCabronAndHydrogenSpeciesTemp = new String();
		String numberOfCabronAndHydrogenAndOxygenSpeciesTemp = new String();
		String numberOfNitrogenSpeciesInOntoKinTemp = new String();
		
		/**
		 * Run SPARQL queries and update the cache (csv files) on given time that is between 02:00am and 02:05am. 
		 */
		if ( (time.compareTo("020000")>0)  && (time.compareTo("020500")<0) && (am_pm_marker.equalsIgnoreCase("AM"))) {
			
			/**
			 * @author NK510 (caresssd@hermes.cam.ac.uk)
			 * 
			 * Generate data to be shown in bar chart. 
			 */
			
			File labelFile  = new File(statisticsFolderPath+"label.csv");
			
			if(!labelFile.exists()) {
				
				labelFile.createNewFile();
			}
			
			File ontocompchemFile  = new File(statisticsFolderPath+"ontocompchem.csv");
			
			if(!ontocompchemFile.exists()) {
				
				ontocompchemFile.createNewFile();
			}
			
			File ontoKinFile  = new File(statisticsFolderPath+"ontokin.csv");
			
			if(!ontoKinFile.exists()) {
				
				ontoKinFile.createNewFile();
			}
			
		PropertiesManager propertiesManager = new PropertiesManager();		
		ontoCompChemMap.putAll(propertiesManager.getFrequencyOfSpeciesPerDate(ontocompchemkb,QueryString.getSpeciesIRIFromOntoCompChem()));
		ontoKinMap.putAll(propertiesManager.getFrequencyOfSpeciesPerDate(ontokinkb,QueryString.getSpeciesIRIFromOntoKin()));
		updatedOntoCompChemMap.putAll(new PropertiesManager().updateFrequenciesMapData(ontoKinMap, ontoCompChemMap));
		updatedOntoKinMap.putAll(new PropertiesManager().updateFrequenciesMapData(updatedOntoCompChemMap,ontoKinMap));
		
		for(Map.Entry<String, String> map: updatedOntoKinMap.entrySet()) {
			
			logger.info("key: " + map.getKey() + " value: " +map.getValue());
		}

		
		/**
		 * @author NK510 (caresssd@hermes.cam.ac.uk)
		 * Updated onto compchem data
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
		
//		labelList.addAll(labelListTemp);
//		ontoCompChemDataSetList.addAll(ontoCompChemDataSetListTemp);
//		ontoKinDataSetList.addAll(ontoKinDataSetListTemp);

		/**
		 * write results in csv files
		 */
	   logger.info("Data set writen in label.csv file:");
	   String labelCSVFile = statisticsFolderPath +"label.csv";
       FileWriter labelCSVWriter = new FileWriter(labelCSVFile);

	   for(String s: labelListTemp) {
			
			    labelCSVWriter.append(s);
			    labelCSVWriter.append(",");
			    
			    logger.info(s);
	   }
	   labelCSVWriter.append("\n");

	   labelCSVWriter.flush();
	   labelCSVWriter.close();
		
		
		logger.info("OntoCompChem data set writen in ontocompchem.csv file:");
		String ontoCompChemCSVFile = statisticsFolderPath +"ontocompchem.csv";
        FileWriter ontoCompChemCSVWriter = new FileWriter(ontoCompChemCSVFile);
		for(String s: ontoCompChemDataSetListTemp) {
			
			ontoCompChemCSVWriter.append(s);
		    ontoCompChemCSVWriter.append(",");
		
		    
			logger.info(s);
		}
		ontoCompChemCSVWriter.append("\n");
		
		ontoCompChemCSVWriter.flush();
		ontoCompChemCSVWriter.close();
		
		
		
		logger.info("OntoKin data set writen in ontokin.csv file:");	
		String ontoKinCSVFile = statisticsFolderPath +"ontokin.csv";
      FileWriter ontoKinCSVWriter = new FileWriter(ontoKinCSVFile);
		for(String s: ontoKinDataSetListTemp) {
			
			ontoKinCSVWriter.append(s);
		    ontoKinCSVWriter.append(",");
			logger.info(s);
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
	
	logger.info("data set writen in table1.csv file:");
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

logger.info("data set writen in table1.csv file:");
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

}	
		

		/**
		 * Reads data from table1.csv file 
		 */
		logger.info("reading table1.csv file :");
		BufferedReader table1Reader = new BufferedReader(new FileReader(statisticsFolderPath+"table1.csv"));
		
		String table1Row="";				
		String[] table1Data =null;
		while ((table1Row = table1Reader.readLine()) != null) {
			table1Data = table1Row.split(","); 
		}
		

		for(int i =0;i<table1Data.length;i++) {			
			logger.info(table1Data[i]);				
		}
		
//		numberOfCalculations = table1Data[0];
		numberOfCalculations=new QueryManager().getQuery(ontocompchemkb,QueryString.getNumberOfGaussianCalculations());
		numberOfReactionMechanisms=table1Data[1];
		numberOfSpeciesInOntoKin=table1Data[2];
		numberOfChemicalReactions=table1Data[3];
		numberOfSynonyms=table1Data[4];
		numberOfSpeciesInOntoSpecies=table1Data[5];
		
		
		table1Reader.close();
		

		
		
		/**
		 * Reads csv data to plot it on bar chart 
		 */
		
		BufferedReader csvLabelReader = new BufferedReader(new FileReader(statisticsFolderPath+"label.csv"));
		
		String row="";				
		String[] data =null;
		while ((row = csvLabelReader.readLine()) != null) {
			data = row.split(","); 
		}
		
		logger.info("reading label.csv file :");
		for(int i =0;i<data.length;i++) {
			
			labelList.add(data[i]);
			
			logger.info(data[i]);				
		}
		
		csvLabelReader.close();
		
	    BufferedReader ontocompchemCsvReader = new BufferedReader(new FileReader(statisticsFolderPath+"ontocompchem.csv"));
		
		String ontocompchemRrow="";				
		String[] ontocompchemData =null;
		while ((ontocompchemRrow = ontocompchemCsvReader.readLine()) != null) {
			ontocompchemData = ontocompchemRrow.split(","); 
		}
		
		logger.info("reading ontocompchem.csv file :");
		for(int i =0;i<ontocompchemData.length;i++) {
			
			ontoCompChemDataSetList.add(ontocompchemData[i]);
			
			logger.info(ontocompchemData[i]);				
		}
		
		ontocompchemCsvReader.close();
		
      BufferedReader ontokinCsvReader = new BufferedReader(new FileReader(statisticsFolderPath+"ontokin.csv"));
		
		String ontokinRrow="";				
		String[] ontokinData =null;
		while ((ontokinRrow = ontokinCsvReader.readLine()) != null) {
			ontokinData = ontokinRrow.split(","); 
		}
		
		logger.info("reading ontokin.csv file :");
		for(int i =0;i<ontokinData.length;i++) {
			
			ontoKinDataSetList.add(ontokinData[i]);
			
			logger.info(ontokinData[i]);				
		}
		
		ontokinCsvReader.close();
		
		
		/**
		 * Reads data from table2.csv file 
		 */
		logger.info("reading table2.csv file :");
		BufferedReader table2Reader = new BufferedReader(new FileReader(statisticsFolderPath+"table2.csv"));
		
		String table2Row="";				
		String[] table2Data =null;
		while ((table2Row = table2Reader.readLine()) != null) {
			table2Data = table2Row.split(","); 
		}
		

		for(int i =0;i<table2Data.length;i++) {			
			logger.info(table2Data[i]);				
		}
		numberOfReactionsHydrocarbonSpecies = table2Data[0];
		numberOfReactionsThatInvolveNitrogenSpecies=table2Data[1];		
		numberOfReactionsThatInvolveOxygenHydrocarbonSpecies=table2Data[2];		
		numberOfCabronAndHydrogenSpecies=table2Data[3];		
		numberOfCabronAndHydrogenAndOxygenSpecies=table2Data[4];		
		numberOfNitrogenSpeciesInOntoKin=table2Data[5];
		
		table2Reader.close();
		
		/**
		 * 
		 * @author NK510 (caresssd@hermes.cam.ac.uk)
		 * Line below is implemented by Dr Feroz Farazi (msff2@cam.ac.uk). He contributed in implementation of reading "value" in sparql query result as JSONObject.  
		 * 
		 */
		
//		numberOfAgents = JsonPath.read(QueryManager.getNumberOfAgents().toString(), "$.results.bindings[0].sum.value");
		

		
		return SUCCESS;
	}

}