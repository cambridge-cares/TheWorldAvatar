package uk.ac.ceb.como.action;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.util.Date;

import java.util.LinkedList;
import java.util.List;
import java.util.Properties;

import org.apache.log4j.Logger;

import com.opensymphony.xwork2.ActionSupport;

import uk.ac.ceb.como.properties.PropertiesManager;

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
	
	private static String statisticsFolderPath = kbProperties.getProperty("data.folder.path");
	
	private String numberOfCalculations;

	private String numberOfSpeciesInOntoSpecies ;

	private String numberOfReactionMechanisms ;

	private String numberOfSpeciesInOntoKin ;

	private String numberOfChemicalReactions;
	
	/**
	 * This is temporally commented.
	 */
//	private String numberOfAgents;
	
	private String numberOfSynonyms;
	
	private String numberOfCabronAndHydrogenSpecies;
	
	private String numberOfCabronAndHydrogenAndOxygenSpecies;
	
	private String numberOfNitrogenSpeciesInOntoKin;
	
	private String numberOfReactionsThatInvolveOxygenHydrocarbonSpecies;
	
	private String numberOfReactionsThatInvolveNitrogenSpecies;
	
	private String numberOfReactionsHydrocarbonSpecies;
	
	private LinkedList<String> labelList = new LinkedList<String>();
	
	private LinkedList<String> labelListThreeMonths = new LinkedList<String>();
	
	private LinkedList<String> ontoCompChemDataSetList = new LinkedList<String>();
	
	private LinkedList<String> ontoCompChemDataSetListThreeMonths = new LinkedList<String>();
	
	private LinkedList<String> ontoKinDataSetList = new LinkedList<String>();
	
	private LinkedList<String> ontoKinDataSetListThreeMonths = new LinkedList<String>();
	
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

	public LinkedList<String> getLabelListThreeMonths() {
		return labelListThreeMonths;
	}

	public void setLabelListThreeMonths(LinkedList<String> labelListThreeMonths) {
		this.labelListThreeMonths = labelListThreeMonths;
	}
	
	public LinkedList<String> getOntoCompChemDataSetListThreeMonths() {
		return ontoCompChemDataSetListThreeMonths;
	}

	public void setOntoCompChemDataSetListThreeMonths(LinkedList<String> ontoCompChemDataSetListThreeMonths) {
		this.ontoCompChemDataSetListThreeMonths = ontoCompChemDataSetListThreeMonths;
	}

	public LinkedList<String> getOntoKinDataSetListThreeMonths() {
		return ontoKinDataSetListThreeMonths;
	}

	public void setOntoKinDataSetListThreeMonths(LinkedList<String> ontoKinDataSetListThreeMonths) {
		this.ontoKinDataSetListThreeMonths = ontoKinDataSetListThreeMonths;
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
	public String execute() throws IOException, ParseException {		
		
		logger.info("started reading csv files:");
		
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
		 * Reads data from table1.csv file 
		 */
		logger.info("reading table1.csv file :");
		BufferedReader table1Reader = new BufferedReader(new FileReader(statisticsFolderPath+"table1.csv"));
		
		String table1Row="";				
		String[] table1Data =null;
		while ((table1Row = table1Reader.readLine()) != null) {
			table1Data = table1Row.split(","); 
		}
		
		numberOfCalculations = table1Data[0];
		/**
		 * Line below is commented because we are not querying ontocompchem knowledge graph when running statistics service. 
		 */
//		numberOfCalculations=new QueryManager().getQuery(ontocompchemkb,QueryString.getNumberOfGaussianCalculations());
		numberOfReactionMechanisms=table1Data[1];
		numberOfSpeciesInOntoKin=table1Data[2];
		numberOfChemicalReactions=table1Data[3];
		numberOfSynonyms=table1Data[4];
		numberOfSpeciesInOntoSpecies=table1Data[5];
		
		table1Reader.close();		
		
		
		/**
		 * Reads csv data to plot it on bar chart  for the period of last three months starting from current date (date when last OWL was uploaded)
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
			
		
		}
		

		/**
		 * Calculates date that is three months earlier than date of last uploaded OWL file.
		 */
		LocalDate threeMonthsEarlierDate = PropertiesManager.getDateThreeMonthsEarlier(labelList,3);
		
		logger.info("three months earlier date: " + threeMonthsEarlierDate.toString());

	     
		/**
		 * Populates label list with dates that are three months earlier than current date (last date when OWL file is uploaded)
		 */
		for(int i =0;i<data.length;i++) {
			
			Date labelDate = new SimpleDateFormat("yyyy-MM-dd").parse(data[i].toString());
            String formattedLabelDate = new SimpleDateFormat("yyyy-MM-dd").format(labelDate);		     
		    LocalDate statedLabelDate = LocalDate.parse(formattedLabelDate);
		     
		     if(threeMonthsEarlierDate.isBefore(statedLabelDate) || threeMonthsEarlierDate.isEqual(statedLabelDate)) {

					labelListThreeMonths.add(data[i]); 
		     }
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
		}
		
	  logger.info("reading ontocompchem data for last three months");
	  
	  int sizeOfLabelLastThreeMonths = labelListThreeMonths.size();
	  
	  logger.info("number of dates used in bar chart: " + sizeOfLabelLastThreeMonths);
	  
	  int sizeOfLabelList = labelList.size();
	  
	  int difference = sizeOfLabelList - sizeOfLabelLastThreeMonths;
	  
	  /**
	   * Adding ontocompchem data into ontocomcphem data set list last three months.
	   */
	  for(int i = difference;i<ontocompchemData.length;i++) {
		  
		  ontoCompChemDataSetListThreeMonths.add(ontocompchemData[i]);
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
			
		}
		
		
		for(int i = difference;i<ontokinData.length;i++) {
			
			ontoKinDataSetListThreeMonths.add(ontokinData[i]);	
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
//			logger.info(table2Data[i]);				
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