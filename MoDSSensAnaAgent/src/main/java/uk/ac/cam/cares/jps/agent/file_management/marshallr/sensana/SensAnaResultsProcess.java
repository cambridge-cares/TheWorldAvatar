package uk.ac.cam.cares.jps.agent.file_management.marshallr.sensana;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.agent.mechanism.sensana.MoDSSensAnaAgentException;
import uk.ac.cam.cares.jps.kg.OntoKinKG;

public class SensAnaResultsProcess {
	private Logger logger = LoggerFactory.getLogger(SensAnaResultsProcess.class);
	
	public static void main(String[] args) throws IOException, MoDSSensAnaAgentException {
		
//		String resultsFolder = "C:\\Users\\jb2197\\Desktop\\PODE_Project\\Data\\SensAna\\";
//		
//		String maxDcDtOH = "login-cpu.hpc.cam.ac.uk_3705638140418000_SensAna_MaxDcDt_OH";
//		String maxDcDtCO = "login-cpu.hpc.cam.ac.uk_3705655429858500_SensAna_MaxDcDt_CO";
//		String concCO = "login-cpu.hpc.cam.ac.uk_3705683489571800_SensAna_Conc_CO";
//		String concOH = "login-cpu.hpc.cam.ac.uk_3705704050765200_SensAna_Conc_OH";
//		String temp400K = "login-cpu.hpc.cam.ac.uk_3705731441602300_SensAna_Temp_400K";
//		String maxDpDt = "login-cpu.hpc.cam.ac.uk_3705768273696900_SensAna_MaxDpDt";
//		String maxDtDt= "login-cpu.hpc.cam.ac.uk_3705790466069800_SensAna_MaxDtDt";
//		
//		int topN = 20;
//		
//		List<String> sensAnaCases = new ArrayList<>();
//		sensAnaCases.add(maxDcDtOH);
//		sensAnaCases.add(maxDcDtCO);
//		sensAnaCases.add(concCO);
//		sensAnaCases.add(concOH);
//		sensAnaCases.add(temp400K);
//		sensAnaCases.add(maxDpDt);
//		sensAnaCases.add(maxDtDt);
//		
//		for (String sensAna : sensAnaCases) {
//			SensAnaResultsProcess sensAnaResultsProcess = new SensAnaResultsProcess();
//			sensAnaResultsProcess.processResults(resultsFolder.concat(sensAna), topN);
//		}
		
		SensAnaResultsProcess sensAnaResultsProcess = new SensAnaResultsProcess();
		List<String> rxnIRI = sensAnaResultsProcess.processResults("C:\\Users\\jb2197\\MoDSSensAnaAgent_4639325665088300\\login-skylake.hpc.cam.ac.uk_4639325666472100\\output", "http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ReactionMechanism_73656018231261", 2);
		System.out.println(rxnIRI);
		
//		String mechanismIRI = "http://www.theworldavatar.com/kb/ontokin/pode_mechanism_original.owl#ReactionMechanism_73656018231261";
		
//		System.setProperty("hadoop.home.dir", "C:\\Hadoop");
//		List<Double> inputData = new ArrayList<>();
//		inputData.add(35.5);
//		inputData.add(36.5);
//		inputData.add(38.5);
//		inputData.add(32.5);
		
//		resolved the jackson version conflict problem following below link
//		https://programming.vip/docs/resolution-of-jackson-version-conflict-in-spark-application.html
		
//		SparkConf conf = new SparkConf().setAppName("startingSpark").setMaster("local[*]");
//		JavaSparkContext sc = new JavaSparkContext(conf);
//		
//		JavaRDD<Double> myRdd = sc.parallelize(inputData);
//		System.out.println(myRdd.count());
//		Double result = myRdd.reduce((Double value1, Double value2) -> value1 + value2);
//		
//		System.out.println(result);
//		sc.close();
//		SparkSession spark = SparkSession.builder().appName("Java Spark SQL Example").config("spark.master", "local").getOrCreate();
//		System.out.println(resultsFolder+maxDcDtOH+simResults);
//		Dataset<Row> csv = spark.read().format("csv").option("header","true").load(resultsFolder+maxDcDtOH+simResults);
//		csv.show();
	}
	
	
	public List<String> processResults(String jobFolderPath, String mechanismIRI, Integer topN) throws IOException, MoDSSensAnaAgentException {
		// process ign sens results, get a file to record the rxns selected
		List<String> ignRxnIRI = processResponse("subtype_IgnitionDelay", mechanismIRI, jobFolderPath, topN);
		// process flame sens results, get a file to record the rxns selected
		List<String> flameRxnIRI = processResponse("subtype_LaminarFlameSpeed", mechanismIRI, jobFolderPath, topN);
		// merge the two files and get one final list
		List<String> supersetRxns = mergeTwoResponses(ignRxnIRI, flameRxnIRI);
		
		logger.info("Sensitivity analysis results have been processed successfully.");
		return supersetRxns;
	}
	
	
	public List<String> processResponse(String response, String mechanismIRI, String jobFolderPath, Integer topN) throws IOException, MoDSSensAnaAgentException {
		if (!jobFolderPath.endsWith("\\")) {
			jobFolderPath = jobFolderPath.concat("\\");
		}
		String responseResults = jobFolderPath+"SensitivityAnalysis\\SensitivityAnalysis_"+response+".csv";
		String sensAnaResults = jobFolderPath+"SensitivityAnalysis\\SensitivityAnalysis_Sensitivities.csv";
		String deriResults = jobFolderPath+"SensitivityAnalysis\\SensitivityAnalysis_Derivatives.csv";
		String rxnOutputResults = jobFolderPath+"SensitivityAnalysis\\SensitivityAnalysis_SelectedRxns_"+response+".csv";
		
		// take input, process, then write results to output file
		// determine the cases that to be used for reaction selection
		LinkedHashMap<Integer, String> successCases = identifyCases(response, new File(responseResults), new File(sensAnaResults));
		// get the list of active parameters tested in the sensitivity analysis
		LinkedHashMap<Integer, String> activeParameters = getSensParameters(new File(deriResults));
		// load sensitivity analysis results into one map <rxnIndex, averaged sensitivity over all successfully simulated cases>
		LinkedHashMap<String, Double> sensAnaForRxns = computeSensForRxns(successCases, activeParameters, new File(sensAnaResults));		
		// get the top N reactions that most sensitive
		LinkedHashMap<String, Double> selectedRxns = getTopNRxns(sensAnaForRxns, topN);
		
		// output this result to file
		List<String> listOfRxnIRI = writeSelectedRxns(mechanismIRI, new File(rxnOutputResults), selectedRxns);
		
		logger.info("Sensitivity analysis for response "+response+" have been processed successfully.");
		return listOfRxnIRI;
	}
	
	private LinkedHashMap<Integer, String> identifyCases(String response, File simFile, File sensFile) throws IOException, MoDSSensAnaAgentException {
		// read the list of cases and simulation results with original parameters
		String[] casesList = null;
		String[] simResults = null;
		if (simFile.isFile()) {
			BufferedReader br = new BufferedReader(new FileReader(simFile));
			casesList = br.readLine().split(",");
			simResults = br.readLine().split(",");
			br.close();
		}
		
		// read the list of all cases for all responses
		List<String> allCasesList = new ArrayList<>();
		if (sensFile.isFile()) {
			BufferedReader br = new BufferedReader(new FileReader(sensFile));
			allCasesList = new ArrayList<>(Arrays.asList(br.readLine().split(",")));
			br.close();
		}
		
		// select only the cases that simulated successfully, i.e., result is within the pre-specified range
		LinkedHashMap<Integer, String> successCases = new LinkedHashMap<Integer, String>();
		for (int i = 0; i < casesList.length; i++) {
			// TODO further parametrised this comparison part to better cope with range
			if (response.toLowerCase().contains("ign") || response.toLowerCase().contains("delay")) {
				if (Double.valueOf(simResults[i]) > 0 && Double.valueOf(simResults[i]) < 500) {
					successCases.put(allCasesList.indexOf(casesList[i]), casesList[i]);
				}
			} else if (response.toLowerCase().contains("flame") || response.toLowerCase().contains("speed")) {
				if (Double.valueOf(simResults[i]) > 0) {
					successCases.put(allCasesList.indexOf(casesList[i]), casesList[i]);
				}
			}
		}
		
		return successCases;
	}
	
	private LinkedHashMap<Integer, String> getSensParameters(File deriFile) throws IOException, MoDSSensAnaAgentException {
		LinkedHashMap<Integer, String> activeParameters = new LinkedHashMap<Integer, String>();
		if (deriFile.isFile()) {
			BufferedReader br = new BufferedReader(new FileReader(deriFile));
			// pass the first header line
			String line = br.readLine();
			int i = 1; // this indicates the start index of reaction
			while ((line = br.readLine()) != null) {
				// only take the last element of the string
				activeParameters.put(i, line.substring(line.lastIndexOf(",")+1));
				i++; // move to the next reaction
			}
			br.close();
		}
		
		return activeParameters;
	}
	
	private LinkedHashMap<String, Double> computeSensForRxns(LinkedHashMap<Integer, String> successCases, LinkedHashMap<Integer, String> activeParameters, File sensAnaFile) throws IOException, MoDSSensAnaAgentException {
		LinkedHashMap<String, Double> sensTableForRxns = new LinkedHashMap<String, Double>();
		if (sensAnaFile.isFile()) {
			BufferedReader br = new BufferedReader(new FileReader(sensAnaFile));
			// read the header to get the list of simulation cases
			String[] casesHeader = br.readLine().split(",");
			String line = null;
			int i = 1; // this indicates the start index of reaction
			while ((line = br.readLine()) != null) {
				double accSens = 0;
				String[] rxnSens = line.split(",");
				for (Integer j : successCases.keySet()) {
					// IMPORTANT - take absolute value of sensitivity results
					accSens = accSens + Math.abs(Double.valueOf(rxnSens[j]));
				}
				accSens = accSens / successCases.size();
				sensTableForRxns.put(activeParameters.get(i), accSens);
				i++; // move to the next reaction
			}
			br.close();
		}
		
		return sensTableForRxns;
	}
	
	private LinkedHashMap<String, Double> getTopNRxns(Map<String, Double> allRxns, Integer topN) throws IOException, MoDSSensAnaAgentException {
		// sort all reactions based on its sensitivity values
		LinkedHashMap<String, Double> sortedRxns = sortRxnSens(allRxns);
		
		// select only the top N reactions based on user requirement
		LinkedHashMap<String, Double> topNRxns = new LinkedHashMap<String, Double>();
		List<String> keys = new ArrayList<>(sortedRxns.keySet());
		for (String rxnNo : keys.subList(0, topN)) {
			topNRxns.put(rxnNo, sortedRxns.get(rxnNo));
		}
		
		return topNRxns;
	}
	
	private LinkedHashMap<String, Double> sortRxnSens(Map<String, Double> allRxns) throws IOException, MoDSSensAnaAgentException {
		// get the list of reactions based on the map of key and value
		List<Entry<String, Double>> listOfRxns = new LinkedList<Entry<String, Double>>(allRxns.entrySet());
		
		// sort the list of reactions based on its sensitivity values
		Collections.sort(listOfRxns, new Comparator<Entry<String, Double>>() {
			public int compare(Entry<String, Double> sens1, Entry<String, Double> sens2) {
				return sens2.getValue().compareTo(sens1.getValue());
			}
		});
		
		// put sorted reactions in the format of LinkedHashMap so that guarantee the order
		LinkedHashMap<String, Double> rxnsInOrder = new LinkedHashMap<String, Double>();
		for (Entry<String, Double> rxn : listOfRxns) {
			rxnsInOrder.put(rxn.getKey(), rxn.getValue());
		}
		
		return rxnsInOrder;
	}
	
	private List<String> writeSelectedRxns(String mechanismIRI, File resultsFile, LinkedHashMap<String, Double> selectedRxns) throws IOException, MoDSSensAnaAgentException {
		List<String[]> dataLines = new ArrayList<>();
		dataLines.add(new String[] {"No", "RxnIRI", "RxnEquation", "RxnSensitivity"});
		List<String> listOfRxnIRI = new ArrayList<>();
		for (String rxn : selectedRxns.keySet()) {
			OntoKinKG ontoKinKg = new OntoKinKG();
			LinkedHashMap<String, String> rxnIRIandEqu = ontoKinKg.queryReactionBasedOnNo(mechanismIRI, rxn.substring(rxn.lastIndexOf("_")+1));
			String iri = new String();
			String equ = new String();
			for (String rxnIRI : rxnIRIandEqu.keySet()) {
				iri = rxnIRI;
				equ = rxnIRIandEqu.get(rxnIRI);
				listOfRxnIRI.add(iri);
			}
			
			dataLines.add(new String[] {rxn.substring(rxn.lastIndexOf("_")+1), iri, equ, Double.toString(selectedRxns.get(rxn))});
		}
		
		try (PrintWriter pw = new PrintWriter(resultsFile)) {
			dataLines.stream()
			.map(this::convertToCSV)
			.forEach(pw::println);
		}
		
		return listOfRxnIRI;
	}
	
	private List<String> mergeTwoResponses(List<String> response1, List<String> response2) throws IOException, MoDSSensAnaAgentException {
		for (String res : response2) {
			if (!response1.contains(res)) {
				response1.add(res);
			}
		}
		
		return response1;
	}
	
	/**
	 * Convert a string array to a string in the format of CSV file. 
	 * 
	 * @param data
	 * @return
	 */
	private String convertToCSV(String[] data) {
	    return Stream.of(data)
	      .map(this::escapeSpecialCharacters)
	      .collect(Collectors.joining(","));
	}
	
	/**
	 * Escape special characters when converting string array to string in the format of CSV file. 
	 * 
	 * @param data
	 * @return
	 */
	private String escapeSpecialCharacters(String data) {
	    String escapedData = data.replaceAll("\\R", " ");
	    if (data.contains(",") || data.contains("\"") || data.contains("'")) {
	        data = data.replace("\"", "\"\"");
	        escapedData = "\"" + data + "\"";
	    }
	    return escapedData;
	}
	
}
