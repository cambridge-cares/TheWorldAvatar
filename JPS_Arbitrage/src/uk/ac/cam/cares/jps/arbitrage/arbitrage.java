package uk.ac.cam.cares.jps.arbitrage;


import com.cmclinnovations.mods.api.MoDSAPI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URI;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;


import com.hp.hpl.jena.util.FileUtils;
import com.ibm.icu.util.Calendar;
import com.ibm.icu.util.GregorianCalendar;

import edu.stanford.smi.protege.exception.OntologyLoadException;
import edu.stanford.smi.protegex.owl.ProtegeOWL;
import edu.stanford.smi.protegex.owl.jena.JenaOWLModel;
import edu.stanford.smi.protegex.owl.model.OWLDatatypeProperty;
import edu.stanford.smi.protegex.owl.model.OWLIndividual;
import edu.stanford.smi.protegex.owl.model.OWLModel;
import edu.stanford.smi.protegex.owl.model.OWLNamedClass;
import edu.stanford.smi.protegex.owl.model.RDFIndividual;
import edu.stanford.smi.protegex.owl.model.RDFProperty;
import edu.stanford.smi.protegex.owl.model.RDFSNamedClass;



public class arbitrage {
	
	public static String Formatting_date(String date) throws Exception {
		
		/** this function takes a string of "dd MMM yyyy HH:mm:ss zz" and
        processes it into a string digestable by ADMS i.e. "yyyy, DDD, HH" */ 
		Calendar cal1 = new GregorianCalendar();
		Calendar cal2 = new GregorianCalendar();	
		
		/** create a string with a 0th date of the current year to calculate the number of days in the year*/
		String beginning = "00 Jan "+date.split(" ")[2]+" 00:00:00 GMT";

		
		/** define the date format and parse*/
		SimpleDateFormat sdf = new SimpleDateFormat("dd MMM yyyy HH:mm:ss zz");
		cal1.setTime(sdf.parse(date));
		cal2.setTime(sdf.parse(beginning));
		
		/** calculate the number of days in the year*/
		long days = (cal1.getTimeInMillis() - cal2.getTimeInMillis())/(1000*3600*24);
		
		/** put together a string to be added to the MET file require for ADMS*/
		String result = ", "+date.split(" ")[2]+", "+Float.toString(days)+", "+Float.toString(Calendar.HOUR);

		return result;
	}
	
	
	public static void Running() throws Exception {
		
		/** ontology addresses*/
		String ontoPath = "http://www.mascem.gecad.isep.ipp.pt/ontologies/electricity-markets.owl";
		String ontoPath2 = "http://www.semanticweb.org/janusz/ontologies/2018/3/untitled-ontology-13";

		
		/** mapping weather data headers (as defined in the python file) to ontology instances and data properties **/
		Map<String, String[]> map = new HashMap<String, String[]>();
		map.put("U",new String[] {ontoPath+"#"+"WindSpeed_SG_Value",ontoPath2+"#"+"numericalValue",""});
		map.put("PHI",new String[] {ontoPath+"#"+"WindDirection_SG_Value",ontoPath2+"#"+"numericalValue",""});
		map.put("T0C",new String[] {ontoPath+"#"+"Temperature_SG_Value",ontoPath2+"#"+"numericalValue",""});
		map.put("CL",new String[] {ontoPath+"#"+"CloudCover_SG_Value",ontoPath2+"#"+"numericalValue",""});
		map.put("Date",new String[] {ontoPath+"#"+"Time_SG_Value",ontoPath2+"#"+"numericalValue",""});


		   /**get model from an owl file*/
		   String filePath = "C:/Users/Janusz/Desktop/Commodity_prices/Ontology/OntoArbitrage_Market_KB.owl";
		   OWLModel owlModel = null;
		   
		   try {
		      owlModel = ProtegeOWL.createJenaOWLModelFromURI("file:/"+filePath);
		     } catch (OntologyLoadException e1) {
		      e1.printStackTrace();
		     }

		   /** this loop creates instances of OWL individuals to get the weather data**/
		   for (Map.Entry<String, String[]> entry : map.entrySet()){
//			   System.out.println(entry.getValue()[0]);
//			   System.out.println(entry.getValue()[1]);
			   OWLIndividual M = owlModel.getOWLIndividual(entry.getValue()[0]);
		       String name = M.getPropertyValueLiteral(owlModel.getOWLProperty(entry.getValue()[1])).getString();
		       entry.getValue()[2] = name;
//			   System.out.println(name);
		   }
		   
		   /** define headers required by ADMS in its MET file and then write it in the format: storage entry per row and the corresponding data in the final row */ 
		   String[] storage = {"VARIABLES:", "9", "STATION DCNN", "YEAR", "TDAY", "THOUR", "T0C", "U", "PHI", "P", "CL", "DATA:"};
		   String temp = "4238.0";		   // the final line starts with the number of weather station
		   for (int i = 0; i <storage.length; i++){
			   if (map.get(storage[i]) == null && storage[i] != "YEAR") {
				continue;   
			   }
			   
			   if (storage[i] == "YEAR") {
			   temp += Formatting_date(map.get("Date")[2]); // formats the date appropriately for ADMS
		   }
		   else {
			   temp += ", "+map.get(storage[i])[2];
		   }
		   }
		   System.out.println(temp);
		 
		  		  
		   /** addresses of, respectively, input, meteorological and output files*/
		   String filePath_apl = "C:/Users/Janusz/Desktop/ADMS/test.apl";
		   String filePath_met = "C:/Users/Janusz/Desktop/ADMS/test.met";
		   String filePath_gst = "C:/Users/Janusz/Desktop/ADMS/test.gst";

		   /** write the met file*/		   
		   try {
	            // Assume default encoding.
	            // Always wrap FileWriter in BufferedWriter.
			   FileWriter fileWriter = new FileWriter(filePath_met, false);
	           BufferedWriter bufferedwriter = new BufferedWriter(fileWriter);
			   for (String entry : storage){
				   bufferedwriter.write(entry);
				   bufferedwriter.newLine();
			   }
			   bufferedwriter.write(temp);
			   bufferedwriter.close();
		   }
		   catch (IOException e){
			   System.out.println(e.getMessage());
		   }
	
		   /** read the apl file and store all the lines; replace one with the address of the met file*/		
	        String line = null;
	        ArrayList<String> apl = new ArrayList<String>();

		   try {
	            // FileReader reads text files in the default encoding.
	            FileReader fileReader = 
	                new FileReader(filePath_apl);

	            // Always wrap FileReader in BufferedReader.
	            BufferedReader bufferedReader = 
	                new BufferedReader(fileReader);

	            while((line = bufferedReader.readLine()) != null) {
	            	
	            	if (line.contains("MetDataFileWellFormedPath")) {
	            		apl.add("MetDataFileWellFormedPath = "+ filePath_met);
	            		continue;
	            	}
	            	apl.add(line);
	            }   
	            
	            // Always close files.
	            bufferedReader.close();         
	        }
	        catch(FileNotFoundException ex) {
	            System.out.println("Unable to open file");                
	        }
		   catch (IOException e){
			   System.out.println(e.getMessage());
		   }
		   
		   /** write the apl file*/		
		   try {
			   
	            // Assume default encoding.
	            // Always wrap FileWriter in BufferedWriter.
			   FileWriter fileWriter = new FileWriter(filePath_apl, false);
	           BufferedWriter bufferedwriter = new BufferedWriter(fileWriter);
			   for (int i = 0; i <apl.size(); i++){
				   bufferedwriter.write(apl.get(i));
				   bufferedwriter.newLine();
			   }
			   bufferedwriter.close();
		   }
		   catch (IOException e){
			   System.out.println(e.getMessage());
		   }
		   
		   
		   
		   /**run the python script executing ADMS
		   Runtime rt = Runtime.getRuntime();
		   String[] commands = {"C:\\Program Files (x86)\\CERC\\ADMS 5\\ADMSModel.exe", "/e2", "ADMS", "test.APL"};
		   Process proc = rt.exec(commands);
		   proc.waitFor();  
		   */
		   
		   /**get model from an owl file*/
		   FileInputStream inFile= new FileInputStream(filePath);
		   Reader in = new InputStreamReader(inFile,"UTF-8");
		   JenaOWLModel jenaOwlModel = ProtegeOWL.createJenaOWLModelFromReader(in);		   

		   /** store the addresses of the newly created files in the ontology */
		   try{
			    jenaOwlModel.getOWLIndividual(ontoPath+"#ADMS_SG_APL_Input").setPropertyValue(jenaOwlModel.getOWLDatatypeProperty(ontoPath2+"#numericalValue"), filePath_apl);	   
			    jenaOwlModel.getOWLIndividual(ontoPath+"#ADMS_SG_MET_Input").setPropertyValue(jenaOwlModel.getOWLDatatypeProperty(ontoPath2+"#numericalValue"), filePath_met);	   
			    jenaOwlModel.getOWLIndividual(ontoPath+"#ADMS_SG_GST_Output").setPropertyValue(jenaOwlModel.getOWLDatatypeProperty(ontoPath2+"#numericalValue"), filePath_gst);	   
			   }
		   catch (Exception e1) {
			      e1.printStackTrace();
			     }

		   /**save the model to another owl file*/
		   Collection errors = new ArrayList();
		   jenaOwlModel.save(new URI("file:/"+filePath), FileUtils.langXMLAbbrev, errors, jenaOwlModel.getOntModel());
		   System.out.println("File saved with " + errors.size() + " errors.");  
		   }
		   
	public static void Running_analysis_1() throws Exception {
		
		/** this function executes 4 Python scripts which download market data and stores it in separate CSV files */ 
		
		String CPO_download = new String("C:\\Users\\Janusz\\Desktop\\Commodity_prices\\CPO_to_FAME.py"); 
		
		String market_data_plot = new String("C:\\Users\\Janusz\\Desktop\\Commodity_prices\\Market_data\\arbitrage_CPO.png"); 

		String[][] commands = {{"python", CPO_download, market_data_plot},
				};

		/**Python scripts are run via cmd one by one*/
		Runtime rt = Runtime.getRuntime();
		for (int i = 0; i <commands.length; i++){
			Process proc = rt.exec(commands[i]);
			proc.waitFor();
			if (i == commands.length-1){
				proc.destroy();
				}
		}
		   
		   
		   
	}
	
	
	public static void Downloading_market_data() throws Exception {
		
		/** this function executes 4 Python scripts which download market data and stores it in separate CSV files */ 
		
		String CPO_download = new String("C:\\Users\\Janusz\\Desktop\\Commodity_prices\\Market_data\\CPO_download.pyw"); 
		String FAME_download = new String("C:\\Users\\Janusz\\Desktop\\Commodity_prices\\Market_data\\FAME_download.pyw"); 
		String ZCE_download = new String("C:\\Users\\Janusz\\Desktop\\Commodity_prices\\Market_data\\ZCE_download.pyw"); 
		String HNG_download = new String("C:\\Users\\Janusz\\Desktop\\Commodity_prices\\Market_data\\HNG_download.pyw");
		
		String CPO_data = new String("C:\\Users\\Janusz\\Desktop\\Commodity_prices\\Market_data\\CPO_data.csv"); 
		String FAME_data = new String("C:\\Users\\Janusz\\Desktop\\Commodity_prices\\Market_data\\FAME_data.csv");
		String ZCE_data = new String("C:\\Users\\Janusz\\Desktop\\Commodity_prices\\Market_data\\ZCE_data.csv"); 
		String HNG_data = new String("C:\\Users\\Janusz\\Desktop\\Commodity_prices\\Market_data\\HNG_data.csv"); 

		String[][] commands = {{"python", CPO_download, CPO_data},
				{"python", FAME_download, FAME_data},
				{"python", ZCE_download, ZCE_data},
				{"python", HNG_download, HNG_data}
				};

		/**Python scripts are run via cmd one by one*/
		Runtime rt = Runtime.getRuntime();
		for (int i = 0; i <commands.length; i++){
			Process proc = rt.exec(commands[i]);
			proc.waitFor();
			if (i == commands.length-1){
				proc.destroy();
				}
		}

		
		/** URIs of ontologies used to define KBs in which market data will be stored*/ 
		String ontoPath = "http://www.mascem.gecad.isep.ipp.pt/ontologies/electricity-markets.owl";
		String ontoPath2 = "http://www.semanticweb.org/janusz/ontologies/2018/3/untitled-ontology-13";

		
		
		/** knowledge base from an owl file in a jenaOWL model; URIs of relevant individuals and their properties are defined and
		 * locations of the CSV files with the market data are stored in KB one by one */
		String filePath = "C:/Users/Janusz/Desktop/Commodity_prices/Ontology/OntoArbitrage_Market_KB.owl";
		FileInputStream inFile= new FileInputStream(filePath);
		Reader in = new InputStreamReader(inFile,"UTF-8");
		JenaOWLModel jenaOwlModel = ProtegeOWL.createJenaOWLModelFromReader(in);
		
		
		String[][] addresses = {{ontoPath+"#"+"data", ontoPath2+"#"+"CMECrudePalmOil_001"},
				{ontoPath+"#"+"data", ontoPath2+"#"+"CMEBiodiesel_001"},
				{ontoPath+"#"+"data", ontoPath2+"#"+"ZCEMethanol_001"},
				{ontoPath+"#"+"data", ontoPath2+"#"+"CMENaturalGas_001"}
				};
		
		for (int i = 0; i <addresses.length; i++){
			RDFProperty property = jenaOwlModel.getRDFProperty(addresses[i][0]);
			RDFIndividual individual = jenaOwlModel.getRDFIndividual(addresses[i][1]);
			individual.setPropertyValue(property, commands[i][2]);
		}
				

		   
		/**save the updated model file; also, any error messages are collected and printed*/
		Collection errors = new ArrayList();
		jenaOwlModel.save(new URI("file:/"+filePath), FileUtils.langXMLAbbrev, errors, jenaOwlModel.getOntModel());
		System.out.println("File saved with " + errors.size() + " errors.");  
		   
		   
		   
	}
	
	public static void Downloading_currencies() throws Exception {
		
		/** this function executes a Python script which downloads exchange rates and stores it in separate CSV files;
		 * the currencies are defined within the script; the rates are printed to the console by the script thus allowing to store them
		 * in KB */ 

		String currency_download = new String("C:\\Users\\Janusz\\Desktop\\Commodity_prices\\Market_data\\exchange_rates.pyw"); 

		String currency_data = new String("C:\\Users\\Janusz\\Desktop\\Commodity_prices\\Market_data\\exchange_rates.csv"); 

		String[] commands = {"python", currency_download, currency_data};

		/**run the Python script downloading the data into a csv file and capture its console output*/
		Runtime rt = Runtime.getRuntime();
		Process proc = rt.exec(commands);
		proc.waitFor();
		BufferedReader stdInput = new BufferedReader(new InputStreamReader(proc.getInputStream()));
		proc.destroy();

		/** split the console output into headers and exchange rates*/
		String[] headers = stdInput.readLine().split(",");
		String[] data = stdInput.readLine().split(",");
		
		/** URIs of ontologies used to define KBs in which market data will be stored*/ 
		String ontoPath = "http://www.semanticweb.org/janusz/ontologies/2018/3/untitled-ontology-15"; //KB
		String ontoPath2 = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl";
		
		/** URIs of relevant individuals and their properties are defined */
		String[][] addresses = new String[headers.length][];
		for (int i = 0; i <addresses.length; i++){
			addresses[i] = new String[] {ontoPath2+"#"+"numericalValue", ontoPath+"#"+"V_"+headers[i]};
			System.out.println(addresses[i][1]);
		}
		
		
		
		/** knowledge base from an owl file in a jenaOWL model; rates are stored in KB one by one */
		String filePath = "C:/Users/Janusz/Desktop/Commodity_prices/Ontology/OntoArbitrage_PlantInfo_KB.owl";
		FileInputStream inFile= new FileInputStream(filePath);
		Reader in = new InputStreamReader(inFile,"UTF-8");
		JenaOWLModel jenaOwlModel = ProtegeOWL.createJenaOWLModelFromReader(in);

		for (int i = 0; i <addresses.length; i++){
			RDFProperty property = jenaOwlModel.getRDFProperty(addresses[i][0]);
			RDFIndividual individual = jenaOwlModel.getRDFIndividual(addresses[i][1]);
			individual.setPropertyValue(property, data[i]);
		}
				

		/**save the updated model file; also, any error messages are collected and printed*/
		Collection errors = new ArrayList();
		jenaOwlModel.save(new URI("file:/"+filePath), FileUtils.langXMLAbbrev, errors, jenaOwlModel.getOntModel());
		System.out.println("File saved with " + errors.size() + " errors.");  
		   
		   
		   
	}
	
	
	public static void Storing_Aspen_data() throws Exception {
		
		/** this function executes a Python script which prints input and output headers and data from an Aspen model;
		 *  information to be sourced from the model and printed is defined in the script;
		 *  data is stored in the relevant KB*/ 
		
		String Aspen_data = new String("C:\\Users\\Janusz\\Desktop\\Commodity_prices\\print_Aspen_data.pyw");

		String[] commands = {"python", Aspen_data};

		/**run the Python script printing the Aspen data and capture the console output*/
		Runtime rt = Runtime.getRuntime();
		Process proc = rt.exec(commands);
		proc.waitFor();
		BufferedReader stdInput = new BufferedReader(new InputStreamReader(proc.getInputStream()));
		proc.destroy();

		/** split the console output into headers and exchange rates*/
		String[] headers = stdInput.readLine().split(",");
		String[] data = stdInput.readLine().split(",");
		
		/** URIs of ontologies used to define KBs in which market data will be stored*/ 
		String ontoPath = "http://www.semanticweb.org/janusz/ontologies/2018/3/untitled-ontology-15"; //KB
		String ontoPath2 = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl";
		
		/** URIs of relevant individuals and their properties are defined */
		String[][] addresses = new String[headers.length][];

		for (int i = 0; i <addresses.length; i++){
			addresses[i] = new String[] {ontoPath2+"#"+"numericalValue", ontoPath+"#"+"V_"+headers[i]};
		}
		
		
		
		/** knowledge base from an owl file in a jenaOWL model; rates are stored in KB one by one */
		String filePath = "C:/Users/Janusz/Desktop/Commodity_prices/Ontology/OntoArbitrage_PlantInfo_KB.owl";
		FileInputStream inFile= new FileInputStream(filePath);
		Reader in = new InputStreamReader(inFile,"UTF-8");
		JenaOWLModel jenaOwlModel = ProtegeOWL.createJenaOWLModelFromReader(in);

		for (int i = 0; i <addresses.length; i++){
			RDFProperty property = jenaOwlModel.getRDFProperty(addresses[i][0]);
			RDFIndividual individual = jenaOwlModel.getRDFIndividual(addresses[i][1]);
			individual.setPropertyValue(property, data[i]);
		}
				

		/**save the updated model file; also, any error messages are collected and printed*/
		Collection errors = new ArrayList();
		jenaOwlModel.save(new URI("file:/"+filePath), FileUtils.langXMLAbbrev, errors, jenaOwlModel.getOntModel());
		System.out.println("File saved with " + errors.size() + " errors.");  
		   
		   
		   
	}
	
	public static void Reading_data() throws Exception {
		
		/** this function executes a Python script which prints input and output headers and data from an Aspen model;
		 *  information to be sourced from the model and printed is defined in the script;
		 *  data is stored in the relevant KB*/ 
		
		String print = new String("C:\\Users\\Janusz\\Desktop\\Commodity_prices\\print_headers.pyw");

		String[] commands = {"python", print};

		/**run the Python script printing the Aspen data and capture the console output*/
		Runtime rt = Runtime.getRuntime();
		Process proc = rt.exec(commands);
		proc.waitFor();
		BufferedReader stdInput = new BufferedReader(new InputStreamReader(proc.getInputStream()));
		proc.destroy();

		/** split the console output into headers and exchange rates*/
		String[] headers = stdInput.readLine().split(",");
		
		/** URIs of ontologies used to define KBs in which market data will be stored*/ 
		String ontoPath = "http://www.semanticweb.org/janusz/ontologies/2018/3/untitled-ontology-15"; //KB
		String ontoPath2 = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl";
		
		
		
		
		/** ontology addresses*/
		String ontoPath3 = "http://www.mascem.gecad.isep.ipp.pt/ontologies/electricity-markets.owl";
		String ontoPath4 = "http://www.semanticweb.org/janusz/ontologies/2018/3/untitled-ontology-13";
		
		
		
		String[][] addresses2 = {{ontoPath3+"#"+"data", ontoPath4+"#"+"CMECrudePalmOil_001"},
				{ontoPath3+"#"+"data", ontoPath4+"#"+"CMEBiodiesel_001"},
				{ontoPath3+"#"+"data", ontoPath4+"#"+"ZCEMethanol_001"},
				{ontoPath3+"#"+"data", ontoPath4+"#"+"CMENaturalGas_001"}
				};

		
		/** URIs of relevant individuals and their properties are defined */
		String[][] addresses = new String[headers.length][];
		for (int i = 0; i <addresses.length; i++){
			addresses[i] = new String[] {ontoPath2+"#"+"numericalValue", ontoPath+"#"+headers[i]};
			System.out.println(addresses[i][1]);
		}
		

		   /**get model from an owl file*/
		   String filePath = "C:/Users/Janusz/Desktop/Commodity_prices/Ontology/OntoArbitrage_PlantInfo_KB.owl";
		   OWLModel owlModel = null;
		   
		   try {
		      owlModel = ProtegeOWL.createJenaOWLModelFromURI("file:/"+filePath);
		     } catch (OntologyLoadException e1) {
		      e1.printStackTrace();
		     }

				
			for (int i = 0; i <addresses.length; i++){
				RDFIndividual individual = owlModel.getRDFIndividual(addresses[i][1]);
				String name = individual.getPropertyValueLiteral(owlModel.getRDFProperty(addresses[i][0])).getString();
				System.out.println(name);
				}

			
			
			   /**get model from an owl file*/
			   String filePath2 = "C:/Users/Janusz/Desktop/Commodity_prices/Ontology/OntoArbitrage_Market_KB.owl";
			   OWLModel owlModel2 = null;
			   
			   try {
				   owlModel2 = ProtegeOWL.createJenaOWLModelFromURI("file:/"+filePath2);
			     } catch (OntologyLoadException e1) {
			      e1.printStackTrace();
			     }
			
			
			for (int i = 0; i <addresses2.length; i++){
				RDFIndividual individual = owlModel2.getRDFIndividual(addresses2[i][1]);
				String name = individual.getPropertyValueLiteral(owlModel2.getRDFProperty(addresses2[i][0])).getString();
				System.out.println(name);
				}
		   
		   
	}
	

	public static void MoDS(final String[] args) {
	    final String simDir = "E:\\MoDS_Projects\\Arbitrage\\Models\\CPO_to_FAME";
	    final String surrogateAlgName = "HDMR_0%2E9%2D1%2E1_001";
	    // Example is for a surrogate with 10 input variables
	    // Note that IndexOutOfBoundsException is thrown if you supply too many inputs, but *no exception is thrown if you supply too few!*.
	    final List<Double> inputs_1 = new ArrayList<>(Arrays.asList(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0));
	    final List<Double> inputs_2 = new ArrayList<>(Arrays.asList(2.0, 4.0, 6.0, 8.0, 10.0, 12.0, 14.0, 16.0, 18.0, 20.0));
	    final List<Double> inputs_3 = new ArrayList<>(Arrays.asList(3.0, 6.0, 9.0, 12.0, 15.0, 18.0, 21.0, 24.0, 27.0, 30.0));
	    final List<List<Double>> inputs_batch = new ArrayList<>(Arrays.asList(inputs_1, inputs_2, inputs_3));
	
	    // Evaluate the surrogate for a single set of inputs.  The result is a List<Double> of size Noutputs
	    List<Double> outputs1 = MoDSAPI.evaluateSurrogate(simDir, surrogateAlgName, inputs_1);
	    // Evaluate the surrogate for multiple sets of inputs.  The result is a List<List<Double>> with outer size NinputSets (3 in this case) and inner size Noutputs
	    List<List<Double>> outputs_batch = MoDSAPI.batchEvaluateSurrogate(simDir, surrogateAlgName, inputs_batch);
}	  	   
	
	public static void main(String[] args) throws Exception {
		//Downloading_market_data();
		//Downloading_currencies();
		//Storing_Aspen_data();
		//Reading_data();
		Running_analysis_1();
		//Running();
	}
}
