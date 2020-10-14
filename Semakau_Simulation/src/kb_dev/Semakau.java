package kb_dev;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URI;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import com.cmclinnovations.modsapi.MoDSAPI;
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

public class KB_Dev {
	
	
	public static String PVPIN = new String("C:/apache-tomcat-8.0.24/webapps/input/PVPIN.CSV"); 
	
	public static String PrPWOUTPVCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/PrPWOUTPV.CSV");
	public static String Sim4 = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/Sim_PV1"); 
	public static String XVALUE4 = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/XVALUEPV.CSV");
	
	public static String baseURL = "C:\\apache-tomcat-8.0.24/webapps/ROOT/SemakauPV/";
	
		
	/*public static String Formatting_date(String date) throws Exception {
		
		// this function takes a string of "dd MMM yyyy HH:mm:ss zz" and processes it into a string digestable by ADMS i.e. "yyyy, DDD, HH"  
		Calendar cal1 = new GregorianCalendar();
		Calendar cal2 = new GregorianCalendar();	
		
		// create a string with a 0th date of the current year to calculate the number of days in the year
		String beginning = "00 Jan "+date.split(" ")[2]+" 00:00:00 GMT";
        System.out.println("Beginning" + beginning);
        System.out.println("Date Received" + date.split("GMT")[0].trim());
		
        String trimedDate = date.split("GMT")[0].trim();
        
		// define the date format and parse
		SimpleDateFormat sdf = new SimpleDateFormat("dd MMM yyyy HH:mm:ss z", Locale.ENGLISH);
		cal1.setTime(sdf.parse(date));
		cal2.setTime(sdf.parse(beginning));
	
		// calculate the number of days in the year
		long days = (cal1.getTimeInMillis() - cal2.getTimeInMillis())/(1000*3600*24);
		
		// put together a string to be added to the MET file require for ADMS
		String result = ", "+date.split(" ")[2]+", "+Float.toString(days)+", "+Float.toString(Calendar.HOUR);

		return result;
	}
	*/
	
	public static void Downloading() throws Exception {
		
		
		String gotoFolder = new String("cd " + baseURL );
		String Download_script = new String("Request.py"); //  python script downloading and formatting the weather data
		String Weather_URL = new String("https://inetapps.nus.edu.sg/fas/geog"); //  URL of the weather info page
		String Weather_Data = new String(baseURL + "data.csv"); //  CSV file that contains the weather information 
		String runbat = new String("start " + baseURL + "doRequest.bat");
		/**run the python script downloading the data into a csv file*/
	
		/*
		Runtime rt = Runtime.getRuntime();
//		String[] commands = {gotoFolder,"python", Download_script, Weather_URL, Weather_Data};
		String[] commands = {runbat};
		Process proc = rt.exec(commands);
		proc.waitFor();  
		*/
		
		
		ProcessBuilder pb = new ProcessBuilder("cmd", "/c", "doRequest.bat");
		File dir = new File(baseURL);
		pb.directory(dir);
		Process p = pb.start();
		
		/** start a loop to read weather data from a CSV file; the last line is most recent so only this (and the headers) will be stored*/
		String line = null;
		String last_line = null;
		BufferedReader fileReader = new BufferedReader(new FileReader(Weather_Data));
		
		/** split the headers into an array*/
		String[] headers = fileReader.readLine().split(",");
		
		while ((line = fileReader.readLine()) != null) {			   
		   last_line =  line;
		}
		 fileReader.close();		
		 
		 /** split the data into an array*/
		 String[] data = last_line.split(",");
//		 System.out.println(data[0]);
   
		String ontoPath = "http://www.theworldavatar.com/PVF-001.owl";
		String ontoPath2 = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl";
		
		/** mapping weather data headers (as defined in the python file) to ontology instances and data properties **/
		Map<String, String[]> map = new HashMap<String, String[]>();
		map.put("Solar Irradiance (W/m2)",new String[] {ontoPath+"#"+"V_Irradiance_to_PVF-001",ontoPath2+"#"+"numericalValue"});
		map.put("Date",new String[] {ontoPath+"#"+"Time_SG_Value",ontoPath2+"#"+"numericalValue"});
		
		/** load your knowledge base from an owl file; additionally */
		String filePath = baseURL + "PVF-001.owl";
		FileInputStream inFile= new FileInputStream(filePath);
		Reader in = new InputStreamReader(inFile,"UTF-8");
		JenaOWLModel jenaOwlModel = ProtegeOWL.createJenaOWLModelFromReader(in);		   

		/** this loop creates instances of OWL individuals to hold the weather data**/
		/*for (int i = 0; i <data.length; i++){
		    if (map.get(headers[i]) != null){
			    System.out.println(headers[i]);	
			    OWLIndividual individual = jenaOwlModel.getOWLIndividual(map.get(headers[i])[0]);
			    OWLDatatypeProperty property = jenaOwlModel.getOWLDatatypeProperty(map.get(headers[i])[1]);
			    if (individual != null){
			    	individual.setPropertyValue(property, data[i]);	   
		    	}
		    	else{
		    		OWLNamedClass named_class = jenaOwlModel.getOWLNamedClass("http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#ScalarValue");		
		    		named_class.createRDFIndividual(map.get(headers[i])[0]).setPropertyValue(property, data[i]);	   
		    	}

		    }
		}	   */
		
		OWLIndividual irradiance = jenaOwlModel.getOWLIndividual("http://www.theworldavatar.com/PVF-001.owl#V_Irradiance_to_PVF-001");
		OWLIndividual time = jenaOwlModel.getOWLIndividual("http://www.theworldavatar.com/PVF-001.owl#Time_SG_Value");
		 OWLDatatypeProperty property = jenaOwlModel.getOWLDatatypeProperty("http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue");
		
		 irradiance.setPropertyValue(property, data[3]);
		 time.setPropertyValue(property, data[1]+" "+data[0]+" "+data[2]);
			
		
		FileWriter fW_PV_P = null; // create an object later used for writing a .csv file for the parameterised PW model, using P values from powergen layer.
		// create an object later used for writing a .csv file for the parameterised PW model, using Q values from powergen layer.

	    
	    OWLIndividual area = jenaOwlModel.getOWLIndividual("http://www.theworldavatar.com/PVF-001.owl#V_Area_PVF-001");
	    OWLIndividual efficiency = jenaOwlModel.getOWLIndividual("http://www.theworldavatar.com/PVF-001.owl#ValueOfEfficiencyOfPVF-001");
	    
	    OWLIndividual individual5 = jenaOwlModel.getOWLIndividual("http://www.theworldavatar.com/PVF-001.owl#V_Pset_PVF-001");
	    OWLIndividual nomV = jenaOwlModel.getOWLIndividual("http://www.theworldavatar.com/PVF-001.owl#V_NominalV_PVF-001");
	    
	       String areavalue = area.getPropertyValueLiteral(jenaOwlModel.getOWLProperty("http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue")).getString();
	       String effvalue = efficiency.getPropertyValueLiteral(jenaOwlModel.getOWLProperty("http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue")).getString();
	       String irvalue = irradiance.getPropertyValueLiteral(jenaOwlModel.getOWLProperty("http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue")).getString();
	       String nomvalue = nomV.getPropertyValueLiteral(jenaOwlModel.getOWLProperty("http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue")).getString();
	       
	       individual5.setPropertyValue(property,Float.parseFloat(irvalue)*Float.parseFloat(areavalue)*Float.parseFloat(effvalue)/1000000); 
		try {
			
			fW_PV_P = new FileWriter(PVPIN);
			

			
			fW_PV_P.append("   ,OBJECTID, P_set_MW, V_set_kV, setPUVoltage");
			fW_PV_P.append("\n");

			
			
				//for loadpoint  writing
			for (int i = 0; i < 3; i++) { // iterate over all members of the Load_Point attributeslist (total of 108), which contains information on multiple graphic elements
				fW_PV_P.append("OBJECTID"); // add the header "FID" to the data stream of the file that will contain the pwr_P value.
				fW_PV_P.append(",");
				fW_PV_P.append(String.valueOf(i+1)); // capture the FID value, convert it to a string and add it to the data stream
				fW_PV_P.append(", ");
				fW_PV_P.append(String.valueOf(Float.parseFloat(irvalue)*Float.parseFloat(areavalue)*Float.parseFloat(effvalue)/1000000)); // capture the pwr_P value that corresponds to the FID, convert it to a string and add it to the data stream
				fW_PV_P.append(", ");
				fW_PV_P.append(nomvalue);
				fW_PV_P.append(", ");
				Double a=Double.parseDouble(nomvalue);
				Double b= a/0.75;
				String setpuvoltage= Double.toString(b);
				fW_PV_P.append(setpuvoltage);
				fW_PV_P.append("\n");
	
				}

			
			
			fW_PV_P.flush(); // passes the data from fW_PG to PGIN.csv
						
			fW_PV_P.close();
			
		} catch (Exception e) {
			e.printStackTrace();
		}
		System.out.println("value of irradiance ="+ Float.parseFloat(irvalue));
		System.out.println("value of power ="+ Float.parseFloat(irvalue)*Float.parseFloat(areavalue)*Float.parseFloat(effvalue)/1000000);
		   
		/**save the updated model file*/
		Collection errors = new ArrayList();
		jenaOwlModel.save(new URI("file:////" + filePath.replace("\\", "/")), FileUtils.langXMLAbbrev, errors, jenaOwlModel.getOntModel());
		System.out.println("File saved with " + errors.size() + " errors.");  
		   
		
		   
	}
	
	
	public static void Running_Semakau() throws Exception {
		
		List<List<Double>> xData = new ArrayList<>(1);
		List<Double> xRow = new ArrayList<>();
		List<List<Double>> yData;
		
		BufferedReader fileReader = null;
		FileWriter XValue = null;
		
		XValue = new FileWriter(XVALUE4);
		for (int k = 0; k < 3; k++) {
			String line = null;
			fileReader = new BufferedReader(new FileReader(PVPIN)); // read the pwr_p data from CSV file
			fileReader.readLine();                                  // Read the CSV file header to skip it
			while ((line = fileReader.readLine()) != null) {
				String[] data = line.split(",");
				System.out.println("1"/*ArcGISFID[k]*/ + " & " + data[1]);
				if (/*ArcGISFID[k]*/"1".equals(data[1])) {                  // append the pwr_P value if the ArcGIS FID is the demanded FID
					XValue.append("X" + k + "=" + data[2].trim());
					XValue.append("\n");
					System.out.println("Xvalue" + k + "=" + data[2]);
					xRow.add(Double.parseDouble(data[2].trim()));  // convert string to double and add it tO xRow
					break;
				}
			}
			
			}
		for (int n = 0; n < 3; n++) {
			String line1 = null;
			fileReader = new BufferedReader(new FileReader(PVPIN)); // read the  pwr_Q data from CSV file
			while ((line1 = fileReader.readLine()) != null) {
				String[] data = line1.split(",");
				if (/*ArcGISFID[k]*/"1".equals(data[1])) {                                          // append the pwr_Q value if the ArcGIS FID is the demanded FID
					XValue.append("X" + n+3 + "=" + data[3].trim());
					XValue.append("\n");
					System.out.println("Xvalue_puV" + n + "=" + data[3]);
					xRow.add(Double.parseDouble(data[3].trim()));                            // convert string to double and add to xRow
					break;
				}
			}
		}
		XValue.flush();                                                                       // passes the data from LPPIN to XValue.csv
		XValue.close();

		
		xData.add(xRow); 
		System.out.println("xData=" + xData);
		
		String simDir = Sim4;                                                                          // pass the directory of the power world sorrogate model to simDir
		String modelName = "HDMR_Alg_1";
		FileWriter fileWriter = null;
		try {			
			// System.out.println(MoDSAPI.class);
			fileWriter = new FileWriter(PrPWOUTPVCSV); // filewriter for the output csv
			System.load("C:/apache-tomcat-8.0.24/webapps/ROOT/MoDS_Java_API_0.1.dll");                     // not recommended--Messing with the library path on the command line		
			
//			ArrayList<String> xNames = MoDSAPI.getXVarNamesFromAPI(simDir, modelName);
			ArrayList<String> yNames = MoDSAPI.getYVarNamesFromAPI(simDir, modelName);

				for (int j = 0; j < yNames.size(); j++) {
					fileWriter.append(yNames.get(j));                                                   // write the yNames to the output CSV file
					fileWriter.append(",");
				}
				
			} catch (Error e) {
		e.printStackTrace();
				
			} catch (IOException e) {
		e.printStackTrace();			
			}

		yData = MoDSAPI.evaluateSurrogate(simDir, modelName, xData); // call MoDS API to evaluate the surrogate model basing on the MoDS simulation file "simDir -> modelNam" and the input xData that was collected before
		System.out.println("Success!");														
		System.out.println("yData=" + yData); 
		
		for (int j = 0; j < yData.size(); j++) {
			try {
				fileWriter.append("\n");
				for (int k = 0; k < yData.get(j).size(); k++) {
					fileWriter.append(Double.toString(yData.get(j).get(k)));                        // write the yData to the output CSV file
					fileWriter.append(",");
				}
			} catch (IOException e) {
				e.printStackTrace();
			} finally {
				try {
					fileWriter.flush();
					fileWriter.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}
		
		/*load the new ontology to save the output value" */
		
		
		String csvFile = PrPWOUTPVCSV;
        String line = "";
        String cvsSplitBy = ",";

        /** load your knowledge base from an owl file; additionally */
		String filePath = baseURL + "PVF-001.owl";
		FileInputStream inFile= new FileInputStream(filePath);
		Reader in = new InputStreamReader(inFile,"UTF-8");
		JenaOWLModel jenaOwlModel = ProtegeOWL.createJenaOWLModelFromReader(in);	
        
        try (BufferedReader br = new BufferedReader(new FileReader(csvFile))) 
        {
        	br.readLine();
        	int i=0;
            while ((line = br.readLine()) != null) {
            	
                // use comma as separator
                String[] data = line.split(cvsSplitBy);
                OWLIndividual individual1 = jenaOwlModel.getOWLIndividual("http://www.theworldavatar.com/PVF-001.owl#V_theta_PVF-001");
                OWLIndividual individual2 = jenaOwlModel.getOWLIndividual("http://www.theworldavatar.com/PVF-001.owl#V_ActualV_PVF-001");
                OWLIndividual individual3 = jenaOwlModel.getOWLIndividual("http://www.theworldavatar.com/PVF-001.owl#V_P_PVF-001");
                OWLIndividual individual4 = jenaOwlModel.getOWLIndividual("http://www.theworldavatar.com/PVF-001.owl#V_Q_PVF-001");
                
			    OWLDatatypeProperty property = jenaOwlModel.getOWLDatatypeProperty("http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue");
			
			       
			       
			    individual1.setPropertyValue(property, data[29]);	
			    individual2.setPropertyValue(property, data[30]);	
			    individual3.setPropertyValue(property, data[33]);	
			    individual4.setPropertyValue(property, data[34]);	
			    
			  	
			    
                System.out.println("theta=  " + data[29]);
                System.out.println("actualv=  " + data[30]);
                System.out.println("P=  " + data[33]);
                System.out.println("Q=  " + data[34]);
                
                i++ ;
            }

        } catch (IOException e) {
            e.printStackTrace();
        }
		
        //URI file=URI.create("file:///C:/apache-tomcat-8.0.24/webapps/ROOT/SemakauPV/PVF-001.owl");
        //jenaOwlModel.save(file);
        		  
        		  
        			/**save the updated model file*/
        			Collection errors = new ArrayList();
        			jenaOwlModel.save(new URI("file:////" + filePath.replace("\\", "/")), FileUtils.langXMLAbbrev, errors, jenaOwlModel.getOntModel());
        			System.out.println("File saved with " + errors.size() + " errors.");
        			System.out.println("file saved!");
		  
	
	}   
	
	public static void main(String[] args) throws Exception {
		System.out.println("Starting Process");
		Downloading();
		Running_Semakau();
		 
	}
	
}
