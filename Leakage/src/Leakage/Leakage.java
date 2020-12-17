package Leakage;

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
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
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
import edu.stanford.smi.protegex.owl.model.OWLObjectProperty;



public class Leakage {
	public static String PVPIN = new String("C:/apache-tomcat-8.0.24/webapps/input/TLIN.CSV"); 

	//public static String PrPWOUTPVCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/TLOUT.CSV");
	//public static String Sim4 = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/Sim1"); 
	//public static String XVALUE4 = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/XLEAK.CSV");

	public static String baseURL = "C:\\apache-tomcat-8.0.24/webapps/ROOT/GasLeakage/";
	
	static String tempvalue;
	static String pressvalue;
	static String diametervalue;
	static String Pamb;
	static String gammavalue;
	static String mvalue;
	static String compound="chlorine"; //input of compound
	static String compound2;
	static Double flowrate;
	static String tank;
	

	
	
public static void Downloading() throws Exception {
			
		
		/**run the python script downloading the data into a csv file*/
	
		/*
		Runtime rt = Runtime.getRuntime();
//		String[] commands = {gotoFolder,"python", Download_script, Weather_URL, Weather_Data};
		String[] commands = {runbat};
		Process proc = rt.exec(commands);
		proc.waitFor();  
		*/
		Double inputTemp =303.15;
		Double inputPres = 200000.0;
		
		
	if (compound=="chlorine")

	{
		if(inputTemp>=273 && inputTemp<=343.15 && inputPres>=101325 && inputPres<=1200000)
		{tank="TankID_1574";
	compound2="Chlorine";
		}
		else
		{System.out.println("the condition is not applicable");
				}
	}
	
	else if (compound=="ammonia")
	{
		if(inputTemp>=300 && inputTemp<=327.594 && inputPres>=101325 && inputPres<=1800000)
		{tank="TankID_1354";
	compound2="Ammonia";
	}
		else
		{System.out.println("the condition is not applicable");
				}
	}
	else if(compound =="Isobutylene")
	{
		if(inputTemp>=266.25 && inputTemp<=325.15 && inputPres>=101325 && inputPres<=1800000)
		{
		tank="TankID_1329";
	compound2="Isobutylene";
	}
		else
		{System.out.println("the condition is not applicable");
				}
	}
	else if(compound =="MethylChloride")
	{ 
		if(inputTemp>=248.95 && inputTemp<=325.15 && inputPres>=101325 && inputPres<=1800000)
		{
		tank="TankID_1328";
	compound2="MethylChloride";
	}
		else
		{System.out.println("the condition is not applicable");
				}
	}
		
		ProcessBuilder pb = new ProcessBuilder("cmd", "/c", "doRequest.bat");
		File dir = new File(baseURL);
		pb.directory(dir);
		Process p = pb.start();
		

		
		/** load your knowledge base from an owl file; additionally */
		String filePath = baseURL + tank+".owl";
		FileInputStream inFile= new FileInputStream(filePath);
		Reader in = new InputStreamReader(inFile,"UTF-8");
		JenaOWLModel jenaOwlModel = ProtegeOWL.createJenaOWLModelFromReader(in);		   


		
		OWLIndividual temperature = jenaOwlModel.getOWLIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+tank+".owl#V_Temperature_"+compound2+"In"+tank);
		OWLIndividual pressure = jenaOwlModel.getOWLIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+tank+".owl#V_Pressure_"+compound2+"In"+tank);
		OWLIndividual diameter = jenaOwlModel.getOWLIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+tank+".owl#V_Diameter_"+tank+"_LeakHole1");
		OWLIndividual extp = jenaOwlModel.getOWLIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+tank+".owl#V_ambientPressure");
		
		OWLIndividual material = jenaOwlModel.getOWLIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+tank+".owl#ChemSpecies_1");
		OWLObjectProperty contains = jenaOwlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#containsDirectly");  
		
		
		OWLIndividual substance = jenaOwlModel.getOWLIndividual("http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#"+compound);  
		material.setPropertyValue(contains, substance);
		
		OWLIndividual gamma = jenaOwlModel.getOWLIndividual("http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#V_HeatCapRat_"+compound);
		OWLIndividual cp = jenaOwlModel.getOWLIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+tank+".owl#V_Cp_"+compound2+"In"+tank);
			OWLIndividual m = jenaOwlModel.getOWLIndividual("http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#ValueOfMolecularWeightOf"+compound);
		  
		OWLDatatypeProperty property = jenaOwlModel.getOWLDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
		
		//input value modified  
		temperature.setPropertyValue(property,new Float (inputTemp));
		 pressure.setPropertyValue(property, new Float(inputPres));
		 diameter.setPropertyValue(property, new Float(0.01));
		 
		 
		 	
		
		FileWriter fW_PV_P = null; // create an object later used for writing a .csv file for the parameterised PW model, using P values from powergen layer.
		// create an object later used for writing a .csv file for the parameterised PW model, using Q values from powergen layer.

	    
	    	    
	        pressvalue = pressure.getPropertyValueLiteral(jenaOwlModel.getOWLProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue")).getString();
	        diametervalue = diameter.getPropertyValueLiteral(jenaOwlModel.getOWLProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue")).getString();
	        tempvalue = temperature.getPropertyValueLiteral(jenaOwlModel.getOWLProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue")).getString();
	       Pamb = extp.getPropertyValueLiteral(jenaOwlModel.getOWLProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue")).getString();
	        
	        mvalue = m.getPropertyValueLiteral(jenaOwlModel.getOWLProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue")).getString();
	      
	        
	       Double CpCl= (8.28+0.00056*Float.parseFloat(tempvalue))*4184/Float.parseFloat(mvalue); //in joule/kg.K
	       Double CpNH3= (6.7+0.0063*Float.parseFloat(tempvalue))*4184/Float.parseFloat(mvalue); //in joule/kg.K
	       Double CpCH3Cl =(34090+72460*Math.pow((1723/Float.parseFloat(tempvalue))/Math.sinh(1723/Float.parseFloat(tempvalue)), 2)+44800*Math.pow((780.5/Float.parseFloat(tempvalue))/Math.cosh(780.5/Float.parseFloat(tempvalue)), 2))/Float.parseFloat(mvalue); //in J/kg.K
	       Double CpIsobut= (24970+211.8*Float.parseFloat(tempvalue)-5.7357*10E-5*Math.pow(Float.parseFloat(tempvalue), 3)+0.0014526*10E-10*Math.pow(Float.parseFloat(tempvalue), 4))/Float.parseFloat(mvalue); //in J/kg.K
	        
	       Double gammacalcCL = (8.28+0.00056*Float.parseFloat(tempvalue))/(8.28+0.00056*Float.parseFloat(tempvalue)-1.987); 
	        Double gammacalcNH3 = (6.7+0.0063*Float.parseFloat(tempvalue))/(6.7+0.0063*Float.parseFloat(tempvalue)-1.987);
	        Double gammacalcCH3Cl = CpCH3Cl*Float.parseFloat(mvalue)/(CpCH3Cl*Float.parseFloat(mvalue)-8314.4598);
	        Double gammacalcIsobut = CpIsobut*Float.parseFloat(mvalue)/(CpIsobut*Float.parseFloat(mvalue)-8314.4598);
	        
	        if(compound=="chlorine")
	        {
	        gamma.setPropertyValue(property, new Float(gammacalcCL));
	        cp.setPropertyValue(property, new Float(CpCl));
	        System.out.println ("compound= chlorine");
	        System.out.println("cp of chlorine=" +CpCl);
	        }
	        else if (compound=="ammonia")
	        {
	        	cp.setPropertyValue(property, new Float(CpNH3));
		        gamma.setPropertyValue(property, new Float(gammacalcNH3));
		        System.out.println ("compound= ammonia");
		        
		        }
	        else if (compound=="MethylChloride")
	        {   cp.setPropertyValue(property, new Float(CpCH3Cl));
		        gamma.setPropertyValue(property, new Float(gammacalcCH3Cl));
		        System.out.println ("compound= MethylChloride");
		        }
	        else if (compound=="Isobutylene")
	        {   cp.setPropertyValue(property, new Float(CpIsobut));
		        gamma.setPropertyValue(property, new Float(gammacalcIsobut));
		        System.out.println ("compound= Isobutylene");
		        }
	        
	        
	        gammavalue = gamma.getPropertyValueLiteral(jenaOwlModel.getOWLProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue")).getString();
	        
	        try {
			
			fW_PV_P = new FileWriter(PVPIN);
			

			
			fW_PV_P.append("No, Diameter, Pressure, Temperature");
			fW_PV_P.append("\n");

			
			
				//for loadpoint  writing
			 // iterate over all members of the Load_Point attributeslist (total of 108), which contains information on multiple graphic elements
				fW_PV_P.append("1."); // add the header "FID" to the data stream of the file that will contain the pwr_P value.
				fW_PV_P.append(",");
				fW_PV_P.append(diametervalue); // capture the FID value, convert it to a string and add it to the data stream
				fW_PV_P.append(", ");
				fW_PV_P.append(pressvalue); // capture the pwr_P value that corresponds to the FID, convert it to a string and add it to the data stream
				fW_PV_P.append(", ");
				fW_PV_P.append(tempvalue);
				fW_PV_P.append("\n");
	
				

			
			
			fW_PV_P.flush(); // passes the data from fW_PG to PGIN.csv
						
			fW_PV_P.close();
			
		} catch (Exception e) {
			e.printStackTrace();
		}
		System.out.println("value of D ="+ diametervalue);
		System.out.println("value of Po ="+ pressvalue);
		System.out.println("value of T ="+ tempvalue);

		System.out.println("value of Mw ="+ mvalue);
		System.out.println("value of gamma ="+ gammavalue);
		   
		/**save the updated model file*/
		Collection errors = new ArrayList();
		jenaOwlModel.save(new URI("file:////" + filePath.replace("\\", "/")), FileUtils.langXMLAbbrev, errors, jenaOwlModel.getOntModel());
		System.out.println("File saved with " + errors.size() + " errors.");  
		     
	}


public static void Running_Model() throws FileNotFoundException, UnsupportedEncodingException, OntologyLoadException, URISyntaxException {
	
	String filePath = baseURL + tank+".owl";
	FileInputStream inFile= new FileInputStream(filePath);
	Reader in = new InputStreamReader(inFile,"UTF-8");
	JenaOWLModel jenaOwlModel = ProtegeOWL.createJenaOWLModelFromReader(in);	
	
	Double pchoke= Float.parseFloat(pressvalue)*Math.pow(2/(Float.parseFloat(gammavalue)+1), Float.parseFloat(gammavalue)/(Float.parseFloat(gammavalue)-1));
	
Double choked=1*0.25*3.14*(Float.parseFloat(diametervalue)*Float.parseFloat(diametervalue))*Float.parseFloat(pressvalue)*Math.pow(1*Float.parseFloat(mvalue)/1000*Float.parseFloat(gammavalue)/(8.314*Float.parseFloat(tempvalue))*Math.pow(2/(Float.parseFloat(gammavalue)+1),((Float.parseFloat(gammavalue)-1)*(Float.parseFloat(gammavalue)+1))),0.5);
	
	Double nonchoke=1*0.25*3.14*(Float.parseFloat(diametervalue)*Float.parseFloat(diametervalue))*Float.parseFloat(pressvalue)*Math.pow(2*1*Float.parseFloat(mvalue)/1000*Float.parseFloat(gammavalue)/(8.314*Float.parseFloat(tempvalue)*(Float.parseFloat(gammavalue)-1))*(Math.pow(Float.parseFloat(Pamb)/Float.parseFloat(pressvalue),2/Float.parseFloat(gammavalue))-Math.pow(Float.parseFloat(Pamb)/Float.parseFloat(pressvalue),(Float.parseFloat(gammavalue)+1)/Float.parseFloat(gammavalue))),0.5);
	
	System.out.println("Pchoked= "+pchoke);
	
	if (pchoke<Float.parseFloat(Pamb))
	{
		 flowrate=nonchoke;
		 System.out.println("it's non choked");
		 
		 
	}
	else
	{
		flowrate=choked;
		System.out.println("it's choked");
	}
	
	System.out.println("flow output= "+flowrate);

	OWLIndividual massf = jenaOwlModel.getOWLIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+tank+".owl#V_massF_LeakageStream_001");
	
	 OWLDatatypeProperty property = jenaOwlModel.getOWLDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
	
	 massf.setPropertyValue(property, new Float(flowrate));
	 
	 /**save the updated model file*/
		Collection errors = new ArrayList();
		jenaOwlModel.save(new URI("file:////" + filePath.replace("\\", "/")), FileUtils.langXMLAbbrev, errors, jenaOwlModel.getOntModel());
		System.out.println("File saved with " + errors.size() + " errors.");  
}




public static void main(String[] args) throws Exception {
	System.out.println("Starting Process");
	Downloading();
    Running_Model();
	 
}
	
	
}
