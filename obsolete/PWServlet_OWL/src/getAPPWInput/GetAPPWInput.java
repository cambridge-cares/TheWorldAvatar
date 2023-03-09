package getAPPWInput;

import java.util.ArrayList;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.esri.core.io.UserCredentials;
import com.esri.core.map.Feature;
import com.esri.core.map.FeatureResult;
import com.esri.core.tasks.query.QueryParameters;
import com.esri.core.tasks.query.QueryTask;

import edu.stanford.smi.protege.exception.OntologyLoadException;
import edu.stanford.smi.protegex.owl.ProtegeOWL;
import edu.stanford.smi.protegex.owl.model.OWLIndividual;
import edu.stanford.smi.protegex.owl.model.OWLModel;

public class GetAPPWInput {
	
	public ArrayList<Double> getAPPWInput(ArrayList<String[]> editStack) {
		ArrayList<Map<String, Object>> attributeslist_MX = new ArrayList<Map<String, Object>>(); // additional ArrayList for mixer
		ArrayList<Map<String, Object>> attributeslist_HX = new ArrayList<Map<String, Object>>(); // additional ArrayList for heat exchanger
		UserCredentials user = new UserCredentials();
		user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");	
		
		String layer = null, OBJECTID = null, appCallFlag = null;
		layer = editStack.get(0)[0];
		OBJECTID = editStack.get(0)[1];
		appCallFlag = editStack.get(0)[2];
		System.out.println("layer="+layer);
		System.out.println("OBJECTID="+OBJECTID);
		
		ArrayList<Double> xRow = new ArrayList<Double>();
		
		String uri = "File:/C:/apache-tomcat-8.0.24/webapps/ROOT/BiodieselPlant3.owl";
		
		try{			
            OWLModel owlModel = ProtegeOWL.createJenaOWLModelFromURI(uri);                  
            
            if(layer.equals("Heater_cooler")||layer.equals("Mixer")){
            	
		/**Check whether the oil input flowrate/temperature was changed, 
		 * if yes, take the value from ArcGIS database, 
		 * if no, take the value from owl file */
		if(layer.equals("Heater_cooler")||OBJECTIDtoHXNum.get(Integer.parseInt(OBJECTID)).equals("10E01B3")){
			System.out.println(OBJECTIDtoHXNum.get(Integer.parseInt(OBJECTID))+"o_O");
			
			for (Integer key : OBJECTIDtoHXB3.keySet()) {
				try {
					QueryParameters qParameter_HX = new QueryParameters();                       // create an instance  of QueryParameters to be used  for querying  ArcGIS database for predefined data
					qParameter_HX.setWhere("OBJECTID='" + key + "'");                            // define FID address of an ArcGIS element
					qParameter_HX.setOutFields(new String[] { "*" });                            // fetch all  attributes of an ArcGIS element using *
					QueryTask qTask_HX = null;                                                   // create an instance of QueryTask to store URL address of appropriate database and user credentials necessary for accessing it
					Feature graphic_HX = null;                                                   // create an instance of Feature to store an ArcGIS element

					qTask_HX = new QueryTask( "http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/heater_cooler/FeatureServer/0", user); // store URL address of appropriate databaseand user credentials
					FeatureResult fResult_HX = qTask_HX.execute(qParameter_HX);                   // FeatureResult is used to store information from ArcGIS database requested using qParameter_LP  and qTask_LP
					graphic_HX = (Feature) fResult_HX.iterator().next();                          // queryResult.iterator() iterates over the elements in fResult_LP and stores it in graphic_LP; qParameter_LP requests information about a single element only
					attributeslist_HX.add(graphic_HX.getAttributes());                            // append information about the  element in graphic_LP to ArrayList attributeslist_LP

				} catch (Exception e) {
					e.printStackTrace();                                                            // It prints the stack trace of the Exception to System.err. It's a very simple, but very useful tool for diagnosing an Exception. It tells you what happened and where in the code this happened.
				}
			}
			
			for (int i = 0; i < attributeslist_HX.size(); i++) {
				for (String key : attributeslist_HX.get(i).keySet()) { // go through  all the  heat exchangers in biodiesel plant
					if (key == "OBJECTID") {
//50-56
						if (OBJECTIDtoHXB3.get(i + 50).equals("10E01B3")) { // "10E01" is the heat exchanger for oil to be heated before feeding to the reactor							
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_HX.get(i).get("MatIn1Qnt")))); // add the feeding mole flowrate of oil to xRow
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_HX.get(i).get("MatIn1_T")))); // add the temperature of oil to xRow
						}
					}
				}
			}
		
		}else{
			OWLIndividual x1 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#F_OIL_V");                       
            String x1value = x1.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();             
            xRow.add(Double.valueOf(x1value)/807.3);              //molar flowrate	
     
            
            OWLIndividual x2 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#T_OIL_V");                
            String x2value = x2.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();               
            xRow.add(Double.valueOf(x2value));
		}
		/**Check whether the input flowrate/temperature of methanol was modified, 
		 * if yes, take the value from ArcGIS database, 
		 * if no, take the value from owl file*/
		if(layer.equals("Mixer")||OBJECTIDtoMXNum.get(Integer.parseInt(OBJECTID)).equals("MX01B3")){
			System.out.println(OBJECTIDtoMXNum.get(Integer.parseInt(OBJECTID))+"o_O");
			
			for (int key : OBJECTIDtoMXB3.keySet()) {
				try {
					QueryParameters qParameter_MX = new QueryParameters(); // create an instance of QueryParameters to be used for querying ArcGIS database for predefined data
					qParameter_MX.setWhere("OBJECTID='" + key + "'"); // define FID address of an ArcGIS element
					qParameter_MX.setOutFields(new String[] { "*" }); // fetch all attributes of an ArcGIS element using *
					QueryTask qTask_MX = null; // create an instance of QueryTask to store URL address of appropriate database and user credentials necessary for accessing it
					Feature graphic_MX = null; // create an instance of Feature to store an ArcGIS element

					qTask_MX = new QueryTask("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Mixer/FeatureServer/0",user); // store URL address of appropriate databaseand user credentials

					FeatureResult fResult_MX = qTask_MX.execute(qParameter_MX); // FeatureResult is  used to store information from ArcGIS database  requested using qParameter_LP and qTask_LP
					graphic_MX = (Feature) fResult_MX.iterator().next(); // queryResult.iterator() iterates over the elements in fResult_LP and stores it in graphic_LP; qParameter_LP requests information about a single element only
					attributeslist_MX.add(graphic_MX.getAttributes()); // append information about the element in graphic_LP to ArrayList attributeslist_LP

				} catch (Exception e) {
					e.printStackTrace(); // It prints the stack trace of the Exception to System.err. It's a very simple, but very useful tool for diagnosing an Exception. It tells you  what happened and where in the code this happened.
				}
			}
			for (int i = 0; i < attributeslist_MX.size(); i++) {
				for (String key : attributeslist_MX.get(i).keySet()) { // go through all  the mixers in biodiesel plant
					if (key == "OBJECTID") {
//19-21
						if (OBJECTIDtoMXB3.get(i + 19).equals("MX01B3")) { // "mx01" is the mixer for methanol and the catalyst to be mixed before feeding to the reactor
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_MX.get(i).get("MatIn2Qnt")))); // add the feeding mole flowrate of methanol  to xRow
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_MX.get(i).get("MatIn2_T")))); // add the temperature of the feeding methanol flow to xRow							
						}
					}
				}
			}			
		}else{
			OWLIndividual x3 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#F_MEOH_V");           
            String x3value = x3.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
            xRow.add(Double.valueOf(x3value)/32.04);           // molar flowrate
                        
            OWLIndividual x4 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#T_MEOH_V");           
            String x4value = x4.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
            xRow.add(Double.valueOf(x4value));
		}
		
		/**Check whether the input flowrate water and operational pressure of the reboiler was modified, 
		 * if yes, take the value from ArcGIS database, 
		 * if no, take the value from owl file*/
		if(layer.equals("Heater_cooler")||OBJECTIDtoMXNum.get(Integer.parseInt(OBJECTID)).equals("BoilerB3")){
			for (int key : OBJECTIDtoMXB3.keySet()) {
				try {
					QueryParameters qParameter_MX = new QueryParameters(); // create an instance of QueryParameters to be used for querying ArcGIS database for predefined data
					qParameter_MX.setWhere("OBJECTID='" + key + "'"); // define FID address of an ArcGIS element
					qParameter_MX.setOutFields(new String[] { "*" }); // fetch all attributes of an ArcGIS element using *
					QueryTask qTask_MX = null; // create an instance of QueryTask to store URL address of appropriate database and user credentials necessary for accessing it
					Feature graphic_MX = null; // create an instance of Feature to store an ArcGIS element

					qTask_MX = new QueryTask("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Mixer/FeatureServer/0",user); // store URL address of appropriate databaseand user credentials

					FeatureResult fResult_MX = qTask_MX.execute(qParameter_MX); // FeatureResult is  used to store information from ArcGIS database  requested using qParameter_LP and qTask_LP
					graphic_MX = (Feature) fResult_MX.iterator().next(); // queryResult.iterator() iterates over the elements in fResult_LP and stores it in graphic_LP; qParameter_LP requests information about a single element only
					attributeslist_MX.add(graphic_MX.getAttributes()); // append information about the element in graphic_LP to ArrayList attributeslist_LP

				} catch (Exception e) {
					e.printStackTrace(); // It prints the stack trace of the Exception to System.err. It's a very simple, but very useful tool for diagnosing an Exception. It tells you  what happened and where in the code this happened.
				}
			}
			for (int i = 0; i < attributeslist_HX.size(); i++) {
				for (String key : attributeslist_HX.get(i).keySet()) { // go through all the heat exchanger in biodiesel plant
					if (key == "OBJECTID") {
						if (OBJECTIDtoHXB3.get(i + 50).equals("BoilerB3")) {
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_HX.get(i).get("MatOut6Qnt")))); // add the temperature of the outlet cold stream  to xRow
							xRow.add(Double.parseDouble(String.valueOf(attributeslist_HX.get(i).get("Operate_P")))); // add the temperature of the outlet cold stream  to xRow
						}
					}
				}
			}	
		}else{
			OWLIndividual x5 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#F_RE-WATER_V");           
            String x5value = x5.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
            xRow.add(Double.valueOf(x5value)/18.01527);
            
            OWLIndividual x6 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#P_Boiling_V");           
            String x6value = x6.getPropertyValueLiteral(owlModel.getOWLProperty("system:numericalValue")).getString();            
            xRow.add(Double.valueOf(x6value)/100);
		}
       }
		System.out.println(xRow);
		}catch (OntologyLoadException ex) {
	           Logger.getLogger(PWServlet.class.getName()).log(Level.SEVERE, null, ex); 
	           System.out.println(ex);
	        }	
	}

}
