package aPServlet;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CountDownLatch;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.esri.core.geodatabase.GeodatabaseFeatureServiceTable;
import com.esri.core.io.EsriSecurityException;
import com.esri.core.io.UserCredentials;
import com.esri.core.map.CallbackListener;
import com.esri.core.map.Feature;
import com.esri.core.map.FeatureResult;
import com.esri.core.tasks.query.QueryParameters;
import com.esri.core.tasks.query.QueryTask;

public class APServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	public static Map<String, String> ArcGISFIDtoPWBusNum = new HashMap<>(); // Maps ArcGIS FID (key) to BusNum (value) in PowerWorld
	public static Map<String, String> PWBusNumtoArcGISFID = new HashMap<>(); // reverse mapping BusNum to ArcGIS FID
	public static Map<String, String> SubstationtoPWBusNum = new HashMap<>(); // Maps substation number to HV and LV bus numbers
	public static Map<String, String> PWBusNumtoSubstation = new HashMap<>(); // reverse mapping
 	public static String INCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/IN.CSV");
 	public static String testCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/test.CSV");
	public static String BUSCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/BUS.CSV");
	public static String runPythonCommand = new String("python C:/apache-tomcat-8.0.24/webapps/ROOT/PWrun.pyw"); // ensure that python environment variable is set to python34
 	public static String httpReqCSV = new String("C:/apache-tomcat-8.0.24/webapps/APtest/httpReq.CSV"); // (mjk, 151115) differentiating function calls "Run PowerWorld" and "Run parameterised PW"
 	public static String httpReqCSVAPS = new String("C:/apache-tomcat-8.0.24/webapps/APtest/httpReqAPS.CSV"); 
 	public static String flag2CSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/flag2.CSV"); // (mjk, 151115) to see how far runPowerWorld() is being executed

 	public static Map<String, String> APSimNamtoFID = new HashMap<>(); // ZL-151125 Maps 
 	public static String APINCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/APIN.CSV"); //ZL-151124
 	public static String APOUTCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/APOUT.CSV"); //ZL-151124
 	public static String runPythonCommandAP = new String("python C:/apache-tomcat-8.0.24/webapps/ROOT/APrun.pyw"); //ZL-151124
 	
    public APServlet() {
        super();
		// Hard Coded ArcGISFIDtoPWBusNum
		ArcGISFIDtoPWBusNum.put("61","80"); //Air Liquide
		
		for (Map.Entry<String, String> entry : ArcGISFIDtoPWBusNum.entrySet()) { // reverse mapping
			PWBusNumtoArcGISFID.put(entry.getValue(), entry.getKey());
		}
		
		// first bus number is high voltage bus, second bus number is low voltage bus
		SubstationtoPWBusNum.put("UHT1HV", "2");
		
		for (Map.Entry<String, String> entry : SubstationtoPWBusNum.entrySet()) {
			PWBusNumtoSubstation.put(entry.getValue(), entry.getKey().substring(3, 4)); // remove all characters from key, only need FID
		}
		
		//
		APSimNamtoFID.put("Jbiod", "1"); //ZL-151127		

    }
 
        protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {        
		ArrayList<String[]> editStack = new ArrayList<String[]>(); // reconstruct editStack from query string received
		String[] QueryTask = request.getParameter("QueryTask").split(",");
//		String[] FIDs = request.getParameter("OBJECTID").split(",");
//		String[] appCallFlag = request.getParameter("appCallFlag").split(","); // (mjk, 151115) adding flag indicating which function has been called: PowerWorld, parameterised PW, AspenPlus, parameterised AP		
		for (int i=0; i<QueryTask.length; i++) {
//			editStack.add(new String[] {layers[i], FIDs[i]});
			editStack.add(new String[] {QueryTask[i]}); // Here, "editStack" for only one layer modification looks like this: [Load_Points,103,PW]
		}

		FileWriter flag1 = null; // (mjk, 151115) testing structure of DataOutputStream object and of wr object
		flag1 = new FileWriter(httpReqCSVAPS);
		flag1.append("QueryTask=" + QueryTask[0]);
//		flag1.append(", FIDs=" + FIDs[0]);
//		flag1.append(", appCallFlag=" + appCallFlag[0]);
		flag1.flush();
		flag1.close(); // (mjk, 151115) writing this file works fine.
		
//		if(appCallFlag[0]=="PW") {

//		runAspenPlus(editStack);  //ZL-151126
		
	} // of doPost()
	
	// allows manual updating using a browser
	// e.g. entering http://localhost:8080/PWServlet/?layers=Load_Points&FIDs=103
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		ArrayList<String[]> editStack = new ArrayList<String[]>();		
		String[] layers = request.getParameter("layers").split(",");
		String[] FIDs = request.getParameter("FIDs").split(",");
		for (int i=0; i<layers.length; i++) {
			editStack.add(new String[] {layers[i], FIDs[i]});	
		}
		runAspenPlus(editStack);  //ZL-151126
	}
 
    	public void runAspenPlus(ArrayList<String[]> editStack){
		ArrayList<String[]> skeleton = new ArrayList<String[]>(); 
		ArrayList<Map<String, Object>> attributeslist = new ArrayList<Map<String, Object>>(); 		
		ArrayList<String> layers = new ArrayList<String>();
	    UserCredentials user = new UserCredentials();
	    user.setUserAccount("jparksimulator", "c4tjpark"); 
	    
	    for (int i=0; i<editStack.size(); i++){
	    	String layer = (String) editStack.get(i)[0];	
			String graphicFID = (String) editStack.get(i)[1];
			
			QueryParameters query = new QueryParameters();
			query.setWhere("FID='" + graphicFID + "'");			// find graphic using FID
			query.setOutFields(new String[] {"*"});				// fetch all attributes using *

			QueryTask task = null;
			Feature graphic = null;
			if(layer.equals("ChemProcess")) {
				// variable names specific to chem_process
				skeleton.add(new String[]{"OIL,MEOH","FOIL,FMEOH"}); //???
				try {
					task = new QueryTask("http://services6.arcgis.com/MXY8H7lIySnKUlD3/arcgis/rest/services/MyMapService/FeatureServer/0", user);
				} catch (EsriSecurityException e) {
					e.printStackTrace();
				}
				
				try {
					FeatureResult fResult = task.execute(query);		// extracts data from the "task" object and writes them to fResult.
					graphic = (Feature) fResult.iterator().next();		// extracts data from fResult (using typecasting) and writes them to graphic. fResult.iterator() should only return one feature
				} catch (Exception e) {
					e.printStackTrace();
				}
				attributeslist.add(graphic.getAttributes());
				layers.add(layer);	
				if (!skeleton.isEmpty()) {
					writeAPCSV(skeleton, attributeslist, layers); 
					runPyScript(editStack);
		 			readAPCSV();
				}
			}
	    }
	}
	
    public void runPyScript(ArrayList<String[]> editStack) {
//		String[] appCallFlag = null;
//		appCallFlag[0] = (String) editStack.get(0)[2];
		try {
//			if(appCallFlag[0]=="AP") {
				Process p = Runtime.getRuntime().exec(runPythonCommandAP);
				p.waitFor();
				System.out.println("Exit Value (0 means success): " + p.exitValue()); // if console prints 0 it means success
				BufferedReader br = new BufferedReader(new InputStreamReader(p.getInputStream()));
				String line; // retrieves console from python script
				System.out.println("Python input:");
				while ((line=br.readLine())!=null) {
					System.out.println(line); // print input array from Python (see python code for more details)
				}
				line = br.readLine();
//			} //ZL-151124						
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

 	public void writeAPCSV(ArrayList<String[]> skeleton, ArrayList<Map<String, Object>> attributeslist, ArrayList<String> layers){  
  	        FileWriter flieWriter = null;
  	        
  	        try{
  	        flieWriter = new FileWriter(APINCSV);
  	        for(int i=0; i<skeleton.size(); i++){
  	         flieWriter.append(skeleton.get(i)[0]); //write headers-Aspen Plus input names
  	         flieWriter.append("\n");  //New line
  	         String[] ArcGISfields = skeleton.get(i)[1].split(",");
  	         Map<String,Object> attributes = attributeslist.get(i); //pulls all date fields available from ArcGIS
  	         
			 for(int j=0; j<ArcGISfields.length; j++) {
			    if(ArcGISfields[j].equals("FOIL")) {
				String ArcGISOILF =String.valueOf(attributes.get("Matln1Qnt"));
				flieWriter.append(ArcGISOILF);
				}
				else if(ArcGISfields[j].equals("FMEOH")){
				String ArcGISMEOHF =String.valueOf(attributes.get("Matln2Qnt"));
				flieWriter.append(ArcGISMEOHF);
				}			  	  	         
  	         } 	         
//  	         String layer = layers.get(i);
//  	         if(layer.equals("ChemProcess")){
        
//  	         }
  	        }
  	       } catch (Exception e) {
  				e.printStackTrace();
  			} finally {
  				try {
  					flieWriter.flush();
  					flieWriter.close();
  				} catch (IOException e) {
  					e.printStackTrace();
  				}
  			}
  	}
	
	public void readAPCSV(){
	  BufferedReader fileReader =null;
	  UserCredentials user = new UserCredentials();
	  user.setUserAccount("jparksimulator", "c4tjpark");
	  
	  try{
	     long start =System.currentTimeMillis(); //start a timer
		 String line =null;
		 fileReader =new BufferedReader(new FileReader(APOUTCSV));
		 fileReader.readLine(); //Read the CSV flie header to skip it
		 QueryParameters loadAllFeatures =new QueryParameters();
		 loadAllFeatures.setWhere("FID IS NOT NULL");
		 GeodatabaseFeatureServiceTable ChemProcessTable = new GeodatabaseFeatureServiceTable("http://services6.arcgis.com/MXY8H7lIySnKUlD3/arcgis/rest/services/MyMapService/FeatureServer", user, 0);
		 ChemProcessTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
		 ChemProcessTable.initialize();
		 
		 while((line=fileReader.readLine())!=null){
		    String[] data =line.split(",");
			//data[0]=SimName
			//data[1]=BIOD_FLOW
		    //data[3]=GLYC_FLOW
			
//			String SimName =String.valueOf(data[0].trim()); //data[0] is the simulation name
			String ArcGISFID =APSimNamtoFID.get(data[0].trim()); //trim() removes trailing white spaces
			
			if(ArcGISFID !=null){
			  Map<String,Object> ChemProcessAttributes =ChemProcessTable.getFeature(Long.parseLong(ArcGISFID)).getAttributes();
			  if(!data[1].trim().isEmpty()) {
			    ChemProcessAttributes.put("MatOut3Qnt",Float.parseFloat(data[1].trim()));
			  }
			  if(!data[3].trim().isEmpty()) {
			    ChemProcessAttributes.put("MatOut4Qnt",Float.parseFloat(data[3].trim()));
				  }
			  ChemProcessTable.updateFeature(Long.parseLong(ArcGISFID),ChemProcessAttributes); //update feature table locally
			}
			
		 }
		 ChemProcessTable.applyEdits(null); // commit local updates onto server	    
		 System.out.println("Updating process took " + String.valueOf(System.currentTimeMillis()-start) + "ms");  //tells how long it took to update
	  }catch (Exception e) {
			e.printStackTrace();
		} finally {
			try {
				fileReader.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}	  
	}

}
