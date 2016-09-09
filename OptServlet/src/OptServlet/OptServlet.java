/** This is the servlet for the optimization function in J-Park Simulator --Li ZHOU 25.08.2016*/
package OptServlet;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.gams.api.GAMSDatabase;
import com.gams.api.GAMSJob;
import com.gams.api.GAMSOptions;
import com.gams.api.GAMSParameter;
import com.gams.api.GAMSWorkspace;
import com.gams.api.GAMSWorkspaceInfo;

import edu.stanford.smi.protege.exception.OntologyLoadException;
import edu.stanford.smi.protegex.owl.ProtegeOWL;
import edu.stanford.smi.protegex.owl.model.OWLIndividual;
import edu.stanford.smi.protegex.owl.model.OWLModel;


public class OptServlet extends HttpServlet {

	/**
	 * author Li ZHOU
	 */
	private static final long serialVersionUID = 1L;
	public static String httpReqCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/httpReq.CSV"); 
	
    public OptServlet(){
		
	}
    
    /**
	 * @param args
	 */
    
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		/**reconstructing editStack from request string so that the information can be passed on to the relevant methods*/
		String[] OptT = request.getParameter("OptT").split(",");
		String[] Flag = request.getParameter("Flag").split(",");
		String[] P1 = request.getParameter("Parameter1").split(",");
		String[] P2 = request.getParameter("Parameter2").split(",");
		
								
		/**check whether the httpRequst has been correctly received */
		FileWriter flag = null;                                 // testing structure of DataOutputStream object and of wr object
		flag = new FileWriter(httpReqCSV);
		flag.append("OptT=" + OptT[0]);
		flag.append(", Flag=" + Flag[0]);
		flag.flush();
		flag.close(); 
 				
		switch (Flag[0]) {
		case("Load"):
			System.out.println("start " + OptT[0] + "ing...");
	    
	        final String ModelInformation = LoadModelInformation().toString();	
	        response.setContentLength(ModelInformation.length());
	        response.getOutputStream().write(ModelInformation.getBytes());		    
	        response.getOutputStream().flush();
	        response.getOutputStream().close();
	        System.out.println("Successful loading!");
	        break;
	        
		case("Optimize"):
			System.out.println("start optimizing...");
			
			final String OptResult = GAMSOptimization(P1[0], P2[0]).toString();
		    response.setContentLength(OptResult.length());
            response.getOutputStream().write(OptResult.getBytes());		    
            response.getOutputStream().flush();
            response.getOutputStream().close();
            System.out.println("Successful Optmizing!");
            break;
		
		}

	
  }
    
    

	public ArrayList<String> LoadModelInformation () {
    	ArrayList<String> ModelInformation =  new ArrayList<String>();
    	
    	try {
			String uri = "File:/C:/apache-tomcat-8.0.24/webapps/ROOT/BiodieselPlant_.owl";
            OWLModel owlModel = ProtegeOWL.createJenaOWLModelFromURI(uri);
            
            /**associating P1 to a JAVA OWLIndividual variable*/
            OWLIndividual P1 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOfPriceOfBiodiesel");
            String P1name = ", PriceOfBiodiesel";
            ModelInformation.add(P1name);
            String[] P1unit = P1.getPropertyValue(owlModel.getOWLProperty("system:hasUnitOfMeasure")).toString().split("#");
            ModelInformation.add(P1unit[1].split(" ")[0]);
            
            /**associating P2 to a JAVA OWLIndividual variable*/
            OWLIndividual P2 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOfPriceOfUtility");
            String P2name = "PriceOfUtility";
            ModelInformation.add(P2name);
            String[] P2unit = P2.getPropertyValue(owlModel.getOWLProperty("system:hasUnitOfMeasure")).toString().split("#");
            ModelInformation.add(P2unit[1].split(" ")[0]);
            ModelInformation.add(","); 
            
            /**associating x1 to a JAVA OWLIndividual variable*/
            OWLIndividual x1 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOfOIL_Mole_Flow");
            String x1name = "OIL_Mole_Flow";
            ModelInformation.add(x1name);            
            String[] x1unit = x1.getPropertyValue(owlModel.getOWLProperty("system:hasUnitOfMeasure")).toString().split("#");
            ModelInformation.add(x1unit[1].split(" ")[0]);	
            
            /**associating x2 to a JAVA OWLIndividual variable*/
            OWLIndividual x2 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOfOIL_Temperature");
            String x2name = "OIL_Temperature";
            ModelInformation.add(x2name);           
            String[] x2unit = x2.getPropertyValue(owlModel.getOWLProperty("system:hasUnitOfMeasure")).toString().split("#");
            ModelInformation.add(x2unit[1].split(" ")[0]);	
            
            /**associating x3 to a JAVA OWLIndividual variable*/
            OWLIndividual x3 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOfMEOH_Mole_Flow");
            String x3name = "MEOH_Mole_Flow";
            ModelInformation.add(x3name);             
            String[] x3unit = x3.getPropertyValue(owlModel.getOWLProperty("system:hasUnitOfMeasure")).toString().split("#");
            ModelInformation.add(x3unit[1].split(" ")[0]);	
            
            /**associating x4 to a JAVA OWLIndividual variable*/
            OWLIndividual x4 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOfMEOH_Temperature");
            String x4name = "MEOH_Temperature";
            ModelInformation.add(x4name);             
            String[] x4unit = x4.getPropertyValue(owlModel.getOWLProperty("system:hasUnitOfMeasure")).toString().split("#");
            ModelInformation.add(x4unit[1].split(" ")[0]);	
            
            /**associating x5 to a JAVA OWLIndividual variable*/
            OWLIndividual x5 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOfREWATER_Mole_Flow");
            String x5name = "REWATER_Mole_Flow";
            ModelInformation.add(x5name);            
            String[] x5unit = x5.getPropertyValue(owlModel.getOWLProperty("system:hasUnitOfMeasure")).toString().split("#");
            ModelInformation.add(x5unit[1].split(" ")[0]);	
            
            /**associating x6 to a JAVA OWLIndividual variable*/
            OWLIndividual x6 = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#ValueOfBoiler_Pressure");
            String x6name = "Boiler_Pressure";
            ModelInformation.add(x6name);             
            String[] x6unit = x6.getPropertyValue(owlModel.getOWLProperty("system:hasUnitOfMeasure")).toString().split("#");
            ModelInformation.add(x6unit[1].split(" ")[0]);	 
            ModelInformation.add(",");
			
		} catch (OntologyLoadException ex) {
            Logger.getLogger(OptServlet.class.getName()).log(Level.SEVERE, null, ex);
        }
    	System.out.println("ModelInformation = " + ModelInformation);
		return ModelInformation;    	
    }
    
	
    public ArrayList<String> GAMSOptimization (String string, String string2) {
    	ArrayList<String> OptimizationResult =  new ArrayList<String>();
    	double P1 = Double.parseDouble(string);
    	double P2 = Double.parseDouble(string2);
    	
    	
    	Double PriceOfProduct, PriceOfUtility;
    	
    	try {
			//load owl file
			String uri = "File:/C:/apache-tomcat-8.0.24/webapps/ROOT/BiodieselPlant_.owl";
			OWLModel owlModel = ProtegeOWL.createJenaOWLModelFromURI(uri);
			
			/**associating Model1 to a JAVA OWLIndividual variable*/
	        OWLIndividual M = owlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#GAMSModelOfB3");
	        String GAMSModel = M.getPropertyValueLiteral(owlModel.getOWLProperty("mathematical_model_extended:hasGAMSCode")).getString();  //This line needs to be modified!!
	        
			
	        PriceOfProduct = P1;
	        PriceOfUtility = P2;
	        
	        System.out.println("biodieselPrice= " + PriceOfProduct + "; utilityPrice= " +  PriceOfUtility);
	        
	        /**add a GAMS model*/
	        GAMSWorkspaceInfo  wsInfo  = new GAMSWorkspaceInfo();
	        
	        /**create a directory*/
	        File workingDirectory = new File(System.getProperty("user.dir"), "GAMS_Test");
	        workingDirectory.mkdir();
	        wsInfo.setWorkingDirectory(workingDirectory.getAbsolutePath());	        
	        
	        /**create a workspace*/
	        GAMSWorkspace ws = new GAMSWorkspace(wsInfo);
	            
	        /**prepare input data*/
	        List<String> userDefineP = Arrays.asList("biodieselProduct", "utility");
	        
	        Map<String, Double> pr = new HashMap<String, Double>();
	        {
	             pr.put("biodieselProduct", PriceOfProduct);
	             pr.put("utility", PriceOfUtility);
	        } 
	            
	        /**add a database and add input data into the database*/
	        GAMSDatabase db = ws.addDatabase();
	            
	        GAMSParameter price = db.addParameter("price", 1, "user specified price");
	        for (String ud : userDefineP) {
	            price.addRecord(ud).setValue( pr.get(ud) );
	        }    
	        
	        // create and run a job from the GAMSModel and read gdx include file from the database
	        GAMSJob GAMSjob_test = ws.addJobFromString(GAMSModel);
	        GAMSOptions opt = ws.addOptions();
	        
	        opt.defines("gdxincname", db.getName());
	        
	        GAMSjob_test.run(opt, db);
	        System.out.println("GAMS Finished Optizing job!");
	        
	        System.out.println("Output: " + GAMSjob_test.OutDB().getVariable("OBJ").findRecord().getLevel());
	        
	        DecimalFormat dff = new DecimalFormat("#.##");
	        OptimizationResult.add(String.valueOf(dff.format(GAMSjob_test.OutDB().getVariable("x").findRecord("1").getLevel())));
	        OptimizationResult.add(String.valueOf(dff.format(GAMSjob_test.OutDB().getVariable("x").findRecord("2").getLevel())));
	        OptimizationResult.add(String.valueOf(dff.format(GAMSjob_test.OutDB().getVariable("x").findRecord("3").getLevel())));
	        OptimizationResult.add(String.valueOf(dff.format(GAMSjob_test.OutDB().getVariable("x").findRecord("4").getLevel())));
	        OptimizationResult.add(String.valueOf(dff.format(GAMSjob_test.OutDB().getVariable("x").findRecord("5").getLevel())));
	        OptimizationResult.add(String.valueOf(dff.format(GAMSjob_test.OutDB().getVariable("x").findRecord("6").getLevel())));
	        OptimizationResult.add(",");
			
		} catch (OntologyLoadException e) {
			e.printStackTrace();
		} 
    	
		return OptimizationResult;    	    	
    }
    

}
