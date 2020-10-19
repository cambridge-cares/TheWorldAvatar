package uk.ac.cam.cares.jps.agent.matlab;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.agent.configuration.gPROMSAgentConfiguration;
import uk.ac.cam.cares.jps.agent.configuration.gPROMSAgentProperty;
import uk.ac.cam.cares.jps.agent.gPROMS.gPROMSAgent;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;


/**
 * Servlet implementation class JPSMatlabAgent
 */
@WebServlet("/JPSMatlabAgent")
public class JPSMatlabAgent extends JPSHttpServlet {
	private static final long serialVersionUID = 1L;
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public JPSMatlabAgent() {
        super();
        // TODO Auto-generated constructor stub
    }
    

	//Read the gPROMS output file from /res/input/ directory
	
	//Input filename: Pump_power.csv
	
	//Get the values starting from row 2 and store it in array
	
	//Loop the array till end and multiply ActivePower values with 0.5 in a new array key to get the reactive power
	
	//Create a new CSV file and write it into the output directory /ElChemo/matlab
	
	//Output filename: matInput.dat
    
    //Create a batch file to execute MATLAB
	
	@Override
	 //this should ONLY be called by scenarioAgent
	   	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
			JSONObject jo = AgentCaller.readJsonParameter(request);
			//String baseUrl= QueryBroker.getLocalDataPath();
			//check name of scenario: 
			//String sourceUrl = JPSContext.getScenarioUrl(requestParams);
			//String sourceName = BucketHelper.getScenarioName(sourceUrl);
			//logger.info("Scenario Url" + sourceUrl);
			//jo.put("baseUrl", baseUrl);
			// Input file path
			String current = System.getProperty("user.home");
			
			String pathToInputFile = current + "\\input\\matlab.csv";
			//String inputFile = gPROMSAgent.getMatlabFile();
			//System.out.println("\n gPROMS outputput file is generated.\n gPROMS output file is located at:" + inputFile +"\n");
			
		 
			// Appending reactive power value on the Pump_power CSV file
			BufferedReader csvReader = null;
			try {
				csvReader = new BufferedReader(new FileReader(pathToInputFile));
			} catch (FileNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} 
			String row;
			try {
				
				ArrayList<ArrayList<String>> output = new ArrayList<ArrayList<String>>();
				
				row = csvReader.readLine();			
				
				while ((row = csvReader.readLine()) != null) {
				    String[] input = row.split(",");
				    
				    
				    double input2 = Double.parseDouble(input[1]) * 0.5; 
				    
				    ArrayList<String> inner = new ArrayList<String>();        

				    inner.add(input[0]);     
				    inner.add(input[1]);
				    inner.add(Double.toString(input2));
				    output.add(inner);		    
				    
				}
				//close the reader
				csvReader.close();
				
				//Write the ArrayList into CSV into the path specified
				
				String matInputFile = current + "\\matlab\\matInput.dat";
				System.out.printf("\n Matlab input file is generated.\n Matlab input file is located at:" + matInputFile +"\n");
				 
				FileWriter csvWriter = new FileWriter(matInputFile);
				
				for (List<String> rowData : output) {
				    csvWriter.append(String.join(",", rowData));
				    csvWriter.append("\n");
				}
				
				//close the writer 
				csvWriter.flush();
				csvWriter.close();		
				
		
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			//Create file path for batch file
			String batchFile = current + "\\matlab\\call_matlab.bat";
			System.out.printf("\n Matlab batch file generated. \n Batch file to execute matlab from cmd prompt is created at location:" + batchFile +"\n");
			//File path for Matlab script file
			String scriptFile = current + "\\matlab\\Run_Script.m";
			System.out.println("\n Executing the matlab script file. \n Matlab script file is located at:" + scriptFile + "\n");
			//Command string 
			String cmd = "matlab -nodisplay -nosplash -nodesktop -r \"run('"+ scriptFile + "');exit;\"";
	        //System.out.println(cmd);
			//Creating batch file
		    try {
		    	File file = new File(batchFile);
	            FileWriter writer = new FileWriter(batchFile, true);
	            writer.write(cmd);
	            writer.close();
	            //Execute batch file 
	            Runtime rs = Runtime.getRuntime();
	            try {
					rs.exec(batchFile).waitFor();
				} catch (InterruptedException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
	            System.out.printf("\nCompleted Execution\n");
				file.delete();
	            
	        } catch (IOException e) {
	            e.printStackTrace();
	        }
			File tempFile = new File(pathToInputFile);
			tempFile.delete();
			
		    return jo;
		}


}
