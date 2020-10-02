package uk.ac.cam.cares.jps.agent.matlab;

import java.io.BufferedReader;
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
	
	//Input filename: input.csv
	
	//Get the values starting from row 2 and store it in array
	
	//Loop the array till end and multiply ActivePower values with 0.5 in a new array key to get the reactive power
	
	//Create a new CSV file and write it into the output directory /res/matlab
	
	//Output filename: output.dat
	
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
			String pathToInputFile = "C:/JParkSimulator-git-project/JPS_DIGITAL_TWIN/src/main/resources/input_mat/input.csv";
			
		 
			// Appending on the 
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
				
				String pathToOutputFile = "C:/JParkSimulator-git-project/JPS_DIGITAL_TWIN/src/main/resources/matlab/output.dat";			
				 
				FileWriter csvWriter = new FileWriter(pathToOutputFile);
				
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
			//Execute matlab
			Runtime rs = Runtime.getRuntime();
		    try {
				try {
					System.out.printf("\n----------------------Starting the execution of the electrical system-----------------------\n ");
					rs.exec("C:/JParkSimulator-git-project/JPS_DIGITAL_TWIN/src/main/resources/matlab/call_matlab.bat").waitFor();
					
				} catch (InterruptedException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				System.out.printf("\n---------------------------Completed Execution-------------------------------\n");
				
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		    //response.getWriter().append("Served at: ").append(request.getContextPath());
		    return jo;
		}


}
