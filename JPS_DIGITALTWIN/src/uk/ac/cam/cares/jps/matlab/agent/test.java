package uk.ac.cam.cares.jps.matlab.agent;

import java.io.IOException;
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
 * Servlet implementation class test
 */
@WebServlet("/test")
public class test extends JPSHttpServlet {
	private static final long serialVersionUID = 1L;
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public test() {
        super();
        // TODO Auto-generated constructor stub
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
    
    /*
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
		
		// Call matlab function to execute the electrical model in matlab
		
		//Created a executable script (call_Matalab.sh) to run the script.
		
		//For windows machine change the file_name_macos.sh extention to file_name_win.bat 
		
		//It is running on no display mode.
		
		 Runtime rs = Runtime.getRuntime();
		    try {
				try {
					System.out.printf("\n----------------------Starting the execution of the electrical system-----------------------\n ");
					rs.exec("/Users/gourab/Matlab/call_matlab.sh").waitFor();
				} catch (InterruptedException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				System.out.printf("\n---------------------------Completed Execution-------------------------------\n");
				
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		
		
		response.getWriter().append("Served at: ").append(request.getContextPath());
	}
	*/

	
	@Override
	 //this should ONLY be called by scenarioAgent
	   	protected JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
			JSONObject jo = AgentCaller.readJsonParameter(request);
			String baseUrl= QueryBroker.getLocalDataPath();
			//check name of scenario: 
			String sourceUrl = JPSContext.getScenarioUrl(requestParams);
			String sourceName = BucketHelper.getScenarioName(sourceUrl);
			//logger.info("Scenario Url" + sourceUrl);
			jo.put("baseUrl", baseUrl);
			Runtime rs = Runtime.getRuntime();
		    try {
				try {
					System.out.printf("\n----------------------Starting the execution of the electrical system-----------------------\n ");
					rs.exec("/Users/gourab/Matlab/call_matlab.sh").waitFor();
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
	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
		doGet(request, response);
	}

}
