/*ZL-20160305: this servlet code is wrote for the OPAL-RT service of the JParkSimulator
**set up of the WEB-INF folder of this project (OPARTServlet\WebContent): 1) a web.xml file; 2) include the necessary libraries in the lib folder.
*/

package OPALRTServlet;

import java.io.BufferedReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class OPALRTServlet extends HttpServlet {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	public static ArrayList<String[]> editStack;	 //global variable for receiving and storing the httpRequest information
	public static String httpReqCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/httpReqTest.CSV");   //address to write the .csv file where to checke the httpRequest message
	
	public static String runPythonCommand = new String("python C:/apache-tomcat-8.0.24/webapps/ROOTT/sample_static_running.py"); // ensure that python environment variable is set to python34
	
	public OPALRTServlet() {
		
		
	}
	
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {    //doPost method to handle the httpRequest and httpResponse
		ArrayList<String[]> editStack = new ArrayList<String[]>(); // variable for receiving/storing the httpRequest information, and passing the message to relevant methods
		String[] layers = request.getParameter("layers").split(",");
		String[] OBJECTIDs = request.getParameter("OBJECTIDs").split(","); //OBJECTID indicate which particular entity being modified, not fully used at the moment, might be very useful in the future(can be used to improve the efficiency of JPS) 
		String[] appCallFlag = request.getParameter("appCallFlag").split(","); // appCallFlage indicate which function of the JPS being called from the applet side
		String[] QueryT = request.getParameter("QueryT").split(",");  //new parameter for the query function

		for (int i = 0; i < layers.length; i++) {
			editStack.add(new String[] { layers[i], OBJECTIDs[i], appCallFlag[i], QueryT[i]});
		}

		FileWriter flag1 = null;                                                      //filewriter to check whether the httpRequest have been correctly received
		flag1 = new FileWriter(httpReqCSV);
		flag1.append("layers=" + layers[0]);
		flag1.append(", OBJECTIDs=" + OBJECTIDs[0]);
		flag1.append(", appCallFlag=" + appCallFlag[0]);
		flag1.append(", QueryT=" + QueryT[0]);
		flag1.flush();
		flag1.close();  
		
		System.out.println("message received!");
	}

	//method to call the python script in order to evaluate the corresponding model
	public void runPyScript(ArrayList<String[]> editStack) {
		String appCallFlag = null;
		appCallFlag = editStack.get(0)[2];                                               // flag indicating which function has been called (PowerWorld, parameterised PW, AspenPlus, parameterised AP)

		try {
			System.out.println(appCallFlag);
			switch (appCallFlag) {

			case ("OPALRT"):                                                                     // when appCallFlag=AP indicating that the run Aspenplus button has been pressed, then the following actions are going to be taken
				System.out.println(appCallFlag + " Button was pressed! (runPyScript)");          // for double checking
				Process p = Runtime.getRuntime().exec(runPythonCommand);                         // call python script to run OPAL-RT
				p.waitFor();
				System.out.println("Exit Value (0 means success): " + p.exitValue());            // if console prints 0 it means success
				BufferedReader br = new BufferedReader(new InputStreamReader( p.getInputStream()));
				String line;                                                                     // retrieves console from python script
				System.out.println("Python input:");
				while ((line = br.readLine()) != null) {
					System.out.println(line);                                                    // print input array from Python (see python code for more details)
				}
				line = br.readLine();
				break;		
							
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
}
