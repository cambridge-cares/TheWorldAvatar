//this servlet code is wrote for the OPA-RT service of the JParkSimulator

package OPARTServlet;

import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class OPARTServlet extends HttpServlet {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	public static ArrayList<String[]> editStack;	
	public static String httpReqCSV = new String("C:/apache-tomcat-8.0.24/webapps/ROOT/httpReqTest.CSV");
	
	public OPARTServlet() {
		
		
	}
	
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		ArrayList<String[]> editStack = new ArrayList<String[]>(); // reconstructeditStack from query string received
		String[] layers = request.getParameter("layers").split(",");
		String[] OBJECTIDs = request.getParameter("OBJECTIDs").split(","); // ZL-151209 OBJECTID indicating parameter of which unit in chemical process has been changed
		String[] appCallFlag = request.getParameter("appCallFlag").split(","); // (mjk, 151115) adding flag indicating which  function has been called: PowerWorld,  parameterised  PW, AspenPlus, parameterised AP
		String[] QueryT = request.getParameter("QueryT").split(",");


		for (int i = 0; i < layers.length; i++) {
			editStack.add(new String[] { layers[i], OBJECTIDs[i], appCallFlag[i], QueryT[i]});
		}

		FileWriter flag1 = null; // (mjk, 151115) testing structure of DataOutputStream object and of wr object
		flag1 = new FileWriter(httpReqCSV);
		flag1.append("layers=" + layers[0]);
		flag1.append(", OBJECTIDs=" + OBJECTIDs[0]);
		flag1.append(", appCallFlag=" + appCallFlag[0]);
		flag1.append(", QueryT=" + QueryT[0]);
		flag1.flush();
		flag1.close(); // (mjk, 151115) writing this file works fine.
		
		System.out.println("message received!");
	}
	
}
