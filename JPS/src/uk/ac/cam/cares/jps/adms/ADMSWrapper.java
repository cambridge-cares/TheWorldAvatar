package uk.ac.cam.cares.jps.adms;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.*;

import org.apache.http.HttpResponse;
import org.apache.http.ParseException;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
/**
 * Servlet implementation class ADMSWrapper
 */
@WebServlet("/ADMSWrapper")
public class ADMSWrapper extends HttpServlet {
	private static final long serialVersionUID = 1L;

    /**
     * Default constructor. 
     */
    public ADMSWrapper() {
        // TODO Auto-generated constructor stub
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
		
		String selectedSource = request.getParameter("selectedSource");
		String buildingTopNode = request.getParameter("buildingTopNode");
		String coordinates = request.getParameter("coordinates");
		String[] substances = request.getParameterValues("substances");
		Integer buildingLimit = 2;
		if (request.getParameter("buildingLimit")!=null) {
			buildingLimit = Integer.parseInt(request.getParameter("buildingLimit"));
		}
		
		Boolean filterSource = (request.getParameter("filterSource").equals("true"));
		
//		
//		System.out.println("buildingLimit" + buildingLimit);
//		System.out.println("selectedSource" + selectedSource);
//		System.out.println("buildingTopNode" + buildingTopNode);
//		System.out.println("coordinates" + coordinates);
//		System.out.println("substances" + Arrays.toString(substances));
//		System.out.println("filterSource" + filterSource);
//		
		ArrayList<String> args  = new ArrayList<String>();
		//  [selectedSource, buildingTopNode,coordinates, substances, builingLimit,filterSource];
		args.add(selectedSource.toString());
		args.add(buildingTopNode.toString());
		args.add(coordinates.toString());
		args.add(Arrays.toString(substances));
		args.add(buildingLimit.toString());
		args.add(filterSource.toString());
		
		System.out.println(args);
		runPython("admsMain.py", args, response);
		String startADMSRequets = "http://localhost/JPS/ADMSStarter";
		HttpUriRequest request1 = new HttpGet(startADMSRequets);
		HttpResponse httpResponse = null;
		try {
			httpResponse = HttpClientBuilder.create().build().execute(request1);
		} catch (IOException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		
 
		
		
	}
 
	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
		doGet(request, response);
	}
	
	
	
	
	
	public String runPython(String filename , ArrayList<String> args, HttpServletResponse response) 
	{ //need to call myscript.py and also pass arg1 as its arguments.
	  //and also myscript.py path is in C:\Demo\myscript.py
		// ServletContext context = getServletContext();
		// String fullPath = context.getRealPath("/WEB-INF/admsInput/admsMain.py");// Such path is in the folder where your tomcat for this project is installed 
		
		String fullPath = "C:\\TOMCAT\\webapps\\JPS\\workingdir\\ADMS\\caresjpsadmsinputs\\" + filename ; // Hardcoded
		String[] cmd = new String[2 + args.size()];
		cmd[0] = "python3";// Hardcoded
		cmd[1] = fullPath;
		for(int i = 0; i < args.size(); i++) {
		cmd[i+2] = args.get(i);
		}
		// create runtime to execute external command
		Runtime rt = Runtime.getRuntime();
		Process pr = null;
		
		System.out.println("cmd : " + cmd);
		try {
			pr = rt.exec(cmd);
			System.out.print("Executing");
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		 
		// retrieve output from python script
		BufferedReader bfr = new BufferedReader(new InputStreamReader(pr.getInputStream()));
		String line = "";
		String resultString = "";
		try {
			while((line = bfr.readLine()) != null) {
			// display each output line form python script
			resultString += line;
			}
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return resultString;
		
		 
	
	}
	

}
