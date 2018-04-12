package uk.ac.cam.cares.jps.adms;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Servlet implementation class ADMSGetBuildingsIRI
 */
@WebServlet("/ADMSGetBuildingsIRI")
public class ADMSGetBuildingsIRI extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public ADMSGetBuildingsIRI() {
        super();
        // TODO Auto-generated constructor stub
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub

		String coordinates  = request.getParameter("coordinates");
		// String coordinates = " {'xmin':79480, 'xmax':79490, 'ymin':454670, 'ymax':454680}";
		ArrayList<String> args  = new ArrayList<String>();
		args.add(coordinates);
		String rawResult = runPython("buildingsIRI.py", args, response);
	    String result = rawResult.split("###")[1];
	    response.getWriter().write(result);
	
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
		ServletContext context = getServletContext();
//	    String fullPath0 = context.getRealPath("/workingdir/ADMS/caresjpsadmsinputs");// Such path is in the folder where your tomcat for this project is installed 
//		try {
//			response.getWriter().write(fullPath0);
//		} catch (IOException e1) {
//			// TODO Auto-generated catch block
//			e1.printStackTrace();
//		}
//	    
//		return "Something";
////	    
		String fullPath =  context.getRealPath("/workingdir/ADMS/caresjpsadmsinputs/") + filename ; // Hardcoded
		String[] cmd = new String[2 + args.size()];
		cmd[0] = "python";// Hardcoded
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
