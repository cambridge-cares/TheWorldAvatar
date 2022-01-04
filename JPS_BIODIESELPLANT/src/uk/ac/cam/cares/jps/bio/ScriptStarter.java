package uk.ac.cam.cares.jps.bio;


import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;


/**
 * Servlet implementation class SPARQLEndPoint
 */
@WebServlet("/startScript")
public class ScriptStarter extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public ScriptStarter() {
        super();
        // TODO Auto-generated constructor stub
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
		
		
		
			String path = request.getParameter("path").replaceAll("@", "/");
			
			String[] path2 = path.split(">");
			System.out.println("Path --- " + path2);
 
			
			
			startSript(path2);
			 
	//	System.out.println("uri:	" + uri);
	      	response.setContentType("text/html");
	        response.setHeader("Cache-control", "no-cache, no-store");
	        response.setHeader("Pragma", "no-cache");
	        response.setHeader("Expires", "-1");
	        response.setHeader("Access-Control-Allow-Origin", "*");
	        response.setStatus(HttpServletResponse.SC_OK);
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
		doGet(request, response);
	}

	public void startSript(String[] path)
	{
		String s = "";
		String result = "";
		try {
 

				String[] cmd  = path;
	            Process p = Runtime.getRuntime().exec(cmd);// execute the python script to do the query

	            BufferedReader stdInput = new BufferedReader(new 
	                 InputStreamReader(p.getInputStream()));
	            System.out.println("Here is the standard output of the command:\n");
	            while ((s = stdInput.readLine()) != null) {
	                result+= s + "\n";
	            }
	            
	            System.out.println("result" + result);
	 
	        }
	        catch (IOException e) {
	            System.out.println("exception happened - here's what I know: ");
	            e.printStackTrace();
	            System.exit(-1);
	        }
	}
	
	
 
	
}
