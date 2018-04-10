package uk.ac.cam.cares.jps.adms;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.catalina.tribes.util.Arrays;

/**
 * Servlet implementation class AMDSStarter
 */
@WebServlet("/AMDSStarter")
public class ADMSStarter extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public ADMSStarter() {
        super();
        // TODO Auto-generated constructor stub
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
		String res = startADMS();
		response.getWriter().write(res);
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
		doGet(request, response);
	}
	
	
	public String startADMS()
	{
		// ServletContext context = getServletContext();
		String fullPath ="/eclipse-workspace/JPS/python/caresjpsadmsinputs";// Such path is in the folder where your tomcat for this project is installed 
		System.out.println("full path" + fullPath);
		String[] cmd = {fullPath + "/startADMS.sh"}; //// Hardcoded 
		Runtime rt = Runtime.getRuntime();
		Process pr = null;
		System.out.println("cmd : " + Arrays.toString(cmd));
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

		try {
			while((line = bfr.readLine()) != null) {
			// display each output line form python script
			System.out.println(line);
			}
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return line;
	}
	 

}
