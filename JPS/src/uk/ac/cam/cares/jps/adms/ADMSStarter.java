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

import uk.ac.cam.cares.jps.base.util.*;
import uk.ac.cam.cares.jps.base.config.AgentLocator;

/**
 * Servlet implementation class AMDSStarter
 */
@WebServlet("/ADMSStarter")
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
		String startADMSCommand = "\"C:\\Program Files (x86)\\CERC\\ADMS 5\\ADMSModel.exe\" /e2 /ADMS \"test.apl\"";
		ServletContext context = getServletContext();
		String targetFolder  =  AgentLocator.getPathToWorkingDir(this) + "/" + "ADMS";   // execute adms within the target folder where the input file is generated in the previous step
		CommandHelper.executeSingleCommand(targetFolder, startADMSCommand);
		return "Started ADMS"; 
	}
	 

}
