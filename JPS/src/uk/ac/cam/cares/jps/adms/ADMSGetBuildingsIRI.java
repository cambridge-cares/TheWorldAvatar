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

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.util.*;

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
		ArrayList<String> args  = new ArrayList<String>();
		
		args.add("python");
		args.add("buildingsIRI.py");
		args.add(coordinates);  // start buildingsIRI.py passing coordinates as the parameter e.g. String coordinates = " {'xmin':79480, 'xmax':79490, 'ymin':454670, 'ymax':454680}";
		String targetFolder = "C:/TOMCAT/webapps/JPS/python/caresjpsadmsinputs/" ;
		String rawResult = CommandHelper.executeCommands(targetFolder, args);
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


	
}
