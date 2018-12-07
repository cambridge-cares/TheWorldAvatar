package uk.ac.cam.cares.jps.adms;

import java.io.IOException;
import java.util.ArrayList;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.util.CommandHelper;

/**
 * Servlet implementation class ADMSOutput
 */
@WebServlet("/ADMSOutputAll")
public class ADMSOutputAll extends HttpServlet {
	private static final long serialVersionUID = 1L;
	private static Logger logger = LoggerFactory.getLogger(ADMSOutputAll.class);

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 * get all adms output in one go
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		request.setCharacterEncoding("UTF-8");
			
		String targetFolder = AgentLocator.getNewPathToPythonScript("caresjpsadmsinputs", this);
		String outputFile = AgentLocator.getPathToJpsWorkingDir() + "/JPS/ADMS/test.levels.gst";
				
		ArrayList<String> args = new ArrayList<String>();
		System.out.println("================ output file ===============");
		System.out.println(outputFile);
		System.out.println("============================================");
		args.add("python");
		args.add("gstReader.py"); 
		args.add(outputFile);
		args.add("4");
		args.add("5");
		args.add("");
		
		String result = CommandHelper.executeCommands(targetFolder, args);
		 
		logger.debug("=== Result === :" + result);
		response.setContentType("application/json");
		response.getWriter().write(result);
	}
}
