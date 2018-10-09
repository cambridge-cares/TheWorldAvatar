package uk.ac.cam.cares.jps.adms;

import java.io.IOException;
import java.util.ArrayList;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.util.CommandHelper;

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
 	}

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse
	 *      response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
 
		String selectedSource = request.getParameter("selectedSource");
		String buildingTopNode = request.getParameter("buildingTopNode");
		String coordinates = request.getParameter("coordinates");
		String[] substances = request.getParameterValues("substances");
		Integer buildingLimit = 2;
		if (request.getParameter("buildingLimit") != null) {
			buildingLimit = Integer.parseInt(request.getParameter("buildingLimit"));
		}

		Integer buildingNumber = 25;
		if (request.getParameter("buildingLimit") != null) {
			buildingNumber = Integer.parseInt(request.getParameter("buildingLimit"));
		}

		Boolean filterSource = (request.getParameter("filterSource").equals("true"));

		// ==================== Start admsMain to create input files for ADMS
		// ===================
		ArrayList<String> args = new ArrayList<String>();
		args.add("python");
		args.add("admsMain.py"); // python admsMain.py %1 %2
		args.add(selectedSource.toString());
		args.add(coordinates.toString().replaceAll(",", "#"));
		ServletContext context = getServletContext();
		String fullPath = AgentLocator.getPathToWorkingDir(this) + "/" + "ADMS";
		args.add(fullPath); // this extra parameter tells the python script where to put the input files, in
							// this case, working dir
		String targetFolder = AgentLocator.getNewPathToPythonScript("/caresjpsadmsinputs", this);
		CommandHelper.executeCommands(targetFolder, args);
		response.getWriter().write("Success");
		// =========================================================================================

	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse
	 *      response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
 		doGet(request, response);
	}

}
