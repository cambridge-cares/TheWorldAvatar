package uk.ac.cam.cares.jps.adms;

import java.io.IOException;
import java.util.ArrayList;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
 
@WebServlet("/ADMSGetBuildingsIRI")
public class ADMSGetBuildingsIRI extends HttpServlet {
	private static final long serialVersionUID = 1L;

	public ADMSGetBuildingsIRI() {
		super();
	}


	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		String coordinates = request.getParameter("coordinates");
		ArrayList<String> args = new ArrayList<String>();

		args.add("python");
		args.add("buildingsIRI.py");
		args.add(coordinates); // start buildingsIRI.py passing coordinates as the parameter e.g. String
								// coordinates = " {'xmin':79480, 'xmax':79490, 'ymin':454670, 'ymax':454680}";
		String targetFolder = AgentLocator.getAbsolutePath("python/caresjpsadmsinputs/", this);
		String rawResult = CommandHelper.executeCommands(targetFolder, args);
		String result = rawResult.split("###")[1];
		response.getWriter().write(result);

	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse
	 *      response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		// TODO Auto-generated method stub
		doGet(request, response);
	}

}
