package uk.ac.cam.cares.jps.adms;

import java.io.IOException;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.util.CommandHelper;

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
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse
	 *      response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		// TODO Auto-generated method stub
		String targetFolder = request.getParameter("targetFolder");
		String res = startADMS(targetFolder);
		response.getWriter().write(res);
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

	public String startADMS(String targetFolder) {
		// ServletContext context = getServletContext();
		String startADMSCommand = "\"C:\\Program Files (x86)\\CERC\\ADMS 5\\ADMSModel.exe\" /e2 /ADMS \"test.apl\"";
		CommandHelper.executeSingleCommand(targetFolder, startADMSCommand);
		return targetFolder;
	}

}
