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

@WebServlet("/ADMSStarter")
public class ADMSStarter extends HttpServlet {
	private static final long serialVersionUID = 1L;


	public ADMSStarter() {
		super();
	}


	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		String targetFolder = request.getParameter("targetFolder");
		if(request.getServerName().contains("localhost")) {
			response.getWriter().write("Running in localhost, ADMS won't be started");
		}
		else {
			String res = startADMS(targetFolder);
			response.getWriter().write("ADMS finished simulation");
		}

	}

 
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		doGet(request, response);
	}

	public String startADMS(String targetFolder) {
		String startADMSCommand = "\"C:\\Program Files (x86)\\CERC\\ADMS 5\\ADMSModel.exe\" /e2 /ADMS \"test.apl\"";
		CommandHelper.executeSingleCommand(targetFolder, startADMSCommand);
		return targetFolder;
	}

}
