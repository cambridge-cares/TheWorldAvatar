package uk.ac.cam.cares.jps.adms;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.util.CommandHelper;

@WebServlet("/ADMSStarter")
public class ADMSStarter extends HttpServlet {
	private static final long serialVersionUID = 1L;


	public ADMSStarter() {
		super();
	}


	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		System.out.println("ADMS Starter is called");
		String targetFolder = request.getParameter("targetFolder");
		JSONObject result = new JSONObject();
		try {
			result.put("folder", targetFolder);
		} catch (JSONException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		if(request.getServerName().contains("localhost")) {
			//below line is applicable only for Kevin so that the local adms could run and show the local output file
			//String res = startADMS(targetFolder);
			response.getWriter().write(result.toString());
		}
		else {
			String res = startADMS(targetFolder);
			response.getWriter().write(result.toString());
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
