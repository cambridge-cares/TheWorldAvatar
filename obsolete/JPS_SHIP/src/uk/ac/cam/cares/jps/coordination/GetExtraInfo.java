package uk.ac.cam.cares.jps.coordination;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.BadRequestException;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.CommandHelper;

/**
 * Servlet implementation class GetExtraInfo
 */
@WebServlet("/GetExtraInfo")
public class GetExtraInfo extends JPSAgent {
	private static final long serialVersionUID = 1L;
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public GetExtraInfo() {
        super();
        // TODO Auto-generated constructor stub
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	/*protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// TODO Auto-generated method stub
		JSONObject r = AgentCaller.readJsonParameter(request);
		String oripath=r.getString("filepath");

		// this is required because unix file paths do not appear as IRIs to the triple store
        // so we have to add file:/ in front of the path
		if (!CommandHelper.isWindows()) {
			oripath = oripath.split("file:/")[1];
		}
		String path="";
		if(oripath.contains(".gst")) {
			path = oripath.split("/JPS_ADMS")[0];
		}
		else if(oripath.contains(".dat")){
			path = oripath.split("/output")[0];
		}

		String outputFile = path + "/extra_info.json";
		// get what file is stored in the folder 
		// DAT / GST
        String result = new QueryBroker().readFileLocal(outputFile);
        System.out.println("info json selected= "+outputFile);
		response.getWriter().write(result);
	}*/

	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		if(validateInput(requestParams)){
			String oripath=requestParams.getString("filepath");
			if (!CommandHelper.isWindows()) {
				oripath = oripath.split("file:/")[1];
			}
			String path="";
			if(oripath.contains(".gst")) {
				path = oripath.split("/JPS_ADMS")[0];
			}
			else if(oripath.contains(".dat")){
				path = oripath.split("/output")[0];
			}

			String outputFile = path + "/extra_info.json";
			// get what file is stored in the folder
			// DAT / GST
			String result = new QueryBroker().readFileLocal(outputFile);
			System.out.println("info json selected= "+outputFile);
			JSONObject r= new JSONObject(result);
			requestParams=r;
		}
		return requestParams;
	}

	@Override
	public  boolean validateInput(JSONObject requestParams)throws BadRequestException {
		boolean validate= true;
		if(requestParams.isEmpty()){
			throw new BadRequestException("RequestParam is empty.");
		}else if(!requestParams.has("filepath") || requestParams.isNull("filepath")){
			throw new BadRequestException("RequestParam does not contain the key:filepath or key:filepath is null.");
		}else{
			String filepath=requestParams.getString("filepath");
			if(filepath.isEmpty()){
				throw new BadRequestException("The key:filepath is empty.");
			}
		}
		return validate;
	}
}
