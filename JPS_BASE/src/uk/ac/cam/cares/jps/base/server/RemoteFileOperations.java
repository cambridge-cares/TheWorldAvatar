package uk.ac.cam.cares.jps.base.server;

import java.io.IOException;
import java.net.URI;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.KeyValueServer;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.util.Misc;

@WebServlet(urlPatterns = {"/update"})
public class RemoteFileOperations extends HttpServlet {
	
	private static final long serialVersionUID = 7937678884547203019L;
	private static Logger logger = LoggerFactory.getLogger(RemoteFileOperations.class);

	@Override
	public void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
			
		String path = request.getPathInfo();
		logger.info("called for path=" + path);
		
		JSONObject jo = AgentCaller.readJsonParameter(request);
		
		String result = null;
		if ("/update".equals(path)) {
			
			result = update(jo);
			
		} else {
			throw new JPSRuntimeException("unknown operation");
		}
		
		
		AgentCaller.printToResponse(result, response);	
	}
	
	private String update(JSONObject jo) {
		
		String resource = Misc.notNull(jo, JPSConstants.SCENARIO_RESOURCE);
		String sparqlUpdate = Misc.notNull(jo, JPSConstants.QUERY_SPARQL_UPDATE);
		
		URI uri = AgentCaller.createURI(resource);
		String rootPath = KeyValueServer.get("absdir.root");
		String localFile = rootPath + uri.getPath();
		
		
		
		
		return "";
	}
}
