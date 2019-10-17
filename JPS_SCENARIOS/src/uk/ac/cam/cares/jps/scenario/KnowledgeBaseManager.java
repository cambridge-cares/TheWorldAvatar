package uk.ac.cam.cares.jps.scenario;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.FileUtils;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.BucketHelper;
import uk.ac.cam.cares.jps.base.scenario.ScenarioHelper;

@WebServlet(urlPatterns = {"/kb/*", "/data/*"})
public class KnowledgeBaseManager extends HttpServlet {

	private static final long serialVersionUID = -4195274773048314961L;
	
	private static Logger logger = LoggerFactory.getLogger(KnowledgeBaseManager.class);
	private static final String DATASET_MISC = "misc";
	private static final String DATASET_META = "meta";
	
	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		
		System.out.println("KnowledgeBaseManager GET");
		
		String path = req.getPathInfo();
		logger.info("path = " + path);
		String datasetName = checkForDataset(path);
		if (DATASET_MISC.contentEquals(datasetName)) {
			
		} else {
			throw new JPSRuntimeException("unknown dataset = " + datasetName);
		}
	}
	
	@Override
	protected void doPut(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {

		System.out.println("KnowledgeBaseManager PUT");
		
		String url = req.getRequestURL().toString();
		logger.info("request url = " + url);	
		
		String path = req.getPathInfo();
		logger.info("path = " + path);
		String datasetName = checkForDataset(path);
		
		JSONObject jo = AgentCaller.readJsonParameter(req);
		String sparqlUpdate = jo.optString(JPSConstants.QUERY_SPARQL_UPDATE);
		logger.info("sparql update = " + sparqlUpdate);
		
		if (sparqlUpdate.isEmpty()) {
			InputStream stream = null;
			try {
				stream = req.getInputStream();				
				String filePath = BucketHelper.getLocalPath(url);
				File file = new File(filePath);
				//Scanner s = new Scanner(stream).useDelimiter("\\A");
				//String result = s.hasNext() ? s.next() : "";
				FileUtils.copyInputStreamToFile(stream, file);
			} finally {
				stream.close();
			}
		} else {
			System.out.println("TODO: sparql update");
		}
	}
	
	public String checkForDataset(String path) {
		
		int i = path.indexOf("/");
		if (i < 0) {
			return path;
		}
		
		return path.substring(0, i);
	}
	
	
	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	private void doGetOLD(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
			
		String path = request.getServletPath() + request.getPathInfo();
		String cutPath = ScenarioHelper.cutHash(path);;
		String bucket = ScenarioHelper.getScenarioBucket(JPSConstants.SCENARIO_NAME_BASE);
		String hostport = KeyValueManager.get(IKeys.HOST) + "_" + KeyValueManager.get(IKeys.PORT);
		String localFile = bucket + "/" + hostport + cutPath;		
		String result =  new QueryBroker().readFile(localFile); 
		AgentCaller.printToResponse(result, response);
	}
}
