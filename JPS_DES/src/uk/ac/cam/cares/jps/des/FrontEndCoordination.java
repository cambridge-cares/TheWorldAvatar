package uk.ac.cam.cares.jps.des;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.annotate.MetaDataQuery;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

@WebServlet(urlPatterns = { "/showDESResult"})

public class FrontEndCoordination  extends JPSAgent{

	private static final long serialVersionUID = 1L;

	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
	    requestParams = processRequestParameters(requestParams, null);
	    return requestParams;
	}
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams,HttpServletRequest request) {
    	 	JSONObject responseParams = requestParams;
	    	String v = AgentCaller.executeGetWithJsonParameter("JPS_DES/GetBlock", requestParams.toString());
 			System.gc();
 			responseParams = new JSONObject(v);
 	    		 
 			
    	return responseParams;
    }
   
    

    
}
