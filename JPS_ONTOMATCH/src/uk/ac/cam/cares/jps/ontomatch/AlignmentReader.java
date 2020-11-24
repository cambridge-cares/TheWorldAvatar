package uk.ac.cam.cares.jps.ontomatch;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.BadRequestException;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.query.ResourcePathConverter;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.ontomatch.alignment.AlignmentIOHelper;
import uk.ac.cam.cares.jps.paramsValidator.ParamsValidateHelper;
import uk.ac.cam.cares.jps.paramsValidator.ParamsValidateHelper.CUSTOMVALUETYPE;

/**
 * Agent that reads content from alignment kg(rdf format), for visualization purpose
 * Input from KG: alignment Ontology of one matching process
 * @author shaocong zhang
 * @version 1.0
 * @since 2020-11-25
 */

@WebServlet(urlPatterns = { "/alignment" })
public class AlignmentReader extends JPSAgent {
    
	private static final long serialVersionUID = -4365515995166685342L;

	
	
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		//logger.info("AlignmentReader agent");
		JSONObject jo = requestParams;
		String afileIRI = "";
		Double threshold = 0.0;
		//read parameters
		try {
			afileIRI = jo.getString("alignmentIRI");
			threshold = jo.getDouble("threshold");
		} catch (JSONException e1) {
			e1.printStackTrace();
		}

		//logger.info("reading alignment from  " + afileIRI);
	 	JSONArray instances2Equal;

		JSONObject result = new JSONObject();
		try {
			//get list of matched IRIs
			String localAddress = ResourcePathConverter.convertToLocalPath(afileIRI);
			instances2Equal = AlignmentIOHelper.readAlignmentFileAsJSONArray(localAddress, threshold);
			result.put("alignmentlist", instances2Equal);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return result;
	}


    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (requestParams.isEmpty()||!requestParams.has("alignmentIRI")) {
            throw new BadRequestException();
        }
		Map<String, CUSTOMVALUETYPE> paramTypes = new HashMap<String, CUSTOMVALUETYPE>();
	     paramTypes.put("alignmentIRI",CUSTOMVALUETYPE.URL);
	     return ParamsValidateHelper.validateALLParams(requestParams, paramTypes);
    }	

}

