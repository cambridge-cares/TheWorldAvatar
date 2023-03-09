package uk.ac.cam.cares.jps.ontomatch;

import java.util.HashMap;
import java.util.Map;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.annotate.MetaDataAnnotator;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.util.AsyncPythonHelper;
import uk.ac.cam.cares.jps.ontomatch.properties.OntomatchProperties;
import uk.ac.cam.cares.jps.paramsValidator.ParamsValidateHelper;
import uk.ac.cam.cares.jps.paramsValidator.ParamsValidateHelper.CUSTOMVALUETYPE;

/**
 * Agent that performs element level matching, four types can be chosen: string,
 * word, domain, value Functionally it is a servlet wrapper around the python
 * programs that actually does the job Output to KG: Alignment ontology of this
 * measurement
 * 
 * @author shaocong zhang
 * @version 1.0
 * @since 2020-09-08
 */

@WebServlet(urlPatterns = { "/elementMatcher" })
public class ElementMatcher extends JPSAgent {

	private static final long serialVersionUID = -7032945484999523116L;
	private String venvname = OntomatchProperties.getInstance().getProperty(OntomatchProperties.VENV_NAME);
	private AsyncPythonHelper pyHelper =  AsyncPythonHelper.getInstance(venvname);
	/** Types of element matchers **/
	public enum MATCHERTYPE {
		DOMAIN, STRING, WORD, VALUE, I_STRING, I_WORD
	}

	/**
	 * Types of matching type, only related to function names in py script, no
	 * algorithm difference
	 **/
	public enum MATCHING_TYPE {
		INDIVIDUAL, TERM
	}

	@Override
	public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		JSONObject jo = requestParams;
		String savePath = null, targetOnto = null, sourceOnto = null, modelPath = null, dictPath = null;
		MATCHERTYPE type = null;
		MATCHING_TYPE matchingType = null;
		String tgtPkl, srcPkl, pyMatchFunction = null;
		JSONObject resultObj = new JSONObject();
		if (validateInput(requestParams)) {

		/*** read params ****/
		try {
			savePath = jo.getString("alignmentFileAddress");
			targetOnto = jo.getString("targetOntoIRI");
			sourceOnto = jo.getString("sourceOntoIRI");
			type = MATCHERTYPE.valueOf(jo.getString("matcherType"));
			matchingType = MATCHING_TYPE.valueOf(jo.getString("matchingType"));
			//logger.info("element matcher agent: matching type:" + matchingType + " matcherType:" + type + " savePath:"
			//		+ savePath + " targetOntology:" + targetOnto + " sourceOnto:" + sourceOnto);
			/** special case: domain matcher needs extra params **/
			if (type == MATCHERTYPE.DOMAIN) {
				modelPath = jo.getString("modelAddress");
				dictPath = jo.getString("dictAddress");
				logger.info("element matcher agent: extra params: model address:"+modelPath+" dictAddress"+dictPath);
			}

		} catch (Exception e1) {
			throw new JPSRuntimeException(e1);

		}
		/*** call python and write alignment to kG(rdf) *****/
		try {
			/** get matching function name in py */
			pyMatchFunction = getPyFunctionName(matchingType);
			/** query pkl address ***/
			tgtPkl = queryLexicalPklAddressfromMetaData(targetOnto);
			srcPkl = queryLexicalPklAddressfromMetaData(sourceOnto);
			System.out.println("element matcher agent: get pre-processfile for:"+targetOnto+" from metadata:"+tgtPkl);
			System.out.println("element matcher agent: get pre-processfile for:"+sourceOnto+" from metadata:"+srcPkl);

			/** get matcher script addr ****/
			String[] results = null;
			String matcherLocation = getMatcherScriptName(type);// get script name by matcher type and call
			/** construct params list according to matcher type, then call python ***/
			if (type == MATCHERTYPE.DOMAIN) {// special case: domain matcher needs extra params
				String[] paras = { savePath, srcPkl, tgtPkl, pyMatchFunction, modelPath, dictPath };
				results = pyHelper.callPython(matcherLocation, paras, ElementMatcher.class);
			} else {// other cases
				String[] paras = { savePath, srcPkl, tgtPkl, pyMatchFunction };
				results = pyHelper.callPython(matcherLocation, paras, ElementMatcher.class);
			}
			System.out.println(results[0]);
			System.out.println(results[1]);

			/** check output stream for err/sucess **/
			if (results[0].contains("success")) {
				resultObj.put("success", true);
			} else {
				resultObj.put("error", results[1]);
				throw new JPSRuntimeException(results[1]);
			}
		} catch (Exception e) {
			throw new JPSRuntimeException(e);
		}
		}
		return resultObj;
	}

	/***
	 *
	 * Query address of pre-process pkl file for the ontology, which is stored in
	 * metadata
	 * 
	 * @param IRI
	 * @return address of the pre-process file
	 */
	private String queryLexicalPklAddressfromMetaData(String IRI) {
		String addr = null;
		String queryStr = "PREFIX dcterms: <http://purl.org/dc/terms/> " + "SELECT ?addr  " + "WHERE {"
				+ "?addr  dcterms:subject <" + IRI + ">." + "}";
		System.out.println(queryStr);
		String resultStr = KnowledgeBaseClient.query(MetaDataAnnotator.getMetadataSetUrl(), null, queryStr);
		try {
			JSONObject jo = new JSONObject(resultStr);
			System.out.println(resultStr);
			addr = jo.getJSONObject("results").getJSONArray("bindings").getJSONObject(0).getJSONObject("addr")
					.getString("value");
		} catch (Exception e) {
			throw new JPSRuntimeException(e);
		}
		return addr;
	}

	/**
	 * get script path specific to matcher type
	 * 
	 * @param matcherType
	 * @return
	 * @throws Exception
	 */
	private String getMatcherScriptName(MATCHERTYPE matcherType) throws Exception {
		switch (matcherType) {
		case STRING:
			return OntomatchProperties.getInstance().getProperty(OntomatchProperties.PY_NAME_STRINGMATCHER);
		case WORD:
			return OntomatchProperties.getInstance().getProperty(OntomatchProperties.PY_NAME_BOWMATCHER);
		case DOMAIN:
			return OntomatchProperties.getInstance().getProperty(OntomatchProperties.PY_NAME_DOMAINMATCHER);
		case VALUE:
			return OntomatchProperties.getInstance().getProperty(OntomatchProperties.PY_NAME_VALUEMATCHER);
		case I_STRING:
			return OntomatchProperties.getInstance().getProperty(OntomatchProperties.PY_NAME_ISTRINGMATCHER);
		case I_WORD:
			return OntomatchProperties.getInstance().getProperty(OntomatchProperties.PY_NAME_IBOWMATCHER);
		default:
			throw new JPSRuntimeException("Matcher Type not Supported");
		}
	}

	/***
	 * get function name in py according to matching type
	 * 
	 * @param matchingType
	 * @return function name
	 * @throws Exception
	 */
	private String getPyFunctionName(MATCHING_TYPE matchingType) throws Exception {
		switch (matchingType) {
		case TERM:
			return OntomatchProperties.getInstance().getProperty(OntomatchProperties.MATCH_TERM_FUNCTION_NAME);
		case INDIVIDUAL:
			return OntomatchProperties.getInstance().getProperty(OntomatchProperties.MATCH_INDIVIDUAL_FUNCTION_NAME);
		default:
			throw new JPSRuntimeException("Matching Type not Found");
		}
	}


    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (requestParams.isEmpty()||!requestParams.has("alignmentFileAddress")||!requestParams.has("matchingType")||!requestParams.has("targetOntoIRI")||!requestParams.has("sourceOntoIRI")||!requestParams.has("matcherType")) {
            throw new BadRequestException();
        }
		Map<String, CUSTOMVALUETYPE> paramTypes = new HashMap<String, CUSTOMVALUETYPE>();
	     paramTypes.put("alignmentFileAddress",CUSTOMVALUETYPE.PATH );
	     paramTypes.put("targetOntoIRI", CUSTOMVALUETYPE.URL);
	     paramTypes.put("sourceOntoIRI", CUSTOMVALUETYPE.URL);

			if (!ParamsValidateHelper.validateALLParams(requestParams, paramTypes)) {
				throw new BadRequestException();
			}
			return true;        
    }	

}
