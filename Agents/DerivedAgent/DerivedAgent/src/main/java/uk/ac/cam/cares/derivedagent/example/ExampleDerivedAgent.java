/*
 * Copyright (c) 2011-2021 CMCL Innovations - All Rights Reserved
 *
 * This application and all inherent data, source files, information and graphics are
 * the copyright and sole property of Computational Modelling Cambridge Ltd (CMCL Innovations).
 *
 * Any unauthorised redistribution or reproduction of part, or all, of the contents of this
 * application in any form is prohibited under UK Copyright Law. You may not, except with the
 * express written permission of CMCL Innovations, distribute or commercially exploit this
 * application or its content. All other rights reserved.
 *
 * For more information please contact support(@)cmclinnovations.com
 */
package uk.ac.cam.cares.derivedagent.example;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.derivedquantity.DerivedQuantityClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesRDBClient;

import javax.servlet.annotation.WebServlet;

/**
 * This agent has three functions
 * 1) MinTime: input - time series, queries min time from its input, and write a new instance in kg
 * 2) MaxTime: input - time series, queries max time from its input, and write a new instance in kg
 * 3) TimeDuration: inputs - MinTime & MaxTime, queries min time and max time, calculates the difference and write it to kg
 * @author Kok Foong Lee
 */
@WebServlet(urlPatterns = {ExampleDerivedAgent.URL_MINTIME, ExampleDerivedAgent.URL_MAXTIME, ExampleDerivedAgent.URL_DURATION})
public class ExampleDerivedAgent extends JPSAgent {
	private static final long serialVersionUID = 1L;

	// ============================ Static variables ===========================
	// logs are written to a hard coded location (C:/JPS_DATA/logs), defined in log4j2.xml located in src
    private static final Logger LOGGER = LoggerFactory.getLogger(ExampleDerivedAgent.class);

    public static final String URL_MINTIME = "/TimeSeriesAgent/MinTime";
    public static final String URL_MAXTIME = "/TimeSeriesAgent/MaxTime";
    public static final String URL_DURATION = "/TimeSeriesAgent/TimeDuration";

    // ================================ Methods ================================
    /**
     * Processes HTTP requests.
     *
     * @param requestParams Request parameters in a JSONObject
     * @param request HTTP Servlet Request
     * @return
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        String path = request.getServletPath();
        JSONObject response = new JSONObject();
        ExampleConfig.initProperties();
        RemoteStoreClient storeClient = new RemoteStoreClient(ExampleConfig.kgurl,ExampleConfig.kgurl,ExampleConfig.kguser,ExampleConfig.kgpassword);
    	ExampleSparqlClient sparqlClient = new ExampleSparqlClient(storeClient);

        if (validateInput(requestParams,path,sparqlClient)) {
        	JSONArray inputs = requestParams.getJSONArray(DerivedQuantityClient.AGENT_INPUT_KEY);
        	
        	TimeSeriesRDBClient<Integer> tsClient = new TimeSeriesRDBClient<Integer>(Integer.class);
        	tsClient.setRdbURL(ExampleConfig.dburl); 
        	tsClient.setRdbUser(ExampleConfig.dbuser);
        	tsClient.setRdbPassword(ExampleConfig.dbpassword);
        	
        	String inputdata_iri;
        	String createdInstance;
        	
	        switch (path) {
	        	case URL_MINTIME:
	        		LOGGER.info("Querying min time");
	        		inputdata_iri = inputs.getString(0);
        			Integer mintime = tsClient.getMinTime(inputdata_iri);
        			createdInstance = sparqlClient.createMinTime(mintime);
        			LOGGER.info("created a new min time instance " + createdInstance);
        			response.put(DerivedQuantityClient.AGENT_OUTPUT_KEY, new JSONArray().put(createdInstance));
	        		break;
	        	    
	        	case URL_MAXTIME:
	        		LOGGER.info("Querying max time");
	        		inputdata_iri = inputs.getString(0);
        			Integer maxtime = tsClient.getMaxTime(inputdata_iri);
        			createdInstance = sparqlClient.createMaxTime(maxtime);
        			LOGGER.info("created a new max time instance " + createdInstance);
        			response.put(DerivedQuantityClient.AGENT_OUTPUT_KEY, new JSONArray().put(createdInstance));
	        		
	        		break;
	        	case URL_DURATION:
	        		LOGGER.info("Processing duration");
	        		Integer mintime_input = null; Integer maxtime_input = null;

	        		// validate input should already ensure that one of them is a max time and the other is a min time
	        		if (sparqlClient.isMaxTime(inputs.getString(0))) {
	        			maxtime_input = sparqlClient.getValue(inputs.getString(0));
	        			mintime_input = sparqlClient.getValue(inputs.getString(1));
	        		} else if (sparqlClient.isMinTime(inputs.getString(0))) {
	        			mintime_input = sparqlClient.getValue(inputs.getString(0));
	        			maxtime_input = sparqlClient.getValue(inputs.getString(1));
	        		}
	        		
	        		// calculate a new value and create a new instance
	        		int timeduration = maxtime_input - mintime_input;
	        		createdInstance = sparqlClient.createTimeDuration(timeduration);
	        		LOGGER.info("created a new time duration instance " + createdInstance);
	        		response.put(DerivedQuantityClient.AGENT_OUTPUT_KEY, new JSONArray().put(createdInstance));
	        		break;
	        }
        } else {
        	LOGGER.error("Invalid input for " + path);
        }
        
        return response;
    }

    public boolean validateInput(JSONObject requestParams, String path, ExampleSparqlClient sparqlClient) throws BadRequestException {
        boolean valid = false;
        JSONArray inputs = requestParams.getJSONArray(DerivedQuantityClient.AGENT_INPUT_KEY);
        switch (path) {
	    	case URL_MINTIME:
	    		LOGGER.info("Checking input for min time");
	    		
	    		if (inputs.length() == 1) {
	    			if (sparqlClient.isInputData(inputs.getString(0))) {
	    				valid = true;
	    			}
	    		}
	    		
	    		break;
	    	
	    	case URL_MAXTIME:
	    		LOGGER.info("Checking input for max time");
	    		
	    		if (inputs.length() == 1) {
	    			if (sparqlClient.isInputData(inputs.getString(0))) {
	    				valid = true;
	    			}
	    		}
	    		break;
	    		
	    	case URL_DURATION:
	    		LOGGER.info("Checking duration");
	    		
	    		// if the first input is max time, the second one must be min time, and vice versa
	    		if (inputs.length() == 2) {
	    			if (sparqlClient.isMaxTime(inputs.getString(0))) {
	    				if (sparqlClient.isMinTime(inputs.getString(1))) {
	    					valid = true;
	    				}
	    			} else if (sparqlClient.isMinTime(inputs.getString(0))) {
	    				if (sparqlClient.isMaxTime(inputs.getString(1))) {
	    					valid = true;
	    				}
	    			}
	    		}
	    		
	    		break;
	    }
        
        return valid;
    }

}
