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
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

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

        if (validateInput(requestParams,path)) {
	        switch (path) {
	        	case URL_MINTIME:
	        		LOGGER.info("Querying min time");
	        	
	        	case URL_MAXTIME:
	        		LOGGER.info("Querying max time");
	        		
	        	case URL_DURATION:
	        		LOGGER.info("Processing duration");
	        }
        }
        
        return new JSONObject();
    }

    public boolean validateInput(JSONObject requestParams,String path) throws BadRequestException {
        boolean valid = false;
        
        switch (path) {
	    	case URL_MINTIME:
	    		LOGGER.info("Checking min time");
	    		break;
	    	
	    	case URL_MAXTIME:
	    		LOGGER.info("Checking max time");
	    		break;
	    		
	    	case URL_DURATION:
	    		LOGGER.info("Checking duration");
	    		break;
	    }
        
        return valid;
    }

}
