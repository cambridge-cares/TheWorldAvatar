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
package uk.ac.cam.cares.jps.agent.agent_eg;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Controller;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/**
 *
 * @author Owen Parry {@literal <oparry@cmclinnovations.com>}
 */
@Controller
@WebServlet(urlPatterns = {AdditionAgent.API_PATTERN})
public class AdditionAgent extends JPSAgent {

    // ============================ Static variables ===========================
    private static final Logger LOGGER = LoggerFactory.getLogger(AdditionAgent.class);

    static final String API_PATTERN = "/api/v1";
    private static final String BAD_REQUEST_MSG_KEY = "bad_request";
    private static final String REQUEST_RECEIVED_MSG = "Request received.";
    private static final String INVALID_REQUEST_MSG = "Invalid request.";

    private static final String FIRST_PARAM_KEY = "a";
    private static final String SECOND_PARAM_KEY = "b";
    private static final String RESULT_KEY = "c";

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
        System.out.println(REQUEST_RECEIVED_MSG + " (two arg)");
        return processRequestParameters(requestParams);
    }

    /**
     * Processes HTTP requests.
     *
     * @param requestParams Request parameters as a JSONObject
     * @return
     */
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        boolean isValidInput;
        System.out.println(REQUEST_RECEIVED_MSG);
        try {
            isValidInput = validateInput(requestParams);
        } catch (BadRequestException e) {
            return requestParams.put(BAD_REQUEST_MSG_KEY, e.getMessage());
        }
        if (isValidInput) {
            System.out.println("keys are " + requestParams.keySet().toString());
            Double resultVal = requestParams.getDouble(FIRST_PARAM_KEY) + requestParams.getDouble(SECOND_PARAM_KEY);
            JSONObject result = new JSONObject();
            result.append(RESULT_KEY, resultVal);
            return result;
        } else {
            System.out.println(INVALID_REQUEST_MSG);
            throw new JPSRuntimeException(INVALID_REQUEST_MSG);
        }
    }

    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        boolean valid = true;//requestParams.has(FIRST_PARAM_KEY) && requestParams.has(SECOND_PARAM_KEY);
        if (valid) {
            System.out.println("VALID CALL");
        } else {
            System.out.println("INVALID CALL");
        }
        return valid;
    }
//
//    public static void main(String[] args) {
//        JSONObject params = new JSONObject("%7B%22a%22%3A1%2C+%22b%22%3A2%7D");
//        AdditionAgent agent = new AdditionAgent();
//        JSONObject ret = agent.processRequestParameters(params);
//        int x = 1;
//    }

}
