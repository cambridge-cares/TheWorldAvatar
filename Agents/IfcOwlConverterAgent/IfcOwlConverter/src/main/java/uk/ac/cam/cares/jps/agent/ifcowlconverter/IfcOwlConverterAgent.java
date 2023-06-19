package uk.ac.cam.cares.jps.agent.ifcowlconverter;

import be.ugent.IfcSpfReader;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.*;

/**
 * A converter agent that converts the IFC model into a TTL triples with the IfcOwl ontology as well as list of TTL file outputs.
 *
 * @author qhouyee
 */
@WebServlet(urlPatterns = {"/"})
public class IfcOwlConverterAgent extends JPSAgent {
    private String baseURI= "http://www.theworldavatar.com/ifc/resources_" + UUID.randomUUID() + "/";
    private String dirPath = Paths.get(System.getProperty("user.dir"),"data").toString();
    private static final String KEY_BASEURI = "uri";

    private static final Logger LOGGER = LogManager.getLogger(IfcOwlConverterAgent.class);

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        return processRequestParameters(requestParams);
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        JSONObject jsonMessage = new JSONObject();
        if (validateInput(requestParams)) {
            // Check if we should overwrite the base uri based on request
            if (!requestParams.getString(KEY_BASEURI).equals("default")){
                setBaseURI(requestParams.getString(KEY_BASEURI));
                LOGGER.debug("Detected new URI. Default Base URI will be overwritten.");
            }
            jsonMessage = this.runAgent();
            LOGGER.info("IfcOwl conversion is successfully completed!");
            jsonMessage.accumulate("Result", "IfcOwl conversion is successfully completed!");
        } else {
            LOGGER.fatal("Request parameters for the IfcOwlConverter are not defined correctly.");
            jsonMessage.put("Result", "Request parameters for the IfcOwlConverter are not defined correctly.");
        }
        return jsonMessage;
    }

    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        boolean validate = false;
        if (requestParams.has(KEY_BASEURI)) {
            String baseURI = requestParams.getString(KEY_BASEURI);
            // Base URI passed must either be default or a valid URL (starts with http/https and ends with / or #)
            validate = baseURI.equals("default") ||
                    (baseURI.startsWith("http://www.") || baseURI.startsWith("https://www.")) && (baseURI.endsWith("/") || baseURI.endsWith("#"));
        }
        return validate;
    }

    /**
     * Runs the agent logic to convert the IFC file into a IfcOwl TTL file using the IFC2RDF library.
     * The difference in jena arq versions necessitates a separate agent.
     *
     */
    protected JSONObject runAgent() {
        JSONObject jsonMessage = new JSONObject();
        String[] args = {"--baseURI", this.getBaseURI(), "--dir", this.dirPath};
        LOGGER.info("Executing conversion to IfcOwl instances...");
        try {
            IfcSpfReader.main(args);
        } catch (IOException e) {
            throw new JPSRuntimeException("Conversion to IfcOwl is unsuccessful. " +e.getMessage());
        } catch(NullPointerException e){
            throw new JPSRuntimeException("No IFC file is detected in the directory: " + this.dirPath);
        }
        return jsonMessage;
    }

    public String getBaseURI() {
        return baseURI;
    }

    public void setBaseURI(String baseURI) {
        this.baseURI = baseURI;
    }
}
