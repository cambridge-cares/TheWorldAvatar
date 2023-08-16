package uk.ac.cam.cares.jps.agent.assetmanager;

import org.json.JSONException;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.*;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * A class for instantiating, retrieving and printing QR code for assets in CARES.
 * Will be used for the asset manager app.
 */
@WebServlet(urlPatterns = {"/retrieve", "/instantiate", "print"})
public class AssetManagerAgent extends JPSAgent{
    /**
     * Global params
     */
    //Request params
    String requestURL;
    public final String KEY_AGENTPROPERTIES = "AGENTPROPERTIES";
    public final String KEY_EXCELFILE = "EXCELFILE";
    public final String KEY_IRIMAPFILE = "IRIMAPFILE";
    public final String KEY_FOLDERqr = "FOLDERQR";

    //Properties params
    public String ENDPOINT_KG_ASSET, ENDPOINT_KG_DEVICE, ENDPOINT_KG_PURCHASEDOC;
    public String ENDPOINT_PRINTER;

    /*
     * ontology enums
     */
    public enum OntologyList {
        ONTODEV,
        ONTOLAB,
        ONTOSYSTEM,
        ONTOINMA,
        ONTOEPE,
        ONTOASSET
    }

    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(AssetManagerAgent.class);

    //Clients and util classes
    private static RemoteStoreClient storeClient;
    public QRPrinter printerHandler;
    public AssetKGInterface instanceHandler;
    
    //hanlde request
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        requestURL = request.getRequestURL().toString();

        return getRequestParameters(requestParams, request.getServletPath());
    }


    public JSONObject getRequestParameters(JSONObject requestParams, String urlPath) {
        JSONObject jsonMessage = new JSONObject();
        if (validateInput(requestParams, urlPath)) {
            LOGGER.info("Passing request to Asset Manager Agent..");
            String agentProperties = System.getenv(requestParams.getString(KEY_AGENTPROPERTIES));
            String excelFile = System.getenv(requestParams.getString(KEY_EXCELFILE));
            String iriMapperFile = System.getenv(requestParams.getString(KEY_IRIMAPFILE));
            String FOLDER_QR = System.getenv(requestParams.getString(KEY_FOLDERqr));

            try {
                readPropFile(agentProperties);
            } catch (Exception e) {
                throw new JPSRuntimeException("Failed to read agent.properties file: ", e);
            }
            

            String[] args = new String[] {ENDPOINT_KG_ASSET, ENDPOINT_KG_DEVICE, ENDPOINT_KG_PURCHASEDOC, ENDPOINT_PRINTER, FOLDER_QR, excelFile, iriMapperFile};
            try {
                printerHandler = new QRPrinter(ENDPOINT_PRINTER, FOLDER_QR);
            } catch (Exception e) {
                throw new JPSRuntimeException("Failed to create QR code printer handler", e);
            }
            

            if (urlPath.contains("retrieve")){
                String ID = requestParams.getString("ID");
                jsonMessage = retrieveAssetInstance(args, ID);
            }
            else if (urlPath.contains("instantiate")){
                jsonMessage = instantiateAsset(args);
            }
            else if (urlPath.contains("print")){
                String ID = requestParams.getString("ID");
                jsonMessage = contactPrintServer(args, ID);
            }

            jsonMessage.accumulate("Result", "Command Success");
            requestParams = jsonMessage;
        }
        else {
            jsonMessage.put("Result", "Request parameters are not defined correctly.");
            requestParams = jsonMessage;
        }
        return requestParams;
    }

    //Read properties file
    private void readPropFile (String propFile) throws IOException{
        try (InputStream input = new FileInputStream(propFile)) {
            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);

            try {
                // Read the mappings folder from the properties file
                ENDPOINT_KG_ASSET = prop.getProperty("endpoint.kg.asset");
                ENDPOINT_KG_DEVICE= prop.getProperty("endpoint.kg.device");
                ENDPOINT_KG_PURCHASEDOC  = prop.getProperty("endpoint.kg.purchasedocs");
                ENDPOINT_PRINTER = prop.getProperty("endpoint.printer");
            }
            catch (Exception e) {
                throw new IOException ("The endpoint keys cannot be retrieved from the properties file: ", e);
            }
            

        }
        catch (Exception e) {
            throw new JPSRuntimeException("Failed to read properties file: ", e);
        }
    }

    public boolean validateInput(JSONObject requestParams, String pathURL) throws BadRequestException {
        boolean validate = true;
        String agentProperties;

        if (requestParams.isEmpty()) {
            validate = false;
        }
        else {
            validate = requestParams.has(KEY_AGENTPROPERTIES);
            if (validate == true) {
                validate = requestParams.has(KEY_EXCELFILE);
            }
            if (validate == true) {
                validate = requestParams.has(KEY_IRIMAPFILE);
            }
            if (validate == true) {
                validate = requestParams.has(KEY_IRIMAPFILE);
            }
            if (validate == true) {
                agentProperties = (requestParams.getString(KEY_AGENTPROPERTIES));
                
                if (System.getenv(agentProperties) == null) {
                    validate = false; 
                }

                if (pathURL.contains("retrieve") || pathURL.contains("print")) {
                    validate = requestParams.has("ID");
                }
           
            }

        }
        return validate;
    }

    //hanlde instantiate
    public JSONObject instantiateAsset (String[] arg) {
        JSONObject message = new JSONObject();
        //create IRI if not exist or need update
        //send to asset inst
        return message;
    }

    //Create IRI
    //record IRI

    //handle print
    public JSONObject contactPrintServer (String[] arg, String ID){
        JSONObject message = new JSONObject();
        message.put("Result", "Printing job initiated for ID: " + ID);
        // get IRI from ID
        
        // get QR code
        // send to server
        return message;
    }

    
    
    //retrieve
    public JSONObject retrieveAssetInstance (String[] arg, String ID){
        JSONObject message = new JSONObject();
        // create query  
        // display result
        return message;
    }



}