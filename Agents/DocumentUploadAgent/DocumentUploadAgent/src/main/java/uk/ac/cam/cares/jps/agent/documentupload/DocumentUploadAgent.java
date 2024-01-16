package uk.ac.cam.cares.jps.agent.documentupload;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;

import java.io.*;
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
@WebServlet(urlPatterns = {
    "/adddocument"
})
public class DocumentUploadAgent extends JPSAgent{
    /**
     * Global params
     */
    //Request params
    String requestURL;
    public final String KEY_AGENTPROPERTIES = "AGENTPROPERTIES";
    


    //Properties params
    public String ENDPOINT_KG;
    public String URL_DOCS;
    public String KG_USERNAME = "", KG_PASSWORD = "";
    public String FOLDER_DOCS;

    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(DocumentUploadAgent.class);

    //Clients and util classes
    public KGInterface instanceHandler;
    
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
            String agentProperties = System.getenv(KEY_AGENTPROPERTIES);
            FOLDER_DOCS = System.getenv("FOLDERDOCS");
            ENDPOINT_KG = requestParams.getString("kgEndpoint");
            if(requestParams.has("kgUser") && requestParams.has("kgPass")){
                KG_USERNAME = requestParams.getString("kgUser");
                KG_PASSWORD = requestParams.getString("kgPass");
            }
            
            try {
                readPropFile(agentProperties);
            } catch (Exception e) {
                throw new JPSRuntimeException("Failed to read agent.properties file: ", e);
            }
            
            try{
                if (KG_USERNAME.isBlank() && KG_PASSWORD.isBlank()){
                    instanceHandler = new KGInterface(ENDPOINT_KG);
                }
                else if(!(KG_USERNAME.isBlank() && KG_PASSWORD.isBlank())){
                    instanceHandler = new KGInterface(ENDPOINT_KG, KG_USERNAME, KG_PASSWORD);
                }
                else{
                    throw new JPSRuntimeException("Either username or password for blazegraph authentication is empty."+
                    " Please ensure eiher both field to be filled when auth is needed or both are empty when not.");
                }
                
            } catch(Exception e) {
                jsonMessage.put("Result",  e.toString());
                requestParams = jsonMessage;
            }
            
             if (urlPath.contains("adddocument")){
                    if (!requestParams.has("comments")){requestParams.put("comments", "");}
                    if (!requestParams.has("documentType")){requestParams.put("comments", "");}
                if (requestParams.has("documentIRI")){
                    jsonMessage = addDocument(
                        requestParams.getString("documentIRI"), 
                        requestParams.getString("documentType"), 
                        requestParams.getString("encoded"), 
                        requestParams.getString("fileName"),
                        requestParams.getBoolean("overwrite"),
                        requestParams.getBoolean("instantiate")
                    );
                }
                else{
                    jsonMessage = addDocument(
                        requestParams.getString("documentType"), 
                        requestParams.getString("encoded"), 
                        requestParams.getString("fileName"),
                        requestParams.getBoolean("overwrite"),
                        requestParams.getBoolean("instantiate")
                    );
                }
                
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
                URL_DOCS = prop.getProperty("url.manual");
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
        
        if (System.getenv(KEY_AGENTPROPERTIES) == null) {
            return false; 
        }

        if(validate){
            if (pathURL.contains("adddocument")) {
                validate = requestParams.has("encoded");
                validate = requestParams.has("fileName");
                validate = requestParams.has("kgEndpoint");
            }
        }

        
        return validate;
    }
    public JSONObject addDocument( String documentType, String encoded, String fileName, Boolean overwrite, Boolean instantiate) {
        return addDocument("", documentType, encoded, fileName, overwrite, instantiate);
    }

    public JSONObject addDocument(String documentIRI, String documentType, String encoded, String fileName, Boolean overwrite, Boolean instantiate) {
        JSONObject message = new JSONObject();
        if(!(encoded.isBlank() || encoded == null)){
            File file = new File(FOLDER_DOCS+fileName);
            LOGGER.debug("FILENAME::"+FOLDER_DOCS+fileName);
            /*
            if(!file.exists()) { 
                try {
                    //file.getParentFile().mkdirs();
                    file.createNewFile();
                    LOGGER.debug("Created empty file...");
                } catch (Exception e) {
                    message.accumulate("Result", "Failed to create file:"+ e);
                }
                
            }
            */
            try{
                if (file.createNewFile()){
                    LOGGER.debug("Created empty file...");
                }
                else{
                    if (!overwrite){
                        throw new JPSRuntimeException("File exists. Please use a different name or set 'overwrite' to true to overwrite the file");
                        //message.accumulate("Result", "File exists. Please use a different name or set 'overwrite' to true to overwrite the file");
                    }
                }
            }
            catch(Exception e){
                message.accumulate("Result", "Failed to create file:"+ e);
                return message;
            }
            

            try ( FileOutputStream fos = new FileOutputStream(file, false); ) {
                byte[] decoder = Base64.getDecoder().decode(encoded);
                fos.write(decoder);
                message.accumulate("Result", "File saved successfully");
            } catch (Exception e) {
                message.accumulate("Result","Failed to save file:"+ e);
            }
        }
        //Create manual instance
        String fileURL = URL_DOCS + fileName;
        if (instantiate){
            if (documentIRI.isBlank() || documentIRI == null) {
                documentIRI = instanceHandler.addDocument(fileURL, documentType);
            }
            else{
                instanceHandler.addDocument(documentIRI, fileURL, documentType);
            }
        }
        
        JSONObject docData = new JSONObject();
        docData.put("documentIRI", documentIRI);
        docData.put("documentURL", fileURL);
        message.put("documentData", docData);
        return message;
    }

}