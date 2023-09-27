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
import java.io.FileOutputStream;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;

/**
 * A class for instantiating, retrieving and printing QR code for assets in CARES.
 * Will be used for the asset manager app.
 */
@WebServlet(urlPatterns = {
    "/retrieve", 
    "/retrievebydocs", 
    "/getuidata", 
    "/instantiate", 
    "/print", 
    "/printbulk",
    "/addmanualpdf"
})
public class AssetManagerAgent extends JPSAgent{
    /**
     * Global params
     */
    //Request params
    String requestURL;
    public final String KEY_AGENTPROPERTIES = "AGENTPROPERTIES";
    public final String KEY_FOLDERqr = "FOLDERQR";
    public final String KEY_FOLDERmanual = "FOLDERMANUAL";

    //Properties params
    public String ENDPOINT_KG_ASSET, ENDPOINT_KG_DEVICE, ENDPOINT_KG_PURCHASEDOC;
    public String ENDPOINT_PRINTER;
    public Double TARGET_QR_SIZE;

    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(AssetManagerAgent.class);

    //Clients and util classes
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
            String agentProperties = System.getenv(KEY_AGENTPROPERTIES);
            String FOLDER_QR = System.getenv(KEY_FOLDERqr);
            String FOLDER_MANUAL = System.getenv(KEY_FOLDERmanual);
            JSONObject assetData = requestParams.getJSONObject("assetData");

            try {
                readPropFile(agentProperties);
            } catch (Exception e) {
                throw new JPSRuntimeException("Failed to read agent.properties file: ", e);
            }
            

            String[] args = new String[] {
                ENDPOINT_KG_ASSET, 
                ENDPOINT_KG_DEVICE, 
                ENDPOINT_KG_PURCHASEDOC, 
                ENDPOINT_PRINTER, 
                FOLDER_QR, 
                FOLDER_MANUAL
            };
            
            try {
                printerHandler = new QRPrinter(ENDPOINT_PRINTER, FOLDER_QR, TARGET_QR_SIZE);
            } catch (Exception e) {
                throw new JPSRuntimeException("Failed to create QR code printer handler", e);
            }
            
            try{
                instanceHandler = new AssetKGInterface(ENDPOINT_KG_ASSET, ENDPOINT_KG_DEVICE, ENDPOINT_KG_PURCHASEDOC);
            } catch(Exception e) {

            }
            
            if (urlPath.contains("retrievebydocs")){
                JSONObject docsIRI = requestParams.getJSONObject("assetData").getJSONObject("ID");
                jsonMessage = getItemsByDocs(docsIRI);
            }
            else if (urlPath.contains("retrieve")){
                String ID = requestParams.getString("ID");
                jsonMessage = retrieveAssetInstance(args, ID);
            }

            else if (urlPath.contains("getuidata")){
                jsonMessage = getDataForUI();
            }
            else if (urlPath.contains("instantiate")){
                jsonMessage = instantiateAsset(args, assetData);
            }
            else if (urlPath.contains("addmanualpdf")){
                jsonMessage = addManualPDF(args, assetData.getString("encodedPDF"), assetData.getString("fileName"));
            }
            
            else if (urlPath.contains("printbulk")){
                String[] IRI = assetData.getJSONArray("IRI").toList().toArray(new String[0]);
                jsonMessage = contactPrintServer(args, IRI);
            }
            else if (urlPath.contains("print")){
                String IRI = assetData.getString("IRI");
                jsonMessage = contactPrintServer(args, IRI);
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
                TARGET_QR_SIZE = Double.parseDouble(prop.getProperty("target_qr_size"));
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

        if (requestParams.isEmpty()) {
            validate = false;
        }
        else {
            validate = requestParams.has("assetData");
            if (validate == true) {
                
                if (System.getenv(KEY_AGENTPROPERTIES) == null) {
                    validate = false; 
                }

                if (pathURL.contains("retrieve")) {
                    validate = requestParams.getJSONObject("assetData").has("ID");
                }
                if (pathURL.contains("print")) {
                    validate = requestParams.getJSONObject("assetData").has("IRI");
                }
           
            }

        }
        return validate;
    }

    //hanlde instantiate
    public JSONObject instantiateAsset (String[] arg, JSONObject assetData) {
        JSONObject message = new JSONObject();
        // handle input
        if (validateAssetData(assetData)){
            try {
                JSONObject iriResult = instanceHandler.instantiate(assetData);
                message.accumulate("Result", iriResult);
            } catch (Exception e) {
                message.accumulate("Result", "Instantiation failed: " + e);
            }
            
        }
        else{
            message.accumulate("Result", "Instantiation failed: " + 
                "Asset data is invalid. "+
                "Input requires minimum of ID (yyyy-mm-dd/id), Name, Location and ontology prefix and class."
            );
        }
        return message;
    }

    //validate asset data
    private Boolean validateAssetData (JSONObject data){
        if(!data.has("ID")){
            return false;
        }
        else{
            String id = data.getString("ID");
            if (!(id ==null || id.isBlank())){
                String[] idSplits = id.split("/");
                if (idSplits.length!=2){
                    return false;
                }
                if (idSplits[0].length()!=8 && idSplits[0].split("-").length!=3){
                    return false;
                }
            }
            
        }
        if (!data.has("Name")){
            return false;
        }
        if (!data.has("BuildingLocation")){
            return false;
        }
        if(!data.has("Prefix") && !data.has("AssetClass")){
            return false;
        }

        return true;
    }

    //handle print
    public JSONObject contactPrintServer (String[] arg, String IRI){
        JSONObject message = new JSONObject();
        message.put("Result", "Printing job initiated for IRI: " + IRI);
        String[] iriArray = {IRI};
        // send to server
        try{
            printerHandler.PrintQRBulk(iriArray);
        }
        catch(Exception e) {
            message.accumulate("Result", "Failed to contact printer: " + e);
        }
        
        return message;
    }

    public JSONObject contactPrintServer (String[] arg, String[] IRIArray){
        JSONObject message = new JSONObject();
        message.put("Result", "Bulk printing job initiated for ID: " + IRIArray);
        try{
            printerHandler.PrintQRBulk(IRIArray);
        }
        catch(Exception e) {
            message.accumulate("Result", "Failed to print qr codes: " + e);
        }
        return message;
    }

    public JSONObject getDataForUI (){
        JSONObject message = new JSONObject();
        message.accumulate("result", instanceHandler.getRequiredIriUI());
        return message;
    }

    public JSONObject getItemsByDocs (JSONObject docsIRI) {
        JSONObject message = new JSONObject();
        String InvoiceIRI = docsIRI.getString("invoice");
        String POiri = docsIRI.getString("PO");
        String DOiri = docsIRI.getString("DO");
        message.accumulate("Result", instanceHandler.getItemListByDocIRI(InvoiceIRI, POiri, DOiri));
        return message;
    }

    //retrieve
    public JSONObject retrieveAssetInstance (String[] arg, String ID){
        JSONObject message = new JSONObject();
        message.accumulate("Result", instanceHandler.retrieve(ID));
        return message;
    }

    public JSONObject addManualPDF(String[] arg, String encodedPDF, String fileName) {
        JSONObject message = new JSONObject();
        File file = new File(arg[5]+fileName);
        LOGGER.debug("FILENAME::"+arg[5]+fileName);
        if(!(file.exists() && !file.isDirectory())) { 
            try {
                //file.getParentFile().mkdirs();
                file.createNewFile();
            } catch (Exception e) {
                message.accumulate("Result", "Failed to create PDF:"+ e);
            }
            
        }
        try ( FileOutputStream fos = new FileOutputStream(file); ) {
            byte[] decoder = Base64.getDecoder().decode(encodedPDF);
            fos.write(decoder);
            message.accumulate("Result", "PDF saved successfully");
        } catch (Exception e) {
            message.accumulate("Result","Failed to save PDF:"+ e);
        }
    
        return message;
    }

}