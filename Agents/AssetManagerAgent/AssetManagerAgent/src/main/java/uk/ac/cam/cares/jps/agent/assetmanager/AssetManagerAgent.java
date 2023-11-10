package uk.ac.cam.cares.jps.agent.assetmanager;

import org.json.JSONArray;
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
    "/addmaintenance",
    "/print", 
    "/printbulk",
    "/addmanual",
    "/delete"
})
public class AssetManagerAgent extends JPSAgent{
    /**
     * Global params
     */
    //Request params
    String requestURL;
    public final String KEY_AGENTPROPERTIES = "AGENTPROPERTIES";
    public final String KEY_ONTOMAPPROPERTIES = "ONTOMAPPROPERTIES";
    public final String KEY_FOLDERqr = "FOLDERQR";
    public final String KEY_FOLDERmanual = "FOLDERMANUAL";
    public final String KEY_TSSEARCH = "TSSEARCH";

    //Properties params
    public String ENDPOINT_KG_ASSET, ENDPOINT_KG_OFFICE, ENDPOINT_KG_PURCHASEDOC, ENDPOINT_KG_LAB, ENDPOINT_KG_BMS;
    public String ENDPOINT_PRINTER;
    public Double TARGET_QR_SIZE;
    public String URL_MANUAL;
    public String KG_USERNAME, KG_PASSWORD;

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
            String ontoMapProperties = System.getenv(KEY_ONTOMAPPROPERTIES);
            String FOLDER_QR = System.getenv(KEY_FOLDERqr);
            String FOLDER_MANUAL = System.getenv(KEY_FOLDERmanual);
            JSONObject assetData = requestParams.getJSONObject("assetData");

            try {
                readPropFile(agentProperties);
            } catch (Exception e) {
                throw new JPSRuntimeException("Failed to read agent.properties file: ", e);
            }

            String[] args = new String[] {
                ENDPOINT_KG_ASSET, //0
                ENDPOINT_KG_OFFICE, //1
                ENDPOINT_KG_LAB, //2
                ENDPOINT_KG_PURCHASEDOC, //3 
                ENDPOINT_PRINTER,  //4
                FOLDER_QR,  //5
                FOLDER_MANUAL, //6
                URL_MANUAL, //7
                ENDPOINT_KG_BMS //8
            };
            
            try {
                printerHandler = new QRPrinter(ENDPOINT_PRINTER, FOLDER_QR, TARGET_QR_SIZE);
            } catch (Exception e) {
                throw new JPSRuntimeException("Failed to create QR code printer handler", e);
            }
            
            try{
                if (KG_USERNAME.isBlank() && KG_PASSWORD.isBlank()){
                    instanceHandler = new AssetKGInterface(ENDPOINT_KG_ASSET, ENDPOINT_KG_OFFICE, ENDPOINT_KG_PURCHASEDOC, ENDPOINT_KG_LAB);
                }
                else if(!(KG_USERNAME.isBlank() && KG_PASSWORD.isBlank())){
                    instanceHandler = new AssetKGInterface(ENDPOINT_KG_ASSET, ENDPOINT_KG_OFFICE, ENDPOINT_KG_PURCHASEDOC, ENDPOINT_KG_LAB, KG_USERNAME, KG_PASSWORD);
                }
                else{
                    throw new JPSRuntimeException("Either username or password for blazegraph authentication is empty."+
                    " Please ensure eiher both field to be filled when auth is needed or both are empty when not.");
                }
                
            } catch(Exception e) {

            }
            
            if (urlPath.contains("retrievebydocs")){
                JSONObject docsIRI = assetData.getJSONObject("ID");
                jsonMessage = getItemsByDocs(docsIRI);
            }
            else if (urlPath.contains("retrieve")){
                String ID = assetData.getString("ID");
                String dbName = "";
                JSONArray pred = new JSONArray();
                int searchDepth = -1;
                if (assetData.has("dbName")){
                    dbName = assetData.getString("dbName");
                }
                if(assetData.has("checkedPredicates")){
                    pred = assetData.getJSONArray("checkedPredicates");
                }
                if(assetData.has("searchDepth")){
                    searchDepth = assetData.getInt("searchDepth");
                }
                jsonMessage = retrieveAssetInstance(args, ID, dbName, pred, searchDepth);
            }

            else if (urlPath.contains("getuidata")){
                jsonMessage = getDataForUI();
            }
            else if (urlPath.contains("instantiate")){
                if(assetData.getString("Prefix").isBlank() || assetData.getString("Prefix") == null){
                    try (InputStream input = new FileInputStream(ontoMapProperties)) {
                        // Load properties file from specified path
                        Properties prop = new Properties();
                        prop.load(input);

                        try {
                            String AssetClass = assetData.getString("AssetClass");
                            assetData.put("Prefix", prop.getProperty(AssetClass));

                        }
                        catch (Exception e) {
                            throw new IOException ("The asset class keys cannot be retrieved from the properties file: ", e);
                        }
                        

                    }
                    catch (Exception e) {
                        throw new JPSRuntimeException("Failed to read properties file: ", e);
                    }
                }
                jsonMessage = instantiateAsset(args, assetData);
            }
            else if (urlPath.contains("addmanual")){
                jsonMessage = addManual(args, assetData.getString("targetID"), assetData.getString("comments"), assetData.getString("documentType"), assetData.getString("encoded"), assetData.getString("fileName"));
            }
            
            else if (urlPath.contains("printbulk")){
                String[] IRI = assetData.getJSONArray("IRI").toList().toArray(new String[0]);
                jsonMessage = contactPrintServer(args, IRI);
            }
            else if (urlPath.contains("print")){
                String IRI = assetData.getString("IRI");
                jsonMessage = contactPrintServer(args, IRI);
            }
            else if(urlPath.contains("addmaintenance")){
                jsonMessage = addMantainanceData(args, assetData);
            }
            else if(urlPath.contains("delete")){
                jsonMessage = deleteAsset(args, assetData.getString("ID"));
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
                ENDPOINT_KG_OFFICE= prop.getProperty("endpoint.kg.office");
                ENDPOINT_KG_LAB= prop.getProperty("endpoint.kg.lab");
                ENDPOINT_KG_PURCHASEDOC  = prop.getProperty("endpoint.kg.purchasedocs");
                ENDPOINT_KG_BMS = prop.getProperty("endpoint.kg.bms");
                KG_USERNAME = prop.getProperty("auth.kg.user");
                KG_PASSWORD = prop.getProperty("auth.kg.pass");
                ENDPOINT_PRINTER = prop.getProperty("endpoint.printer");
                TARGET_QR_SIZE = Double.parseDouble(prop.getProperty("target_qr_size"));
                URL_MANUAL = prop.getProperty("url.manual");
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

        if (pathURL.contains("getuidata")){return true;} //Needed no body, do no validation
        

        validate = requestParams.has("assetData");
        if(validate){
            if (pathURL.contains("retrieve")) {
                validate = requestParams.getJSONObject("assetData").has("ID");
            }
            if (pathURL.contains("print")) {
                validate = requestParams.getJSONObject("assetData").has("IRI");
            }
            if (pathURL.contains("addmanual")) {
                validate = requestParams.getJSONObject("assetData").has("encoded");
                validate = requestParams.getJSONObject("assetData").has("fileName");
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

    public JSONObject addMantainanceData (String[] arg, JSONObject assetData){
        JSONObject message = new JSONObject();
        if (validateMaintenanceData(assetData)){
            instanceHandler.addMaintenanceData(assetData);
        }
        else{
            message.accumulate("Result", "Instantiation failed: " + 
                "Maintenance data is invalid. "+
                "Input requires minimum of ID (yyyy-mm-dd/id), service provider, and last OR next service date"
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

    private Boolean validateMaintenanceData (JSONObject data) {
        if (!data.has("ID")){
            return false;
        }
        if (!data.has("LastService") || !data.has("NextService")){
            return false;
        }
        if(!data.has("ServiceProvider")){
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
        return contactPrintServer(arg, iriArray);
    }

    public JSONObject contactPrintServer (String[] arg, String[] IDArray){
        JSONObject message = new JSONObject();
        Map<String, String> idIRImap = new HashMap<String, String>();
        message.put("Result", "Bulk printing job initiated for ID: " + IDArray);
        for (String ID : IDArray){
            String IRI;
            if(validateID(ID)){
                IRI = instanceHandler.existenceChecker.getIRIStringbyID(ID);
            }else{
                IRI = ID;
                ID = instanceHandler.existenceChecker.getIDbyIRIString(IRI);
            }
            idIRImap.put(ID, IRI);
        }
        try{
            printerHandler.PrintQRBulk(idIRImap);
        }
        catch(Exception e) {
            message.accumulate("Result", "Failed to print qr codes: " + e);
        }
        return message;
    }

    Boolean validateID (String id) {
        String[] idSplits = id.split("/");
        if (idSplits.length!=2){
            return false;
        }
        if (idSplits[0].length()!=8 && idSplits[0].split("-").length!=3){
            return false;
        }
        return true;
    }

    public JSONObject getDataForUI (){
        JSONObject message = new JSONObject();
        message.accumulate("result", instanceHandler.getRequiredIriUI());
        try {
            message.getJSONObject("result").put("Type", getAssetClass());
        } catch (Exception e) {
            throw new JPSRuntimeException("Failed to add asset class type: ", e);
        }
        
        return message;
    }

    private JSONArray getAssetClass() throws IOException{
        String propFile = System.getenv(KEY_ONTOMAPPROPERTIES);
        JSONArray result = new JSONArray();
        try (InputStream input = new FileInputStream(propFile)) {
            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);

            try {
                // Read the mappings folder from the properties file
                result = new JSONArray(Collections.list((prop.keys())));
            }
            catch (Exception e) {
                throw new IOException ("The endpoint keys cannot be retrieved from the properties file: ", e);
            }
            

        }
        catch (Exception e) {
            throw new JPSRuntimeException("Failed to read properties file: ", e);
        }

        return result;
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
    public JSONObject retrieveAssetInstance (String[] arg, String ID, String dbName, JSONArray pred, int searchDepth){
        JSONObject message = new JSONObject();
        String IRI;
        String db;
        JSONArray checkedPred = pred;
        int depth = searchDepth;
        Boolean isBMSdevice = false;

        if (depth < 0){
            depth = getMaxDepth();
        }

        try {
            if(validateID(ID)){
                IRI = instanceHandler.existenceChecker.getIRIStringbyID(ID);
            }else{
                IRI = ID;
                ID = instanceHandler.existenceChecker.getIDbyIRIString(IRI);
            }

            //When ID is not there, the asset is possibly a BMS device, check for lab namespace
            if (IRI == null || ID == null){
                if (IRI == null) {
                    ID = instanceHandler.existenceChecker.getLabelbyIRIString(IRI);
                }
                else if (ID == null) {
                    IRI = instanceHandler.existenceChecker.getIRIbyLabelString(ID);
                }
                isBMSdevice = true;
            }
            
            //If still null, then item doesn't exist
            if (ID == null || IRI == null) {throw new JPSRuntimeException("ID/IRI not detected in the asset KG.");}
        } catch (Exception e) {
            throw new JPSRuntimeException("Failed to get ID/IRI. ID has to be a valid ID or an IRI.", e);
        }
        
        if (dbName.isBlank()){
            /*
            String[] splits = IRI.split("/");
            db = splits[splits.length - 2]; //DB name is always the second last, before the UUID
            //Assumes the endpoint is the same as the assetendpoint, just a different namespace
            db = arg[0].substring(0, arg[0].lastIndexOf("namespace") + 9) + "/" + db + "/sparql";
            */
            db = arg[8];
        }
        else {
            db = dbName;
        }
        
        if (checkedPred.isEmpty()){
            checkedPred = getAllCheckedPred();
        }
        message.accumulate("ID", new String[] {ID, IRI});
        if(!isBMSdevice){
            message.accumulate("Result", instanceHandler.retrieve(ID));
        }
        else{
            message.accumulate("Result", new JSONArray(new String[] {}));
        }
        
        message.accumulate("Result", instanceHandler.itemMeasuresBool(db, IRI, pred, depth));
        return message;
    }

    private JSONArray getAllCheckedPred() {
        //TODO read from prop file or something and get all checked predicate for measured
        try {
            JSONArray result = new JSONArray();
            String rawPred = getTsSearchParam("predicate");
            for (String predicate : rawPred.split(",")){
                predicate = predicate.trim();
                result.put(predicate);
            }

            return result; 

        } catch (Exception e) {
           throw new JPSRuntimeException("Failed to parse checked predicates from properties file", e);
        }
    }

    private int getMaxDepth() {
        //TODO get the maximum depth from a prop file or smth idk
        try {
            return Integer.parseInt(getTsSearchParam("depth"));    
        } catch (Exception e) {
            // TODO: handle exception
            throw new JPSRuntimeException("Failed to parse depth from properties file", e);
        }
        
    }

    private String getTsSearchParam(String key) throws IOException{
        String propFile = System.getenv(KEY_TSSEARCH);
        try (InputStream input = new FileInputStream(propFile)) {
            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);

            try {
                // Read the mappings folder from the properties file
                return prop.getProperty(key);
            }
            catch (Exception e) {
                throw new IOException ("The endpoint keys cannot be retrieved from the properties file: ", e);
            }
        }
        catch (Exception e) {
            throw new JPSRuntimeException("Failed to read properties file: ", e);
        }
    }

    public JSONObject addManual(String[] arg, String targetID, String comments, String documentType, String encoded, String fileName) {
        JSONObject message = new JSONObject();
        if(!(encoded.isBlank() || encoded == null)){
            File file = new File(arg[6]+fileName);
            LOGGER.debug("FILENAME::"+arg[6]+fileName);
            if(!file.exists()) { 
                try {
                    //file.getParentFile().mkdirs();
                    file.createNewFile();
                    LOGGER.debug("Created empty pdf...");
                } catch (Exception e) {
                    message.accumulate("Result", "Failed to create PDF:"+ e);
                }
                
            }
            try ( FileOutputStream fos = new FileOutputStream(file); ) {
                byte[] decoder = Base64.getDecoder().decode(encoded);
                fos.write(decoder);
                message.accumulate("Result", "PDF saved successfully");
            } catch (Exception e) {
                message.accumulate("Result","Failed to save PDF:"+ e);
            }
        }
        //Create manual instance
        String fileURL = arg[7] + fileName;
        instanceHandler.addDataSheet(fileURL, documentType, comments, targetID);
    
        return message;
    }

    public JSONObject deleteAsset (String[] args, String ID){
        JSONObject message = new JSONObject();
        
        String IRI;
        
        try {
            if(validateID(ID)){
                IRI = instanceHandler.existenceChecker.getIRIStringbyID(ID);
            }else{
                IRI = ID;
                ID = instanceHandler.existenceChecker.getIDbyIRIString(IRI);
            }
        } catch (Exception e) {
            throw new JPSRuntimeException("Failed to get ID/IRI. ID has to be a valid ID or an IRI.", e);
        }
        try {
            instanceHandler.delete(ID);
        } catch (Exception e) {
            throw new JPSRuntimeException("Failed to delete asset:" + ID,e);
        }
        
        return message;
    }

}