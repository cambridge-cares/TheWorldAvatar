package uk.ac.cam.cares.jps.agent.devinst;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.model.vocabulary.RDFS;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.*;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class DevInstQueryBuilder {
    private RemoteStoreClient storeClient;
    private RemoteStoreClient ontodevStoreClient;
    private RemoteStoreClient sarefStoreClient;
    private JSONObject IRIMap;
    
    // prefix
	private static final String ONTODEV = "https://www.theworldavatar.com/kg/ontodevice/";
    private static final Prefix P_DEV = SparqlBuilder.prefix("ontodevice",iri(ONTODEV));
    private static final Prefix P_SAREF = SparqlBuilder.prefix("saref", iri("https://saref.etsi.org/core/"));
    private static final Prefix P_AGENT = SparqlBuilder.prefix("ontoagent",iri("https://www.theworldavatar.com/kg/ontoagent/"));
    private static final Prefix P_OM = SparqlBuilder.prefix("om", iri("http://www.ontology-of-units-of-measure.org/resource/om-2/"));

    //classes
    /*
     * Each smart sensor is composed of 1 microcontroller
     * Each microcontroller can be composed of several sensor (main sensor)
     * Each sensor can be a collective of several sensor (Composition Sensor)
     * (eg. a temperature sensor composed of several thermometer)
     * Each Composition Sensor measures 1 raw reading type (a thermometer only measures temperture)
     */
    private static String sensLabel;
    private static Iri SmartSensor_UUID;
    private static Iri MicroController_UUID;
    private static Map<String, Iri> Sensor_UUID_Map = new HashMap<>();
    private static Map<String, Iri> SensorComp_UUID_Map = new HashMap<>();
    private static Map<String, String> SensorCompToSensorMap = new HashMap<>();

    private static Map<String, Iri> Measurement_UUID_Map = new HashMap<>();
    private static List<String> AdditionalQuery = new ArrayList<>();

    private static Map<String, String> SensorTypeMap = new HashMap<>();
    private static Map<String, Iri> SensorTypeIRIMap = new HashMap<>();
    
    private static Map<String, Iri> MeasurementTypeMap = new HashMap<>();
    private static Map<String, String> MeasurementToUnitMap = new HashMap<>();
    private static Map<String, Iri> UnitIRIMap = new HashMap<>();

    private List<String> taskArr = new ArrayList<>();
    private List<String> commandArr = new ArrayList<>();

    private static final Iri SmartSensor = P_DEV.iri("SmartSensor");
    private static final Iri MicroController = P_DEV.iri("MicroController");
    private static final Iri Sensor = P_SAREF.iri("Sensor");

    //properties
    private static final Iri consistsOf = P_SAREF.iri("consistsOf");
    private static final Iri Task = P_SAREF.iri("Task");
    private static final Iri hasCommand = P_SAREF.iri("hasCommand");
    private static final Iri Command = P_SAREF.iri("Command");
    private static final Iri accomplishes = P_SAREF.iri("accomplishes");
    private static final Iri sendsSignalTo = P_DEV.iri("sendsSignalTo");
    private static final Iri measures = P_DEV.iri("measures");
    private static final Iri hasUnit = P_OM.iri("hasUnit");
    private static final Iri symbol = P_OM.iri("symbol");
    
    //LOGGER
    private static final Logger LOGGER = LogManager.getLogger(DevInstAgent.class);
    
    
    //Methods
    public DevInstQueryBuilder (RemoteStoreClient sparqlClient, RemoteStoreClient ontodevValidator, RemoteStoreClient sarefValidator) {
        this.storeClient = sparqlClient;
        this.ontodevStoreClient = ontodevValidator;
        this.sarefStoreClient = sarefValidator;
    }

    void InsertDevice(JSONObject desc) {
        LOGGER.info("Instantiating devices...");
        //Get data from descriptor
        parseJSON(desc);

        //Write query & Update DB   
        InstantiateDevices();
        LOGGER.info("Instantiation success!");
    }

    /*
     * Parses JSON data and obtain IRI for each instances.
     * Takes in the JSON template from request
     * 
     * @param desc: The device descriptor
     */
    void parseJSON (JSONObject desc) {
        LOGGER.info("Parsing device descriptor...");
        JSONObject MicroController = desc.getJSONObject("MicroController");
        IRIMap = desc.getJSONObject("IRIMapper");
        
        try{
            //smartSensor data
            sensLabel = MicroController.getString("label");
            
            SmartSensor_UUID = getIri(MicroController.getString("name"), false);
            
            MicroController_UUID =  getIri(MicroController.getString("type"), false);

            JSONObject MainSensorMap = MicroController.getJSONObject("MainSensorMap");
            for (String MainSensorName : MainSensorMap.keySet()) {
                Iri MainSensor_UUID = getIri(MainSensorName, false);
                Sensor_UUID_Map.put(MainSensorName, MainSensor_UUID);

                JSONObject MainSensor = MainSensorMap.getJSONObject(MainSensorName);
                for(String SensorName: MainSensor.keySet()){
                    JSONObject Sensor = MainSensor.getJSONObject(SensorName);
                    SensorCompToSensorMap.put(SensorName, MainSensorName);

                    Iri Sensor_UUID = getIri(SensorName, false);
                    SensorComp_UUID_Map.put(SensorName, Sensor_UUID);
                    
                    String Measurement_ID_String = Sensor.getJSONObject("output").getString("fieldname");
                    Iri Measurement_UUID = getIri(Measurement_ID_String, false);
                    Measurement_UUID_Map.put(SensorName, Measurement_UUID);

                    String MeasurementType_ID_String = Sensor.getJSONObject("output").getString("type");
                    Iri MeasurementType_UUID = getIri(MeasurementType_ID_String, false);
                    MeasurementTypeMap.put(SensorName, MeasurementType_UUID);

                    String SensorCompType_String = Sensor.getString("type");
                    SensorTypeMap.put(SensorName, SensorCompType_String);
                    Iri SensorType_IRI = getIri(SensorCompType_String, true);
                    SensorTypeIRIMap.put(SensorCompType_String, SensorType_IRI);

                    String unit = Sensor.getJSONObject("output").getString("unit");
                    MeasurementToUnitMap.put(Measurement_ID_String, unit);
                    Iri unit_IRI = getIri(unit, false);
                    UnitIRIMap.put(unit, unit_IRI);

                }
            }

            //Additional Queries
            JSONArray additional = desc.getJSONArray("AdditionalQuery");

            for(int i = 0; i < additional.length(); i++) {
                String query = additional.getString(i);
                AdditionalQuery.add(query);
            }

            //Tasks and commands
            JSONArray sensorTask = desc.getJSONArray("Task");
            JSONArray sensorComm = desc.getJSONArray("Command");
            for (int i = 0; i < sensorTask.length(); i++){
                String task = sensorTask.getString(i);
                taskArr.add(task);
            }
            for (int i = 0; i < sensorComm.length(); i++){
                String comm = sensorComm.getString(i);
                commandArr.add(comm);
            }
        }
        catch( JSONException e) {
            throw new JPSRuntimeException("Failed to obtain IRI from IRI Mapper.", e);
        }

    }


    /*
     * Creates the update queries and send update request to triple store.
     * 
     */
    public void InstantiateDevices() {
        ModifyQuery modify = Queries.MODIFY();
        modify.prefix(P_AGENT, P_DEV, P_OM, P_SAREF);

        //Instantiate the microcontroller, sensors, variable instances

        modify.insert(SmartSensor_UUID.isA(SmartSensor));
        modify.insert(SmartSensor_UUID.has(RDFS.LABEL, Rdf.literalOf(sensLabel)));
        
        modify.insert(MicroController_UUID.isA(MicroController));
        
        for(String SensorName : Sensor_UUID_Map.keySet()){
            Iri UUID = Sensor_UUID_Map.get(SensorName);
            modify.insert(UUID.isA(Sensor));
        }

        for(String SensorCompName : SensorComp_UUID_Map.keySet()){
            Iri UUID = SensorComp_UUID_Map.get(SensorCompName);
            Iri Type_UUID = SensorTypeIRIMap.get(SensorTypeMap.get(SensorCompName));
            modify.insert(UUID.isA(Type_UUID));

            Iri Measurement_UUID = Measurement_UUID_Map.get(SensorCompName);
            Iri MeasurementType_IRI = MeasurementTypeMap.get(SensorCompName);
            modify.insert(Measurement_UUID.isA(MeasurementType_IRI));
        }

        //Connect properties
        //Connect the microcontroller and sensor instances

        modify.insert(SmartSensor_UUID.has(consistsOf, MicroController_UUID));
        for(String SensorName : Sensor_UUID_Map.keySet()){
            Iri UUID = Sensor_UUID_Map.get(SensorName);
            modify.insert(SmartSensor_UUID.has(consistsOf, UUID));
            modify.insert(UUID.has(sendsSignalTo, MicroController_UUID));
        }

        for(String SensorCompName : SensorCompToSensorMap.keySet()){
            String MainSensorName = SensorCompToSensorMap.get(SensorCompName);
            Iri MainSensor_UUID = Sensor_UUID_Map.get(MainSensorName);
            Iri CompSensor_UUID = SensorComp_UUID_Map.get(SensorCompName);

            modify.insert(MainSensor_UUID.has(consistsOf, CompSensor_UUID));

            Iri Measurement_UUID = Measurement_UUID_Map.get(SensorCompName);
            modify.insert(CompSensor_UUID.has(measures, Measurement_UUID));

        }

        storeClient.executeUpdate(modify.getQueryString());

        
        
        //Add aditional query
        if (AdditionalQuery.size() > 0){
            LOGGER.info("Executing additional queries...");
            modify = Queries.MODIFY();
            for(String query: AdditionalQuery){
                String[] splits = query.split("\\s+");
                List<Iri> listIRI = new ArrayList<>();
                for(int i=0; i < 3; i++){
                    Iri component;
                    if (splits[i].contains("IRIMapper=")){
                        String componentString = splits[i].split("=")[1];
                        if (componentString.contains(";") ){
                            String[] keywordIdPair = componentString.split(";");
                            
                            if(keywordIdPair[0].toLowerCase().equals("gen")){
                                if (keywordIdPair.length == 3){
                                    component = genIRI(keywordIdPair[1], keywordIdPair[2]);
                                }
                                else{
                                    throw new JPSRuntimeException("gen keyword in Additional query requires 3 sections separated by ';' : gen;ID;prefix.", null);
                                }
                                
                            }
                            
                            else if(keywordIdPair[0].toLowerCase().equals("find")){
                                if (keywordIdPair.length == 2){
                                    component = findIriMatch(keywordIdPair[1]);
                                }
                                else{
                                    throw new JPSRuntimeException("find keyword in Additional query requires 2 sections separated by ';' : find;ID", null);
                                }
                                
                            }
                            else {
                                throw new JPSRuntimeException("Unrecognised pattern in addtional queries: " + query, null);
                            }
                            
                        }
                        else{
                            //Check is not done here as the addtiional query concepts is not neccessarily
                            //from ontodevice or saref.
                            component = getIri(componentString, false);
                        }
                    }
                    else{
                        component = iri(splits[i]);
                    }
                    
                    listIRI.add(component);
                }
                TriplePattern triple = GraphPatterns.tp(listIRI.get(0), listIRI.get(1), listIRI.get(2));
                modify.insert(triple);
            }
            //System.out.println(modify.getQueryString());
            storeClient.executeUpdate(modify.getQueryString());
        }

        //Task and command
        if (taskArr.size() > 0) {
            taskArr.add("NotProvided");
        }
            LOGGER.info("Executing tasks queries...");
            Iri taskIRI = genIRI("Task", storeClient.getUpdateEndpoint());

            modify = Queries.MODIFY();
            modify.prefix(P_AGENT, P_DEV, P_OM, P_SAREF);
            modify.insert(SmartSensor_UUID.has(accomplishes, taskIRI));
            modify.insert(taskIRI.isA(Task));

            for (String task : taskArr){
                modify. insert(taskIRI.has(RDFS.COMMENT, Rdf.literalOf(task)));
            }

            modify.insert(MicroController_UUID.has(accomplishes, taskIRI));
            
            for(String SensorName : Sensor_UUID_Map.keySet()){
                Iri UUID = Sensor_UUID_Map.get(SensorName);
                modify.insert(UUID.has(accomplishes, taskIRI));
            }

            for(String SensorCompName : SensorComp_UUID_Map.keySet()){
                Iri UUID = SensorComp_UUID_Map.get(SensorCompName);
                modify.insert(UUID.has(accomplishes, taskIRI));
            }
            storeClient.executeUpdate(modify.getQueryString());
        

        if (commandArr.size() > 0) {
            LOGGER.info("Executing command queries...");
            Iri commandIRI = genIRI("Command", storeClient.getUpdateEndpoint());

            modify = Queries.MODIFY();
            modify.prefix(P_AGENT, P_DEV, P_OM, P_SAREF);
            modify.insert(SmartSensor_UUID.has(hasCommand, commandIRI));
            modify.insert(commandIRI.isA(Command));

            for (String command : commandArr){
                modify. insert(commandIRI.has(RDFS.COMMENT, Rdf.literalOf(command)));
            }

            modify.insert(MicroController_UUID.has(hasCommand, commandIRI));
            
            for(String SensorName : Sensor_UUID_Map.keySet()){
                Iri UUID = Sensor_UUID_Map.get(SensorName);
                modify.insert(UUID.has(hasCommand, commandIRI));
            }

            for(String SensorCompName : SensorComp_UUID_Map.keySet()){
                Iri UUID = SensorComp_UUID_Map.get(SensorCompName);
                modify.insert(UUID.has(hasCommand, commandIRI));
            }
            storeClient.executeUpdate(modify.getQueryString());
        }

    }

    /*
     * Find/generate the IRI
     * 
     * IRIMapper takes priority, if IRIMapper has IRI, use the specified IRI
     * If String "find" is provided, agent will search for possible IRI match
     * If String "gen" is provided, agent will generate a UUID
     * 
     * If IRIMapper is empty, then throw error
     * Check is done by ID in the json file
     * All IRI generated should contain the same ID name, but different UUID
     * 
     * @param ID: The ID specified in the device descriptor
     * 
     */
    private Iri getIri(String ID, Boolean doCheck) {
        if (IRIMap.has(ID)){
            String iriString = IRIMap.getString(ID);
            
            if (iriString.toLowerCase().equals("gen")){
                //Defaults to Ontodevice
                return genIRI(ID, P_DEV);
            }
            else if (iriString.toLowerCase().equals("find")){
                return findIriMatch(ID);
            }
            else {
                Boolean valid = true;
                if (doCheck){
                    valid = checkConceptExistence(iri(iriString));
                }
               if(valid) return iri(iriString);
               else{
                throw new JPSRuntimeException("Concept does not exist in SAREF or ontoDevice:" + iriString +". Please check the IRI in the IRIMapper.", null);
               }
            }
        }
        else{
            throw new JPSRuntimeException("IRI ID: " + ID +" is not in IRIMapper. Please provide either the IRI or the following keyword:\n" +
            "find : Find the IRI in the knowledge graph with the same ID. \n" + 
            "gen: Generate new IRI ending with random UUID. Based on ontodevice.\n"
            );
        }

    }

    /*
     * Find the IRI containing the same ID in the KG
     * If more than 1 is found throws error
     * If none is find, throws error
     * 
     * @param ID: The ID from the device decriptor file
     * @returns Iri : The IRI found in knowledge graph
     */
    private Iri findIriMatch (String ID){
        //TODO Currently pulls the whowl database on call. Is there a way to simplify this?
        SelectQuery query = Queries.SELECT();
        JSONArray queryResult = new JSONArray();
        List<String> result = new ArrayList<>();
        query.where(query.var().has(query.var(), query.var()) );
        try{
            queryResult = storeClient.executeQuery(query.getQueryString());
        }catch (Exception e) {
            throw new JPSRuntimeException("Failed to execute query.", e);
        }

        for(int i =0 ; i < queryResult.length(); i++){
            JSONObject row = queryResult.getJSONObject(i);
            for (String key : row.keySet()){
                String iriFound = row.getString(key);
                if (iriFound.contains(ID) && !result.contains(iriFound)){
                    result.add(iriFound);
                }
            }
        }


        if (result.size() > 1) {
            throw new JPSRuntimeException("More than 1 possible IRI match is found: " + result + ". Resolve by specifying in IRIMapper.");
        }

        if (result.size() == 0) {
            throw new JPSRuntimeException("No possible match found for ID: " + ID + ". Resolve by specifying in IRIMapper.");
        }
        
        return iri(result.get(0));

    }

    /*
     * Generate new IRI
     */
    private Iri genIRI (String ID, Prefix prefix) {
        return prefix.iri(ID + "_" + UUID.randomUUID());
    }

    private Iri genIRI (String ID, String prefix) {
        return iri(prefix + ID + "_" + UUID.randomUUID());
    }

    /*
     * Check if the specified IRI exists in the ontodevice or SAREF concepts.
     * The owl files are stored in a local server.
     */

    Boolean checkConceptExistence(Iri target) {
        SelectQuery query = Queries.SELECT();
        
        //Query for all class acting as subject or object
        //query.where(query.var().has(query.var(),target));
        query.where(target.has(query.var(), query.var()));

        //Look for class in ontodevice
        JSONArray ontodevResult = ontodevStoreClient.executeQuery(query.getQueryString());
        System.out.println(ontodevResult);
        if (ontodevResult.length() >= 1) {return true;}

        //Look for class in SAREF
        JSONArray sarefResult = sarefStoreClient.executeQuery(query.getQueryString());
        System.out.println(ontodevResult);
        if (sarefResult.length() >= 1) {return true;}

        return false; 
    }

}
