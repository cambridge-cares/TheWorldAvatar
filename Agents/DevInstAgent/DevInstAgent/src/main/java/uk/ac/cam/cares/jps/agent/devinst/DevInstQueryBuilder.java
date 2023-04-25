package uk.ac.cam.cares.jps.agent.devinst;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.PropertyPaths;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.model.vocabulary.RDFS;

import org.json.JSONArray;
import org.json.JSONObject;
import java.io.InputStream;
import java.util.*;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class DevInstQueryBuilder {
    private RemoteStoreClient storeClient;
    
    // prefix
	private static final String ONTODEV = "https://www.theworldavatar.com/kg/ontodevice/";
    private static final Prefix P_DEV = SparqlBuilder.prefix("ontodevice",iri(ONTODEV));
    private static final Prefix P_SAREF = SparqlBuilder.prefix("saref", iri("https://saref.etsi.org/core/"));
    private static final Prefix P_TS = SparqlBuilder.prefix("ontotimeseries", iri("https://www.theworldavatar.com/kg/ontotimeseries/"));
    private static final Prefix P_DERIV = SparqlBuilder.prefix("saref", iri("https://www.theworldavatar.com/kg/ontoderivation/"));
    private static final Prefix P_AGENT = SparqlBuilder.prefix("ontoagent",iri("https://www.theworldavatar.com/kg/ontoagent/"));

    //classes
    /*
     * Each smart sensor is composed of 1 microcontrller
     * Each microcontroller can be composed of several sensor
     * Each sensor can be a collective of several sensor (Composition Sensor)
     * (eg. a temperature sensor composed of several thermometer)
     * Each Composition Sensor measures 1 raw reading type (a thermometer only measures temperture)
     * Each reading can be derived to other variable.
     *  - several types of readings can be derived to 1 variable
     *  - several derived variable can be derived from 1 reading type
     */
    private static String sensLabel;
    private static Iri SmartSensor_UUID;
    private static Iri MicroController_UUID;
    private static Map<String, Iri> Sensor_UUID_Map = new HashMap<>();
    private static Map<String, Iri> SensorComp_UUID_Map = new HashMap<>(); //Is This neccesarry?
    

    private static Map<String, Iri> Measurement_UUID_Map; //Can one sensor have more than one measurement type?
    private static Map<String, List<String>> DerivedToRawMap = new HashMap<>();
    private static Map<String, Iri> Derived_UUID_Map = new HashMap<>();

    //TODO Implement iterator(?) for Composition sensor and measurement units
    //TODO This includes changing the tmeplate .json file, add unts, unitsIRI, sensorType, sensorTypeIRI

    private static Map<String, String> SensorTypeMap;
    private static Map<String, Iri> SensorTypeIRIMap;
    
    private static Map<String, String> MeasurementToUnitMap;
    private static Map<String, Iri> UnitIRIMap;

    private static final Iri SmartSensor = P_DEV.iri("SmartSensor");
    private static final Iri MicroController = P_DEV.iri("MicroController");
    private static final Iri Sensor = P_SAREF.iri("Sensor");
    private static final Iri Service = P_AGENT.iri("Service");
    private static final Iri DerivationWithTimeSeries = P_DERIV.iri("DerivationWithTimeSeries");

    //properties
    private static final Iri consistsOf = P_SAREF.iri("consistsOf");
    private static final Iri sendsSignalTo = P_DEV.iri("sendsSignalTo");
    private static final Iri measures = P_DEV.iri("measures");
    private static final Iri hasTimeSeries = P_TS.iri("hasTimeSeries");
    private static final Iri belongsTo = P_DERIV.iri("belongsTo");
    private static final Iri isDerivedUsing = P_DERIV.iri("isDerivedUsing");
    private static final Iri isDerivedFrom = P_DERIV.iri("isDerivedFrom");
    
    //LOGGER
    private static final Logger LOGGER = LogManager.getLogger(DevInstAgentLauncher.class);
    
    
    //Methods
    public DevInstQueryBuilder (RemoteStoreClient storeClient) {
        this.storeClient = storeClient;
    }

    void InsertDevice(JSONObject desc) {
        //Get data from descriptor
        parseJSON(desc);

        //Write query

        //Update DB
        
    }

    void parseJSON (JSONObject desc) {
        JSONObject MicroController = desc.getJSONObject("MicroController");
        JSONObject IRIMap = desc.getJSONObject("IRIMapper");
        
        //smartSensor data
        sensLabel = MicroController.getString("label");
        
        String SmartSensor_UUID_String = genUUID(IRIMap.getString(MicroController.getString("name")));
        SmartSensor_UUID = P_DEV.iri(SmartSensor_UUID_String);

        String MicroController_UUID_String = genUUID(IRIMap.getString(MicroController.getString("type")));
        MicroController_UUID = P_DEV.iri(MicroController_UUID_String);

        JSONObject MainSensorMap = desc.getJSONObject("MainSensorMap");
        for (String MainSensorName : MainSensorMap.keySet()) {
            String MainSensor_UUID_String = genUUID(MainSensorName);
            Iri MainSensor_UUID = P_DEV.iri(MainSensor_UUID_String);
            Sensor_UUID_Map.put(MainSensorName, MainSensor_UUID);

            JSONObject MainSensor = MainSensorMap.getJSONObject(MainSensorName);
            for(String SensorName: MainSensor.keySet()){
                JSONObject Sensor = MainSensor.getJSONObject(SensorName);

                String Sensor_UUID_String = Sensor.getString("name");
                //TODO Check IRIMAp first, then if its null, default to oontodevice
                //Iri Sensor_UUID = P_DEV.iri(Sensor_UUID_String);
                Iri Sensor_UUID = P_DEV.iri(genUUID(Sensor_UUID_String));
                SensorComp_UUID_Map.put(Sensor.getString("name"), Sensor_UUID);
                
                String Measurement_UUID_String = genUUID(Sensor.getJSONObject("output").getString("fieldname"));
                Iri Measurement_UUID = P_DEV.iri(Measurement_UUID_String);
                Measurement_UUID_Map.put(Measurement_UUID_String, Measurement_UUID);

                String SensorCompType_String = Sensor.getString("type");
                //TODO Check IRIMAp first, then if its null, default to oontodevice
                SensorTypeMap.put(Sensor.getString("name"), SensorCompType_String);
                Iri SensorType_IRI = iri(IRIMap.getString(Sensor.getString("type")));
                SensorTypeIRIMap.put(Sensor.getString("name"), SensorType_IRI);

                String unit = Sensor.getJSONObject("output").getString("unit");
                MeasurementToUnitMap.put(Measurement_UUID_String, unit);
                //TODO Check IRIMAp first, then if its null, default to oontodevice OR OM
                Iri unit_IRI = iri(IRIMap.getString(unit));
                UnitIRIMap.put(unit, unit_IRI);

            }
        }

        JSONObject Derivation = desc.getJSONObject("derivation");
        for(String derivVarKey : Derivation.keySet()){
            JSONObject deriv = Derivation.getJSONObject(derivVarKey);

            Iri derivVar_IRI = iri(IRIMap.getString(derivVarKey));
            Derived_UUID_Map.put(derivVarKey, derivVar_IRI);

            JSONArray derivFromArray = deriv.getJSONArray("derivedFrom");
            List<String> derivFromList = new ArrayList<String>();
            for(int i = 0; i < derivFromArray.length(); i ++){
                String varName = derivFromArray.getString(i);
                derivFromList.add(varName);
                

            }
            DerivedToRawMap.put(derivVarKey, derivFromList);

        }




    }

    //TODO Make UUID generator method
    //TODO Add UUID to IRI strings

    String genUUID(String name){

        return UUID.randomUUID() + name;
    }






}
