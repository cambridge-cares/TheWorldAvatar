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
    private static final Prefix P_DERIV = SparqlBuilder.prefix("saref", iri("https://www.theworldavatar.com/kg/ontoderivation/"));
    private static final Prefix P_AGENT = SparqlBuilder.prefix("ontoagent",iri("https://www.theworldavatar.com/kg/ontoagent/"));
    private static final Prefix P_OM = SparqlBuilder.prefix("om", iri("http://www.ontology-of-units-of-measure.org/resource/om-2/"));

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
    private static Map<String, Iri> SensorComp_UUID_Map = new HashMap<>();
    private static Map<String, String> SensorCompToSensorMap = new HashMap<>();

    private static Map<String, Iri> Measurement_UUID_Map; //Can one sensor have more than one measurement type?
    private static Map<String, List<Iri>> DerivedToRawMap = new HashMap<>();
    private static Map<String, Iri> Derived_UUID_Map = new HashMap<>();
    private static Map<String, Iri> DerivedType_UUID_Map = new HashMap<>();
    private static Map<String, Iri> DerivationWithTimeSeries_UUID_Map = new HashMap<>();
    private static Map<String, String> DerivedToUnitMap;

    //TODO Implement iterator(?) for Composition sensor and measurement units
    //TODO This includes changing the tmeplate .json file, add unts, unitsIRI, sensorType, sensorTypeIRI

    private static Map<String, String> SensorTypeMap;
    private static Map<String, Iri> SensorTypeIRIMap;
    
    private static Map<String, Iri> MeasurementTypeMap;
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
    private static final Iri belongsTo = P_DERIV.iri("belongsTo");
    private static final Iri isDerivedUsing = P_DERIV.iri("isDerivedUsing");
    private static final Iri isDerivedFrom = P_DERIV.iri("isDerivedFrom");
    private static final Iri hasUnit = P_OM.iri("hasUnit");
    private static final Iri symbol = P_OM.iri("symbol");
    
    //LOGGER
    private static final Logger LOGGER = LogManager.getLogger(DevInstAgentLauncher.class);
    
    
    //Methods
    public DevInstQueryBuilder (RemoteStoreClient storeClient) {
        this.storeClient = storeClient;
    }

    void InsertDevice(JSONObject desc) {
        //Get data from descriptor
        parseJSON(desc);

        //Write query & Update DB
        InstantiateDevices();
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
                SensorCompToSensorMap.put(SensorName, MainSensorName);

                //String Sensor_UUID_String = Sensor.getString("name");
                //TODO Check IRIMAp first, then if its null, default to oontodevice
                //Iri Sensor_UUID = P_DEV.iri(Sensor_UUID_String);
                Iri Sensor_UUID = P_DEV.iri(genUUID(SensorName));
                SensorComp_UUID_Map.put(SensorName, Sensor_UUID);
                
                String Measurement_UUID_String = Sensor.getJSONObject("output").getString("fieldname");
                Iri Measurement_UUID = P_DEV.iri(genUUID(Measurement_UUID_String));
                Measurement_UUID_Map.put(SensorName, Measurement_UUID);

                String MeasurementType_UUID_String = Sensor.getJSONObject("output").getString("type");
                Iri MeasurementType_UUID = iri(IRIMap.getString(MeasurementType_UUID_String));
                MeasurementTypeMap.put(SensorName, MeasurementType_UUID);

                String SensorCompType_String = Sensor.getString("type");
                //TODO Check IRIMAp first, then if its null, default to oontodevice
                SensorTypeMap.put(SensorName, SensorCompType_String);
                Iri SensorType_IRI = iri(IRIMap.getString(SensorCompType_String));
                SensorTypeIRIMap.put(SensorCompType_String, SensorType_IRI);

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

            //Iri derivVar_IRI = P_DEV.iri(genUUID(derivVarKey));
            String DerivedIRI_String = deriv.getString("IRI");
            Iri derivVar_IRI = iri(DerivedIRI_String);
            Derived_UUID_Map.put(derivVarKey, derivVar_IRI);

            //Takes in SensorComp names instead as its 1:1 correspondene with the measured var.
            //This is to take into account of multiple sensorComp with the same measured var name.
            JSONArray derivFromArray = deriv.getJSONArray("derivedFromSensor");
            List<Iri> derivFromList = new ArrayList<Iri>();
            for(int i = 0; i < derivFromArray.length(); i ++){
                String varName = derivFromArray.getString(i);
                Iri measurementIRI = Measurement_UUID_Map.get(varName);
                derivFromList.add(measurementIRI);
                

            }
            DerivedToRawMap.put(derivVarKey, derivFromList);


            String derivVarType= deriv.getString("type");
            Iri derivVarType_IRI= iri(IRIMap.getString(derivVarType));
            DerivedType_UUID_Map.put(derivVarKey, derivVarType_IRI);

            String derivVarUnit= deriv.getString("unit");
            DerivedToUnitMap.put(derivVarKey, derivVarUnit);
            Iri unit_IRI = iri(IRIMap.getString(derivVarUnit));
            UnitIRIMap.put(derivVarUnit, unit_IRI);

            String derivationWithTimeSeries = derivVarKey + "DerivationWithTimeSeries";
            String derivationWithTimeSeries_UUID_String = genUUID(derivationWithTimeSeries);
            Iri derivationWithTimeSeries_UUID = P_DEV.iri(derivationWithTimeSeries_UUID_String);
            DerivationWithTimeSeries_UUID_Map.put(derivVarKey, derivationWithTimeSeries_UUID);

        }




    }

    //TODO Make UUID generator method
    //TODO Add UUID to IRI strings

    String genUUID(String name){

        return name + UUID.randomUUID();
    }

    void InstantiateDevices() {
        ModifyQuery modify = Queries.MODIFY();
        modify.prefix(P_AGENT, P_DERIV, P_DEV, P_OM, P_SAREF);

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

        for (String DerivedName : Derived_UUID_Map.keySet()){
            Iri Derivation_UUID = Derived_UUID_Map.get(DerivedName);
            Iri DerivationTypeIRI = DerivedType_UUID_Map.get(DerivedName);
            modify.insert(Derivation_UUID.isA(DerivationTypeIRI));

            Iri DerivationWithTimeSeries_UUID = DerivationWithTimeSeries_UUID_Map.get(DerivedName);
            modify.insert(DerivationWithTimeSeries_UUID.isA(DerivationWithTimeSeries));
        }

        //TODO Add agent instantiation

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

        //Connect the Derived state instances

        for(String derivedKey: Derived_UUID_Map.keySet()){
            Iri DerivedIRI = Derived_UUID_Map.get(derivedKey);
            Iri DerivationWithTimeSeries_UUID = DerivationWithTimeSeries_UUID_Map.get(derivedKey);
            modify.insert(DerivedIRI.has(belongsTo, DerivationWithTimeSeries_UUID));

            List<Iri> RawList = DerivedToRawMap.get(derivedKey);
            for (Iri rawIRI : RawList) {
                modify.insert(DerivedIRI.has(isDerivedFrom, rawIRI));
            }

        }
        


    }


}
