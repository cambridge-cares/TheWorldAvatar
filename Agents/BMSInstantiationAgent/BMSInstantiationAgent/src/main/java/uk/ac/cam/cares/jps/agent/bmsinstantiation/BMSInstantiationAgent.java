package uk.ac.cam.cares.jps.agent.bmsinstantiation;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.InsertDataQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Properties;
import java.util.Scanner;
import java.util.UUID;

/**
 * Class to retrieve data from CSV file and storing it with connection to The World Avatar (Knowledge Base).
 * @author */ 
public class BMSInstantiationAgent {

    /**
     * Logger for reporting info/errors.
     */

    private static final Logger LOGGER = LogManager.getLogger(BMSInstantiationAgentLauncher.class);

    public static final String OntoCarpark = "https://www.theworldavatar.com/kg/ontocarpark/";
    

    public String queryEndpoint;
    public String updateEndpoint;

    RemoteStoreClient kbClient;
    /**
     * Namespaces for ontologies
     */
    public static final String ONTODEVICE_NS = "https://www.theworldavatar.com/kg/ontodevice/";
    public static final String ONTOBMS_NS = "https://www.theworldavatar.com/kg/ontobms/";
    public static final String ONTOCAPE_CPS_NS = "http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/chemical_process_system.owl#";
    public static final String ONTOCAPE_CPS_BEHAVIOR_NS = "http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#";
    public static final String ONTOCAPE_CPS_FUNCTION_PROCESS_NS = "http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#";
    public static final String ONTOCAPE_PHASE_SYSTEM_NS = "http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#";
    public static final String ONTOCAPE_MATERIAL_NS = "http://www.theworldavatar.com/ontology/ontocape/material/material.owl#";
    public static final String ONTOCAPE_SYSTEM_NS = "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#";
    public static final String ONTOCAPE_MATERIAL_SUBSTANCE_NS = "http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#";
    public static final String ONTOCAPE_NETWORK_SYSTEM_NS = "http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#";
    public static final String ONTOSPECIES_NS = "http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#";
    public static final String RDFS_NS = "http://www.w3.org/2000/01/rdf-schema#";
    public static final String SAREF_NS = "https://saref.etsi.org/core/";
    public static final String OM_NS = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    public static final String ONTOTIMESERIES_NS = "https://www.theworldavatar.com/kg/ontotimeseries/";
    public static final String S4BLDG_NS = "https://saref.etsi.org/saref4bldg/";
    public static final String META_MODEL_MEREOLOGY_NS = "http://www.theworldavatar.com/ontology/meta_model/mereology/mereology.owl#";


    /**
     * Prefixes
     */
    private static final Prefix PREFIX_ONTODEVICE = SparqlBuilder.prefix("ontodevice", iri(ONTODEVICE_NS));
    private static final Prefix PREFIX_ONTOBMS = SparqlBuilder.prefix("ontobms", iri(ONTOBMS_NS));
    private static final Prefix PREFIX_ONTOCAPE_CPS = SparqlBuilder.prefix("ontocape_cps", iri(ONTOCAPE_CPS_NS));
    private static final Prefix PREFIX_ONTOCAPE_CPS_BEHAVIOR = SparqlBuilder.prefix("ontocape_cps_behavior", iri(ONTOCAPE_CPS_BEHAVIOR_NS));
    private static final Prefix PREFIX_ONTOCAPE_CPS_FUNCTION_PROCESS = SparqlBuilder.prefix("ontocape_cps_function", iri(ONTOCAPE_CPS_FUNCTION_PROCESS_NS));
    private static final Prefix PREFIX_ONTOCAPE_PHASE_SYSTEM = SparqlBuilder.prefix("ontocape_cps_phase_system", iri(ONTOCAPE_PHASE_SYSTEM_NS));
    private static final Prefix PREFIX_ONTOCAPE_MATERIAL = SparqlBuilder.prefix("ontocape_material", iri(ONTOCAPE_MATERIAL_NS));
    private static final Prefix PREFIX_ONTOCAPE_SYSTEM = SparqlBuilder.prefix("ontocape_system", iri(ONTOCAPE_SYSTEM_NS));
    private static final Prefix PREFIX_ONTOCAPE_MATERIAL_SUBSTANCE = SparqlBuilder.prefix("ontocape_material_substance", iri(ONTOCAPE_MATERIAL_SUBSTANCE_NS));
    private static final Prefix PREFIX_ONTOCAPE_NETWORK_SYSTEM = SparqlBuilder.prefix("ontocape_network_system", iri(ONTOCAPE_NETWORK_SYSTEM_NS));
    private static final Prefix PREFIX_ONTOSPECIES = SparqlBuilder.prefix("ontospecies", iri(ONTOSPECIES_NS));
    private static final Prefix PREFIX_RDFS = SparqlBuilder.prefix("rdfs", iri(RDFS_NS));
    private static final Prefix PREFIX_SAREF = SparqlBuilder.prefix("saref", iri(SAREF_NS));
    private static final Prefix PREFIX_OM = SparqlBuilder.prefix("om", iri(OM_NS));
    private static final Prefix PREFIX_ONTOTIMESERIES = SparqlBuilder.prefix("ontotimeseries", iri(ONTOTIMESERIES_NS));
    private static final Prefix PREFIX_S4BLDG = SparqlBuilder.prefix("s4bldg", iri(S4BLDG_NS));
    private static final Prefix PREFIX_META_MODEL_MEREOLOGY = SparqlBuilder.prefix("meta_model_mereology", iri(META_MODEL_MEREOLOGY_NS));
	/**
     * Relationships
     */ 
	private static final Iri consistsOf = PREFIX_SAREF.iri("consistsOf");
    private static final Iri hasState = PREFIX_SAREF.iri("hasState");
	private static final Iri hasValue = PREFIX_OM.iri("hasValue");
	private static final Iri observes = PREFIX_ONTODEVICE.iri("observes");
	private static final Iri measures = PREFIX_ONTODEVICE.iri("measures");
    private static final Iri hasQuantity = PREFIX_ONTODEVICE.iri("hasQuantity");
    private static final Iri hasSetpoint = PREFIX_ONTODEVICE.iri("hasSetpoint");
    private static final Iri label = PREFIX_RDFS.iri("label");
    private static final Iri sendsSignalTo = PREFIX_ONTODEVICE.iri("sendsSignalTo");
    private static final Iri hasSashOpenPercentage = PREFIX_ONTOBMS.iri("hasSashOpenPercentage");
    private static final Iri hasAirFlowRate = PREFIX_ONTOBMS.iri("hasAirFlowRate");
    private static final Iri sensesSubstance = PREFIX_ONTODEVICE.iri("sensesSubstance");
    private static final Iri hasUnit = PREFIX_OM.iri("hasUnit");
    private static final Iri symbol = PREFIX_OM.iri("symbol");
    private static final Iri hasLowLevelState = PREFIX_ONTOBMS.iri("hasLowLevelState");
    private static final Iri hasMediumLevelState = PREFIX_ONTOBMS.iri("hasMediumLevelState");
    private static final Iri hasHighLevelState = PREFIX_ONTOBMS.iri("hasHighLevelState");
    private static final Iri extractsAirFrom = PREFIX_ONTOBMS.iri("extractsAirFrom");
    private static final Iri hasInput_ontobms = PREFIX_ONTOBMS.iri("hasInput");
    private static final Iri hasInput_ontocape = PREFIX_ONTOCAPE_NETWORK_SYSTEM.iri("hasInput");
    private static final Iri refersToGeneralizedAmount = PREFIX_ONTOCAPE_CPS.iri("refersToGeneralizedAmount");
    private static final Iri hasSubsystem = PREFIX_ONTOCAPE_SYSTEM.iri("hasSubsystem");
    private static final Iri hasProperty = PREFIX_ONTOCAPE_SYSTEM.iri("hasProperty");
    private static final Iri refersToMaterial = PREFIX_ONTOCAPE_CPS_BEHAVIOR.iri("refersToMaterial");
    private static final Iri hasWaterFlowRate = PREFIX_ONTOBMS.iri("hasWaterFlowRate");
    private static final Iri isAttachedTo = PREFIX_ONTODEVICE.iri("isAttachedTo");
    private static final Iri thermodynamicBehavior = PREFIX_ONTOCAPE_MATERIAL.iri("thermodynamicBehavior");
    private static final Iri hasPressure = PREFIX_ONTODEVICE.iri("hasPressure");
    private static final Iri hasTemperature = PREFIX_ONTODEVICE.iri("hasTemperature");
    private static final Iri hasRelativeHumidity = PREFIX_ONTODEVICE.iri("hasRelativeHumidity");
    private static final Iri isConnectedTo = PREFIX_ONTODEVICE.iri("isConnectedTo");
    private static final Iri hasControlParameter = PREFIX_ONTODEVICE.iri("hasControlParameter");
    private static final Iri hasFeedbackParameter = PREFIX_ONTODEVICE.iri("hasFeedbackParameter");
    private static final Iri hasOpening = PREFIX_ONTOBMS.iri("hasOpening");
    private static final Iri isPartOf = PREFIX_META_MODEL_MEREOLOGY.iri("isPartOf");
    private static final Iri hasPart = PREFIX_META_MODEL_MEREOLOGY.iri("hasPart");
    private static final Iri hasCumulativeEnergyConsumption = PREFIX_ONTODEVICE.iri("hasCumulativeEnergyConsumption");
    private static final Iri hasHeatDuty = PREFIX_ONTOBMS.iri("hasHeatDuty");
    private static final Iri hasOutput = PREFIX_ONTOCAPE_NETWORK_SYSTEM.iri("hasOutput");
    /**
     * Classes
     */
    private static final Iri FumeHood = PREFIX_ONTOBMS.iri("FumeHood");
    private static final Iri WalkInFumeHood = PREFIX_ONTOBMS.iri("WalkInFumeHood");
    private static final Iri VAVSystem = PREFIX_ONTOBMS.iri("VAVSystem");
    private static final Iri Pipe = PREFIX_ONTOBMS.iri("Pipe");
    private static final Iri Duct = PREFIX_ONTOBMS.iri("Duct");
    private static final Iri Valve = PREFIX_S4BLDG.iri("Valve");
    private static final Iri WaterCoil = PREFIX_ONTOBMS.iri("WaterCoil");
    private static final Iri Filter = PREFIX_S4BLDG.iri("Filter");
    private static final Iri Fan = PREFIX_S4BLDG.iri("Fan");
    private static final Iri Tee = PREFIX_ONTOBMS.iri("Tee");
    private static final Iri MakeupAirUnit = PREFIX_ONTOBMS.iri("MakeupAirUnit");
    private static final Iri SupplyVAV = PREFIX_ONTOBMS.iri("SupplyVAV");
    private static final Iri ExhaustVAV = PREFIX_ONTOBMS.iri("ExhaustVAV");
    private static final Iri Damper = PREFIX_S4BLDG.iri("Damper");
    private static final Iri ExhaustFan = PREFIX_ONTOBMS.iri("ExhaustFan");
    private static final Iri ElectricalCoil = PREFIX_ONTOBMS.iri("ElectricalCoil");
    private static final Iri FanCoilUnit = PREFIX_ONTOBMS.iri("FanCoilUnit");
    private static final Iri DamperState = PREFIX_ONTOBMS.iri("DamperState");
    private static final Iri SashOpeningSensor = PREFIX_ONTODEVICE.iri("SashOpeningSensor");
    private static final Iri StatusSensor = PREFIX_ONTODEVICE.iri("StatusSensor");
    private static final Iri FlowSensor = PREFIX_ONTODEVICE.iri("FlowSensor");
    private static final Iri Setpoint = PREFIX_ONTODEVICE.iri("Setpoint");
    private static final Iri State = PREFIX_SAREF.iri("State");
    private static final Iri Percentage = PREFIX_OM.iri("Percentage");
    private static final Iri Measure = PREFIX_OM.iri("Measure");
    private static final Iri VolumetricFlowRate = PREFIX_OM.iri("VolumetricFlowRate");
    private static final Iri Mixture = PREFIX_OM.iri("Mixture");
    private static final Iri SingularUnit  = PREFIX_OM.iri("SingularUnit");
    private static final Iri Unit  = PREFIX_OM.iri("Unit");
    private static final Iri UnitDivision  = PREFIX_OM.iri("UnitDivision");
    private static final Iri CanopyHood  = PREFIX_ONTOBMS.iri("CanopyHood");
    private static final Iri SwitchState  = PREFIX_ONTOBMS.iri("SwitchState");
    private static final Iri LowLevelState  = PREFIX_ONTOBMS.iri("LowLevelState");
    private static final Iri MediumLevelState  = PREFIX_ONTOBMS.iri("MediumLevelState");
    private static final Iri HighLevelState  = PREFIX_ONTOBMS.iri("HighLevelState");
    private static final Iri CAVSystem  = PREFIX_ONTOBMS.iri("CAVSystem");
    private static final Iri Temperature = PREFIX_OM.iri("Temperature");
    private static final Iri Pressure = PREFIX_OM.iri("Pressure");
    private static final Iri ProcessStream = PREFIX_ONTOCAPE_CPS_FUNCTION_PROCESS.iri("ProcessStream");
    private static final Iri GeneralizedAmount = PREFIX_ONTOCAPE_CPS_BEHAVIOR.iri("GeneralizedAmount");
    private static final Iri ContinuousMaterialAmount = PREFIX_ONTOCAPE_CPS_BEHAVIOR.iri("ContinuousMaterialAmount");
    private static final Iri ConvectiveTransportRate = PREFIX_ONTOCAPE_CPS_BEHAVIOR.iri("ConvectiveTransportRate");
    private static final Iri Material = PREFIX_ONTOCAPE_MATERIAL.iri("Material");
    private static final Iri SinglePhase = PREFIX_ONTOCAPE_PHASE_SYSTEM.iri("SinglePhase");
    private static final Iri PressureSensor = PREFIX_ONTODEVICE.iri("PressureSensor");
    private static final Iri TemperatureSensor = PREFIX_SAREF.iri("TemperatureSensor");
    private static final Iri RelativeHumidity = PREFIX_OM.iri("RelativeHumidity");
    private static final Iri HumiditySensor = PREFIX_ONTODEVICE.iri("HumiditySensor");
    private static final Iri ControlSensor = PREFIX_ONTODEVICE.iri("ControlSensor");
    private static final Iri FeedbackSensor = PREFIX_ONTODEVICE.iri("FeedbackSensor");
    private static final Iri StartStopState = PREFIX_SAREF.iri("StartStopState");
    private static final Iri AlarmState = PREFIX_ONTODEVICE.iri("AlarmState");
    private static final Iri Frequency = PREFIX_OM.iri("Frequency");
    private static final Iri MultiLevelState = PREFIX_SAREF.iri("MultiLevelState");
    private static final Iri ExtractionArm = PREFIX_ONTOBMS.iri("ExtractionArm");
    private static final Iri TemperatureChange = PREFIX_ONTOCAPE_CPS_FUNCTION_PROCESS.iri("TemperatureChange");
    private static final Iri Heat = PREFIX_OM.iri("Heat");
    private static final Iri UnitMultiplication = PREFIX_OM.iri("UnitMultiplication");
    private static final Iri HeatDuty = PREFIX_ONTOBMS.iri("HeatDuty");
    private static final Iri PrefixedUnit = PREFIX_OM.iri("PrefixedUnit");
    /*
     * Instances
     */
    private static final Iri Air = PREFIX_ONTOCAPE_MATERIAL_SUBSTANCE.iri("Air");
    private static final Iri percent  = PREFIX_OM.iri("percent");
    private static final Iri cubicMetrePerHour  = PREFIX_OM.iri("cubicMetrePerHour");
    private static final Iri litrePerMinute = PREFIX_OM.iri("litrePerMinute");
    private static final Iri degreeCelsius = PREFIX_OM.iri("degreeCelsius");
    private static final Iri kilowattHour = PREFIX_OM.iri("kilowattHour");
    private static final Iri kilowatt = PREFIX_OM.iri("kilowatt");
    /*
     * csv filepath
     */
    String csvFilePath ;

    /*
     * csv scanner 
     */
    Scanner sc;

    /**
     * 
     * @param CSVFile
     * @throws IOException
     */
    public BMSInstantiationAgent(String filePath, String clientProp) throws IOException {
        this.csvFilePath = filePath;
        loadconfigs(clientProp);

        kbClient = new RemoteStoreClient();

        kbClient.setUpdateEndpoint(updateEndpoint);
        kbClient.setQueryEndpoint(queryEndpoint);
    }

    public void instantiateFH() throws FileNotFoundException {
        String FH_label = null;
        String SashOpening_IRI = null;
        String SashOpening_IRI_label = null;
        String State_IRI = null;
        String State_IRI_label = null;
        String VAV_label = null;
        String DamperState_IRI = null;
        String DamperState_IRI_label = null;
        String AirFlow_IRI = null;
        String AirFlow_IRI_label = null;
        String Setpoint_IRI = null;
        String Setpoint_IRI_label = null;
        
		File file = new File(csvFilePath);
        sc = new Scanner(file);

        //Retrieve each of the necessary labels and IRIs
        while (sc.hasNext()) {
            String string =  sc.nextLine();

            String key = string.split(",")[0];
            LOGGER.info("The key is " + key + " and the string is " + string);
            switch (key) {
                case "FH_label":
                FH_label = string.split(",")[1];
                break;
                case "SashOpening_IRI":
                SashOpening_IRI = string.split(",")[1];
                break;
                case "SashOpening_IRI_label":
                SashOpening_IRI_label = string.split(",")[1];
                break;
                case "State_IRI":
                State_IRI = string.split(",")[1];
                break;
                case "State_IRI_label":
                State_IRI_label = string.split(",")[1];
                break;
                case "VAV_label":
                VAV_label = string.split(",")[1];
                break;
                case "DamperState_IRI":
                DamperState_IRI = string.split(",")[1];
                break;
                case "DamperState_IRI_label":
                DamperState_IRI_label = string.split(",")[1];
                break;
                case "AirFlow_IRI":
                AirFlow_IRI = string.split(",")[1];
                break;
                case "AirFlow_IRI_label":
                AirFlow_IRI_label = string.split(",")[1];
                break;
                case "Setpoint_IRI":
                Setpoint_IRI = string.split(",")[1];
                break;
                case "Setpoint_IRI_label":
                Setpoint_IRI_label = string.split(",")[1];
                break;
            }
        }

        //instantiate the FH instance
        //include label
        String FH_Instance = "https://www.theworldavatar.com/kg/ontobms/" + FH_label + "_" + UUID.randomUUID();
        String SashOpenPercentage_Instance = "https://www.theworldavatar.com/kg/ontobms/SashOpenPercentage_" + UUID.randomUUID();
        String SashOpSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/SashOpeningSensor_" + UUID.randomUUID();
        String StatusSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/StatusSensor_" + UUID.randomUUID();
        TriplePattern P1 = iri(FH_Instance).isA(FumeHood);
        TriplePattern P2 = iri(FH_Instance).has(label, FH_label);
        TriplePattern P3 = iri(FH_Instance).has(hasSashOpenPercentage, iri(SashOpenPercentage_Instance));
        TriplePattern P4 = iri(FH_Instance).has(hasState, iri(State_IRI));
        TriplePattern P5 = iri(FH_Instance).has(consistsOf, iri(StatusSensor_Instance));
        TriplePattern P6 = iri(FH_Instance).has(consistsOf, iri(SashOpSensor_Instance));
        InsertDataQuery insert = Queries.INSERT_DATA(P1, P2, P3, P4, P5, P6);
        insert.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert.getQueryString());

        //instantiate SashOP Sensor
        String VAV_Instance = "https://www.theworldavatar.com/kg/ontobms/" + VAV_label + "_" + UUID.randomUUID();
        TriplePattern P7 = iri(SashOpSensor_Instance).isA(SashOpeningSensor);
        TriplePattern P7_1 = iri(SashOpSensor_Instance).has(label, FH_label + " Sash Opening Sensor");
        TriplePattern P8 = iri(SashOpSensor_Instance).has(measures, iri(SashOpenPercentage_Instance));
        TriplePattern P9 = iri(SashOpenPercentage_Instance).isA(Percentage);
        TriplePattern P10 = iri(SashOpSensor_Instance).has(sendsSignalTo, iri(VAV_Instance));
        TriplePattern P11 = iri(SashOpenPercentage_Instance).has(hasValue, iri(SashOpening_IRI));
        TriplePattern P12 = iri(SashOpening_IRI).isA(Measure);
        TriplePattern P12_1 = iri(SashOpening_IRI).has(hasUnit, percent);
        TriplePattern P12_2 = percent.isA(Unit);
        TriplePattern P12_3 = percent.isA(SingularUnit);
        TriplePattern P12_4 = percent.has(symbol, "%");
        TriplePattern P12_5 = percent.has(label, "percent");
        TriplePattern P12_6 = iri(SashOpening_IRI).has(label, SashOpening_IRI_label);
        InsertDataQuery insert1 = Queries.INSERT_DATA(P7, P7_1, P8, P9, P10, P11, P12, P12_1, P12_2, P12_3, P12_4, P12_5, P12_6);
        insert1.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert1.getQueryString());

        //instantiate Status Sensor
        TriplePattern P13 = iri(StatusSensor_Instance).isA(StatusSensor);
        TriplePattern P13_1 = iri(StatusSensor_Instance).has(label, FH_label + " Status Sensor");
        TriplePattern P14 = iri(StatusSensor_Instance).has(observes, iri(State_IRI));
        TriplePattern P15 = iri(State_IRI).isA(State);
        TriplePattern P15_1 = iri(State_IRI).has(label, State_IRI_label);
        TriplePattern P16 = iri(StatusSensor_Instance).has(sendsSignalTo, iri(VAV_Instance));
        InsertDataQuery insert2 = Queries.INSERT_DATA(P13, P13_1, P14, P15, P15_1, P16);
        insert2.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert2.getQueryString());

        //instantiate VAVSystem
        TriplePattern P17 = iri(VAV_Instance).isA(VAVSystem);
        TriplePattern P17_1 = iri(VAV_Instance).has(label, VAV_label);
        String Setpoint_Instance = "https://www.theworldavatar.com/kg/ontobms/Setpoint_" + UUID.randomUUID();
        TriplePattern P18 = iri(VAV_Instance).has(hasSetpoint, iri(Setpoint_Instance));
        String FlowSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/FlowSensor_" + UUID.randomUUID();
        String DamperStateSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/DamperStateSensor_" + UUID.randomUUID();
        TriplePattern P19 = iri(VAV_Instance).has(consistsOf, iri(FlowSensor_Instance));
        TriplePattern P20 = iri(VAV_Instance).has(consistsOf, iri(DamperStateSensor_Instance));
        String VolumetricFlowRate_Instance = "https://www.theworldavatar.com/kg/ontobms/VolumetricFlowRate_" + UUID.randomUUID();
        TriplePattern P21 = iri(VAV_Instance).has(hasAirFlowRate, iri(VolumetricFlowRate_Instance));
        TriplePattern P22 = iri(VAV_Instance).has(hasState, iri(DamperState_IRI));
        TriplePattern P23 = iri(VolumetricFlowRate_Instance).isA(VolumetricFlowRate);
        InsertDataQuery insert3 = Queries.INSERT_DATA(P17, P17_1, P18, P19, P20, P21, P22, P23);
        insert3.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert3.getQueryString());

        //instantiate setpoint
        TriplePattern P24 = iri(Setpoint_Instance).isA(Setpoint);
        String SetpointQuantity_Instance = "https://www.theworldavatar.com/kg/ontobms/SetpointQuantity_" + UUID.randomUUID();
        TriplePattern P25 = iri(Setpoint_Instance).has(hasQuantity, iri(SetpointQuantity_Instance));
        TriplePattern P26 = iri(SetpointQuantity_Instance).isA(VolumetricFlowRate);
        TriplePattern P27 = iri(SetpointQuantity_Instance).has(hasValue, iri(Setpoint_IRI));
        TriplePattern P28 = iri(Setpoint_IRI).isA(Measure);
        TriplePattern P28_1 = iri(Setpoint_IRI).has(hasUnit, cubicMetrePerHour);
        TriplePattern P28_2 = cubicMetrePerHour.isA(UnitDivision);
        TriplePattern P28_3 = cubicMetrePerHour.has(symbol, "m3/h");
        TriplePattern P28_4 = cubicMetrePerHour.has(label, "cubic metre per hour");
        TriplePattern P28_5 = iri(Setpoint_IRI).has(label, Setpoint_IRI_label);
        InsertDataQuery insert4 = Queries.INSERT_DATA(P24, P25, P26, P27, P28, P28_1, P28_2, P28_3, P28_4, P28_5);
        insert4.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert4.getQueryString());

        //instantiate AirFlow Sensor
        TriplePattern P29 = iri(FlowSensor_Instance).isA(FlowSensor);
        TriplePattern P29_1 = iri(FlowSensor_Instance).has(label, VAV_label + " AirFlow Sensor");
        TriplePattern P30 = iri(FlowSensor_Instance).has(sendsSignalTo, iri(VAV_Instance));
        TriplePattern P31 = iri(FlowSensor_Instance).has(measures, iri(VolumetricFlowRate_Instance));
        TriplePattern P32 = iri(VolumetricFlowRate_Instance).has(hasValue, iri(AirFlow_IRI));
        TriplePattern P33 = iri(AirFlow_IRI).isA(Measure);
        TriplePattern P33_1 = iri(FlowSensor_Instance).has(sensesSubstance, Air);
        TriplePattern P33_2 = Air.isA(Mixture);
        TriplePattern P33_3 = iri(AirFlow_IRI).has(hasUnit, cubicMetrePerHour);
        TriplePattern P33_4 = cubicMetrePerHour.isA(UnitDivision);
        TriplePattern P33_5 = cubicMetrePerHour.has(symbol, "m3/h");
        TriplePattern P33_6 = cubicMetrePerHour.has(label, "cubic metre per hour");
        TriplePattern P33_7 = iri(AirFlow_IRI).has(label, AirFlow_IRI_label);
        InsertDataQuery insert5 = Queries.INSERT_DATA(P29, P29_1, P30, P31, P32, P33, P33_1, P33_2, P33_3, P33_4, P33_5, P33_6, P33_7);
        insert5.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM, PREFIX_ONTOCAPE_MATERIAL_SUBSTANCE);
        kbClient.executeUpdate(insert5.getQueryString());

        //instantiate damper state sensor
        TriplePattern P34 = iri(DamperStateSensor_Instance).isA(StatusSensor);
        TriplePattern P34_1 = iri(DamperStateSensor_Instance).has(label, VAV_label + " Damper State Sensor");
        TriplePattern P35 = iri(DamperStateSensor_Instance).has(sendsSignalTo, iri(VAV_Instance));
        TriplePattern P36 = iri(DamperStateSensor_Instance).has(observes, iri(DamperState_IRI));
        TriplePattern P37 = iri(DamperState_IRI).isA(DamperState);
        TriplePattern P37_1 = iri(DamperState_IRI).has(label, DamperState_IRI_label);
        InsertDataQuery insert6 = Queries.INSERT_DATA(P34, P34_1, P35, P36, P37, P37_1);
        insert6.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert6.getQueryString());
        }

    public void instantiateWFH() throws FileNotFoundException {
        String WFH_label = null;
        String SashOpening_IRI = null;
        String SashOpening_IRI_label = null;
        String State_IRI = null;
        String State_IRI_label = null;
        String VAV_label = null;
        String DamperState_IRI = null;
        String DamperState_IRI_label = null;
        String AirFlow_IRI = null;
        String AirFlow_IRI_label = null;
        String Setpoint_IRI = null;
        String Setpoint_IRI_label = null;
        
		File file = new File(csvFilePath);
        sc = new Scanner(file);

        //Retrieve each of the necessary labels and IRIs
        while (sc.hasNext()) {
            String string =  sc.nextLine();

            String key = string.split(",")[0];
            LOGGER.info("The key is " + key + " and the string is " + string);
            switch (key) {
                case "WFH_label":
                WFH_label = string.split(",")[1];
                break;
                case "SashOpening_IRI":
                SashOpening_IRI = string.split(",")[1];
                break;
                case "SashOpening_IRI_label":
                SashOpening_IRI_label = string.split(",")[1];
                break;
                case "State_IRI":
                State_IRI = string.split(",")[1];
                break;
                case "State_IRI_label":
                State_IRI_label = string.split(",")[1];
                break;
                case "VAV_label":
                VAV_label = string.split(",")[1];
                break;
                case "DamperState_IRI":
                DamperState_IRI = string.split(",")[1];
                break;
                case "DamperState_IRI_label":
                DamperState_IRI_label = string.split(",")[1];
                break;
                case "AirFlow_IRI":
                AirFlow_IRI = string.split(",")[1];
                break;
                case "AirFlow_IRI_label":
                AirFlow_IRI_label = string.split(",")[1];
                break;
                case "Setpoint_IRI":
                Setpoint_IRI = string.split(",")[1];
                break;
                case "Setpoint_IRI_label":
                Setpoint_IRI_label = string.split(",")[1];
                break;
            }
        }

        //instantiate the WFH instance
        //include label
        String WFH_Instance = "https://www.theworldavatar.com/kg/ontobms/" + WFH_label + "_" + UUID.randomUUID();
        String SashOpenPercentage_Instance = "https://www.theworldavatar.com/kg/ontobms/SashOpenPercentage_" + UUID.randomUUID();
        String SashOpSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/SashOpeningSensor_" + UUID.randomUUID();
        String StatusSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/StatusSensor_" + UUID.randomUUID();
        TriplePattern P1 = iri(WFH_Instance).isA(WalkInFumeHood);
        TriplePattern P2 = iri(WFH_Instance).has(label, WFH_label);
        TriplePattern P3 = iri(WFH_Instance).has(hasSashOpenPercentage, iri(SashOpenPercentage_Instance));
        TriplePattern P4 = iri(WFH_Instance).has(hasState, iri(State_IRI));
        TriplePattern P5 = iri(WFH_Instance).has(consistsOf, iri(StatusSensor_Instance));
        TriplePattern P6 = iri(WFH_Instance).has(consistsOf, iri(SashOpSensor_Instance));
        InsertDataQuery insert = Queries.INSERT_DATA(P1,P2, P3, P4, P5, P6);
        insert.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert.getQueryString());

        //instantiate SashOP Sensor
        String VAV_Instance = "https://www.theworldavatar.com/kg/ontobms/" + VAV_label + "_" + UUID.randomUUID();
        TriplePattern P7 = iri(SashOpSensor_Instance).isA(SashOpeningSensor);
        TriplePattern P7_1 = iri(SashOpSensor_Instance).has(label, WFH_label + " Sash Opening Sensor");
        TriplePattern P8 = iri(SashOpSensor_Instance).has(measures, iri(SashOpenPercentage_Instance));
        TriplePattern P9 = iri(SashOpenPercentage_Instance).isA(Percentage);
        TriplePattern P10 = iri(SashOpSensor_Instance).has(sendsSignalTo, iri(VAV_Instance));
        TriplePattern P11 = iri(SashOpenPercentage_Instance).has(hasValue, iri(SashOpening_IRI));
        TriplePattern P12 = iri(SashOpening_IRI).isA(Measure);
        TriplePattern P12_1 = iri(SashOpening_IRI).has(hasUnit, percent);
        TriplePattern P12_2 = percent.isA(Unit);
        TriplePattern P12_3 = percent.isA(SingularUnit);
        TriplePattern P12_4 = percent.has(symbol, "%");
        TriplePattern P12_5 = percent.has(label, "percent");
        TriplePattern P12_6 = iri(SashOpening_IRI).has(label, SashOpening_IRI_label);
        InsertDataQuery insert1 = Queries.INSERT_DATA(P7, P7_1, P8, P9, P10, P11, P12, P12_1, P12_2, P12_3, P12_4, P12_5, P12_6);
        insert1.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert1.getQueryString());

        //instantiate Status Sensor
        TriplePattern P13 = iri(StatusSensor_Instance).isA(StatusSensor);
        TriplePattern P13_1 = iri(StatusSensor_Instance).has(label, WFH_label + " Status Sensor");
        TriplePattern P14 = iri(StatusSensor_Instance).has(observes, iri(State_IRI));
        TriplePattern P15 = iri(State_IRI).isA(State);
        TriplePattern P15_1 = iri(State_IRI).has(label, State_IRI_label);
        TriplePattern P16 = iri(StatusSensor_Instance).has(sendsSignalTo, iri(VAV_Instance));
        InsertDataQuery insert2 = Queries.INSERT_DATA(P13, P13_1, P14, P15, P15_1, P16);
        insert2.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert2.getQueryString());

        //instantiate VAVSystem
        TriplePattern P17 = iri(VAV_Instance).isA(VAVSystem);
        TriplePattern P17_1 = iri(VAV_Instance).has(label, VAV_label);
        String Setpoint_Instance = "https://www.theworldavatar.com/kg/ontobms/Setpoint_" + UUID.randomUUID();
        TriplePattern P18 = iri(VAV_Instance).has(hasSetpoint, iri(Setpoint_Instance));
        String FlowSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/FlowSensor_" + UUID.randomUUID();
        String DamperStateSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/DamperStateSensor_" + UUID.randomUUID();
        TriplePattern P19 = iri(VAV_Instance).has(consistsOf, iri(FlowSensor_Instance));
        TriplePattern P20 = iri(VAV_Instance).has(consistsOf, iri(DamperStateSensor_Instance));
        String VolumetricFlowRate_Instance = "https://www.theworldavatar.com/kg/ontobms/VolumetricFlowRate_" + UUID.randomUUID();
        TriplePattern P21 = iri(VAV_Instance).has(hasAirFlowRate, iri(VolumetricFlowRate_Instance));
        TriplePattern P22 = iri(VAV_Instance).has(hasState, iri(DamperState_IRI));
        TriplePattern P23 = iri(VolumetricFlowRate_Instance).isA(VolumetricFlowRate);
        InsertDataQuery insert3 = Queries.INSERT_DATA(P17, P17_1, P18, P19, P20, P21, P22, P23);
        insert3.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert3.getQueryString());

        //instantiate setpoint
        TriplePattern P24 = iri(Setpoint_Instance).isA(Setpoint);
        String SetpointQuantity_Instance = "https://www.theworldavatar.com/kg/ontobms/SetpointQuantity_" + UUID.randomUUID();
        TriplePattern P25 = iri(Setpoint_Instance).has(hasQuantity, iri(SetpointQuantity_Instance));
        TriplePattern P26 = iri(SetpointQuantity_Instance).isA(VolumetricFlowRate);
        TriplePattern P27 = iri(SetpointQuantity_Instance).has(hasValue, iri(Setpoint_IRI));
        TriplePattern P28 = iri(Setpoint_IRI).isA(Measure);
        TriplePattern P28_1 = iri(Setpoint_IRI).has(hasUnit, cubicMetrePerHour);
        TriplePattern P28_2 = cubicMetrePerHour.isA(UnitDivision);
        TriplePattern P28_3 = cubicMetrePerHour.has(symbol, "m3/h");
        TriplePattern P28_4 = cubicMetrePerHour.has(label, "cubic metre per hour");
        TriplePattern P28_5 = iri(Setpoint_IRI).has(label, Setpoint_IRI_label);
        InsertDataQuery insert4 = Queries.INSERT_DATA(P24, P25, P26, P27, P28, P28_1, P28_2, P28_3, P28_4, P28_5);
        insert4.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert4.getQueryString());

        //instantiate AirFlow Sensor
        TriplePattern P29 = iri(FlowSensor_Instance).isA(FlowSensor);
        TriplePattern P29_1 = iri(FlowSensor_Instance).has(label, VAV_label + " AirFlow Sensor");
        TriplePattern P30 = iri(FlowSensor_Instance).has(sendsSignalTo, iri(VAV_Instance));
        TriplePattern P31 = iri(FlowSensor_Instance).has(measures, iri(VolumetricFlowRate_Instance));
        TriplePattern P32 = iri(VolumetricFlowRate_Instance).has(hasValue, iri(AirFlow_IRI));
        TriplePattern P33 = iri(AirFlow_IRI).isA(Measure);
        TriplePattern P33_1 = iri(FlowSensor_Instance).has(sensesSubstance, Air);
        TriplePattern P33_2 = Air.isA(Mixture);
        TriplePattern P33_3 = iri(AirFlow_IRI).has(hasUnit, cubicMetrePerHour);
        TriplePattern P33_4 = cubicMetrePerHour.isA(UnitDivision);
        TriplePattern P33_5 = cubicMetrePerHour.has(symbol, "m3/h");
        TriplePattern P33_6 = cubicMetrePerHour.has(label, "cubic metre per hour");
        TriplePattern P33_7 = iri(AirFlow_IRI).has(label, AirFlow_IRI_label);
        InsertDataQuery insert5 = Queries.INSERT_DATA(P29, P29_1, P30, P31, P32, P33, P33_1, P33_2, P33_3, P33_4, P33_5, P33_6, P33_7);
        insert5.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM, PREFIX_ONTOCAPE_MATERIAL_SUBSTANCE);
        kbClient.executeUpdate(insert5.getQueryString());

        //instantiate damper state sensor
        TriplePattern P34 = iri(DamperStateSensor_Instance).isA(StatusSensor);
        TriplePattern P34_1 = iri(DamperStateSensor_Instance).has(label, VAV_label + " Damper State Sensor");
        TriplePattern P35 = iri(DamperStateSensor_Instance).has(sendsSignalTo, iri(VAV_Instance));
        TriplePattern P36 = iri(DamperStateSensor_Instance).has(observes, iri(DamperState_IRI));
        TriplePattern P37 = iri(DamperState_IRI).isA(DamperState);
        TriplePattern P37_1 = iri(DamperState_IRI).has(label, DamperState_IRI_label);
        InsertDataQuery insert6 = Queries.INSERT_DATA(P34, P34_1, P35, P36, P37, P37_1);
        insert6.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert6.getQueryString());
    }

    public void instantiateCH_VAV() throws FileNotFoundException {
        String CH_label = null;
        String LowLevelState_IRI = null;
        String LowLevelState_IRI_label = null;
        String MediumLevelState_IRI = null;
        String MediumLevelState_IRI_label = null;
        String HighLevelState_IRI = null;
        String HighLevelState_IRI_label = null;
        String VAV_label = null;
        String DamperState_IRI = null;
        String DamperState_IRI_label = null;
        String AirFlow_IRI = null;
        String AirFlow_IRI_label = null;
        String Setpoint_IRI = null;
        String Setpoint_IRI_label = null;

        File file = new File(csvFilePath);
        sc = new Scanner(file);

        //Retrieve each of the necessary labels and IRIs
        while (sc.hasNext()) {
            String string =  sc.nextLine();

            String key = string.split(",")[0];
            LOGGER.info("The key is " + key + " and the string is " + string);
            switch (key) {
                case "CH_label":
                CH_label = string.split(",")[1];
                break;
                case "LowLevelState_IRI":
                LowLevelState_IRI = string.split(",")[1];
                break;
                case "LowLevelState_IRI_label":
                LowLevelState_IRI_label = string.split(",")[1];
                break;
                case "MediumLevelState_IRI":
                MediumLevelState_IRI = string.split(",")[1];
                break;
                case "MediumLevelState_IRI_label":
                MediumLevelState_IRI_label = string.split(",")[1];
                break;
                case "HighLevelState_IRI":
                HighLevelState_IRI = string.split(",")[1];
                break;
                case "HighLevelState_IRI_label":
                HighLevelState_IRI_label = string.split(",")[1];
                break;
                case "VAV_label":
                VAV_label = string.split(",")[1];
                break;
                case "DamperState_IRI":
                DamperState_IRI = string.split(",")[1];
                break;
                case "DamperState_IRI_label":
                DamperState_IRI_label = string.split(",")[1];
                break;
                case "AirFlow_IRI":
                AirFlow_IRI = string.split(",")[1];
                break;
                case "AirFlow_IRI_label":
                AirFlow_IRI_label = string.split(",")[1];
                break;
                case "Setpoint_IRI":
                Setpoint_IRI = string.split(",")[1];
                break;
                case "Setpoint_IRI_label":
                Setpoint_IRI_label = string.split(",")[1];
                break;
            }
        }

        //instantiate the CH instance
        //include label
        String CH_Instance = "https://www.theworldavatar.com/kg/ontobms/" + CH_label + "_VAV_" + UUID.randomUUID();
        String State_Instance = "https://www.theworldavatar.com/kg/ontobms/State_" + UUID.randomUUID();
        String StatusSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/StatusSensor_" + UUID.randomUUID();
        TriplePattern P1 = iri(CH_Instance).isA(CanopyHood);
        TriplePattern P2 = iri(CH_Instance).has(label, CH_label);
        TriplePattern P3 = iri(CH_Instance).has(hasState, iri(State_Instance));
        TriplePattern P4 = iri(CH_Instance).has(consistsOf, iri(StatusSensor_Instance));
        InsertDataQuery insert = Queries.INSERT_DATA(P1,P2, P3, P4);
        insert.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert.getQueryString());

        //instantiate Status Sensor
        String VAV_Instance = "https://www.theworldavatar.com/kg/ontobms/" + VAV_label + "_" + UUID.randomUUID();
        TriplePattern P13 = iri(StatusSensor_Instance).isA(StatusSensor);
        TriplePattern P13_1 = iri(StatusSensor_Instance).has(label, CH_label + " Status Sensor");
        TriplePattern P14 = iri(StatusSensor_Instance).has(observes, iri(State_Instance));
        TriplePattern P15 = iri(State_Instance).isA(SwitchState);
        TriplePattern P16 = iri(StatusSensor_Instance).has(sendsSignalTo, iri(VAV_Instance));
        TriplePattern P16_1 = iri(State_Instance).has(hasLowLevelState, iri(LowLevelState_IRI));
        TriplePattern P16_2 = iri(LowLevelState_IRI).isA(LowLevelState);
        TriplePattern P16_3 = iri(State_Instance).has(hasMediumLevelState, iri(MediumLevelState_IRI));
        TriplePattern P16_4 = iri(MediumLevelState_IRI).isA(MediumLevelState);
        TriplePattern P16_5 = iri(State_Instance).has(hasHighLevelState, iri(HighLevelState_IRI));
        TriplePattern P16_6 = iri(HighLevelState_IRI).isA(HighLevelState);
        TriplePattern P16_7 = iri(LowLevelState_IRI).has(label, LowLevelState_IRI_label);
        TriplePattern P16_8 = iri(MediumLevelState_IRI).has(label, MediumLevelState_IRI_label);
        TriplePattern P16_9 = iri(HighLevelState_IRI).has(label, HighLevelState_IRI_label);
        InsertDataQuery insert2 = Queries.INSERT_DATA(P13, P13_1, P14, P15, P16, P16_1, P16_2, P16_3, P16_4, P16_5, P16_6, P16_7, P16_8, P16_9);
        insert2.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert2.getQueryString());

        //instantiate VAVSystem
        TriplePattern P17 = iri(VAV_Instance).isA(VAVSystem);
        TriplePattern P17_1 = iri(VAV_Instance).has(label, VAV_label);
        TriplePattern P17_2 = iri(VAV_Instance).has(extractsAirFrom, iri(CH_Instance));
        String Setpoint_Instance = "https://www.theworldavatar.com/kg/ontobms/Setpoint_" + UUID.randomUUID();
        TriplePattern P18 = iri(VAV_Instance).has(hasSetpoint, iri(Setpoint_Instance));
        String FlowSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/FlowSensor_" + UUID.randomUUID();
        String DamperStateSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/DamperStateSensor_" + UUID.randomUUID();
        TriplePattern P19 = iri(VAV_Instance).has(consistsOf, iri(FlowSensor_Instance));
        TriplePattern P20 = iri(VAV_Instance).has(consistsOf, iri(DamperStateSensor_Instance));
        String VolumetricFlowRate_Instance = "https://www.theworldavatar.com/kg/ontobms/VolumetricFlowRate_" + UUID.randomUUID();
        TriplePattern P21 = iri(VAV_Instance).has(hasAirFlowRate, iri(VolumetricFlowRate_Instance));
        TriplePattern P22 = iri(VAV_Instance).has(hasState, iri(DamperState_IRI));
        TriplePattern P23 = iri(VolumetricFlowRate_Instance).isA(VolumetricFlowRate);
        InsertDataQuery insert3 = Queries.INSERT_DATA(P17, P17_1, P17_2, P18, P19, P20, P21, P22, P23);
        insert3.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert3.getQueryString());

        //instantiate setpoint
        TriplePattern P24 = iri(Setpoint_Instance).isA(Setpoint);
        String SetpointQuantity_Instance = "https://www.theworldavatar.com/kg/ontobms/SetpointQuantity_" + UUID.randomUUID();
        TriplePattern P25 = iri(Setpoint_Instance).has(hasQuantity, iri(SetpointQuantity_Instance));
        TriplePattern P26 = iri(SetpointQuantity_Instance).isA(VolumetricFlowRate);
        TriplePattern P27 = iri(SetpointQuantity_Instance).has(hasValue, iri(Setpoint_IRI));
        TriplePattern P28 = iri(Setpoint_IRI).isA(Measure);
        TriplePattern P28_1 = iri(Setpoint_IRI).has(hasUnit, cubicMetrePerHour);
        TriplePattern P28_2 = cubicMetrePerHour.isA(UnitDivision);
        TriplePattern P28_3 = cubicMetrePerHour.has(symbol, "m3/h");
        TriplePattern P28_4 = cubicMetrePerHour.has(label, "cubic metre per hour");
        TriplePattern P28_5 = iri(Setpoint_IRI).has(label, Setpoint_IRI_label);
        InsertDataQuery insert4 = Queries.INSERT_DATA(P24, P25, P26, P27, P28, P28_1, P28_2, P28_3, P28_4, P28_5);
        insert4.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert4.getQueryString());

        //instantiate AirFlow Sensor
        TriplePattern P29 = iri(FlowSensor_Instance).isA(FlowSensor);
        TriplePattern P29_1 = iri(FlowSensor_Instance).has(label, VAV_label + " AirFlow Sensor");
        TriplePattern P30 = iri(FlowSensor_Instance).has(sendsSignalTo, iri(VAV_Instance));
        TriplePattern P31 = iri(FlowSensor_Instance).has(measures, iri(VolumetricFlowRate_Instance));
        TriplePattern P32 = iri(VolumetricFlowRate_Instance).has(hasValue, iri(AirFlow_IRI));
        TriplePattern P33 = iri(AirFlow_IRI).isA(Measure);
        TriplePattern P33_1 = iri(FlowSensor_Instance).has(sensesSubstance, Air);
        TriplePattern P33_2 = Air.isA(Mixture);
        TriplePattern P33_3 = iri(AirFlow_IRI).has(hasUnit, cubicMetrePerHour);
        TriplePattern P33_4 = cubicMetrePerHour.isA(UnitDivision);
        TriplePattern P33_5 = cubicMetrePerHour.has(symbol, "m3/h");
        TriplePattern P33_6 = cubicMetrePerHour.has(label, "cubic metre per hour");
        TriplePattern P33_7 = iri(AirFlow_IRI).has(label, AirFlow_IRI_label);
        InsertDataQuery insert5 = Queries.INSERT_DATA(P29, P29_1, P30, P31, P32, P33, P33_1, P33_2, P33_3, P33_4, P33_5, P33_6, P33_7);
        insert5.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM, PREFIX_ONTOCAPE_MATERIAL_SUBSTANCE);
        kbClient.executeUpdate(insert5.getQueryString());

        //instantiate damper state sensor
        TriplePattern P34 = iri(DamperStateSensor_Instance).isA(StatusSensor);
        TriplePattern P34_1 = iri(DamperStateSensor_Instance).has(label, VAV_label + " Damper State Sensor");
        TriplePattern P35 = iri(DamperStateSensor_Instance).has(sendsSignalTo, iri(VAV_Instance));
        TriplePattern P36 = iri(DamperStateSensor_Instance).has(observes, iri(DamperState_IRI));
        TriplePattern P37 = iri(DamperState_IRI).isA(DamperState);
        TriplePattern P37_1 = iri(DamperState_IRI).has(label, DamperState_IRI_label);
        InsertDataQuery insert6 = Queries.INSERT_DATA(P34, P34_1, P35, P36, P37, P37_1);
        insert6.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert6.getQueryString());
    }

    public void instantiateCH_CAV() throws FileNotFoundException {
        String CH_label = null;
        String CAV_label = null;
        String DamperState_IRI = null;
        String DamperState_IRI_label = null;
        String AirFlow_IRI = null;
        String AirFlow_IRI_label = null;
        String Setpoint_IRI = null;
        String Setpoint_IRI_label = null;

        File file = new File(csvFilePath);
        sc = new Scanner(file);

        //Retrieve each of the necessary labels and IRIs
        while (sc.hasNext()) {
            String string =  sc.nextLine();

            String key = string.split(",")[0];
            LOGGER.info("The key is " + key + " and the string is " + string);
            switch (key) {
                case "CH_label":
                CH_label = string.split(",")[1];
                break;
                case "CAV_label":
                CAV_label = string.split(",")[1];
                break;
                case "DamperState_IRI":
                DamperState_IRI = string.split(",")[1];
                break;
                case "DamperState_IRI_label":
                DamperState_IRI_label = string.split(",")[1];
                break;
                case "AirFlow_IRI":
                AirFlow_IRI = string.split(",")[1];
                break;
                case "AirFlow_IRI_label":
                AirFlow_IRI_label = string.split(",")[1];
                break;
                case "Setpoint_IRI":
                Setpoint_IRI = string.split(",")[1];
                break;
                case "Setpoint_IRI_label":
                Setpoint_IRI_label = string.split(",")[1];
                break;
            }
        }

        //instantiate the CH instance
        //include label
        String CH_Instance = "https://www.theworldavatar.com/kg/ontobms/" + CH_label + UUID.randomUUID();
        TriplePattern P1 = iri(CH_Instance).isA(CanopyHood);
        TriplePattern P2 = iri(CH_Instance).has(label, CH_label);
        InsertDataQuery insert = Queries.INSERT_DATA(P1,P2);
        insert.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert.getQueryString());

        //instantiate CAVSystem
        String CAV_Instance = "https://www.theworldavatar.com/kg/ontobms/" + CAV_label + "_" + UUID.randomUUID();
        TriplePattern P17 = iri(CAV_Instance).isA(CAVSystem);
        TriplePattern P17_1 = iri(CAV_Instance).has(label, CAV_label);
        TriplePattern P17_2 = iri(CAV_Instance).has(extractsAirFrom, iri(CH_Instance));
        String Setpoint_Instance = "https://www.theworldavatar.com/kg/ontobms/Setpoint_" + UUID.randomUUID();
        TriplePattern P18 = iri(CAV_Instance).has(hasSetpoint, iri(Setpoint_Instance));
        String FlowSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/FlowSensor_" + UUID.randomUUID();
        String DamperStateSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/DamperStateSensor_" + UUID.randomUUID();
        TriplePattern P19 = iri(CAV_Instance).has(consistsOf, iri(FlowSensor_Instance));
        TriplePattern P20 = iri(CAV_Instance).has(consistsOf, iri(DamperStateSensor_Instance));
        String VolumetricFlowRate_Instance = "https://www.theworldavatar.com/kg/ontobms/VolumetricFlowRate_" + UUID.randomUUID();
        TriplePattern P21 = iri(CAV_Instance).has(hasAirFlowRate, iri(VolumetricFlowRate_Instance));
        TriplePattern P22 = iri(CAV_Instance).has(hasState, iri(DamperState_IRI));
        TriplePattern P23 = iri(VolumetricFlowRate_Instance).isA(VolumetricFlowRate);
        InsertDataQuery insert3 = Queries.INSERT_DATA(P17, P17_1, P17_2, P18, P19, P20, P21, P22, P23);
        insert3.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert3.getQueryString());

        //instantiate setpoint
        TriplePattern P24 = iri(Setpoint_Instance).isA(Setpoint);
        String SetpointQuantity_Instance = "https://www.theworldavatar.com/kg/ontobms/SetpointQuantity_" + UUID.randomUUID();
        TriplePattern P25 = iri(Setpoint_Instance).has(hasQuantity, iri(SetpointQuantity_Instance));
        TriplePattern P26 = iri(SetpointQuantity_Instance).isA(VolumetricFlowRate);
        TriplePattern P27 = iri(SetpointQuantity_Instance).has(hasValue, iri(Setpoint_IRI));
        TriplePattern P28 = iri(Setpoint_IRI).isA(Measure);
        TriplePattern P28_1 = iri(Setpoint_IRI).has(hasUnit, cubicMetrePerHour);
        TriplePattern P28_2 = cubicMetrePerHour.isA(UnitDivision);
        TriplePattern P28_3 = cubicMetrePerHour.has(symbol, "m3/h");
        TriplePattern P28_4 = cubicMetrePerHour.has(label, "cubic metre per hour");
        TriplePattern P28_5 = iri(Setpoint_IRI).has(label, Setpoint_IRI_label);
        InsertDataQuery insert4 = Queries.INSERT_DATA(P24, P25, P26, P27, P28, P28_1, P28_2, P28_3, P28_4, P28_5);
        insert4.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert4.getQueryString());

        //instantiate AirFlow Sensor
        TriplePattern P29 = iri(FlowSensor_Instance).isA(FlowSensor);
        TriplePattern P29_1 = iri(FlowSensor_Instance).has(label, CAV_label + " AirFlow Sensor");
        TriplePattern P30 = iri(FlowSensor_Instance).has(sendsSignalTo, iri(CAV_Instance));
        TriplePattern P31 = iri(FlowSensor_Instance).has(measures, iri(VolumetricFlowRate_Instance));
        TriplePattern P32 = iri(VolumetricFlowRate_Instance).has(hasValue, iri(AirFlow_IRI));
        TriplePattern P33 = iri(AirFlow_IRI).isA(Measure);
        TriplePattern P33_1 = iri(FlowSensor_Instance).has(sensesSubstance, Air);
        TriplePattern P33_2 = Air.isA(Mixture);
        TriplePattern P33_3 = iri(AirFlow_IRI).has(hasUnit, cubicMetrePerHour);
        TriplePattern P33_4 = cubicMetrePerHour.isA(UnitDivision);
        TriplePattern P33_5 = cubicMetrePerHour.has(symbol, "m3/h");
        TriplePattern P33_6 = cubicMetrePerHour.has(label, "cubic metre per hour");
        TriplePattern P33_7 = iri(AirFlow_IRI).has(label, AirFlow_IRI_label);
        InsertDataQuery insert5 = Queries.INSERT_DATA(P29, P29_1, P30, P31, P32, P33, P33_1, P33_2, P33_3, P33_4, P33_5, P33_6, P33_7);
        insert5.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM, PREFIX_ONTOCAPE_MATERIAL_SUBSTANCE);
        kbClient.executeUpdate(insert5.getQueryString());

        //instantiate damper state sensor
        TriplePattern P34 = iri(DamperStateSensor_Instance).isA(StatusSensor);
        TriplePattern P34_1 = iri(DamperStateSensor_Instance).has(label, CAV_label  + " Damper State Sensor");
        TriplePattern P35 = iri(DamperStateSensor_Instance).has(sendsSignalTo, iri(CAV_Instance));
        TriplePattern P36 = iri(DamperStateSensor_Instance).has(observes, iri(DamperState_IRI));
        TriplePattern P37 = iri(DamperState_IRI).isA(DamperState);
        TriplePattern P37_1 = iri(DamperState_IRI).has(label, DamperState_IRI_label);
        InsertDataQuery insert6 = Queries.INSERT_DATA(P34, P34_1, P35, P36, P37, P37_1);
        insert6.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert6.getQueryString());
    }

    public void instantiatePipe() throws FileNotFoundException {
        String Pipe_label = null;
        String CHW_label = null;
        String TempSetpoint_IRI = null;
        String TempSetpoint_IRI_label = null;
        String PressureSetpoint_IRI = null;
        String PressureSetpoint_IRI_label = null;
        String WaterFlowRate_IRI = null;
        String WaterFlowRate_unit = null;
        String WaterFlowRate_label = null;
        String PressureSensor_label = null;
        String Pressure_IRI = null;
        String Pressure_label = null;
        String TemperatureSensor_label = null;
        String Temperature_IRI = null;
        String Temperature_label = null;

        File file = new File(csvFilePath);
        sc = new Scanner(file);

        //Retrieve each of the necessary labels and IRIs
        while (sc.hasNext()) {
            String string = sc.nextLine();

            String key = string.split(",")[0];
            LOGGER.info("The key is " + key + " and the string is " + string);
            switch (key) {
                case "Pipe_label":
                    Pipe_label = string.split(",")[1];
                    break;
                case "CHW_label":
                    CHW_label = string.split(",")[1];
                    break;
                case "TempSetpoint_IRI":
                    TempSetpoint_IRI = string.split(",")[1];
                    break;
                case "TempSetpoint_IRI_label":
                    TempSetpoint_IRI_label = string.split(",")[1];
                    break;
                case "PressureSetpoint_IRI":
                    PressureSetpoint_IRI = string.split(",")[1];
                    break;
                 case "PressureSetpoint_IRI_label":
                    PressureSetpoint_IRI_label = string.split(",")[1];
                    break;
                case "WaterFlowRate_IRI":
                    WaterFlowRate_IRI = string.split(",")[1];
                    break;
                case "WaterFlowRate_unit":
                    WaterFlowRate_unit = string.split(",")[1];
                    break;
                case "WaterFlowRate_label":
                    WaterFlowRate_label = string.split(",")[1];
                    break;
                case "PressureSensor_label":
                    PressureSensor_label = string.split(",")[1];
                    break;
                case "Pressure_IRI":
                    Pressure_IRI = string.split(",")[1];
                    break;
                case "Pressure_label":
                    Pressure_label = string.split(",")[1];
                    break;
                case "TemperatureSensor_label":
                    TemperatureSensor_label = string.split(",")[1];
                    break;
                case "Temperature_IRI":
                    Temperature_IRI = string.split(",")[1];
                    break;
                case "Temperature_label":
                    Temperature_label = string.split(",")[1];
                    break;
            }
        }

        //instantiate the Pipe instance
        //include label
        String Pipe_Instance = "https://www.theworldavatar.com/kg/ontobms/" + Pipe_label + "_" + UUID.randomUUID();
        TriplePattern P1 = iri(Pipe_Instance).isA(Pipe);
        TriplePattern P2 = iri(Pipe_Instance).has(label, Pipe_label);
        String CHW_Instance = "https://www.theworldavatar.com/kg/ontobms/" + CHW_label;
        TriplePattern P3 = iri(Pipe_Instance).has(hasInput_ontobms, iri(CHW_Instance));
        InsertDataQuery insert = Queries.INSERT_DATA(P1, P2, P3);
        insert.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert.getQueryString());

        //instantiate temp setpoint if tempsetpoint iri provided
        if (TempSetpoint_IRI != null) {
            String TempSetpoint_Instance = "https://www.theworldavatar.com/kg/ontobms/TempSetpoint_" + UUID.randomUUID();
            TriplePattern P4 = iri(Pipe_Instance).has(hasSetpoint, iri(TempSetpoint_Instance));
            TriplePattern P5 = iri(TempSetpoint_Instance).isA(Setpoint);
            String QTempSetpoint_Instance = "https://www.theworldavatar.com/kg/ontobms/Q_TempSetpoint_" + UUID.randomUUID();
            TriplePattern P6 = iri(TempSetpoint_Instance).has(hasQuantity, iri(QTempSetpoint_Instance));
            TriplePattern P7 = iri(QTempSetpoint_Instance).isA(Temperature);
            TriplePattern P8 = iri(QTempSetpoint_Instance).has(hasValue, iri(TempSetpoint_IRI));
            TriplePattern P9 = iri(TempSetpoint_IRI).isA(Measure);
            TriplePattern P10 = iri(TempSetpoint_IRI).has(label, TempSetpoint_IRI_label);
            InsertDataQuery insert1 = Queries.INSERT_DATA(P4, P5, P6, P7, P8, P9, P10);
            insert1.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
            kbClient.executeUpdate(insert1.getQueryString());
        }

        //instantiate pressure setpoint if pressuresetpoint iri provided
        if (PressureSetpoint_IRI != null) {
            String PressureSetpoint_Instance = "https://www.theworldavatar.com/kg/ontobms/PressureSetpoint_" + UUID.randomUUID();
            TriplePattern P10 = iri(Pipe_Instance).has(hasSetpoint, iri(PressureSetpoint_Instance));
            TriplePattern P11 = iri(PressureSetpoint_Instance).isA(Setpoint);
            String QPressureSetpoint_Instance =  "https://www.theworldavatar.com/kg/ontobms/Q_PressureSetpoint_" + UUID.randomUUID();
            TriplePattern P12 = iri(PressureSetpoint_Instance).has(hasQuantity, iri(QPressureSetpoint_Instance));
            TriplePattern P13 = iri(QPressureSetpoint_Instance).isA(Pressure);
            TriplePattern P14 = iri(QPressureSetpoint_Instance).has(hasValue, iri(PressureSetpoint_IRI));
            TriplePattern P15 = iri(PressureSetpoint_IRI).isA(Measure);
            TriplePattern P16 = iri(PressureSetpoint_IRI).has(label, PressureSetpoint_IRI_label);
            InsertDataQuery insert2 = Queries.INSERT_DATA(P10, P11, P12, P13, P14, P15, P16);
            insert2.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
            kbClient.executeUpdate(insert2.getQueryString());
        }

        //instantiate CHW
        TriplePattern P16 = iri(CHW_Instance).isA(ProcessStream);
        String GeneralizedAmountCHW_Instance = "https://www.theworldavatar.com/kg/ontobms/GeneralizedAmount_" + CHW_label;
        TriplePattern P17 = iri(CHW_Instance).has(refersToGeneralizedAmount, iri(GeneralizedAmountCHW_Instance));
        TriplePattern P18 = iri(GeneralizedAmountCHW_Instance).isA(GeneralizedAmount);
        String MaterialAmountCHW_Instance = "https://www.theworldavatar.com/kg/ontobms/MaterialAmount_" + CHW_label;
        TriplePattern P19 = iri(GeneralizedAmountCHW_Instance).has(hasSubsystem, iri(MaterialAmountCHW_Instance));
        TriplePattern P20 = iri(MaterialAmountCHW_Instance).isA(ContinuousMaterialAmount);
        String MaterialCHW_Instance = "https://www.theworldavatar.com/kg/ontobms/Material_" + CHW_label;
        TriplePattern P21 = iri(MaterialAmountCHW_Instance).has(refersToMaterial, iri(MaterialCHW_Instance));
        InsertDataQuery insert3 = Queries.INSERT_DATA(P16, P17, P18, P19, P20, P21);
        insert3.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM, PREFIX_ONTOCAPE_CPS_FUNCTION_PROCESS, PREFIX_ONTOCAPE_SYSTEM, PREFIX_ONTOCAPE_CPS, PREFIX_ONTOCAPE_CPS_BEHAVIOR);
        kbClient.executeUpdate(insert3.getQueryString());

        //instantiate volumetricF CHW if waterflowrate iri and waterflowrate unit are provided
        if (WaterFlowRate_IRI != null && WaterFlowRate_unit != null) {
            String VolumetricFCHW_Instance = "https://www.theworldavatar.com/kg/ontobms/VolumetricF_" + CHW_label;
            TriplePattern P22 = iri(MaterialAmountCHW_Instance).has(hasProperty, iri(VolumetricFCHW_Instance));
            TriplePattern P23 = iri(VolumetricFCHW_Instance).isA(ConvectiveTransportRate);
            String QCHW_Instance = "https://www.theworldavatar.com/kg/ontobms/Q_" + CHW_label;
            TriplePattern P24 = iri(VolumetricFCHW_Instance).has(hasWaterFlowRate, iri(QCHW_Instance));
            TriplePattern P25 = iri(QCHW_Instance).isA(VolumetricFlowRate);
            TriplePattern P26 = iri(QCHW_Instance).has(hasValue, iri(WaterFlowRate_IRI));
            TriplePattern P27 = iri(WaterFlowRate_IRI).isA(Measure);

            InsertDataQuery insert4 = Queries.INSERT_DATA(P22, P23, P24, P25, P26, P27);
            insert4.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM, PREFIX_ONTOCAPE_SYSTEM, PREFIX_ONTOCAPE_CPS_BEHAVIOR);
            kbClient.executeUpdate(insert4.getQueryString());

            if (WaterFlowRate_unit.equals("cubicMetrePerHour")) {
                TriplePattern P27_1 = iri(WaterFlowRate_IRI).has(hasUnit, cubicMetrePerHour);
                TriplePattern P27_2 = cubicMetrePerHour.isA(UnitDivision);
                TriplePattern P27_3 = cubicMetrePerHour.has(symbol, "m3/h");
                TriplePattern P27_4 = cubicMetrePerHour.has(label, "cubic metre per hour");
                InsertDataQuery insert5 = Queries.INSERT_DATA(P27_1, P27_2, P27_3, P27_4);
                insert5.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
                kbClient.executeUpdate(insert5.getQueryString());
            } else if (WaterFlowRate_unit.equals("litrePerMinute")) {
                TriplePattern P27_1 = iri(WaterFlowRate_IRI).has(hasUnit, litrePerMinute);
                TriplePattern P27_2 = litrePerMinute.isA(UnitDivision);
                TriplePattern P27_3 = litrePerMinute.has(symbol, "l/min");
                TriplePattern P27_4 = litrePerMinute.has(label, "litre per minute");
                InsertDataQuery insert5 = Queries.INSERT_DATA(P27_1, P27_2, P27_3, P27_4);
                insert5.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
                kbClient.executeUpdate(insert5.getQueryString());
            } else {
                LOGGER.error("Water flow rate hasUnit did not instantiate as WaterFLowRate_unit provided is invalid.");
            }

            if (WaterFlowRate_label != null) {
                TriplePattern P27_5 = iri(WaterFlowRate_IRI).has(label, WaterFlowRate_label);
                InsertDataQuery insert6 = Queries.INSERT_DATA(P27_5);
                insert6.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
                kbClient.executeUpdate(insert6.getQueryString());
            }

            //instantiate flow sensor
            String FlowSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/FlowSensor_" + UUID.randomUUID();
            TriplePattern P28 = iri(FlowSensor_Instance).isA(FlowSensor);
            TriplePattern P29 = iri(FlowSensor_Instance).has(isAttachedTo, iri(Pipe_Instance));
            TriplePattern P30 = iri(FlowSensor_Instance).has(measures, iri(QCHW_Instance));
            InsertDataQuery insert7 = Queries.INSERT_DATA(P28, P29, P30);
            insert7.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
            kbClient.executeUpdate(insert7.getQueryString());
        }

        //instantiate material CHW
        TriplePattern P31 = iri(MaterialCHW_Instance).isA(Material);
        String StreamPhaseCHW_Instance = "https://www.theworldavatar.com/kg/ontobms/StreamPhase_" + CHW_label;
        TriplePattern P32 = iri(MaterialCHW_Instance).has(thermodynamicBehavior, iri(StreamPhaseCHW_Instance));
        TriplePattern P33 = iri(StreamPhaseCHW_Instance).isA(SinglePhase);
        InsertDataQuery insert8 = Queries.INSERT_DATA(P31, P32, P33);
        insert8.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM, PREFIX_ONTOCAPE_MATERIAL, PREFIX_ONTOCAPE_PHASE_SYSTEM);
        kbClient.executeUpdate(insert8.getQueryString());

        //instantiate pressure and pressure sensor if pressure iri and pressuresensor label provided
        if (Pressure_IRI != null && PressureSensor_label != null) {
            String Pressure_Instance = "https://www.theworldavatar.com/kg/ontobms/Pressure_" + UUID.randomUUID();
            TriplePattern P34 = iri(StreamPhaseCHW_Instance).has(hasPressure, iri(Pressure_Instance));
            TriplePattern P35 = iri(Pressure_Instance).isA(Pressure);
            TriplePattern P36 = iri(Pressure_Instance).has(hasValue, iri(Pressure_IRI));
            TriplePattern P37 = iri(Pressure_IRI).isA(Measure);
            String PressureSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/PressureSensor_" + UUID.randomUUID();
            TriplePattern P38 = iri(PressureSensor_Instance).isA(PressureSensor);
            TriplePattern P39 = iri(PressureSensor_Instance).has(label, PressureSensor_label);
            TriplePattern P40 = iri(PressureSensor_Instance).has(measures, iri(Pressure_Instance));
            TriplePattern P41 = iri(PressureSensor_Instance).has(isAttachedTo, iri(Pipe_Instance));
            InsertDataQuery insert9 = Queries.INSERT_DATA(P34, P35, P36, P37, P38, P39, P40, P41);
            insert9.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
            kbClient.executeUpdate(insert9.getQueryString());

            if (Pressure_label != null) {
                TriplePattern P42 = iri(Pressure_Instance).has(label, Pressure_label);
                TriplePattern P43 = iri(Pressure_IRI).has(label, Pressure_label);
                InsertDataQuery insert10 = Queries.INSERT_DATA(P42, P43);
                insert10.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
                kbClient.executeUpdate(insert10.getQueryString());
            }
        }

        //instantiate temperature and temperature sensor if temperature and temperaturesensor label provided
        if (Temperature != null && TemperatureSensor_label != null) {
            String Temp_Instance = "https://www.theworldavatar.com/kg/ontobms/Temp_" + UUID.randomUUID();
            TriplePattern P43 = iri(StreamPhaseCHW_Instance).has(hasTemperature, iri(Temp_Instance));
            TriplePattern P44 = iri(Temp_Instance).isA(Temperature);
            TriplePattern P45 = iri(Temp_Instance).has(hasValue, iri(Temperature_IRI));
            TriplePattern P46 = iri(Temperature_IRI).isA(Measure);
            String TemperatureSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/TemperatureSensor_" + UUID.randomUUID();
            TriplePattern P47 = iri(TemperatureSensor_Instance).isA(TemperatureSensor);
            TriplePattern P48 = iri(TemperatureSensor_Instance).has(label, TemperatureSensor_label);
            TriplePattern P49 = iri(TemperatureSensor_Instance).has(isAttachedTo, iri(Pipe_Instance));
            TriplePattern P50 = iri(TemperatureSensor_Instance).has(measures, iri(Temp_Instance));
            InsertDataQuery insert11 = Queries.INSERT_DATA(P43, P44, P45, P46, P47, P48, P49, P50);
            insert11.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
            kbClient.executeUpdate(insert11.getQueryString());

            if (Temperature_label != null) {
                TriplePattern P51 = iri(Temp_Instance).has(label, Temperature_label);
                TriplePattern P52 = iri(Temperature_IRI).has(label, Temperature_label);
                InsertDataQuery insert12 = Queries.INSERT_DATA(P51, P52);
                insert12.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
                kbClient.executeUpdate(insert12.getQueryString());
            }
        }
    }

    public void instantiateDuct() throws FileNotFoundException {
        String Duct_label = null;
        String St_name = null;
        String TempSetpoint_IRI = null;
        String TempSetpoint_IRI_label = null;
        String PressureSetpoint_IRI = null;
        String PressureSetpoint_IRI_label = null;
        String Q_TotalSupplyAirflow_name = null;
        String TotalSupplyAirflow_label = null;
        String TotalSupplyAirflow_IRI = null;
        String Pressure_IRI_label = null;
        String Pressure_IRI = null;
        String PressureSensor_label = null;
        String Temperature_IRI = null;
        String Temperature_IRI_label = null;
        String TemperatureSensor_label = null;
        String Humidity_IRI = null;
        String HumiditySensor_label = null;
        String Humidity_IRI_label = null;

        File file = new File(csvFilePath);
        sc = new Scanner(file);

        // Retrieve each of the necessary labels and IRIs
        while (sc.hasNext()) {
            String string = sc.nextLine();

            String key = string.split(",")[0];
            LOGGER.info("The key is " + key + " and the string is " + string);
            switch (key) {
                case "Duct_label":
                    Duct_label = string.split(",")[1];
                    break;
                case "St_name":
                    St_name = string.split(",")[1];
                    break;
                case "TempSetpoint_IRI":
                    TempSetpoint_IRI = string.split(",")[1];
                    break;
                case "TempSetpoint_IRI_label":
                    TempSetpoint_IRI_label = string.split(",")[1];
                    break;
                case "PressureSetpoint_IRI":
                    PressureSetpoint_IRI = string.split(",")[1];
                    break;
                case "PressureSetpoint_IRI_label":
                    PressureSetpoint_IRI_label = string.split(",")[1];
                    break;
                case "Q_TotalSupplyAirflow_name":
                    Q_TotalSupplyAirflow_name = string.split(",")[1];
                    break;
                case "TotalSupplyAirflow_label":
                    TotalSupplyAirflow_label = string.split(",")[1];
                    break;
                case "TotalSupplyAirflow_IRI":
                    TotalSupplyAirflow_IRI = string.split(",")[1];
                    break;
                case "Pressure_IRI_label":
                    Pressure_IRI_label = string.split(",")[1];
                    break;
                case "Pressure_IRI":
                    Pressure_IRI = string.split(",")[1];
                    break;
                case "PressureSensor_label":
                    PressureSensor_label = string.split(",")[1];
                    break;
                case "Temperature_IRI":
                    Temperature_IRI = string.split(",")[1];
                    break;
                case "Temperature_IRI_label":
                    Temperature_IRI_label = string.split(",")[1];
                    break;
                case "TemperatureSensor_label":
                    TemperatureSensor_label = string.split(",")[1];
                    break;
                case "Humidity_IRI":
                    Humidity_IRI = string.split(",")[1];
                    break;
                case "Humidity_IRI_label":
                    Humidity_IRI_label = string.split(",")[1];
                    break;
                case "HumiditySensor_label":
                    HumiditySensor_label = string.split(",")[1];
                    break;
            }
        }

        //instantiate the Duct instance
        //include label
        String Duct_Instance = "https://www.theworldavatar.com/kg/ontobms/" + Duct_label + "_" + UUID.randomUUID();
        TriplePattern P1 = iri(Duct_Instance).isA(Duct);
        TriplePattern P2 = iri(Duct_Instance).has(label, Duct_label);
        String St_Instance = "https://www.theworldavatar.com/kg/ontobms/" + St_name;
        TriplePattern P3 = iri(Duct_Instance).has(hasInput_ontobms, iri(St_Instance));
        InsertDataQuery insert = Queries.INSERT_DATA(P1, P2, P3);
        insert.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert.getQueryString());

        //instantiate supply temp setpoint if tempsetpoint iri provided
        if (TempSetpoint_IRI != null) {
            String TempSetpoint_Instance = "https://www.theworldavatar.com/kg/ontobms/TempSetpoint_" + UUID.randomUUID();
            TriplePattern P4 = iri(Duct_Instance).has(hasSetpoint, iri(TempSetpoint_Instance));
            TriplePattern P5 = iri(TempSetpoint_Instance).isA(Setpoint);
            String QTempSetpoint_Instance = "https://www.theworldavatar.com/kg/ontobms/Q_TempSetpoint_" + UUID.randomUUID();
            TriplePattern P6 = iri(TempSetpoint_Instance).has(hasQuantity, iri(QTempSetpoint_Instance));
            TriplePattern P7 = iri(QTempSetpoint_Instance).isA(Temperature);
            TriplePattern P8 = iri(QTempSetpoint_Instance).has(hasValue, iri(TempSetpoint_IRI));
            TriplePattern P9 = iri(TempSetpoint_IRI).isA(Measure);
            TriplePattern P10 = iri(TempSetpoint_IRI).has(label, TempSetpoint_IRI_label);
            InsertDataQuery insert1 = Queries.INSERT_DATA(P4, P5, P6, P7, P8, P9, P10);
            insert1.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
            kbClient.executeUpdate(insert1.getQueryString());
        }

        //instantiate supply pressure setpoint if pressuresetpoint iri provided
        if (PressureSetpoint_IRI != null) {
            String PressureSetpoint_Instance = "https://www.theworldavatar.com/kg/ontobms/PressureSetpoint_" + UUID.randomUUID();
            TriplePattern P10 = iri(Duct_Instance).has(hasSetpoint, iri(PressureSetpoint_Instance));
            TriplePattern P11 = iri(PressureSetpoint_Instance).isA(Setpoint);
            String QPressureSetpoint_Instance = "https://www.theworldavatar.com/kg/ontobms/Q_PressureSetpoint_" + UUID.randomUUID();
            TriplePattern P12 = iri(PressureSetpoint_Instance).has(hasQuantity, iri(QPressureSetpoint_Instance));
            TriplePattern P13 = iri(QPressureSetpoint_Instance).isA(Pressure);
            TriplePattern P14 = iri(QPressureSetpoint_Instance).has(hasValue, iri(PressureSetpoint_IRI));
            TriplePattern P15 = iri(PressureSetpoint_IRI).isA(Measure);
            TriplePattern P16 = iri(PressureSetpoint_IRI).has(label, PressureSetpoint_IRI_label);
            InsertDataQuery insert2 = Queries.INSERT_DATA(P10, P11, P12, P13, P14, P15, P16);
            insert2.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
            kbClient.executeUpdate(insert2.getQueryString());
        }

        //instantiate st
        TriplePattern P16 = iri(St_Instance).isA(ProcessStream);
        String GeneralizedAmountSt_Instance = "https://www.theworldavatar.com/kg/ontobms/GeneralizedAmount_" + St_name;
        TriplePattern P17 = iri(St_Instance).has(refersToGeneralizedAmount, iri(GeneralizedAmountSt_Instance));
        TriplePattern P18 = iri(GeneralizedAmountSt_Instance).isA(GeneralizedAmount);
        String MaterialAmountSt_Instance = "https://www.theworldavatar.com/kg/ontobms/MaterialAmount_" + St_name;
        TriplePattern P19 = iri(GeneralizedAmountSt_Instance).has(hasSubsystem, iri(MaterialAmountSt_Instance));
        TriplePattern P20 = iri(MaterialAmountSt_Instance).isA(ContinuousMaterialAmount);
        String MaterialSt_Instance = "https://www.theworldavatar.com/kg/ontobms/Material_" + St_name;
        TriplePattern P21 = iri(MaterialAmountSt_Instance).has(refersToMaterial, iri(MaterialSt_Instance));
        InsertDataQuery insert3 = Queries.INSERT_DATA(P16, P17, P18, P19, P20, P21);
        insert3.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM, PREFIX_ONTOCAPE_CPS_FUNCTION_PROCESS, PREFIX_ONTOCAPE_SYSTEM, PREFIX_ONTOCAPE_CPS, PREFIX_ONTOCAPE_CPS_BEHAVIOR);
        kbClient.executeUpdate(insert3.getQueryString());

        //instantiate volumetricF St if qtotalsupplyairflow name, totalsupplyairflow label and totalsupplyairflow iri are provided
        if (TotalSupplyAirflow_label != null && TotalSupplyAirflow_IRI != null) {
            String VolumetricFSt_Instance = "https://www.theworldavatar.com/kg/ontobms/VolumetricF_" + St_name;
            TriplePattern P22 = iri(MaterialAmountSt_Instance).has(hasProperty, iri(VolumetricFSt_Instance));
            TriplePattern P23 = iri(VolumetricFSt_Instance).isA(ConvectiveTransportRate);
            String QTotalSupplyAirflow_Instance = "https://www.theworldavatar.com/kg/ontobms/" + Q_TotalSupplyAirflow_name;
            TriplePattern P24 = iri(VolumetricFSt_Instance).has(hasAirFlowRate, iri(QTotalSupplyAirflow_Instance));
            TriplePattern P25 = iri(QTotalSupplyAirflow_Instance).isA(VolumetricFlowRate);
            TriplePattern P26 = iri(QTotalSupplyAirflow_Instance).has(hasValue, iri(TotalSupplyAirflow_IRI));
            TriplePattern P27 = iri(TotalSupplyAirflow_IRI).isA(Measure);
            TriplePattern P27_1 = iri(TotalSupplyAirflow_IRI).has(label, TotalSupplyAirflow_label);
            TriplePattern P27_2 = iri(TotalSupplyAirflow_IRI).has(hasUnit, cubicMetrePerHour);
            TriplePattern P27_3 = cubicMetrePerHour.isA(UnitDivision);
            TriplePattern P27_4 = cubicMetrePerHour.has(symbol, "m3/h");
            TriplePattern P27_5 = cubicMetrePerHour.has(label, "cubic metre per hour");
            InsertDataQuery insert4 = Queries.INSERT_DATA(P22, P23, P24, P25, P26, P27, P27_1, P27_2, P27_3, P27_4, P27_5);
            insert4.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM, PREFIX_ONTOCAPE_SYSTEM, PREFIX_ONTOCAPE_CPS_BEHAVIOR);
            kbClient.executeUpdate(insert4.getQueryString());

            //instantiate flow sensor
            String FlowSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/FlowSensor_" + UUID.randomUUID();
            TriplePattern P28 = iri(FlowSensor_Instance).isA(FlowSensor);
            TriplePattern P29 = iri(FlowSensor_Instance).has(isAttachedTo, iri(Duct_Instance));
            TriplePattern P30 = iri(FlowSensor_Instance).has(measures, iri(QTotalSupplyAirflow_Instance));
            InsertDataQuery insert5 = Queries.INSERT_DATA(P28, P29, P30);
            insert5.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
            kbClient.executeUpdate(insert5.getQueryString());
        }

        //instantiate material St
        TriplePattern P31 = iri(MaterialSt_Instance).isA(Material);
        String StreamPhaseSt_Instance = "https://www.theworldavatar.com/kg/ontobms/StreamPhase_" + St_name;
        TriplePattern P32 = iri(MaterialSt_Instance).has(thermodynamicBehavior, iri(StreamPhaseSt_Instance));
        TriplePattern P33 = iri(StreamPhaseSt_Instance).isA(SinglePhase);
        InsertDataQuery insert6 = Queries.INSERT_DATA(P31, P32, P33);
        insert6.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM, PREFIX_ONTOCAPE_MATERIAL, PREFIX_ONTOCAPE_PHASE_SYSTEM);
        kbClient.executeUpdate(insert6.getQueryString());

        //instantiate pressure and pressure sensor if pressure label, pressure iri and pressuresensor label are provided
        if (Pressure_IRI_label != null && Pressure_IRI != null && PressureSensor_label != null) {
            String Pressure_Instance = "https://www.theworldavatar.com/kg/ontobms/Pressure_" + UUID.randomUUID();
            TriplePattern P34 = iri(StreamPhaseSt_Instance).has(hasPressure, iri(Pressure_Instance));
            TriplePattern P35 = iri(Pressure_Instance).isA(Pressure);
            TriplePattern P36 = iri(Pressure_Instance).has(hasValue, iri(Pressure_IRI));
            TriplePattern P37 = iri(Pressure_IRI).isA(Measure);
            TriplePattern P38 = iri(Pressure_IRI).has(label, Pressure_IRI_label);
            String PressureSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/PressureSensor_" + UUID.randomUUID();
            TriplePattern P39 = iri(PressureSensor_Instance).isA(PressureSensor);
            TriplePattern P40 = iri(PressureSensor_Instance).has(label, PressureSensor_label);
            TriplePattern P41 = iri(PressureSensor_Instance).has(measures, iri(Pressure_Instance));
            TriplePattern P42 = iri(PressureSensor_Instance).has(isAttachedTo, iri(Duct_Instance));
            InsertDataQuery insert7 = Queries.INSERT_DATA(P34, P35, P36, P37, P38, P39, P40, P41, P42);
            insert7.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
            kbClient.executeUpdate(insert7.getQueryString());
        }

        //instantiate temperature and temperture sensor if temperature iri and temperaturesensor label are provided
        if (Temperature_IRI != null && TempSetpoint_IRI_label != null && TemperatureSensor_label != null) {
            String Temperature_Instance = "https://www.theworldavatar.com/kg/ontobms/Temperature_" + UUID.randomUUID();
            TriplePattern P43 = iri(StreamPhaseSt_Instance).has(hasTemperature, iri(Temperature_Instance));
            TriplePattern P44 = iri(Temperature_Instance).isA(Temperature);
            TriplePattern P45 = iri(Temperature_Instance).has(hasValue, iri(Temperature_IRI));
            TriplePattern P46 = iri(Temperature_IRI).isA(Measure);
            TriplePattern P46_1 = iri(Temperature_IRI).has(label, Temperature_IRI_label);
            String TemperatureSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/TemperatureSensor_" + UUID.randomUUID();
            TriplePattern P47 = iri(TemperatureSensor_Instance).isA(TemperatureSensor);
            TriplePattern P48 = iri(TemperatureSensor_Instance).has(label, TemperatureSensor_label);
            TriplePattern P49 = iri(TemperatureSensor_Instance).has(isAttachedTo, iri(Duct_Instance));
            TriplePattern P50 = iri(TemperatureSensor_Instance).has(measures, iri(Temperature_Instance));
            InsertDataQuery insert8 = Queries.INSERT_DATA(P43, P44, P45, P46, P46_1, P47, P48, P49, P50);
            insert8.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
            kbClient.executeUpdate(insert8.getQueryString());
        }

        //instantiate humidity and humidity sensor if humidity iri and humiditysensor label are provided
        if (Humidity_IRI != null && Humidity_IRI_label != null && HumiditySensor_label != null) {
            String Humidity_Instance = "https://www.theworldavatar.com/kg/ontobms/Humidity_" + UUID.randomUUID();
            TriplePattern P51 = iri(StreamPhaseSt_Instance).has(hasRelativeHumidity, iri(Humidity_Instance));
            TriplePattern P52 = iri(Humidity_Instance).isA(RelativeHumidity);
            TriplePattern P53 = iri(Humidity_Instance).has(hasValue, iri(Humidity_IRI));
            TriplePattern P54 = iri(Humidity_IRI).isA(Measure);
            TriplePattern P54_1 = iri(Humidity_IRI).has(label, Humidity_IRI_label);
            String HumiditySensor_Instance = "https://www.theworldavatar.com/kg/ontobms/HumiditySensor_" + UUID.randomUUID();
            TriplePattern P55 = iri(HumiditySensor_Instance).isA(HumiditySensor);
            TriplePattern P56 = iri(HumiditySensor_Instance).has(label, HumiditySensor_label);
            TriplePattern P57 = iri(HumiditySensor_Instance).has(isAttachedTo, iri(Duct_Instance));
            TriplePattern P58 = iri(HumiditySensor_Instance).has(measures, iri(Humidity_Instance));
            InsertDataQuery insert9 = Queries.INSERT_DATA(P51, P52, P53, P54, P54_1, P55, P56, P57, P58);
            insert9.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
            kbClient.executeUpdate(insert9.getQueryString());
        }
    }

    public void instantiateValve() throws FileNotFoundException {
        String Valve_label = null;
        String ControlPercentage_label = null;
        String ControlPercentage_IRI = null;
        String ControlSensor_name = null;
        String ControlSensor_label = null;
        String FeedbackPercentage_label = null;
        String FeedbackPercentage_IRI = null;
        String FeedbackSensor_name = null;
        String FeedbackSensor_label = null;
        String Pipe1_IRI = null;
        String Pipe2_IRI = null;
        String Pipe3_IRI = null;

        File file = new File(csvFilePath);
        sc = new Scanner(file);

        //Retrieve each of the necessary labels and IRIs
        while (sc.hasNext()) {
            String string = sc.nextLine();

            String key = string.split(",")[0];
            LOGGER.info("The key is " + key + " and the string is " + string);
            switch (key) {
                case "Valve_label":
                    Valve_label = string.split(",")[1];
                    break;
                case "ControlPercentage_label":
                    ControlPercentage_label = string.split(",")[1];
                    break;
                case "ControlPercentage_IRI":
                    ControlPercentage_IRI = string.split(",")[1];
                    break;
                case "ControlSensor_name":
                    ControlSensor_name = string.split(",")[1];
                    break;
                case "ControlSensor_label":
                    ControlSensor_label = string.split(",")[1];
                    break;
                case "FeedbackPercentage_label":
                    FeedbackPercentage_label = string.split(",")[1];
                    break;
                case "FeedbackPercentage_IRI":
                    FeedbackPercentage_IRI = string.split(",")[1];
                    break;
                case "FeedbackSensor_name":
                    FeedbackSensor_name = string.split(",")[1];
                    break;
                case "FeedbackSensor_label":
                    FeedbackSensor_label = string.split(",")[1];
                    break;
                case "Pipe1_IRI":
                    Pipe1_IRI = string.split(",")[1];
                    break;
                case "Pipe2_IRI":
                    Pipe2_IRI = string.split(",")[1];
                    break;
                case "Pipe3_IRI":
                    Pipe3_IRI = string.split(",")[1];
                    break;
            }
        }

        //instantiate the valve instance
        //include label
        String Valve_Instance = "https://www.theworldavatar.com/kg/ontobms/Valve_" + UUID.randomUUID();
        TriplePattern P1 = iri(Valve_Instance).isA(Valve);
        TriplePattern P2 = iri(Valve_Instance).has(label, Valve_label);
        String ControlPercentage_Instance = "https://www.theworldavatar.com/kg/ontobms/ControlPercentage_" + UUID.randomUUID();
        TriplePattern P3 = iri(Valve_Instance).has(hasControlParameter, iri(ControlPercentage_Instance));
        String FeedbackPercentage_Instance = "https://www.theworldavatar.com/kg/ontobms/FeedbackPercentage_" + UUID.randomUUID();
        TriplePattern P4 = iri(Valve_Instance).has(hasFeedbackParameter, iri(FeedbackPercentage_Instance));
        TriplePattern P5 = iri(Pipe1_IRI).has(isConnectedTo, iri(Valve_Instance));
        TriplePattern P5_1 = iri(Valve_Instance).has(isConnectedTo, iri(Pipe1_IRI));
        TriplePattern P6 = iri(Pipe2_IRI).has(isConnectedTo, iri(Valve_Instance));
        TriplePattern P6_1 = iri(Valve_Instance).has(isConnectedTo, iri(Pipe2_IRI));
        InsertDataQuery insert = Queries.INSERT_DATA(P1, P2, P3, P4, P5, P5_1, P6, P6_1);
        insert.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM, PREFIX_S4BLDG);
        kbClient.executeUpdate(insert.getQueryString());

        if (Pipe3_IRI != null) {
            TriplePattern P7 = iri(Pipe3_IRI).has(isConnectedTo, iri(Valve_Instance));
            TriplePattern P7_1 = iri(Valve_Instance).has(isConnectedTo, iri(Pipe3_IRI));
            InsertDataQuery insert1 = Queries.INSERT_DATA(P7, P7_1);
            insert1.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
            kbClient.executeUpdate(insert1.getQueryString());
        }

        //instantiate control percentage and control sensor
        TriplePattern P8 = iri(ControlPercentage_Instance).isA(Percentage);
        TriplePattern P9 = iri(ControlPercentage_Instance).has(label, ControlPercentage_label);
        TriplePattern P10 = iri(ControlPercentage_Instance).has(hasValue, iri(ControlPercentage_IRI));
        TriplePattern P11 = iri(ControlPercentage_IRI).isA(Measure);
        TriplePattern P11_1 = iri(ControlPercentage_IRI).has(label, ControlPercentage_label);
        String ControlSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/" + ControlSensor_name  + "_" + UUID.randomUUID();
        TriplePattern P12 = iri(ControlSensor_Instance).isA(ControlSensor);
        TriplePattern P13 = iri(ControlSensor_Instance).has(label, ControlSensor_label);
        TriplePattern P14 = iri(ControlSensor_Instance).has(measures, iri(ControlPercentage_Instance));
        TriplePattern P15 = iri(ControlSensor_Instance).has(isAttachedTo, iri(Valve_Instance));
        InsertDataQuery insert2 = Queries.INSERT_DATA(P8, P9, P10, P11, P11_1, P12, P13, P14, P15);
        insert2.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert2.getQueryString());

        //instantiate feedback percentage and feedback sensor
        TriplePattern P16 = iri(FeedbackPercentage_Instance).isA(Percentage);
        TriplePattern P17 = iri(FeedbackPercentage_Instance).has(label, FeedbackPercentage_label);
        TriplePattern P18 = iri(FeedbackPercentage_Instance).has(hasValue, iri(FeedbackPercentage_IRI));
        TriplePattern P19 = iri(FeedbackPercentage_IRI).isA(Measure);
        TriplePattern P19_1 = iri(FeedbackPercentage_IRI).has(label, FeedbackPercentage_label);
        String FeedbackSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/" + FeedbackSensor_name + "_" + UUID.randomUUID();
        TriplePattern P20 = iri(FeedbackSensor_Instance).isA(FeedbackSensor);
        TriplePattern P21 = iri(FeedbackSensor_Instance).has(label, FeedbackSensor_label);
        TriplePattern P22 = iri(FeedbackSensor_Instance).has(measures, iri(FeedbackPercentage_Instance));
        TriplePattern P23 = iri(FeedbackSensor_Instance).has(isAttachedTo, iri(Valve_Instance));
        InsertDataQuery insert3 = Queries.INSERT_DATA(P16, P17, P18, P19, P19_1, P20, P21, P22, P23);
        insert3.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert3.getQueryString());
    }

    public void instantiatePreCoolCoil() throws FileNotFoundException {
        String PreCoolCoil_label = null;
        String Pipe1_IRI = null;
        String Pipe2_IRI = null;
        String Duct1_IRI = null;
        String Duct2_IRI = null;

        File file = new File(csvFilePath);
        sc = new Scanner(file);

        //Retrieve each of the necessary labels and IRIs
        while (sc.hasNext()) {
            String string = sc.nextLine();

            String key = string.split(",")[0];
            LOGGER.info("The key is " + key + " and the string is " + string);
            switch (key) {
                case "PreCoolCoil_label":
                    PreCoolCoil_label = string.split(",")[1];
                    break;
                case "Pipe1_IRI":
                    Pipe1_IRI = string.split(",")[1];
                    break;
                case "Pipe2_IRI":
                    Pipe2_IRI = string.split(",")[1];
                    break;
                case "Duct1_IRI":
                    Duct1_IRI = string.split(",")[1];
                    break;
                case "Duct2_IRI":
                    Duct2_IRI = string.split(",")[1];
                    break;
            }
        }

        //instantiate the pre cool coil instance
        //include label
        String PreCoolCoil_Instance = "https://www.theworldavatar.com/kg/ontobms/PreCoolCoil_" + UUID.randomUUID();
        TriplePattern P1 = iri(PreCoolCoil_Instance).isA(WaterCoil);
        TriplePattern P2 = iri(PreCoolCoil_Instance).has(label, PreCoolCoil_label);
        TriplePattern P3 = iri(Pipe1_IRI).has(isConnectedTo, iri(PreCoolCoil_Instance));
        TriplePattern P3_1 = iri(PreCoolCoil_Instance).has(isConnectedTo, iri(Pipe1_IRI));
        TriplePattern P4 = iri(Pipe2_IRI).has(isConnectedTo, iri(PreCoolCoil_Instance));
        TriplePattern P4_1 = iri(PreCoolCoil_Instance).has(isConnectedTo, iri(Pipe2_IRI));
        TriplePattern P5 = iri(Duct1_IRI).has(isConnectedTo, iri(PreCoolCoil_Instance));
        TriplePattern P5_1 = iri(PreCoolCoil_Instance).has(isConnectedTo, iri(Duct1_IRI));
        TriplePattern P6 = iri(Duct2_IRI).has(isConnectedTo, iri(PreCoolCoil_Instance));
        TriplePattern P6_1 = iri(PreCoolCoil_Instance).has(isConnectedTo, iri(Duct2_IRI));
        InsertDataQuery insert = Queries.INSERT_DATA(P1, P2, P3, P3_1, P4, P4_1, P5, P5_1, P6, P6_1);
        insert.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert.getQueryString());
    }

    public void instantiateMidCoil() throws FileNotFoundException {
        String MidCoil_label = null;
        String Pipe1_IRI = null;
        String Pipe2_IRI = null;
        String Duct1_IRI = null;
        String Duct2_IRI = null;

        File file = new File(csvFilePath);
        sc = new Scanner(file);

        //Retrieve each of the necessary labels and IRIs
        while (sc.hasNext()) {
            String string = sc.nextLine();

            String key = string.split(",")[0];
            LOGGER.info("The key is " + key + " and the string is " + string);
            switch (key) {
                case "MidCoil_label":
                    MidCoil_label = string.split(",")[1];
                    break;
                case "Pipe1_IRI":
                    Pipe1_IRI = string.split(",")[1];
                    break;
                case "Pipe2_IRI":
                    Pipe2_IRI = string.split(",")[1];
                    break;
                case "Duct1_IRI":
                    Duct1_IRI = string.split(",")[1];
                    break;
                case "Duct2_IRI":
                    Duct2_IRI = string.split(",")[1];
                    break;
            }
        }

        //instantiate the mid coil instance
        //include label
        String MidCoil_Instance = "https://www.theworldavatar.com/kg/ontobms/MidCoil_" + UUID.randomUUID();
        TriplePattern P1 = iri(MidCoil_Instance).isA(WaterCoil);
        TriplePattern P2 = iri(MidCoil_Instance).has(label, MidCoil_label);
        TriplePattern P3 = iri(Pipe1_IRI).has(isConnectedTo, iri(MidCoil_Instance));
        TriplePattern P3_1 = iri(MidCoil_Instance).has(isConnectedTo, iri(Pipe1_IRI));
        TriplePattern P4 = iri(Pipe2_IRI).has(isConnectedTo, iri(MidCoil_Instance));
        TriplePattern P4_1 = iri(MidCoil_Instance).has(isConnectedTo, iri(Pipe2_IRI));
        TriplePattern P5 = iri(Duct1_IRI).has(isConnectedTo, iri(MidCoil_Instance));
        TriplePattern P5_1 = iri(MidCoil_Instance).has(isConnectedTo, iri(Duct1_IRI));
        TriplePattern P6 = iri(Duct2_IRI).has(isConnectedTo, iri(MidCoil_Instance));
        TriplePattern P6_1 = iri(MidCoil_Instance).has(isConnectedTo, iri(Duct2_IRI));
        InsertDataQuery insert = Queries.INSERT_DATA(P1, P2, P3, P3_1, P4, P4_1, P5, P5_1, P6, P6_1);
        insert.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert.getQueryString());
    }

    public void instantiateOffCoil() throws FileNotFoundException {
        String OffCoil_label = null;
        String Pipe1_IRI = null;
        String Pipe2_IRI = null;
        String Duct1_IRI = null;
        String Duct2_IRI = null;
        String TempSetpoint_IRI = null;

        File file = new File(csvFilePath);
        sc = new Scanner(file);

        //Retrieve each of the necessary labels and IRIs
        while (sc.hasNext()) {
            String string = sc.nextLine();

            String key = string.split(",")[0];
            LOGGER.info("The key is " + key + " and the string is " + string);
            switch (key) {
                case "OffCoil_label":
                    OffCoil_label = string.split(",")[1];
                    break;
                case "Pipe1_IRI":
                    Pipe1_IRI = string.split(",")[1];
                    break;
                case "Pipe2_IRI":
                    Pipe2_IRI = string.split(",")[1];
                    break;
                case "Duct1_IRI":
                    Duct1_IRI = string.split(",")[1];
                    break;
                case "Duct2_IRI":
                    Duct2_IRI = string.split(",")[1];
                    break;
                case "TempSetpoint_IRI":
                    TempSetpoint_IRI = string.split(",")[1];
                    break;
            }
        }

        //instantiate the off coil instance
        //include label
        String OffCoil_Instance = "https://www.theworldavatar.com/kg/ontobms/OffCoil_" + UUID.randomUUID();
        TriplePattern P1 = iri(OffCoil_Instance).isA(WaterCoil);
        TriplePattern P2 = iri(OffCoil_Instance).has(label, OffCoil_label);
        TriplePattern P3 = iri(Pipe1_IRI).has(isConnectedTo, iri(OffCoil_Instance));
        TriplePattern P3_1 = iri(OffCoil_Instance).has(isConnectedTo, iri(Pipe1_IRI));
        TriplePattern P4 = iri(Pipe2_IRI).has(isConnectedTo, iri(OffCoil_Instance));
        TriplePattern P4_1 = iri(OffCoil_Instance).has(isConnectedTo, iri(Pipe2_IRI));
        TriplePattern P5 = iri(Duct1_IRI).has(isConnectedTo, iri(OffCoil_Instance));
        TriplePattern P5_1 = iri(OffCoil_Instance).has(isConnectedTo, iri(Duct1_IRI));
        TriplePattern P6 = iri(Duct2_IRI).has(isConnectedTo, iri(OffCoil_Instance));
        TriplePattern P6_1 = iri(OffCoil_Instance).has(isConnectedTo, iri(Duct2_IRI));
        InsertDataQuery insert = Queries.INSERT_DATA(P1, P2, P3, P3_1, P4, P4_1, P5, P5_1, P6, P6_1);
        insert.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert.getQueryString());

        //instantiate temperature set point
        String TemperatureSetpoint_Instance = "https://www.theworldavatar.com/kg/ontobms/TemperatureSetpoint_" + UUID.randomUUID();
        TriplePattern P7 = iri(OffCoil_Instance).has(hasSetpoint, iri(TemperatureSetpoint_Instance));
        TriplePattern P8 = iri(TemperatureSetpoint_Instance).isA(Setpoint);
        String QOffCoilTempSetpoint_Instance = "https://www.theworldavatar.com/kg/ontobms/Q_OffCoilTempSetpoint_" + UUID.randomUUID();
        TriplePattern P9 = iri(TemperatureSetpoint_Instance).has(hasQuantity, iri(QOffCoilTempSetpoint_Instance));
        TriplePattern P10 = iri(QOffCoilTempSetpoint_Instance).isA(Temperature);
        TriplePattern P11 = iri(QOffCoilTempSetpoint_Instance).has(hasValue, iri(TempSetpoint_IRI));
        TriplePattern P12 = iri(TempSetpoint_IRI).isA(Measure);
        InsertDataQuery insert1 = Queries.INSERT_DATA(P7, P8, P9, P10, P11, P12);
        insert1.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert1.getQueryString());
    }

    public void instantiateFilter() throws FileNotFoundException {
        String Filter_label = null;
        String Duct1_IRI = null;
        String Duct2_IRI = null;
        String FilterState_IRI = null;
        String FilterAlarm_label = null;

        File file = new File(csvFilePath);
        sc = new Scanner(file);

        //Retrieve each of the necessary labels and IRIs
        while (sc.hasNext()) {
            String string = sc.nextLine();

            String key = string.split(",")[0];
            LOGGER.info("The key is " + key + " and the string is " + string);
            switch (key) {
                case "Filter_label":
                    Filter_label = string.split(",")[1];
                    break;
                case "Duct1_IRI":
                    Duct1_IRI = string.split(",")[1];
                    break;
                case "Duct2_IRI":
                    Duct2_IRI = string.split(",")[1];
                    break;
                case "FilterState_IRI":
                    FilterState_IRI = string.split(",")[1];
                    break;
                case "FilterAlarm_label":
                    FilterAlarm_label = string.split(",")[1];
                    break;
            }
        }

        //instantiate the filter instance
        //include label
        String Filter_Instance = "https://www.theworldavatar.com/kg/ontobms/Filter_" + UUID.randomUUID();
        TriplePattern P1 = iri(Filter_Instance).isA(Filter);
        TriplePattern P2 = iri(Filter_Instance).has(label, Filter_label);
        TriplePattern P3 = iri(Duct1_IRI).has(isConnectedTo, iri(Filter_Instance));
        TriplePattern P3_1 = iri(Filter_Instance).has(isConnectedTo, iri(Duct1_IRI));
        TriplePattern P4 = iri(Duct2_IRI).has(isConnectedTo, iri(Filter_Instance));
        TriplePattern P4_1 = iri(Filter_Instance).has(isConnectedTo, iri(Duct2_IRI));
        TriplePattern P5 = iri(Filter_Instance).has(hasState, iri(FilterState_IRI));
        InsertDataQuery insert = Queries.INSERT_DATA(P1, P2, P3, P3_1, P4, P4_1, P5);
        insert.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM, PREFIX_S4BLDG);
        kbClient.executeUpdate(insert.getQueryString());

        //instantiate filter state and status sensor
        TriplePattern P6 = iri(FilterState_IRI).isA(AlarmState);
        TriplePattern P7 = iri(FilterState_IRI).has(label, FilterAlarm_label);
        String StatusSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/StatusSensor_" + UUID.randomUUID();
        TriplePattern P8 = iri(StatusSensor_Instance).isA(StatusSensor);
        TriplePattern P9 = iri(StatusSensor_Instance).has(observes, iri(FilterState_IRI));
        TriplePattern P10 = iri(StatusSensor_Instance).has(isAttachedTo, iri(Filter_Instance));
        InsertDataQuery insert1 = Queries.INSERT_DATA(P6, P7, P8, P9, P10);
        insert1.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert1.getQueryString());
    }

    public void instantiateFan() throws FileNotFoundException {
        String Fan_label = null;
        String Duct1_IRI = null;
        String Duct2_IRI = null;
        String ControlFrequency_IRI = null;
        String ControlFrequency_IRI_label = null;
        String FeedbackFrequency_IRI = null;
        String FeedbackFrequency_IRI_label = null;
        String State_IRI = null;
        String State_IRI_label = null;
        String MultiLevelState_IRI = null;
        String MultiLevelState_label = null;
        String AlarmState_IRI = null;
        String AlarmState_label = null;

        File file = new File(csvFilePath);
        sc = new Scanner(file);

        //Retrieve each or the necessary labels and IRIs
        while (sc.hasNext()) {
            String string = sc.nextLine();

            String key = string.split(",")[0];
            LOGGER.info("The key is " + key + " and the string is " + string);
            switch (key) {
                case "Fan_label":
                    Fan_label = string.split(",")[1];
                    break;
                case "Duct1_IRI":
                    Duct1_IRI = string.split(",")[1];
                    break;
                case "Duct2_IRI":
                    Duct2_IRI = string.split(",")[1];
                    break;
                case "ControlFrequency_IRI":
                    ControlFrequency_IRI = string.split(",")[1];
                    break;
                case "ControlFrequency_IRI_label":
                    ControlFrequency_IRI_label = string.split(",")[1];
                    break;
                case "FeedbackFrequency_IRI":
                    FeedbackFrequency_IRI = string.split(",")[1];
                    break;
                case "FeedbackFrequency_IRI_label":
                    FeedbackFrequency_IRI_label = string.split(",")[1];
                    break;
                case "State_IRI":
                    State_IRI = string.split(",")[1];
                    break;
                case "State_IRI_label":
                    State_IRI_label = string.split(",")[1];
                    break;
                case "MultiLevelState_IRI":
                    MultiLevelState_IRI = string.split(",")[1];
                    break;
                case "MultiLevelState_label":
                    MultiLevelState_label = string.split(",")[1];
                    break;
                case "AlarmState_IRI":
                    AlarmState_IRI = string.split(",")[1];
                    break;
                case "AlarmState_label":
                    AlarmState_label = string.split(",")[1];
                    break;
            }
        }

        //instantiate the fan instance
        String Fan_Instance = "https://www.theworldavatar.com/kg/ontobms/MAU_" + Fan_label + "_" + UUID.randomUUID();
        TriplePattern P1 = iri(Fan_Instance).isA(Fan);
        TriplePattern P2 = iri(Duct1_IRI).has(isConnectedTo, iri(Fan_Instance));
        TriplePattern P2_1 = iri(Fan_Instance).has(isConnectedTo, iri(Duct1_IRI));
        TriplePattern P3 = iri(Duct2_IRI).has(isConnectedTo, iri(Fan_Instance));
        TriplePattern P3_1 = iri(Fan_Instance).has(isConnectedTo, iri(Duct2_IRI));
        String ControlFrequency_Instance = "https://www.theworldavatar.com/kg/ontobms/ControlFrequency_" + UUID.randomUUID();
        TriplePattern P4 = iri(Fan_Instance).has(hasControlParameter, iri(ControlFrequency_Instance));
        String ControlSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/VSDControlSensor_" + UUID.randomUUID();
        TriplePattern P5 = iri(Fan_Instance).has(consistsOf, iri(ControlSensor_Instance));
        String FeedbackFrequency_Instance = "https://www.theworldavatar.com/kg/ontobms/FeedbackFrequency_" + UUID.randomUUID();
        TriplePattern P6 = iri(Fan_Instance).has(hasFeedbackParameter, iri(FeedbackFrequency_Instance));
        String FeedbackSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/VSDFeedbackSensor_" + UUID.randomUUID();
        TriplePattern P7 = iri(Fan_Instance).has(consistsOf, iri(FeedbackSensor_Instance));
        TriplePattern P8 = iri(Fan_Instance).has(hasState, iri(State_IRI));
        InsertDataQuery insert = Queries.INSERT_DATA(P1, P2, P2_1, P3, P3_1, P4, P5, P6, P7, P8);
        insert.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM, PREFIX_S4BLDG);
        kbClient.executeUpdate(insert.getQueryString());

        //instantiate control frequency and control sensor
        TriplePattern P9 = iri(ControlFrequency_Instance).isA(Frequency);
        TriplePattern P10 = iri(ControlFrequency_Instance).has(hasValue, iri(ControlFrequency_IRI));
        TriplePattern P11 = iri(ControlFrequency_IRI).isA(Measure);
        TriplePattern P12 = iri(ControlSensor_Instance).isA(ControlSensor);
        TriplePattern P13 = iri(ControlSensor_Instance).has(measures, iri(ControlFrequency_Instance));
        TriplePattern P13_1 = iri(ControlFrequency_IRI).has(label, ControlFrequency_IRI_label);
        InsertDataQuery insert1 = Queries.INSERT_DATA(P9, P10, P11, P12, P13, P13_1);
        insert1.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert1.getQueryString());

        //instantiate feedback frequency and feedback sensor
        TriplePattern P14 = iri(FeedbackFrequency_Instance).isA(Frequency);
        TriplePattern P15 = iri(FeedbackFrequency_Instance).has(hasValue, iri(FeedbackFrequency_IRI));
        TriplePattern P16 = iri(FeedbackFrequency_IRI).isA(Measure);
        TriplePattern P17 = iri(FeedbackSensor_Instance).isA(FeedbackSensor);
        TriplePattern P18 = iri(FeedbackSensor_Instance).has(measures, iri(FeedbackFrequency_Instance));
        TriplePattern P18_1 = iri(FeedbackFrequency_IRI).has(label, FeedbackFrequency_IRI_label);
        InsertDataQuery insert2 = Queries.INSERT_DATA(P14, P15, P16, P17, P18, P18_1);
        insert2.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert2.getQueryString());

        //instantiate state and state status sensor if state iri provided
        if (State_IRI != null) {
            String StateStatusSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/StatusSensor_" + UUID.randomUUID();
            TriplePattern P19 = iri(Fan_Instance).has(consistsOf, iri(StateStatusSensor_Instance));
            TriplePattern P20 = iri(State_IRI).isA(State);
            TriplePattern P20_1 = iri(State_IRI).has(label, State_IRI_label);
            TriplePattern P21 = iri(StateStatusSensor_Instance).isA(StatusSensor);
            TriplePattern P22 = iri(StateStatusSensor_Instance).has(observes, iri(State_IRI));
            InsertDataQuery insert3 = Queries.INSERT_DATA(P19, P20, P20_1, P21, P22);
            insert3.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
            kbClient.executeUpdate(insert3.getQueryString());
        }

        //instantiate multi level state and multi level state status sensor if multilevel state iri and multilevel state label are provided
        if (MultiLevelState_IRI != null && MultiLevelState_label != null) {
            TriplePattern P23 = iri(Fan_Instance).has(hasState, iri(MultiLevelState_IRI));
            String MultiStateSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/StatusSensor_" + UUID.randomUUID();
            TriplePattern P24 = iri(Fan_Instance).has(consistsOf, iri(MultiStateSensor_Instance));
            TriplePattern P25 = iri(MultiLevelState_IRI).isA(MultiLevelState);
            TriplePattern P26 = iri(MultiLevelState_IRI).has(label, MultiLevelState_label);
            TriplePattern P27 = iri(MultiStateSensor_Instance).isA(StatusSensor);
            TriplePattern P28 = iri(MultiStateSensor_Instance).has(observes, iri(MultiLevelState_IRI));
            InsertDataQuery insert4 = Queries.INSERT_DATA(P23, P24, P25, P26, P27, P28);
            insert4.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
            kbClient.executeUpdate(insert4.getQueryString());
        }

        //instantiate alarm state and alarm state status sensor if alarm state iri and alarm state label are provided
        if (AlarmState_IRI != null && AlarmState_label != null) {
            TriplePattern P29 = iri(Fan_Instance).has(hasState, iri(AlarmState_IRI));
            String AlarmStateSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/StatusSensor_" + UUID.randomUUID();
            TriplePattern P30 = iri(Fan_Instance).has(consistsOf, iri(AlarmStateSensor_Instance));
            TriplePattern P31 = iri(AlarmState_IRI).isA(AlarmState);
            TriplePattern P32 = iri(AlarmState_IRI).has(label, AlarmState_label);
            TriplePattern P33 = iri(AlarmStateSensor_Instance).isA(StatusSensor);
            TriplePattern P34 = iri(AlarmStateSensor_Instance).has(observes, iri(AlarmState_IRI));
            InsertDataQuery insert5 = Queries.INSERT_DATA(P29, P30, P31, P32, P33, P34);
            insert5.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
            kbClient.executeUpdate(insert5.getQueryString());
        }
    }

    public void instantiateTJoint() throws FileNotFoundException {
        String Pipe1_IRI = null;
        String Pipe2_IRI = null;
        String Pipe3_IRI = null;

        File file = new File(csvFilePath);
        sc = new Scanner(file);

        //Retrieve each of the necessary IRIs
        while (sc.hasNext()) {
            String string = sc.nextLine();

            String key = string.split(",")[0];
            LOGGER.info("The key is " + key + " and the string is " + string);
            switch (key) {
                case "Pipe1_IRI":
                    Pipe1_IRI = string.split(",")[1];
                    break;
                case "Pipe2_IRI":
                    Pipe2_IRI = string.split(",")[1];
                    break;
                case "Pipe3_IRI":
                    Pipe3_IRI = string.split(",")[1];
                    break;
            }
        }

        //instantiate the T-Joint instance
        String TJoint_Instance = "https://www.theworldavatar.com/kg/ontobms/T-Joint_" + UUID.randomUUID();
        TriplePattern P1 = iri(TJoint_Instance).isA(Tee);
        TriplePattern P2 = iri(Pipe1_IRI).has(isConnectedTo, iri(TJoint_Instance));
        TriplePattern P2_1 = iri(TJoint_Instance).has(isConnectedTo, iri(Pipe1_IRI));
        TriplePattern P3 = iri(Pipe2_IRI).has(isConnectedTo, iri(TJoint_Instance));
        TriplePattern P3_1 = iri(TJoint_Instance).has(isConnectedTo, iri(Pipe2_IRI));
        TriplePattern P4 = iri(Pipe3_IRI).has(isConnectedTo, iri(TJoint_Instance));
        TriplePattern P4_1 = iri(TJoint_Instance).has(isConnectedTo, iri(Pipe3_IRI));
        InsertDataQuery insert = Queries.INSERT_DATA(P1, P2, P2_1, P3, P3_1, P4, P4_1);
        insert.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert.getQueryString());
    }

    public void instantiateCooling() throws FileNotFoundException {
        String CoolingEnergy_IRI = null;
        String CoolingEnergy_IRI_label = null;
        String CoolingDuty_IRI = null;
        String CoolingDuty_IRI_label = null;
        ArrayList<String> Inputs = new ArrayList<String>();
        ArrayList<String> Outputs = new ArrayList<String>();

        File file = new File(csvFilePath);
        sc = new Scanner(file);

        //Retrieve each of the necessary IRIs
        while (sc.hasNext()) {
            String string = sc.nextLine();

            String key = string.split(",")[0];
            LOGGER.info("The key is " + key + " and the string is " + string);
            switch (key) {
                case "CoolingEnergy_IRI":
                    CoolingEnergy_IRI = string.split(",")[1];
                    break;
                case "CoolingEnergy_IRI_label":
                    CoolingEnergy_IRI_label = string.split(",")[1];
                    break;
                case "CoolingDuty_IRI":
                    CoolingDuty_IRI = string.split(",")[1];
                    break;
                case "CoolingDuty_IRI_label":
                    CoolingDuty_IRI_label = string.split(",")[1];
                    break;
                case "Input":
                    Inputs.add(string.split(",")[1]);
                    break;
                case "Output":
                    Outputs.add(string.split(",")[1]);
                    break;
            }
        }

        //instantiate the Cooling instance
        String Cooling_Instance = "https://www.theworldavatar.com/kg/ontobms/Cooling_" + UUID.randomUUID();
        TriplePattern P1 = iri(Cooling_Instance).isA(TemperatureChange);
        String TotalCooling_Instance = "https://www.theworldavatar.com/kg/ontobms/TotalCooling_" + UUID.randomUUID();
        TriplePattern P2 = iri(TotalCooling_Instance).isA(TemperatureChange);
        TriplePattern P3 = iri(Cooling_Instance).has(isPartOf, iri(TotalCooling_Instance));
        TriplePattern P4 = iri(TotalCooling_Instance).has(hasPart, iri(Cooling_Instance));
        InsertDataQuery insert = Queries.INSERT_DATA(P1, P2, P3, P4);
        insert.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM, PREFIX_ONTOCAPE_CPS_FUNCTION_PROCESS, PREFIX_META_MODEL_MEREOLOGY);
        kbClient.executeUpdate(insert.getQueryString());

        //instantiate cooling energy
        String CoolingEnergy_Instance = "https://www.theworldavatar.com/kg/ontobms/CoolingEnergy_" + UUID.randomUUID();
        TriplePattern P5 = iri(CoolingEnergy_Instance).isA(Heat);
        TriplePattern P6 = iri(TotalCooling_Instance).has(hasCumulativeEnergyConsumption, iri(CoolingEnergy_Instance));
        TriplePattern P7 = iri(CoolingEnergy_Instance).has(hasValue, iri(CoolingEnergy_IRI));
        TriplePattern P8 = iri(CoolingEnergy_IRI).isA(Measure);
        TriplePattern P8_1 = iri(CoolingEnergy_IRI).has(hasUnit, kilowattHour);
        TriplePattern P8_2 = kilowattHour.isA(UnitMultiplication);
        TriplePattern P8_3 = kilowattHour.has(symbol, "kWh");
        TriplePattern P8_4 = kilowattHour.has(label, "kilowatt hour");
        TriplePattern P8_5 = iri(CoolingEnergy_IRI).has(label, CoolingEnergy_IRI_label);
        InsertDataQuery insert1 = Queries.INSERT_DATA(P5, P6, P7, P8, P8_1, P8_2, P8_3, P8_4, P8_5);
        insert1.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert1.getQueryString());

        //instantiate cooling duty
        String CoolingDuty_Instance = "https://www.theworldavatar.com/kg/ontobms/CoolingDuty_" + UUID.randomUUID();
        TriplePattern P9 = iri(CoolingDuty_Instance).isA(HeatDuty);
        TriplePattern P10 = iri(TotalCooling_Instance).has(hasHeatDuty, iri(CoolingDuty_Instance));
        TriplePattern P11 = iri(CoolingDuty_Instance).has(hasValue, iri(CoolingDuty_IRI));
        TriplePattern P12 = iri(CoolingDuty_IRI).isA(Measure);
        TriplePattern P12_1 = iri(CoolingDuty_IRI).has(hasUnit, kilowatt);
        TriplePattern P12_2 = kilowatt.isA(PrefixedUnit);
        TriplePattern P12_3 = kilowatt.has(symbol, "kW");
        TriplePattern P12_4 = kilowatt.has(label, "kilowatt");
        TriplePattern P12_5 = iri(CoolingDuty_IRI).has(label, CoolingDuty_IRI_label);
        InsertDataQuery insert2 = Queries.INSERT_DATA(P9, P10, P11, P12, P12_1, P12_2, P12_3, P12_4, P12_5);
        insert2.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert2.getQueryString());

        //instantiate inputs
        TriplePattern P13;
        for (int i = 0; i < Inputs.size(); i++) {
            String input = Inputs.get(i);
            P13 = iri(Cooling_Instance).has(hasInput_ontocape, iri(input));
            InsertDataQuery insert3 = Queries.INSERT_DATA(P13);
            insert3.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM, PREFIX_ONTOCAPE_NETWORK_SYSTEM);
            kbClient.executeUpdate(insert3.getQueryString());
        }

        //instantiate outputs
        TriplePattern P14;
        for (int i = 0; i < Outputs.size(); i++) {
            String output = Outputs.get(i);
            P14 = iri(Cooling_Instance).has(hasOutput, iri(output));
            InsertDataQuery insert4 = Queries.INSERT_DATA(P14);
            insert4.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM, PREFIX_ONTOCAPE_NETWORK_SYSTEM);
            kbClient.executeUpdate(insert4.getQueryString());
        }
    }

    public void instantiateMAU() throws FileNotFoundException {
        String MAU_label = null;
        String MAUState_IRI = null;
        String MAUState_IRI_label = null;
        ArrayList<String> IRIs = new ArrayList<String>();

        File file = new File(csvFilePath);
        sc = new Scanner(file);

        //Retrieve each of the necessary labels and IRIs
        while (sc.hasNext()) {
            String string = sc.nextLine();

            String key = string.split(",")[0];
            LOGGER.info("The key is " + key + " and the string is " + string);
            switch (key) {
                case "MAU_label":
                    MAU_label = string.split(",")[1];
                    break;
                case "MAUState_IRI":
                    MAUState_IRI = string.split(",")[1];
                    break;
                case "MAUState_IRI_label":
                    MAUState_IRI_label = string.split(",")[1];
                    break;
                case "IRI":
                    IRIs.add(string.split(",")[1]);
                    break;
            }
        }

        //instantiate the MAU instance
        //include label
        String MAU_Instance = "https://www.theworldavatar.com/kg/ontobms/" + MAU_label + "_" + UUID.randomUUID();
        TriplePattern P1 = iri(MAU_Instance).isA(MakeupAirUnit);
        TriplePattern P2 = iri(MAU_Instance).has(label, MAU_label);
        String StatusSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/StatusSensor_" + UUID.randomUUID();
        TriplePattern P3 = iri(StatusSensor_Instance).isA(StatusSensor);
        TriplePattern P4 = iri(MAU_Instance).has(consistsOf, iri(StatusSensor_Instance));
        TriplePattern P5 = iri(MAU_Instance).has(hasState, iri(MAUState_IRI));
        TriplePattern P6 = iri(MAUState_IRI).isA(StartStopState);
        TriplePattern P6_1 = iri(MAUState_IRI).has(label, MAUState_IRI_label);
        TriplePattern P7 = iri(StatusSensor_Instance).has(observes, iri(MAUState_IRI));
        InsertDataQuery insert = Queries.INSERT_DATA(P1, P2, P3, P4, P5, P6, P6_1, P7);
        insert.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert.getQueryString());

        //instantiate all consistsOf properties
        TriplePattern P8;
        for (int i = 0; i < IRIs.size(); i++) {
            String Iri = IRIs.get(i);
            P8 = iri(MAU_Instance).has(consistsOf, iri(Iri));
            InsertDataQuery insert1 = Queries.INSERT_DATA(P8);
            insert1.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
            kbClient.executeUpdate(insert1.getQueryString());
        }
    }

    public void instantiateVAV_S() throws FileNotFoundException {
        String VAVS_label = null;
        String AirFlow_IRI = null;
        String AirFlow_IRI_label = null;
        String DamperState_IRI = null;
        String DamperState_IRI_label = null;
        String SetpointQuantity_IRI = null;
        String SetpointQuantity_IRI_label = null;
        String Room_IRI = null;
        String Temperature_IRI = null;
        String Temperature_IRI_label = null;
        String RelativeHumidity_IRI = null;
        String RelativeHumidity_IRI_label = null;
        String HotWaterCmd_IRI = null;
        String HotWaterCmd_IRI_label = null;
        String HotWaterStatus_IRI = null;
        String HotWaterStatus_IRI_label = null;
        String HeaterControl_IRI = null;
        String HeaterControl_IRI_label = null;
        String HeaterPower_IRI = null;
        String HeaterPower_IRI_label = null;
        String HeaterStatus_IRI = null;
        String HeaterStatus_IRI_label = null;
        ArrayList<String> IRIs  = new ArrayList<String>();

        File file = new File(csvFilePath);
        sc = new Scanner(file);

        //Retrieve each of the necessary labels and IRIs
        while (sc.hasNext()) {
            String string = sc.nextLine();

            String key = string.split(",")[0];
            LOGGER.info("The key is " + key + " and the string is " + string);
            switch (key) {
                case "VAVS_label":
                    VAVS_label = string.split(",")[1];
                    break;
                case "AirFlow_IRI":
                    AirFlow_IRI = string.split(",")[1];
                    break;
                case "AirFlow_IRI_label":
                    AirFlow_IRI_label = string.split(",")[1];
                    break;
                case "DamperState_IRI":
                    DamperState_IRI = string.split(",")[1];
                    break;
                case "DamperState_IRI_label":
                    DamperState_IRI_label = string.split(",")[1];
                    break;
                case "SetpointQuantity_IRI":
                    SetpointQuantity_IRI = string.split(",")[1];
                    break;
                case "SetpointQuantity_IRI_label":
                    SetpointQuantity_IRI_label = string.split(",")[1];
                    break;
                case "Room_IRI":
                    Room_IRI = string.split(",")[1];
                    break;
                case "Temperature_IRI":
                    Temperature_IRI = string.split(",")[1];
                    break;
                case "Temperature_IRI_label":
                    Temperature_IRI_label = string.split(",")[1];
                    break;
                case "RelativeHumidity_IRI":
                    RelativeHumidity_IRI = string.split(",")[1];
                    break;
                case "RelativeHumidity_IRI_label":
                    RelativeHumidity_IRI_label = string.split(",")[1];
                    break;
                case "HotWaterCmd_IRI":
                    HotWaterCmd_IRI = string.split(",")[1];
                    break;
                case "HotWaterCmd_IRI_label":
                    HotWaterCmd_IRI_label = string.split(",")[1];
                    break;
                case "HotWaterStatus_IRI":
                    HotWaterStatus_IRI = string.split(",")[1];
                    break;
                case "HotWaterStatus_IRI_label":
                    HotWaterStatus_IRI_label = string.split(",")[1];
                    break;
                case "HeaterControl_IRI":
                    HeaterControl_IRI = string.split(",")[1];
                    break;
                case "HeaterControl_IRI_label":
                    HeaterControl_IRI_label = string.split(",")[1];
                    break;
                case "HeaterPower_IRI":
                    HeaterPower_IRI = string.split(",")[1];
                    break;
                case "HeaterPower_IRI_label":
                    HeaterPower_IRI_label = string.split(",")[1];
                    break;
                case "HeaterStatus_IRI":
                    HeaterStatus_IRI = string.split(",")[1];
                    break;
                case "HeaterStatus_IRI_label":
                    HeaterStatus_IRI_label = string.split(",")[1];
                    break;
                case "IRI":
                    IRIs.add(string.split(",")[1]);
                    break;
            }
        }

        //instantiate the VAV-S instance
        //include label
        String VAVS_Instance = "https://www.theworldavatar.com/kg/ontobms/" + VAVS_label + "_" + UUID.randomUUID();
        TriplePattern P1 = iri(VAVS_Instance).isA(SupplyVAV);
        TriplePattern P2 = iri(VAVS_Instance).has(label, VAVS_label);
        String AirFlow_Instance = "https://www.theworldavatar.com/kg/ontobms/AirFlow_" + UUID.randomUUID();
        TriplePattern P3 = iri(VAVS_Instance).has(hasAirFlowRate, iri(AirFlow_Instance));
        String AirFlowSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/AirFlowSensor_" + UUID.randomUUID();
        TriplePattern P4 = iri(VAVS_Instance).has(consistsOf, iri(AirFlowSensor_Instance));
        TriplePattern P5 = iri(VAVS_Instance).has(hasState, iri(DamperState_IRI));
        String DamperStateSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/DamperStateSensor_" + UUID.randomUUID();
        TriplePattern P6 = iri(VAVS_Instance).has(consistsOf, iri(DamperStateSensor_Instance));
        String Setpoint_Instance = "https://www.theworldavatar.com/kg/ontobms/Setpoint_" + UUID.randomUUID();
        TriplePattern P7 = iri(VAVS_Instance).has(hasSetpoint, iri(Setpoint_Instance));
        InsertDataQuery insert = Queries.INSERT_DATA(P1, P2, P3, P4, P5, P6, P7);
        insert.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert.getQueryString());

        //instantiate air flow and air flow sensor
        TriplePattern P8 = iri(AirFlow_Instance).isA(VolumetricFlowRate);
        TriplePattern P9 = iri(AirFlow_Instance).has(hasValue, iri(AirFlow_IRI));
        TriplePattern P10 = iri(AirFlow_IRI).isA(Measure);
        TriplePattern P10_1 = iri(AirFlow_IRI).has(hasUnit, cubicMetrePerHour);
        TriplePattern P10_2 = cubicMetrePerHour.isA(UnitDivision);
        TriplePattern P10_3 = cubicMetrePerHour.has(symbol, "m3/h");
        TriplePattern P10_4 = cubicMetrePerHour.has(label, "cubic metre per hour");
        TriplePattern P10_5 = iri(AirFlow_IRI).has(label, AirFlow_IRI_label);
        TriplePattern P11 = iri(AirFlowSensor_Instance).isA(FlowSensor);
        TriplePattern P12 = iri(AirFlowSensor_Instance).has(measures, iri(AirFlow_Instance));
        TriplePattern P13 = iri(AirFlowSensor_Instance).has(sendsSignalTo, iri(VAVS_Instance));
        InsertDataQuery insert1 = Queries.INSERT_DATA(P8, P9, P10, P10_1, P10_2, P10_3, P10_4, P10_5, P11, P12, P13);
        insert1.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert1.getQueryString());

        //instantiate damper state and damper state sensor
        TriplePattern P14 = iri(DamperState_IRI).isA(DamperState);
        TriplePattern P15 = iri(DamperStateSensor_Instance).isA(StatusSensor);
        TriplePattern P16 = iri(DamperStateSensor_Instance).has(observes, iri(DamperState_IRI));
        TriplePattern P17 = iri(DamperStateSensor_Instance).has(sendsSignalTo, iri(VAVS_Instance));
        TriplePattern P17_1 = iri(DamperState_IRI).has(label, DamperState_IRI_label);
        InsertDataQuery insert2 = Queries.INSERT_DATA(P14, P15, P16, P17, P17_1);
        insert2.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert2.getQueryString());

        //instantiate setpoint
        TriplePattern P18 = iri(Setpoint_Instance).isA(Setpoint);
        String SetpointQuantity_Instance = "https://www.theworldavatar.com/kg/ontobms/SetpointQuantity_" + UUID.randomUUID();
        TriplePattern P19 = iri(Setpoint_Instance).has(hasQuantity, iri(SetpointQuantity_Instance));
        TriplePattern P20 = iri(SetpointQuantity_Instance).isA(VolumetricFlowRate);
        TriplePattern P21 = iri(SetpointQuantity_Instance).has(hasValue, iri(SetpointQuantity_IRI));
        TriplePattern P22 = iri(SetpointQuantity_IRI).isA(Measure);
        TriplePattern P22_1 = iri(SetpointQuantity_IRI).has(hasUnit, cubicMetrePerHour);
        TriplePattern P22_2 = cubicMetrePerHour.isA(UnitDivision);
        TriplePattern P22_3 = cubicMetrePerHour.has(symbol, "m3/h");
        TriplePattern P22_4 = cubicMetrePerHour.has(label, "cubic metre per hour");
        TriplePattern P22_5 = iri(SetpointQuantity_IRI).has(label, SetpointQuantity_IRI_label);
        InsertDataQuery insert3 = Queries.INSERT_DATA(P18, P19, P20, P21, P22, P22_1, P22_2, P22_3, P22_4, P22_5);
        insert3.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert3.getQueryString());

        //instantiate temperature and temperature sensor if room iri and temperature iri are provided
        if (Room_IRI != null && Temperature_IRI != null && Temperature_IRI_label != null) {
            String TemperatureSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/TemperatureSensor_" + UUID.randomUUID();
            TriplePattern P23 = iri(VAVS_Instance).has(consistsOf, iri(TemperatureSensor_Instance));
            TriplePattern P24 = iri(TemperatureSensor_Instance).isA(TemperatureSensor);
            String Temperature_Instance = "https://www.theworldavatar.com/kg/ontobms/Temperature_" + UUID.randomUUID();
            TriplePattern P25 = iri(TemperatureSensor_Instance).has(measures, iri(Temperature_Instance));
            TriplePattern P26 = iri(Temperature_Instance).isA(Temperature);
            TriplePattern P27 = iri(Temperature_Instance).has(hasValue, iri(Temperature_IRI));
            TriplePattern P28 = iri(Temperature_IRI).isA(Measure);
            TriplePattern P28_1 = iri(Temperature_IRI).has(hasUnit, degreeCelsius);
            TriplePattern P28_2 = degreeCelsius.isA(Unit);
            TriplePattern P28_3 = degreeCelsius.isA(SingularUnit);
            TriplePattern P28_4 = degreeCelsius.has(label, "degree celsius");
            TriplePattern P28_5 = iri(Temperature_IRI).has(label, Temperature_IRI_label);
            TriplePattern P29 = iri(Room_IRI).has(hasTemperature, iri(Temperature_Instance));
            InsertDataQuery insert4 = Queries.INSERT_DATA(P23, P24, P25, P26, P27, P28, P28_1, P28_2, P28_3, P28_4, P28_5, P29);
            insert4.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
            kbClient.executeUpdate(insert4.getQueryString());
        }

        //instantiate relative humidity and humidity sensor if room iri and relative humidity iri are provided
        if (Room_IRI != null && RelativeHumidity_IRI != null && RelativeHumidity_IRI_label != null) {
            String HumiditySensor_Instance = "https://www.theworldavatar.com/kg/ontobms/HumiditySensor_" + UUID.randomUUID();
            TriplePattern P30 = iri(VAVS_Instance).has(consistsOf, iri(HumiditySensor_Instance));
            TriplePattern P31 = iri(HumiditySensor_Instance).isA(HumiditySensor);
            String RelativeHumidity_Instance = "https://www.theworldavatar.com/kg/ontobms/RelativeHumidity_" + UUID.randomUUID();
            TriplePattern P32 = iri(HumiditySensor_Instance).has(measures, iri(RelativeHumidity_Instance));
            TriplePattern P33 = iri(RelativeHumidity_Instance).isA(RelativeHumidity);
            TriplePattern P34 = iri(RelativeHumidity_Instance).has(hasValue, iri(RelativeHumidity_IRI));
            TriplePattern P35 = iri(RelativeHumidity_IRI).isA(Measure);
            TriplePattern P35_1 = iri(RelativeHumidity_IRI).has(hasUnit, percent);
            TriplePattern P35_2 = percent.isA(Unit);
            TriplePattern P35_3 = percent.isA(SingularUnit);
            TriplePattern P35_4 = percent.has(symbol, "%");
            TriplePattern P35_5 = percent.has(label, "percent");
            TriplePattern P35_6 = iri(RelativeHumidity_IRI).has(label, RelativeHumidity_IRI_label);
            TriplePattern P36 = iri(Room_IRI).has(hasRelativeHumidity, iri(RelativeHumidity_Instance));
            InsertDataQuery insert5 = Queries.INSERT_DATA(P30, P31, P32, P33, P34, P35, P35_1, P35_2, P35_3, P35_4, P35_5, P35_6, P36);
            insert5.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
            kbClient.executeUpdate(insert5.getQueryString());
        }

        //instantiate HVAV water coil, valve, control sensor, status sensor, hot water cmd and hot water status if
        //hot water cmd iri and hot water status iri are provided
        if (HotWaterCmd_IRI != null && HotWaterStatus_IRI != null && HotWaterCmd_IRI_label != null && HotWaterStatus_IRI_label != null) {
            String HVAVWaterCoil_Instance = "https://www.theworldavatar.com/kg/ontobms/HVAV_" + UUID.randomUUID();
            TriplePattern P37 = iri(VAVS_Instance).has(consistsOf, iri(HVAVWaterCoil_Instance));
            TriplePattern P38 = iri(HVAVWaterCoil_Instance).isA(WaterCoil);
            String Valve_Instance = "https://www.theworldavatar.com/kg/ontobms/Valve_" + UUID.randomUUID();
            TriplePattern P39 = iri(HVAVWaterCoil_Instance).has(consistsOf, iri(Valve_Instance));
            TriplePattern P40 = iri(Valve_Instance).isA(Valve);
            InsertDataQuery insert6 = Queries.INSERT_DATA(P37, P38, P39, P40);
            insert6.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM, PREFIX_S4BLDG);
            kbClient.executeUpdate(insert6.getQueryString());

            TriplePattern P41 = iri(Valve_Instance).has(hasState, iri(HotWaterStatus_IRI));
            TriplePattern P42 = iri(HotWaterStatus_IRI).isA(State);
            TriplePattern P42_1 = iri(HotWaterStatus_IRI).has(label, HotWaterStatus_IRI_label);
            String HotWaterStatusSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/StatusSensor_" + UUID.randomUUID();
            TriplePattern P43 = iri(Valve_Instance).has(consistsOf, iri(HotWaterStatusSensor_Instance));
            TriplePattern P44 = iri(HotWaterStatusSensor_Instance).isA(StatusSensor);
            TriplePattern P45 = iri(HotWaterStatusSensor_Instance).has(observes, iri(HotWaterStatus_IRI));
            InsertDataQuery insert7 = Queries.INSERT_DATA(P41, P42, P42_1, P43, P44, P45);
            insert7.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
            kbClient.executeUpdate(insert7.getQueryString());

            String ValveControlPercentage_Instance = "https://www.theworldavatar.com/kg/ontobms/ControlPercentage_" + UUID.randomUUID();
            TriplePattern P46 = iri(Valve_Instance).has(hasControlParameter, iri(ValveControlPercentage_Instance));
            TriplePattern P47 = iri(ValveControlPercentage_Instance).isA(Percentage);
            TriplePattern P48 = iri(ValveControlPercentage_Instance).has(hasValue, iri(HotWaterCmd_IRI));
            TriplePattern P49 = iri(HotWaterCmd_IRI).isA(Measure);
            TriplePattern P49_1 = iri(HotWaterCmd_IRI).has(hasUnit, percent);
            TriplePattern P49_2 = percent.isA(Unit);
            TriplePattern P49_3 = percent.isA(SingularUnit);
            TriplePattern P49_4 = percent.has(symbol, "%");
            TriplePattern P49_5 = percent.has(label, "percent");
            TriplePattern P49_6 = iri(HotWaterCmd_IRI).has(label, HotWaterCmd_IRI_label);
            String ValveControlSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/ControlSensor_" + UUID.randomUUID();
            TriplePattern P50 = iri(ValveControlSensor_Instance).has(isAttachedTo, iri(Valve_Instance));
            TriplePattern P51 = iri(ValveControlSensor_Instance).isA(ControlSensor);
            TriplePattern P52 = iri(ValveControlSensor_Instance).has(measures, iri(ValveControlPercentage_Instance));
            InsertDataQuery insert8 = Queries.INSERT_DATA(P46, P47, P48, P49, P49_1, P49_2, P49_3, P49_4, P49_5, P49_6, P50, P51, P52);
            insert8.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
            kbClient.executeUpdate(insert8.getQueryString());
        }

        //instantiate HVAV electrical coil, control sensor, status sensors, heater control, heater power and heater status if
        //heater control iri, heater power iri and heater status iri are provided
        if (HeaterControl_IRI != null && HeaterPower_IRI != null && HeaterStatus_IRI != null && HeaterControl_IRI_label != null && HeaterPower_IRI_label != null && HeaterStatus_IRI_label != null) {
            String HVAVElectricalCoil_Instance = "https://www.theworldavatar.com/kg/ontobms/HVAV_" + UUID.randomUUID();
            TriplePattern P53 = iri(VAVS_Instance).has(consistsOf, iri(HVAVElectricalCoil_Instance));
            TriplePattern P54 = iri(HVAVElectricalCoil_Instance).isA(ElectricalCoil);
            TriplePattern P55 = iri(HVAVElectricalCoil_Instance).has(hasState, iri(HeaterPower_IRI));
            TriplePattern P56 = iri(HeaterPower_IRI).isA(State);
            TriplePattern P56_1 = iri(HeaterPower_IRI).has(label, HeaterPower_IRI_label);
            String HeaterPowerStatusSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/StatusSensor_" + UUID.randomUUID();
            TriplePattern P57 = iri(HVAVElectricalCoil_Instance).has(consistsOf, iri(HeaterPowerStatusSensor_Instance));
            TriplePattern P58 = iri(HeaterPowerStatusSensor_Instance).isA(StatusSensor);
            TriplePattern P59 = iri(HeaterPowerStatusSensor_Instance).has(observes, iri(HeaterPower_IRI));
            InsertDataQuery insert9 = Queries.INSERT_DATA(P53, P54, P55, P56, P56_1, P57, P58, P59);
            insert9.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
            kbClient.executeUpdate(insert9.getQueryString());

            TriplePattern P60 = iri(HVAVElectricalCoil_Instance).has(hasState, iri(HeaterStatus_IRI));
            TriplePattern P61 = iri(HeaterStatus_IRI).isA(State);
            String HeaterStateStatusSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/StatusSensor_" + UUID.randomUUID();
            TriplePattern P62 = iri(HVAVElectricalCoil_Instance).has(consistsOf, iri(HeaterStateStatusSensor_Instance));
            TriplePattern P63 = iri(HeaterStateStatusSensor_Instance).isA(StatusSensor);
            TriplePattern P64 = iri(HeaterStateStatusSensor_Instance).has(observes, iri(HeaterStatus_IRI));
            TriplePattern P64_1 = iri(HeaterStatus_IRI).has(label, HeaterStatus_IRI_label);
            InsertDataQuery insert10 = Queries.INSERT_DATA(P60, P61, P62, P63, P64, P64_1);
            insert10.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
            kbClient.executeUpdate(insert10.getQueryString());

            String ElectricalCoilControlPercentage_Instance = "https://www.theworldavatar.com/kg/ontobms/ControlPercentage_" + UUID.randomUUID();
            TriplePattern P65 = iri(HVAVElectricalCoil_Instance).has(hasControlParameter, iri(ElectricalCoilControlPercentage_Instance));
            TriplePattern P66 = iri(ElectricalCoilControlPercentage_Instance).isA(Percentage);
            TriplePattern P67 = iri(ElectricalCoilControlPercentage_Instance).has(hasValue, iri(HeaterControl_IRI));
            TriplePattern P68 = iri(HeaterControl_IRI).isA(Measure);
            TriplePattern P68_1 = iri(HeaterControl_IRI).has(hasUnit, percent);
            TriplePattern P68_2 = percent.isA(Unit);
            TriplePattern P68_3 = percent.isA(SingularUnit);
            TriplePattern P68_4 = percent.has(symbol, "%");
            TriplePattern P68_5 = percent.has(label, "percent");
            TriplePattern P68_6 = iri(HeaterControl_IRI).has(label, HeaterControl_IRI_label);
            String ElectricalCoilControlSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/ControlSensor_" + UUID.randomUUID();
            TriplePattern P69 = iri(ElectricalCoilControlSensor_Instance).has(isAttachedTo, iri(HVAVElectricalCoil_Instance));
            TriplePattern P70 = iri(ElectricalCoilControlSensor_Instance).isA(ControlSensor);
            TriplePattern P71 = iri(ElectricalCoilControlSensor_Instance).has(measures, iri(ElectricalCoilControlPercentage_Instance));
            InsertDataQuery insert11 = Queries.INSERT_DATA(P65, P66, P67, P68, P68_1, P68_2, P68_3, P68_4, P68_5, P68_6, P69, P70, P71);
            insert11.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
            kbClient.executeUpdate(insert11.getQueryString());
        }

        //instantiate all sendsSignalTo properties
        TriplePattern P72;
        for (int i = 0; i < IRIs.size(); i++) {
            String Iri = IRIs.get(i);
            P72 = iri(VAVS_Instance).has(sendsSignalTo, iri(Iri));
            InsertDataQuery insert12 = Queries.INSERT_DATA(P72);
            insert12.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
            kbClient.executeUpdate(insert12.getQueryString());
        }
    }

    public void instantiateVAV_E() throws FileNotFoundException {
        String VAVE_label = null;
        String AirFlow_IRI = null;
        String AirFlow_IRI_label = null;
        String DamperState_IRI = null;
        String DamperState_IRI_label = null;
        String SetpointQuantity_IRI = null;
        String SetpointQuantity_IRI_label = null;
        String Percentage_IRI = null;
        String Percentage_IRI_label = null;

        File file = new File(csvFilePath);
        sc = new Scanner(file);

        //Retrieve each of the necessary labels and IRIs
        while (sc.hasNext()) {
            String string = sc.nextLine();

            String key = string.split(",")[0];
            LOGGER.info("The key is " + key + " and the string is " + string);
            switch (key) {
                case "VAVE_label":
                    VAVE_label = string.split(",")[1];
                    break;
                case "AirFlow_IRI":
                    AirFlow_IRI = string.split(",")[1];
                    break;
                case "AirFlow_IRI_label":
                    AirFlow_IRI_label = string.split(",")[1];
                    break;
                case "DamperState_IRI":
                    DamperState_IRI = string.split(",")[1];
                    break;
                case "DamperState_IRI_label":
                    DamperState_IRI_label = string.split(",")[1];
                    break;
                case "SetpointQuantity_IRI":
                    SetpointQuantity_IRI = string.split(",")[1];
                    break;
                case "SetpointQuantity_IRI_label":
                    SetpointQuantity_IRI_label = string.split(",")[1];
                    break;
                case "Percentage_IRI":
                    Percentage_IRI = string.split(",")[1];
                    break;
                case "Percentage_IRI_label":
                    Percentage_IRI_label = string.split(",")[1];
                    break;
            }
        }

        //instantiate the VAV-E instance
        String VAVE_Instance = "https://www.theworldavatar.com/kg/ontobms/" + VAVE_label + "_" + UUID.randomUUID();
        TriplePattern P1 = iri(VAVE_Instance).isA(ExhaustVAV);
        TriplePattern P2 = iri(VAVE_Instance).has(label, VAVE_label);
        String AirFlow_Instance = "https://www.theworldavatar.com/kg/ontobms/AirFlow_" + UUID.randomUUID();
        TriplePattern P3 = iri(VAVE_Instance).has(hasAirFlowRate, iri(AirFlow_Instance));
        String AirFlowSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/AirFlowSensor_" + UUID.randomUUID();
        TriplePattern P4 = iri(VAVE_Instance).has(consistsOf, iri(AirFlowSensor_Instance));
        TriplePattern P5 = iri(VAVE_Instance).has(hasState, iri(DamperState_IRI));
        String DamperStateSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/DamperStateSensor_" + UUID.randomUUID();
        TriplePattern P6 = iri(VAVE_Instance).has(consistsOf, iri(DamperStateSensor_Instance));
        String Setpoint_Instance = "https://www.theworldavatar.com/kg/ontobms/Setpoint_" + UUID.randomUUID();
        TriplePattern P7 = iri(VAVE_Instance).has(hasSetpoint, iri(Setpoint_Instance));
        InsertDataQuery insert = Queries.INSERT_DATA(P1, P2, P3, P4, P5, P6, P7);
        insert.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert.getQueryString());

        //instantiate air flow and air flow sensor
        TriplePattern P8 = iri(AirFlow_Instance).isA(VolumetricFlowRate);
        TriplePattern P9 = iri(AirFlow_Instance).has(hasValue, iri(AirFlow_IRI));
        TriplePattern P10 = iri(AirFlow_IRI).isA(Measure);
        TriplePattern P10_1 = iri(AirFlow_IRI).has(hasUnit, cubicMetrePerHour);
        TriplePattern P10_2 = cubicMetrePerHour.isA(UnitDivision);
        TriplePattern P10_3 = cubicMetrePerHour.has(symbol, "m3/h");
        TriplePattern P10_4 = cubicMetrePerHour.has(label, "cubic metre per hour");
        TriplePattern P10_5 = iri(AirFlow_IRI).has(label, AirFlow_IRI_label);
        TriplePattern P11 = iri(AirFlowSensor_Instance).isA(FlowSensor);
        TriplePattern P12 = iri(AirFlowSensor_Instance).has(measures, iri(AirFlow_Instance));
        TriplePattern P13 = iri(AirFlowSensor_Instance).has(sendsSignalTo, iri(VAVE_Instance));
        InsertDataQuery insert1 = Queries.INSERT_DATA(P8, P9, P10, P10_1, P10_2, P10_3, P10_4, P10_5, P11, P12, P13);
        insert1.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert1.getQueryString());

        //instantiate damper state and damper state sensor
        TriplePattern P14 = iri(DamperState_IRI).isA(DamperState);
        TriplePattern P15 = iri(DamperStateSensor_Instance).isA(StatusSensor);
        TriplePattern P16 = iri(DamperStateSensor_Instance).has(observes, iri(DamperState_IRI));
        TriplePattern P17 = iri(DamperStateSensor_Instance).has(sendsSignalTo, iri(VAVE_Instance));
        TriplePattern P17_1 = iri(DamperState_IRI).has(label, DamperState_IRI_label);
        InsertDataQuery insert2 = Queries.INSERT_DATA(P14, P15, P16, P17, P17_1);
        insert2.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert2.getQueryString());

        //instantiate setpoint
        TriplePattern P18 = iri(Setpoint_Instance).isA(Setpoint);
        String SetpointQuantity_Instance = "https://www.theworldavatar.com/kg/ontobms/SetpointQuantity_" + UUID.randomUUID();
        TriplePattern P19 = iri(Setpoint_Instance).has(hasQuantity, iri(SetpointQuantity_Instance));
        TriplePattern P20 = iri(SetpointQuantity_Instance).isA(VolumetricFlowRate);
        TriplePattern P21 = iri(SetpointQuantity_Instance).has(hasValue, iri(SetpointQuantity_IRI));
        TriplePattern P22 = iri(SetpointQuantity_IRI).isA(Measure);
        TriplePattern P22_1 = iri(SetpointQuantity_IRI).has(hasUnit, cubicMetrePerHour);
        TriplePattern P22_2 = cubicMetrePerHour.isA(UnitDivision);
        TriplePattern P22_3 = cubicMetrePerHour.has(symbol, "m3/h");
        TriplePattern P22_4 = cubicMetrePerHour.has(label, "cubic metre per hour");
        TriplePattern P22_5 = iri(SetpointQuantity_IRI).has(label, SetpointQuantity_IRI_label);
        InsertDataQuery insert3 = Queries.INSERT_DATA(P18, P19, P20, P21, P22, P22_1, P22_2, P22_3, P22_4, P22_5);
        insert3.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert3.getQueryString());

        //instantiate percentage and extraction arm if percentage iri provided
        if (Percentage_IRI != null) {
            String ExtractionArm_Instance = "https://www.theworldavatar.com/kg/ontobms/ExtractionArm_" + UUID.randomUUID();
            TriplePattern P23 = iri(VAVE_Instance).has(consistsOf, iri(ExtractionArm_Instance));
            TriplePattern P24 = iri(ExtractionArm_Instance).isA(ExtractionArm);
            String Percentage_Instance = "https://www.theworldavatar.com/kg/ontobms/Percentage_" + UUID.randomUUID();
            TriplePattern P25 = iri(ExtractionArm_Instance).has(hasOpening, iri(Percentage_Instance));
            TriplePattern P26 = iri(Percentage_Instance).isA(Percentage);
            TriplePattern P27 = iri(Percentage_Instance).has(hasValue, iri(Percentage_IRI));
            TriplePattern P28 = iri(Percentage_IRI).isA(Measure);
            TriplePattern P28_1 = iri(Percentage_IRI).has(hasUnit, percent);
            TriplePattern P28_2 = percent.isA(Unit);
            TriplePattern P28_3 = percent.isA(SingularUnit);
            TriplePattern P28_4 = percent.has(symbol, "%");
            TriplePattern P28_5 = percent.has(label, "percent");
            TriplePattern P28_6 = iri(Percentage_IRI).has(label, Percentage_IRI_label);
            InsertDataQuery insert4 = Queries.INSERT_DATA(P23, P24, P25, P26, P27, P28, P28_1, P28_2, P28_3, P28_4, P28_5, P28_6);
            insert4.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
            kbClient.executeUpdate(insert4.getQueryString());
        }
    }

    public void instantiateDamper() throws FileNotFoundException {
        String Duct1_IRI = null;
        String Duct2_IRI = null;

        File file = new File(csvFilePath);
        sc = new Scanner(file);

        //Retrieve each of the necessary IRIs
        while (sc.hasNext()) {
            String string = sc.nextLine();

            String key = string.split(",")[0];
            LOGGER.info("The key is " + key + " and the string is " + string);
            switch (key) {
                case "Duct1_IRI":
                    Duct1_IRI = string.split(",")[1];
                    break;
                case "Duct2_IRI":
                    Duct2_IRI = string.split(",")[1];
                    break;
            }
        }

        //instantiate the damper instance
        String Damper_Instance = "https://www.theworldavatar.com/kg/ontobms/Damper_" + UUID.randomUUID();
        TriplePattern P1 = iri(Damper_Instance).isA(Damper);
        TriplePattern P2 = iri(Duct1_IRI).has(isConnectedTo, iri(Damper_Instance));
        TriplePattern P2_1 = iri(Damper_Instance).has(isConnectedTo, iri(Duct1_IRI));
        TriplePattern P3 = iri(Duct2_IRI).has(isConnectedTo, iri(Damper_Instance));
        TriplePattern P3_1 = iri(Damper_Instance).has(isConnectedTo, iri(Duct2_IRI));
        InsertDataQuery insert = Queries.INSERT_DATA(P1, P2, P2_1, P3, P3_1);
        insert.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM, PREFIX_S4BLDG);
        kbClient.executeUpdate(insert.getQueryString());
    }

    public void instantiateEAF() throws FileNotFoundException {
        String EAF_label = null;
        String EAFState_IRI = null;
        String EAFState_IRI_label = null;
        ArrayList<String> IRIs = new ArrayList<String>();

        File file = new File(csvFilePath);
        sc = new Scanner(file);

        //Retrieve each of the necessary labels and IRIs
        while (sc.hasNext()) {
            String string = sc.nextLine();

            String key = string.split(",")[0];
            LOGGER.info("The key is " + key + " and the string is " + string);
            switch (key) {
                case "EAF_label":
                    EAF_label = string.split(",")[1];
                    break;
                case "EAFState_IRI":
                    EAFState_IRI = string.split(",")[1];
                    break;
                case "EAFState_IRI_label":
                    EAFState_IRI_label = string.split(",")[1];
                    break;
                case "IRI":
                    IRIs.add(string.split(",")[1]);
                    break;
            }
        }

        //instantiate the EAF instance
        //include label
        String EAF_Instance = "https://www.theworldavatar.com/kg/ontobms/" + EAF_label + "_" + UUID.randomUUID();
        TriplePattern P1 = iri(EAF_Instance).isA(ExhaustFan);
        TriplePattern P2 = iri(EAF_Instance).has(label, EAF_label);
        String StatusSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/StatusSensor_" + UUID.randomUUID();
        TriplePattern P3 = iri(StatusSensor_Instance).isA(StatusSensor);
        TriplePattern P4 = iri(EAF_Instance).has(consistsOf, iri(StatusSensor_Instance));
        TriplePattern P5 = iri(EAF_Instance).has(hasState, iri(EAFState_IRI));
        TriplePattern P6 = iri(EAFState_IRI).isA(StartStopState);
        TriplePattern P6_1 = iri(EAFState_IRI).has(label, EAFState_IRI_label);
        TriplePattern P7 = iri(StatusSensor_Instance).has(observes, iri(EAFState_IRI));
        InsertDataQuery insert = Queries.INSERT_DATA(P1, P2, P3, P4, P5, P6, P6_1, P7);
        insert.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert.getQueryString());

        //instantiate all consistsOf properties
        TriplePattern P8;
        for (int i = 0; i < IRIs.size(); i++) {
            String Iri = IRIs.get(i);
            P8 = iri(EAF_Instance).has(consistsOf, iri(Iri));
            InsertDataQuery insert1 = Queries.INSERT_DATA(P8);
            insert1.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
            kbClient.executeUpdate(insert1.getQueryString());
        }
    }

    public void instantiateFCU_HE() throws FileNotFoundException {
        String FCUHE_label = null;
        String Duct1_IRI = null;
        String Duct2_IRI = null;
        String Pipe1_IRI = null;
        String Pipe2_IRI = null;

        File file = new File(csvFilePath);
        sc = new Scanner(file);

        //Retrieve each of the necessary labels and IRIs
        while (sc.hasNext()) {
            String string = sc.nextLine();

            String key = string.split(",")[0];
            LOGGER.info("The key is " + key + " and the string is " + string);
            switch (key) {
                case "FCUHE_label":
                    FCUHE_label = string.split(",")[1];
                    break;
                case "Duct1_IRI":
                    Duct1_IRI = string.split(",")[1];
                    break;
                case "Duct2_IRI":
                    Duct2_IRI = string.split(",")[1];
                    break;
                case "Pipe1_IRI":
                    Pipe1_IRI = string.split(",")[1];
                    break;
                case "Pipe2_IRI":
                    Pipe2_IRI = string.split(",")[1];
                    break;
            }
        }

        //instantiate the FCU-HE instance
        //include label
        String FCUHE_Instance = "https://www.theworldavatar.com/kg/ontobms/" + FCUHE_label + "_" + UUID.randomUUID();
        TriplePattern P1 = iri(FCUHE_Instance).isA(WaterCoil);
        TriplePattern P2 = iri(FCUHE_Instance).has(label, FCUHE_label);
        TriplePattern P3 = iri(Duct1_IRI).has(isConnectedTo, iri(FCUHE_Instance));
        TriplePattern P3_1 = iri(FCUHE_Instance).has(isConnectedTo, iri(Duct1_IRI));
        TriplePattern P4 = iri(Duct2_IRI).has(isConnectedTo, iri(FCUHE_Instance));
        TriplePattern P4_1 = iri(FCUHE_Instance).has(isConnectedTo, iri(Duct2_IRI));
        TriplePattern P5 = iri(Pipe1_IRI).has(isConnectedTo, iri(FCUHE_Instance));
        TriplePattern P5_1 = iri(FCUHE_Instance).has(isConnectedTo, iri(Pipe1_IRI));
        TriplePattern P6 = iri(Pipe2_IRI).has(isConnectedTo, iri(FCUHE_Instance));
        TriplePattern P6_1 = iri(FCUHE_Instance).has(isConnectedTo, iri(Pipe2_IRI));
        InsertDataQuery insert = Queries.INSERT_DATA(P1, P2, P3, P3_1, P4, P4_1, P5, P5_1, P6, P6_1);
        insert.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert.getQueryString());
    }

    public void instantiateFCU() throws FileNotFoundException {
        String FCU_label = null;
        String FCUState_IRI = null;
        String FCUState_IRI_label = null;
        String Temperature_IRI = null;
        String Temperature_IRI_label = null;
        String TemperatureSetpoint_IRI = null;
        String TemperatureSetpoint_IRI_label = null;
        String Room_IRI = null;
        ArrayList<String> IRIs = new ArrayList<String>();

        File file = new File(csvFilePath);
        sc = new Scanner(file);

        //Retrieve each of the necessary labels and IRIs
        while (sc.hasNext()) {
            String string = sc.nextLine();

            String key = string.split(",")[0];
            LOGGER.info("The key is " + key + " and the string is " + string);
            switch (key) {
                case "FCU_label":
                    FCU_label = string.split(",")[1];
                    break;
                case "FCUState_IRI":
                    FCUState_IRI = string.split(",")[1];
                    break;
                case "FCUState_IRI_label":
                    FCUState_IRI_label = string.split(",")[1];
                    break;
                case "Temperature_IRI":
                    Temperature_IRI = string.split(",")[1];
                    break;
                case "Temperature_IRI_label":
                    Temperature_IRI_label = string.split(",")[1];
                    break;
                case "TemperatureSetpoint_IRI":
                    TemperatureSetpoint_IRI = string.split(",")[1];
                    break;
                case "TemperatureSetpoint_IRI_label":
                    TemperatureSetpoint_IRI_label = string.split(",")[1];
                    break;
                case "Room_IRI":
                    Room_IRI = string.split(",")[1];
                    break;
                case "IRI":
                    IRIs.add(string.split(",")[1]);
                    break;
            }
        }

        //instantiate the FCU instance
        //include label
        String FCU_Instance = "https://www.theworldavatar.com/kg/ontobms/" + FCU_label + "_" + UUID.randomUUID();
        TriplePattern P1 = iri(FCU_Instance).isA(FanCoilUnit);
        TriplePattern P2 = iri(FCU_Instance).has(label, FCU_label);
        String StatusSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/StatusSensor_" + UUID.randomUUID();
        TriplePattern P3 = iri(StatusSensor_Instance).isA(StatusSensor);
        TriplePattern P4 = iri(FCU_Instance).has(consistsOf, iri(StatusSensor_Instance));
        TriplePattern P5 = iri(FCU_Instance).has(hasState, iri(FCUState_IRI));
        TriplePattern P6 = iri(FCUState_IRI).isA(StartStopState);
        TriplePattern P6_1 = iri(FCUState_IRI).has(label, FCUState_IRI_label);
        TriplePattern P7 = iri(StatusSensor_Instance).has(observes, iri(FCUState_IRI));
        InsertDataQuery insert = Queries.INSERT_DATA(P1, P2, P3, P4, P5, P6, P6_1, P7);
        insert.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert.getQueryString());

        //instantiate temperature
        String TemperatureSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/TemperatureSensor_" + UUID.randomUUID();
        TriplePattern P8 = iri(TemperatureSensor_Instance).isA(TemperatureSensor);
        TriplePattern P9 = iri(FCU_Instance).has(consistsOf, iri(TemperatureSensor_Instance));
        String Temperature_Instance = "https://www.theworldavatar.com/kg/ontobms/Temperature_" + UUID.randomUUID();
        TriplePattern P10 = iri(Temperature_Instance).isA(Temperature);
        TriplePattern P11 = iri(TemperatureSensor_Instance).has(measures, iri(Temperature_Instance));
        TriplePattern P12 = iri(Temperature_Instance).has(hasValue, iri(Temperature_IRI));
        TriplePattern P13 = iri(Temperature_IRI).isA(Measure);
        TriplePattern P13_1 = iri(Temperature_IRI).has(hasUnit, degreeCelsius);
        TriplePattern P13_2 = degreeCelsius.isA(Unit);
        TriplePattern P13_3 = degreeCelsius.isA(SingularUnit);
        TriplePattern P13_4 = degreeCelsius.has(label, "degree celsius");
        TriplePattern P13_5 = iri(Temperature_IRI).has(label,Temperature_IRI_label);
        TriplePattern P14 = iri(Room_IRI).has(hasTemperature, iri(Temperature_Instance));
        InsertDataQuery insert1 = Queries.INSERT_DATA(P8, P9, P10, P11, P12, P13, P13_1, P13_2, P13_3, P13_4, P13_5, P14);
        insert1.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert1.getQueryString());

        //instantiate temperature setpoint
        String TemperatureSetpoint_Instance = "https://www.theworldavatar.com/kg/ontobms/" + FCU_label + "_TempSetpoint_" + UUID.randomUUID();
        TriplePattern P15 = iri(Room_IRI).has(hasSetpoint, iri(TemperatureSetpoint_Instance));
        TriplePattern P16 = iri(TemperatureSetpoint_Instance).isA(Setpoint);
        String QTemperatureSetpoint_Instance = "https://www.theworldavatar.com/kg/ontobms/Q_" + FCU_label + "_TempSetpoint_" + UUID.randomUUID();
        TriplePattern P17 = iri(QTemperatureSetpoint_Instance).isA(Temperature);
        TriplePattern P18 = iri(TemperatureSetpoint_Instance).has(hasQuantity, iri(QTemperatureSetpoint_Instance));
        TriplePattern P19 = iri(QTemperatureSetpoint_Instance).has(hasValue, iri(TemperatureSetpoint_IRI));
        TriplePattern P19_1 = iri(TemperatureSetpoint_IRI).isA(Measure);
        TriplePattern P19_2 = iri(TemperatureSetpoint_IRI).has(hasUnit, degreeCelsius);
        TriplePattern P19_3 = iri(TemperatureSetpoint_IRI).has(label, TemperatureSetpoint_IRI_label);
        InsertDataQuery insert2 = Queries.INSERT_DATA(P15, P16, P17, P18, P19, P19_1, P19_2, P19_3);
        insert2.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert2.getQueryString());

        //instantiate all consistsOf properties
        TriplePattern P20;
        for (int i = 0; i < IRIs.size(); i++) {
            String Iri = IRIs.get(i);
            P20 = iri(FCU_Instance).has(consistsOf, iri(Iri));
            InsertDataQuery insert3 = Queries.INSERT_DATA(P20);
            insert3.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
            kbClient.executeUpdate(insert3.getQueryString());
        }
    }

    public void loadconfigs(String filepath) throws IOException {
        File file = new File(filepath);
        if(!file.exists())
        {
            throw new FileNotFoundException("There was no file found in the path");
        }
        
        try(InputStream input = new FileInputStream(file))
        {
            Properties prop = new Properties();
            prop.load(input);

            if(prop.containsKey("sparql.query.endpoint"))
            {
                queryEndpoint = prop.getProperty("sparql.query.endpoint");
            }
            else
            {
                throw new IOException("The file is missing: \"sparql.query.endpoint=<queryEndpoint>\"");
            }

            if(prop.containsKey("sparql.update.endpoint"))
            {
                updateEndpoint = prop.getProperty("sparql.update.endpoint");
            }
            else
            {
                throw new IOException("The file is missing: \"sparql.update.endpoint=<updateEndpoint>\"");
            }
        }
    }

}
