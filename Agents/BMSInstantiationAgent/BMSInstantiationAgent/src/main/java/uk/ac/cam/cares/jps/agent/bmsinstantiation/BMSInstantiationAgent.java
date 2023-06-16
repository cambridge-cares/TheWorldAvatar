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
    public static final String ONTOCAPE_CPS_BEHAVIOR_NS = "http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#";
    public static final String ONTOCAPE_PHASE_SYSTEM_NS = "http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#";
    public static final String ONTOCAPE_MATERIAL_NS = "http://www.theworldavatar.com/ontology/ontocape/material/material.owl#";
    public static final String ONTOCAPE_SYSTEM_NS = "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#";
    public static final String ONTOCAPE_MATERIAL_SUBSTANCE = "http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#";
    public static final String ONTOSPECIES_NS = "http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#";
    public static final String RDFS_NS = "http://www.w3.org/2000/01/rdf-schema#";
    public static final String SAREF_NS = "https://saref.etsi.org/core/";
    public static final String OM_NS = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    public static final String ONTOTIMESERIES_NS = "https://www.theworldavatar.com/kg/ontotimeseries/";
    
    
	/**
     * Prefixes
     */ 
	private static final Prefix PREFIX_ONTODEVICE = SparqlBuilder.prefix("ontodevice", iri(ONTODEVICE_NS));
	private static final Prefix PREFIX_ONTOBMS = SparqlBuilder.prefix("ontobms", iri(ONTOBMS_NS));
	private static final Prefix PREFIX_ONTOCAPE_CPS_BEHAVIOR = SparqlBuilder.prefix("ontocape_cps_behavior", iri(ONTOCAPE_CPS_BEHAVIOR_NS));
    private static final Prefix PREFIX_ONTOCAPE_PHASE_SYSTEM = SparqlBuilder.prefix("ontocape_cps_phase_system", iri(ONTOCAPE_PHASE_SYSTEM_NS));
    private static final Prefix PREFIX_ONTOCAPE_MATERIAL = SparqlBuilder.prefix("ontocape_material", iri(ONTOCAPE_MATERIAL_NS));
    private static final Prefix PREFIX_ONTOCAPE_SYSTEM = SparqlBuilder.prefix("ontocape_system", iri(ONTOCAPE_SYSTEM_NS));
    private static final Prefix PREFIX_ONTOCAPE_MATERIAL_SUBSTANCE = SparqlBuilder.prefix("ontocape_material_substance", iri(ONTOCAPE_MATERIAL_SUBSTANCE));
    private static final Prefix PREFIX_ONTOSPECIES = SparqlBuilder.prefix("ontospecies", iri(ONTOSPECIES_NS));
    private static final Prefix PREFIX_RDFS = SparqlBuilder.prefix("rdfs", iri(RDFS_NS));
    private static final Prefix PREFIX_SAREF = SparqlBuilder.prefix("saref", iri(SAREF_NS));
    private static final Prefix PREFIX_OM = SparqlBuilder.prefix("om", iri(OM_NS));
    private static final Prefix PREFIX_ONTOTIMESERIES = SparqlBuilder.prefix("ontotimeseries", iri(ONTOTIMESERIES_NS));
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
    /**
     * Classes
     */
    private static final Iri FumeHood = PREFIX_ONTOBMS.iri("FumeHood");
    private static final Iri WalkInFumeHood = PREFIX_ONTOBMS.iri("WalkInFumeHood");
    private static final Iri VAVSystem = PREFIX_ONTOBMS.iri("VAVSystem");
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
    /*
     * Instances
     */
    private static final Iri Air = PREFIX_ONTOCAPE_MATERIAL_SUBSTANCE.iri("Air");
    private static final Iri percent  = PREFIX_OM.iri("percent");
    private static final Iri cubicMetrePerHour  = PREFIX_OM.iri("cubicMetrePerHour");
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

    public void instatiateFH() throws FileNotFoundException {
        String FH_label = null;
        String SashOpening_IRI = null;
        String State_IRI = null;
        String VAV_label = null;
        String DamperState_IRI = null;
        String AirFlow_IRI = null;
        String Setpoint_IRI = null;
        
		File file = new File(csvFilePath);
        sc = new Scanner(file);

        //Retrieve each of the necessary labels and IRIs
        while (sc.hasNext()) {
            String string =  sc.next();

            String key = string.split(",")[0];
            LOGGER.info("The key is " + key + " and the string is " + string);
            switch (key) {
                case "FH_label":
                FH_label = string.split(",")[1];
                break;
                case "SashOpening_IRI":
                SashOpening_IRI = string.split(",")[1];
                break;
                case "State_IRI":
                State_IRI = string.split(",")[1];
                break;
                case "VAV_label":
                VAV_label = string.split(",")[1];
                break;
                case "DamperState_IRI":
                DamperState_IRI = string.split(",")[1];
                break;
                case "AirFlow_IRI":
                AirFlow_IRI = string.split(",")[1];
                break;
                case "Setpoint_IRI":
                Setpoint_IRI = string.split(",")[1];
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
        InsertDataQuery insert = Queries.INSERT_DATA(P1,P2, P3, P4, P5, P6);
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
        InsertDataQuery insert1 = Queries.INSERT_DATA(P7, P7_1, P8, P9, P10, P11, P12, P12_1, P12_2, P12_3, P12_4, P12_5);
        insert1.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert1.getQueryString());

        //instantiate Status Sensor
        TriplePattern P13 = iri(StatusSensor_Instance).isA(StatusSensor);
        TriplePattern P13_1 = iri(StatusSensor_Instance).has(label, FH_label + " Status Sensor");
        TriplePattern P14 = iri(StatusSensor_Instance).has(observes, iri(State_IRI));
        TriplePattern P15 = iri(State_IRI).isA(State);
        TriplePattern P16 = iri(StatusSensor_Instance).has(sendsSignalTo, iri(VAV_Instance));
        InsertDataQuery insert2 = Queries.INSERT_DATA(P13, P13_1, P14, P15, P16);
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
        InsertDataQuery insert4 = Queries.INSERT_DATA(P24, P25, P26, P27, P28, P28_1, P28_2, P28_3, P28_4);
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
        InsertDataQuery insert5 = Queries.INSERT_DATA(P29, P29_1, P30, P31, P32, P33, P33_1, P33_2, P33_3, P33_4, P33_5, P33_6);
        insert5.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM, PREFIX_ONTOCAPE_MATERIAL_SUBSTANCE);
        kbClient.executeUpdate(insert5.getQueryString());

        //instantiate damper state sensor
        TriplePattern P34 = iri(DamperStateSensor_Instance).isA(StatusSensor);
        TriplePattern P34_1 = iri(DamperStateSensor_Instance).has(label, VAV_label + " Damper State Sensor");
        TriplePattern P35 = iri(DamperStateSensor_Instance).has(sendsSignalTo, iri(VAV_Instance));
        TriplePattern P36 = iri(DamperStateSensor_Instance).has(observes, iri(DamperState_IRI));
        TriplePattern P37 = iri(DamperState_IRI).isA(DamperState);
        InsertDataQuery insert6 = Queries.INSERT_DATA(P34, P34_1, P35, P36, P37);
        insert6.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert6.getQueryString());
        }

        public void instatiateWFH() throws FileNotFoundException {
        String WFH_label = null;
        String SashOpening_IRI = null;
        String State_IRI = null;
        String VAV_label = null;
        String DamperState_IRI = null;
        String AirFlow_IRI = null;
        String Setpoint_IRI = null;
        
		File file = new File(csvFilePath);
        sc = new Scanner(file);

        //Retrieve each of the necessary labels and IRIs
        while (sc.hasNext()) {
            String string =  sc.next();

            String key = string.split(",")[0];
            LOGGER.info("The key is " + key + " and the string is " + string);
            switch (key) {
                case "WFH_label":
                WFH_label = string.split(",")[1];
                break;
                case "SashOpening_IRI":
                SashOpening_IRI = string.split(",")[1];
                break;
                case "State_IRI":
                State_IRI = string.split(",")[1];
                break;
                case "VAV_label":
                VAV_label = string.split(",")[1];
                break;
                case "DamperState_IRI":
                DamperState_IRI = string.split(",")[1];
                break;
                case "AirFlow_IRI":
                AirFlow_IRI = string.split(",")[1];
                break;
                case "Setpoint_IRI":
                Setpoint_IRI = string.split(",")[1];
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
        InsertDataQuery insert1 = Queries.INSERT_DATA(P7, P7_1, P8, P9, P10, P11, P12, P12_1, P12_2, P12_3, P12_4, P12_5);
        insert1.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert1.getQueryString());

        //instantiate Status Sensor
        TriplePattern P13 = iri(StatusSensor_Instance).isA(StatusSensor);
        TriplePattern P13_1 = iri(StatusSensor_Instance).has(label, WFH_label + " Status Sensor");
        TriplePattern P14 = iri(StatusSensor_Instance).has(observes, iri(State_IRI));
        TriplePattern P15 = iri(State_IRI).isA(State);
        TriplePattern P16 = iri(StatusSensor_Instance).has(sendsSignalTo, iri(VAV_Instance));
        InsertDataQuery insert2 = Queries.INSERT_DATA(P13, P13_1, P14, P15, P16);
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
        InsertDataQuery insert4 = Queries.INSERT_DATA(P24, P25, P26, P27, P28, P28_1, P28_2, P28_3, P28_4);
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
        InsertDataQuery insert5 = Queries.INSERT_DATA(P29, P29_1, P30, P31, P32, P33, P33_1, P33_2, P33_3, P33_4, P33_5, P33_6);
        insert5.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM, PREFIX_ONTOCAPE_MATERIAL_SUBSTANCE);
        kbClient.executeUpdate(insert5.getQueryString());

        //instantiate damper state sensor
        TriplePattern P34 = iri(DamperStateSensor_Instance).isA(StatusSensor);
        TriplePattern P34_1 = iri(DamperStateSensor_Instance).has(label, VAV_label + " Damper State Sensor");
        TriplePattern P35 = iri(DamperStateSensor_Instance).has(sendsSignalTo, iri(VAV_Instance));
        TriplePattern P36 = iri(DamperStateSensor_Instance).has(observes, iri(DamperState_IRI));
        TriplePattern P37 = iri(DamperState_IRI).isA(DamperState);
        InsertDataQuery insert6 = Queries.INSERT_DATA(P34, P34_1, P35, P36, P37);
        insert6.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
        kbClient.executeUpdate(insert6.getQueryString());
        }

        public void instatiateCH_VAV() throws FileNotFoundException {
            String CH_label = null;
            String LowLevelState_IRI = null;
            String MediumLevelState_IRI = null;
            String HighLevelState_IRI = null;
            String VAV_label = null;
            String DamperState_IRI = null;
            String AirFlow_IRI = null;
            String Setpoint_IRI = null;
            
            File file = new File(csvFilePath);
            sc = new Scanner(file);
    
            //Retrieve each of the necessary labels and IRIs
            while (sc.hasNext()) {
                String string =  sc.next();
    
                String key = string.split(",")[0];
                LOGGER.info("The key is " + key + " and the string is " + string);
                switch (key) {
                    case "CH_label":
                    CH_label = string.split(",")[1];
                    break;
                    case "LowLevelState_IRI":
                    LowLevelState_IRI = string.split(",")[1];
                    break;
                    case "MediumLevelState_IRI":
                    MediumLevelState_IRI = string.split(",")[1];
                    break;
                    case "HighLevelState_IRI":
                    HighLevelState_IRI = string.split(",")[1];
                    break;
                    case "VAV_label":
                    VAV_label = string.split(",")[1];
                    break;
                    case "DamperState_IRI":
                    DamperState_IRI = string.split(",")[1];
                    break;
                    case "AirFlow_IRI":
                    AirFlow_IRI = string.split(",")[1];
                    break;
                    case "Setpoint_IRI":
                    Setpoint_IRI = string.split(",")[1];
                    break;
                }
            }
    
            //instantiate the CH instance
            //include label
            String CH_Instance = "https://www.theworldavatar.com/kg/ontobms/" + CH_label + "_VAV_" + UUID.randomUUID();
            String State_Instance = "https://www.theworldavatar.com/kg/ontobms/State_" + UUID.randomUUID();
            String StatusSensor_Instance = "https://www.theworldavatar.com/kg/ontobms/StatusSensor_" + UUID.randomUUID();
            TriplePattern P1 = iri(CH_Instance).isA(CanopyHood);
            TriplePattern P2 = iri(CH_Instance).has(label, CH_label + "-VAV");
            TriplePattern P3 = iri(CH_Instance).has(hasState, iri(State_Instance));
            TriplePattern P4 = iri(CH_Instance).has(consistsOf, iri(StatusSensor_Instance));
            InsertDataQuery insert = Queries.INSERT_DATA(P1,P2, P3, P4);
            insert.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
            kbClient.executeUpdate(insert.getQueryString());
    
            //instantiate Status Sensor
            String VAV_Instance = "https://www.theworldavatar.com/kg/ontobms/" + VAV_label + "_" + UUID.randomUUID();
            TriplePattern P13 = iri(StatusSensor_Instance).isA(StatusSensor);
            TriplePattern P13_1 = iri(StatusSensor_Instance).has(label, CH_label + "-VAV Status Sensor");
            TriplePattern P14 = iri(StatusSensor_Instance).has(observes, iri(State_Instance));
            TriplePattern P15 = iri(State_Instance).isA(SwitchState);
            TriplePattern P16 = iri(StatusSensor_Instance).has(sendsSignalTo, iri(VAV_Instance));
            TriplePattern P16_1 = iri(State_Instance).has(hasLowLevelState, iri(LowLevelState_IRI));
            TriplePattern P16_2 = iri(LowLevelState_IRI).isA(LowLevelState);
            TriplePattern P16_3 = iri(State_Instance).has(hasMediumLevelState, iri(MediumLevelState_IRI));
            TriplePattern P16_4 = iri(MediumLevelState_IRI).isA(MediumLevelState);
            TriplePattern P16_5 = iri(State_Instance).has(hasHighLevelState, iri(HighLevelState_IRI));
            TriplePattern P16_6 = iri(HighLevelState_IRI).isA(HighLevelState);
            InsertDataQuery insert2 = Queries.INSERT_DATA(P13, P13_1, P14, P15, P16, P16_1, P16_2, P16_3, P16_4, P16_5, P16_6);
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
            InsertDataQuery insert4 = Queries.INSERT_DATA(P24, P25, P26, P27, P28, P28_1, P28_2, P28_3, P28_4);
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
            InsertDataQuery insert5 = Queries.INSERT_DATA(P29, P29_1, P30, P31, P32, P33, P33_1, P33_2, P33_3, P33_4, P33_5, P33_6);
            insert5.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM, PREFIX_ONTOCAPE_MATERIAL_SUBSTANCE);
            kbClient.executeUpdate(insert5.getQueryString());
    
            //instantiate damper state sensor
            TriplePattern P34 = iri(DamperStateSensor_Instance).isA(StatusSensor);
            TriplePattern P34_1 = iri(DamperStateSensor_Instance).has(label, VAV_label + " Damper State Sensor");
            TriplePattern P35 = iri(DamperStateSensor_Instance).has(sendsSignalTo, iri(VAV_Instance));
            TriplePattern P36 = iri(DamperStateSensor_Instance).has(observes, iri(DamperState_IRI));
            TriplePattern P37 = iri(DamperState_IRI).isA(DamperState);
            InsertDataQuery insert6 = Queries.INSERT_DATA(P34, P34_1, P35, P36, P37);
            insert6.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
            kbClient.executeUpdate(insert6.getQueryString());
        }

        public void instatiateCH_CAV() throws FileNotFoundException {
            String CH_label = null;
            String CAV_label = null;
            String DamperState_IRI = null;
            String AirFlow_IRI = null;
            String Setpoint_IRI = null;
                
            File file = new File(csvFilePath);
            sc = new Scanner(file);
        
            //Retrieve each of the necessary labels and IRIs
            while (sc.hasNext()) {
                String string =  sc.next();
        
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
                    case "AirFlow_IRI":
                    AirFlow_IRI = string.split(",")[1];
                    break;
                    case "Setpoint_IRI":
                    Setpoint_IRI = string.split(",")[1];
                    break;
                }
            }
            
            //instantiate the CH instance
            //include label
            String CH_Instance = "https://www.theworldavatar.com/kg/ontobms/" + CH_label + "_CAV_" + UUID.randomUUID();
            TriplePattern P1 = iri(CH_Instance).isA(CanopyHood);
            TriplePattern P2 = iri(CH_Instance).has(label, CH_label + "-" + CAV_label);
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
            InsertDataQuery insert4 = Queries.INSERT_DATA(P24, P25, P26, P27, P28, P28_1, P28_2, P28_3, P28_4);
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
            InsertDataQuery insert5 = Queries.INSERT_DATA(P29, P29_1, P30, P31, P32, P33, P33_1, P33_2, P33_3, P33_4, P33_5, P33_6);
            insert5.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM, PREFIX_ONTOCAPE_MATERIAL_SUBSTANCE);
            kbClient.executeUpdate(insert5.getQueryString());
        
            //instantiate damper state sensor
            TriplePattern P34 = iri(DamperStateSensor_Instance).isA(StatusSensor);
            TriplePattern P34_1 = iri(DamperStateSensor_Instance).has(label, CAV_label  + " Damper State Sensor");
            TriplePattern P35 = iri(DamperStateSensor_Instance).has(sendsSignalTo, iri(CAV_Instance));
            TriplePattern P36 = iri(DamperStateSensor_Instance).has(observes, iri(DamperState_IRI));
            TriplePattern P37 = iri(DamperState_IRI).isA(DamperState);
            InsertDataQuery insert6 = Queries.INSERT_DATA(P34, P34_1, P35, P36, P37);
            insert6.prefix(PREFIX_ONTOBMS, PREFIX_RDFS, PREFIX_ONTODEVICE, PREFIX_SAREF, PREFIX_OM);
            kbClient.executeUpdate(insert6.getQueryString());
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
