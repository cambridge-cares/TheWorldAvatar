package uk.ac.cam.cares.jps.agent.historicalntuenergy;
import org.json.JSONObject;
import org.json.JSONArray;
import uk.ac.cam.cares.jps.base.util.JSONKeyToIRIMapper;

import java.io.File;
import java.io.FileInputStream;
import java.util.ArrayList;
import java.io.IOException;
import java.io.InputStream;
import java.util.*;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import java.util.List;
import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import org.eclipse.rdf4j.sparqlbuilder.core.query.InsertDataQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;

public class HistoricalQueryBuilder {

    private static final Logger LOGGER = LogManager.getLogger(HistoricalNTUEnergyAgentLauncher.class);

    /**
     * Prefixes
     */
    public static final String PowsysPrefix = "https://www.theworldavatar.com/kg/ontopowsys/";
    public static final String builtEnvPrefix = "https://www.theworldavatar.com/kg/ontobuiltenv/";
    public static final String OmPrefix = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    public static final String OntoCapeUnitPrefix = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#";
    public static final String generatedIRIPrefix = OmPrefix + "measure";


    /**
     * Classes
     */
    public static final String powerSystem = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#PowerSystem";
    public static final String building = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#Building";
    public static final String powerSystemModel = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#PowerSystemModel";
    public static final String busNode = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#BusNode";
    public static final String powerGenerator = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#PowerGenerator";
    public static final String branch = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#UndergroundCable";
    public static final String absorbedActivePower = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#AbsorbedActivePower";
    public static final String absorbedReactivePower = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#AbsorbedReactivePower";
    public static final String voltageAngle = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#VoltageAngle";
    public static final String voltageMagnitude = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#VoltageMagnitude";
    public static final String SIDerivedUnit = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#SI_DerivedUnit";
    public static final String photovoltaicPanel = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#PhotovoltaicPanel";

    public static final String BaseTestingIrradiance = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#BaseTestingIrradiance";
    public static final String BaseTestingTemperature = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#BaseTestingTemperature";
    public static final String NominalOperatingTemperature = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#NominalOperatingTemperature";
    public static final String PanelArea = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#PanelArea";
    public static final String PanelLength = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#PanelLength";
    public static final String PanelWidth = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#PanelWidth";
    public static final String RatedCurrent = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#RatedCurrent";
    public static final String RatedVoltage = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#RatedVoltage";
    public static final String RatedEfficiency = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#RatedEfficiency";
    public static final String RatedPower = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#RatedPower";
    public static final String TemperatureCoefficientOfPower = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#TemperatureCoefficientOfPower";
    public static final String Tilt = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#Tilt";
    public static final String University = "https://www.theworldavatar.com/kg/ontobuiltenv/University";



    // undeveloped NTU ontologies
    public static final String venue = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#Venue";
    public static final String classSchedule = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#ClassSchedule";

    /**
     * Model variables
     */
    public static final String PowerSystemModelPrefix = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#";
    public static final String PowerSystemRealizaionPrefix = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#";
    public static final String modelVariableSpecification = "http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#ModelVariableSpecification";
    public static final String Measure = "http://www.ontology-of-units-of-measure.org/resource/om-2/Measure";


    /**
     * Predicates
     */
    private static final String OMHasValue = OmPrefix + "hasValue";
    private static final String OntoCapeHasValue = "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue";
    private static final String hasActivePowerAbsorbed = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasActivePowerAbsorbed";
    private static final String hasReactivePowerAbsorbed = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasReactivePowerAbsorbed";
    private static final String hasActivePowerGenerated = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasActivePowerGenerated";
    private static final String hasReactivePowerGenerated = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasReactivePowerGenerated";
    private static final String hasVoltageAngle = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasVoltageAngle";
    private static final String hasVoltageMagnitude = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasVoltageMagnitude";

    private static final String hasUnit = OmPrefix + "hasUnit";
    private static final String hasSubSystem = "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasSubsystem";
    private static final String hasBusNode = "http://www.theworldavatar.com/ontology/ontopowsys/OntoPowSys.owl#hasBusNode";
    private static final String isModeledBy = "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isModeledBy";
    private static final String hasModelVariable = "http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable";
    private static final String numericalValue = "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue";
    private static final String hasUnitOfMeasure = "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure";
    private static final String rdfsLabel = "http://www.w3.org/2000/01/rdf-schema#label";
    private static final String contains = "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#contains";
    private static final String hasBaseTestingIrradiance = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#hasBaseTestingIrradiance";
    private static final String hasBaseTestingTemperature = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#hasBaseTestingTemperature";
    private static final String hasNominalOperatingTemperature = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#hasNominalOperatingTemperature";
    private static final String hasPanelLength = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#hasPanelLength";
    private static final String hasPanelArea = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#hasPanelArea";
    private static final String hasPanelWidth = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#hasPanelWidth";
    private static final String hasRatedCurrent = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#hasRatedCurrent";
    private static final String hasRatedVoltage = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#hasRatedVoltage";
    private static final String hasRatedEfficiency = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#hasRatedEfficiency";
    private static final String hasTemperatureCoeffOfPower = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#hasTemperatureCoeffOfPower";
    private static final String hasRatedPower = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#hasRatedPower";
    private static final String hasTiltAngle = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#hasTiltAngle";
    private static final String hasOntoCityGML = "https://www.theworldavatar.com/kg/ontobuiltenv/hasOntoCityGMLRepresentation";



    // Undeveloped venue-related ontologies
    private static final String hasVenueName = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#hasVenueName";
    private static final String hasVenueCapacity = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#hasVenueCapacity";
    private static final String hasLocation = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#hasLocation";
    private static final String hasBookableByStaff = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#hasBookableByStaff";
    private static final String hasBookableByStudent = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#hasBookableByStudent";

    // Undeveloped class-related ontologies
    private static final String hasType = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#hasType";
    private static final String hasGroup = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#hasGroup";
    private static final String hasDay = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#hasDay";
    private static final String hasTime = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#hasTime";
    private static final String hasVenue = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#hasVenue";
    private static final String hasRemark = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#hasRemark";
    private static final String hasCode = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#hasCode";
    private static final String hasName = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#hasName";
    private static final String hasStudentProfiles = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#hasStudentProfiles";
    private static final String hasPropertyUsage = "https://www.theworldavatar.com/kg/ontobuiltenv/hasPropertyUsage";

    /**
     * Individuals
     */
    private static final String kilowatt = OmPrefix + "kilowatt";
    private static final String kilovoltamperereactive = OmPrefix + "kilovoltamperereactive";
    private static final String omDegree = OmPrefix + "degree";
    private static final String kilovolt = OmPrefix + "kilovolt";
    public static final String MW = OntoCapeUnitPrefix + "MW";
    public static final String Mvar = OntoCapeUnitPrefix + "Mvar";
    public static final String MVA = OntoCapeUnitPrefix + "MVA";
    public static final String degree = OntoCapeUnitPrefix + "degree";
    public static final String kV = OntoCapeUnitPrefix + "kV";


    /**
     * NTU building to Bus number mapping
     */
    HashMap<String, Integer> NTUBuildingToBusNum = new HashMap<>();
    HashMap<String, Integer> NTUBuildingToGMLRepresentation = new HashMap<>();

    /**
     * Building abbreviations to full name mapping
     */
    HashMap<String, String> BuildingAbbrevToName = new HashMap<>();

    /**
     * Branch-related parameters stored in a hashmap
     */
    HashMap<String, List<String>> branchParams = new HashMap<String, List<String>>();
    /**
     * Generator-related parameters stored in a hashmap
     */
    HashMap<String, List<String>> generatorParams = new HashMap<String, List<String>>();
    /**
     * PV-related parameters stored in a hashmap
     */
    HashMap<String, List<String>> pvParams = new HashMap<String, List<String>>();

    /**
     * Venue-related parameters stored in a hashmap
     */
    HashMap<String, List<String>> NTUVenues = new HashMap<String, List<String>>();

    /**
     * ClassSchedule-related parameters stored in a hashmap
     */
    HashMap<String, List<String>> NTUClassSchedule = new HashMap<String, List<String>>();

    /**
     * Bus-related parameters stored in a hashmap
     */
    HashMap<String, List<String>> busParams = new HashMap<String, List<String>>();

    /**
     * HashMaps for model variables
     */
    HashMap<String, String> BusNodeModelVariables = new HashMap<>();
    HashMap<String, String> BranchModelVariables = new HashMap<>();
    HashMap<String, String> GeneratorModelVariables = new HashMap<>();
    HashMap<String, String> PVModelVariables = new HashMap<>();

    RemoteStoreClient kbClient;
    public String agentProperties;

    private List<JSONKeyToIRIMapper> mappings;

    public HistoricalQueryBuilder(String agentProp, RemoteStoreClient kbClient, JSONArray busNodeSpecs, JSONArray branchSpecs, JSONArray generatorSpecs, JSONArray pvSpecs, JSONArray venueInfo) throws IOException
    {
        agentProperties = agentProp;
        loadproperties(agentProperties);
        this.kbClient = kbClient;

        // Specifies the relationship between building names and bus node numbers
        // This can be improved by importing a data file
        NTUBuildingToBusNum.put("GENERATOR_NODE", 1);
        NTUBuildingToBusNum.put("N1_3", 2);
        NTUBuildingToBusNum.put("NYA", 3);
        NTUBuildingToBusNum.put("CANTEEN_2", 4);
        NTUBuildingToBusNum.put("CONNECTION_NODE", 5);
        NTUBuildingToBusNum.put("SBS", 6);
        NTUBuildingToBusNum.put("EMB", 7);
        NTUBuildingToBusNum.put("RTP", 8);
        NTUBuildingToBusNum.put("N_2", 9);
        NTUBuildingToBusNum.put("N_2_1", 10);
        NTUBuildingToBusNum.put("SPMS", 11);
        NTUBuildingToBusNum.put("HALL_4", 12);
        NTUBuildingToBusNum.put("PIONEER_HALL", 13);
        NTUBuildingToBusNum.put("NEC", 14);
        NTUBuildingToBusNum.put("THE_WAVE", 15);

        NTUBuildingToGMLRepresentation.put("N1_3", 40);
        NTUBuildingToGMLRepresentation.put("NYA", 442);
        NTUBuildingToGMLRepresentation.put("CANTEEN_2", 190);
        NTUBuildingToGMLRepresentation.put("SBS", 120);
        NTUBuildingToGMLRepresentation.put("EMB", 1);
        NTUBuildingToGMLRepresentation.put("RTP", 92);
        NTUBuildingToGMLRepresentation.put("N_2", 47);
        NTUBuildingToGMLRepresentation.put("N_2_1", 454);
        NTUBuildingToGMLRepresentation.put("SPMS", 488);
        NTUBuildingToGMLRepresentation.put("HALL_4", 17);
        NTUBuildingToGMLRepresentation.put("PIONEER_HALL", 387);
        NTUBuildingToGMLRepresentation.put("NEC", 399);
        NTUBuildingToGMLRepresentation.put("THE_WAVE", 277);


        // Specifies the relationship between building abbreviations and full names
        BuildingAbbrevToName.put("GENERATOR_NODE", "Generator Node");
        BuildingAbbrevToName.put("N1_3", "Block N1.3");
        BuildingAbbrevToName.put("NYA", "Nanyang Auditorium");
        BuildingAbbrevToName.put("CANTEEN_2", "Canteen 2");
        BuildingAbbrevToName.put("CONNECTION_NODE", "Connection Node");
        BuildingAbbrevToName.put("SBS", "School of Biological Sciences");
        BuildingAbbrevToName.put("EMB", "Experimental Medicine Building");
        BuildingAbbrevToName.put("RTP", "Research Techno Plaza");
        BuildingAbbrevToName.put("N_2", "Block N2");
        BuildingAbbrevToName.put("N_2_1", "Block N2.1");
        BuildingAbbrevToName.put("SPMS", "School of Physical and Mathematical Sciences");
        BuildingAbbrevToName.put("HALL_4", "Hall of Residence 4");
        BuildingAbbrevToName.put("PIONEER_HALL", "Pioneer Hall");
        BuildingAbbrevToName.put("NEC", "Nanyang Executive Centre");
        BuildingAbbrevToName.put("THE_WAVE", "The Wave");

        // Specifies the bus node parameters
        for (int i = 0; i < busNodeSpecs.length(); i++) {
            JSONObject jsonObject = busNodeSpecs.getJSONObject(i);
            for (String key : jsonObject.keySet()) {
                String value = jsonObject.getString(key);
                if (!busParams.containsKey(key)) {
                    busParams.put(key, new ArrayList<>());
                }
                busParams.get(key).add(value);
            }
        }
        // Add BusNode model variables class URIs
        for (String key : busParams.keySet()) {
            BusNodeModelVariables.put(key, PowerSystemModelPrefix + key);
        }

        // Specifies Branch model parameters
        for (int i = 0; i < branchSpecs.length(); i++) {
            JSONObject jsonObject = branchSpecs.getJSONObject(i);
            for (String key : jsonObject.keySet()) {
                String value = jsonObject.getString(key);
                if (!branchParams.containsKey(key)) {
                    branchParams.put(key, new ArrayList<>());
                }
                branchParams.get(key).add(value);
            }
        }
        // Add Branch model variables class URIs
        for (String key : branchParams.keySet()) {
            BranchModelVariables.put(key, PowerSystemModelPrefix + key);
        }

        // Specifies the power generator parameters
        for (int i = 0; i < generatorSpecs.length(); i++) {
            JSONObject jsonObject = generatorSpecs.getJSONObject(i);
            for (String key : jsonObject.keySet()) {
                String value = jsonObject.getString(key);
                if (!generatorParams.containsKey(key)) {
                    generatorParams.put(key, new ArrayList<>());
                }
                generatorParams.get(key).add(value);
            }
        }
        // Add Generator model variables class URIs
        for (String key : generatorParams.keySet()) {
            GeneratorModelVariables.put(key, PowerSystemModelPrefix + key);
        }

        // Specifies the PV parameters
        for (int i = 0; i < pvSpecs.length(); i++) {
            JSONObject jsonObject = pvSpecs.getJSONObject(i);
            for (String key : jsonObject.keySet()) {
                String value = jsonObject.getString(key);
                if (!pvParams.containsKey(key)) {
                    pvParams.put(key, new ArrayList<>());
                }
                pvParams.get(key).add(value);
            }
        }
        // Add PV variables class URIs
        for (String key : pvParams.keySet()) {
            PVModelVariables.put(key, PowerSystemRealizaionPrefix + key);
        }

        // pop up venue information in the hashmap
        for (int i = 0; i < venueInfo.length(); i++) {
            JSONObject jsonObject = venueInfo.getJSONObject(i);
            for (String key : jsonObject.keySet()) {
                String value = jsonObject.getString(key);
                if (!NTUVenues.containsKey(key)) {
                    NTUVenues.put(key, new ArrayList<>());
                }
                NTUVenues.get(key).add(value);
            }
        }

    }

    public void loadproperties(String propfile) throws IOException
    {
        try(InputStream input = new FileInputStream(propfile))
        {
            Properties prop = new Properties();
            prop.load(input);
            String mappingfolder;
            try
            {
                mappingfolder = System.getenv(prop.getProperty("ntuenergy.mappingfolder"));
            }
            catch(NullPointerException e)
            {
                throw new IOException("The key ntuenergy.mappingfolder cannot be found");
            }
            if(mappingfolder == null)
            {
                throw new InvalidPropertiesFormatException("The properties file does not contain the key ntuenergy.mappingfolder with a path to the folder containing the required JSON key to IRI Mappings");
            }
            mappings = new ArrayList<>();
            File folder = new File(mappingfolder);
            File[] mappingFiles = folder.listFiles();

            if(mappingFiles.length == 0)
            {
                throw new IOException("No files in folder");
            }
            else
            {
                for(File mappingFile : mappingFiles)
                {
                    JSONKeyToIRIMapper mapper = new JSONKeyToIRIMapper(generatedIRIPrefix, mappingFile.getAbsolutePath());
                    mappings.add(mapper);
                    mapper.saveToFile(mappingFile.getAbsolutePath());
                }
            }
        }
    }
    /**
     * Given an individual IRI, return the name of the building
     * @param IRI
     * @return nameOfBuilding
     */
    public String getBuildingNameFromIRI(String IRI){
        int firstUnderscoreIndex = IRI.indexOf('_');
        if (firstUnderscoreIndex != -1) {
            int endIndex = IRI.indexOf("_Q_KVAR", firstUnderscoreIndex + 1);
            if (endIndex == -1) {
                endIndex = IRI.indexOf("_P_KW", firstUnderscoreIndex + 1);
            }
            if (endIndex == -1) {
                endIndex = IRI.indexOf("_VA_DEGREE", firstUnderscoreIndex + 1);
            }
            if (endIndex == -1) {
                endIndex = IRI.indexOf("_VM_KV", firstUnderscoreIndex + 1);
            }
            if (endIndex == -1) {
                endIndex = IRI.indexOf("_GQ_KVAR", firstUnderscoreIndex + 1);
            }
            if (endIndex == -1) {
                endIndex = IRI.indexOf("_GP_KW", firstUnderscoreIndex + 1);
            }
            if (endIndex != -1) {
                return IRI.substring(firstUnderscoreIndex + 1, endIndex);
            }
        }
        return null;
    }
    public String getBuildingNameFromBusNum(Integer busNum){
        for (Map.Entry<String, Integer> entry : NTUBuildingToBusNum.entrySet()) {
            if (entry.getValue().equals(busNum)) {
                return entry.getKey();
            }
        }
        return null;
    }

    /**
     * Instantiate all triples to instantiate NTU energy consumption KG
     * Design details of the KG can be found:
     * https://miro.com/app/board/uXjVPiu-6dg=/?share_link_id=810452198946
     */
    public void instantiateTriples(){
        String powerSystemIRI = PowsysPrefix + "PowerSystem_NTU";
        TriplePattern powerSysisType = iri(powerSystemIRI).isA(iri(powerSystem));
        InsertDataQuery powersysInsertion = Queries.INSERT_DATA(powerSysisType);
        kbClient.executeUpdate(powersysInsertion.getQueryString());

        /**
         * Instantiate Venue-related triples
         */
        for (int entry=0; entry < NTUVenues.get("FACILITY").size(); entry++){

            String venueIRI = PowsysPrefix + "NTU_Venue_" + String.valueOf(entry);

            String venueName = NTUVenues.get("FACILITY").get(entry);
            LOGGER.info("Instantiating Venue name: " + venueName);
            String venueCapacity = NTUVenues.get("CAPACITY").get(entry);
            String location = NTUVenues.get("LOCATION").get(entry);
            String bookableByStaff = NTUVenues.get("Bookable by staff").get(entry);
            String bookableByStudent = NTUVenues.get("Bookable by student organisations").get(entry);

            TriplePattern venueIsType = iri(venueIRI).isA(iri(venue));
            TriplePattern venueHasName = iri(venueIRI).has(iri(hasVenueName), venueName);
            TriplePattern venueHasCapacity = iri(venueIRI).has(iri(hasVenueCapacity), venueCapacity);
            TriplePattern venueHasLocation = iri(venueIRI).has(iri(hasLocation), location);
            TriplePattern venueHasBookableByStaff = iri(venueIRI).has(iri(hasBookableByStaff), bookableByStaff);
            TriplePattern venueHasBookableByStudent = iri(venueIRI).has(iri(hasBookableByStudent), bookableByStudent);

            InsertDataQuery venueInsertion = Queries.INSERT_DATA(venueIsType, venueHasName, venueHasCapacity, venueHasLocation, venueHasBookableByStaff, venueHasBookableByStudent);
            kbClient.executeUpdate(venueInsertion.getQueryString());
        }



        /**
         * Instantiate PV-related triples
         */
        for (int entry=0; entry < pvParams.get("BuildingName").size(); entry++){
            String buildingFullName = pvParams.get("BuildingName").get(entry);
            String buildingIRI = null;
            for (Map.Entry<String, String> buildingEntry : BuildingAbbrevToName.entrySet()) {
                if (buildingEntry.getValue().equals(buildingFullName)) {
                    buildingIRI = PowsysPrefix + "NTU_Building_" + buildingEntry.getKey();
                }
            }
            if (buildingIRI == null) {
                throw new NullPointerException("The building name in PV_specs cannot be found in the system");
            }

            String PVIRI = PowsysPrefix + "NTU_PV_" + String.valueOf(entry);
            TriplePattern PVisType = iri(PVIRI).isA(iri(photovoltaicPanel));

            String baseTestIrradianceIRI = PowsysPrefix + "NTU_PV_" + String.valueOf(entry) + "_BaseTestIrradiance";
            TriplePattern baseTestIrradianceisType = iri(baseTestIrradianceIRI).isA(iri(BaseTestingIrradiance));
            String baseTestTemperatureIRI = PowsysPrefix + "NTU_PV_" + String.valueOf(entry) + "_BaseTestTemperature";
            TriplePattern baseTestTemperatureisType = iri(baseTestTemperatureIRI).isA(iri(BaseTestingTemperature));
            String nominaltemperatureIRI = PowsysPrefix + "NTU_PV_" + String.valueOf(entry) + "_NominalTemperature";
            TriplePattern nominaltemperatureisType = iri(nominaltemperatureIRI).isA(iri(NominalOperatingTemperature));
            String panelareaIRI = PowsysPrefix + "NTU_PV_" + String.valueOf(entry) + "_PanelArea";
            TriplePattern panelareaisType = iri(panelareaIRI).isA(iri(PanelArea));
            String panellengthIRI = PowsysPrefix + "NTU_PV_" + String.valueOf(entry) + "_PanelLength";
            TriplePattern panellengthisType = iri(panellengthIRI).isA(iri(PanelLength));
            String panelwidthIRI = PowsysPrefix + "NTU_PV_" + String.valueOf(entry) + "_PanelWidth";
            TriplePattern panelwidthisType = iri(panelwidthIRI).isA(iri(PanelWidth));
            String ratedcurrentIRI = PowsysPrefix + "NTU_PV_" + String.valueOf(entry) + "_RatedCurrent";
            TriplePattern ratedcurrentisType = iri(ratedcurrentIRI).isA(iri(RatedCurrent));
            String ratedvoltageIRI = PowsysPrefix + "NTU_PV_" + String.valueOf(entry) + "_RatedVoltage";
            TriplePattern ratedvoltageisType = iri(ratedvoltageIRI).isA(iri(RatedVoltage));
            String ratedefficiencyIRI = PowsysPrefix + "NTU_PV_" + String.valueOf(entry) + "_RatedEfficiency";
            TriplePattern ratedefficiencyisType = iri(ratedefficiencyIRI).isA(iri(RatedEfficiency));
            String ratedpowerIRI = PowsysPrefix + "NTU_PV_" + String.valueOf(entry) + "_RatedPower";
            TriplePattern ratedpowerisType = iri(ratedpowerIRI).isA(iri(RatedPower));
            String temperaturecoefficientIRI = PowsysPrefix + "NTU_PV_" + String.valueOf(entry) + "_TemperatureCoefficient";
            TriplePattern temperaturecoefficientisType = iri(temperaturecoefficientIRI).isA(iri(TemperatureCoefficientOfPower));
            String tiltIRI = PowsysPrefix + "NTU_PV_" + String.valueOf(entry) + "_Tilt";
            TriplePattern tiltisType = iri(tiltIRI).isA(iri(Tilt));

            TriplePattern BuildingHasPV = iri(buildingIRI).has(iri(contains), iri(PVIRI));
            TriplePattern PVHasBTR = iri(PVIRI).has(iri(hasBaseTestingIrradiance), iri(baseTestIrradianceIRI));
            TriplePattern PVHasBTT = iri(PVIRI).has(iri(hasBaseTestingTemperature), iri(baseTestTemperatureIRI));
            TriplePattern PVHasNT = iri(PVIRI).has(iri(hasNominalOperatingTemperature), iri(nominaltemperatureIRI));
            TriplePattern PVHasPA = iri(PVIRI).has(iri(hasPanelArea), iri(panelareaIRI));
            TriplePattern PVHasPL = iri(PVIRI).has(iri(hasPanelLength), iri(panellengthIRI));
            TriplePattern PVHasPW = iri(PVIRI).has(iri(hasPanelWidth), iri(panelwidthIRI));
            TriplePattern PVHasRC = iri(PVIRI).has(iri(hasRatedCurrent), iri(ratedcurrentIRI));
            TriplePattern PVHasRV = iri(PVIRI).has(iri(hasRatedVoltage), iri(ratedvoltageIRI));
            TriplePattern PVHasRE = iri(PVIRI).has(iri(hasRatedEfficiency), iri(ratedefficiencyIRI));
            TriplePattern PVHasRP = iri(PVIRI).has(iri(hasRatedPower), iri(ratedpowerIRI));
            TriplePattern PVHasTC = iri(PVIRI).has(iri(hasTemperatureCoeffOfPower), iri(temperaturecoefficientIRI));
            TriplePattern PVHasT = iri(PVIRI).has(iri(hasTiltAngle), iri(tiltIRI));

            String BTRScalarValueIRI = PowsysPrefix + "PV_" + String.valueOf(entry) + "_ScalarValue_" + "BaseTestIrradiance";
            TriplePattern BTRHasValue = iri(baseTestIrradianceIRI).has(iri(OntoCapeHasValue), iri(BTRScalarValueIRI));
            String BTTScalarValueIRI = PowsysPrefix + "PV_" + String.valueOf(entry) + "_ScalarValue_" + "BaseTestTemperature";
            TriplePattern BTTHasValue = iri(baseTestTemperatureIRI).has(iri(OntoCapeHasValue), iri(BTTScalarValueIRI));
            String NTScalarValueIRI = PowsysPrefix + "PV_" + String.valueOf(entry) + "_ScalarValue_" + "NominalTemperature";
            TriplePattern NTHasValue = iri(nominaltemperatureIRI).has(iri(OntoCapeHasValue), iri(NTScalarValueIRI));
            String PAScalarValueIRI = PowsysPrefix + "PV_" + String.valueOf(entry) + "_ScalarValue_" + "PanelArea";
            TriplePattern PAHasValue = iri(panelareaIRI).has(iri(OntoCapeHasValue), iri(PAScalarValueIRI));
            String PLScalarValueIRI = PowsysPrefix + "PV_" + String.valueOf(entry) + "_ScalarValue_" + "PanelLength";
            TriplePattern PLHasValue = iri(panellengthIRI).has(iri(OntoCapeHasValue), iri(PLScalarValueIRI));
            String PWScalarValueIRI = PowsysPrefix + "PV_" + String.valueOf(entry) + "_ScalarValue_" + "PanelWidth";
            TriplePattern PWHasValue = iri(panelwidthIRI).has(iri(OntoCapeHasValue), iri(PWScalarValueIRI));
            String RCScalarValueIRI = PowsysPrefix + "PV_" + String.valueOf(entry) + "_ScalarValue_" + "RatedCurrent";
            TriplePattern RCHasValue = iri(ratedcurrentIRI).has(iri(OntoCapeHasValue), iri(RCScalarValueIRI));
            String RVScalarValueIRI = PowsysPrefix + "PV_" + String.valueOf(entry) + "_ScalarValue_" + "RatedVoltage";
            TriplePattern RVHasValue = iri(ratedvoltageIRI).has(iri(OntoCapeHasValue), iri(RVScalarValueIRI));
            String REScalarValueIRI = PowsysPrefix + "PV_" + String.valueOf(entry) + "_ScalarValue_" + "RatedEfficiency";
            TriplePattern REHasValue = iri(ratedefficiencyIRI).has(iri(OntoCapeHasValue), iri(REScalarValueIRI));
            String RPScalarValueIRI = PowsysPrefix + "PV_" + String.valueOf(entry) + "_ScalarValue_" + "RatedPower";
            TriplePattern RPHasValue = iri(ratedpowerIRI).has(iri(OntoCapeHasValue), iri(RPScalarValueIRI));
            String TCScalarValueIRI = PowsysPrefix + "PV_" + String.valueOf(entry) + "_ScalarValue_" + "TemperatureCoefficient";
            TriplePattern TCHasValue = iri(temperaturecoefficientIRI).has(iri(OntoCapeHasValue), iri(TCScalarValueIRI));
            String TScalarValueIRI = PowsysPrefix + "PV_" + String.valueOf(entry) + "_ScalarValue_" + "Tilt";
            TriplePattern THasValue = iri(tiltIRI).has(iri(OntoCapeHasValue), iri(TScalarValueIRI));

            String BTRnumValue = pvParams.get("BaseTestingIrradiance").get(entry);
            TriplePattern SpecNumericalValue = iri(BTRScalarValueIRI).has(iri(numericalValue), BTRnumValue);
            String BTTnumValue = pvParams.get("BaseTestingTemperature").get(entry);
            TriplePattern BTTSpecNumericalValue = iri(BTTScalarValueIRI).has(iri(numericalValue), BTTnumValue);
            String NTnumValue = pvParams.get("NominalOperatingTemperature").get(entry);
            TriplePattern NTSpecNumericalValue = iri(NTScalarValueIRI).has(iri(numericalValue), NTnumValue);
            String PAnumValue = pvParams.get("PanelArea").get(entry);
            TriplePattern PASpecNumericalValue = iri(PAScalarValueIRI).has(iri(numericalValue), PAnumValue);
            String PLnumValue = pvParams.get("PanelLength").get(entry);
            TriplePattern PLSpecNumericalValue = iri(PLScalarValueIRI).has(iri(numericalValue), PLnumValue);
            String PWnumValue = pvParams.get("PanelWidth").get(entry);
            TriplePattern PWSpecNumericalValue = iri(PWScalarValueIRI).has(iri(numericalValue), PWnumValue);
            String RCnumValue = pvParams.get("RatedCurrent").get(entry);
            TriplePattern RCSpecNumericalValue = iri(RCScalarValueIRI).has(iri(numericalValue), RCnumValue);
            String RVnumValue = pvParams.get("RatedVoltage").get(entry);
            TriplePattern RVSpecNumericalValue = iri(RVScalarValueIRI).has(iri(numericalValue), RVnumValue);
            String REnumValue = pvParams.get("RatedEfficiency").get(entry);
            TriplePattern RESpecNumericalValue = iri(REScalarValueIRI).has(iri(numericalValue), REnumValue);
            String RPnumValue = pvParams.get("RatedPower").get(entry);
            TriplePattern RPSpecNumericalValue = iri(RPScalarValueIRI).has(iri(numericalValue), RPnumValue);
            String TCnumValue = pvParams.get("TemperatureCoefficientOfPower").get(entry);
            TriplePattern TCSpecNumericalValue = iri(TCScalarValueIRI).has(iri(numericalValue), TCnumValue);
            String TnumValue = pvParams.get("Tilt").get(entry);
            TriplePattern TSpecNumericalValue = iri(TScalarValueIRI).has(iri(numericalValue), TnumValue);

            InsertDataQuery pvInsertion = Queries.INSERT_DATA(BuildingHasPV, PVisType, baseTestIrradianceisType, baseTestTemperatureisType,
                    nominaltemperatureisType, panelareaisType, panellengthisType, panelwidthisType, ratedcurrentisType, ratedvoltageisType,
                    ratedefficiencyisType, ratedpowerisType, temperaturecoefficientisType, tiltisType, BuildingHasPV, PVHasBTR, PVHasBTT,
                    PVHasNT, PVHasPA, PVHasPL, PVHasPW, PVHasRC, PVHasRV, PVHasRE, PVHasRP, PVHasTC, PVHasT, BTRHasValue, BTTHasValue,
                    NTHasValue, PAHasValue, PLHasValue, PWHasValue, RCHasValue, RVHasValue, REHasValue, RPHasValue, TCHasValue, THasValue,
                    SpecNumericalValue, BTTSpecNumericalValue, NTSpecNumericalValue, PASpecNumericalValue, PLSpecNumericalValue,
                    PWSpecNumericalValue, RCSpecNumericalValue, RVSpecNumericalValue, RESpecNumericalValue, RPSpecNumericalValue,
                    TCSpecNumericalValue, TSpecNumericalValue);
            kbClient.executeUpdate(pvInsertion.getQueryString());
        }


        /**
         * Instantiate GENERATOR-related triples
         */
        for (int entry=0; entry < 1; entry++){
            String powerGeneratorIRI = PowsysPrefix + "NTU_PowerGenerator_" + String.valueOf(entry);
            String GeneratorModelIRI = PowsysPrefix + "NTU_PowerGenerator_" +  String.valueOf(entry) + "_Model";
            TriplePattern PSHasGenerator = iri(powerSystemIRI).has(iri(hasSubSystem), iri(powerGeneratorIRI));
            TriplePattern GeneratorisType = iri(powerGeneratorIRI).isA(iri(powerGenerator));
            TriplePattern GeneratorIsModeledByModel = iri(powerGeneratorIRI).has(iri(isModeledBy), iri(GeneratorModelIRI));
            TriplePattern GeneratorModelisType = iri(GeneratorModelIRI).isA(iri(powerSystemModel));
            InsertDataQuery generatorInsertion1 = Queries.INSERT_DATA(PSHasGenerator, GeneratorisType, GeneratorIsModeledByModel, GeneratorModelisType);
            kbClient.executeUpdate(generatorInsertion1.getQueryString());

            for (Map.Entry<String, String> set : GeneratorModelVariables.entrySet()) {
                String GeneratorModelVariable = set.getValue();
                String GeneratorModelName = set.getKey();
                String GeneratorModelVariableIRI = PowsysPrefix + "PowerGenerator_" + String.valueOf(entry) + "_ModelVariable_" + GeneratorModelName;
                TriplePattern ModelHasVariable = iri(GeneratorModelIRI).has(iri(hasModelVariable), iri(GeneratorModelVariableIRI));
                TriplePattern VariableisTypeVariable = iri(GeneratorModelVariableIRI).isA(iri(GeneratorModelVariable));
                String ModelVariableSpecificationIRI = PowsysPrefix + "PowerGenerator_" + String.valueOf(entry) + "_ModelVariableSpec_" + GeneratorModelName;
                TriplePattern GeneratorModelVariableHasValueSpec = iri(GeneratorModelVariableIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationIRI));
                TriplePattern ModelVariableSpecificationisType = iri(ModelVariableSpecificationIRI).isA(iri(modelVariableSpecification));
                String numValue = generatorParams.get(GeneratorModelName).get(entry);
                TriplePattern SpecNumericalValue = iri(ModelVariableSpecificationIRI).has(iri(numericalValue), numValue);
                TriplePattern SpecUnit = null;
                TriplePattern SpecUnitIsType = null;
                if(ModelVariableSpecificationIRI.contains("QMax")){
                    SpecUnit = iri(ModelVariableSpecificationIRI).has(iri(hasUnitOfMeasure), iri(Mvar));
                    SpecUnitIsType = iri(Mvar).isA(iri(SIDerivedUnit));
                }
                if(ModelVariableSpecificationIRI.contains("QMin")){
                    SpecUnit = iri(ModelVariableSpecificationIRI).has(iri(hasUnitOfMeasure), iri(Mvar));
                    SpecUnitIsType = iri(Mvar).isA(iri(SIDerivedUnit));
                }
                if(ModelVariableSpecificationIRI.contains("mBase")){
                    SpecUnit = iri(ModelVariableSpecificationIRI).has(iri(hasUnitOfMeasure), iri(MVA));
                    SpecUnitIsType = iri(MVA).isA(iri(SIDerivedUnit));
                }
                if(ModelVariableSpecificationIRI.contains("Qg")){
                    SpecUnit = iri(ModelVariableSpecificationIRI).has(iri(hasUnitOfMeasure), iri(Mvar));
                    SpecUnitIsType = iri(Mvar).isA(iri(SIDerivedUnit));
                }
                if(ModelVariableSpecificationIRI.contains("Pg")){
                    SpecUnit = iri(ModelVariableSpecificationIRI).has(iri(hasUnitOfMeasure), iri(MW));
                    SpecUnitIsType = iri(MW).isA(iri(SIDerivedUnit));
                }
                if(ModelVariableSpecificationIRI.contains("Pmin")){
                    SpecUnit = iri(ModelVariableSpecificationIRI).has(iri(hasUnitOfMeasure), iri(MW));
                    SpecUnitIsType = iri(MW).isA(iri(SIDerivedUnit));
                }
                if(ModelVariableSpecificationIRI.contains("Pmax")){
                    SpecUnit = iri(ModelVariableSpecificationIRI).has(iri(hasUnitOfMeasure), iri(MW));
                    SpecUnitIsType = iri(MW).isA(iri(SIDerivedUnit));
                }

                InsertDataQuery GeneratorInsertion2 = null;
                if(SpecUnit == null){
                    GeneratorInsertion2 = Queries.INSERT_DATA(ModelHasVariable, VariableisTypeVariable,
                            GeneratorModelVariableHasValueSpec, ModelVariableSpecificationisType, SpecNumericalValue);
                }
                else{
                    GeneratorInsertion2 = Queries.INSERT_DATA(ModelHasVariable, VariableisTypeVariable,
                            GeneratorModelVariableHasValueSpec, ModelVariableSpecificationisType, SpecNumericalValue, SpecUnit, SpecUnitIsType);
                }

                kbClient.executeUpdate(GeneratorInsertion2.getQueryString());
            }
        }
        /**
         * Instantiate RBANCH-related triples
         */
        for (int entry=0; entry < 14; entry++) {
            String branchIRI = PowsysPrefix + "NTU_Branch_" +  String.valueOf(entry);
            String branchModelIRI = PowsysPrefix + "NTU_Branch_" + String.valueOf(entry) + "_Model";
            TriplePattern PSHasBranch = iri(powerSystemIRI).has(iri(hasSubSystem), iri(branchIRI));
            TriplePattern branchisType = iri(branchIRI).isA(iri(branch));
            TriplePattern branchIsModeledByModel = iri(branchIRI).has(iri(isModeledBy), iri(branchModelIRI));
            TriplePattern branchModelisType = iri(branchModelIRI).isA(iri(powerSystemModel));
            InsertDataQuery branchInsertion1 = Queries.INSERT_DATA(PSHasBranch, branchisType, branchIsModeledByModel, branchModelisType);
            kbClient.executeUpdate(branchInsertion1.getQueryString());
            for (Map.Entry<String, String> set : BranchModelVariables.entrySet()) {
                String BranchModelVariable = set.getValue();
                String BranchModelName = set.getKey();
                String BranchModelVariableIRI = PowsysPrefix + "Branch_" + String.valueOf(entry) + "_ModelVariable_" + BranchModelName;
                TriplePattern ModelHasVariable = iri(branchModelIRI).has(iri(hasModelVariable), iri(BranchModelVariableIRI));
                TriplePattern VariableisTypeVariable = iri(BranchModelVariableIRI).isA(iri(BranchModelVariable));
                String ModelVariableSpecificationIRI = PowsysPrefix + "Branch_" + String.valueOf(entry) + "_ModelVariableSpec_" + BranchModelName;
                TriplePattern BranchModelVariableHasValueSpec = iri(BranchModelVariableIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationIRI));
                TriplePattern ModelVariableSpecificationisType = iri(ModelVariableSpecificationIRI).isA(iri(modelVariableSpecification));
                String numValue = branchParams.get(BranchModelName).get(entry);
                TriplePattern SpecNumericalValue = iri(ModelVariableSpecificationIRI).has(iri(numericalValue), numValue);
                TriplePattern SpecUnit = null;
                TriplePattern SpecUnitIsType = null;
                if(ModelVariableSpecificationIRI.contains("AngleMax")){
                    SpecUnit = iri(ModelVariableSpecificationIRI).has(iri(hasUnitOfMeasure), iri(degree));
                    SpecUnitIsType = iri(degree).isA(iri(SIDerivedUnit));
                }
                if(ModelVariableSpecificationIRI.contains("AngleMin")){
                    SpecUnit = iri(ModelVariableSpecificationIRI).has(iri(hasUnitOfMeasure), iri(degree));
                    SpecUnitIsType = iri(degree).isA(iri(SIDerivedUnit));
                }
                if(ModelVariableSpecificationIRI.contains("Angle")){
                    SpecUnit = iri(ModelVariableSpecificationIRI).has(iri(hasUnitOfMeasure), iri(degree));
                    SpecUnitIsType = iri(kV).isA(iri(SIDerivedUnit));
                }
                InsertDataQuery BranchInsertion2 = null;
                if(SpecUnit == null) {
                    BranchInsertion2 = Queries.INSERT_DATA(ModelHasVariable, VariableisTypeVariable, BranchModelVariableHasValueSpec, ModelVariableSpecificationisType, SpecNumericalValue);
                }
                else{
                    BranchInsertion2 = Queries.INSERT_DATA(ModelHasVariable, VariableisTypeVariable, BranchModelVariableHasValueSpec, ModelVariableSpecificationisType, SpecNumericalValue, SpecUnit, SpecUnitIsType);
                }
                kbClient.executeUpdate(BranchInsertion2.getQueryString());
            }
        }
        /**
         * Instantiate BUSNODE-related triples
         */
        for (int entry=1; entry < 16; entry++) {
            String buildingName = getBuildingNameFromBusNum(entry);
            String buildingIRI = PowsysPrefix + "NTU_Building_" + buildingName;
            String busNodeIRI = PowsysPrefix + "NTU_BusNode_" + buildingName + "_" +String.valueOf(entry);
            TriplePattern BuildingHasBusNode = iri(buildingIRI).has(iri(hasBusNode), iri(busNodeIRI));
            TriplePattern BuildingHasPropertyUsage = iri(buildingIRI).has(iri(hasPropertyUsage), iri(University));
            TriplePattern BuildingisType = iri(buildingIRI).isA(iri(building));
            String buildingFullName = BuildingAbbrevToName.get(buildingName);
            TriplePattern BuildingLabel = iri(buildingIRI).has(iri(rdfsLabel), buildingFullName);
            String busNodeModelIRI = PowsysPrefix + "NTU_BusNode_Model_" + buildingName + '_' +String.valueOf(entry) + "_Model";
            TriplePattern PSHasBusNode = iri(powerSystemIRI).has(iri(hasSubSystem), iri(busNodeIRI));
            TriplePattern BusNodeIsType = iri(busNodeIRI).isA(iri(busNode));
            TriplePattern BusNodeIsModeledByModel = iri(busNodeIRI).has(iri(isModeledBy), iri(busNodeModelIRI));
            TriplePattern BusNodeModelisType = iri(busNodeModelIRI).isA(iri(powerSystemModel));
            InsertDataQuery busNodeInsertion1 = Queries.INSERT_DATA(PSHasBusNode, BusNodeIsType, BusNodeIsModeledByModel, BusNodeModelisType, BuildingHasBusNode, BuildingisType, BuildingLabel);
            kbClient.executeUpdate(busNodeInsertion1.getQueryString());
            for (Map.Entry<String, String> set : BusNodeModelVariables.entrySet()) {
                String BusNodeModelVariable = set.getValue();
                String BusNodeModelName = set.getKey();
                String BusNodeModelVariableIRI = PowsysPrefix + "BusNode_" + String.valueOf(entry) + "_ModelVariable_" + BusNodeModelName;
                TriplePattern ModelHasVariable = iri(busNodeModelIRI).has(iri(hasModelVariable), iri(BusNodeModelVariableIRI));
                TriplePattern VariableisTypeVariable = iri(BusNodeModelVariableIRI).isA(iri(BusNodeModelVariable));
                String ModelVariableSpecificationIRI = PowsysPrefix + "BusNode_" + String.valueOf(entry) + "_ModelVariableSpec_" + BusNodeModelName;
                TriplePattern BusNodeModelVariableHasValueSpec = iri(BusNodeModelVariableIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationIRI));
                TriplePattern ModelVariableSpecificationisType = iri(ModelVariableSpecificationIRI).isA(iri(modelVariableSpecification));
                String numValue = busParams.get(BusNodeModelName).get(entry - 1);
                TriplePattern SpecNumericalValue = iri(ModelVariableSpecificationIRI).has(iri(numericalValue), numValue);
                TriplePattern SpecUnit = null;
                TriplePattern SpecUnitIsType = null;
                if(ModelVariableSpecificationIRI.contains("baseKV")){
                    SpecUnit = iri(ModelVariableSpecificationIRI).has(iri(hasUnitOfMeasure), iri(kV));
                    SpecUnitIsType = iri(kV).isA(iri(SIDerivedUnit));
                }
                if(ModelVariableSpecificationIRI.contains("VmMin")){
                    SpecUnit = iri(ModelVariableSpecificationIRI).has(iri(hasUnitOfMeasure), iri(kV));
                    SpecUnitIsType = iri(kV).isA(iri(SIDerivedUnit));
                }
                if(ModelVariableSpecificationIRI.contains("VmMax")){
                    SpecUnit = iri(ModelVariableSpecificationIRI).has(iri(hasUnitOfMeasure), iri(kV));
                    SpecUnitIsType = iri(kV).isA(iri(SIDerivedUnit));
                }
                if(ModelVariableSpecificationIRI.contains("Vm")){
                    SpecUnit = iri(ModelVariableSpecificationIRI).has(iri(hasUnitOfMeasure), iri(kV));
                    SpecUnitIsType = iri(kV).isA(iri(SIDerivedUnit));
                }
                if(ModelVariableSpecificationIRI.contains("Va")){
                    SpecUnit = iri(ModelVariableSpecificationIRI).has(iri(hasUnitOfMeasure), iri(degree));
                    SpecUnitIsType = iri(degree).isA(iri(SIDerivedUnit));
                }

                InsertDataQuery BusNodeInsertion2 = null;
                if(SpecUnit == null) {
                    BusNodeInsertion2 = Queries.INSERT_DATA(ModelHasVariable, VariableisTypeVariable,
                            BusNodeModelVariableHasValueSpec, ModelVariableSpecificationisType, SpecNumericalValue);
                } else {
                    BusNodeInsertion2 = Queries.INSERT_DATA(ModelHasVariable, VariableisTypeVariable,
                            BusNodeModelVariableHasValueSpec, ModelVariableSpecificationisType, SpecNumericalValue, SpecUnit, SpecUnitIsType);
                }
                kbClient.executeUpdate(BusNodeInsertion2.getQueryString());
            }
        }

        /**
         * Instantiate Timeseries-related triples
         */
        for (JSONKeyToIRIMapper mapping : mappings){
            List<String> iris = mapping.getAllIRIs();
            for (String iri:iris){
                String buildingName = getBuildingNameFromIRI(iri);
                int busNum = NTUBuildingToBusNum.get(buildingName);
                String busNodeIRI = PowsysPrefix + "NTU_BusNode_" + buildingName + '_' +String.valueOf(busNum);
                TriplePattern isTypeMeasure = iri(iri).isA(iri(Measure));
                InsertDataQuery insertion = Queries.INSERT_DATA(isTypeMeasure);
                kbClient.executeUpdate(insertion.getQueryString());
                if(iri.contains("KW")){
                    String absorbedActivePowerIRI = PowsysPrefix + "BusNode_" + String.valueOf(busNum) + "_AbsorbedActivePower";
                    TriplePattern BNHasActivePowerAbsorbed = iri(busNodeIRI).has(iri(hasActivePowerAbsorbed), iri(absorbedActivePowerIRI));
                    TriplePattern omHasUnit = iri(iri).has(iri(hasUnit), iri(kilowatt));
                    TriplePattern APHasValue = iri(absorbedActivePowerIRI).has(iri(OMHasValue), iri(iri));
                    TriplePattern typeAP = iri(absorbedActivePowerIRI).isA(iri(absorbedActivePower));
                    InsertDataQuery insert = Queries.INSERT_DATA(omHasUnit, APHasValue, typeAP, BNHasActivePowerAbsorbed);
                    kbClient.executeUpdate(insert.getQueryString());
                }
                else if(iri.contains("KVAR")){
                    String absorbedReactivePowerIRI = PowsysPrefix + "BusNode_" + String.valueOf(busNum) + "_AbsorbedReactivePower";
                    TriplePattern BNHasReactivePowerAbsorbed = iri(busNodeIRI).has(iri(hasReactivePowerAbsorbed), iri(absorbedReactivePowerIRI));
                    TriplePattern omHasUnit = iri(iri).has(iri(hasUnit), iri(kilovoltamperereactive));
                    TriplePattern RAPHasValue = iri(absorbedReactivePowerIRI).has(iri(OMHasValue), iri(iri));
                    TriplePattern typeRAP = iri(absorbedReactivePowerIRI).isA(iri(absorbedReactivePower));
                    InsertDataQuery insert = Queries.INSERT_DATA(omHasUnit, RAPHasValue, typeRAP, BNHasReactivePowerAbsorbed);
                    kbClient.executeUpdate(insert.getQueryString());
                }
                else if(iri.contains("DEGREE")){
                    String voltageAngleIRI = PowsysPrefix + "BusNode_" + String.valueOf(busNum) + "_VoltageAngle";
                    TriplePattern BNHasVoltageAngle = iri(busNodeIRI).has(iri(hasVoltageAngle), iri(voltageAngleIRI));
                    TriplePattern omHasUnit = iri(iri).has(iri(hasUnit), iri(omDegree));
                    TriplePattern VAHasValue = iri(voltageAngleIRI).has(iri(OMHasValue), iri(iri));
                    TriplePattern typeVA = iri(voltageAngleIRI).isA(iri(voltageAngle));
                    InsertDataQuery insert = Queries.INSERT_DATA(omHasUnit, VAHasValue, typeVA, BNHasVoltageAngle);
                    kbClient.executeUpdate(insert.getQueryString());
                }
                else if(iri.contains("KV")){
                    String voltageMagnitudeIRI = PowsysPrefix + "BusNode_" + String.valueOf(busNum) + "_VoltageMagnitude";
                    TriplePattern BNHasVoltageMagnitude = iri(busNodeIRI).has(iri(hasVoltageMagnitude), iri(voltageMagnitudeIRI));
                    TriplePattern omHasUnit = iri(iri).has(iri(hasUnit), iri(kilovolt));
                    TriplePattern VMHasValue = iri(voltageMagnitudeIRI).has(iri(OMHasValue), iri(iri));
                    TriplePattern typeVM = iri(voltageMagnitudeIRI).isA(iri(voltageMagnitude));
                    InsertDataQuery insert = Queries.INSERT_DATA(omHasUnit, VMHasValue, typeVM, BNHasVoltageMagnitude);
                    kbClient.executeUpdate(insert.getQueryString());
                }
            }
        }

        /**
         * Instantiate the building GML Representations
         */
        for (Map.Entry<String, Integer> entry : NTUBuildingToGMLRepresentation.entrySet()) {
            String building_abbrv = entry.getKey();
            Integer GMLRepre = entry.getValue();
            TriplePattern BdHasGML = iri(PowsysPrefix + "NTU_Building_" + building_abbrv).has(iri(hasOntoCityGML),GMLRepre.toString());
            InsertDataQuery insert = Queries.INSERT_DATA(BdHasGML);
            kbClient.executeUpdate(insert.getQueryString());
        }
    }
}

