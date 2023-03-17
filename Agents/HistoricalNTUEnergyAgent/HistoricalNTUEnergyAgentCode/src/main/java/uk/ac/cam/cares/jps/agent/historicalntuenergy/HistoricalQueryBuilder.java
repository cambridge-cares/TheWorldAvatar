package uk.ac.cam.cares.jps.agent.historicalntuenergy;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.util.JSONKeyToIRIMapper;

import java.io.File;
import java.io.FileInputStream;
import java.util.ArrayList;
import java.util.Iterator;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import java.io.FileNotFoundException;
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
    public static final String OmPrefix = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    public static final String generatedIRIPrefix = OmPrefix + "measure";
    public String queryEndpoint;
    public String updateEndpoint;

    /**
     * Classes
     */
    public static final String powerSystem = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#PowerSystem";
    public static final String powerSystemModel = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#PowerSystemModel";
    public static final String busNode = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#BusNode";
    public static final String absorbedActivePower = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#AbsorbedActivePower";
    public static final String absorbedReactivePower = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#AbsorbedReactivePower";
    public static final String BusNumber = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#BusNumber";
    public static final String BusType = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#BusType";
    public static final String Area = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Area";
    public static final String Bs = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Bs";
    public static final String Gs = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Gs";
    public static final String BaseKV = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#baseKV";
    public static final String VmMin = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#VmMin";
    public static final String VmMax = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#VmMax";
    public static final String Zone = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Zone";
    public static final String Vm = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Vm";
    public static final String Va = "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Va";
    public static final String modelVariableSpecification = "http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#ModelVariableSpecification";
    public static final String Measure = "http://www.ontology-of-units-of-measure.org/resource/om-2/Measure";



    /**
     * Relationships
     */
    private static final String OMHasValue = OmPrefix + "hasValue";
    private static final String OntoCapeHasValue = "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue";
    private static final String hasActivePowerAbsorbed = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasActivePowerAbsorbed";
    private static final String hasReactivePowerAbsorbed = "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasReactivePowerAbsorbed";

    private static final String hasUnit = OmPrefix + "hasUnit";
    private static final String hasSubSystem = "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasSubsystem";
    private static final String isModeledBy = "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isModeledBy";
    private static final String hasModelVariable = "http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable";
    private static final String numericalValue = "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue";
    private static final String hasUnitOfMeasure = "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure";

    /**
     * Individuals
     */
    private static final String kilowatt = OmPrefix + "kilowatt";
    private static final String kilovoltamperereactive = OmPrefix + "kilovoltamperereactive";
    private static final String OntoCapekV = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#kV";
    private static final String OntoCapeDegree = "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#degree";


    /**
     * NTU building to Bus number mapping
     */
    HashMap<String, Integer> NTUBuildingToBusNum = new HashMap<>();


    RemoteStoreClient kbClient;
    public String agentProperties;
    public String clientProperties;
    public JSONObject readings;

    private List<JSONKeyToIRIMapper> mappings;

    public HistoricalQueryBuilder(String agentProp, String clientProp) throws IOException
    {
        agentProperties = agentProp;
        clientProperties = clientProp;

        loadconfigs(clientProperties);
        //readings endpoints from client.properties

        loadproperties(agentProperties);

        NTUBuildingToBusNum.put("None", 1);
        NTUBuildingToBusNum.put("N1_3", 2);
        NTUBuildingToBusNum.put("NYA", 3);
        NTUBuildingToBusNum.put("CANTEEN_2", 4);
        NTUBuildingToBusNum.put("None", 5);
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

        kbClient = new RemoteStoreClient();
        kbClient.setUpdateEndpoint(updateEndpoint);
        kbClient.setQueryEndpoint(queryEndpoint);
    }

    public void loadconfigs(String filepath) throws IOException
    {
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

    /*
    JSONKeyToIRIMapper IRIGenerator = new JSONKeyToIRIMapper();
    String generatedIRI = IRIGenerator.generateIRI(generatedIRIPrefix, "123");
    */

    public String getBuildingNameFromIRI(String IRI){
        int firstUnderscoreIndex = IRI.indexOf('_');
        if (firstUnderscoreIndex != -1) {
            int endIndex = IRI.indexOf("_Q_KVAR", firstUnderscoreIndex + 1);
            if (endIndex == -1) {
                endIndex = IRI.indexOf("_P_KW", firstUnderscoreIndex + 1);
            }
            if (endIndex != -1) {
                return IRI.substring(firstUnderscoreIndex + 1, endIndex);
            }
        }
        return null;
    }

    public void instantiateTriples(){
        InsertDataQuery insertion;

        for (JSONKeyToIRIMapper mapping : mappings){
            List<String> iris = mapping.getAllIRIs();
            for (String iri:iris){
                String buildingName = getBuildingNameFromIRI(iri);

                //IRI isType OM:measure
                TriplePattern isTypeMeasure = iri(iri).isA(iri(Measure));
                insertion = Queries.INSERT_DATA(isTypeMeasure);
                kbClient.executeUpdate(insertion.getQueryString());

                if(iri.contains("KW")){

                    //IRI hasUnit OM:kilowatt
                    TriplePattern hasUnitKW = iri(iri).has(iri(hasUnit), iri(kilowatt));

                    //activePowerIRI hasValue IRI
                    String absorbedActivePowerIRI = PowsysPrefix +buildingName + "_AbsorbedActivePower";
                    TriplePattern APHasValue = iri(absorbedActivePowerIRI).has(iri(OMHasValue), iri(iri));

                    //activePowerIRI isType AbsorbedActivePower
                    TriplePattern typeAP = iri(absorbedActivePowerIRI).isA(iri(absorbedActivePower));

                    //busNodeIRI hasActivePowerAbsorbed activePowerIRI
                    String busNodeIRI = PowsysPrefix + buildingName + "_BusNode";
                    TriplePattern BNHasActivePowerAbsorbed = iri(busNodeIRI).has(iri(hasActivePowerAbsorbed), iri(absorbedActivePowerIRI));

                    //busNodeIRI isType BusNode
                    TriplePattern typeBN = iri(busNodeIRI).isA(iri(busNode));

                    //BusNodeIRI isModeledBy PowerSystemModelIRI
                    String PowerSystemModelIRI = PowsysPrefix + buildingName + "_PowerSystemModel";
                    TriplePattern BNHasPowerSystemModel = iri(busNodeIRI).has(iri(isModeledBy), iri(PowerSystemModelIRI));

                    //PowerSystemModelIRI isType PowerSystemModell
                    TriplePattern typePSM = iri(PowerSystemModelIRI).isA(iri(powerSystemModel));

                    //PowerSystemModelIRI hasModelVariable BusNumebrIRI ---
                    String BusNumberIRI = PowsysPrefix + buildingName + "_BusNumber";
                    TriplePattern PSMHasModelVariableBN = iri(PowerSystemModelIRI).has(iri(hasModelVariable), iri(BusNumberIRI));

                    //BusNumebr hasValue ModelVariableSpecificationBNIRI
                    String ModelVariableSpecificationBNIRI = PowsysPrefix + buildingName + "_ModelVariable_BusNumber";
                    TriplePattern BNHasModelVariableSpecBN = iri(BusNumberIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationBNIRI));

                    //ModelVariableSpecificationBNIRI isType ModelVariableSpecification
                    TriplePattern typeModelVariableSpecBN = iri(ModelVariableSpecificationBNIRI).isA(iri(modelVariableSpecification));

                    //ModelVariableSpecificationBNIRI numericalValue (Int)
                    LOGGER.info(buildingName);
                    int busNumber = NTUBuildingToBusNum.get(buildingName);
                    LOGGER.info(busNumber);
                    TriplePattern BNNumerical = iri(ModelVariableSpecificationBNIRI).has(iri(numericalValue), busNumber);

                    //PowerSystemModelIRI hasModelVariable Area ---
                    String AreaIRI = PowsysPrefix + buildingName + "_Area";
                    TriplePattern PSMHasModelVariableA = iri(PowerSystemModelIRI).has(iri(hasModelVariable), iri(AreaIRI));

                    //Area hasValue ModelVariableSpecificationAIRI
                    String ModelVariableSpecificationAIRI = PowsysPrefix + buildingName + "_ModelVariable_Area";
                    TriplePattern AHasModelVariableSpecA = iri(AreaIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationAIRI));

                    //ModelVariableSpecificationAIRI isType ModelVariableSpecification
                    TriplePattern typeModelVariableSpecA = iri(ModelVariableSpecificationAIRI).isA(iri(modelVariableSpecification));

                    //ModelVariableSpecificationAIRI numericalValue (Int)
                    TriplePattern ANumerical = iri(ModelVariableSpecificationAIRI).has(iri(numericalValue), 1);

                    //PowerSystemModelIRI hasModelVariable Bs ---
                    String BsIRI = PowsysPrefix + buildingName + "_Bs";
                    TriplePattern PSMHasModelVariableB = iri(PowerSystemModelIRI).has(iri(hasModelVariable), iri(BsIRI));

                    //Bs hasValue ModelVariableSpecificationBIRI
                    String ModelVariableSpecificationBIRI = PowsysPrefix + buildingName + "_ModelVariable_Bs";
                    TriplePattern BHasModelVariableSpecB = iri(BsIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationBIRI));

                    //ModelVariableSpecificationBIRI isType ModelVariableSpecification
                    TriplePattern typeModelVariableSpecB = iri(ModelVariableSpecificationBIRI).isA(iri(modelVariableSpecification));

                    //ModelVariableSpecificationBIRI numericalValue (Int)
                    TriplePattern BNumerical = iri(ModelVariableSpecificationBIRI).has(iri(numericalValue), 0);

                    //PowerSystemModelIRI hasModelVariable Gs ---
                    String GsIRI = PowsysPrefix + buildingName + "_Gs";
                    TriplePattern PSMHasModelVariableG = iri(PowerSystemModelIRI).has(iri(hasModelVariable), iri(GsIRI));


                    //Gs hasValue ModelVariableSpecificationGIRI
                    String ModelVariableSpecificationGIRI = PowsysPrefix + buildingName + "_ModelVariable_Gs";
                    TriplePattern GHasModelVariableSpecG = iri(GsIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationGIRI));

                    //ModelVariableSpecificationGIRI isType ModelVariableSpecification
                    TriplePattern typeModelVariableSpecG = iri(ModelVariableSpecificationGIRI).isA(iri(modelVariableSpecification));

                    //ModelVariableSpecificationGIRI numericalValue (Int)
                    TriplePattern GNumerical = iri(ModelVariableSpecificationGIRI).has(iri(numericalValue), 0);

                    //PowerSystemModelIRI hasModelVariable baseKV ---
                    String baseKVIRI = PowsysPrefix + buildingName + "_baseKV";
                    TriplePattern PSMHasModelVariablebKV = iri(PowerSystemModelIRI).has(iri(hasModelVariable), iri(baseKVIRI));

                    //baseKV hasValue ModelVariableSpecificationbKVIRI
                    String ModelVariableSpecificationbKVIRI = PowsysPrefix + buildingName + "_ModelVariable_baseKV";
                    TriplePattern bKVHasModelVariableSpecbKV = iri(baseKVIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationbKVIRI));

                    //ModelVariableSpecificationbKVIRI isType ModelVariableSpecification
                    TriplePattern typeModelVariableSpecbKV = iri(ModelVariableSpecificationbKVIRI).isA(iri(modelVariableSpecification));

                    //ModelVariableSpecificationbKVIRI numericalValue (Int)
                    TriplePattern bKVNumerical = iri(ModelVariableSpecificationbKVIRI).has(iri(numericalValue), 11);

                    // ModelVariableSpecificationbKVIRI hasUnitOfMeasure OntoCapeKV
                    TriplePattern bKVMeasure = iri(ModelVariableSpecificationbKVIRI).has(iri(hasUnitOfMeasure), iri(OntoCapekV));

                    //PowerSystemModelIRI hasModelVariable VmMin ---
                    String VmMinIRI = PowsysPrefix + buildingName +  "_VmMin";
                    TriplePattern PSMHasModelVariableVmMin = iri(PowerSystemModelIRI).has(iri(hasModelVariable), iri(VmMinIRI));

                    //VmMin hasValue ModelVariableSpecificationVmMinIRI
                    String ModelVariableSpecificationVmMinIRI = PowsysPrefix + buildingName + "_ModelVariable_VmMin";
                    TriplePattern VmMinHasModelVariableSpecVmMin = iri(VmMinIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationVmMinIRI));

                    //ModelVariableSpecificationVmMinIRI isType ModelVariableSpecification
                    TriplePattern typeModelVariableSpecVmMin = iri(ModelVariableSpecificationVmMinIRI).isA(iri(modelVariableSpecification));

                    //ModelVariableSpecificationVmMinIRI numericalValue (Double)
                    TriplePattern VmMinNumerical = iri(ModelVariableSpecificationVmMinIRI).has(iri(numericalValue), 0.9);

                    // ModelVariableSpecificationVmMinIRI hasUnitOfMeasure OntoCapeKV
                    TriplePattern VmMinMeasure = iri(ModelVariableSpecificationVmMinIRI).has(iri(hasUnitOfMeasure), iri(OntoCapekV));

                    //PowerSystemModelIRI hasModelVariable VmMax ---
                    String VmMaxIRI = PowsysPrefix + buildingName + "_VmMax";
                    TriplePattern PSMHasModelVariableVmMax = iri(PowerSystemModelIRI).has(iri(hasModelVariable), iri(VmMaxIRI));

                    //VmMax hasValue ModelVariableSpecificationVmMaxIRI
                    String ModelVariableSpecificationVmMaxIRI = PowsysPrefix + buildingName + "_ModelVariable_VmMax";
                    TriplePattern VmMaxHasModelVariableSpecVmMax = iri(VmMaxIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationVmMaxIRI));

                    //ModelVariableSpecificationVmMaxIRI isType ModelVariableSpecification
                    TriplePattern typeModelVariableSpecVmMax = iri(ModelVariableSpecificationVmMaxIRI).isA(iri(modelVariableSpecification));

                    //ModelVariableSpecificationVmMaxIRI numericalValue (Double)
                    TriplePattern VmMaxNumerical = iri(ModelVariableSpecificationVmMaxIRI).has(iri(numericalValue), 1.1);

                    // ModelVariableSpecificationVmMaxIRI hasUnitOfMeasure OntoCapeKV
                    TriplePattern VmMaxMeasure = iri(ModelVariableSpecificationVmMaxIRI).has(iri(hasUnitOfMeasure), iri(OntoCapekV));

                    //PowerSystemModelIRI hasModelVariable Zone ---
                    String ZoneIRI = PowsysPrefix + buildingName + "_Zone";
                    TriplePattern PSMHasModelVariableZone = iri(PowerSystemModelIRI).has(iri(hasModelVariable), iri(ZoneIRI));

                    //Zone hasValue ModelVariableSpecificationZoneIRI
                    String ModelVariableSpecificationZoneIRI = PowsysPrefix + buildingName + "_ModelVariable_Zone";
                    TriplePattern ZoneHasModelVariableSpecZone = iri(ZoneIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationZoneIRI));

                    //ModelVariableSpecificationZoneIRI isType ModelVariableSpecification
                    TriplePattern typeModelVariableSpecZone = iri(ModelVariableSpecificationZoneIRI).isA(iri(modelVariableSpecification));

                    //ModelVariableSpecificationZoneIRI numericalValue (Int)
                    TriplePattern ZoneNumerical = iri(ModelVariableSpecificationZoneIRI).has(iri(numericalValue), 1);

                    //PowerSystemModelIRI hasModelVariable Vm ---
                    String VmIRI = PowsysPrefix + buildingName + "_Vm";
                    TriplePattern PSMHasModelVariableVm = iri(PowerSystemModelIRI).has(iri(hasModelVariable), iri(VmIRI));

                    //Vm hasValue ModelVariableSpecificationVmIRI
                    String ModelVariableSpecificationVmIRI = PowsysPrefix + buildingName + "_ModelVariable_Vm";
                    TriplePattern VmHasModelVariableSpecVm = iri(VmIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationVmIRI));

                    //ModelVariableSpecificationVmIRI isType ModelVariableSpecification
                    TriplePattern typeModelVariableSpecVm = iri(ModelVariableSpecificationVmIRI).isA(iri(modelVariableSpecification));

                    //ModelVariableSpecificationVmIRI numericalValue (Double)
                    TriplePattern VmNumerical = iri(ModelVariableSpecificationVmIRI).has(iri(numericalValue), 1);

                    // ModelVariableSpecificationVmIRI hasUnitOfMeasure OntoCapeKV
                    TriplePattern VmMeasure = iri(ModelVariableSpecificationVmIRI).has(iri(hasUnitOfMeasure), iri(OntoCapekV));

                    // PowerSystemModelIRI hasModelVariable Va ---
                    String VaIRI = PowsysPrefix + buildingName + "_Va";
                    TriplePattern PSMHasModelVariableVa = iri(PowerSystemModelIRI).has(iri(hasModelVariable), iri(VaIRI));

                    // Va hasValue ModelVariableSpecificationVaIRI
                    String ModelVariableSpecificationVaIRI = PowsysPrefix + buildingName + "_ModelVariable_Va";
                    TriplePattern VaHasModelVariableSpecVa = iri(VaIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationVaIRI));

                    // ModelVariableSpecificationVaIRI isType ModelVariableSpecification
                    TriplePattern typeModelVariableSpecVa = iri(ModelVariableSpecificationVaIRI).isA(iri(modelVariableSpecification));

                    // ModelVariableSpecificationVaIRI numericalValue (Double)
                    TriplePattern VaNumerical = iri(ModelVariableSpecificationVaIRI).has(iri(numericalValue), 0);

                    // ModelVariableSpecificationVaIRI hasUnitOfMeasure OntoCapeKV
                    TriplePattern VaMeasure = iri(ModelVariableSpecificationVaIRI).has(iri(hasUnitOfMeasure), iri(OntoCapeDegree));

                    // PowerSystemModelIRI hasModelVariable BusType ---
                    String busTypeIRI = PowsysPrefix + buildingName + "_BusType";
                    TriplePattern PSMHasModelVariableBusType = iri(PowerSystemModelIRI).has(iri(hasModelVariable), iri(busTypeIRI));

                    // BusType hasValue ModelVariableSpecificationBusTypeIRI
                    String ModelVariableSpecificationBusTypeIRI = PowsysPrefix + buildingName + "_ModelVariable_BusType";
                    TriplePattern busTypeHasModelVariableSpecBusType = iri(busTypeIRI).has(iri(OntoCapeHasValue), iri(ModelVariableSpecificationBusTypeIRI));

                    // ModelVariableSpecificationBusTypeIRI isType ModelVariableSpecification
                    TriplePattern typeModelVariableSpecBusType = iri(ModelVariableSpecificationBusTypeIRI).isA(iri(modelVariableSpecification));

                    // ModelVariableSpecificationBusTypeIRI nominalValue (String)
                    TriplePattern busTypeNominal = iri(ModelVariableSpecificationBusTypeIRI).has(iri(numericalValue), 1);

                    //PowerSystemIRI hasSubSystem busNodeIRI
                    String powerSystemIRI = PowsysPrefix + "PowerSystem_" + buildingName;
                    TriplePattern PSHasSubsystem = iri(powerSystemIRI).has(iri(hasSubSystem), iri(busNodeIRI));

                    //PowerSystemIRI isType PowerSystem
                    TriplePattern typePS = iri(powerSystemIRI).isA(iri(powerSystem));


                    //Assign rdf:type to all modelvariables
                    TriplePattern typeBusNumber = iri(BusNumberIRI).isA(iri(BusNumber));
                    TriplePattern typeArea = iri(AreaIRI).isA(iri(Area));
                    TriplePattern typeBs = iri(BsIRI).isA(iri(Bs));
                    TriplePattern typeGs = iri(GsIRI).isA(iri(Gs));
                    TriplePattern typeBaseKV = iri(baseKVIRI).isA(iri(BaseKV));
                    TriplePattern typeVmMin = iri(VmMinIRI).isA(iri(VmMin));
                    TriplePattern typeVmMax = iri(VmMaxIRI).isA(iri(VmMax));
                    TriplePattern typeZone = iri(ZoneIRI).isA(iri(Zone));
                    TriplePattern typeVm = iri(VmIRI).isA(iri(Vm));
                    TriplePattern typeVa = iri(VaIRI).isA(iri(Va));
                    TriplePattern typeBt = iri(busTypeIRI).isA(iri(BusType));


                    insertion = Queries.INSERT_DATA(hasUnitKW, APHasValue, typeAP,
                            BNHasActivePowerAbsorbed, typeBN, PSHasSubsystem, typePS, BNHasPowerSystemModel, typePSM,
                            PSMHasModelVariableB, BNumerical, typeModelVariableSpecB, BHasModelVariableSpecB,
                            PSMHasModelVariableG, GHasModelVariableSpecG,typeModelVariableSpecG, GNumerical,  typeModelVariableSpecG,
                            PSMHasModelVariableA, typeModelVariableSpecA, AHasModelVariableSpecA, ANumerical,
                            PSMHasModelVariablebKV,bKVHasModelVariableSpecbKV, typeModelVariableSpecbKV, bKVNumerical, bKVMeasure,
                            PSMHasModelVariableVmMin, VmMinHasModelVariableSpecVmMin, typeModelVariableSpecVmMin, VmMinNumerical, VmMinMeasure,
                            PSMHasModelVariableVmMax, VmMaxHasModelVariableSpecVmMax, VmMaxNumerical, typeModelVariableSpecVmMax, VmMaxMeasure,
                            PSMHasModelVariableZone, ZoneHasModelVariableSpecZone, typeModelVariableSpecZone, ZoneNumerical,
                            PSMHasModelVariableVm, VmHasModelVariableSpecVm, typeModelVariableSpecVm, VmNumerical, VmMeasure,
                            PSMHasModelVariableVa, VaHasModelVariableSpecVa, typeModelVariableSpecVa, VaNumerical,VaMeasure,
                            PSMHasModelVariableBusType, busTypeHasModelVariableSpecBusType, typeModelVariableSpecBusType, busTypeNominal,
                            PSMHasModelVariableBN, BNHasModelVariableSpecBN, typeModelVariableSpecBN, BNNumerical,
                            typeBusNumber, typeArea, typeBs, typeGs, typeBaseKV, typeVmMin, typeVmMax, typeZone, typeVm, typeVa, typeBt
                            );
                    kbClient.executeUpdate(insertion.getQueryString());

                }
                else if(iri.contains("KVAR")){


                    TriplePattern omHasUnit = iri(iri).has(iri(hasUnit), iri(kilovoltamperereactive));
                    //ReactivePowerIRI hasValue IRI
                    String absorbedReactivePowerIRI = PowsysPrefix + buildingName + "_AbsorbedReactivePower";
                    TriplePattern RAPHasValue = iri(absorbedReactivePowerIRI).has(iri(OMHasValue), iri(iri));

                    //activePowerIRI isType AbsorbedReactivePower
                    TriplePattern typeRAP = iri(absorbedReactivePowerIRI).isA(iri(absorbedReactivePower));

                    //busNodeIRI hasReactivePowerAbsorbed activePowerIRI
                    String busNodeIRI = PowsysPrefix + buildingName + "_BusNode";
                    TriplePattern BNHasReactivePowerAbsorbed = iri(busNodeIRI).has(iri(hasReactivePowerAbsorbed), iri(absorbedReactivePowerIRI));

                    InsertDataQuery insert = Queries.INSERT_DATA(omHasUnit, RAPHasValue, typeRAP, BNHasReactivePowerAbsorbed);

                    kbClient.executeUpdate(insert.getQueryString());

                }
            }
        }

    }



}
