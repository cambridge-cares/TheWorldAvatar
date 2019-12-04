package uk.ac.cam.cares.jps.base.listener;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.servlet.ServletContextEvent;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;


public class BaseChimneyOntologyModelManager extends BaseOntologyModelManager {

    private static final String CHIMNEY = "Chimney-1";
    public static final String OWL_CHIMNEY = CHIMNEY + ".owl";
    protected static final String OWL_CHIMNEY_TEMP = CHIMNEY + "-temp.owl";
    public static final String CPT_NUMVAL = "#numericalValue";
    public static final String CPT_HASPROP = "#hasProperty";
    public static final String CPT_HASVAL = "#hasValue";
    public static final String CPT_HASUOM = "#hasUnitOfMeasure";
    public static final String CPT_SCLVAL = "#ScalarValue";
    public static final String CPT_CONTAINS =  "#contains";
    public static final String CPT_PMAMT = "#ParticulateMaterialAmount";
    public static final String CPT_CONVMFLR = "#ConvectiveMassFlowrate";
    public static final String CPT_SINGPART = "#SingleParticle";
    public static final String CPT_HASREPRPART = "#hasRepresentativeParticle";
    public static final String CPT_SI_M = "SI_unit.owl#m";
    public static final String CPT_SI_GPS = "derived_SI_units.owl#g_per_s";
    public static final String CPT_SI_KGPCM = "derived_SI_units.owl#kg_per_cubic_m";
    public static final String CPT_DIAM = "#Diameter";
    public static final String CPT_HASLEN = "#has_length";
    public static final String CPT_DENS = "#Density";
    public static final String CPT_MASSFR = "#MassFraction";
    public static final String CPT_HASDENS = "#has_density";
    private static final String IRI_ONTOLOGY = "http://www.theworldavatar.com/ontology/";
    private static final String IRI_ONTOCAPE = IRI_ONTOLOGY + "ontocape/";
    private static final String OWL_ONTOCAPE_UPPER = IRI_ONTOCAPE + "upper_level/";
    private static final String OWL_ONTOCAPE_UPPER_SYSTEM = OWL_ONTOCAPE_UPPER + "system.owl";
    private static final String OWL_ONTOCAPE_CPS_BEHAVIOR = IRI_ONTOCAPE + "chemical_process_system/CPS_behavior/behavior.owl";
    private static final String OWL_ONTOCAPE_SUP_CONCEPTS_SI = IRI_ONTOCAPE + "supporting_concepts/SI_unit/";
    private static final String OWL_ONTOCAPE_SUP_CONCEPTS_GEOM = IRI_ONTOCAPE + "supporting_concepts/geometry/geometry.owl";
    private static final String OWL_ONTOCAPE_MAT_PS = IRI_ONTOCAPE + "material/phase_system/phase_system.owl";
    public static final String SPQ_WASTE = "PREFIX j4:<" + IRI_ONTOCAPE + "chemical_process_system/CPS_function/process.owl#> " +
            "PREFIX j7:<" + IRI_ONTOLOGY + "meta_model/topology/topology.owl#> " +
            "SELECT ?wasteStream \r\n" +
            "WHERE " +
            "{ ?chimney j7:hasOutput ?wasteStream ." +
            "  ?wasteStream a j4:NonReusableWasteProduct ." +
            "}";

    private static final String PATH_KB_BASE = ABSDIR_ROOT + "/kb/base/";
    public static final String PATH_KB_BASE_TEST = ABSDIR_ROOT_TEST + "/kb/base/";
    public static final String PATH_BASE_CHIMNEY = PATH_KB_BASE + OWL_CHIMNEY_TEMP;
    public static final String PATH_BASE_CHIMNEY_TEST = PATH_KB_BASE_TEST + OWL_CHIMNEY_TEMP;
    public static final String IRI_KB_BASE = IRI_KB + "base/";

    private static ConcurrentHashMap<String, String> speciesMap = new ConcurrentHashMap<>();
    private static Logger logger = LoggerFactory.getLogger(BaseChimneyOntologyModelManager.class);

    public void contextInitialized(ServletContextEvent sce) {
        logger.info("initializing the local ontology manager");
        try {
            baseEntityModel = createBaseChimneyModel();
            logger.info("createbase chimney is successful");
        } catch (IOException e) {
            logger.error("initializing the local ontology manager failed");
            throw new JPSRuntimeException("Could not create base model for chimney: " + PATH_BASE_CHIMNEY);
        } finally {
            setConcepts();
            setSpecies();
            logger.info("setspecies is finished" + "species map size= "+ String.valueOf(speciesMap.size()));
        }
    }

    private OntModel createBaseChimneyModel() throws IOException {
        String content = getBaseChimneyContent();
        return createModelFromString(content);
    }

    private void setConcepts() {
        try {
            conceptMap.put(CPT_NUMVAL, baseEntityModel.getDatatypeProperty(OWL_ONTOCAPE_UPPER_SYSTEM + CPT_NUMVAL));
            conceptMap.put(CPT_HASPROP, baseEntityModel.getObjectProperty(OWL_ONTOCAPE_UPPER_SYSTEM + CPT_HASPROP));
            conceptMap.put(CPT_HASVAL, baseEntityModel.getObjectProperty(OWL_ONTOCAPE_UPPER_SYSTEM + CPT_HASVAL));
            conceptMap.put(CPT_HASUOM, baseEntityModel.getObjectProperty(OWL_ONTOCAPE_UPPER_SYSTEM + CPT_HASUOM));
            conceptMap.put(CPT_SCLVAL, baseEntityModel.getOntClass(OWL_ONTOCAPE_UPPER_SYSTEM + CPT_SCLVAL));
            conceptMap.put(CPT_CONTAINS, baseEntityModel.getObjectProperty(OWL_ONTOCAPE_UPPER_SYSTEM + CPT_CONTAINS));
            conceptMap.put(CPT_PMAMT, baseEntityModel.getOntClass(OWL_ONTOCAPE_CPS_BEHAVIOR + CPT_PMAMT));
            conceptMap.put(CPT_CONVMFLR, baseEntityModel.getOntClass(OWL_ONTOCAPE_CPS_BEHAVIOR + CPT_CONVMFLR));
            conceptMap.put(CPT_SINGPART, baseEntityModel.getOntClass(OWL_ONTOCAPE_CPS_BEHAVIOR + CPT_SINGPART));
            conceptMap.put(CPT_HASREPRPART, baseEntityModel.getObjectProperty(OWL_ONTOCAPE_CPS_BEHAVIOR + CPT_HASREPRPART));
            conceptMap.put(CPT_SI_M, baseEntityModel.getIndividual(OWL_ONTOCAPE_SUP_CONCEPTS_SI + CPT_SI_M));
            conceptMap.put(CPT_SI_GPS, baseEntityModel.getIndividual(OWL_ONTOCAPE_SUP_CONCEPTS_SI + CPT_SI_GPS));
            conceptMap.put(CPT_SI_KGPCM, baseEntityModel.getIndividual(OWL_ONTOCAPE_SUP_CONCEPTS_SI + CPT_SI_KGPCM));
            conceptMap.put(CPT_DIAM, baseEntityModel.getOntClass(OWL_ONTOCAPE_SUP_CONCEPTS_GEOM + CPT_DIAM));
            conceptMap.put(CPT_HASLEN, baseEntityModel.getObjectProperty(OWL_ONTOCAPE_SUP_CONCEPTS_GEOM + CPT_HASLEN));
            conceptMap.put(CPT_DENS, baseEntityModel.getOntClass(OWL_ONTOCAPE_MAT_PS + CPT_DENS));
            conceptMap.put(CPT_MASSFR, baseEntityModel.getOntClass(OWL_ONTOCAPE_MAT_PS + CPT_MASSFR));
            conceptMap.put(CPT_HASDENS, baseEntityModel.getObjectProperty(OWL_ONTOCAPE_MAT_PS + CPT_HASDENS));
        } catch (Exception e) {
            throw new JPSRuntimeException("Could not load required " + IRI_ONTOCAPE + " concepts using " + PATH_BASE_CHIMNEY + " model.");
        }
    }

    private void setSpecies() {
        speciesMap.put("CO", "ChemSpecies_Carbon__monoxide");
        speciesMap.put("CO2", "ChemSpecies_Carbon__dioxide");
        speciesMap.put("NO2", "ChemSpecies_Nitrogen__dioxide");
        speciesMap.put("HC", "PseudoComponent_Unburned_Hydrocarbon");
        speciesMap.put("NOx", "PseudoComponent_Nitrogen__oxides");
        speciesMap.put("SO2", "ChemSpecies_Sulfur__dioxide");
        speciesMap.put("O3", "ChemSpecies_Ozone");
    }

    public static String getBaseChimneyContent() throws IOException {
        File source;
        if (!AgentLocator.isJPSRunningForTest()) {
            source = new File(PATH_BASE_CHIMNEY);
        } else {
            source = new File(PATH_BASE_CHIMNEY_TEST);
        }
        return new String(Files.readAllBytes(source.toPath()), StandardCharsets.UTF_8);
    }

    public static Map getSpeciesMap() {
        return speciesMap;
    }

    public static ResultSet getWasteStreamInfo(OntModel jenaOwlModel) {
        return query(SPQ_WASTE, jenaOwlModel);
    }
}
