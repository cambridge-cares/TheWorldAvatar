package uk.ac.cam.cares.jps.base.listener;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.query.*;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.shared.Lock;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import javax.servlet.ServletContextEvent;
import javax.servlet.annotation.WebListener;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

@WebListener
public class BaseChimneyOntologyModelManager extends BaseOntologyModelManager {

    private static final String CHIMNEY = "Chimney-1";
    protected static final String ABSDIR_ROOT = "C://TOMCAT/webapps/ROOT";
    private static final String ABSDIR_KB = ABSDIR_ROOT + "/kb/";
    protected static final String ABSDIR_ROOT_TEST = "/home/arek/IdeaProjects/JParkSimulator-git/JPS_SHIP";
    public static final String ABSDIR_KB_TEST = ABSDIR_ROOT_TEST + "/kb/";


    private static final String PATH_KB_SHIPS = ABSDIR_ROOT + "/kb/ships/";
    public static final String PATH_KB_SHIPS_TEST = ABSDIR_ROOT_TEST + "/kb/ships/";


    private static final String IRI_BASE = "http://www.theworldavatar.com";
    private static final String IRI_KB = IRI_BASE + "/kb/";
    public static final String IRI_KB_SHIPS = IRI_KB + "ships/";
    private static final String IRI_BASE_TEST = "http://localhost:8080/JPS_SHIP";
    private static final String IRI_KB_TEST = IRI_BASE_TEST + "/kb/";
    public static final String IRI_KB_SHIPS_TEST = IRI_KB_TEST + "ships/";
    public static final String OWL_CHIMNEY = CHIMNEY + ".owl";
    protected static final String OWL_CHIMNEY_TEMP = CHIMNEY + "-temp.owl";
    public static final String PATH_BASE_CHIMNEY = PATH_KB_SHIPS + OWL_CHIMNEY_TEMP;
    public static final String PATH_BASE_CHIMNEY_TEST = PATH_KB_SHIPS_TEST + OWL_CHIMNEY_TEMP;

    private static final String IRI_ONTOLOGY = "http://www.theworldavatar.com/ontology/";
    private static final String IRI_ONTOCAPE = IRI_ONTOLOGY + "ontocape/";
    private static final String OWL_ONTOCAPE_UPPER = IRI_ONTOCAPE + "upper_level/";
    private static final String OWL_ONTOCAPE_UPPER_SYSTEM = OWL_ONTOCAPE_UPPER + "system.owl";
    private static final String OWL_ONTOCAPE_CPS_BEHAVIOR = IRI_ONTOCAPE + "chemical_process_system/CPS_behavior/behavior.owl";
    private static final String OWL_ONTOCAPE_SUP_CONCEPTS_SI = IRI_ONTOCAPE + "supporting_concepts/SI_unit/";
    private static final String OWL_ONTOCAPE_SUP_CONCEPTS_GEOM = IRI_ONTOCAPE + "supporting_concepts/geometry/geometry.owl";
    private static final String OWL_ONTOCAPE_MAT_PS = IRI_ONTOCAPE + "material/phase_system/phase_system.owl";
    public static final String CPT_NUMVAL = "#numericalValue";

    public static final String SPQ_WASTE = "PREFIX j4:<" + IRI_ONTOCAPE + "chemical_process_system/CPS_function/process.owl#> " +
            "PREFIX j7:<" + IRI_ONTOLOGY + "meta_model/topology/topology.owl#> " +
            "SELECT ?wasteStream \r\n" +
            "WHERE " +
            "{ ?chimney j7:hasOutput ?wasteStream ." +
            "  ?wasteStream a j4:NonReusableWasteProduct ." +
            "}";
    private static final String EX_SAVE_OWL =  "Saving OWL failed: ";

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

    private static OntModel baseChimneyModel;
    private static ConcurrentHashMap<String, String> speciesMap = new ConcurrentHashMap<>();
    private static Logger logger = LoggerFactory.getLogger(BaseChimneyOntologyModelManager.class);

    @Override
    public void contextInitialized(ServletContextEvent sce) {
        logger.info("initializing the local ontology manager");
        try {
            baseChimneyModel = createBaseChimneyModel();
        } catch (IOException e) {
            logger.error("initializing the local ontology manager failed");
            throw new JPSRuntimeException("Could not create base model for chimney: " + PATH_BASE_CHIMNEY);
        } finally {
            setConcepts();
            setSpecies();
        }
    }

    @Override
    public void contextDestroyed(ServletContextEvent sce) {
        baseChimneyModel.close();
    }

    private OntModel createBaseChimneyModel() throws IOException {
        String content = getBaseChimneyContent();
        return createModelFromString(content);
    }

    private void setConcepts() {
        try {
            conceptMap.put(CPT_NUMVAL, baseChimneyModel.getDatatypeProperty(OWL_ONTOCAPE_UPPER_SYSTEM + CPT_NUMVAL));
            conceptMap.put(CPT_HASPROP, baseChimneyModel.getObjectProperty(OWL_ONTOCAPE_UPPER_SYSTEM + CPT_HASPROP));
            conceptMap.put(CPT_HASVAL, baseChimneyModel.getObjectProperty(OWL_ONTOCAPE_UPPER_SYSTEM + CPT_HASVAL));
            conceptMap.put(CPT_HASUOM, baseChimneyModel.getObjectProperty(OWL_ONTOCAPE_UPPER_SYSTEM + CPT_HASUOM));
            conceptMap.put(CPT_SCLVAL, baseChimneyModel.getOntClass(OWL_ONTOCAPE_UPPER_SYSTEM + CPT_SCLVAL));
            conceptMap.put(CPT_CONTAINS, baseChimneyModel.getObjectProperty(OWL_ONTOCAPE_UPPER_SYSTEM + CPT_CONTAINS));
            conceptMap.put(CPT_PMAMT, baseChimneyModel.getOntClass(OWL_ONTOCAPE_CPS_BEHAVIOR + CPT_PMAMT));
            conceptMap.put(CPT_CONVMFLR, baseChimneyModel.getOntClass(OWL_ONTOCAPE_CPS_BEHAVIOR + CPT_CONVMFLR));
            conceptMap.put(CPT_SINGPART, baseChimneyModel.getOntClass(OWL_ONTOCAPE_CPS_BEHAVIOR + CPT_SINGPART));
            conceptMap.put(CPT_HASREPRPART, baseChimneyModel.getObjectProperty(OWL_ONTOCAPE_CPS_BEHAVIOR + CPT_HASREPRPART));
            conceptMap.put(CPT_SI_M, baseChimneyModel.getIndividual(OWL_ONTOCAPE_SUP_CONCEPTS_SI + CPT_SI_M));
            conceptMap.put(CPT_SI_GPS, baseChimneyModel.getIndividual(OWL_ONTOCAPE_SUP_CONCEPTS_SI + CPT_SI_GPS));
            conceptMap.put(CPT_SI_KGPCM, baseChimneyModel.getIndividual(OWL_ONTOCAPE_SUP_CONCEPTS_SI + CPT_SI_KGPCM));
            conceptMap.put(CPT_DIAM, baseChimneyModel.getOntClass(OWL_ONTOCAPE_SUP_CONCEPTS_GEOM + CPT_DIAM));
            conceptMap.put(CPT_HASLEN, baseChimneyModel.getObjectProperty(OWL_ONTOCAPE_SUP_CONCEPTS_GEOM + CPT_HASLEN));
            conceptMap.put(CPT_DENS, baseChimneyModel.getOntClass(OWL_ONTOCAPE_MAT_PS + CPT_DENS));
            conceptMap.put(CPT_MASSFR, baseChimneyModel.getOntClass(OWL_ONTOCAPE_MAT_PS + CPT_MASSFR));
            conceptMap.put(CPT_HASDENS, baseChimneyModel.getObjectProperty(OWL_ONTOCAPE_MAT_PS + CPT_HASDENS));
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

    public static void save(OntModel jenaOwlModel, String iriOfChimney, String mmsi) {
        ReadWriteLock readWriteLock = new ReentrantReadWriteLock();
        readWriteLock.writeLock().lock();
        try {
            saveToOwl(jenaOwlModel, iriOfChimney);
        } catch (IOException e) {
            throw new JPSRuntimeException(EX_SAVE_OWL + iriOfChimney);
        } finally {
            readWriteLock.writeLock().unlock();
        }
    }

    public static void saveToOwl(OntModel jenaOwlModel, String iriOfChimney) throws IOException {
        String filePath2;
        if (!AgentLocator.isJPSRunningForTest()) {
            filePath2= iriOfChimney.replaceAll(IRI_KB, ABSDIR_KB).split("#")[0];
        } else {
            filePath2= iriOfChimney.replaceAll(IRI_KB_TEST, ABSDIR_KB_TEST).split("#")[0];
        }
        logger.info("the filepath created= "+filePath2);

        try {
            prepareDirectory(filePath2);
        } catch (IOException e) {
            throw new JPSRuntimeException(EX_SAVE_OWL + filePath2);
        } finally {
            FileOutputStream out = new FileOutputStream(filePath2);

            jenaOwlModel.write(out, "RDF/XML-ABBREV");
            out.close();
        }

    }

    public static void prepareDirectory(String filePath2) throws IOException {
        File stockDir = new File(filePath2).getParentFile();

        boolean stockdir = true;

        if (!stockDir.exists()) {
            stockdir = stockDir.mkdirs();
        }
        if (stockdir) {
            File[] listOfFiles = stockDir.listFiles();
            if (listOfFiles != null) {
                for (File listOfFile : listOfFiles) {
                    listOfFile.delete();
                    //@todo AC: work on general concurrent filesystem access solution for JPS
                	/*
                    if (!listOfFile.delete()) {
                        throw new IOException("Could not clean up: " + filePath2);
                    }*/
                }
            }
        } else {
            throw new IOException("No such directory: " + filePath2);
        }
    }


    public static ResultSet getWasteStreamInfo(OntModel jenaOwlModel) {
        return query(SPQ_WASTE, jenaOwlModel);
    }

    public static ResultSet query(String sparql, OntModel model) {
        Query query = QueryFactory.create(sparql);
        ResultSet rs;
        model.enterCriticalSection(Lock.READ);
        try {
            QueryExecution queryExec = QueryExecutionFactory.create(query, model);
            rs = queryExec.execSelect();
        } finally {
            model.leaveCriticalSection() ;
        }

        return ResultSetFactory.copyResults(rs);
    }
}
