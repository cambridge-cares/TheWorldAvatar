package uk.ac.cam.cares.jps.ship.listener;

import org.apache.jena.ontology.*;
import org.apache.jena.query.*;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.ship.model.ShipEntity;

import javax.persistence.EntityManager;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.annotation.WebListener;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@WebListener
public class LocalOntologyModelManager implements ServletContextListener {

    private static final String CHIMNEY = "Chimney-1";
    //private static final String ABSDIR_ROOT = "/home/arek/IdeaProjects/JParkSimulator-git/JPS_SHIP";
    private static final String ABSDIR_ROOT = "C://TOMCAT/webapps/ROOT";
    private static final String PATH_KB_SHIPS = ABSDIR_ROOT + "/kb/ships/";
    private static final String IRI_KB = "http://www.theworldavatar.com/kb/";
    private static final String IRI_KB_SHIPS = IRI_KB + "ships/";
    private static final String IRI_AGENT_SHIP = "/JPS_SHIP";
    private static final String OWL_CHIMNEY = CHIMNEY + ".owl";
    private static final String OWL_CHIMNEY_TEMP = CHIMNEY + "-temp.owl";
    private static final String PATH_BASE_CHIMNEY = PATH_KB_SHIPS + OWL_CHIMNEY_TEMP;
    private static final String IRI_ONTOLOGY = "http://www.theworldavatar.com/ontology/";
    private static final String IRI_ONTOCAPE = IRI_ONTOLOGY + "ontocape/";
    private static final String OWL_ONTOCAPE_UPPER = IRI_ONTOCAPE + "upper_level/";
    private static final String OWL_ONTOCAPE_UPPER_SYSTEM = OWL_ONTOCAPE_UPPER + "system.owl";
    private static final String OWL_ONTOCAPE_CPS_BEHAVIOR = IRI_ONTOCAPE + "chemical_process_system/CPS_behavior/behavior.owl";
    private static final String OWL_ONTOCAPE_SUP_CONCEPTS_SI = IRI_ONTOCAPE + "supporting_concepts/SI_unit/";
    private static final String OWL_ONTOCAPE_SUP_CONCEPTS_GEOM = IRI_ONTOCAPE + "supporting_concepts/geometry/geometry.owl";
    private static final String OWL_ONTOCAPE_MAT_PS = IRI_ONTOCAPE + "material/phase_system/phase_system.owl";
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
    private static final String SPQ_WASTE = "PREFIX j4:<" + IRI_ONTOCAPE + "chemical_process_system/CPS_function/process.owl#> " +
            "PREFIX j7:<" + IRI_ONTOLOGY + "meta_model/topology/topology.owl#> " +
            "SELECT ?wasteStream \r\n" +
            "WHERE " +
            "{ ?chimney j7:hasOutput ?wasteStream ." +
            "  ?wasteStream a j4:NonReusableWasteProduct ." +
            "}";

    private static OntModel jenaOwlModel;
    private static OntModel baseChimneyModel;
    private static ConcurrentHashMap<String, Resource> conceptMap = new ConcurrentHashMap<>();
    private static ConcurrentHashMap<String, String> speciesMap = new ConcurrentHashMap<>();

    @Override
    public void contextInitialized(ServletContextEvent sce) {
        jenaOwlModel = ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
        try {
            baseChimneyModel = createBaseChimneyModel();
            setConcepts();
            setSpecies();
        } catch (IOException e) {
            throw new JPSRuntimeException("Could not create base model for chimney.");
        }
    }

    @Override
    public void contextDestroyed(ServletContextEvent sce) {
        jenaOwlModel.close();
    }

    public static OntModel createChimneyModelForMMSI(String mmsi) throws IOException {
        String baseURL = KeyValueManager.get(IKeys.URL_SCHEME) + KeyValueManager.get(IKeys.HOST)
                + ":" + KeyValueManager.get(IKeys.PORT) + IRI_AGENT_SHIP;
        String shipKbURL = baseURL + KeyValueManager.get(IKeys.PATH_KNOWLEDGEBASE_SHIPS);
        String content = getBaseChimneyContent();
        content = content.replaceAll(IRI_KB_SHIPS + OWL_CHIMNEY, shipKbURL + mmsi + "/" + OWL_CHIMNEY);

        return createModelFromString(content);
    }

    private static OntModel createModelFromString(String content) {
        byte[] contentBytes = content.getBytes(StandardCharsets.UTF_8);
        OntModel model = jenaOwlModel;
        model.read(new ByteArrayInputStream(contentBytes), null);
        return model;
    }

    private OntModel createBaseChimneyModel() throws IOException {
        String content = getBaseChimneyContent();
        return createModelFromString(content);
    }

    private static String getBaseChimneyContent() throws IOException {
        File source = new File(PATH_BASE_CHIMNEY);
        return new String(Files.readAllBytes(source.toPath()), StandardCharsets.UTF_8);
    }

    private void setConcepts() {
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

    public static Resource getConcept(String name) {
        return conceptMap.get(name);
    }

    public static Map getSpeciesMap() {
        return speciesMap;
    }

    public static void save(OntModel jenaOwlModel, String iriOfChimney, String mmsi) throws IOException {

        String filePath2= iriOfChimney.replaceAll(IRI_KB, KeyValueManager.get(IKeys.ABSDIR_ROOT) + "/kb").split("#")[0]; //update the file locally

        File stockDir = new File(filePath2).getParentFile();

        boolean stockdir = true;

        if (!stockDir.exists()) {
            stockdir = stockDir.mkdirs();
        }
        if (stockdir) {
            File[] listOfFiles = stockDir.listFiles();
            if (listOfFiles != null) {
                for (File listOfFile : listOfFiles) {
                    if (!listOfFile.delete()) {
                        throw new IOException("Could not clean up: " + filePath2);
                    }
                }
            }
        } else {
            throw new IOException("No such directory: " + filePath2);
        }


        FileOutputStream out = new FileOutputStream(filePath2);

        jenaOwlModel.write(out, "RDF/XML-ABBREV");
        jenaOwlModel.close();
        out.close();

        linkChimneyToShip(iriOfChimney, mmsi);
    }

    /**
     * Creates chimney iri record in PostgreSQL with ship mmsi as foreign key.
     * @see uk.ac.cam.cares.jps.ship.model.ShipDetailsEntity
     * @param iriOfChimney Chimney IRI of ship identified by mmsi
     * @param mmsi Maritime Mobile Service Identity (ID of ship)
     */
    private static void linkChimneyToShip(String iriOfChimney, String mmsi) {
        EntityManager em = LocalEntityManagerFactory.createEntityManager();
        ShipEntity ship = em.find(ShipEntity.class, Integer.decode(mmsi));
        ship.getShipPollutionByMmsi().setChimneyIri(iriOfChimney);
        em.getTransaction().begin();
        em.persist(ship);
        em.getTransaction().commit();
    }

    public static ResultSet getWasteStreamInfo(OntModel jenaOwlModel) {
        return query(SPQ_WASTE, jenaOwlModel);
    }

    public static ResultSet query(String sparql, OntModel model) {
        Query query = QueryFactory.create(sparql);
        QueryExecution queryExec = QueryExecutionFactory.create(query, model);
        ResultSet rs = queryExec.execSelect();

        return ResultSetFactory.copyResults(rs);
    }
}
