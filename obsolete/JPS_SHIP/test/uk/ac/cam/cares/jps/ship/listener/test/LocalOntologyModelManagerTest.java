package uk.ac.cam.cares.jps.ship.listener.test;

import junit.framework.TestCase;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.ship.ServletTestHelper;
import uk.ac.cam.cares.jps.ship.listener.LocalOntologyModelManager;

import javax.servlet.ServletConfig;
import javax.servlet.ServletContextEvent;
import javax.servlet.annotation.WebListener;
import java.io.File;
import java.util.Map;

public class LocalOntologyModelManagerTest extends TestCase {

    public void testNewLocalOntologyModelManager() {
        LocalOntologyModelManager lomm = null;
        try {
            lomm = new LocalOntologyModelManager();
        } catch (Exception e) {
            throw new JPSRuntimeException(e);
        } finally {
            assertNotNull(lomm);
            assertEquals(lomm.getClass(), new LocalOntologyModelManager().getClass());
            assertEquals(1, lomm.getClass().getAnnotations().length);
            assertTrue(lomm.getClass().isAnnotationPresent(WebListener.class));
        }
    }

    public void testLocalOntologyModelManagerFields() {
        LocalOntologyModelManager lomm = new LocalOntologyModelManager();
        assertEquals(3, lomm.getClass().getDeclaredFields().length);
    }

    public void testLocalOntologyModelManagerMethods() {
        assertTrue(true);
        LocalOntologyModelManager lomm = new LocalOntologyModelManager();
        ServletConfig ctx = ServletTestHelper.getServletConfig();
        ServletContextEvent event = new ServletContextEvent(ctx.getServletContext());
        //Test Initialisation: baseChimney, species & concepts loaded
        try {
            lomm.contextInitialized(event);
        } catch (Exception e) {
            throw new JPSRuntimeException(e);
        } finally {
            Map species = lomm.getSpeciesMap();
            assertFalse(species.isEmpty());
            assertEquals(7, lomm.getSpeciesMap().size());
            assertTrue(lomm.getSpeciesMap().containsKey("CO"));
            assertTrue(lomm.getSpeciesMap().containsKey("CO2"));
            assertTrue(lomm.getSpeciesMap().containsKey("NO2"));
            assertTrue(lomm.getSpeciesMap().containsKey("HC"));
            assertTrue(lomm.getSpeciesMap().containsKey("NOx"));
            assertTrue(lomm.getSpeciesMap().containsKey("SO2"));
            assertTrue(lomm.getSpeciesMap().containsKey("O3"));
            assertEquals("ChemSpecies_Carbon__monoxide", lomm.getSpeciesMap().get("CO"));
            assertEquals("ChemSpecies_Carbon__dioxide", lomm.getSpeciesMap().get("CO2"));
            assertEquals("ChemSpecies_Nitrogen__dioxide", lomm.getSpeciesMap().get("NO2"));
            assertEquals("PseudoComponent_Unburned_Hydrocarbon", lomm.getSpeciesMap().get("HC"));
            assertEquals("PseudoComponent_Nitrogen__oxides", lomm.getSpeciesMap().get("NOx"));
            assertEquals("ChemSpecies_Sulfur__dioxide", lomm.getSpeciesMap().get("SO2"));
            assertEquals("ChemSpecies_Ozone", lomm.getSpeciesMap().get("O3"));
            assertEquals("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue",
                    lomm.getConcept(lomm.CPT_NUMVAL).toString());
            assertEquals("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasProperty",
                    lomm.getConcept(lomm.CPT_HASPROP).toString());
            assertEquals("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue",
                    lomm.getConcept(lomm.CPT_HASVAL).toString());
            assertEquals("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure",
                    lomm.getConcept(lomm.CPT_HASUOM).toString());
            assertEquals("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue",
                    lomm.getConcept(lomm.CPT_SCLVAL).toString());
            assertEquals("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#contains",
                    lomm.getConcept(lomm.CPT_CONTAINS).toString());
            assertEquals("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#ParticulateMaterialAmount",
                    lomm.getConcept(lomm.CPT_PMAMT).toString());
            assertEquals("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#ConvectiveMassFlowrate",
                    lomm.getConcept(lomm.CPT_CONVMFLR).toString());
            assertEquals("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#SingleParticle",
                    lomm.getConcept(lomm.CPT_SINGPART).toString());
            assertEquals("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#hasRepresentativeParticle",
                    lomm.getConcept(lomm.CPT_HASREPRPART).toString());
            assertEquals("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#m",
                    lomm.getConcept(lomm.CPT_SI_M).toString());
            assertEquals("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#g_per_s",
                    lomm.getConcept(lomm.CPT_SI_GPS).toString());
            assertEquals("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#kg_per_cubic_m",
                    lomm.getConcept(lomm.CPT_SI_KGPCM).toString());
            assertEquals("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#Diameter",
                    lomm.getConcept(lomm.CPT_DIAM).toString());
            assertEquals("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#has_length",
                    lomm.getConcept(lomm.CPT_HASLEN).toString());
            assertEquals("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#Density",
                    lomm.getConcept(lomm.CPT_DENS).toString());
            assertEquals("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#MassFraction",
                    lomm.getConcept(lomm.CPT_MASSFR).toString());
            assertEquals("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#has_density",
                    lomm.getConcept(lomm.CPT_HASDENS).toString());

        }
        //Test creating chinmeys
        OntModel chimney = null;
        try {
            chimney = lomm.createChimneyModelForMMSI("1234567890");
        } catch (Exception e) {
            throw new JPSRuntimeException(e);
        } finally {
            assertNotNull(chimney);
            assertEquals("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue",
                    chimney.getBaseModel().getGraph().find().next().getPredicate().toString());
        }
        //Test SPARQL "wasteStream"
        ResultSet waste = null;
        try {
            waste = lomm.getWasteStreamInfo(chimney);
        } catch (Exception e) {
            throw new JPSRuntimeException(e);
        } finally {
            assertNotNull(waste);
            assertEquals(1, waste.getResultVars().size());
            assertEquals("wasteStream", waste.getResultVars().get(0));
            assertEquals(waste.getResultVars(), lomm.query(lomm.SPQ_WASTE, chimney).getResultVars());
        }
        //Test prepareDirectory
        String path = "./tmp/file.tst";
        try {
            lomm.prepareDirectory(path);
        } catch (Exception e) {
            throw new JPSRuntimeException(e);
        } finally {
            File dir = new File(path).getParentFile();
            assertTrue(dir.exists());
            dir.delete();
        }
        //Test saveToOwl
        String iri = lomm.IRI_KB_SHIPS_TEST + path;
        File file = new File(lomm.PATH_KB_SHIPS_TEST + path);
        try {
            assertFalse(file.exists());
            assertNotNull(chimney);
            lomm.saveToOwl(chimney, iri, "123");
            assertTrue(file.exists());
        } catch (Exception e) {
            throw new JPSRuntimeException(e);
        } finally {
            file.delete();
        }
    }

}
