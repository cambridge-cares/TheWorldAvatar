package uk.ac.cam.cares.jps.dispersion.listener;

import java.io.IOException;

import javax.servlet.ServletContextListener;

import org.apache.jena.ontology.OntModel;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.listener.BaseChimneyOntologyModelManager;

public class LocalOntologyModelManager extends BaseChimneyOntologyModelManager implements ServletContextListener {

    public static final String PATH_KB_SHIPS_TEST = ABSDIR_ROOT_TEST + "/kb/ships/";

    public static final String IRI_KB_SHIPS = IRI_KB + "ships/";
    public static final String IRI_KB_SHIPS_TEST = IRI_KB_TEST + "ships/";

    public static OntModel createChimneyModelForMMSI(String mmsi) throws IOException {
        String shipKbURL;
        if (!AgentLocator.isJPSRunningForTest()) {
            shipKbURL = IRI_KB_SHIPS;
        } else {
            shipKbURL = IRI_KB_SHIPS_TEST;
        }

        String content = getBaseChimneyContent();
        content = content.replaceAll(IRI_KB_BASE + OWL_CHIMNEY, shipKbURL + mmsi + "/" + OWL_CHIMNEY);
        System.out.println("new iri ship= "+shipKbURL + mmsi + "/" + OWL_CHIMNEY);
        return createModelFromString(content);
    }

}
