package uk.ac.cam.cares.jps.ship.listener;

import org.apache.jena.ontology.OntModel;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.listener.BaseChimneyOntologyModelManager;

import javax.servlet.annotation.WebListener;
import java.io.IOException;

@WebListener
public class LocalOntologyModelManager extends BaseChimneyOntologyModelManager {

    public static OntModel createChimneyModelForMMSI(String mmsi) throws IOException {
        String shipKbURL;
        if (!AgentLocator.isJPSRunningForTest()) {
            shipKbURL = IRI_KB_SHIPS;
        } else {
            shipKbURL = IRI_KB_SHIPS_TEST;
        }

        String content = getBaseChimneyContent();
        content = content.replaceAll(IRI_KB_SHIPS + OWL_CHIMNEY, shipKbURL + mmsi + "/" + OWL_CHIMNEY);

        return createModelFromString(content);
    }

}
