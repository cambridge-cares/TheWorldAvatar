package uk.ac.cam.cares.jps.powsys.listener;


import org.apache.jena.ontology.OntModel;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.listener.BaseChimneyOntologyModelManager;

import javax.servlet.annotation.WebListener;
import java.io.IOException;

@WebListener
public class LocalOntologyModelManager extends BaseChimneyOntologyModelManager {

    public static OntModel createChimneyModelForName(String name) throws IOException {
        //@todo LKA - implementation
        String plantKbURL = null;

        if (!AgentLocator.isJPSRunningForTest()) {

        } else {

        }

        String content = getBaseChimneyContent();
        content = content.replaceAll(IRI_KB_BASE + OWL_CHIMNEY, plantKbURL + name + "/" + OWL_CHIMNEY);

        return createModelFromString(content);
    }

}

