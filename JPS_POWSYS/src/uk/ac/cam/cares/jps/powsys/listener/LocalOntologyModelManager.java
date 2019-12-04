package uk.ac.cam.cares.jps.powsys.listener;


import java.io.IOException;

import javax.servlet.ServletContextListener;
import javax.servlet.annotation.WebListener;

import org.apache.jena.ontology.OntModel;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.listener.BaseChimneyOntologyModelManager;

@WebListener
public class LocalOntologyModelManager extends BaseChimneyOntologyModelManager implements ServletContextListener {
/*
    private static ConcurrentHashMap<String, String> speciesMap = new ConcurrentHashMap<>();
    private static void setSpecies() {
        speciesMap.put("CO", "ChemSpecies_Carbon__monoxide");
        speciesMap.put("CO2", "ChemSpecies_Carbon__dioxide");
        speciesMap.put("NO2", "ChemSpecies_Nitrogen__dioxide");
        speciesMap.put("HC", "PseudoComponent_Unburned_Hydrocarbon");
        speciesMap.put("NOx", "PseudoComponent_Nitrogen__oxides");
        speciesMap.put("SO2", "ChemSpecies_Sulfur__dioxide");
        speciesMap.put("O3", "ChemSpecies_Ozone");
    }
    public static Map getSpeciesMap() {
        return speciesMap;
    }
*/
	 //design iri=http://www.theworldavatar.com/kb/powerplants/"plantname"/Chimney-001.owl
    public static OntModel createChimneyModelForChimneyIRI(String name) throws IOException {
        //@todo LKA - implementation
        String plantKbURL = null;
        //setSpecies();

        if (!AgentLocator.isJPSRunningForTest()) {
        	plantKbURL=IRI_KB+"powerplants/";
        } else {
        	plantKbURL=IRI_KB_TEST+"powerplants/";

        }

        String content = getBaseChimneyContent();
        content = content.replaceAll(IRI_KB_BASE + OWL_CHIMNEY, name.split("#")[0]);

        return createModelFromString(content);
    }

}

