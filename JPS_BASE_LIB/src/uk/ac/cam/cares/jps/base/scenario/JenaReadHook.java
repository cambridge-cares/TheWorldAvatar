package uk.ac.cam.cares.jps.base.scenario;

import org.apache.jena.ontology.OntDocumentManager;
import org.apache.jena.ontology.OntDocumentManager.ReadHook;
import org.apache.jena.rdf.model.Model;
import org.apache.logging.log4j.ThreadContext;

import uk.ac.cam.cares.jps.base.config.JPSConstants;

/**
 * If Jena loads an OWL file into a Jena model, then - by default - Jena also loads automatically 
 * all OWL files that are specified by their URLs in the import statements of the original OWL file. 
 * When using scenarios, this URL must be resolved and loaded from the scenario agent. When calling the
 * method <code>prepareReadHook</code> once before loading an OWL file into a Jena model, 
 * then all import URLs are redirected according to the given scenario.
 * 
 * @author Andreas
 *
 */
public class JenaReadHook implements ReadHook {
	
	private static ReadHook originalReadHook = null;
	
	public static synchronized void prepareReadHook() {
		
		if (originalReadHook != null) {
			// read hook was already prepared
			return;
		}

		OntDocumentManager manager = OntDocumentManager.getInstance();
		originalReadHook = manager.getReadHook();
		manager.setReadHook(new JenaReadHook());
	}
	
	@Override
	public String beforeRead(Model model, String source, OntDocumentManager odm) {

		String scenarioUrl = ThreadContext.get(JPSConstants.SCENARIO_URL);	
		if (BucketHelper.isBaseScenario(scenarioUrl)) {
			return originalReadHook.beforeRead(model, source, odm);
		}
		
		String redirectedSource = new ScenarioClient().getReadUrl(scenarioUrl, source).toString();
		return originalReadHook.beforeRead(model, redirectedSource, odm);
	}

	@Override
	public void afterRead(Model model, String source, OntDocumentManager odm) {
		originalReadHook.afterRead(model, source, odm);
	}
}
