package uk.ac.cam.cares.jps.semakaupv.test;

import org.apache.jena.ontology.OntModel;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.semakaupv.SemakauPV;

public class TestSemakauPV extends TestCase {
	String ENIRI="http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/SemakauElectricalNetwork.owl#SemakauElectricalNetwork";
	String irradSensorIRI="http://www.theworldavatar.com/kb/sgp/singapore/SGSolarIrradiationSensor-001.owl#SGSolarIrradiationSensor-001";
	public void testrunMods() {
		SemakauPV a= new SemakauPV();
		OntModel model = new SemakauPV().readModelGreedy(ENIRI);
		a.runMODS(model,irradSensorIRI);
		
		System.out.println("it's done");
	}

}
