package uk.ac.cam.cares.jps.powsys.coordination;

import org.apache.jena.ontology.OntModel;

public class GeneratorInfo {

	OntModel model;
	String generatorIri;
	String plantIri;
	double x;
	double y;
	String busNumberIri;
	double distanceToSlackBus;
	double distanceToClosestBus;
	String closestBusNumber;
}
