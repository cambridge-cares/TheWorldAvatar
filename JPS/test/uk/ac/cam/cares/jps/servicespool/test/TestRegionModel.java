package uk.ac.cam.cares.jps.servicespool.test;

import java.io.StringWriter;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RDFFormat;
import org.apache.jena.vocabulary.RDF;
import org.junit.Test;

import uk.ac.cam.cares.jps.building.CRSTransformer;

public class TestRegionModel {

	@Test
	 public void test() {
		Model regionModel = ModelFactory.createDefaultModel();
		Property hasX = regionModel.createProperty("http://test.com/Property/hasX");
		Property hasY = regionModel.createProperty("http://test.com/Property/hasY");
		Property hasUpperPoint = regionModel.createProperty("http://test.com/Property/upperPoint");
		Property hasLowerPoint = regionModel.createProperty("http://test.com/Property/lowerPoint");
		Resource Region = regionModel.createResource("http://test.com/aRegionInstance");
		Region.addProperty(RDF.type, "http://test.com/ontology/Region");
		Resource UpperPoint = regionModel.createResource("http://test.com/upperPoint");
		UpperPoint.addProperty(RDF.type, "http://test.com/ontology/Point");
		UpperPoint.addLiteral(hasX, 80000);
		UpperPoint.addLiteral(hasY, 455190);
		Resource LowerPoint = regionModel.createResource("http://test.com/lowerPoint");
		LowerPoint.addProperty(RDF.type, "http://test.com/ontology/Point");
		LowerPoint.addLiteral(hasX, 79480);
		LowerPoint.addLiteral(hasY, 454670);
		Region.addProperty(hasLowerPoint, LowerPoint);
		Region.addProperty(hasUpperPoint, UpperPoint);
		Property hasReferenceSystem = regionModel.createProperty("http://test.com/Property/referenceSystem");
		Region.addLiteral(hasReferenceSystem, CRSTransformer.EPSG_25833);

		StringWriter out = new StringWriter();
		RDFDataMgr.write(out, regionModel, RDFFormat.RDFJSON);

		System.out.println(out.toString());
		
		
	}

}
