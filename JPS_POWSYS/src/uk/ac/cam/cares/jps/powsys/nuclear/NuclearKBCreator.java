package uk.ac.cam.cares.jps.powsys.nuclear;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URISyntaxException;

import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.ObjectProperty;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.ModelFactory;

public class NuclearKBCreator {
	public static String baseURL2 = "D:\\KBDev-git/irp3-JPS-KBDev-git/Server Ontology Configuration Root/kb/powerplants/";
	public static String baseURL = "D:\\JPS/JParkSimulator-git/JPS_POWSYS/testres/";
	String plantname=null;
	
	static Individual nuclear;

	static Individual m;
	static Individual degree;
	static Individual MW;
	static Individual length;
	static Individual xaxis;
	static Individual yaxis;
		
	private OntClass powerplantclass = null;
	private OntClass organizationclass = null;
	private OntClass coordinateclass = null;
	private OntClass coordinatesystemclass = null;
	private OntClass valueclass = null;
	private OntClass scalarvalueclass = null;

	private OntClass designcapacityclass = null;


	private ObjectProperty hasdimension = null;
	private ObjectProperty referto = null;
	private ObjectProperty hascoordinatesystem = null;
	private ObjectProperty hasx = null;
	private ObjectProperty hasy = null;
	private ObjectProperty hasvalue = null;
	private ObjectProperty hasunit = null;
	private ObjectProperty hasaddress = null;
	private ObjectProperty isownedby = null;
	private ObjectProperty designcapacity = null;
	private ObjectProperty hasyearofbuilt = null;
	private ObjectProperty realizes = null;
	
	private ObjectProperty consumesprimaryfuel = null;
	private ObjectProperty hasemission = null;
	private ObjectProperty hascosts = null;
	private ObjectProperty hasannualgeneration = null;
	private ObjectProperty usesgenerationtechnology = null;

	private DatatypeProperty numval = null;
	private DatatypeProperty hasname = null;
	
	public void initOWLClasses(OntModel jenaOwlModel) {
		powerplantclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#PowerPlant");
		coordinateclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#StraightCoordinate");
		coordinatesystemclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#ProjectedCoordinateSystem");
		valueclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#CoordinateValue");
		scalarvalueclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue");
		designcapacityclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#DesignCapacity");
		
		consumesprimaryfuel = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#consumesPrimaryFuel");
		hasdimension = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasDimension");
		referto = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#refersToAxis");
		hascoordinatesystem = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasGISCoordinateSystem");
		hasx = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_x");
		hasy = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_y");
		hasvalue = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");
		hasunit = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");
		designcapacity = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#designCapacity");
		
		numval = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
		nuclear = jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Nuclear");
		MW=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#MW");
		degree=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#degree");
		length=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/physical_dimension/physical_dimension.owl#length");
		xaxis=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#x-axis");
		yaxis=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#y-axis");
	}
	
	public void startConversion(String csvfile) throws URISyntaxException {
		  String line = "";
	        String cvsSplitBy = ",";
		try (BufferedReader br = new BufferedReader(new FileReader(csvfile))) {

			while ((line = br.readLine()) != null) {
				String[] data = line.split(cvsSplitBy);

				// System.out.println("Country [code= " + country[4] + " , name=" + country[5] +
				// "]");

				String filePath = baseURL + "planttemplatekb2.owl"; // the empty owl file

				// System.out.println("data8= "+data[8]);

				String filePath2 = baseURL2 + data[8] + ".owl"; // the result of written owl file

				// System.out.println(filePath2);
				FileInputStream inFile = new FileInputStream(filePath);
				Reader in = new InputStreamReader(inFile, "UTF-8");

				OntModel jenaOwlModel = ModelFactory.createOntologyModel();
				jenaOwlModel.read(in, null);

				initOWLClasses(jenaOwlModel);

				doConversion(jenaOwlModel, data[8], data[0], data[11], data[2], data[3], data[10], data[9], "0", "0",
						data[6], data[1], data[4]); // plant,country,owner,fuel,tech,x,y,emission,cost,anngen,capa,age

				/** save the updated model file */
				LandlotsKB ins = new LandlotsKB();
				ins.savefile(jenaOwlModel, filePath2);
			}

		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public void doConversion(OntModel jenaOwlModel, String plantname ,String countryname,String ownername,String fueltype,String tech,String xnumval,String ynumval,String emissionnumval,String costnumval,String anngennumval,String capanumval,String agenumval){
		Individual plant = powerplantclass.createIndividual("http://www.theworldavatar.com/kb/powerplants/" + plantname + ".owl#"+plantname);
		
		Individual plantcoordinate = coordinatesystemclass.createIndividual("http://www.theworldavatar.com/kb/powerplants/" + plantname + ".owl#CoordinateSystem_of_"+plantname);
		Individual xcoordinate = coordinateclass.createIndividual("http://www.theworldavatar.com/kb/powerplants/" + plantname + ".owl#x_coordinate_of_"+plantname);
		Individual ycoordinate = coordinateclass.createIndividual("http://www.theworldavatar.com/kb/powerplants/" + plantname + ".owl#y_coordinate_of_"+plantname);
		Individual xcoordinatevalue = valueclass.createIndividual("http://www.theworldavatar.com/kb/powerplants/" + plantname + ".owl#v_x_coordinate_of_"+plantname);
		Individual ycoordinatevalue = valueclass.createIndividual("http://www.theworldavatar.com/kb/powerplants/" + plantname + ".owl#v_y_coordinate_of_"+plantname);
		
		Individual capa = designcapacityclass.createIndividual("http://www.theworldavatar.com/kb/powerplants/" + plantname + ".owl#capa_of_"+plantname);
		Individual capavalue = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/powerplants/" + plantname + ".owl#v_capa_of_"+plantname);
		
		Individual powergeneration = jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NuclearGeneration");
		
		plant.addProperty(hascoordinatesystem, plantcoordinate);
		
		plantcoordinate.addProperty(hasx, xcoordinate);
		xcoordinate.addProperty(hasvalue, xcoordinatevalue);
		xcoordinate.addProperty(referto, xaxis);
		xcoordinate.addProperty(hasdimension, length);
		xcoordinatevalue.addProperty(numval, jenaOwlModel.createTypedLiteral(xnumval));
		xcoordinatevalue.addProperty(hasunit, degree);
		
		plantcoordinate.addProperty(hasy, ycoordinate);
		ycoordinate.addProperty(hasvalue, ycoordinatevalue);
		ycoordinate.addProperty(referto, yaxis);
		ycoordinate.addProperty(hasdimension, length);
		ycoordinatevalue.addProperty(numval, jenaOwlModel.createTypedLiteral(ynumval));
		ycoordinatevalue.addProperty(hasunit, degree);
		
		plant.setPropertyValue(designcapacity, capa);
		capa.setPropertyValue(hasvalue, capavalue);
		capavalue.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(capanumval));
		capavalue.setPropertyValue(hasunit, MW);
		
		plant.setPropertyValue(realizes, powergeneration);
		powergeneration.setPropertyValue(consumesprimaryfuel, nuclear);
		
	}

}
