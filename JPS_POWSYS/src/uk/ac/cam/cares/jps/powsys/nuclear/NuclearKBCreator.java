package uk.ac.cam.cares.jps.powsys.nuclear;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URISyntaxException;
import java.util.ArrayList;

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
		
	private OntClass nuclearpowerplantclass = null;
	private OntClass organizationclass = null;
	private OntClass coordinateclass = null;
	private OntClass coordinatesystemclass = null;
	private OntClass valueclass = null;
	private OntClass scalarvalueclass = null;

	private OntClass designcapacityclass = null;
	private OntClass generatedactivepowerclass=null;
	private OntClass nucleargeneratorclass = null;


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
	
	private ObjectProperty hasSubsystem = null;
	private ObjectProperty hasActivepowergenerated=null;

	private DatatypeProperty numval = null;
	private DatatypeProperty hasname = null;
	private BufferedReader br;
	
	public void initOWLClasses(OntModel jenaOwlModel) {
		nuclearpowerplantclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#NuclearPlant");
		nucleargeneratorclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#NuclearGenerator");
		coordinateclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#StraightCoordinate");
		coordinatesystemclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#ProjectedCoordinateSystem");
		valueclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#CoordinateValue");
		scalarvalueclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue");
		designcapacityclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#DesignCapacity");
		generatedactivepowerclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#GeneratedActivePower");
		
		consumesprimaryfuel = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#consumesPrimaryFuel");
		hasdimension = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasDimension");
		referto = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#refersToAxis");
		hascoordinatesystem = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasGISCoordinateSystem");
		hasx = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_x");
		hasy = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_y");
		hasvalue = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");
		hasunit = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");
		designcapacity = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#designCapacity");
		hasActivepowergenerated=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasActivePowerGenerated");
		
		numval = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
		nuclear = jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Nuclear");
		MW=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#MW");
		degree=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#degree");
		length=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/physical_dimension/physical_dimension.owl#length");
		xaxis=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#x-axis");
		yaxis=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#y-axis");
	}
	
	public void startConversion(String csvfileoutput, String csvfileinput) throws URISyntaxException {
		  String line = "";
	        String cvsSplitBy = ",";
	        
	        //reading from output file and put that to owl file
		try (BufferedReader br = new BufferedReader(new FileReader(csvfileoutput))) {

			while ((line = br.readLine()) != null) {
				String[] data = line.split(cvsSplitBy);

				// System.out.println("Country [code= " + country[4] + " , name=" + country[5] +
				// "]");

				String filePath = baseURL + "plantgeneratortemplate.owl"; // the empty owl file

				// System.out.println("data8= "+data[8]);

				String filePath2 = baseURL2 + data[8] + ".owl"; // the result of written owl file

				// System.out.println(filePath2);
				FileInputStream inFile = new FileInputStream(filePath);
				Reader in = new InputStreamReader(inFile, "UTF-8");

				OntModel jenaOwlModel = ModelFactory.createOntologyModel();
				jenaOwlModel.read(in, null);

				initOWLClasses(jenaOwlModel);

				doConversion(jenaOwlModel, data[0], data[1], data[2],data[3],data[4]); // plant,fuel,x,y,capa

				/** save the updated model file */
				LandlotsKB ins = new LandlotsKB();
				ins.savefile(jenaOwlModel, filePath2);
			}

		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	
	public void gatherInformation(String csvfileoutput, String csvfileinputlandlot, String inputparameter) throws NumberFormatException, IOException {
		String line = "";
        String cvsSplitBy = ",";
        int linereader=0;
        ArrayList<LandlotObject> lotlisted= new ArrayList<LandlotObject>();
        br = new BufferedReader(new FileReader(csvfileinputlandlot));
			while ((line = br.readLine()) != null) {
				if(linereader==0) {
	        		//System.out.println("skipped because it's header");
	        	}
				else {
				String[] data = line.split(cvsSplitBy);
				System.out.println("data8= "+data[8]);
				LandlotObject lots= new LandlotObject(data[0]);
				lots.setx(Double.valueOf(data[2]));
				lots.sety(Double.valueOf(data[1]));
				lotlisted.add(lots);
				}
				linereader++;
				
			}
			System.out.println("lots info are captured");
			
//			ArrayList<NuclearPlantType> nuclearlisted= new ArrayList<NuclearPlantType>();
//			linereader=0;
//	        br = new BufferedReader(new FileReader(inputparameter));
//				while ((line = br.readLine()) != null) {
//					if(linereader==0) {
//		        		//System.out.println("skipped because it's header");
//		        	}
//					else {
//					String[] data = line.split(cvsSplitBy);
//					System.out.println("data8= "+data[8]);
//					NuclearPlantType nuclear= new NuclearPlantType(data[0]);
//					nuclear.setcapacitya(Double.valueOf(data[2]));
//					nuclear.setcapacityb(Double.valueOf(data[1]));
//					nuclearlisted.add(nuclear);
//					}
//					linereader++;
//					
//				}
			
			
			

        
	}
	
	public void doConversion(OntModel jenaOwlModel, String plantname ,String fueltype,String xnumval,String ynumval,String capanumval){
		String nuclearplantgeneratoriri="http://www.theworldavatar.com/kb/sgp/jurongisland/nuclearpowerplants/";
		String generatorname=""; //temporary
		
		Individual plant = nuclearpowerplantclass.createIndividual(nuclearplantgeneratoriri + plantname + ".owl#"+plantname);
		
		Individual plantcoordinate = coordinatesystemclass.createIndividual(nuclearplantgeneratoriri + plantname + ".owl#CoordinateSystem_of_"+plantname);
		Individual xcoordinate = coordinateclass.createIndividual(nuclearplantgeneratoriri + plantname + ".owl#x_coordinate_of_"+plantname);
		Individual ycoordinate = coordinateclass.createIndividual(nuclearplantgeneratoriri + plantname + ".owl#y_coordinate_of_"+plantname);
		Individual xcoordinatevalue = valueclass.createIndividual(nuclearplantgeneratoriri + plantname + ".owl#v_x_coordinate_of_"+plantname);
		Individual ycoordinatevalue = valueclass.createIndividual(nuclearplantgeneratoriri + plantname + ".owl#v_y_coordinate_of_"+plantname);
		
		Individual capa = designcapacityclass.createIndividual(nuclearplantgeneratoriri + plantname + ".owl#capa_of_"+plantname);
		Individual capavalue = scalarvalueclass.createIndividual(nuclearplantgeneratoriri + plantname + ".owl#v_capa_of_"+plantname);
		
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
		
		plant.addProperty(designcapacity, capa);
		capa.addProperty(hasvalue, capavalue);
		capavalue.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(capanumval));
		capavalue.addProperty(hasunit, MW);
		
		plant.setPropertyValue(realizes, powergeneration);
		powergeneration.setPropertyValue(consumesprimaryfuel, nuclear);
		
		int numberofreactor=5;
		for(int f=0; f<numberofreactor;f++){
			Individual generator=nucleargeneratorclass.createIndividual(nuclearplantgeneratoriri + generatorname + ".owl#"+generatorname);
			Individual capagen = generatedactivepowerclass.createIndividual(nuclearplantgeneratoriri + generatorname + ".owl#GeneratedActivePower_"+generatorname);
			Individual capagenvalue = scalarvalueclass.createIndividual(nuclearplantgeneratoriri + generatorname + ".owl#V_GeneratedActivePower_"+generatorname);
			
			plant.addProperty(hasSubsystem, generator);
			Individual gencoordinate = coordinatesystemclass.createIndividual(nuclearplantgeneratoriri + generatorname + ".owl#CoordinateSystem_of_"+generatorname);
			Individual xgencoordinate = coordinateclass.createIndividual(nuclearplantgeneratoriri + generatorname + ".owl#x_coordinate_of_"+generatorname);
			Individual ygencoordinate = coordinateclass.createIndividual(nuclearplantgeneratoriri + generatorname+ ".owl#y_coordinate_of_"+generatorname);
			Individual xgencoordinatevalue = valueclass.createIndividual(nuclearplantgeneratoriri + generatorname + ".owl#v_x_coordinate_of_"+generatorname);
			Individual ygencoordinatevalue = valueclass.createIndividual(nuclearplantgeneratoriri + generatorname + ".owl#v_y_coordinate_of_"+generatorname);
			
			generator.addProperty(hascoordinatesystem, gencoordinate);
			
			gencoordinate.addProperty(hasx, xgencoordinate);
			xgencoordinate.addProperty(hasvalue, xgencoordinatevalue);
			xgencoordinate.addProperty(referto, xaxis);
			xgencoordinate.addProperty(hasdimension, length);
			xgencoordinatevalue.addProperty(numval, jenaOwlModel.createTypedLiteral(xnumval));
			xgencoordinatevalue.addProperty(hasunit, degree);
			
			gencoordinate.addProperty(hasy, ygencoordinate);
			ygencoordinate.addProperty(hasvalue, ygencoordinatevalue);
			ygencoordinate.addProperty(referto, yaxis);
			ygencoordinate.addProperty(hasdimension, length);
			ygencoordinatevalue.addProperty(numval, jenaOwlModel.createTypedLiteral(ynumval));
			ygencoordinatevalue.addProperty(hasunit, degree);
			
			generator.addProperty(hasActivepowergenerated, capagen);
			capagen.addProperty(hasvalue, capagenvalue);
			capagenvalue.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(capanumval));
			capagenvalue.addProperty(hasunit, MW);
		}
		
		
	}

}
