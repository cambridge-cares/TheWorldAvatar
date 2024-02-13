package uk.ac.cam.cares.jps.powsys.nuclear;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collection;

import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.ObjectProperty;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.ModelFactory;

import uk.ac.cam.cares.jps.powsys.util.Util;


public class LandlotsKB {
	OntModel jenaOwlModel = null;
	private DatatypeProperty numval = null;
	private ObjectProperty hasProperty = null;
	private ObjectProperty hasValue = null;
	private ObjectProperty hascoordinatesystem = null;
	private ObjectProperty hasx = null;
	private ObjectProperty hasdistance = null;
	private ObjectProperty hasy = null;
	private ObjectProperty hasunit = null;
	private ObjectProperty hasarea = null;
	private ObjectProperty hassurfacegeometry = null;
	private OntClass particleclass = null;
	private OntClass flowclass = null;
	private OntClass scalarvalueclass = null;
	private OntClass scalarquantityclass = null;
	private OntClass coordinatevalueclass = null;
	private OntClass coordinateclass = null;
	private OntClass coordinatesystemclass = null;
	private OntClass LandLotsclass = null;
	private OntClass surfaceclass = null;
	private OntClass areaclass = null;
	
	public static String baseURL2 = "D:\\KBDev-git/irp3-JPS-KBDev-git/Server Ontology Configuration Root/kb/sgp/jurongisland/";
	public static String filename="JurongIslandLandlots";
	
	static Individual gpers;
	static Individual m2;
	static Individual m;
	static Individual degree;
	static Individual kg_per_m3;
	
	public void savefile(OntModel jenaOwlModel, String filePath2) throws URISyntaxException, FileNotFoundException {

		FileOutputStream out = new FileOutputStream(filePath2);
		
		Collection errors = new ArrayList();
		jenaOwlModel.write(out, "RDF/XML-ABBREV");

		
		System.out.println("File saved with " + errors.size() + " errors.");
	}
	
	public void initOWLClasses(OntModel jenaOwlModel) {
		numval = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");	
		hasProperty=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasProperty");
		hasValue=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");
		hasunit = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");
		hascoordinatesystem = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasGISCoordinateSystem");
		hasx = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_x");
		hasy = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_y");
		hassurfacegeometry = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#hasSurfaceGeometry");
		hasarea = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#has_area");
		surfaceclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#Surface");
		scalarvalueclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue");
		coordinatesystemclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#ProjectedCoordinateSystem");
		coordinateclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#AngularCoordinate"); 
		coordinatevalueclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#CoordinateValue");
		LandLotsclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#Landlot"); //need modif
		areaclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#SurfaceArea"); //need modif
		degree=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#degree");
		m=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#m");
		m2=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#m.m");
		 kg_per_m3=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#kg_per_cubic_m");
		 scalarquantityclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarQuantity");
		 hasdistance = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#hasDistanceToClosestWaterSources");
	}
	
	public void startConversion() throws Exception {
    	
		String baseURL = Util.getResourceDir(this);
    	String filePath = baseURL + "/LotsTemplate.owl"; // the empty owl file 
		FileInputStream inFile = new FileInputStream(filePath);
    	Reader in = new InputStreamReader(inFile, "UTF-8");
	    			
    	OntModel jenaOwlModel = ModelFactory.createOntologyModel();
    	jenaOwlModel.read(in, null);
		initOWLClasses(jenaOwlModel);
		
		String filePath2 = baseURL2 +filename+".owl"; // the result of written owl file
		
		doConversion(jenaOwlModel); 
		
		/** save the updated model file */
		savefile(jenaOwlModel, filePath2);
	
	}  	
	
	public void doConversion(OntModel jenaOwlModel) {
		
		String csvFile = Util.getResourceDir(this) + "/Landlots.csv";
		
        String line = "";
        String cvsSplitBy = ",";
        int linereader=0;
    	try (BufferedReader br = new BufferedReader(new FileReader(csvFile))) {

            while ((line = br.readLine()) != null) {
            	if(linereader==0) {
            		System.out.println("skipped because it's header");

            	}
            	else {
            		
                // use comma as separator
                String[] iri = line.split(cvsSplitBy);
                System.out.println("content1= "+iri[0]);
                Individual lotsid = LandLotsclass.createIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+".owl#Lots-ID"+iri[0]);
                
                Individual lotcoorsys=coordinatesystemclass.createIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+".owl#CoordinateSystemofLots-ID"+iri[0]);
            	lotsid.addProperty(hascoordinatesystem, lotcoorsys);
            	Individual lotx = coordinateclass.createIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+".owl#X_Lots-ID"+iri[0]);
            	Individual loty=coordinateclass.createIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+".owl#Y_Lots-ID"+iri[0]);
            	lotcoorsys.addProperty(hasx,lotx);
            	lotcoorsys.addProperty(hasy,loty);
            	Individual valuelotx = coordinatevalueclass.createIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+".owl#V_X_Lots-ID"+iri[0]);
            	Individual valueloty=coordinatevalueclass.createIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+".owl#V_Y_Lots-ID"+iri[0]);
            	lotx.addProperty(hasValue,valuelotx);
            	loty.addProperty(hasValue,valueloty);
            	valuelotx.addProperty(hasunit,degree);
            	valueloty.addProperty(hasunit,degree);
            	valuelotx.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(iri[2])));
            	valueloty.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(iri[1])));
            	
            	Individual lotsurface=surfaceclass.createIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+".owl#SurfaceofLots-ID"+iri[0]);
            	lotsid.addProperty(hassurfacegeometry, lotsurface);
            	Individual lotarea=areaclass.createIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+".owl#SurfaceAreaofLots-ID"+iri[0]);
            	lotsurface.addProperty(hasarea, lotarea);
            	Individual valuelotarea = scalarvalueclass.createIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+".owl#V_SurfaceAreaofLots-ID"+iri[0]);
            	lotarea.addProperty(hasValue, valuelotarea);
            	valuelotarea.addProperty(hasunit, m2);
            	valuelotarea.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(iri[3])));
            	
            	Individual distance=scalarquantityclass.createIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+".owl#WaterSourceClosestDistanceOfLots-ID"+iri[0]);
            	lotsid.addProperty(hasdistance, distance);
            	Individual vdistance=scalarvalueclass.createIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"+filename+".owl#V_WaterSourceClosestDistanceOfLots-ID"+iri[0]);
            	distance.addProperty(hasValue, vdistance);
            	vdistance.addProperty(hasunit,m);
            	vdistance.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(iri[4])));
            	
            	}              	
            	linereader++;
            }

        } catch (IOException e) {
            e.printStackTrace();
        }
		
		
	}
	
	public static void main(String[] args) throws Exception {
		LandlotsKB a = new LandlotsKB();
		a.startConversion();
	}



}
