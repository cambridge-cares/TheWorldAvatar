package citygmlkb;

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
import java.util.UUID;

//import javax.xml.parsers.ParserConfigurationException;





import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.ObjectProperty;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.ModelFactory;

//import edu.stanford.smi.protegex.owl.ProtegeOWL;
//import edu.stanford.smi.protegex.owl.jena.JenaOWLModel;
//import edu.stanford.smi.protegex.owl.model.OWLDatatypeProperty;
//import edu.stanford.smi.protegex.owl.model.OntClass;
//import edu.stanford.smi.protegex.OntClassctProperty;
//import edu.stanford.smi.protegex.owl.model.RDFIndividual;
//import edu.stanford.smi.protegex.owl.model.RDFSDatatype;

import org.json.JSONException;
import org.json.JSONObject;
import org.xml.sax.SAXException;
 /**
  * 
  * 
  * minheight is ground(0m) to base building (both in geojson and citygml are the same)
  * height is ground(0m) to the highest top of building (now only in geojson)
  * estimated height in this owl file= relative height of the building only compared to the ground ( don't care about the elevation of ground)
  * @author KADIT01
  *
  */

public class CityGMLkbfromgeojson {
	static String datacoordinate;
	static Individual groundins;
	static Individual wallins;
	static Individual roofins;
	static Individual compositesurface;

	private OntClass buildingclass = null;
	private OntClass buildingpartclass = null;
	private OntClass roofclass = null;
	private OntClass groundclass = null;
	private OntClass wallclass = null;
	private OntClass solidclass = null;
	private OntClass compositesurfaceclass = null;
	private OntClass polygonclass = null;
	private OntClass ringclass = null;
	private OntClass pointclass = null;
	private OntClass multisurfaceclass = null;
	private OntClass citymodelclass = null;
	private OntClass envelopeclass = null;
	private OntClass lengthclass = null;
	private OntClass angleclass = null;
	private OntClass rooftypeclass = null;
	private OntClass multicurveclass = null;
	private OntClass linestringclass = null;
	private OntClass appearanceclass = null;
	private OntClass x3dclass = null;
	private OntClass color = null;
	private OntClass double0and1 = null;
	private OntClass doubleattributetype = null;
	private OntClass coordinateclass = null;
	private OntClass coordinatesystemclass = null;
	private OntClass valueclass = null;
	private OntClass scalarvalueclass = null;
	private OntClass Diskclass = null;
	private OntClass Rectangleclass = null;

	private ObjectProperty hascoordinatesystem = null;
	private ObjectProperty hasx = null;
	private ObjectProperty hasy = null;
	private ObjectProperty hasz = null;
	private ObjectProperty hassurfacegeometry = null;
	private ObjectProperty cityobjectmember = null;
	private ObjectProperty boundedBy = null;
	private ObjectProperty lod2Solid = null;
	private ObjectProperty lod2MultiSurface = null;
	private ObjectProperty surfaceMember = null;
	private ObjectProperty curveMember = null;
	private ObjectProperty exterior = null;
	private ObjectProperty interior = null;
	private ObjectProperty consistsofbuildingpart = null;
	private ObjectProperty measuredHeight = null;
	private ObjectProperty hasangle = null;
	private ObjectProperty roofTyperelation = null;
	private ObjectProperty lowercorner = null;
	private ObjectProperty uppercorner = null;
	private ObjectProperty lod2TerrainIntersection = null;
	private ObjectProperty appearancerelation = null;
	private ObjectProperty diffuseColor = null;
	private ObjectProperty ambientintensity = null;
	private ObjectProperty surfacedatamemberrelation = null;
	private ObjectProperty doubleattribute = null;
	private ObjectProperty contains = null;
	private ObjectProperty hasvalue = null;
	private ObjectProperty lowerHeight = null;
	private ObjectProperty hasunit = null;
	private ObjectProperty haslength = null;

	private DatatypeProperty numval = null;
	private DatatypeProperty srsname = null;
	private DatatypeProperty id = null;
	private DatatypeProperty value = null;
	private DatatypeProperty target = null;
	private DatatypeProperty name = null;

	static Individual m;
	static Individual degree;


	
	public static String baseURL = "D:\\citygmllearn/";

	public static String baseURL2 = "D:\\citygmllearn/hkbuilding/";

	public void savefile(OntModel jenaOwlModel, String filePath2) throws URISyntaxException, FileNotFoundException {

		Collection errors = new ArrayList();
		
		FileOutputStream out = new FileOutputStream(filePath2);
		
		jenaOwlModel.write(out, "RDF/XML-ABBREV");
		System.out.println("File saved with " + errors.size() + " errors.");
	}
	
	public static double[] centroid(ArrayList<String> xvalueground, ArrayList<String> yvalueground) {
		int totvertices = xvalueground.size();
		double[] x = new double[totvertices];
		double[] y = new double[totvertices];
		double result = 0;
		double Cx = 0;
		double Cy = 0;

		// Converting arraylist of strings to arraylist of doubles
		for (int a = 0; a < totvertices; a++) {
			x[a] = Double.parseDouble(xvalueground.get(a));
			y[a] = Double.parseDouble(yvalueground.get(a));
		}
		// calculation of the A
		for (int a = 0; a < (totvertices - 1); a++) {
			result += x[a] * y[a + 1] - x[a + 1] * y[a];
		}

		double A = 0.5 * result;

		// calculation of the Cx
		for (int a = 0; a < totvertices - 1; a++) {
			Cx += (x[a] + x[a + 1]) * (x[a] * y[a + 1] - x[a + 1] * y[a]) / 6 / A;
		}
		// calculation of the Cy
		for (int a = 0; a < totvertices - 1; a++) {
			Cy += (y[a] + y[a + 1]) * (x[a] * y[a + 1] - x[a + 1] * y[a]) / 6 / A;

		}
		/*
		 * System.out.println("total point=" + totvertices); System.out.println(
		 * "x centroid=" + Cx); System.out.println("y centroid=" + Cy);
		 */
		
		return new double[] { Cx, Cy, A };
	}
	
	public void startConversion() throws Exception {


			 //String source= "D:/Users/KADIT01/Downloads/GEOJSON_MARINA_BAY.json";
		//String source= "D:/Users/KADIT01/Downloads/sample2.json";
		String source= "D:/citygmllearn/hkbuilding/hk_buildings.json";
			 String kbname= "HongkongDistrict02.owl";
			 //String kbname= "hkbuildjson.owl";
//		 String source= "C:/Users/kevin/Downloads/sgbuildupdate/sgbuild.geojson";
//		 String kbname= "sgbuildjson.owl";

			/** load your knowledge base from an owl file; additionally */
			String filePath = baseURL + "gmlkb2.owl"; // the empty owl file
			String filePath2 = baseURL2 + kbname; // the result of written owl
													// file
			System.out.println(filePath);
			FileInputStream inFile = new FileInputStream(filePath);
			Reader in = new InputStreamReader(inFile, "UTF-8");
			OntModel jenaOwlModel = ModelFactory.createOntologyModel();
        	jenaOwlModel.read(in, null);

			initOWLClasses(jenaOwlModel);

			doConversion(jenaOwlModel, source, kbname);

			/** save the updated model file */
			savefile(jenaOwlModel, filePath2);

		
	}

	public void initOWLClasses(OntModel jenaOwlModel) {
		buildingclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#BuildingType");
		buildingpartclass = jenaOwlModel
				.getOntClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#BuildingPartType");
		roofclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#RoofSurfaceType");
		groundclass = jenaOwlModel
				.getOntClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#GroundSurfaceType");
		wallclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#WallSurfaceType");
		solidclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#SolidType");
		compositesurfaceclass = jenaOwlModel
				.getOntClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#CompositeSurfaceType");
		polygonclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#PolygonType");
		ringclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#LinearRingType");
		pointclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#PointType");
		multisurfaceclass = jenaOwlModel
				.getOntClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#MultiSurfaceType");
		citymodelclass = jenaOwlModel
				.getOntClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#CityModelType");
		envelopeclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType");
		lengthclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#LengthType");
		angleclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#AngleType");
		rooftypeclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#RoofTypeType");
		multicurveclass = jenaOwlModel
				.getOntClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#MultiCurveType");
		linestringclass = jenaOwlModel
				.getOntClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#LineStringType");
		appearanceclass = jenaOwlModel
				.getOntClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#AppearanceType");
		x3dclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#X3DMaterialType");
		color = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#Color");
		double0and1 = jenaOwlModel
				.getOntClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#doubleBetween0and1");
		doubleattributetype = jenaOwlModel
				.getOntClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#DoubleAttributeType");
		coordinateclass = jenaOwlModel.getOntClass(
				"http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#StraightCoordinate");
		coordinatesystemclass = jenaOwlModel.getOntClass(
				"http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#ProjectedCoordinateSystem");
		valueclass = jenaOwlModel.getOntClass(
				"http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#CoordinateValue");
		scalarvalueclass = jenaOwlModel
				.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue");
		Diskclass = jenaOwlModel.getOntClass(
				"http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#Disk");
		Rectangleclass = jenaOwlModel.getOntClass(
				"http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#Rectangle");

		hascoordinatesystem = jenaOwlModel.getObjectProperty(
				"http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasGISCoordinateSystem");
		hasx = jenaOwlModel.getObjectProperty(
				"http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_x");
		hasy = jenaOwlModel.getObjectProperty(
				"http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_y");
		hasz = jenaOwlModel.getObjectProperty(
				"http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_z");
		hassurfacegeometry = jenaOwlModel.getObjectProperty(
				"http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#hasSurfaceGeometry");
		cityobjectmember = jenaOwlModel
				.getObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#cityObjectMember");
		boundedBy = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#boundedBy");
		lod2Solid = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#lod2Solid");
		lod2MultiSurface = jenaOwlModel
				.getObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#lod2MultiSurface");
		surfaceMember = jenaOwlModel
				.getObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#surfaceMember");
		curveMember = jenaOwlModel
				.getObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#curveMember");
		exterior = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#exterior");
		interior = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#interior");
		consistsofbuildingpart = jenaOwlModel
				.getObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#consistsOfBuildingPart");
		measuredHeight = jenaOwlModel
				.getObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#measuredHeight");
		hasangle = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#angle");
		roofTyperelation = jenaOwlModel
				.getObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#roofType");
		lowercorner = jenaOwlModel
				.getObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#lowerCornerPoint");
		uppercorner = jenaOwlModel
				.getObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#upperCornerPoint");
		lod2TerrainIntersection = jenaOwlModel
				.getObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#lod2TerrainIntersection");
		appearancerelation = jenaOwlModel
				.getObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#Appearance");
		diffuseColor = jenaOwlModel
				.getObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#diffuseColor");
		ambientintensity = jenaOwlModel
				.getObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#ambientIntensity");
		surfacedatamemberrelation = jenaOwlModel
				.getObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#surfaceDataMember");
		doubleattribute = jenaOwlModel
				.getObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#doubleAttribute");
		lowerHeight=jenaOwlModel
				.getObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#lowerHeight");

		contains = jenaOwlModel.getObjectProperty(
				"http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#contains");
		hasvalue = jenaOwlModel.getObjectProperty(
				"http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");
		hasunit = jenaOwlModel.getObjectProperty(
				"http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");
		numval = jenaOwlModel.getDatatypeProperty(
				"http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
		haslength = jenaOwlModel.getObjectProperty(
				"http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#has_length");

		srsname = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#srsname");
		id = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#id");
		value = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#value");
		target = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#targetID");
		name = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#name");
		degree=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#degree");
		m=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#m");
		//System.out.println("degree0= "+ degree.getURI());

		System.out.println("degree2= "+ degree.toString());

		//xsdDouble = jenaOwlModel.getRDFSDatatypeByName("xsd:double");
	}

	public static String readFile(String filename) {
	    String result = "";
	    try {
	        BufferedReader br = new BufferedReader(new FileReader(filename));
	        StringBuilder sb = new StringBuilder();
	        String line = br.readLine();
	        while (line != null) {
	            sb.append(line);
	            line = br.readLine();
	        }
	        result = sb.toString();
	        
	    } catch(Exception e) {
	        e.printStackTrace();
	    }


	    return result;
	}
	
	public void doConversion(OntModel jenaOwlModel, String source, String kbname)
			throws URISyntaxException, SAXException, IOException, JSONException {
		
		String prefix="http://www.theworldavatar.com/kb/hkg/hongkong/buildings/";


	    String jsonData = readFile(source);
	    JSONObject jobj = new JSONObject(jsonData);
	    int numberofbuilding= jobj.getJSONArray("features").length();
		Individual citymodel = citymodelclass.createIndividual(prefix + kbname + "#CityModel001");
		Individual envelope=envelopeclass.createIndividual(prefix + kbname + "#Envelope001");
		citymodel.addProperty(boundedBy, envelope);
		envelope.setPropertyValue(srsname, jenaOwlModel.createTypedLiteral("EPSG:4326"));
		
		Individual uppoint=pointclass.createIndividual(prefix + kbname + "#UpperPointBoundary");
		envelope.addProperty(uppercorner, uppoint);
		Individual uppointcoordinates=coordinatesystemclass.createIndividual(prefix + kbname + "#UpperPointBoundary_Coordinates");
		uppoint.addProperty(hascoordinatesystem, uppointcoordinates);
		
		Individual xpointsup = coordinateclass.createIndividual(prefix + kbname + "#x_UpperPointBoundary");
		
		Individual ypointsup = coordinateclass.createIndividual(prefix + kbname + "#y_UpperPointBoundary");
		
		Individual zpointsup = coordinateclass.createIndividual(prefix + kbname + "#z_UpperPointBoundary");
		
		uppointcoordinates.addProperty(hasx, xpointsup);
		uppointcoordinates.addProperty(hasy, ypointsup);
		uppointcoordinates.addProperty(hasz, zpointsup);
		
		Individual vxpointsup = valueclass.createIndividual(prefix + kbname + "#V_x_UpperPointBoundary");
		Individual vypointsup = valueclass.createIndividual(prefix + kbname + "#V_y_UpperPointBoundary");
		Individual vzpointsup = valueclass.createIndividual(prefix + kbname + "#V_z_upperPointBoundary");
		xpointsup.addProperty(hasvalue, vxpointsup);
		ypointsup.addProperty(hasvalue, vypointsup);
		zpointsup.addProperty(hasvalue, vzpointsup);
		
		vxpointsup.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(jobj.getDouble("xmax")));
		vypointsup.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(jobj.getDouble("ymax")));
		vzpointsup.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(jobj.optString("zmax", "0.0")));
		
		System.out.println(hasunit.getLocalName());
		vxpointsup.addProperty(hasunit, degree);
		vypointsup.addProperty(hasunit, degree);
		vzpointsup.addProperty(hasunit, degree);
		
		Individual lowpoint=pointclass.createIndividual(prefix + kbname + "#LowerPointBoundary");
		envelope.addProperty(lowercorner, lowpoint);
		Individual lowpointcoordinates=coordinatesystemclass.createIndividual(prefix + kbname + "#LowerPointBoundary_Coordinates");
		lowpoint.addProperty(hascoordinatesystem, lowpointcoordinates);
		
			Individual xpointslow = coordinateclass.createIndividual(prefix + kbname + "#x_LowerPointBoundary");
			
			Individual ypointslow = coordinateclass.createIndividual(prefix + kbname + "#y_LowerPointBoundary");
			
			Individual zpointslow = coordinateclass.createIndividual(prefix + kbname + "#z_LowerPointBoundary");
		
			
			lowpointcoordinates.addProperty(hasx, xpointslow);
			lowpointcoordinates.addProperty(hasy, ypointslow);
			lowpointcoordinates.addProperty(hasz, zpointslow);
			
			Individual vxpointslow = valueclass.createIndividual(prefix + kbname + "#V_x_LowerPointBoundary");
			Individual vypointslow = valueclass.createIndividual(prefix + kbname + "#V_y_LowerPointBoundary");
			Individual vzpointslow = valueclass.createIndividual(prefix + kbname + "#V_z_LowerPointBoundary");
			
			xpointslow.addProperty(hasvalue, vxpointslow);
			ypointslow.addProperty(hasvalue, vypointslow);
			zpointslow.addProperty(hasvalue, vzpointslow);
			
			vxpointslow.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(jobj.getDouble("xmin")));
			vypointslow.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(jobj.getDouble("ymin")));
			vzpointslow.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(jobj.optString("zmin","0.0")));
			
			vxpointslow.addProperty(hasunit, degree);
			vypointslow.addProperty(hasunit, degree);
			vzpointslow.addProperty(hasunit, degree);
			
		for (int o=0;o<numberofbuilding;o++) //41 for singapore, 32 for hongkong 55 for mbs
		{
			String iriidentifier = "B"+UUID.randomUUID();
			
			Individual buildingentity = buildingclass.createIndividual(prefix + kbname + "#Building" + iriidentifier);

			
			citymodel.addProperty(cityobjectmember, buildingentity);
			
			Individual groundins = groundclass.createIndividual(prefix + kbname	+ "#GroundSurfaceOfBuilding_"+ iriidentifier);
			buildingentity.addProperty(boundedBy, groundins);
			
			
			Individual surfaceins = multisurfaceclass.createIndividual(prefix + kbname	+ "#MultiSurfaceGroundOfBuilding_"+ iriidentifier);
			groundins.addProperty(lod2MultiSurface, surfaceins);
			
			Individual polygonins = polygonclass.createIndividual(prefix + kbname	+ "#PolygonSurfaceGroundOfBuilding_"+ iriidentifier);
			surfaceins.addProperty(surfaceMember, polygonins);
			
			Individual linearringins = ringclass.createIndividual(prefix + kbname	+ "#LinearRingOfGroundOfBuilding_"+ iriidentifier);
			polygonins.addProperty(exterior, linearringins);
			

			String building = jobj.getJSONArray("features").getString(o);
			JSONObject buildingobj = new JSONObject(building);
			buildingentity.setPropertyValue(id, jenaOwlModel.createTypedLiteral(buildingobj.getString("id")));
			String coordinates=buildingobj.getJSONObject("geometry").getString("coordinates");
			System.out.println("coordinates obtained= "+coordinates);
			String clearresult= coordinates.replace("[","").replace("]","").trim();
			System.out.println("clear coordinates obtained= "+clearresult);
			int amountdata=clearresult.split(",").length;
			
			ArrayList<String> xvalueground = new ArrayList<String>();
			ArrayList<String> yvalueground = new ArrayList<String>();
			for(int a=0;a<amountdata;a+=2)
			{
				xvalueground.add(clearresult.split(",")[a]);
				yvalueground.add(clearresult.split(",")[a+1]);
			}
			
			Double xcenter=centroid(xvalueground,yvalueground)[0];
			Double ycenter=centroid(xvalueground,yvalueground)[1];
			
			
			

				int i=0;
				
			System.out.println("amount of data= "+amountdata);
				for (int d = 0; d < amountdata/2; d ++) {
					Individual pointins = pointclass.createIndividual(prefix + kbname	+ "#GroundOfBuilding_"+ iriidentifier+"_Points"+String.format("%02d", d));
					linearringins.addProperty(contains, pointins);
					Individual pointscoordinate = coordinatesystemclass.createIndividual(prefix + kbname + "#GroundOfBuilding_" + iriidentifier + "Point_"+ String.format("%02d", d) + "_Coordinates");

					System.out.println("dddddddd used= "+d);
				String x = clearresult.split(",")[1+d+i-1];
				int xindex=1+d+i-1;
				System.out.println("index x used= "+xindex);
				System.out.println("making xpoints= "+i);
				Individual xpoints = coordinateclass.createIndividual(prefix + kbname + "#"+ iriidentifier + "x_Point_" + String.format("%02d", i));
				System.out.println("xpoints made= "+i);
				Individual xvalpoints = valueclass.createIndividual(prefix + kbname+ "#V_" + iriidentifier + "x_Point_" + String.format("%02d", i));
				xpoints.addProperty(hasvalue, xvalpoints);
				xvalpoints.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(x)));
				xvalpoints.addProperty(hasunit, degree);

				String y = clearresult.split(",")[1+d+i];
				int yindex=1+d+i;
				System.out.println("index y used= "+yindex);
				System.out.println("making ypoints= "+i);
				Individual ypoints = coordinateclass.createIndividual(prefix + kbname + "#"	+ iriidentifier + "y_Point_" + String.format("%02d", i));
				System.out.println("ypoints made= "+i);
				Individual yvalpoints = valueclass.createIndividual(prefix + kbname+ "#V_" + iriidentifier + "y_Point_" + String.format("%02d", i));
				ypoints.addProperty(hasvalue, yvalpoints);
				yvalpoints.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(y)));
				yvalpoints.addProperty(hasunit, degree);
				
				Individual zpoints = coordinateclass.createIndividual(prefix + kbname + "#"	+ iriidentifier + "z_Point_" + String.format("%02d", i));
				Individual zvalpoints = valueclass.createIndividual(prefix + kbname+ "#V_" + iriidentifier + "z_Point_" + String.format("%02d", i));
				zpoints.addProperty(hasvalue, zvalpoints);
				zvalpoints.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double("0")));
				zvalpoints.addProperty(hasunit,degree);
				
				i++;
				
				pointins.addProperty(hascoordinatesystem, pointscoordinate);
				pointscoordinate.addProperty(hasx, xpoints);
				pointscoordinate.addProperty(hasy, ypoints);
				pointscoordinate.addProperty(hasz, zpoints);
				System.out.println("======================================================");
				System.out.println("1point finished");
				System.out.println("======================================================");
				}
		
			//}
			
				Individual buildingcoordins = coordinatesystemclass.createIndividual(prefix + kbname	+ "#CoordinateSystemOfBuilding_"+ iriidentifier);
				buildingentity.addProperty(hascoordinatesystem, buildingcoordins);
				
				Individual buildingcoordx = coordinateclass.createIndividual(prefix + kbname	+ "#x_Building_"+ iriidentifier);
				buildingcoordins.addProperty(hasx, buildingcoordx);
				
				Individual buildingcoordy = coordinateclass.createIndividual(prefix + kbname	+ "#y_Building_"+ iriidentifier);
				buildingcoordins.addProperty(hasy, buildingcoordy);
				
				Individual buildingcoordxvalue = valueclass.createIndividual(prefix + kbname	+ "#V_x_Building_"+ iriidentifier);
				buildingcoordx.addProperty(hasvalue, buildingcoordxvalue);
				String xcentre = buildingobj.getJSONObject("properties").optString("xcenter",String.valueOf(xcenter));
				buildingcoordxvalue.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(xcentre)));
				buildingcoordxvalue.addProperty(hasunit, degree);
				
				Individual buildingcoordyvalue = valueclass.createIndividual(prefix + kbname	+ "#V_y_Building_"+ iriidentifier);
				buildingcoordy.addProperty(hasvalue, buildingcoordyvalue);
				String ycentre = buildingobj.getJSONObject("properties").optString("ycenter",String.valueOf(ycenter));
				buildingcoordyvalue.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double (ycentre)));
				buildingcoordyvalue.addProperty(hasunit, degree);
				
				Individual minheightofbuilding = lengthclass.createIndividual(prefix+ kbname + "#MinHeight_Building_" + iriidentifier);
				Individual minheightval = scalarvalueclass.createIndividual(prefix	+ kbname + "#V_MinHeight_Building_" + iriidentifier);
				buildingentity.addProperty(lowerHeight, minheightofbuilding);
				minheightofbuilding.addProperty(hasvalue, minheightval);
				minheightval.addProperty(hasunit, m);
				
				String lowerheightnumber = buildingobj.getJSONObject("properties").optString("minHeight", "0");
				minheightval.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double (lowerheightnumber)));
				
				Individual heightofbuilding = lengthclass.createIndividual(prefix+ kbname + "#EstimatedHeight_Building_" + iriidentifier);
				Individual heightval = scalarvalueclass.createIndividual(prefix	+ kbname + "#V_EstimatedHeight_Building_" + iriidentifier);
				buildingentity.addProperty(measuredHeight, heightofbuilding);
				heightofbuilding.addProperty(hasvalue, heightval);
				heightval.addProperty(hasunit, m);
				String namevalue = buildingobj.getJSONObject("properties").optString("name", "unknown");
				buildingentity.setPropertyValue(name,jenaOwlModel.createTypedLiteral(namevalue));
				
				Double optionalheight=10.0+Double.valueOf(lowerheightnumber);
				
				
				String height = buildingobj.getJSONObject("properties").optString("height", String.valueOf(optionalheight));
				
				Double estimatedheight=Double.valueOf(height)-Double.valueOf(lowerheightnumber);
				heightval.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(estimatedheight)));			
				System.out.println("height= "+height);
				System.out.println("minheight= "+lowerheightnumber);
				System.out.println("estimated height= "+estimatedheight);
				System.out.println("======================================================");
				System.out.println("1building finished");
				System.out.println("======================================================");
			
		}	
		
	}// comment in wrong

	
	public static void main(String[] args) throws Exception {
		System.out.println("Starting Process");
		CityGMLkbfromgeojson converter = new CityGMLkbfromgeojson();
		converter.startConversion();

	}
	
}
