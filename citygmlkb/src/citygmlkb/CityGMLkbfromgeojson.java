package citygmlkb;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.UUID;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.hp.hpl.jena.util.FileUtils;

import edu.stanford.smi.protegex.owl.ProtegeOWL;
import edu.stanford.smi.protegex.owl.jena.JenaOWLModel;
import edu.stanford.smi.protegex.owl.model.OWLDatatypeProperty;
import edu.stanford.smi.protegex.owl.model.OWLNamedClass;
import edu.stanford.smi.protegex.owl.model.OWLObjectProperty;
import edu.stanford.smi.protegex.owl.model.RDFIndividual;
import edu.stanford.smi.protegex.owl.model.RDFSDatatype;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;


public class CityGMLkbfromgeojson {
	static String datacoordinate;
	static RDFIndividual groundins;
	static RDFIndividual wallins;
	static RDFIndividual roofins;
	static RDFIndividual compositesurface;

	private OWLNamedClass buildingclass = null;
	private OWLNamedClass buildingpartclass = null;
	private OWLNamedClass roofclass = null;
	private OWLNamedClass groundclass = null;
	private OWLNamedClass wallclass = null;
	private OWLNamedClass solidclass = null;
	private OWLNamedClass compositesurfaceclass = null;
	private OWLNamedClass polygonclass = null;
	private OWLNamedClass ringclass = null;
	private OWLNamedClass pointclass = null;
	private OWLNamedClass multisurfaceclass = null;
	private OWLNamedClass citymodelclass = null;
	private OWLNamedClass envelopeclass = null;
	private OWLNamedClass lengthclass = null;
	private OWLNamedClass angleclass = null;
	private OWLNamedClass rooftypeclass = null;
	private OWLNamedClass multicurveclass = null;
	private OWLNamedClass linestringclass = null;
	private OWLNamedClass appearanceclass = null;
	private OWLNamedClass x3dclass = null;
	private OWLNamedClass color = null;
	private OWLNamedClass double0and1 = null;
	private OWLNamedClass doubleattributetype = null;
	private OWLNamedClass coordinateclass = null;
	private OWLNamedClass coordinatesystemclass = null;
	private OWLNamedClass valueclass = null;
	private OWLNamedClass scalarvalueclass = null;
	private OWLNamedClass Diskclass = null;
	private OWLNamedClass Rectangleclass = null;

	private OWLObjectProperty hascoordinatesystem = null;
	private OWLObjectProperty hasx = null;
	private OWLObjectProperty hasy = null;
	private OWLObjectProperty hasz = null;
	private OWLObjectProperty hassurfacegeometry = null;
	private OWLObjectProperty cityobjectmember = null;
	private OWLObjectProperty boundedBy = null;
	private OWLObjectProperty lod2Solid = null;
	private OWLObjectProperty lod2MultiSurface = null;
	private OWLObjectProperty surfaceMember = null;
	private OWLObjectProperty curveMember = null;
	private OWLObjectProperty exterior = null;
	private OWLObjectProperty interior = null;
	private OWLObjectProperty consistsofbuildingpart = null;
	private OWLObjectProperty measuredHeight = null;
	private OWLObjectProperty hasangle = null;
	private OWLObjectProperty roofTyperelation = null;
	private OWLObjectProperty lowercorner = null;
	private OWLObjectProperty uppercorner = null;
	private OWLObjectProperty lod2TerrainIntersection = null;
	private OWLObjectProperty appearancerelation = null;
	private OWLObjectProperty diffuseColor = null;
	private OWLObjectProperty ambientintensity = null;
	private OWLObjectProperty surfacedatamemberrelation = null;
	private OWLObjectProperty doubleattribute = null;
	private OWLObjectProperty contains = null;
	private OWLObjectProperty hasvalue = null;
	private OWLObjectProperty hasunit = null;
	private OWLObjectProperty haslength = null;

	private OWLDatatypeProperty numval = null;
	private OWLDatatypeProperty srsname = null;
	private OWLDatatypeProperty id = null;
	private OWLDatatypeProperty value = null;
	private OWLDatatypeProperty target = null;
	private OWLDatatypeProperty name = null;

	private RDFIndividual m = null;

	private RDFSDatatype xsdDouble = null;
	
	public static String baseURL = "D:\\citygmllearn/";

	public static String baseURL2 = "D:\\citygmllearn/hkbuilding/";

	public void savefile(JenaOWLModel jenaOwlModel, String filePath2) throws URISyntaxException {

		Collection errors = new ArrayList();
		jenaOwlModel.save(new URI("file:////" + filePath2.replace("\\", "/")), FileUtils.langXMLAbbrev, errors,
				jenaOwlModel.getOntModel());
		System.out.println("File saved with " + errors.size() + " errors.");
	}
	
	public void startConversion() throws Exception {


			 String source= "C:/Users/kevin/Downloads/hkbuild.json";
			 String kbname= "hkbuildjson.owl";
//		 String source= "C:/Users/kevin/Downloads/sgbuildupdate/sgbuild.geojson";
//		 String kbname= "sgbuildjson.owl";

			/** load your knowledge base from an owl file; additionally */
			String filePath = baseURL + "gmlkb2.owl"; // the empty owl file
			String filePath2 = baseURL2 + kbname; // the result of written owl
													// file
			System.out.println(filePath);
			FileInputStream inFile = new FileInputStream(filePath);
			Reader in = new InputStreamReader(inFile, "UTF-8");
			JenaOWLModel jenaOwlModel = ProtegeOWL.createJenaOWLModelFromReader(in);

			initOWLClasses(jenaOwlModel);

			doConversion(jenaOwlModel, source, kbname);

			/** save the updated model file */
			savefile(jenaOwlModel, filePath2);

		
	}

	public void initOWLClasses(JenaOWLModel jenaOwlModel) {
		buildingclass = jenaOwlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#BuildingType");
		buildingpartclass = jenaOwlModel
				.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#BuildingPartType");
		roofclass = jenaOwlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#RoofSurfaceType");
		groundclass = jenaOwlModel
				.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#GroundSurfaceType");
		wallclass = jenaOwlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#WallSurfaceType");
		solidclass = jenaOwlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#SolidType");
		compositesurfaceclass = jenaOwlModel
				.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#CompositeSurfaceType");
		polygonclass = jenaOwlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#PolygonType");
		ringclass = jenaOwlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#LinearRingType");
		pointclass = jenaOwlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#PointType");
		multisurfaceclass = jenaOwlModel
				.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#MultiSurfaceType");
		citymodelclass = jenaOwlModel
				.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#CityModelType");
		envelopeclass = jenaOwlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#EnvelopeType");
		lengthclass = jenaOwlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#LengthType");
		angleclass = jenaOwlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#AngleType");
		rooftypeclass = jenaOwlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#RoofTypeType");
		multicurveclass = jenaOwlModel
				.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#MultiCurveType");
		linestringclass = jenaOwlModel
				.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#LineStringType");
		appearanceclass = jenaOwlModel
				.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#AppearanceType");
		x3dclass = jenaOwlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#X3DMaterialType");
		color = jenaOwlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#Color");
		double0and1 = jenaOwlModel
				.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#doubleBetween0and1");
		doubleattributetype = jenaOwlModel
				.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#DoubleAttributeType");
		coordinateclass = jenaOwlModel.getOWLNamedClass(
				"http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#StraightCoordinate");
		coordinatesystemclass = jenaOwlModel.getOWLNamedClass(
				"http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#ProjectedCoordinateSystem");
		valueclass = jenaOwlModel.getOWLNamedClass(
				"http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#CoordinateValue");
		scalarvalueclass = jenaOwlModel
				.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue");
		Diskclass = jenaOwlModel.getOWLNamedClass(
				"http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#Disk");
		Rectangleclass = jenaOwlModel.getOWLNamedClass(
				"http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#Rectangle");

		hascoordinatesystem = jenaOwlModel.getOWLObjectProperty(
				"http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasGISCoordinateSystem");
		hasx = jenaOwlModel.getOWLObjectProperty(
				"http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_x");
		hasy = jenaOwlModel.getOWLObjectProperty(
				"http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_y");
		hasz = jenaOwlModel.getOWLObjectProperty(
				"http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_z");
		hassurfacegeometry = jenaOwlModel.getOWLObjectProperty(
				"http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#hasSurfaceGeometry");
		cityobjectmember = jenaOwlModel
				.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#cityObjectMember");
		boundedBy = jenaOwlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#boundedBy");
		lod2Solid = jenaOwlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#lod2Solid");
		lod2MultiSurface = jenaOwlModel
				.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#lod2MultiSurface");
		surfaceMember = jenaOwlModel
				.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#surfaceMember");
		curveMember = jenaOwlModel
				.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#curveMember");
		exterior = jenaOwlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#exterior");
		interior = jenaOwlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#interior");
		consistsofbuildingpart = jenaOwlModel
				.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#consistsOfBuildingPart");
		measuredHeight = jenaOwlModel
				.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#measuredHeight");
		hasangle = jenaOwlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#angle");
		roofTyperelation = jenaOwlModel
				.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#roofType");
		lowercorner = jenaOwlModel
				.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#lowerCornerPoint");
		uppercorner = jenaOwlModel
				.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#upperCornerPoint");
		lod2TerrainIntersection = jenaOwlModel
				.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#lod2TerrainIntersection");
		appearancerelation = jenaOwlModel
				.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#Appearance");
		diffuseColor = jenaOwlModel
				.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#diffuseColor");
		ambientintensity = jenaOwlModel
				.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#ambientIntensity");
		surfacedatamemberrelation = jenaOwlModel
				.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#surfaceDataMember");
		doubleattribute = jenaOwlModel
				.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#doubleAttribute");

		contains = jenaOwlModel.getOWLObjectProperty(
				"http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#contains");
		hasvalue = jenaOwlModel.getOWLObjectProperty(
				"http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");
		hasunit = jenaOwlModel.getOWLObjectProperty(
				"http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");
		numval = jenaOwlModel.getOWLDatatypeProperty(
				"http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
		haslength = jenaOwlModel.getOWLObjectProperty(
				"http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#has_length");

		srsname = jenaOwlModel.getOWLDatatypeProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#srsname");
		id = jenaOwlModel.getOWLDatatypeProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#id");
		value = jenaOwlModel.getOWLDatatypeProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#value");
		target = jenaOwlModel.getOWLDatatypeProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#targetID");
		name = jenaOwlModel.getOWLDatatypeProperty("http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#name");

		m = jenaOwlModel.getRDFIndividual(
				"http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#m");

		xsdDouble = jenaOwlModel.getRDFSDatatypeByName("xsd:double");
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
	
	public void doConversion(JenaOWLModel jenaOwlModel, String source, String kbname)
			throws URISyntaxException, ParserConfigurationException, SAXException, IOException, JSONException {


	    String jsonData = readFile(source);
	    JSONObject jobj = new JSONObject(jsonData);
		RDFIndividual citymodel = citymodelclass.createRDFIndividual("http://www.theworldavatar.com/kb/hkg/hongkong/buildings/" + kbname + "#CityModel001");
		
		
		for (int o=0;o<32;o++) //41 for singapore, 32 for hongkong
		{
			String iriidentifier = "B"+UUID.randomUUID();
			
			RDFIndividual buildingentity = buildingclass.createRDFIndividual("http://www.theworldavatar.com/kb/hkg/hongkong/buildings/" + kbname + "#Building" + iriidentifier);
			buildingentity.setPropertyValue(id, jenaOwlModel.createRDFSLiteral(String.valueOf(o), xsdDouble));
			citymodel.addPropertyValue(cityobjectmember, buildingentity);
			
			RDFIndividual groundins = groundclass.createRDFIndividual("http://www.theworldavatar.com/kb/hkg/hongkong/buildings/" + kbname	+ "#GroundSurfaceOfBuilding_"+ iriidentifier);
			buildingentity.setPropertyValue(boundedBy, groundins);
			
			
			RDFIndividual surfaceins = multisurfaceclass.createRDFIndividual("http://www.theworldavatar.com/kb/hkg/hongkong/buildings/" + kbname	+ "#MultiSurfaceGroundOfBuilding_"+ iriidentifier);
			groundins.setPropertyValue(lod2MultiSurface, surfaceins);
			
			RDFIndividual polygonins = polygonclass.createRDFIndividual("http://www.theworldavatar.com/kb/hkg/hongkong/buildings/" + kbname	+ "#PolygonSurfaceGroundOfBuilding_"+ iriidentifier);
			surfaceins.setPropertyValue(surfaceMember, polygonins);
			
			RDFIndividual linearringins = ringclass.createRDFIndividual("http://www.theworldavatar.com/kb/hkg/hongkong/buildings/" + kbname	+ "#LinearRingOfGroundOfBuilding_"+ iriidentifier);
			polygonins.setPropertyValue(exterior, linearringins);
			

			String building = jobj.getJSONArray("features").getString(o);
			JSONObject buildingobj = new JSONObject(building);
			String coordinates=buildingobj.getJSONObject("geometry").getString("coordinates");
			System.out.println("coordinates obtained= "+coordinates);
			String clearresult= coordinates.replace("[","").replace("]","").trim();
			System.out.println("clear coordinates obtained= "+clearresult);
			int amountdata=clearresult.split(",").length;
			
		//	for (int count=1;count<=amountdata/2;count++)
		//	{
		//		System.out.println("ccccccc used= "+count);
				int i=0;
//			RDFIndividual pointins = pointclass.createRDFIndividual("http://www.theworldavatar.com/kb/hkg/hongkong/buildings/" + kbname	+ "#GroundOfBuilding_"+ iriidentifier+"_Points"+count);
//			linearringins.setPropertyValue(contains, pointins);
//			RDFIndividual pointscoordinate = coordinatesystemclass.createRDFIndividual("http://www.theworldavatar.com/kb/hkg/hongkong/buildings/" + kbname + "#GroundOfBuilding_" + iriidentifier + "Point_"+ count + "_Coordinates");
			
			System.out.println("amount of data= "+amountdata);
				for (int d = 0; d < amountdata/2; d ++) {
					RDFIndividual pointins = pointclass.createRDFIndividual("http://www.theworldavatar.com/kb/hkg/hongkong/buildings/" + kbname	+ "#GroundOfBuilding_"+ iriidentifier+"_Points"+d);
					linearringins.addPropertyValue(contains, pointins);
					RDFIndividual pointscoordinate = coordinatesystemclass.createRDFIndividual("http://www.theworldavatar.com/kb/hkg/hongkong/buildings/" + kbname + "#GroundOfBuilding_" + iriidentifier + "Point_"+ d + "_Coordinates");

					System.out.println("dddddddd used= "+d);
				String x = clearresult.split(",")[1+d+i-1];
				int xindex=1+d+i-1;
				System.out.println("index x used= "+xindex);
				System.out.println("making xpoints= "+i);
				RDFIndividual xpoints = coordinateclass.createRDFIndividual("http://www.theworldavatar.com/kb/hkg/hongkong/buildings/" + kbname + "#"+ iriidentifier + "x_Point_" + i);
				System.out.println("xpoints made= "+i);
				RDFIndividual xvalpoints = valueclass.createRDFIndividual("http://www.theworldavatar.com/kb/hkg/hongkong/buildings/" + kbname+ "#V_" + iriidentifier + "x_Point_" + i);
				xpoints.setPropertyValue(hasvalue, xvalpoints);
				xvalpoints.addPropertyValue(numval, jenaOwlModel.createRDFSLiteral(x, xsdDouble));
				xvalpoints.setPropertyValue(hasunit, m);

				String y = clearresult.split(",")[1+d+i];
				int yindex=1+d+i;
				System.out.println("index y used= "+yindex);
				System.out.println("making ypoints= "+i);
				RDFIndividual ypoints = coordinateclass.createRDFIndividual("http://www.theworldavatar.com/kb/hkg/hongkong/buildings/" + kbname + "#"	+ iriidentifier + "y_Point_" + i);
				System.out.println("ypoints made= "+i);
				RDFIndividual yvalpoints = valueclass.createRDFIndividual("http://www.theworldavatar.com/kb/hkg/hongkong/buildings/" + kbname+ "#V_" + iriidentifier + "y_Point_" + i);
				ypoints.setPropertyValue(hasvalue, yvalpoints);
				yvalpoints.addPropertyValue(numval, jenaOwlModel.createRDFSLiteral(y, xsdDouble));
				yvalpoints.setPropertyValue(hasunit, m);
				
				RDFIndividual zpoints = coordinateclass.createRDFIndividual("http://www.theworldavatar.com/kb/hkg/hongkong/buildings/" + kbname + "#"	+ iriidentifier + "z_Point_" + i);
				RDFIndividual zvalpoints = valueclass.createRDFIndividual("http://www.theworldavatar.com/kb/hkg/hongkong/buildings/" + kbname+ "#V_" + iriidentifier + "z_Point_" + i);
				zpoints.setPropertyValue(hasvalue, zvalpoints);
				zvalpoints.addPropertyValue(numval, jenaOwlModel.createRDFSLiteral("0", xsdDouble));
				zvalpoints.setPropertyValue(hasunit, m);
				
				i++;
				
				pointins.addPropertyValue(hascoordinatesystem, pointscoordinate);
				pointscoordinate.addPropertyValue(hasx, xpoints);
				pointscoordinate.addPropertyValue(hasy, ypoints);
				pointscoordinate.addPropertyValue(hasz, zpoints);
				System.out.println("======================================================");
				System.out.println("1point finished");
				System.out.println("======================================================");
				}
		
			//}
			
				RDFIndividual buildingcoordins = coordinatesystemclass.createRDFIndividual("http://www.theworldavatar.com/kb/hkg/hongkong/buildings/" + kbname	+ "#CoordinateSystemOfBuilding_"+ iriidentifier);
				buildingentity.setPropertyValue(hascoordinatesystem, buildingcoordins);
				
				RDFIndividual buildingcoordx = coordinateclass.createRDFIndividual("http://www.theworldavatar.com/kb/hkg/hongkong/buildings/" + kbname	+ "#x_Building_"+ iriidentifier);
				buildingcoordins.addPropertyValue(hasx, buildingcoordx);
				
				RDFIndividual buildingcoordy = coordinateclass.createRDFIndividual("http://www.theworldavatar.com/kb/hkg/hongkong/buildings/" + kbname	+ "#y_Building_"+ iriidentifier);
				buildingcoordins.addPropertyValue(hasy, buildingcoordy);
				
				RDFIndividual buildingcoordxvalue = valueclass.createRDFIndividual("http://www.theworldavatar.com/kb/hkg/hongkong/buildings/" + kbname	+ "#V_x_Building_"+ iriidentifier);
				buildingcoordx.setPropertyValue(hasvalue, buildingcoordxvalue);
				String xcentre = buildingobj.getJSONObject("properties").getString("xcenter");
				buildingcoordxvalue.addPropertyValue(numval, jenaOwlModel.createRDFSLiteral(xcentre, xsdDouble));
				buildingcoordxvalue.setPropertyValue(hasunit, m);
				
				RDFIndividual buildingcoordyvalue = valueclass.createRDFIndividual("http://www.theworldavatar.com/kb/hkg/hongkong/buildings/" + kbname	+ "#V_y_Building_"+ iriidentifier);
				buildingcoordy.setPropertyValue(hasvalue, buildingcoordyvalue);
				String ycentre = buildingobj.getJSONObject("properties").getString("ycenter");
				buildingcoordyvalue.addPropertyValue(numval, jenaOwlModel.createRDFSLiteral(ycentre, xsdDouble));
				buildingcoordyvalue.setPropertyValue(hasunit, m);
				
				String height = buildingobj.getJSONObject("properties").getString("height");
				RDFIndividual heightofbuilding = lengthclass.createRDFIndividual("http://www.theworldavatar.com/kb/hkg/hongkong/buildings/"+ kbname + "#EstimatedHeight_Building_" + iriidentifier);
				RDFIndividual heightval = scalarvalueclass.createRDFIndividual("http://www.theworldavatar.com/kb/hkg/hongkong/buildings/"	+ kbname + "#V_EstimatedHeight_Building_" + iriidentifier);
				buildingentity.setPropertyValue(measuredHeight, heightofbuilding);
				heightofbuilding.setPropertyValue(hasvalue, heightval);
				heightval.setPropertyValue(hasunit, m);
				heightval.addPropertyValue(numval, jenaOwlModel.createRDFSLiteral(height, xsdDouble));
					
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
