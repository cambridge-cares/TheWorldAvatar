package citygmlkb;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;

import java.io.Reader;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

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
import edu.stanford.smi.protegex.owl.model.OWLIndividual;

import edu.stanford.smi.protegex.owl.model.OWLNamedClass;
import edu.stanford.smi.protegex.owl.model.OWLObjectProperty;
import edu.stanford.smi.protegex.owl.model.RDFIndividual;
import edu.stanford.smi.protegex.owl.model.RDFSDatatype;


//some times this code creates an owl file that can't be opened because there is a tag exist which is "<owl:Axiom/>"
public class CitygmlkbDH {

	public static String baseURL = "D:\\citygmllearn/";
	public static String baseURL2 = "D:\\citygmllearn/denhaagkb/foldertrial/";
	//directory for the berlin kb
	//public static String baseURL3 = "D:\\citygmllearn/berlinkb/";

	static ArrayList<String> xvaluewall = new ArrayList<String>();
	static ArrayList<String> yvaluewall = new ArrayList<String>();
	static ArrayList<String> zvaluewall = new ArrayList<String>();

	static ArrayList<String> xvalueground = new ArrayList<String>();
	static ArrayList<String> yvalueground = new ArrayList<String>();
	static ArrayList<String> zvalueground = new ArrayList<String>();

	// container for the x vertices of ground every building part
	static ArrayList<String> xgroundspec = new ArrayList<String>();
	// container for the y vertices of ground every building part
	static ArrayList<String> ygroundspec = new ArrayList<String>();
	// container for the z vertices of ground every building part
	static ArrayList<String> zgroundspec = new ArrayList<String>();
	// container for the z vertices of roof every building part
	static ArrayList<String> zroofspec = new ArrayList<String>();
	// container for the z vertices id of ground every building part
	static ArrayList<Integer> zgroundid = new ArrayList<Integer>();
	// container for all x vertices of ground in 1 building
	static ArrayList<String> xgroundbuild = new ArrayList<String>();
	// container for all y vertices of ground in 1 building
	static ArrayList<String> ygroundbuild = new ArrayList<String>();
	// container for all z vertices of ground in 1 building
	static ArrayList<String> zgroundbuild = new ArrayList<String>();
	// container for the z vertices id of ground in every building
	static ArrayList<Integer> zgroundbuildid = new ArrayList<Integer>();

	static ArrayList<String> xoverallground = new ArrayList<String>();
	static ArrayList<String> yoverallground = new ArrayList<String>();

	static ArrayList<Double> x1 = new ArrayList<Double>();
	static ArrayList<Double> y1 = new ArrayList<Double>();
	static ArrayList<Double> A1 = new ArrayList<Double>();

	// container for edge length of a ground area
	static ArrayList<Double> overalllength = new ArrayList<Double>();
	// container for x vertices of a length of ground area
	static ArrayList<Double> xofoveralllength = new ArrayList<Double>();
	// container for y vertices of a length of ground area
	static ArrayList<Double> yofoveralllength = new ArrayList<Double>();
	// container for height
	static ArrayList<Double> overallheight = new ArrayList<Double>();
	// container for height max
	static ArrayList<Double> overallheightmax = new ArrayList<Double>();
	// container for height min
	static ArrayList<Double> overallheightmin = new ArrayList<Double>();

	static ArrayList<String> xvalueroof = new ArrayList<String>();
	static ArrayList<String> yvalueroof = new ArrayList<String>();
	static ArrayList<String> zvalueroof = new ArrayList<String>();

	static ArrayList<String> xvaluelinestring = new ArrayList<String>();
	static ArrayList<String> yvaluelinestring = new ArrayList<String>();
	static ArrayList<String> zvaluelinestring = new ArrayList<String>();

	static int num = 0;

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

	int relativeground;// index of ground surface
	int relativewall;// index of wall surface
	int relativeroof;// index of roof surface
	int relativepoint;// index of point
	int relativesolid;// index of solid surface
	int relativecurve;// index of curve
	int relativebuildingpart;// index of building part
	int relativeforwall;// index of point array in all wall
	int relativeforroof;// index of point array in all roof
	int relativeforground;// index of point array in all ground
	int relativelinestring;// index of linestring
	int relativelinestringpoint;// index of linestring point
	int relativeforlinestringpoint;// index of point array in all linestring
	int relativex3d;// index of appearance data start from 1

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

	public static int circulartest(double A, double per) {

		double T = Math.abs(4 * 22 / 7 * A / Math.pow(per, 2));
		int result;

		if (T <= 1 && T >= 0.9) {
			result = 1; // it's circular based on adms value
			System.out.println("it is circular,T= " + T);
		} else {
			result = 0; // it's rectangular based on adms value
			System.out.println("it is rectangular,T= " + T);
		}
		return result;
	}

	public static double[] perimax(ArrayList<String> xvalueground, ArrayList<String> yvalueground) {
		int totsides = xvalueground.size() - 1;
		double[] edge = new double[totsides];
		double per = 0;
		// calculate the perimeter
		for (int n = 0; n < totsides; n++) {
			edge[n] = Math.sqrt(Math
					.pow((Double.parseDouble(xvalueground.get(n + 1)) - Double.parseDouble(xvalueground.get(n))), 2)
					+ Math.pow((Double.parseDouble(yvalueground.get(n + 1)) - Double.parseDouble(yvalueground.get(n))),
							2));
			per += edge[n];
		}
		// calculate the maximum edge (length)
		double max = edge[0];
		double x0 = Double.parseDouble(xvalueground.get(0));
		double x1 = Double.parseDouble(xvalueground.get(1));
		double y0 = Double.parseDouble(yvalueground.get(0));
		double y1 = Double.parseDouble(yvalueground.get(1));

		for (int count = 1; count < totsides; count++) {
			if (edge[count] > max) {
				max = edge[count];
				x0 = Double.parseDouble(xvalueground.get(count));
				x1 = Double.parseDouble(xvalueground.get(count + 1));
				y0 = Double.parseDouble(yvalueground.get(count));
				y1 = Double.parseDouble(yvalueground.get(count + 1));
			}
		}

		return new double[] { per, max, x0, x1, y0, y1 };
	}
	
	public static double[] simplified(ArrayList<String> xvalueground, ArrayList<String> yvalueground) {
		System.out.println("==============start simplified======================");
		System.out.println("jumlah xgroundspec in function awal= "+xvalueground.size()); 
		ArrayList<String> indextoberemoved = new ArrayList<String>();
		ArrayList<String> xvaluemod = xvalueground;
		ArrayList<String> yvaluemod = yvalueground;
		ArrayList<String> newx = new ArrayList<String>();
		ArrayList<String> newy = new ArrayList<String>();
		int totsides = xvaluemod.size() - 1;
		double[] edge = new double[totsides];
		double per = 0;
		// calculate the perimeter
		for (int n = 0; n < totsides; n++) {
			edge[n] = Math.sqrt(Math
					.pow((Double.parseDouble(xvaluemod.get(n + 1)) - Double.parseDouble(xvaluemod.get(n))), 2)
					+ Math.pow((Double.parseDouble(yvaluemod.get(n + 1)) - Double.parseDouble(yvaluemod.get(n))),
							2));
			per += edge[n];
		}
		double maxtot=0;
		// calculate the maximum edge (length)
		for (int hit = 0; hit < totsides; hit++) {
		double max = edge[hit];
		
		double x1;
		double x2;
		double y1;
		double y2;

		double x0 = Double.parseDouble(xvaluemod.get(hit));
		double y0 = Double.parseDouble(yvaluemod.get(hit));
		
		if (hit+2-totsides==0)
		{ 
		x1 = Double.parseDouble(xvaluemod.get(hit+1));
		 x2 = Double.parseDouble(xvaluemod.get(0));
		 y1 = Double.parseDouble(yvaluemod.get(hit+1));
		 y2 = Double.parseDouble(yvaluemod.get(0));
		
		}
		else if (hit+1-totsides==0)
		{ 
		x1 = Double.parseDouble(xvaluemod.get(0));
		 x2 = Double.parseDouble(xvaluemod.get(1));
		 y1 = Double.parseDouble(yvaluemod.get(0));
		 y2 = Double.parseDouble(yvaluemod.get(1));
		
		}
		else{
		 x1 = Double.parseDouble(xvaluemod.get(hit+1));
		 x2 = Double.parseDouble(xvaluemod.get(hit+2));		
		 y1 = Double.parseDouble(yvaluemod.get(hit+1));
		 y2 = Double.parseDouble(yvaluemod.get(hit+2));
		}
		
		

		Double anglecalc = (Math.abs(Math.atan2(y0 - y1,x0 - x1)- Math.atan2(y2 - y1, x2 - x1))) / Math.PI * 180;
//		System.out.println("totsides = "+totsides);
//		System.out.println("anglecalc = "+anglecalc);
//		System.out.println("hit = "+hit);
//		System.out.println("x0 = "+x0);
//		System.out.println("y0 = "+y0);
//		System.out.println("x1 = "+x1);
//		System.out.println("y1 = "+y1);
//		System.out.println("x2 = "+x2);
//		System.out.println("y2 = "+y2);

		if (anglecalc<183 && anglecalc>177)
		{
			indextoberemoved.add(String.valueOf(hit+1));
		}
		}
		
		System.out.println("indextoberemovedsize= "+indextoberemoved.size());
		int removed= indextoberemoved.size();

		for (int n=0;n<removed;n++)
		{
			System.out.println("indextoberemovedvalue= "+indextoberemoved.get(n));
		System.out.println("removed x= "+xvaluemod.get(Integer.parseInt(indextoberemoved.get(n))-n));
		System.out.println("removed y= "+yvaluemod.get(Integer.parseInt(indextoberemoved.get(n))-n));
		
		xvaluemod.remove(Integer.parseInt(indextoberemoved.get(n))-n);
		yvaluemod.remove(Integer.parseInt(indextoberemoved.get(n))-n);
		
	}
		System.out.println("jumlah xgroundspec in function awal= "+xvalueground.size()); 
		newx.addAll(xvaluemod);
		newy.addAll(yvaluemod);
		if (newx.get(0)!=newx.get(newx.size()-1))
		{
			newx.add(newx.get(0));
			newy.add(newy.get(0));
		}
		
	int newtotal= newx.size();
	System.out.println("newtotal= "+newx.size());
		for (int n=0;n<newtotal;n++)
		{ System.out.println("n= "+n);
			System.out.println("list of newx= "+newx.get(n));
			System.out.println("list of newy= "+newy.get(n));
		}
		
		
System.out.println("maxtot= "+perimax(newx,newy)[1]);
System.out.println("x0= "+perimax(newx,newy)[2]);
System.out.println("y0= "+perimax(newx,newy)[4]);
System.out.println("x1= "+perimax(newx,newy)[3]);
System.out.println("y1= "+perimax(newx,newy)[5]);

double max=perimax(newx,newy)[1];
double x0=perimax(newx,newy)[2];
double x1=perimax(newx,newy)[3];
double y0=perimax(newx,newy)[4];
double y1=perimax(newx,newy)[5];
double xcentre=centroid(newx,newy)[0];
double ycentre=centroid(newx,newy)[1];
double A=Math.abs(centroid(newx,newy)[2]);




System.out.println("jumlah xgroundspec in function akhir= "+xvalueground.size()); 

System.out.println("==============stop simplified======================"); 
         return new double[] { per, max, x0, x1, y0, y1,xcentre,ycentre,A };
	}
	
	public static double angle(double max, double x0, double x1, double y0, double y1) {

		double xinitial = x0;
		double yinitial = y0;
		double x2 = x1;
		double y2 = y1;
		if (x0 > x1) {
			xinitial = x1;
			yinitial = y1;
			x2 = x0;
			y2 = y0;
		}

		double ymod = yinitial + max;
		double xmod = xinitial;
		double result = Math.atan2(ymod - yinitial, xmod - xinitial) - Math.atan2(y2 - yinitial, x2 - xinitial);
		double resultangle = result / Math.PI * 180;
		return resultangle;
	}

	public void savefile(JenaOWLModel jenaOwlModel, String filePath2) throws URISyntaxException {

		Collection errors = new ArrayList();
		jenaOwlModel.save(new URI("file:////" + filePath2.replace("\\", "/")), FileUtils.langXMLAbbrev, errors,
				jenaOwlModel.getOntModel());
		System.out.println("File saved with " + errors.size() + " errors.");
	}

	private void initIndexesForUniqueInstances() {
		relativeground = 1;
		relativewall = 1;
		relativeroof = 1;
		relativepoint = 1;
		relativesolid = 1;
		relativecurve = 1;
		relativebuildingpart = 1;
		relativeforwall = 0;
		relativeforroof = 0;
		relativeforground = 0;
		relativelinestring = 1;
		relativelinestringpoint = 1;
		relativeforlinestringpoint = 0;
		relativex3d = 1;
	}

	//on 4 july 2018, last on building 53
	public void startConversion() throws Exception {

		// int[] notpartial=
		// {/*110,109,107,70,36,81,49,111,62,06,117,116,19,99,106,22,67,85,94,20,93,73,*/80/*,5*/};
		// //71a and 5 some exception
		// int number=notpartial.length;
		for (int jumlah = 4; jumlah <= 4; jumlah++) // end 121
		{
			// String jumlahn=String.format("%02d", jumlah);
			String source = "D:/citygmllearn/denhaaggml/" + 10 + "_buildings" + jumlah + ".xml";
			//String source = "D:/citygmllearn/denhaaggml/49"+"_buildings"  + ".xml";
			String kbname = source.split("/")[3].split(".xml")[0] + ".owl";
			
			//for the berlin kb
			// String source= "D:/citygmllearn/CITYGML_Berlin/3910_5818.gml";
			// String kbname= "3910_5818.owl";

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

		m = jenaOwlModel.getOWLIndividual(
				"http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#m");

		xsdDouble = jenaOwlModel.getRDFSDatatypeByName("xsd:double");
	}

	private void convertEnvelope(JenaOWLModel jenaOwlModel, String kbname, Document doc, RDFIndividual citymodel) { // creating
																													// the
																													// envelope
																													// and
																													// the
																													// coordinate
																													// scope
																													// of
																													// the
																													// system
		NodeList envelopetag = doc.getElementsByTagName("gml:Envelope");
		RDFIndividual envelope = envelopeclass
				.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#Envelope001");

		// coordinate system
		for (int temp = 0; temp < envelopetag.getLength(); temp++) {

			Node nNode = envelopetag.item(temp);

			if (nNode.getNodeType() == Node.ELEMENT_NODE) {

				Element eElement = (Element) nNode;

				datacoordinate = eElement.getAttribute("srsName");
				System.out.println("datacoordinate : " + eElement.getAttribute("srsName"));

				citymodel.addPropertyValue(boundedBy, envelope);
				envelope.addPropertyValue(srsname, datacoordinate);

				RDFIndividual lowerpoint = pointclass.createRDFIndividual(
						"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#LowerPointBoundary");
				RDFIndividual upperpoint = pointclass.createRDFIndividual(
						"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#UpperPointBoundary");
				envelope.addPropertyValue(lowercorner, lowerpoint);
				envelope.addPropertyValue(uppercorner, upperpoint);

				RDFIndividual lowerboundpointscoordinate = coordinatesystemclass.createRDFIndividual(
						"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#LowerPointBoundary_Coordinates");
				RDFIndividual xlowerboundpoints = coordinateclass.createRDFIndividual(
						"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#x_LowerPointBoundary");
				RDFIndividual ylowerboundpoints = coordinateclass.createRDFIndividual(
						"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#y_LowerPointBoundary");
				RDFIndividual zlowerboundpoints = coordinateclass.createRDFIndividual(
						"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#z_LowerPointBoundary");
				RDFIndividual xlowerboundvalpoints = valueclass.createRDFIndividual(
						"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#V_x_LowerPointBoundary");
				RDFIndividual ylowerboundvalpoints = valueclass.createRDFIndividual(
						"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#V_y_LowerPointBoundary");
				RDFIndividual zlowerboundvalpoints = valueclass.createRDFIndividual(
						"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#V_z_LowerPointBoundary");

				lowerpoint.addPropertyValue(hascoordinatesystem, lowerboundpointscoordinate);
				lowerboundpointscoordinate.addPropertyValue(hasx, xlowerboundpoints);
				lowerboundpointscoordinate.addPropertyValue(hasy, ylowerboundpoints);
				lowerboundpointscoordinate.addPropertyValue(hasz, zlowerboundpoints);

				xlowerboundpoints.setPropertyValue(hasvalue, xlowerboundvalpoints);
				ylowerboundpoints.setPropertyValue(hasvalue, ylowerboundvalpoints);
				zlowerboundpoints.setPropertyValue(hasvalue, zlowerboundvalpoints);

				xlowerboundvalpoints.setPropertyValue(numval,
						jenaOwlModel.createRDFSLiteral(
								eElement.getElementsByTagName("gml:lowerCorner").item(0).getTextContent().split(" ")[0],
								xsdDouble));
				ylowerboundvalpoints.setPropertyValue(numval,
						jenaOwlModel.createRDFSLiteral(
								eElement.getElementsByTagName("gml:lowerCorner").item(0).getTextContent().split(" ")[1],
								xsdDouble));
				zlowerboundvalpoints.setPropertyValue(numval,
						jenaOwlModel.createRDFSLiteral(
								eElement.getElementsByTagName("gml:lowerCorner").item(0).getTextContent().split(" ")[2],
								xsdDouble));

				xlowerboundvalpoints.setPropertyValue(hasunit, m);
				ylowerboundvalpoints.setPropertyValue(hasunit, m);
				zlowerboundvalpoints.setPropertyValue(hasunit, m);

				RDFIndividual upperboundpointscoordinate = coordinatesystemclass.createRDFIndividual(
						"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#UpperPointBoundary_Coordinates");
				RDFIndividual xupperboundpoints = coordinateclass.createRDFIndividual(
						"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#x_UpperPointBoundary");
				RDFIndividual yupperboundpoints = coordinateclass.createRDFIndividual(
						"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#y_UpperPointBoundary");
				RDFIndividual zupperboundpoints = coordinateclass.createRDFIndividual(
						"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#z_UpperPointBoundary");
				RDFIndividual xupperboundvalpoints = valueclass.createRDFIndividual(
						"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#V_x_UpperPointBoundary");
				RDFIndividual yupperboundvalpoints = valueclass.createRDFIndividual(
						"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#V_y_UpperPointBoundary");
				RDFIndividual zupperboundvalpoints = valueclass.createRDFIndividual(
						"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#V_z_UpperPointBoundary");

				upperpoint.addPropertyValue(hascoordinatesystem, upperboundpointscoordinate);
				upperboundpointscoordinate.addPropertyValue(hasx, xupperboundpoints);
				upperboundpointscoordinate.addPropertyValue(hasy, yupperboundpoints);
				upperboundpointscoordinate.addPropertyValue(hasz, zupperboundpoints);

				xupperboundpoints.setPropertyValue(hasvalue, xupperboundvalpoints);
				yupperboundpoints.setPropertyValue(hasvalue, yupperboundvalpoints);
				zupperboundpoints.setPropertyValue(hasvalue, zupperboundvalpoints);

				// RDFSLiteral uxLiteral =
				// jenaOwlModel.createRDFSLiteral(eElement.getElementsByTagName("gml:upperCorner").item(0).getTextContent().split("
				// ")[0], xsdDouble);

				xupperboundvalpoints.setPropertyValue(numval,
						jenaOwlModel.createRDFSLiteral(
								eElement.getElementsByTagName("gml:upperCorner").item(0).getTextContent().split(" ")[0],
								xsdDouble));
				yupperboundvalpoints.setPropertyValue(numval,
						jenaOwlModel.createRDFSLiteral(
								eElement.getElementsByTagName("gml:upperCorner").item(0).getTextContent().split(" ")[1],
								xsdDouble));
				zupperboundvalpoints.setPropertyValue(numval,
						jenaOwlModel.createRDFSLiteral(
								eElement.getElementsByTagName("gml:upperCorner").item(0).getTextContent().split(" ")[2],
								xsdDouble));
				xupperboundvalpoints.setPropertyValue(hasunit, m);
				yupperboundvalpoints.setPropertyValue(hasunit, m);
				zupperboundvalpoints.setPropertyValue(hasunit, m);
			}
		}
	}

	public void doConversion(JenaOWLModel jenaOwlModel, String source, String kbname)
			throws URISyntaxException, ParserConfigurationException, SAXException, IOException {

		File fXmlFile = new File(source);
		DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
		DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
		Document doc = dBuilder.parse(fXmlFile);

		doc.getDocumentElement().normalize();

		System.out.println("Root element :" + doc.getDocumentElement().getNodeName());

		NodeList nListbuilding = doc.getElementsByTagName("bldg:Building");

		RDFIndividual citymodel = citymodelclass
				.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#CityModel001");
		citymodel.addPropertyValue(name, doc.getElementsByTagName("gml:name").item(0).getTextContent());

		convertEnvelope(jenaOwlModel, kbname, doc, citymodel);

		// for every building
		String i;

		initIndexesForUniqueInstances();

		for (int tempcount = 1; tempcount <= nListbuilding.getLength(); tempcount++) {
			i = String.format("%03d", tempcount);
			Node nNode2 = nListbuilding.item(tempcount - 1);
			if (nNode2.getNodeType() == Node.ELEMENT_NODE) {
				Element eElementspec = (Element) nNode2;
				NodeList nListpart = eElementspec.getElementsByTagName("bldg:BuildingPart");

				String iriidentifier = eElementspec.getAttribute("gml:id") + "_";

				RDFIndividual building = buildingclass.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/"
						+ kbname + "#Building" + eElementspec.getAttribute("gml:id"));
				citymodel.addPropertyValue(cityobjectmember, building);
				System.out.println("building" + i);

				RDFIndividual buildingcoordinate = coordinatesystemclass
						.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#Building_"
								+ eElementspec.getAttribute("gml:id") + "_Coordinates");
				RDFIndividual xpointcentre = coordinateclass
						.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#x_Building_"
								+ eElementspec.getAttribute("gml:id"));
				RDFIndividual ypointcentre = coordinateclass
						.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#y_Building_"
								+ eElementspec.getAttribute("gml:id"));
				RDFIndividual height = lengthclass.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/"+ kbname + "#EstimatedHeight_Building_" + eElementspec.getAttribute("gml:id"));
				//RDFIndividual length = lengthclass.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/"+ kbname + "#EstimatedLength_Building_" + eElementspec.getAttribute("gml:id"));
				//RDFIndividual width = lengthclass.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname+ "#EstimatedWidth_Building_" + eElementspec.getAttribute("gml:id"));
				//RDFIndividual angle = angleclass.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname+ "#EstimatedAngle_Building_" + eElementspec.getAttribute("gml:id"));
				RDFIndividual xvalcentre = valueclass.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/"
						+ kbname + "#V_x_Building_" + eElementspec.getAttribute("gml:id"));
				RDFIndividual yvalcentre = valueclass.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/"
						+ kbname + "#V_y_Building_" + eElementspec.getAttribute("gml:id"));
				RDFIndividual heightval = scalarvalueclass.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/"+ kbname + "#V_EstimatedHeight_Building_" + eElementspec.getAttribute("gml:id"));
				//RDFIndividual lengthval = scalarvalueclass.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/"+ kbname + "#V_EstimatedLength_Building_" + eElementspec.getAttribute("gml:id"));
				//RDFIndividual widthval = scalarvalueclass.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#V_EstimatedWidth_Building_" + eElementspec.getAttribute("gml:id"));
				//RDFIndividual angleval = scalarvalueclass.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/"+ kbname + "#V_EstimatedAngle_Building_" + eElementspec.getAttribute("gml:id"));

				building.addPropertyValue(measuredHeight, height);
				//building.addPropertyValue(haslength, length);
				//building.addPropertyValue(haslength, width);
				//building.addPropertyValue(hasangle, angle);
				building.addPropertyValue(hascoordinatesystem, buildingcoordinate);

				buildingcoordinate.addPropertyValue(hasx, xpointcentre);
				buildingcoordinate.addPropertyValue(hasy, ypointcentre);

				//length.setPropertyValue(hasvalue, lengthval);
				//width.setPropertyValue(hasvalue, widthval);
				//angle.setPropertyValue(hasvalue, angleval);

				xpointcentre.addPropertyValue(hasvalue, xvalcentre);
				ypointcentre.addPropertyValue(hasvalue, yvalcentre);
				height.addPropertyValue(hasvalue, heightval);

				xvalcentre.setPropertyValue(hasunit, m);
				yvalcentre.setPropertyValue(hasunit, m);
				heightval.setPropertyValue(hasunit, m);
				//lengthval.setPropertyValue(hasunit, m);
				//widthval.setPropertyValue(hasunit, m);
				building.addPropertyValue(id, eElementspec.getAttribute("gml:id"));
				building.addPropertyValue(name, eElementspec.getElementsByTagName("gml:name").item(0).getTextContent());

				// check if there is no building part
				if (nListpart.getLength() == 0) {
					// condition if there is no building parts here
					System.out.println(
							"===========================don't have building parts==============================");

					RDFIndividual rooftype = rooftypeclass.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/"
							+ kbname + "#RoofType_Building" + eElementspec.getAttribute("gml:id"));
					building.addPropertyValue(roofTyperelation, rooftype);
					rooftype.addPropertyValue(value,
							eElementspec.getElementsByTagName("bldg:roofType").item(0).getTextContent()); // if
					// there
					// is
					// any

					buildingGENattributeextraction(kbname, eElementspec, building);

					// take the data under appearance block of building and create the instance
					relativex3d = buildingAppearanceExtractionandMapping(jenaOwlModel, kbname, relativex3d,
							eElementspec, iriidentifier, building);

					createMultiCurveandCoordinatesInstances(jenaOwlModel, kbname, eElementspec, iriidentifier,
							building);

					// specific for solid
					NodeList nListsolid = eElementspec.getElementsByTagName("gml:Solid");
					for (int temp = 1; temp <= nListsolid.getLength(); temp++) {
						String min = null;

						i = String.format("%03d", relativesolid);
						RDFIndividual solid = solidclass.createRDFIndividual(
								"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + iriidentifier + "Solid" + i);
						building.addPropertyValue(lod2Solid, solid);

						compositesurface = compositesurfaceclass
								.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#"
										+ iriidentifier + "CompositeSurface" + i);
						solid.addPropertyValue(exterior, compositesurface);

						
						// specific for ground
						NodeList nListground = eElementspec.getElementsByTagName("bldg:GroundSurface");
						 System.out.println("size of nlistground :"+ nListground.getLength());
						for (int temp0 = 1; temp0 <= nListground.getLength(); temp0++) {

							Node nNode = nListground.item(temp0 - 1);

							// System.out.println("nlistpolygon :" +
							// nListpolygon.getLength());

							if (nNode.getNodeType() == Node.ELEMENT_NODE) {

								Element eElement = (Element) nNode;

								NodeList nListpolygon = eElement.getElementsByTagName("gml:Polygon");
								Node nNodepol = nListpolygon.item(0); // temp0
																		// change
																		// to
																		// ???
								Element eElementpol = (Element) nNodepol;
								i = String.format("%03d", relativeground);
								groundins = groundclass.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/"
										+ kbname + "#" + iriidentifier + "GroundSurface" + i);
								building.addPropertyValue(boundedBy, groundins);

								System.out.println("groundsurface= " + i);

								String ii = String.format("%03d", relativewall + relativeground + relativeroof - 2); // maybe
																														// wrong
								RDFIndividual multisurface = multisurfaceclass
										.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#"
												+ iriidentifier + "MultiSurface" + ii);
								groundins.addPropertyValue(lod2MultiSurface, multisurface);

								RDFIndividual polygon = polygonclass
										.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#"
												+ iriidentifier + "Polygon" + ii);
								multisurface.addPropertyValue(surfaceMember, polygon);
								compositesurface.addPropertyValue(surfaceMember, polygon);

								RDFIndividual linearring = ringclass
										.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#"
												+ iriidentifier + "LinearRing" + ii);
								polygon.addPropertyValue(exterior, linearring);
								polygon.addPropertyValue(id, eElementpol.getAttribute("gml:id"));

								int a = eElement.getElementsByTagName("gml:pos").getLength();

								if (a == 0) {
									extractBuildingGroundCoordinatePostlist(jenaOwlModel, kbname, iriidentifier,
											eElement, linearring);
									min = findingMinimumZValueinGroundSurface();
								}
								// System.out.println("gml number : " + a);

								else {

									//System.out.println("========================");
								//	System.out.println("the pos is applied");
									//System.out.println("========================");

									for (int x = 1; x <= a; x++) {

										String xaxis = eElement.getElementsByTagName("gml:pos").item(x - 1)
												.getTextContent().split(" ")[0];
										String yaxis = eElement.getElementsByTagName("gml:pos").item(x - 1)
												.getTextContent().split(" ")[1];
										String zaxis = eElement.getElementsByTagName("gml:pos").item(x - 1)
												.getTextContent().split(" ")[2];
										xvalueground.add(xaxis);
										yvalueground.add(yaxis);
										zvalueground.add(zaxis);

										createInstancesGroundBuildingPoints(jenaOwlModel, kbname, iriidentifier,
												linearring);
										
									}
									min = findingMinimumZValueinGroundSurface();
								}

								relativeground = relativeground + 1;

								// for the pure building w/o parts centroid
								
							System.out.println("circulartest result= "+circulartest(Math.abs(centroid(xgroundspec, ygroundspec)[2]),
										perimax(xgroundspec, ygroundspec)[0]));
							
								if (circulartest(Math.abs(centroid(xgroundspec, ygroundspec)[2]),
										perimax(xgroundspec, ygroundspec)[0]) == 1) // (A,P)
																					// if
																					// the
																					// shape
																					// is
																					// circle
								{
								//	RDFIndividual geodisk = Diskclass.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname+ "#" + iriidentifier + "Shape_Building_" + tempcount);
									
									RDFIndividual geodisk = Diskclass.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/"+ kbname + "#Shape_Building" + eElementspec.getAttribute("gml:id"));
									
									building.addPropertyValue(hassurfacegeometry, geodisk);
									Double Diameter = Math
											.sqrt(Math.abs(centroid(xgroundspec, ygroundspec)[2]) * 4 * 7 / 22);
									System.out.println("it is circle with the diameter around= " + Diameter);
									//lengthval.setPropertyValue(numval,jenaOwlModel.createRDFSLiteral(String.valueOf(Diameter), xsdDouble));
								} else {
									//RDFIndividual georectangle = Rectangleclass	.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname+ "#" + iriidentifier + "Shape_Building_" + tempcount);
									RDFIndividual georectangle = Rectangleclass.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname+ "#Shape_Building" + eElementspec.getAttribute("gml:id"));
									building.addPropertyValue(hassurfacegeometry, georectangle);
									
									System.out.println("jumlah xgroundspec=" +xgroundspec.size());
									
									Double lengthside = simplified(xgroundspec, ygroundspec)[1];
									System.out.println("jumlah xgroundspec2=" +xgroundspec.size());
									
									Double orientation = angle(simplified(xgroundspec, ygroundspec)[1],
											simplified(xgroundspec, ygroundspec)[2],simplified(xgroundspec, ygroundspec)[3],
											simplified(xgroundspec, ygroundspec)[4], simplified(xgroundspec, ygroundspec)[5]);
									
									System.out.println("jumlah xgroundspec3=" +xgroundspec.size());
									
									Double area=simplified(xgroundspec, ygroundspec)[8];
									Double widthside = area / lengthside;
									
							
									for(int d=0;d<xgroundspec.size();d++)
									{
									System.out.println(xgroundspec.get(d));
									System.out.println(ygroundspec.get(d));
									
									}
									System.out.println(
											"area= " + area);
									System.out.println(
											"it is square with length= " + lengthside + " and width= " + widthside);
									System.out.println("it has angle= " + orientation);
									//Double widthside = Math.abs(Coveralldown) / maxlength;
									
									Double abslength;
									Double abswidthside;
									Double absorientation;
									
									if (lengthside>widthside)
									{
										abslength=lengthside;
										abswidthside=widthside;
										absorientation=orientation;
										
									}
									
									else
									{
										abslength=widthside;
										abswidthside=lengthside;
										if(orientation>90)
										{
										absorientation=orientation-90;
										}
										else
										{
											absorientation=orientation+90;
										}
									}
									
									//angleval.setPropertyValue(numval,jenaOwlModel.createRDFSLiteral(String.valueOf(orientation), xsdDouble));
									//lengthval.setPropertyValue(numval,jenaOwlModel.createRDFSLiteral(String.valueOf(abslength), xsdDouble));
									//widthval.setPropertyValue(numval,jenaOwlModel.createRDFSLiteral(String.valueOf(abswidthside), xsdDouble));

								}
Double xcentre=simplified(xgroundspec,ygroundspec)[6];
Double ycentre=simplified(xgroundspec,ygroundspec)[7];
								xvalcentre.setPropertyValue(numval, jenaOwlModel.createRDFSLiteral(
										String.valueOf(xcentre), xsdDouble));
								yvalcentre.setPropertyValue(numval, jenaOwlModel.createRDFSLiteral(
										String.valueOf(ycentre), xsdDouble));

								System.out.println("xmidcalc= "+xcentre);
								System.out.println("ymidcalc= "+ycentre);
								
							}
						}

						xgroundspec.clear(); // clear the list for the next
												// ground surface
						ygroundspec.clear();// clear the list for the next
											// ground surface
						zgroundspec.clear();

						createWallLayerandDetailsInstances(jenaOwlModel, kbname, eElementspec, iriidentifier, building);

						String max = createRoofLayerandDetailsInstances(jenaOwlModel, kbname, eElementspec,
								iriidentifier, building);//// using the variable
															//// max because
															//// it's also
															//// accompanied by
															//// the function to
															//// calculate the
															//// maximum z of
															//// the roof
															//// surface

						zroofspec.clear();
						zroofspec.clear();
						relativesolid = relativesolid + 1;

						System.out.println("the min of a building= " + min);
						System.out.println("the max of a building= " + max);

						Double buildingheight = Double.parseDouble(max) - Double.parseDouble(min);
						heightval.setPropertyValue(numval,
								jenaOwlModel.createRDFSLiteral(String.valueOf(buildingheight), xsdDouble));
						System.out.println("the height of a building= " + buildingheight);
					}

				}

				// the condition if there is some building parts
				// if there is some buildingpart inside
				else

				{
					System.out
							.println("===================have building parts here====================================");
					Double maxlength = null;
					Double maxheight = null;
					Double minheight = null;
					Double x0length = null;
					Double x1length = null;
					Double y0length = null;
					Double y1length = null;
					int indexmaxlength = 0;
					for (int temp = 1; temp <= nListpart.getLength(); temp++) {

						Node nNode1 = nListpart.item(temp - 1);
						Element eElementspec2 = (Element) nNode1;// nNode2
																	// change
																	// if
																	// there
																	// is
																	// part
						String iriidentifierpart = eElementspec2.getAttribute("gml:id") + "_";
						i = String.format("%03d", relativebuildingpart);
						RDFIndividual buildingpart = buildingpartclass
								.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname
										+ "#BuildingPart" + eElementspec2.getAttribute("gml:id"));
						building.addPropertyValue(consistsofbuildingpart, buildingpart);
						RDFIndividual rooftype = rooftypeclass
								.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname
										+ "#RoofType_BuildingPart" + eElementspec2.getAttribute("gml:id"));
						buildingpart.addPropertyValue(roofTyperelation, rooftype);
						rooftype.addPropertyValue(value,
								eElementspec2.getElementsByTagName("bldg:roofType").item(0).getTextContent()); // if
						// there
						// isn't
						// any
						buildingpart.addPropertyValue(id, eElementspec2.getAttribute("gml:id"));
						buildingpart.addPropertyValue(name,
								eElementspec2.getElementsByTagName("gml:name").item(0).getTextContent());

						buildingpartGENattributeextraction(kbname, eElementspec2, buildingpart);

						relativex3d = buildingPartAppearanceExtractionandMapping(jenaOwlModel, kbname, relativex3d,
								eElementspec2, iriidentifierpart, buildingpart);

						// specific for multicurve
						createMultiCurveandCoordinatesInstances(jenaOwlModel, kbname, eElementspec2, iriidentifierpart,
								buildingpart);

						
						// specific for solid
						NodeList nListsolid = eElementspec2.getElementsByTagName("gml:Solid");
						

						for (int tempsolid = 1; tempsolid <= nListsolid.getLength(); tempsolid++) {
							i = String.format("%03d", relativesolid);
							RDFIndividual solid = solidclass
									.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#"
											+ iriidentifierpart + "Solid" + i);
							buildingpart.addPropertyValue(lod2Solid, solid);

							compositesurface = compositesurfaceclass
									.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#"
											+ iriidentifierpart + "CompositeSurface" + i);
							solid.addPropertyValue(exterior, compositesurface);

							// specific for ground
							String min = null;
							NodeList nListground = eElementspec2.getElementsByTagName("bldg:GroundSurface");
							for (int temp0 = 1; temp0 <= nListground.getLength(); temp0++) {

								Node nNode = nListground.item(temp0 - 1);

								// System.out.println("\nCurrent Element :"
								// + nNode.getNodeName()+temp);

								if (nNode.getNodeType() == Node.ELEMENT_NODE) {
									Element eElement = (Element) nNode;

									NodeList nListpolygon = eElement.getElementsByTagName("gml:Polygon");
									Node nNodepol = nListpolygon.item(0);
									Element eElementpol = (Element) nNodepol;
									i = String.format("%03d", relativeground);
									groundins = groundclass
											.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname
													+ "#" + iriidentifierpart + "GroundSurface" + i);

									buildingpart.addPropertyValue(boundedBy, groundins);

									System.out.println("groundsurface= " + i);

									String ii = String.format("%03d", relativewall + relativeground + relativeroof - 2); // maybe
																															// wrong
									RDFIndividual multisurface = multisurfaceclass
											.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname
													+ "#" + iriidentifierpart + "MultiSurface" + ii);
									groundins.addPropertyValue(lod2MultiSurface, multisurface);

									RDFIndividual polygon = polygonclass
											.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname
													+ "#" + iriidentifierpart + "Polygon" + ii);
									multisurface.addPropertyValue(surfaceMember, polygon);
									compositesurface.addPropertyValue(surfaceMember, polygon);

									RDFIndividual linearring = ringclass
											.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname
													+ "#" + iriidentifierpart + "LinearRing" + ii);
									polygon.addPropertyValue(exterior, linearring);

									polygon.addPropertyValue(id, eElementpol.getAttribute("gml:id"));

									int a = eElement.getElementsByTagName("gml:pos").getLength();

									if (a == 0) {
										extractBuildingPartGroundCoordinatePostlist(jenaOwlModel, kbname,
												iriidentifierpart, eElement, linearring);

										min = findingMinimumZValueinGroundSurface();
									}

									else // pos applied
									{
										//System.out.println("========================");
										//System.out.println("the pos is applied");
										//System.out.println("========================");
										for (int x = 1; x <= a; x++) // a=
																		// amount
																		// of
																		// vertices
										{
											String xaxis = eElement.getElementsByTagName("gml:pos").item(x - 1)
													.getTextContent().split(" ")[0];
											String yaxis = eElement.getElementsByTagName("gml:pos").item(x - 1)
													.getTextContent().split(" ")[1];
											String zaxis = eElement.getElementsByTagName("gml:pos").item(x - 1)
													.getTextContent().split(" ")[2];
											xvalueground.add(xaxis);
											yvalueground.add(yaxis);
											zvalueground.add(zaxis);

											createInstancesGroundBuildingPartPoints(jenaOwlModel, kbname,
													iriidentifierpart, linearring);

										}
										min = findingMinimumZValueinGroundSurface();
									}

									relativeground = relativeground + 1;

									if (circulartest(Math.abs(centroid(xgroundspec, ygroundspec)[2]),
											perimax(xgroundspec, ygroundspec)[0]) == 1) // (A,P)
																						// if
																						// it
																						// is
																						// circular
									{
										RDFIndividual geodisk = Diskclass
												.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname
														+ "#Shape_Building" + eElementspec.getAttribute("gml:id")
														+ "Part_" + eElementspec2.getAttribute("gml:id"));
										buildingpart.addPropertyValue(hassurfacegeometry, geodisk);
										Double Diameter = Math
												.sqrt(Math.abs(centroid(xgroundspec, ygroundspec)[2]) * 4 * 7 / 22);
										System.out.println("it is circle with the diameter around= " + Diameter);
										//lengthval.setPropertyValue(numval,jenaOwlModel.createRDFSLiteral(String.valueOf(Diameter), xsdDouble));
										overalllength.add(Diameter);
										xofoveralllength.add(0.0); 
										xofoveralllength.add(0.0);
										yofoveralllength.add(0.0); 
										yofoveralllength.add(0.0); 
									} 
									
									else {
										RDFIndividual georectangle = Rectangleclass
												.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname
														+ "#Shape_Building" + eElementspec.getAttribute("gml:id")+ "Part_" + eElementspec2.getAttribute("gml:id"));
										
										
										buildingpart.addPropertyValue(hassurfacegeometry, georectangle);
										System.out.println("===================================== ");
										System.out.println("list of xgroundspec size= "+xgroundspec.size());
										for(int ab=0;ab<xgroundspec.size();ab++)
										{
											System.out.println("list of xgroundspec= "+xgroundspec.get(ab));
										}
										
										//Double lengthside = perimax(xgroundspec, ygroundspec)[1]; // get the maximum length
										Double lengthside = simplified(xgroundspec, ygroundspec)[1]; // get the maximum length
										
										System.out.println("lengthside= "+lengthside);
										Double orientation = angle(simplified(xgroundspec, ygroundspec)[1],
												simplified(xgroundspec, ygroundspec)[2],
												simplified(xgroundspec, ygroundspec)[3],
												simplified(xgroundspec, ygroundspec)[4],
												simplified(xgroundspec, ygroundspec)[5]);
										
	Double area = simplified(xgroundspec, ygroundspec)[8]; // get the maximum length
										
										System.out.println("areaside= "+area);
										
										Double widthside = area / lengthside;
										System.out.println(
												"it is square with length= " + lengthside + " and width= " + widthside);
										System.out.println("it has angle= " + orientation);
										// lengthval.addPropertyValue(numval,
										// String.valueOf(lengthside));
										overalllength.add(lengthside);
										xofoveralllength.add(perimax(xgroundspec, ygroundspec)[2]);
										xofoveralllength.add(perimax(xgroundspec, ygroundspec)[3]);
										yofoveralllength.add(perimax(xgroundspec, ygroundspec)[4]);
										yofoveralllength.add(perimax(xgroundspec, ygroundspec)[5]);

										// widthval.addPropertyValue(numval,
										// String.valueOf(widthside));
										// angleval.addPropertyValue(numval,
										// String.valueOf(orientation));
									}

									// to calculate the central for every
									// building part

									x1.add(centroid(xgroundspec, ygroundspec)[0]);
									y1.add(centroid(xgroundspec, ygroundspec)[1]);
									A1.add(Math.abs(centroid(xgroundspec, ygroundspec)[2]));

								}

							}
							
							
							xgroundbuild.addAll(xgroundspec);
							ygroundbuild.addAll(ygroundspec);
							zgroundbuild.addAll(zgroundspec);
							zgroundbuildid.addAll(zgroundid);
							System.out.println("vertices in 1 buildingpart =" + zgroundspec.size());
							xgroundspec.clear(); // clear the list for the
													// next ground surface
							ygroundspec.clear();// clear the list for the
												// next ground surface
							zgroundspec.clear();
							zgroundid.clear();

							// specific for wall
							createWallLayerandDetailsInstances(jenaOwlModel, kbname, eElementspec2, iriidentifierpart,
									buildingpart);

							// specific for roof
							String max = createRoofLayerandDetailsInstances(jenaOwlModel, kbname, eElementspec2,
									iriidentifierpart, buildingpart); // using
																		// the
																		// variable
																		// max
																		// because
																		// it's
																		// also
																		// accompanied
																		// by
																		// the
																		// function
																		// to
																		// calculate
																		// the
																		// maximum
																		// z of
																		// the
																		// roof
																		// surface
							zroofspec.clear();
							relativesolid = relativesolid + 1;
							System.out.println("the min of a building= " + min);
							System.out.println("the max of a building= " + max);

							Double buildingheight = Double.parseDouble(max) - Double.parseDouble(min);
							// heightval.addPropertyValue(numval,
							// String.valueOf(buildingheight));
							overallheight.add(buildingheight);
							overallheightmax.add(Double.parseDouble(max));
							overallheightmin.add(Double.parseDouble(min));
							System.out.println("the height of a building in part= " + buildingheight);
						}

						// find the max length
						maxlength = overalllength.get(0);

						System.out.println("sizeofvertices= " + overalllength.size());
						for (int b = 1; b < overalllength.size(); b++) {
							if (overalllength.get(b) > maxlength) {
								maxlength = overalllength.get(b);
								indexmaxlength = b;
							}
						}

						// find the maxheight
						maxheight = overallheightmax.get(0);

						for (int b = 1; b < overallheightmax.size(); b++) {
							if (overallheightmax.get(b) > maxheight) {
								maxheight = overallheightmax.get(b);

							}
						}
						// find the minheight
						minheight = overallheightmin.get(0);

						for (int b = 1; b < overallheightmin.size(); b++) {
							if (overallheightmin.get(b) < minheight) {
								minheight = overallheightmin.get(b);
							}
						}

						relativebuildingpart = relativebuildingpart + 1;

					}

					// length for the combined building parts
					x0length = xofoveralllength.get(2 * indexmaxlength);
					x1length = xofoveralllength.get(2 * indexmaxlength + 1);
					y0length = yofoveralllength.get(2 * indexmaxlength);
					y1length = yofoveralllength.get(2 * indexmaxlength + 1);

					System.out.println("vertices in 1 building =" + zgroundbuild.size());
					String min = zgroundbuild.get(0);

					for (int b = 1; b < zgroundbuild.size(); b++) {
						if (Double.parseDouble(zgroundbuild.get(b)) < Double.parseDouble(min)) {
							min = zgroundbuild.get(b);

						}
					}
					System.out.println("min z in 1 building =" + min);

					for (int b = 0; b < zgroundbuild.size(); b++) {

						if (zgroundbuild.get(b).equals(min)) {
							System.out.println("index =" + b);
							System.out.println("id building part=" + zgroundbuildid.get(b)); // check if the base is only 1 part(if id always the same)
							if(b<xgroundbuild.size())
							{
							xoverallground.add(xgroundbuild.get(b));
							System.out.println("x axis= " + xgroundbuild.get(b));
							yoverallground.add(ygroundbuild.get(b));
							System.out.println("y axis= " + ygroundbuild.get(b));
							}
						}

					}
					
					

//					if (circulartest(Math.abs(centroid(xoverallground, yoverallground)[2]),
//							perimax(xoverallground, yoverallground)[0]) == 1) // (A,P)
//																				// if
//																				// it
//																				// is
//																				// circular
//					{
//						RDFIndividual geodisk = Diskclass.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/"
//								+ kbname + "#Shape_Building" + eElementspec.getAttribute("gml:id"));
//						building.addPropertyValue(hassurfacegeometry, geodisk);
//
//					} 
				//	else {
						RDFIndividual georectangle = Rectangleclass
								.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname
										+ "#Shape_Building" + eElementspec.getAttribute("gml:id"));
						building.addPropertyValue(hassurfacegeometry, georectangle);

				//	}	
					
					for (int ind = 0; ind < xgroundbuild.size(); ind++) {				
				
						if (Double.valueOf(ygroundbuild.get(ind)) - y0length != 0
								&& Double.valueOf(xgroundbuild.get(ind)) - x0length != 0
								&& Double.valueOf(xgroundbuild.get(ind)) - x1length != 0
								&& Double.valueOf(ygroundbuild.get(ind)) - y1length != 0) {

							Double anglecalc = (Math.atan2(Double.valueOf(ygroundbuild.get(ind)) - y0length,Double.valueOf(xgroundbuild.get(ind)) - x0length)- Math.atan2(y1length - y0length, x1length - x0length)) / Math.PI * 180;
							Double add0 = Math.sqrt(Math.pow((Double.valueOf(xgroundbuild.get(ind)) - x0length), 2)
									+ Math.pow((Double.valueOf(ygroundbuild.get(ind)) - y0length), 2));
							Double add1 = Math.sqrt(Math.pow((Double.valueOf(xgroundbuild.get(ind)) - x1length), 2)
									+ Math.pow((Double.valueOf(ygroundbuild.get(ind)) - y1length), 2));
							
				
							if (anglecalc < 180.5 && anglecalc > 179.5) {

								maxlength = maxlength + add0;

								x0length = Double.valueOf(xgroundbuild.get(ind));
								y0length = Double.valueOf(ygroundbuild.get(ind));
							} else if (anglecalc < 0.5 && anglecalc > -0.5 && add0 > maxlength) {

								maxlength = maxlength + add1;

								x1length = Double.valueOf(xgroundbuild.get(ind));
								y1length = Double.valueOf(ygroundbuild.get(ind));
							}
						}
					
					}
					
					
					Double orientation = angle(maxlength, x0length, x1length, y0length, y1length);
					// angleval.addPropertyValue(numval,
					// String.valueOf(orientation));

					Double overallheight = maxheight - minheight;

				
					
								
					xgroundbuild.clear();
					ygroundbuild.clear();
					zgroundbuild.clear();
					zgroundbuildid.clear();
					overalllength.clear();
					xofoveralllength.clear();
					yofoveralllength.clear();
					overallheightmax.clear();
					overallheightmin.clear();

					// calculate the combined centroid of all building parts
					Double Cxoverallup = 0.0;
					Double Cyoverallup = 0.0;
					Double Coveralldown = 0.0;
					for (int tot = 0; tot < nListpart.getLength(); tot++) {
						// System.out.println("x1= "+x1.get(tot));
						// System.out.println("y1= "+y1.get(tot));
						// System.out.println("A1= "+A1.get(tot));
						Cxoverallup = Cxoverallup + (x1.get(tot) * A1.get(tot));
						Cyoverallup = Cyoverallup + (y1.get(tot) * A1.get(tot));
						Coveralldown = Coveralldown + A1.get(tot);

					}
					double Cxoverall = Cxoverallup / Coveralldown;
					double Cyoverall = Cyoverallup / Coveralldown;
					Double widthside = Math.abs(Coveralldown) / maxlength;
					
					Double abslength;
					Double abswidthside;
					Double absorientation;
					
					if (maxlength>widthside)
					{
						abslength=maxlength;
						abswidthside=widthside;
						absorientation=orientation;
						
					}
					
					else
					{
						abslength=widthside;
						abswidthside=maxlength;
						if(orientation>90)
						{
						absorientation=orientation-90;
						}
						else
						{
							absorientation=orientation+90;
						}
					}
					System.out.println("length= "+abslength);
					System.out.println("width= "+abswidthside);
					
						
					//lengthval.setPropertyValue(numval,jenaOwlModel.createRDFSLiteral(String.valueOf(abslength), xsdDouble));

					//angleval.addPropertyValue(numval,jenaOwlModel.createRDFSLiteral(String.valueOf(absorientation), xsdDouble));

					heightval.addPropertyValue(numval,jenaOwlModel.createRDFSLiteral(String.valueOf(overallheight), xsdDouble));
			
					
					if (Double.isNaN(Cxoverall) && Double.isNaN(Cyoverall)) {
						System.out.println("not needed");
					} else {
						xvalcentre.addPropertyValue(numval,
								jenaOwlModel.createRDFSLiteral(String.valueOf(Cxoverall), xsdDouble));
						yvalcentre.addPropertyValue(numval,
								jenaOwlModel.createRDFSLiteral(String.valueOf(Cyoverall), xsdDouble));
						//widthval.setPropertyValue(numval,jenaOwlModel.createRDFSLiteral(String.valueOf(abswidthside), xsdDouble));

						System.out.println(("centroid of building with parts= " + Cxoverall + " and " + Cyoverall));
					}
					x1.clear();
					y1.clear();
					A1.clear();

				}

			}

		}

		xvaluelinestring.clear();
		yvaluelinestring.clear();
		zvaluelinestring.clear();
		xvalueroof.clear();
		xvaluewall.clear();
		xvalueground.clear();
		yvalueroof.clear();
		yvaluewall.clear();
		yvalueground.clear();
		zvalueroof.clear();
		zvaluewall.clear();
		zvalueground.clear();
		xoverallground.clear();
		yoverallground.clear();

	}// comment in wrong

	/**
	 * @param jenaOwlModel
	 * @param kbname
	 * @param eElementspec2
	 * @param iriidentifierpart
	 * @param buildingpart
	 * @return
	 */
	public String createRoofLayerandDetailsInstances(JenaOWLModel jenaOwlModel, String kbname, Element eElementspec2,
			String iriidentifierpart, RDFIndividual buildingpart) {
		String i;
		NodeList nListroof = eElementspec2.getElementsByTagName("bldg:RoofSurface");
		String max = null;
		for (int temp2 = 1; temp2 <= nListroof.getLength(); temp2++) {

			Node nNode = nListroof.item(temp2 - 1);

			if (nNode.getNodeType() == Node.ELEMENT_NODE) {
				Element eElement = (Element) nNode;
				NodeList nListpolygon = eElement.getElementsByTagName("gml:Polygon");
				Node nNodepol = nListpolygon.item(0);
				Element eElementpol = (Element) nNodepol;
				i = String.format("%03d", relativeroof);
				roofins = roofclass.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#"
						+ iriidentifierpart + "RoofSurface" + i);
				// building.addPropertyValue(boundedBy,
				// roofins);

				buildingpart.addPropertyValue(boundedBy, roofins);

				NodeList Nattrib2 = eElement.getElementsByTagName("gen:doubleAttribute");

				int amount2 = Nattrib2.getLength();
				// System.out.println ("the property of
				// building=" +
				// eElementspec.getAttribute("name"));
				System.out.println("size of attrib roof= " + amount2);
				for (int q = 0; q < amount2; q++) {
					Node nNodex = Nattrib2.item(q);
					Element eElementx = (Element) nNodex;

					if (eElementx.getTagName() == "gen:doubleAttribute"
							&& eElementx.getParentNode().getNodeName() == "bldg:RoofSurface") {
						// System.out.println ("the
						// property of roof=" +
						// eElementx.getAttribute("name"));
						RDFIndividual genatrribute = doubleattributetype
								.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#"
										+ iriidentifierpart + eElementx.getAttribute("name") + "_RoofSurface" + i);
						roofins.addPropertyValue(doubleattribute, genatrribute);
						genatrribute.addPropertyValue(value,
								eElementx.getElementsByTagName("gen:value").item(0).getTextContent());

					}

				}

				// compositesurface.addPropertyValue(surfaceMember,
				// roofins);

				System.out.println("roofsurface= " + i);

				String ii = String.format("%03d", relativewall + relativeground + relativeroof - 2); // maybe
																										// wrong

				RDFIndividual multisurface = multisurfaceclass
						.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#"
								+ iriidentifierpart + "MultiSurface" + ii);
				roofins.addPropertyValue(lod2MultiSurface, multisurface);

				RDFIndividual polygon = polygonclass.createRDFIndividual(
						"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + iriidentifierpart + "Polygon" + ii);
				multisurface.addPropertyValue(surfaceMember, polygon);
				compositesurface.addPropertyValue(surfaceMember, polygon);
				RDFIndividual linearring = ringclass.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/"
						+ kbname + "#" + iriidentifierpart + "LinearRing" + ii);
				polygon.addPropertyValue(exterior, linearring);
				polygon.addPropertyValue(id, eElementpol.getAttribute("gml:id"));
				// System.out.println("\nCurrent Element
				// :" + nNode.getNodeName()+temp);

				int a = eElement.getElementsByTagName("gml:pos").getLength();

				// System.out.println("gml number : " +
				// a);

				if (a == 0) {
					extractRoofCoordinatesPostlist(jenaOwlModel, kbname, iriidentifierpart, eElement, linearring);
					max = findingMaximumZValueinRoofSurface();
					System.out.println("roofhighest= " + max);

				}

				else {
					//System.out.println("========================");
					//System.out.println("the pos is applied");
					//System.out.println("========================");
					for (int x = 1; x <= a; x++) {
						// System.out.println("gml
						// pos"+x+": " +
						// eElement.getElementsByTagName("gml:pos").item(x-1).getTextContent());
						String xaxis = eElement.getElementsByTagName("gml:pos").item(x - 1).getTextContent()
								.split(" ")[0];
						String yaxis = eElement.getElementsByTagName("gml:pos").item(x - 1).getTextContent()
								.split(" ")[1];
						String zaxis = eElement.getElementsByTagName("gml:pos").item(x - 1).getTextContent()
								.split(" ")[2];
						xvalueroof.add(xaxis);
						yvalueroof.add(yaxis);
						zvalueroof.add(zaxis);

						createInstancesRoofBuildingPoints(jenaOwlModel, kbname, iriidentifierpart, linearring);
					}
					max = findingMaximumZValueinRoofSurface();
				}
				relativeroof = 1 + relativeroof;

			}
		}
		return max;
	}

	/**
	 * @param jenaOwlModel
	 * @param kbname
	 * @param eElementspec2
	 * @param iriidentifierpart
	 * @param buildingpart
	 */
	private void createMultiCurveandCoordinatesInstances(JenaOWLModel jenaOwlModel, String kbname,
			Element eElementspec2, String iriidentifierpart, RDFIndividual buildingpart) {
		String i;
		NodeList nListcurve = eElementspec2.getElementsByTagName("gml:MultiCurve");
		for (int tempcurve = 1; tempcurve <= nListcurve.getLength(); tempcurve++) {
			i = String.format("%03d", relativecurve);
			RDFIndividual multicurve = multicurveclass.createRDFIndividual(
					"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + iriidentifierpart + "MultiCurve" + i);
			buildingpart.addPropertyValue(lod2TerrainIntersection, multicurve);

			NodeList nListlinestring = eElementspec2.getElementsByTagName("gml:LineString");
			for (int temp0 = 1; temp0 <= nListlinestring.getLength(); temp0++) {
				Node nNode = nListlinestring.item(temp0 - 1);

				String ii = String.format("%03d", relativelinestring);
				RDFIndividual linestring = linestringclass.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/"
						+ kbname + "#" + iriidentifierpart + "LineString" + ii);
				multicurve.addPropertyValue(curveMember, linestring);

				if (nNode.getNodeType() == Node.ELEMENT_NODE) {
					Element eElement = (Element) nNode;

					int a = eElement.getElementsByTagName("gml:pos").getLength();
					// poslist applied
					if (a == 0) {
						extractLinestringCoordinatePostlist(jenaOwlModel, kbname, iriidentifierpart, linestring,
								eElement);
					}
					// System.out.println("gml number : " +
					// a);

					else {
						//System.out.println("========================");
						//System.out.println("the pos is applied");
						//System.out.println("========================");
						for (int x = 1; x <= a; x++) {
							String xaxis = eElement.getElementsByTagName("gml:pos").item(x - 1).getTextContent()
									.split(" ")[0];
							String yaxis = eElement.getElementsByTagName("gml:pos").item(x - 1).getTextContent()
									.split(" ")[1];
							String zaxis = eElement.getElementsByTagName("gml:pos").item(x - 1).getTextContent()
									.split(" ")[2];
							xvaluelinestring.add(xaxis);
							yvaluelinestring.add(yaxis);
							zvaluelinestring.add(zaxis);

							createLineStringCoordinateInstance(jenaOwlModel, kbname, iriidentifierpart, linestring);
						}
					}
					relativelinestring = relativelinestring + 1;
				}

			}
			relativecurve = relativecurve + 1;

		}
	}

	/**
	 * @param jenaOwlModel
	 * @param kbname
	 * @param eElementspec2
	 * @param iriidentifierpart
	 * @param buildingpart
	 */
	private void createWallLayerandDetailsInstances(JenaOWLModel jenaOwlModel, String kbname, Element eElementspec2,
			String iriidentifierpart, RDFIndividual buildingpart) {
		String i;
		NodeList nListwall = eElementspec2.getElementsByTagName("bldg:WallSurface");
		for (int temp1 = 1; temp1 <= nListwall.getLength(); temp1++) {

			Node nNode = nListwall.item(temp1 - 1);

			if (nNode.getNodeType() == Node.ELEMENT_NODE) {
				Element eElement = (Element) nNode;
				NodeList nListpolygon = eElement.getElementsByTagName("gml:Polygon");
				Node nNodepol = nListpolygon.item(0);
				Element eElementpol = (Element) nNodepol;
				i = String.format("%03d", relativewall);
				// System.out.println("\nCurrent Element
				// :" + nNode.getNodeName()+temp);

				wallins = wallclass.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#"
						+ iriidentifierpart + "WallSurface" + i);

				buildingpart.addPropertyValue(boundedBy, wallins);

				//System.out.println("wallsurface= " + i);

				String ii = String.format("%03d", relativewall + relativeground + relativeroof - 2); // maybe
																										// wrong
				RDFIndividual multisurface = multisurfaceclass
						.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#"
								+ iriidentifierpart + "MultiSurface" + ii);
				wallins.addPropertyValue(lod2MultiSurface, multisurface);
				RDFIndividual polygon = polygonclass.createRDFIndividual(
						"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + iriidentifierpart + "Polygon" + ii);
				multisurface.addPropertyValue(surfaceMember, polygon);
				compositesurface.addPropertyValue(surfaceMember, polygon);
				RDFIndividual linearring = ringclass.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/"
						+ kbname + "#" + iriidentifierpart + "LinearRing" + ii);
				polygon.addPropertyValue(exterior, linearring);
				polygon.addPropertyValue(id, eElementpol.getAttribute("gml:id"));

				int a = eElement.getElementsByTagName("gml:pos").getLength();

				// System.out.println("gml number : " +
				// a);
				if (a == 0) {
					extractWallCoordinatePostlist(jenaOwlModel, kbname, iriidentifierpart, eElement, linearring);
				} else {
					//System.out.println("========================");
					//System.out.println("the pos is applied");
					//System.out.println("========================");

					for (int x = 1; x <= a; x++) {

						String xaxis = eElement.getElementsByTagName("gml:pos").item(x - 1).getTextContent()
								.split(" ")[0];
						String yaxis = eElement.getElementsByTagName("gml:pos").item(x - 1).getTextContent()
								.split(" ")[1];
						String zaxis = eElement.getElementsByTagName("gml:pos").item(x - 1).getTextContent()
								.split(" ")[2];
						xvaluewall.add(xaxis);
						yvaluewall.add(yaxis);
						zvaluewall.add(zaxis);

						createInstancesWallBuildingPoints(jenaOwlModel, kbname, iriidentifierpart, linearring);
					}
				}
				relativewall = relativewall + 1;
				// System.out.println("=========================================="
				// );

			}
		}
	}

	/**
	 * @return
	 */
	private String findingMaximumZValueinRoofSurface() {
		String max;
		max = zroofspec.get(0);
		System.out.println("sizeofroof= " + zroofspec.size());
		for (int b = 1; b < zroofspec.size(); b++) {
			if (Double.parseDouble(zroofspec.get(b)) > Double.parseDouble(max)) {
				max = zroofspec.get(b);
			}
		}
		return max;
	}

	/**
	 * @return
	 */
	private String findingMinimumZValueinGroundSurface() {
		String min;
		min = zgroundspec.get(0);
		System.out.println("sizeofground= " + zgroundspec.size());

		for (int b = 1; b < zgroundspec.size(); b++) {
			if (Double.parseDouble(zgroundspec.get(b)) < Double.parseDouble(min)) {
				min = zgroundspec.get(b);
			}
		}
		System.out.println("groundlowest= " + min);
		return min;
	}

	/**
	 * @param jenaOwlModel
	 * @param kbname
	 * @param iriidentifier
	 * @param eElement
	 * @param linearring
	 */
	private void extractBuildingGroundCoordinatePostlist(JenaOWLModel jenaOwlModel, String kbname, String iriidentifier,
			Element eElement, RDFIndividual linearring) {
		System.out.println("========================");
		System.out.println("the posList is applied");
		System.out.println("========================");

		String lines0[] = eElement.getElementsByTagName("gml:posList").item(0).getTextContent().trim().split("\n");
		System.out.println("number lineblock poslist data (lines0length): " + lines0.length);
		int totalpointinblock = 0;
		totalpointinblock = postlistreaderground(lines0, totalpointinblock);
		System.out.println("totalpointinblock: " + totalpointinblock);

		for (int pt = 0; pt < totalpointinblock / 3; pt++) // 21
															// need
															// to
															// be
															// changed
		{

			createInstancesGroundBuildingPoints(jenaOwlModel, kbname, iriidentifier, linearring);
		}
	}

	/**
	 * @param jenaOwlModel
	 * @param kbname
	 * @param iriidentifierpart
	 * @param eElement
	 * @param linearring
	 */
	private void extractBuildingPartGroundCoordinatePostlist(JenaOWLModel jenaOwlModel, String kbname,
			String iriidentifierpart, Element eElement, RDFIndividual linearring) {
		System.out.println("========================");
		System.out.println("the posList is applied");
		System.out.println("========================");

		String lines0[] = eElement.getElementsByTagName("gml:posList").item(0).getTextContent().trim().split("\n");
		System.out.println("number lineblock poslist data (lines0length): " + lines0.length);
		int totalpointinblock = 0;

		totalpointinblock = postlistreaderground(lines0, totalpointinblock);

		System.out.println("totalpointinblock: " + totalpointinblock);

		for (int pt = 0; pt < totalpointinblock / 3; pt++) // 21
															// need
															// to
															// be
															// changed
		{

			createInstancesGroundBuildingPartPoints(jenaOwlModel, kbname, iriidentifierpart, linearring);
		}
	}

	/**
	 * @param jenaOwlModel
	 * @param kbname
	 * @param iriidentifierpart
	 * @param eElement
	 * @param linearring
	 */
	private void extractRoofCoordinatesPostlist(JenaOWLModel jenaOwlModel, String kbname, String iriidentifierpart,
			Element eElement, RDFIndividual linearring) {
		System.out.println("========================");
		System.out.println("the posList is applied");
		System.out.println("========================");

		String lines0[] = eElement.getElementsByTagName("gml:posList").item(0).getTextContent().trim().split("\n");
		// System.out.println(
		// "number lineblock poslist data (lines0length): " + lines0.length);
		int totalpointinblock = 0;

		totalpointinblock = postlistreaderroof(lines0, totalpointinblock);

		for (int pt = 0; pt < totalpointinblock / 3; pt++) // 21
															// need
															// to
															// be
															// changed
		{

			createInstancesRoofBuildingPoints(jenaOwlModel, kbname, iriidentifierpart, linearring);
		}
	}

	/**
	 * @param jenaOwlModel
	 * @param kbname
	 * @param iriidentifierpart
	 * @param eElement
	 * @param linearring
	 */
	private void extractWallCoordinatePostlist(JenaOWLModel jenaOwlModel, String kbname, String iriidentifierpart,
			Element eElement, RDFIndividual linearring) {
		System.out.println("========================");
		System.out.println("the posList is applied");
		System.out.println("========================");

		String lines0[] = eElement.getElementsByTagName("gml:posList").item(0).getTextContent().trim().split("\n");
		System.out.println("number lineblock poslist data (lines0length): " + lines0.length);
		int totalpointinblock = 0;

		totalpointinblock = postlistreaderwall(lines0, totalpointinblock);

		System.out.println("totalpointinblock: " + totalpointinblock);

		for (int pt = 0; pt < totalpointinblock / 3; pt++) // 21
															// need
															// to
															// be
															// changed
		{

			createInstancesWallBuildingPoints(jenaOwlModel, kbname, iriidentifierpart, linearring);

		}
	}

	/**
	 * @param jenaOwlModel
	 * @param kbname
	 * @param iriidentifier
	 * @param linearring
	 */
	private void createInstancesRoofBuildingPoints(JenaOWLModel jenaOwlModel, String kbname, String iriidentifier,
			RDFIndividual linearring) {
		String i;
		i = String.format("%03d", relativepoint); // or
													// relativepoint??
		RDFIndividual point = pointclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + iriidentifier + "Point" + i);
		linearring.addPropertyValue(contains, point);
		//System.out.println("point= " + i);

		RDFIndividual pointscoordinate = coordinatesystemclass
				.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + iriidentifier + "Point_"
						+ i + "_Coordinates");
		RDFIndividual xpoints = coordinateclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + iriidentifier + "x_Point_" + i);
		RDFIndividual ypoints = coordinateclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + iriidentifier + "y_Point_" + i);
		RDFIndividual zpoints = coordinateclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + iriidentifier + "z_Point_" + i);
		RDFIndividual xvalpoints = valueclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#V_" + iriidentifier + "x_Point_" + i);
		RDFIndividual yvalpoints = valueclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#V_" + iriidentifier + "y_Point_" + i);
		RDFIndividual zvalpoints = valueclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#V_" + iriidentifier + "z_Point_" + i);

		point.addPropertyValue(hascoordinatesystem, pointscoordinate);
		pointscoordinate.addPropertyValue(hasx, xpoints);
		pointscoordinate.addPropertyValue(hasy, ypoints);
		pointscoordinate.addPropertyValue(hasz, zpoints);

		xpoints.setPropertyValue(hasvalue, xvalpoints);
		ypoints.setPropertyValue(hasvalue, yvalpoints);
		zpoints.setPropertyValue(hasvalue, zvalpoints);

		xvalpoints.setPropertyValue(numval, jenaOwlModel.createRDFSLiteral(xvalueroof.get(relativeforroof), xsdDouble));
		yvalpoints.setPropertyValue(numval, jenaOwlModel.createRDFSLiteral(yvalueroof.get(relativeforroof), xsdDouble));
		zvalpoints.setPropertyValue(numval, jenaOwlModel.createRDFSLiteral(zvalueroof.get(relativeforroof), xsdDouble));
		xvalpoints.setPropertyValue(hasunit, m);
		yvalpoints.setPropertyValue(hasunit, m);
		zvalpoints.setPropertyValue(hasunit, m);

		zroofspec.add(zvalueroof.get(relativeforroof));
		relativeforroof = relativeforroof + 1;
		relativepoint = relativepoint + 1;
	}

	/**
	 * @param jenaOwlModel
	 * @param kbname
	 * @param iriidentifier
	 * @param linearring
	 */
	private void createInstancesWallBuildingPoints(JenaOWLModel jenaOwlModel, String kbname, String iriidentifier,
			RDFIndividual linearring) {
		String i;
		i = String.format("%03d", relativepoint);
		RDFIndividual point = pointclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + iriidentifier + "Point" + i);

		//System.out.println("point= " + i);
		linearring.addPropertyValue(contains, point);
		RDFIndividual pointscoordinate = coordinatesystemclass
				.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + iriidentifier + "Point_"
						+ i + "_Coordinates");
		RDFIndividual xpoints = coordinateclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + iriidentifier + "x_Point_" + i);
		RDFIndividual ypoints = coordinateclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + iriidentifier + "y_Point_" + i);
		RDFIndividual zpoints = coordinateclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + iriidentifier + "z_Point_" + i);
		RDFIndividual xvalpoints = valueclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#V_" + iriidentifier + "x_Point_" + i);
		RDFIndividual yvalpoints = valueclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#V_" + iriidentifier + "y_Point_" + i);
		RDFIndividual zvalpoints = valueclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#V_" + iriidentifier + "z_Point_" + i);

		point.addPropertyValue(hascoordinatesystem, pointscoordinate);
		pointscoordinate.addPropertyValue(hasx, xpoints);
		pointscoordinate.addPropertyValue(hasy, ypoints);
		pointscoordinate.addPropertyValue(hasz, zpoints);

		xpoints.setPropertyValue(hasvalue, xvalpoints);
		ypoints.setPropertyValue(hasvalue, yvalpoints);
		zpoints.setPropertyValue(hasvalue, zvalpoints);

		xvalpoints.setPropertyValue(numval, jenaOwlModel.createRDFSLiteral(xvaluewall.get(relativeforwall), xsdDouble));
		yvalpoints.setPropertyValue(numval, jenaOwlModel.createRDFSLiteral(yvaluewall.get(relativeforwall), xsdDouble));
		zvalpoints.setPropertyValue(numval, jenaOwlModel.createRDFSLiteral(zvaluewall.get(relativeforwall), xsdDouble));
		xvalpoints.setPropertyValue(hasunit, m);
		yvalpoints.setPropertyValue(hasunit, m);
		zvalpoints.setPropertyValue(hasunit, m);
		relativeforwall = relativeforwall + 1;
		relativepoint = relativepoint + 1;
	}

	/**
	 * @param jenaOwlModel
	 * @param kbname
	 * @param iriidentifierpart
	 * @param linearring
	 *            have difference in zgroundid.add(...) in buildingparts
	 */
	private void createInstancesGroundBuildingPartPoints(JenaOWLModel jenaOwlModel, String kbname,
			String iriidentifierpart, RDFIndividual linearring) {
		String i;
		i = String.format("%03d", relativepoint);
		RDFIndividual point = pointclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + iriidentifierpart + "Point" + i);

		System.out.println("point= " + i);
		linearring.addPropertyValue(contains, point);

		RDFIndividual pointscoordinate = coordinatesystemclass
				.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + iriidentifierpart
						+ "Point_" + i + "_Coordinates");
		RDFIndividual xpoints = coordinateclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + iriidentifierpart + "x_Point_" + i);
		RDFIndividual ypoints = coordinateclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + iriidentifierpart + "y_Point_" + i);
		RDFIndividual zpoints = coordinateclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + iriidentifierpart + "z_Point_" + i);
		RDFIndividual xvalpoints = valueclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#V_" + iriidentifierpart + "x_Point_" + i);
		RDFIndividual yvalpoints = valueclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#V_" + iriidentifierpart + "y_Point_" + i);
		RDFIndividual zvalpoints = valueclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#V_" + iriidentifierpart + "z_Point_" + i);

		point.addPropertyValue(hascoordinatesystem, pointscoordinate);
		pointscoordinate.addPropertyValue(hasx, xpoints);
		pointscoordinate.addPropertyValue(hasy, ypoints);
		pointscoordinate.addPropertyValue(hasz, zpoints);

		xpoints.setPropertyValue(hasvalue, xvalpoints);
		ypoints.setPropertyValue(hasvalue, yvalpoints);
		zpoints.setPropertyValue(hasvalue, zvalpoints);

		xvalpoints.setPropertyValue(numval,
				jenaOwlModel.createRDFSLiteral(xvalueground.get(relativeforground), xsdDouble));
		yvalpoints.setPropertyValue(numval,
				jenaOwlModel.createRDFSLiteral(yvalueground.get(relativeforground), xsdDouble));
		zvalpoints.setPropertyValue(numval,
				jenaOwlModel.createRDFSLiteral(zvalueground.get(relativeforground), xsdDouble));
		xvalpoints.setPropertyValue(hasunit, m);
		yvalpoints.setPropertyValue(hasunit, m);
		zvalpoints.setPropertyValue(hasunit, m);
		xgroundspec.add(xvalueground.get(relativeforground));
		ygroundspec.add(yvalueground.get(relativeforground));
		zgroundspec.add(zvalueground.get(relativeforground));
		zgroundid.add(relativebuildingpart);
		// System.out.println("gml
		// pos"+x+": " +
		// eElement.getElementsByTagName("gml:pos").item(x-1).getTextContent());
		relativeforground = relativeforground + 1;
		relativepoint = relativepoint + 1;
	}

	/**
	 * @param jenaOwlModel
	 * @param kbname
	 * @param iriidentifier
	 * @param linearring
	 */
	private void createInstancesGroundBuildingPoints(JenaOWLModel jenaOwlModel, String kbname, String iriidentifier,
			RDFIndividual linearring) {
		String i;
		i = String.format("%03d", relativepoint);
		RDFIndividual point = pointclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + iriidentifier + "Point" + i);

		System.out.println("point= " + i);
		linearring.addPropertyValue(contains, point);

		RDFIndividual pointscoordinate = coordinatesystemclass
				.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + iriidentifier + "Point_"
						+ i + "_Coordinates");
		RDFIndividual xpoints = coordinateclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + iriidentifier + "x_Point_" + i);
		RDFIndividual ypoints = coordinateclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + iriidentifier + "y_Point_" + i);
		RDFIndividual zpoints = coordinateclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + iriidentifier + "z_Point_" + i);
		RDFIndividual xvalpoints = valueclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#V_" + iriidentifier + "x_Point_" + i);
		RDFIndividual yvalpoints = valueclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#V_" + iriidentifier + "y_Point_" + i);
		RDFIndividual zvalpoints = valueclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#V_" + iriidentifier + "z_Point_" + i);

		point.addPropertyValue(hascoordinatesystem, pointscoordinate);
		pointscoordinate.addPropertyValue(hasx, xpoints);
		pointscoordinate.addPropertyValue(hasy, ypoints);
		pointscoordinate.addPropertyValue(hasz, zpoints);

		xpoints.setPropertyValue(hasvalue, xvalpoints);
		ypoints.setPropertyValue(hasvalue, yvalpoints);
		zpoints.setPropertyValue(hasvalue, zvalpoints);

		xvalpoints.setPropertyValue(numval,
				jenaOwlModel.createRDFSLiteral(xvalueground.get(relativeforground), xsdDouble));
		yvalpoints.setPropertyValue(numval,
				jenaOwlModel.createRDFSLiteral(yvalueground.get(relativeforground), xsdDouble));
		zvalpoints.setPropertyValue(numval,
				jenaOwlModel.createRDFSLiteral(zvalueground.get(relativeforground), xsdDouble));
		xvalpoints.setPropertyValue(hasunit, m);
		yvalpoints.setPropertyValue(hasunit, m);
		zvalpoints.setPropertyValue(hasunit, m);
		// System.out.println("gml pos"+x+":
		// " +
		// eElement.getElementsByTagName("gml:pos").item(x-1).getTextContent());
		xgroundspec.add(xvalueground.get(relativeforground));
		ygroundspec.add(yvalueground.get(relativeforground));
		zgroundspec.add(zvalueground.get(relativeforground));

		
		relativeforground = relativeforground + 1;
		relativepoint = relativepoint + 1;
	}

	/**
	 * @param jenaOwlModel
	 * @param kbname
	 * @param iriidentifier
	 * @param linestring
	 * @param eElement
	 */
	private void extractLinestringCoordinatePostlist(JenaOWLModel jenaOwlModel, String kbname, String iriidentifier,
			RDFIndividual linestring, Element eElement) {
		System.out.println("========================");
		System.out.println("the posList is applied");
		System.out.println("========================");

		String lines0[] = eElement.getElementsByTagName("gml:posList").item(0).getTextContent().trim().split("\n");
		System.out.println("number lineblock poslist data (lines0length): " + lines0.length);
		int totalpointinblock = 0;
		totalpointinblock = postlistreaderlinestring(lines0, totalpointinblock);
		System.out.println("totalpointinblock: " + totalpointinblock);

		for (int pt = 0; pt < totalpointinblock / 3; pt++) // 21
															// need
															// to
															// be
															// changed
		{

			createLineStringCoordinateInstance(jenaOwlModel, kbname, iriidentifier, linestring);
		}
	}

	/**
	 * @param jenaOwlModel
	 * @param kbname
	 * @param iriidentifier
	 * @param linestring
	 */
	private void createLineStringCoordinateInstance(JenaOWLModel jenaOwlModel, String kbname, String iriidentifier,
			RDFIndividual linestring) {
		String i;
		i = String.format("%03d", relativelinestringpoint);
		RDFIndividual point = pointclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + iriidentifier + "PointLineString" + i);

		//System.out.println("pointLineString= " + i);
		linestring.addPropertyValue(contains, point);

		RDFIndividual pointscoordinate = coordinatesystemclass
				.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + iriidentifier
						+ "PointLineString_" + i + "_Coordinates");
		RDFIndividual xpoints = coordinateclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + iriidentifier + "x_PointLineString_" + i);
		RDFIndividual ypoints = coordinateclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + iriidentifier + "y_PointLineString_" + i);
		RDFIndividual zpoints = coordinateclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + iriidentifier + "z_PointLineString_" + i);
		RDFIndividual xvalpoints = valueclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#V_" + iriidentifier + "x_PointLineString_" + i);
		RDFIndividual yvalpoints = valueclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#V_" + iriidentifier + "y_PointLineString_" + i);
		RDFIndividual zvalpoints = valueclass.createRDFIndividual(
				"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#V_" + iriidentifier + "z_PointLineString_" + i);

		point.addPropertyValue(hascoordinatesystem, pointscoordinate);
		pointscoordinate.addPropertyValue(hasx, xpoints);
		pointscoordinate.addPropertyValue(hasy, ypoints);
		pointscoordinate.addPropertyValue(hasz, zpoints);

		xpoints.setPropertyValue(hasvalue, xvalpoints);
		ypoints.setPropertyValue(hasvalue, yvalpoints);
		zpoints.setPropertyValue(hasvalue, zvalpoints);

		xvalpoints.setPropertyValue(numval,
				jenaOwlModel.createRDFSLiteral(xvaluelinestring.get(relativeforlinestringpoint), xsdDouble));
		yvalpoints.setPropertyValue(numval,
				jenaOwlModel.createRDFSLiteral(yvaluelinestring.get(relativeforlinestringpoint), xsdDouble));
		zvalpoints.setPropertyValue(numval,
				jenaOwlModel.createRDFSLiteral(zvaluelinestring.get(relativeforlinestringpoint), xsdDouble));
		xvalpoints.setPropertyValue(hasunit, m);
		yvalpoints.setPropertyValue(hasunit, m);
		zvalpoints.setPropertyValue(hasunit, m);
		// System.out.println("gml pos"+x+":
		// " +
		// eElement.getElementsByTagName("gml:pos").item(x-1).getTextContent());
		relativeforlinestringpoint = relativeforlinestringpoint + 1;
		relativelinestringpoint = relativelinestringpoint + 1;
	}

	private int buildingPartAppearanceExtractionandMapping(JenaOWLModel jenaOwlModel, String kbname, int relativex3d,
			Element eElementspec2, String iriidentifierpart, RDFIndividual buildingpart) {
		String i;
		RDFIndividual appearance = appearanceclass.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/"
				+ kbname + "#Appearance_BuildingPart" + eElementspec2.getAttribute("gml:id"));
		buildingpart.addPropertyValue(appearancerelation, appearance);

		// for appearance
		NodeList nListx3dmaterial = eElementspec2.getElementsByTagName("app:X3DMaterial");
		for (int tempx3d = 1; tempx3d <= nListx3dmaterial.getLength(); tempx3d++) {
			System.out.println("tempx3d= " + tempx3d);
			System.out.println("value of 3d material= " + nListx3dmaterial.getLength());
			Node nNodemat = nListx3dmaterial.item(tempx3d - 1);

			if (nNodemat.getNodeType() == Node.ELEMENT_NODE) {
				Element eElementsmat = (Element) nNodemat;
				i = String.format("%03d", relativex3d);
				RDFIndividual x3dmaterial = x3dclass.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/"
						+ kbname + "#" + iriidentifierpart + "X3DMaterial" + i);
				appearance.addPropertyValue(surfacedatamemberrelation, x3dmaterial);
				RDFIndividual intensity = double0and1.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/"
						+ kbname + "#" + iriidentifierpart + "Intensity_X3DMaterial" + i);
				x3dmaterial.addPropertyValue(ambientintensity, intensity);
				RDFIndividual intensityvalue = scalarvalueclass
						.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#V_"
								+ iriidentifierpart + "Intensity_X3DMaterial" + i);
				intensity.addPropertyValue(hasvalue, intensityvalue);
				intensityvalue.addPropertyValue(numval, jenaOwlModel.createRDFSLiteral(
						eElementsmat.getElementsByTagName("app:ambientIntensity").item(0).getTextContent(), xsdDouble));
				RDFIndividual diffusecolorins = color.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/"
						+ kbname + "#" + iriidentifierpart + "DiffuseColor_X3DMaterial" + i);
				x3dmaterial.addPropertyValue(diffuseColor, diffusecolorins);
				RDFIndividual diffusecolorvalue = scalarvalueclass
						.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#V_"
								+ iriidentifierpart + "DiffuseColor_X3DMaterial" + i);
				diffusecolorins.addPropertyValue(hasvalue, diffusecolorvalue);
				diffusecolorvalue.addPropertyValue(numval,
						eElementsmat.getElementsByTagName("app:diffuseColor").item(0).getTextContent());
				//System.out.println("value of diffusecolor= "	+ eElementsmat.getElementsByTagName("app:diffuseColor").item(0).getTextContent());

				NodeList nListapptarget = eElementsmat.getElementsByTagName("app:target");
				for (int temp0 = 1; temp0 <= nListapptarget.getLength(); temp0++) {
					String targetvalue = eElementsmat.getElementsByTagName("app:target").item(temp0 - 1)
							.getTextContent();
					x3dmaterial.addPropertyValue(target, targetvalue);
					//System.out.println("value of target uri= " + targetvalue);
				}
			}
			relativex3d = relativex3d + 1;
		}
		return relativex3d;
	}

	private int buildingAppearanceExtractionandMapping(JenaOWLModel jenaOwlModel, String kbname, int relativex3d,
			Element eElementspec, String iriidentifier, RDFIndividual building) {
		String i;
		RDFIndividual appearance = appearanceclass.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/"
				+ kbname + "#Appearance_Building" + eElementspec.getAttribute("gml:id"));
		building.addPropertyValue(appearancerelation, appearance);

		NodeList nListx3dmaterial = eElementspec.getElementsByTagName("app:X3DMaterial");
		for (int tempx3d = 1; tempx3d <= nListx3dmaterial.getLength(); tempx3d++) {
			//System.out.println("tempx3d= " + tempx3d);
		//	System.out.println("value of 3d material= " + nListx3dmaterial.getLength());
			Node nNodemat = nListx3dmaterial.item(tempx3d - 1);

			if (nNodemat.getNodeType() == Node.ELEMENT_NODE) {
				Element eElementsmat = (Element) nNodemat;
				i = String.format("%03d", relativex3d);
				RDFIndividual x3dmaterial = x3dclass.createRDFIndividual(
						"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + iriidentifier + "X3DMaterial" + i);
				appearance.addPropertyValue(surfacedatamemberrelation, x3dmaterial);
				RDFIndividual intensity = double0and1.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/"
						+ kbname + "#" + iriidentifier + "Intensity_X3DMaterial" + i);
				x3dmaterial.addPropertyValue(ambientintensity, intensity);
				RDFIndividual intensityvalue = scalarvalueclass
						.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#V_" + iriidentifier
								+ "Intensity_X3DMaterial" + i);
				intensity.addPropertyValue(hasvalue, intensityvalue);
				intensityvalue.addPropertyValue(numval, jenaOwlModel.createRDFSLiteral(
						eElementsmat.getElementsByTagName("app:ambientIntensity").item(0).getTextContent(), xsdDouble));
				RDFIndividual diffusecolorins = color.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/"
						+ kbname + "#" + iriidentifier + "DiffuseColor_X3DMaterial" + i);
				x3dmaterial.addPropertyValue(diffuseColor, diffusecolorins);
				RDFIndividual diffusecolorvalue = scalarvalueclass
						.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#V_" + iriidentifier
								+ "DiffuseColor_X3DMaterial" + i);
				diffusecolorins.addPropertyValue(hasvalue, diffusecolorvalue);
				diffusecolorvalue.addPropertyValue(numval,
						eElementsmat.getElementsByTagName("app:diffuseColor").item(0).getTextContent());
				System.out.println("value of diffusecolor= "
						+ eElementsmat.getElementsByTagName("app:diffuseColor").item(0).getTextContent());

				NodeList nListapptarget = eElementsmat.getElementsByTagName("app:target");
				for (int temp0 = 1; temp0 <= nListapptarget.getLength(); temp0++) {

					String targetvalue = eElementsmat.getElementsByTagName("app:target").item(temp0 - 1)
							.getTextContent();
					x3dmaterial.addPropertyValue(target, targetvalue);
				//	System.out.println("value of target uri= " + targetvalue);

				}
			}
			relativex3d = relativex3d + 1;
		}
		return relativex3d;
	}

	public void buildingpartGENattributeextraction(String kbname, Element eElementspec2, RDFIndividual buildingpart) {
		NodeList Nattrib = eElementspec2.getElementsByTagName("gen:doubleAttribute");

		int amount = Nattrib.getLength();
		// System.out.println ("the property of building=" +
		// eElementspec.getAttribute("name"));
		System.out.println("size of attrib= " + amount);
		for (int q = 0; q < amount; q++) {
			Node nNodex = Nattrib.item(q);
			Element eElementx = (Element) nNodex;

			if (eElementx.getTagName() == "gen:doubleAttribute"
					&& eElementx.getParentNode().getNodeName() == "bldg:BuildingPart") {
				// System.out.println ("the property of
				// building part=" +
				// eElementx.getAttribute("name"));
				RDFIndividual genatrribute = doubleattributetype.createRDFIndividual(
						"http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#" + eElementx.getAttribute("name")
								+ "_BuildingPart" + eElementspec2.getAttribute("gml:id"));
				buildingpart.addPropertyValue(doubleattribute, genatrribute);
				genatrribute.addPropertyValue(value,
						eElementx.getElementsByTagName("gen:value").item(0).getTextContent());

			}

		}
	}

	public void buildingGENattributeextraction(String kbname, Element eElementspec1, RDFIndividual building) {
		NodeList Nattrib = eElementspec1.getElementsByTagName("gen:doubleAttribute");

		int amount = Nattrib.getLength();
		// System.out.println ("the property of building=" +
		// eElementspec.getAttribute("name"));
		System.out.println("size of attrib= " + amount);
		for (int q = 0; q < amount; q++) {
			Node nNodex = Nattrib.item(q);
			Element eElementx = (Element) nNodex;

			if (eElementx.getTagName() == "gen:doubleAttribute"
					&& eElementx.getParentNode().getNodeName() == "bldg:Building") {
				// System.out.println ("the property of
				// building=" + eElementx.getAttribute("name"));
				RDFIndividual genatrribute = doubleattributetype
						.createRDFIndividual("http://www.theworldavatar.com/kb/nld/thehague/buildings/" + kbname + "#"
								+ eElementx.getAttribute("name") + "_Building" + eElementspec1.getAttribute("gml:id"));
				building.addPropertyValue(doubleattribute, genatrribute);
				genatrribute.addPropertyValue(value,
						eElementx.getElementsByTagName("gen:value").item(0).getTextContent());

			}

		}
	}

	public int postlistreaderlinestring(String[] lines0, int totalpointinblock) {
		for (int x = 0; x < lines0.length; x++) {
			String lines[] = lines0[x].split(" ");
			System.out.println("lines0[x]= " + lines0[x]);
			for (int yy = 0; yy < lines.length / 3; yy++) {
				System.out.println("number: " + num);
				System.out.println("lines[yy]= " + lines[yy]);
				// System.out.println("poslist
				// data: "+ x + "---"
				// +lines[x]);
				// item(0) because in every
				// surface there's only 1
				// postlist
				System.out.println("number of poslist data (lineslength): " + lines.length);
				String xaxis = lines0[x].split(" ")[3 * num];
				System.out.println("x: " + xaxis);
				String yaxis = lines0[x].split(" ")[1 + 3 * num];
				System.out.println("y: " + yaxis);
				String zaxis = lines0[x].split(" ")[2 + 3 * num];
				System.out.println("z: " + zaxis);
				xvaluelinestring.add(xaxis);
				yvaluelinestring.add(yaxis);
				zvaluelinestring.add(zaxis);
				// item(0) because in every
				// surface there's only 1
				// postlist
				num = num + 1;
			}
			num = 0;
			totalpointinblock = totalpointinblock + lines.length;
		}
		return totalpointinblock;
	}

	public int postlistreaderroof(String[] lines0, int totalpointinblock) {
		for (int x = 0; x < lines0.length; x++) {
			String lines[] = lines0[x].split(" ");
			// System.out.println("lines0[x]=
			// "+lines0[x]);
			for (int yy = 0; yy < lines.length / 3; yy++) {
				// System.out.println("number:
				// "+num);
				// System.out.println("lines[yy]=
				// "+lines[yy]);

				// item(0) because in every
				// surface there's only 1
				// postlist
				System.out.println("number of poslist data (lineslength): " + lines.length);
				String xaxis = lines0[x].split(" ")[3 * num];
				System.out.println("x: " + xaxis);
				String yaxis = lines0[x].split(" ")[1 + 3 * num];
				System.out.println("y: " + yaxis);
				String zaxis = lines0[x].split(" ")[2 + 3 * num];
				System.out.println("z: " + zaxis);
				xvalueroof.add(xaxis);
				yvalueroof.add(yaxis);
				zvalueroof.add(zaxis);
				// item(0) because in every
				// surface there's only 1
				// postlist
				num = num + 1;
			}
			num = 0;
			totalpointinblock = totalpointinblock + lines.length;
		}
		return totalpointinblock;
	}

	public int postlistreaderwall(String[] lines0, int totalpointinblock) {
		for (int x = 0; x < lines0.length; x++) {
			String lines[] = lines0[x].split(" ");
			System.out.println("lines0[x]= " + lines0[x]);
			for (int yy = 0; yy < lines.length / 3; yy++) {
				System.out.println("number: " + num);
				System.out.println("lines[yy]= " + lines[yy]);
				// System.out.println("poslist
				// data: "+ x + "---"
				// +lines[x]);
				// item(0) because in every
				// surface there's only 1
				// postlist
				System.out.println("number of poslist data (lineslength): " + lines.length);
				String xaxis = lines0[x].split(" ")[3 * num];
				System.out.println("x: " + xaxis);
				String yaxis = lines0[x].split(" ")[1 + 3 * num];
				System.out.println("y: " + yaxis);
				String zaxis = lines0[x].split(" ")[2 + 3 * num];
				System.out.println("z: " + zaxis);
				xvaluewall.add(xaxis);
				yvaluewall.add(yaxis);
				zvaluewall.add(zaxis);
				// item(0) because in every
				// surface there's only 1
				// postlist
				num = num + 1;

			}
			num = 0;
			totalpointinblock = totalpointinblock + lines.length;
		}
		return totalpointinblock;
	}

	public int postlistreaderground(String[] lines0, int totalpointinblock) {
		for (int x = 0; x < lines0.length; x++) {
			String lines[] = lines0[x].split(" ");
			System.out.println("lines0[x]= " + lines0[x]);
			for (int yy = 0; yy < lines.length / 3; yy++) {
				System.out.println("number: " + num);

				// System.out.println("poslist
				// data: "+ x + "---"
				// +lines[x]);
				// item(0) because in every
				// surface there's only 1
				// postlist
				System.out.println("number of poslist data (lineslength): " + lines.length);
				String xaxis = lines0[x].split(" ")[3 * num];
				System.out.println("x: " + xaxis);
				String yaxis = lines0[x].split(" ")[1 + 3 * num];
				System.out.println("y: " + yaxis);
				String zaxis = lines0[x].split(" ")[2 + 3 * num];
				System.out.println("z: " + zaxis);
				xvalueground.add(xaxis);
				yvalueground.add(yaxis);
				zvalueground.add(zaxis);
				// item(0) because in every
				// surface there's only 1
				// postlist
				num = num + 1;

			}
			num = 0;
			totalpointinblock = totalpointinblock + lines.length;
		}
		return totalpointinblock;
	}

	public static void main(String[] args) throws Exception {
		System.out.println("Starting Process");
		CitygmlkbDH converter = new CitygmlkbDH();
		converter.startConversion();

	}
}
