package uk.ac.cam.cares.jps.powsys.envisualization;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.ListIterator;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

@WebServlet(urlPatterns = { "/ENVisualization/createLineJS", "/ENVisualization/createKMLFile/*", "/ENVisualization/getKMLFile/*" })
public class ENVisualization extends JPSHttpServlet {
	
	private Document doc;
	private Element root;
	private Logger logger = LoggerFactory.getLogger(ENVisualization.class);
	
	/**
	 * Create a KML object.
	 */
	public  ENVisualization() {
		try {
			DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
			DocumentBuilder builder = factory.newDocumentBuilder();
			doc = builder.newDocument();
			Element kml = doc.createElementNS("http://www.opengis.net/kml/2.2", "kml");
			doc.appendChild(kml);
			root = doc.createElement("Document");
			kml.appendChild(root);
			
			Element style = doc.createElement("Style");
			style.setAttribute("id", "polyStyID_0");
			Element linestyle = doc.createElement("LineStyle");
			Element color = doc.createElement("color");
			color.appendChild(doc.createTextNode("FF0000FF"));
			Element width = doc.createElement("width");
			width.appendChild(doc.createTextNode("5"));
			linestyle.appendChild(width);
			linestyle.appendChild(color);
			
			Element PolyStyle = doc.createElement("PolyStyle");
			Element PolyStylecolor = doc.createElement("color");
			PolyStylecolor.appendChild(doc.createTextNode("660088ff"));
			PolyStyle.appendChild(PolyStylecolor);
			style.appendChild(linestyle);
			style.appendChild(PolyStyle);
			root.appendChild(style);
			
			
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	protected void doGetJPS(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		
		String path = request.getServletPath();
		logger.info("path called= "+path);
		

	

		if ("/ENVisualization/createLineJS".equals(path)) {
			
			JSONObject joforEN = AgentCaller.readJsonParameter(request);
			String iriofnetwork = joforEN.getString("electricalnetwork");
			OntModel model = readModelGreedy(iriofnetwork);
			
			String g=createLineJS(model);
			AgentCaller.printToResponse(g, response);
			
		} else if ("/ENVisualization/createKMLFile".equals(path)) {
			
			logger.info("path called here= "+path);
			JSONObject joforEN = AgentCaller.readJsonParameter(request);
			String iriofnetwork = joforEN.getString("electricalnetwork");
			logger.info("Laura   " + iriofnetwork);
			String n=joforEN.getString("n");
			OntModel model = readModelGreedy(iriofnetwork);
			BufferedWriter bufferedWriter = null;
			String b = null;
			System.out.println("test");
			try (FileWriter writer = new FileWriter("C://Users/LONG01/webapps/ROOT/OntoEN/testfinal.kml");
		             BufferedWriter bw = new BufferedWriter(writer)) {
				b = createfinalKML(model);

	           bw.write(b);
				
				
				if (true) {
					writeToResponse(response, b,n);
					return;
				}
				
			} catch (TransformerException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			AgentCaller.printToResponse(b, response);
		}
		else if ("/ENVisualization/getKMLFile".equals(path)) {

			logger.info("path called here= " + path);

			JSONObject joforEN = AgentCaller.readJsonParameter(request);
			String iriofnetwork = joforEN.getString("electricalnetwork");
			String n=joforEN.getString("n");
			OntModel model = readModelGreedy(iriofnetwork);

			String b = null;

				try {
					b = createfinalKML(model);
				} catch (TransformerException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				if (true) {
					writeToResponse(response, b,n);
					AgentCaller.printToResponse(b, response);
					return;
				}

			
		}
		
	}
	
	public void writeToResponse(HttpServletResponse response, String content,String n) {
		try {
			
			logger.info("uploading file");
			
		    //String fileName = "C:/Users/KADIT01/TOMCAT/webapps/ROOT/test2.kml";
		    String fileName = "C:\\Users\\LONG01\\webapps\\ROOT\\OntoEN\\en.kml";
		    String fileType = "text/xml; charset=utf-8";
		    // Find this file id in database to get file name, and file type
		
		    // You must tell the browser the file type you are going to send
		    // for example application/pdf, text/plain, text/html, image/jpg
		    response.setContentType(fileType);
		
		    // Make sure to show the download dialog
		    response.setHeader("Content-disposition","attachment; filename=en"+n+".kml");
		
		    // Assume file name is retrieved from database
		    // For example D:\\file\\test.pdf
		
		    File my_file = new File(fileName);
		
		    // This should send the file to browser
		    OutputStream out = response.getOutputStream();
		    FileInputStream in = new FileInputStream(my_file);
		    
		    //InputStream in = new ByteArrayInputStream(content.getBytes());
		    
		    byte[] buffer = new byte[4096];
		    int length;
		    while ((length = in.read(buffer)) > 0){
		       out.write(buffer, 0, length);
		    }
		    in.close();
		    out.flush();
		    
		    
		    logger.info("uploading file successful");
		    
		} catch (Exception e) {
			e.printStackTrace();
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}
	
	public String createfinalKML(OntModel model) throws TransformerException {
		ENVisualization a = new ENVisualization();

		// ------------FOR GENERATORS-----------------
		List<String[]> generators = a.queryElementCoordinate(model, "PowerGenerator");
		ArrayList<ENVisualization.StaticobjectgenClass> gensmerged = new ArrayList<ENVisualization.StaticobjectgenClass>();
		ArrayList<String> coorddata = new ArrayList<String>();
		for (int e = 0; e < generators.size(); e++) {
			StaticobjectgenClass gh = a.new StaticobjectgenClass();
			gh.setnamegen("/" + generators.get(e)[0].split("#")[1] + ".owl");
			gh.setx(generators.get(e)[1]);
			gh.sety(generators.get(e)[2]);
			//System.out.println("/" + generators.get(e)[0].split("#")[1] + ".owl");

			if (coorddata.contains(gh.getx()) && coorddata.contains(gh.gety())) {
				int index = coorddata.indexOf(gh.getx()) / 2;
				gensmerged.get(index).setnamegen(gensmerged.get(index).getnamegen() + gh.getnamegen());
			} else {
				gensmerged.add(gh);
				coorddata.add(generators.get(e)[1]);
				coorddata.add(generators.get(e)[2]);
			}

		}

		for (int g = 0; g < gensmerged.size(); g++) {
			MapPoint c = new MapPoint(Double.valueOf(gensmerged.get(g).gety()),
					Double.valueOf(gensmerged.get(g).getx()), 0.0, gensmerged.get(g).getnamegen());
			a.addMark(c, "generator");
		}

		// --------------------------------
		
	
		// ------------FOR BUS-----------------
		List<String[]> bus = a.queryElementCoordinate(model, "BusNode");
		ArrayList<ENVisualization.StaticobjectgenClass> bussesmerged = new ArrayList<ENVisualization.StaticobjectgenClass>();
		ArrayList<String> coorddatabus = new ArrayList<String>();
		for (int e = 0; e < bus.size(); e++) {
			StaticobjectgenClass gh = a.new StaticobjectgenClass();
			gh.setnamegen("/" + bus.get(e)[0].split("#")[1] + ".owl");
			gh.setx(bus.get(e)[1]);
			gh.sety(bus.get(e)[2]);
			//System.out.println("/" + bus.get(e)[0].split("#")[1] + ".owl");

			if (coorddatabus.contains(gh.getx()) && coorddatabus.contains(gh.gety())) {
				int index = coorddatabus.indexOf(gh.getx()) / 2;
				bussesmerged.get(index).setnamegen(bussesmerged.get(index).getnamegen() + gh.getnamegen());
			} else {
				bussesmerged.add(gh);
				coorddatabus.add(bus.get(e)[1]);
				coorddatabus.add(bus.get(e)[2]);
			}

		}

		for (int g = 0; g < bussesmerged.size(); g++) {
			MapPoint c = new MapPoint(Double.valueOf(bussesmerged.get(g).gety()),
					Double.valueOf(bussesmerged.get(g).getx()), 0.0, bussesmerged.get(g).getnamegen());
			a.addMark(c, "bus");
		}

		// --------------------------------

		
//		int size2 = bus.size();
//		for (int g = 0; g < size2; g++) {
//			MapPoint c = new MapPoint(Double.valueOf(bus.get(g)[2]), Double.valueOf(bus.get(g)[1]), 0.0,
//					"/" + bus.get(g)[0].split("#")[1] + ".owl");
//			a.addMark(c, "bus");
//		}

		return a.writeFiletoString();
	}
	
	 public class StaticobjectgenClass {
		private String genname = "";
		private String x = "0";
		private String y = "0";
		
		public StaticobjectgenClass() {
		
		}
		
		public void setnamegen(String genname) {
			this.genname= genname;
		}
		public String getnamegen() {
			return genname;
		}
		
		public String getx() {
			return x;
		}
		public void setx(String x) {
			this.x = x;
		}
		public String gety() {
			return y;
		}
		public void sety(String y) {
			this.y = y;
		}
		
	}
	
		public static OntModel readModelGreedy(String iriofnetwork) {
			String electricalnodeInfo = "PREFIX j1:<http://www.jparksimulator.com/ontology/ontoland/OntoLand.owl#> "
					+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
					+ "SELECT ?component "
					+ "WHERE {?entity  a  j2:CompositeSystem  ." + "?entity   j2:hasSubsystem ?component ." + "}";

			QueryBroker broker = new QueryBroker();
			return broker.readModelGreedy(iriofnetwork, electricalnodeInfo);
		}
		
	public ArrayList <Double[]> estimateSquare(double x,double y,double constant){
		ArrayList<Double[]>squrepoints = new ArrayList<Double[]>();
		Double [] points1= {x-(0.0002+constant),y,0.0};
		Double [] points2= {x,y-(0.00015+constant),0.0};
		Double [] points3= {x+(0.0002+constant),y,0.0};
		Double [] points4= {x,y+(0.00015+constant),0.0};
		Double [] points5= {x-(0.0002+constant),y,0.0};
	
			
		squrepoints.add(points1);
		squrepoints.add(points2);
		squrepoints.add(points3);
		squrepoints.add(points4);
		squrepoints.add(points5);
		
		return squrepoints;
	}
	
	/**
	 * Add a placemark to this KML object.
	 * @param mark
	 */
	public  void addMark(MapPoint mark,String type) {
		Element placemark = doc.createElement("Placemark");
		root.appendChild(placemark);
		
		Element name = doc.createElement("name");
		name.appendChild(doc.createTextNode(mark.getName()));
		placemark.appendChild(name);
		
		SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss z");
		Element desc = doc.createElement("description");
		desc.appendChild(doc.createTextNode(mark.getName()+", "+mark.getLatitude() + ", " + mark.getLongitude() + "\n" +
				"Altitude: " + mark.getAltitude() + " meters\n" +
				"Time: " + sdf.format(new Date(mark.getTime()))));
		placemark.appendChild(desc);
		
		Element styleurl = doc.createElement("styleUrl");
		double busconstant=0;
		if(type.contains("bus"))
			{
				styleurl.appendChild(doc.createTextNode("#polyStyID_0"));	
				busconstant=0.00025;
			}
		else if(type.contains("generator")) {
			styleurl.appendChild(doc.createTextNode("#polyStyID_1"));
		}
		
		placemark.appendChild(styleurl);
		
		Element polygon = doc.createElement("Polygon");
		placemark.appendChild(polygon);

		Element tesellate = doc.createElement("tessellate");
		tesellate.appendChild(doc.createTextNode("1"));
		polygon.appendChild(tesellate);
		
		Element altitudeMode = doc.createElement("altitudeMode");
		altitudeMode.appendChild(doc.createTextNode("clampToGround"));
		polygon.appendChild(altitudeMode);
		
		Element outerBoundaryIs = doc.createElement("outerBoundaryIs");
		polygon.appendChild(outerBoundaryIs);
		
		Element LinearRing = doc.createElement("LinearRing");
		outerBoundaryIs.appendChild(LinearRing);
		
		
		ArrayList<Double[]>point=estimateSquare(mark.getLongitude(),mark.getLatitude(),busconstant);
		Element coords = doc.createElement("coordinates");
		coords.appendChild(doc.createTextNode(point.get(0)[0] + "," + point.get(0)[1] + "," + point.get(0)[2]+"\n"
											 +point.get(1)[0] + "," + point.get(1)[1] + "," + point.get(1)[2]+"\n"
											 +point.get(2)[0] + "," + point.get(2)[1] + "," + point.get(2)[2]+"\n"
											 +point.get(3)[0] + "," + point.get(3)[1] + "," + point.get(3)[2]+"\n"
											 +point.get(4)[0] + "," + point.get(4)[1] + "," + point.get(4)[2]+"\n"
											 ));
		LinearRing.appendChild(coords);
		
		
		/*
		 * Element point = doc.createElement("Point"); placemark.appendChild(point);
		 * 
		 * if(mark.getAltitude() > 0) { Element altitudeMode =
		 * doc.createElement("altitudeMode");
		 * altitudeMode.appendChild(doc.createTextNode("absolute"));
		 * point.appendChild(altitudeMode); }
		 * 
		 * Element coords = doc.createElement("coordinates");
		 * coords.appendChild(doc.createTextNode(mark.getLongitude() + ", " +
		 * mark.getLatitude() + ", " + mark.getAltitude())); point.appendChild(coords);
		 */
	}
	
	public  void removeMark(int index) {
        Element element = (Element) doc.getElementsByTagName("Placemark").item(index);
        
        // remove the specific node
        element.getParentNode().removeChild(element);
	}
	
	public  String getremoveMarkname(int index) {
        Element element = (Element) doc.getElementsByTagName("Placemark").item(index);
        Node n=null;
        String value="noname";
        
        for (int i = 0; i < element.getChildNodes().getLength(); i++) {           
     
        	  n= element.getChildNodes().item(i);                            
        	 
        	  if("name".contentEquals(n.getNodeName())) {

        		  value=n.getTextContent();

        	  }
        }
        
        return value;
	}
	
	/**
	 * Add a path to this KML object.
	 * @param path
	 * @param pathName
	 */
	public  void addPath(List<MapPoint> path, String pathName) {
		Element placemark = doc.createElement("Placemark");
		root.appendChild(placemark);
		
		if(pathName != null) {
			Element name = doc.createElement("name");
			name.appendChild(doc.createTextNode(pathName));
			placemark.appendChild(name);
		}
		
		Element lineString = doc.createElement("LineString");
		placemark.appendChild(lineString);
		
		Element extrude = doc.createElement("extrude");
		extrude.appendChild(doc.createTextNode("1"));
		lineString.appendChild(extrude);
		
		Element tesselate = doc.createElement("tesselate");
		tesselate.appendChild(doc.createTextNode("1"));
		lineString.appendChild(tesselate);
		
		Element altitudeMode = doc.createElement("altitudeMode");
		altitudeMode.appendChild(doc.createTextNode("absolute"));
		lineString.appendChild(altitudeMode);
		
		Element coords = doc.createElement("coordinates");
		String points = "";
		ListIterator<MapPoint> itr = path.listIterator();
		while(itr.hasNext()) {
			MapPoint p = itr.next();
			points += p.getLongitude() + "," + p.getLatitude() + "," + p.getAltitude() + "\n";
		}
		coords.appendChild(doc.createTextNode(points));
		lineString.appendChild(coords);
	}
	
	/**
	 * Write this KML object to a file.
	 * @param file
	 * @return
	 * @throws TransformerException 
	 */
	public  String writeFiletoString() throws TransformerException {
		
			TransformerFactory factory = TransformerFactory.newInstance();
			Transformer transformer = factory.newTransformer();
			transformer.setOutputProperty(OutputKeys.INDENT, "yes");
			transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2");
			StringWriter writer = new StringWriter();
			transformer.transform(new DOMSource(doc), new StreamResult(writer));
			String output = writer.getBuffer().toString();//.replaceAll("\n|\r", "");
			//DOMSource src = new DOMSource(doc);
			//StreamResult out = new StreamResult(file);
			//transformer.transform(src, out);
		
		return output;
	}
	
	/**
	 * Read the KML file into this object.
	 * @param file
	 */
	public  void readFile(File file) {
		// TODO read KML file
	}
	
	public List<String[]> queryElementCoordinate(OntModel model,String type) {
	//String[]typelist= {"PowerGenerator","BusNode"};
	
	String gencoordinate = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
			+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
			+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
			+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
			+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
			+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
			+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
			+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
			+ "SELECT ?entity ?valueofx ?valueofy "
			+ "WHERE {?entity  a  j1:"+type+"  ." 
			+ "?entity   j7:hasGISCoordinateSystem ?coorsys ."

			+ "?coorsys  j7:hasProjectedCoordinate_y  ?y  ."
			+ "?y  j2:hasValue ?vy ." 
			+ "?vy  j2:numericalValue ?valueofy ."

			+ "?coorsys  j7:hasProjectedCoordinate_x  ?x  ."
			+ "?x  j2:hasValue ?vx ." 
			+ "?vx  j2:numericalValue ?valueofx ."
			
			+ "}";
	
	ResultSet resultSet = JenaHelper.query(model, gencoordinate);
	String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
	String[] keys = JenaResultSetFormatter.getKeys(result);
	List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
	
	return resultList;
	}
	
	public String createLineJS(OntModel model) throws IOException {
		String branchInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "PREFIX j9: <http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "SELECT ?entity ?busa ?busb "

				+ "WHERE {?entity  a  j1:UndergroundCable  ." 
				+ "?entity j9:hasInput ?busa ."
				+ "?entity j9:hasOutput ?busb ."

				+ "}";
		
		ResultSet resultSet = JenaHelper.query(model, branchInfo);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultListbranch = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		ArrayList<String> busdata= new ArrayList<String>();
		
		String stropen = " var lines=";
	    String strend = ";";
	    ArrayList<String>textcomb=new ArrayList<String>();
		
		//for the first line branch only 
		for (int o=0;o<2;o++) {
			String iri=null;
			if(o==0)	{
				iri="<"+resultListbranch.get(0)[1]+">";
			}
			else {
				iri="<"+resultListbranch.get(0)[2]+">";
			}
			
			String busInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
					+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
					+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
					+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
					+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
					+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
					+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
					+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
					+ "PREFIX j9: <http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
					+ "SELECT ?VoltMagvalue ?valueofx ?valueofy ?BaseKVvalue "
					
					+ "WHERE {"+iri+"  a  j1:BusNode  ." 
					+ iri+"   j2:isModeledBy ?model ."

					
					+ "?model   j5:hasModelVariable ?VM ." 
					+ "?VM  a  j3:Vm  ." 
					+ "?VM  j2:hasValue ?vVM ."
					+ "?vVM   j2:numericalValue ?VoltMagvalue ." // Vm
					
					+ iri+"   j7:hasGISCoordinateSystem ?coorsys ."
					+ "?coorsys  j7:hasProjectedCoordinate_x  ?x  ."
					+ "?x  j2:hasValue ?vx ." 
					+ "?vx  j2:numericalValue ?valueofx ."
					+ "?coorsys  j7:hasProjectedCoordinate_y  ?y  ."
					+ "?y  j2:hasValue ?vy ." 
					+ "?vy  j2:numericalValue ?valueofy ."
					
					+ "?model   j5:hasModelVariable ?BKV ." 
					+ "?BKV  a  j3:baseKV  ." 
					+ "?BKV  j2:hasValue ?vBKV ."
					+ "?vBKV   j2:numericalValue ?BaseKVvalue ." // Base KV1



					+ "}";
			ResultSet resultSet2 = JenaHelper.query(model, busInfo);
			String result2 = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet2);
			String[] keys2 = JenaResultSetFormatter.getKeys(result2);
			List<String[]> resultListbus1 = JenaResultSetFormatter.convertToListofStringArrays(result2, keys2);
			busdata.add(iri);
			busdata.add(resultListbus1.get(0)[0]);
			busdata.add(resultListbus1.get(0)[1]);
			busdata.add(resultListbus1.get(0)[2]);
			busdata.add(resultListbus1.get(0)[3]);

		}

		
	
	    int tick=3;
	    if(Double.valueOf(busdata.get(1))*Double.valueOf(busdata.get(4))>200||Double.valueOf(busdata.get(6))*Double.valueOf(busdata.get(9))>200) {
	    	tick=6;
	    }
	    else if(30>Double.valueOf(busdata.get(1))*Double.valueOf(busdata.get(4))&&Double.valueOf(busdata.get(6))*Double.valueOf(busdata.get(9))<30) {
	    	tick=1;
	    }
	    String linetype="distribute";
	    if(busdata.get(3).contentEquals(busdata.get(8))&&busdata.get(2).contentEquals(busdata.get(7))) {
	    	linetype="transformer";
	    }
	    //String contentbegin="{coors: [{lat: "+busdata.get(3)+", lng: "+busdata.get(2)+"}, {lat: "+busdata.get(8)+", lng: "+busdata.get(7)+"}], vols: ["+Double.valueOf(busdata.get(1))*Double.valueOf(busdata.get(4))+","+Double.valueOf(busdata.get(9))*Double.valueOf(busdata.get(6))+"], thickness: "+tick+", type: '"+linetype+"', name: '/"+resultListbranch.get(0)[0].split("#")[1]+".owl'}"; (temp changes to remove line.js
	    String contentbegin="{\"coors\": [{\"lat\": "+busdata.get(3)+", \"lng\": "+busdata.get(2)+"}, {\"lat\": "+busdata.get(8)+", \"lng\": "+busdata.get(7)+"}], \"vols\": ["+Double.valueOf(busdata.get(1))*Double.valueOf(busdata.get(4))+","+Double.valueOf(busdata.get(9))*Double.valueOf(busdata.get(6))+"], \"thickness\": "+tick+", \"type\": \""+linetype+"\", \"name\": \"/"+resultListbranch.get(0)[0].split("#")[1]+".owl\"}";
	    if(Double.valueOf(busdata.get(1))*Double.valueOf(busdata.get(4))<Double.valueOf(busdata.get(9))*Double.valueOf(busdata.get(6))) {
	     contentbegin="{\"coors\": [{\"lat\": "+busdata.get(8)+", \"lng\": "+busdata.get(7)+"}, {\"lat\": "+busdata.get(3)+", \"lng\": "+busdata.get(2)+"}], \"vols\": ["+Double.valueOf(busdata.get(9))*Double.valueOf(busdata.get(6))+","+Double.valueOf(busdata.get(1))*Double.valueOf(busdata.get(4))+"], \"thickness\": "+tick+", \"type\": \""+linetype+"\", \"name\": \"/"+resultListbranch.get(0)[0].split("#")[1]+".owl\"}";
	    }
	    
	    
	    textcomb.add(contentbegin);
	   
	    
	    //for the rest of the lines branch
	    for (int a=1;a<resultListbranch.size();a++) {
			for (int o=0;o<2;o++) {
				String iri=null;
				if(o==0)	{
					iri="<"+resultListbranch.get(a)[1]+">";
				}
				else {
					iri="<"+resultListbranch.get(a)[2]+">";
				}
				
				String busInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
						+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
						+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
						+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
						+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
						+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
						+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
						+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
						+ "PREFIX j9: <http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
						+ "SELECT ?VoltMagvalue ?valueofx ?valueofy ?BaseKVvalue "
						
						+ "WHERE {"+iri+"  a  j1:BusNode  ." 
						+ iri+"   j2:isModeledBy ?model ."

						
						+ "?model   j5:hasModelVariable ?VM ." 
						+ "?VM  a  j3:Vm  ." 
						+ "?VM  j2:hasValue ?vVM ."
						+ "?vVM   j2:numericalValue ?VoltMagvalue ." // Vm
						
						+ iri+"   j7:hasGISCoordinateSystem ?coorsys ."
						+ "?coorsys  j7:hasProjectedCoordinate_x  ?x  ."
						+ "?x  j2:hasValue ?vx ." 
						+ "?vx  j2:numericalValue ?valueofx ."
						+ "?coorsys  j7:hasProjectedCoordinate_y  ?y  ."
						+ "?y  j2:hasValue ?vy ." 
						+ "?vy  j2:numericalValue ?valueofy ."						
						
						+ "?model   j5:hasModelVariable ?BKV ." 
						+ "?BKV  a  j3:baseKV  ." 
						+ "?BKV  j2:hasValue ?vBKV ."
						+ "?vBKV   j2:numericalValue ?BaseKVvalue ." // Base KV

						+ "}";
				ResultSet resultSet2 = JenaHelper.query(model, busInfo);
				String result2 = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet2);
				String[] keys2 = JenaResultSetFormatter.getKeys(result2);
				List<String[]> resultListbus1 = JenaResultSetFormatter.convertToListofStringArrays(result2, keys2);
				busdata.add(iri);
				busdata.add(resultListbus1.get(0)[0]);
				busdata.add(resultListbus1.get(0)[1]);
				busdata.add(resultListbus1.get(0)[2]);
				busdata.add(resultListbus1.get(0)[3]);
			}
			
			int tick2=3;
			if(Double.valueOf(busdata.get(1+10*a))*Double.valueOf(busdata.get(4+10*a))>200||Double.valueOf(busdata.get(6+10*a))*Double.valueOf(busdata.get(9+10*a))>200) {
		    	tick2=6;
		    }
		    else if(30>Double.valueOf(busdata.get(1+10*a))*Double.valueOf(busdata.get(4+10*a))&&30>Double.valueOf(busdata.get(9+10*a))*Double.valueOf(busdata.get(6+10*a))) {
		    	tick2=1;
		    }
			linetype="distribute";
		    if(busdata.get(3+10*a).contentEquals(busdata.get(8+10*a))&&busdata.get(2+10*a).contentEquals(busdata.get(7+10*a))) {
		    	linetype="transformer";
		    }
//	    	String content="{coors: [{lat: "+busdata.get(3+10*a)+", lng: "+busdata.get(2+10*a)+"}, {lat: "+busdata.get(8+10*a)+", lng: "+busdata.get(7+10*a)+"}], vols: ["+Double.valueOf(busdata.get(1+10*a))*Double.valueOf(busdata.get(4+10*a))+","+Double.valueOf(busdata.get(6+10*a))*Double.valueOf(busdata.get(9+10*a))+"], thickness: "+tick2+", type: '"+linetype+"', name: '/"+resultListbranch.get(a)[0].split("#")[1]+".owl'}"; temp changed to remove line.js
		    String content="{\"coors\": [{\"lat\": "+busdata.get(3+10*a)+", \"lng\": "+busdata.get(2+10*a)+"}, {\"lat\": "+busdata.get(8+10*a)+", \"lng\": "+busdata.get(7+10*a)+"}], \"vols\": ["+Double.valueOf(busdata.get(1+10*a))*Double.valueOf(busdata.get(4+10*a))+","+Double.valueOf(busdata.get(6+10*a))*Double.valueOf(busdata.get(9+10*a))+"], \"thickness\": "+tick2+", \"type\": \""+linetype+"\", \"name\": \"/"+resultListbranch.get(a)[0].split("#")[1]+".owl\"}";
	    	if(Double.valueOf(busdata.get(1+10*a))*Double.valueOf(busdata.get(4+10*a))<Double.valueOf(busdata.get(6+10*a))*Double.valueOf(busdata.get(9+10*a))) {
	    		content="{\"coors\": [{\"lat\": "+busdata.get(8+10*a)+", \"lng\": "+busdata.get(7+10*a)+"}, {\"lat\": "+busdata.get(3+10*a)+", \"lng\": "+busdata.get(2+10*a)+"}], \"vols\": ["+Double.valueOf(busdata.get(6+10*a))*Double.valueOf(busdata.get(9+10*a))+","+Double.valueOf(busdata.get(1+10*a))*Double.valueOf(busdata.get(4+10*a))+"], \"thickness\": "+tick2+", \"type\": \""+linetype+"\", \"name\": \"/"+resultListbranch.get(a)[0].split("#")[1]+".owl\"}";
	    	}
		    
		    textcomb.add(content);
	    	
	    }
	    
	    
//	    BufferedWriter writer = new BufferedWriter(new FileWriter(file));
//	    writer.write(stropen+textcomb.toString()+strend);
//	     
//	    writer.close();
	    String resultfinal=stropen+textcomb.toString()+strend;
	    //return resultfinal; change temporarily for removing line.js
	    return textcomb.toString();
		
	}



}
