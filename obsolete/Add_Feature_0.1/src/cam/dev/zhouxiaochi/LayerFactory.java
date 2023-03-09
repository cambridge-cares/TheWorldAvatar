package cam.dev.zhouxiaochi;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.json.JSONException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.esri.core.geometry.CoordinateConversion;
import com.esri.core.geometry.MultiPath;
import com.esri.core.geometry.Point;
import com.esri.core.geometry.Polygon;
import com.esri.core.geometry.Polyline;
import com.esri.core.io.UserCredentials;
import com.esri.core.map.Graphic;
import com.esri.core.symbol.Symbol;
import com.esri.map.ArcGISFeatureLayer;
import com.esri.map.Layer.LayerStatus;

import cam.dev.zhouxiaochi.FeatureServiceUpdater.LayerType;

/***
 * Class that runs function to generate and populate(load features) layer for
 * one type of objects Require KML for shape information and OWL for attributes
 * data.
 * 
 * @author Shaocong
 *
 */
public class LayerFactory {
	// TODO geometry enum should be put here
	
////////////////static memeber/////////////////////////////////////////	
	private static FeatureServiceUpdater updater = new FeatureServiceUpdater(App.BASE_URL);

	//////////// constructor parameters///////////////////

	private String layerName;
	public String getLayerName() {
		return layerName;
	}

	private String owlLocation;
	private String kmlLocation;
	private LayerType geometryType;
	private String topNodesRegex;
	private Symbol symbol;
	private String identifierAttriXML2OWL = null;
	private UserCredentials user;
	//////////////// utility variables////////////////////////
	private ArcGISFeatureLayer targetLayer;
	private Set<String> attributeNames = new LinkedHashSet<String>();
	private List<String> entityNames = new ArrayList<String>();
	// private Map<String, String> coordinatesStrList = new HashMap<String,
	// String>();
	Boolean useOriginalOrder = true;

	/**
	 * Constructor
	 * 
	 * @param layerName
	 *            name of this layer to be generated
	 * @param owlLocation
	 *            location of the owl file that contains attributes data
	 * @param kmlLocation
	 *            location of the kml file that contains the coordinates info
	 * @param geometryType
	 *            geometry type of entities defined in enum [POLYGON|POLYLINE]
	 * @param topNodesRegex
	 * @throws IOException
	 * @throws Exception
	 */
	public LayerFactory(String layerName, String owlLocation, String kmlLocation, LayerType geometryType,
			String topNodesRegex, UserCredentials user, Symbol symbol) throws IOException, Exception {

		this.layerName = layerName;
		this.owlLocation = owlLocation;
		this.kmlLocation = kmlLocation;
		this.geometryType = geometryType;
		this.topNodesRegex = topNodesRegex;
		this.user = user;
		this.symbol = symbol;
	}
	
	public LayerFactory(LayerFactoryInfo info, UserCredentials user, Symbol symbol) throws IOException, Exception {

		this( info.getLayerName(),
		 info.getOwlSource(),
		info.getKmlSource(),
		info.getGeoType(),
		info.getDeviceNameRegex(),
		user,
		 symbol);
	}

	public LayerFactory(String layerName, String owlLocation, String kmlLocation, LayerType geometryType,
			String topNodesRegex, UserCredentials user,  Symbol symbol, String identifierAttriXML2OWL) throws IOException, Exception {

		this(layerName, owlLocation, kmlLocation, geometryType, topNodesRegex, user, symbol);

		if (identifierAttriXML2OWL != null && !identifierAttriXML2OWL.equals("")) {// identifierAttribute
																					// defined?
																					// =>
																					// Yes!
			useOriginalOrder = false;
			this.identifierAttriXML2OWL = identifierAttriXML2OWL;
		} else {
			System.out.println("Warning: identifierAttribute not defined. Gonna use original order instead");
		}
	}


	
	public void createLoadLayer() throws IOException, Exception {
		targetLayer = new ArcGISFeatureLayer(
				App.BASE_URL+"/" + layerName,
				user);
		
		if (!readAttriNameList()) {// read attribute name list fails?
			return ;// =>terminate
		}
		if (!generateLayer()) {// generate layer fails?
			return;// =>terminate
		}
		///////// wait for generating layer request to be completed////////////

		targetLayer.initializeAsync();
		
		try {

			
		while (!targetLayer.isAllowGeometryUpdates()) {
			if(targetLayer.getStatus() == LayerStatus.ERRORED){
      System.out.println("LAYER ERRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR");
      if(!updater.isLayerExist(layerName)){
      System.out.println("Regerating Layer");
  
		targetLayer = new ArcGISFeatureLayer(
				App.BASE_URL+"/" + layerName,
				user);
      generateLayer();
		targetLayer.initializeAsync();

			}
			}
			
				Thread.sleep(500);
				System.out.println(targetLayer);

				System.out.println("Loading");
			
		}
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
	}
		loadFeatureIntoArcgis();


		////////////////////////////////////////////////////////////////////


	}

	private Map<String, String> readKML() throws ParserConfigurationException, SAXException, IOException {
		// read info from kml, result in coordinates_list map
		Map<String, String> coordinatesStrList = new HashMap<String, String>();

		File inputFile = new File(kmlLocation);

		DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
		DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
		Document doc = dBuilder.parse(inputFile);
		doc.getDocumentElement().normalize();
		Element root = doc.getDocumentElement();
		NodeList featureList = root.getElementsByTagName("coordinates");
		NodeList idAttriList = null;
		if (!useOriginalOrder) {// identifierAttribute defined? => Yes!
			idAttriList = root.getElementsByTagName(identifierAttriXML2OWL);// =>get
																			// identifier
																			// Attribute
																			// node
																			// as
																			// a
																			// list
			if (idAttriList.getLength() != featureList.getLength() || idAttriList.getLength() < 1) {// identifier
																									// attribute
																									// nodes
																									// not
																									// same
																									// number
																									// as
																									// feature
																									// list
																									// or
																									// not
																									// found
																									// at
																									// all?
				System.out.println("Err: chosen identified attribute not exist or not has one for every entity");
				return null;
			}
		}
		for (int idxE = 0; idxE < featureList.getLength(); idxE++) {
			String value = featureList.item(idxE).getTextContent();
			String identifierAttri;
			if (useOriginalOrder) {
				identifierAttri = idxE + "";
			} else {
				identifierAttri = idAttriList.item(idxE).getTextContent();
			}
			if (coordinatesStrList.put(identifierAttri, value) != null) {
				System.out.println("Err: chosen identified attribute not unique for every entity");
				return null;
			}

		}
		return coordinatesStrList;

	}

	private boolean readAttriNameList() throws IOException, Exception {
		// read attribute lists from owl
		// read attribute name set
		/******** filter to top level nodes ******/
		if(owlLocation == null){//owl location not defined?
			System.out.println("Warning: owl location not specified. Entities will be printed out based on kml with no attributes");
		return true;
		}
		
		ArrayList<String> allNodeNames = OWLReader.read_owl_file(owlLocation, null);
		for (String nodeName : allNodeNames) {//// DOES THE NAME MATCH THIS
												//// PATTERN?
			if (nodeName.matches(topNodesRegex)) {// =>YES
				////// =>add name to the entity name list
				entityNames.add(nodeName);
				System.out.println("entity+++++_________++++++++++++++"+nodeName);
			}
		}

		if(entityNames.size() < 1){
			System.out.println("ERR: first level entity list has size 0");

			return false;
		}
		// read on first device to get attribute list
		OWLReader.read_owl_file(owlLocation, (String) entityNames.get(0));
		List<String> nameList = OWLReader.name_list;

		for (String attriName : nameList) {
			//j.2_hasGISCoordinateSystem

			attributeNames.add(deleteID(attriName));

		}
		for (String attributeName : attributeNames) {// print out for debug
			System.out.println("attibute:++++++++++++++++++++++" + attributeName);
		}
		if (attributeNames.size() > 0 ) {// check if
																	// both are
																	// indeed
																	// populated
			return true;
		} else {
			System.out.println("Warning: attribute name list has size 0");
			//return false;
		return true;
		}

	}

	public static String deleteID(String attriName) {
		// delete id and add attribute into attribute name list
		Pattern p = Pattern.compile("^(.+[ID|id]*)(_\\d+)(.*)$");// define
																// regex
																// pattern
		Matcher m = p.matcher(attriName);
		StringBuffer result = new StringBuffer();
		boolean found = false;// flag: if indeed find a match of regex
		while (m.find()) {// find a match?
			m.appendReplacement(result, m.group(1) + m.group(3));// delete
																	// the
																	// id
																	// part
																	// of
																	// attri
																	// name
			found = true;// set flag found to be true
		}
		m.appendTail(result);
		if (found) {// indeed has match?
			return result.toString();
		} else {
			//System.out.println("This attribute does not contain ID:" + attriName);
			return attriName;
		}
	}

	private boolean generateLayer() throws JSONException {
		// check if attribute name set is empty
		int length = attributeNames.size();
		if (length <= 0) {
			System.out.println("Warning: attribute name set empty");
			//return false;
		}

		String[] typeList = new String[length];
		// create layer with generateLayer function
		for (int i = 0; i < length; i++) {
			typeList[i] = "esriFieldTypeString";
		}
		String[] nameList = attributeNames.toArray(new String[length]);
		Map<String, String[]> attrLists = new HashMap<String, String[]>();
		attrLists.put("name", nameList);
		attrLists.put("type", typeList);
		attrLists.put("alias", nameList);
		updater.generateLayer(length, geometryType, attrLists, layerName);
		return true;
	}

	private void loadFeatureIntoArcgis() throws IOException, Exception {
		Map<String, String> coordinatesStrList = readKML();
		//////////check read kml result not null///////////////////////////
		if (coordinatesStrList == null) {
			System.out.println("ERR: can not read coordinate strings from kml");
			return;//terminate
		}
		

		// check geometry type using switch, saved as a template object to be copied each time one is required
		MultiPath geometryTmp;
		switch (geometryType) {
		case POLYGON:
			geometryTmp = new Polygon();
			break;
		case POLYLINE:
			geometryTmp = new Polyline();
			break;
		default:
			System.out.println("ERR: geometry type not defined in LayerFactory.");
			return;
		}
        int entityNamesLength = entityNames.size();
		int coordinateListLength = coordinatesStrList.size();
	
		if(entityNamesLength < 1){// owl file/first level node not defined?
			///to generate using only kml, populate arraylist entityNames with empty strings to run the loop
	     for(int idxEntity = 0 ; idxEntity < coordinateListLength ; idxEntity++){
	    	 entityNames.add("");
	     }
			
		}
		///////check kml entity number vs owl entity number //////////////////
		else if(coordinateListLength> entityNamesLength){//more coordinates set than owl entity? ignore over ones in kml
			System.out.println("WARNING: kml coordinates list length larger than owl entity list, will cut from kml side, Coor num:"+coordinatesStrList.size()+", entity num"+ entityNames.size());
		} else if(coordinateListLength < entityNamesLength){//more owl entity than coordinates? Terminate feature population
			System.out.println("ERR: kml coordinates list length smaller than owl entity list, terminate feature population, Coor num:"+coordinatesStrList.size()+", entity num"+ entityNames.size());
		    return;//terminate
		}
		
		// read owl on each entity, find corresponding kml coordinates, draw
		// geometry///////////

		
		int idxEntity = 0;
		
		Graphic[] adds = new Graphic[entityNames.size()];
		for (String entityName : entityNames) {
			OWLReader.read_owl_file(owlLocation, entityName);
			//////// pack attributes from OWLReader////////////////////////////
			Map<String, Object> attributes = new HashMap<String, Object>();
			int idxAttri = 0;
			for (String attriName : attributeNames) {
				int idxV = 0; boolean found = false;
				 while (!found && idxV < OWLReader.name_list.size()) {
					String valueName = OWLReader.name_list.get(idxV);
					if (deleteID(valueName).equals(attriName)) {
						attributes.put(attriName, OWLReader.value_list.get(idxV));
						found = true;
					}
					idxV++;
				}
				 if(!found){
						attributes.put(attriName, "");
				 }
				idxAttri++;
			}
			//////// Find corresponding coordinates from kml//////////////////
			String key;
			if (useOriginalOrder) {// use original order?
				key = idxEntity + "";// key is index
			} else {// use identifier String as to search
				key = (String) attributes.get(identifierAttriXML2OWL);
			}
			String coordStr = coordinatesStrList.get(key);// get coordStr
			if (coordStr == null) {
				System.out.println("ERR: empty coordinate Str @ " + key);
				return;
			}
			int idxPoint = 0;
			MultiPath aGeometry = (MultiPath) geometryTmp.copy();
			///// Process coordinate Str to construct points////////////////////
			for (String coordinate_string : coordStr.split(",100\n"))// split
																		// each
																		// x,y
																		// pair
			{
				String x_string = coordinate_string.split(",")[0].trim();
				String y_string = coordinate_string.split(",")[1].trim();

				// 01.80N 000.90E
				Point point = CoordinateConversion.decimalDegreesToPoint(y_string + "N " + x_string + "E",
						targetLayer.getDefaultSpatialReference());
				// System.out.println(point);

				if (idxPoint == 0) {
					aGeometry.startPath(point);
				} else {
					aGeometry.lineTo(point);
				}

				idxPoint++;
			}

			///// construct graphics array to update
			
			Graphic building_graphic = new Graphic(aGeometry, symbol, attributes);
			adds[idxEntity] = building_graphic;
			idxEntity++;

		}

		targetLayer.applyEdits(adds, null, null, new ApplyEditCallback(layerName));
		return;
	}

}
