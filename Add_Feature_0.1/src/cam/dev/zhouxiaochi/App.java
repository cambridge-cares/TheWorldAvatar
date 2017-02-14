package cam.dev.zhouxiaochi;

import java.awt.EventQueue;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.xml.parsers.ParserConfigurationException;

import org.json.JSONException;
import org.xml.sax.SAXException;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import com.esri.core.geometry.CoordinateConversion;
import com.esri.core.geometry.Envelope;
import com.esri.core.geometry.Geometry;
import com.esri.core.geometry.GeometryEngine;
import com.esri.core.geometry.Line;
import com.esri.core.geometry.Point;
import com.esri.core.geometry.Polygon;
import com.esri.core.geometry.Polyline;
import com.esri.core.geometry.SpatialReference;
import com.esri.core.geometry.Transformation2D;
import com.esri.core.io.UserCredentials;
import com.esri.core.map.CallbackListener;
import com.esri.core.map.Feature;
import com.esri.core.map.FeatureEditResult;
import com.esri.core.map.FeatureSet;
import com.esri.core.map.Graphic;
import com.esri.core.portal.Portal;
import com.esri.core.portal.WebMap;
import com.esri.core.renderer.SimpleRenderer;
import com.esri.core.symbol.SimpleFillSymbol;
import com.esri.core.symbol.SimpleLineSymbol;
import com.esri.core.tasks.ags.query.Query;
import com.esri.map.ArcGISFeatureLayer;
import com.esri.map.ArcGISFeatureLayer.SELECTION_METHOD;
import com.esri.map.ArcGISTiledMapServiceLayer;
import com.esri.map.GroupLayer;
import com.esri.map.JMap;
import com.esri.map.Layer;
import com.esri.map.LayerEvent;
import com.esri.map.LayerList;
import com.esri.map.LayerListEventListenerAdapter;
import com.esri.map.MapOptions;
import com.esri.map.MapOptions.MapType;
import com.esri.map.MapOverlay;
import com.esri.map.QueryMode;
import com.esri.map.popup.*;
import com.esri.toolkit.overlays.HitTestEvent;
import com.esri.toolkit.overlays.HitTestListener;
import com.esri.toolkit.overlays.HitTestOverlay;
import com.esri.toolkit.overlays.InfoPopupOverlay;

public class App {
	
	 enum LineType{
		WATER(0),
		GAS(1),
		MATERIAL(2),
		AIR(3);
	    private LineType(int id) {
	        this.id = id;
	    }
		private final int id;
	    public int getId() {
	        return id;
	    }
	    
	}

	public final static String BASE_URL = "http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/TEST020/FeatureServer";// Base
																																	// url
																																	// for
																																	// Arcgis
																																	// service!!!!
	public static String[] DEVICE_TYPE_MAP_LOCATIONS = { "map.txt", "map2.txt", "map3.txt" };// list
																					// of
																					// device-type
																					// map
																					// location

	public static String[] PLANT_OWL_FILE_NAME = { "BiodieselPlant3.owl", "BiodieselPlant2WWHR.owl", "BiodieselPlant1WOWHR.owl" };// list
																										// of
																										// owl
																										// files,
																										// note
																										// sequence
																										// corrspond
																										// to
																										// device
																										// type
																										// maps

	public final static String ElECTRICAL_OWL_FILE_NAME = "updated electrical network.owl";// owl
																							// file
																							// for
																							// electrical

	public static int layerID;
	public static ArcGISFeatureLayer Layer;
	public static JCheckBox checkbox;

	// public static String target;
	public static ArrayList<DeviceInfo> deviceInfoList = new ArrayList<DeviceInfo>();
	public static String[] types;

	public static double[] x_array;
	public static double[] y_array;
	public static ArrayList<ArrayList<String>> relationship_array;

	public static Map<String, ArcGISFeatureLayer> LayerMap;
	//public static ArcGISFeatureLayer linelayer;//TODO: ONLY PUT FOR FUTURE USE: EXTRACTING LINE FROM OWL

	public static ArcGISFeatureLayer[] linelayers;

	public static ArrayList<String> combination = new ArrayList<String>();

	public static ArrayList<Graphic[]> all_features = new ArrayList<Graphic[]>();
	public static ArrayList<ArcGISFeatureLayer> all_layers = new ArrayList<ArcGISFeatureLayer>();

	public static int[] count;
	public static final Map<String, ArcGISFeatureLayer> editlayer = new LinkedHashMap<>();
	private static FeatureServiceUpdater featureUpdater;
	//TODO: this might be deleted, this symbol does not affect the final map
	static SimpleLineSymbol lineSymbolMaterial = new SimpleLineSymbol(Color.RED, 500);
	static SimpleLineSymbol lineSymbolWater = new SimpleLineSymbol(Color.BLUE, 500);
	static SimpleLineSymbol lineSymbolGas = new SimpleLineSymbol(Color.ORANGE, 500);
	static SimpleLineSymbol lineSymbolAir = new SimpleLineSymbol(Color.BLACK, 500);

	static SimpleLineSymbol[] lineSymbols = {lineSymbolWater,lineSymbolGas,lineSymbolMaterial,lineSymbolAir};

	/**
	 * DeviceInfo, stores name, type and plantID of one device.
	 * @author Shaocong
	 *
	 */
	public static class DeviceInfo {
		public String name;
		public String type;
		public int plantID;

		public DeviceInfo(String name, String type, int plantID) {
			super();
			this.name = name;
			this.type = type;
			this.plantID = plantID;
		}

	}

	/**
	 * ////Add A device feature to Arcgis Server: A wrapper to call
	 * featureWriter function
	 * 
	 * @param x
	 *            coordi x
	 * @param y
	 *            coordi y
	 * @param thisLayer
	 *            arcgis feature layer to add to
	 * @param type
	 *            type of this device(ex: Reactor)
	 * @param name
	 *            name of this feature
	 * @param plantID 
	 * @throws Exception
	 */
	public static void create_object(double x, double y, ArcGISFeatureLayer thisLayer, String type, String name, int plantID)
			throws Exception {

		thisLayer.setOperationMode(QueryMode.SELECTION_ONLY);

		FeatureWriter featurewriter = new FeatureWriter();
		if (thisLayer.getDefaultSpatialReference() == null) {
			JOptionPane.showMessageDialog(null, "Sorry, the layer is not yet fully loaded");
		} else {
			/**
			 * call feature writer to get the packed up graphic array(info of
			 * features to be added)
			 **/
			Graphic[] g = featurewriter.createFeature(type, x, y, thisLayer.getDefaultSpatialReference(), name, plantID);
			// rect_list.add(featurewriter.obstacle);
			thisLayer.applyEdits(g, null, null, new ApplyEditCallback());// add the new features to
														// layer
			all_layers.add(thisLayer);
			all_features.add(g);
		}
	}

	/***
	 * Add ALL storage tank features to Arcgis Server: A wrapper to call
	 * featureWriter function
	 * 
	 * @param thisLayer
	 *            layer to add to, should be storage tank layer
	 * @throws Exception
	 */
	public static void createStorageTank(ArcGISFeatureLayer thisLayer) throws Exception {

		FeatureWriter featurewriter = new FeatureWriter();
		if (thisLayer.getDefaultSpatialReference() == null) {
			JOptionPane.showMessageDialog(null, "Sorry, the layer is not yet fully loaded");
		} else {
			/**
			 * call feature writer to get the packed up graphic array(info of
			 * features to be added)
			 **/
			Graphic[] g = featurewriter.createFeatureStorage();
			thisLayer.applyEdits(g, null, null, new ApplyEditCallback());// add the new features to
														// layer
			all_layers.add(thisLayer);
			all_features.add(g);
		}
	}

	/***
	 * Add A line feature to Arcgis Server
	 * function
	 * 
	 * @param pipeInfo
	 *            info required to draw a pipe feature, struct with 2 members:
	 *            point list & attribute list.
	 */
	public static void create_line(PipeReader.PipeInfo pipeInfo) {
		// linelayer.setOperationMode(QueryMode.SELECTION_ONLY);

		List<Point> points = pipeInfo.path;
		//TODO
		if (linelayers[0].getDefaultSpatialReference() == null) {
			JOptionPane.showMessageDialog(null, "Sorry, the layer is not yet fully loaded");
		} else {
			///////// Construct polyline from point list////////////
			Polyline line = new Polyline();
			line.startPath((Point) (points.get(0)));
			for (int idxP = 1; idxP < points.size(); idxP++) {
				Point mPt = (Point) (points.get(idxP));
				line.lineTo(mPt);
				System.out.println("+++++draw point" + mPt.getX() + "," + mPt.getY());
			}

			///////////////////////////////////////////////////////////////////

			Graphic polylineGraphic = new Graphic(line, lineSymbolMaterial, pipeInfo.attriList);// construct
																						// graphic
																						// object
																						// with
																						// polyline
																						// ob
																						// &
																						// attribute
																						// list

			Graphic[] adds = { polylineGraphic };// construct graphic array

			//TODO
			linelayers[0].applyEdits(adds, null, null, new ApplyEditCallback());// add graphics to line
															// layer

		}
	}


	final static SimpleFillSymbol testcolor = new SimpleFillSymbol(Color.black, new SimpleLineSymbol(Color.cyan, 1),
			SimpleFillSymbol.Style.SOLID);
	final static SimpleLineSymbol linecolor = new SimpleLineSymbol(Color.pink, 3);

	private JFrame window;
	private static JMap map;

	public App() throws Exception {

		JButton Load_feature = new JButton("Load Feature From OWL");
		Load_feature.setLocation(0, 0);
		Load_feature.setSize(190, 30);

		JButton Draw_connections = new JButton("Draw the connections");
		Draw_connections.setLocation(0, 30);
		Draw_connections.setSize(190, 30);

		JButton Delete = new JButton("Delete the features");
		Delete.setLocation(0, 60);
		Delete.setSize(190, 30);

		window = new JFrame();
		window.setSize(190, 130);
		window.setLocationRelativeTo(null); // center on screen
		window.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		window.getContentPane().setLayout(new BorderLayout(0, 0));

		window.add(Load_feature);
		window.add(Draw_connections);
		window.add(Delete);

		// dispose map just before application window is closed.

		window.addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosing(WindowEvent windowEvent) {
				super.windowClosing(windowEvent);
				map.dispose();
			}
		});

		// -------------------------------------------------------------------------------------------------------------------------

		/****
		 * Event Handler when load_button feature clicked. +++++Lines, devices,
		 * storage Tank features are generated into already generated layers on
		 * Arcgis Server.
		 */
		Load_feature.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				try {
					// drawLines(); // load line feature into Arcgis Server
					drawLinesFromKML();
					System.out.println("draw line from KML finished");
					for (int i = 0; i < deviceInfoList.size(); i++) {// loop through each device in deviceInfoList
																	
						if (!deviceInfoList.get(i).name.equals("storageTank")) {//Is device storage tank?
							DeviceInfo info = deviceInfoList.get(i);//=>NO! Then call create_object to load features into arcgis service
							create_object(x_array[i], y_array[i], LayerMap.get(info.name), info.type, info.name, info.plantID);
						}

					}
					//TODO: COMMENT FOR TESTING ,UNCOMMENT AFTERWARDS
					//createStorageTank(LayerMap.get("storageTank"));// load
																	// //storage
																	// // tank
																	// features individually
				} catch (SAXException | IOException | ParserConfigurationException e) {
					// TODO Auto-generated catch block

					e.printStackTrace();
				} catch (Exception e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}

			}

		});

		// TODO: delete this button?
		Draw_connections.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {

				// drawconnection();
				// rect_list.clear();
				// connections.clear();
			}
		});

		// TODO: does this still work?
		Delete.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {

				for (double point : x_array) {
					System.out.println("x --->" + point);
				}

				System.out.println("Delete Clicked 1 ");
				delete();

			}
		});

		// -------------------------------------------------------------------------------------------------------------------------

		// -------------------------------------------------------------------------------------------------------------------------
		MapOptions mapOptions = new MapOptions(MapType.TOPO);
		map = new JMap(mapOptions);

		ArcGISTiledMapServiceLayer tiledLayer = new ArcGISTiledMapServiceLayer(
				"http://services.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer");

		LayerList layerList = new LayerList();
		layerList.add(tiledLayer);
		UserCredentials user = new UserCredentials();
		user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp"); // Access
																			// secure
																			// feature
																			// layer
																			// service
																			// using
																			// login
																			// username
																			// and
																			// password

		// ================================================================

		LayerMap = new HashMap<String, ArcGISFeatureLayer>();
		
		/////todo: delete after testing
		 SimpleLineSymbol outline = new SimpleLineSymbol(new Color(255, 244, 0), 500);
		 SimpleFillSymbol symbol = new SimpleFillSymbol(new Color(0, 0, 0, 255), outline);
//		LayerFactory factory = new LayerFactory("Landlots", "owl/JParkLandLots.owl", "kml/Landlots.kml", FeatureServiceUpdater.LayerType.POLYGON, "^LandLotID_\\d+$", user,symbol);
		LayerFactory factory = new LayerFactory("EHTLines", "updated electrical network.owl", "kml/EHT Lines.kml", FeatureServiceUpdater.LayerType.POLYLINE, "^EHT-\\d+$", user,symbol);

		factory.createLoadLayer();
		ArcGISFeatureLayer buildinglayer = new ArcGISFeatureLayer(
				"http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/TEST020/FeatureServer/Buildings",
				user);
		readlist();

		relationship_array = new ArrayList<ArrayList<String>>();
		x_array = new double[deviceInfoList.size()];
		y_array = new double[deviceInfoList.size()];

		
		
		
		featureUpdater = new FeatureServiceUpdater(BASE_URL);


//		createLayer("waterline", -1, LineType.WATER);
//		createLayer("gasline", -1,LineType.GAS);
//		createLayer("materialline", -1,LineType.MATERIAL);
//		createLayer("airline", -1,LineType.AIR);
//
//		
//		linelayers = new ArcGISFeatureLayer[4];
//		for(int idxLineLayer = 0;  idxLineLayer < linelayers.length; idxLineLayer++){	
//		linelayers[idxLineLayer]=new ArcGISFeatureLayer(BASE_URL + "/" + idxLineLayer, user);
//		}
//		//TODO: exclude storage layer for testing ,delete -1 after testing
//		for (int i = 0; i < deviceInfoList.size() - 1; i++) {
//
//			DeviceInfo mDeviceInfo = deviceInfoList.get(i);
//			String target = mDeviceInfo.name;
//			createLayer(target, mDeviceInfo.plantID);
//			String idx = FeatureServiceUpdater.layerID;
//
//			System.out.println("#######################################################");
//			System.out.println(OWLReader.relationships);
//
//			relationship_array.add(OWLReader.relationships);
//
//			ArcGISFeatureLayer newLayer = new ArcGISFeatureLayer(BASE_URL + "/" + target, user);
//
//			all_layers.add(newLayer);
//			LayerMap.put(target, newLayer);
//			if(mDeviceInfo.plantID > 0){
//			OWLReader.read_owl_file(PLANT_OWL_FILE_NAME[mDeviceInfo.plantID], target);
//			}
//			x_array[i] = OWLReader.x;//TODO: WHAT IS THIS? IF IS STILL USEFUL NEED TO THINK OF CASE OF STORAGE TANK
//			y_array[i] = OWLReader.y;
//
//			SimpleRenderer renderer = new SimpleRenderer(testcolor);
//			newLayer.setRenderer(renderer);
//			layerList.add(newLayer);
//
//			map.getLayers().add(newLayer);
//
//			editlayer.put(target, newLayer);
//
//		}

		// ================================================================

		//TODO: what does this do, need to put it back?
		//LayerMap.put("linelayer", linelayer);
		//SimpleRenderer renderer2 = new SimpleRenderer(linecolor);
		//linelayer.setRenderer(renderer2);
		
		
		//TODO: REMOVE after testing
//		for(ArcGISFeatureLayer linelayer : linelayers){
//		layerList.add(linelayer);
//		map.getLayers().add(linelayer);
//		}
		layerList.add(tiledLayer);

		Point mapCenter = new Point(11543665, 141400);
		map.setExtent(new Envelope(mapCenter, 7200, 5400));

		window.getContentPane().add(map);
		//TODO: REMOVE after testing
//		BuildingKMLReader reader = new BuildingKMLReader();
//		reader.readkml(buildinglayer);
		

	}

	/**
	 * Starting point of this application.
	 * 
	 * @param args
	 * @throws Exception
	 * @throws IOException
	 */
	public static void main(String[] args) throws IOException, Exception {

		EventQueue.invokeLater(new Runnable() {

			@Override
			public void run() {
				try {
					App application = new App();
					application.window.setVisible(true);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		});
	}

	public static void createLayer(String targetName, int plantId) throws IOException, Exception {
		createLayer(targetName, plantId, LineType.WATER);
	}
	/****
	 * Create layer into Arcgis Service.
	 * 
	 * @param targetName
	 *            name of the device,required for reading Owl file. "linelayer"
	 *            for all line features. "storagetank" for all storage tanks.
	 * @throws IOException
	 * @throws Exception
	 */
	public static void createLayer(String targetName, int plantId,LineType lineType) throws IOException, Exception {
		/////// case: storage tank//////////////////////////////////////
		if (targetName.toLowerCase().contains("storagetank")) {
			System.out.println("Creating storage tank layer");
			StorageTankReader reader = StorageTankReader.getInstance();
			int length = reader.attributes.size();

			String[] typeList = new String[length];
			String[] nameList = new String[length];

			System.out.println("--------------------------------------");

			for (int i = 0; i < length; i++) {
				typeList[i] = "esriFieldTypeString";
			}

			int idxNameList = 0;
			for (Iterator iter = reader.attributes.iterator(); iter.hasNext();) {
				nameList[idxNameList] = (String) iter.next();
				System.out.println("atrri:++++++++   " + nameList[idxNameList]);
				idxNameList++;
			}

			// construct name map of lists
			Map<String, String[]> attrLists = new HashMap<String, String[]>();

			attrLists.put("name", nameList);
			attrLists.put("type", typeList);
			attrLists.put("alias", nameList);
			featureUpdater.generateLayer(length, FeatureServiceUpdater.LayerType.POLYGON, attrLists, targetName);

		}
		/////// case: devices//////////////////////////////////////
		else if (!targetName.contains("line")) {
			// TODO
			OWLReader.read_owl_file(PLANT_OWL_FILE_NAME[plantId], targetName);

			int lengthChemAttris = OWLReader.name_list.size();

			String[] nameListChemicalAttris = new String[lengthChemAttris];

			System.out.println("--------------------------------------");

			for (int i = 0; i < lengthChemAttris; i++) {
				nameListChemicalAttris[i] = OWLReader.name_list.get(i); // get
																		// the
																		// attributes
																		// name
																		// list
																		// (header)
																		// from
																		// the
																		// owl
																		// file

			}
			// Now read electrical owl file
			OWLReader.read_owl_file(ElECTRICAL_OWL_FILE_NAME, targetName);
			int lengthElectricalAttris = OWLReader.name_list.size();

			String[] nameListElectricalAttris = new String[lengthElectricalAttris];
			for (int i = 0; i < lengthElectricalAttris; i++) {
				nameListElectricalAttris[i] = OWLReader.name_list.get(i); // get
																			// the
																			// attributes
																			// name
																			// list
																			// (header)
																			// from
																			// the
																			// owl
																			// file

			}

			String[] typeList = new String[lengthElectricalAttris + lengthChemAttris];

			for (int i = 0; i < typeList.length; i++) {
				typeList[i] = "esriFieldTypeString";
			}

			String[] nameList = concatArr(nameListChemicalAttris, nameListElectricalAttris);
			// construct name map of lists
			Map<String, String[]> attrLists = new HashMap<String, String[]>();

			attrLists.put("name", nameList);
			attrLists.put("type", typeList);
			attrLists.put("alias", nameList);
			int lengthOfEachList = nameList.length;
			featureUpdater.generateLayer(lengthOfEachList, FeatureServiceUpdater.LayerType.POLYGON, attrLists,
					targetName);
		}
		/////// case: lines//////////////////////////////////////
		else if (targetName.contains("line")) {// generate layer list
			// TODO: UNCOMMENT WHEN USING OWL, TEMPERARYLY USING KML
			// PipeReader reader = PipeReader.getInstance();
			// int length = reader.getAttriNameSet().size();

			LineKMLReader reader = LineKMLReader.getInstance(lineType);
			int length = reader.attriNameList.size();
			String[] typeList = new String[length];
			// TODO: COMMENT WHEN USING OWL, TEMPERARYLY USING KML
			// String[] nameList = (String[])
			// reader.getAttriNameSet().toArray(new String[length]);
			String[] nameList = (String[]) reader.attriNameList.toArray(new String[length]);

			System.out.println("--------------------------------------");

			for (int i = 0; i < length; i++) {
				typeList[i] = "esriFieldTypeString";
			}

			Map<String, String[]> attrLists = new HashMap<String, String[]>();

			attrLists.put("name", nameList);
			attrLists.put("type", typeList);
			attrLists.put("alias", nameList);
			featureUpdater.generateLayer(length, FeatureServiceUpdater.LayerType.POLYLINE, attrLists, targetName);// generate
																												// line
																												// layer

		}

	}

	/****
	 * Utility function to concat two array
	 * 
	 * @param a, array a
	 * @param b  array b
	 * @return concated array a+b
	 */
	public static String[] concatArr(String[] a, String[] b) {
		int aLen = a.length;
		int bLen = b.length;
		String[] c = new String[aLen + bLen];
		System.arraycopy(a, 0, c, 0, aLen);
		System.arraycopy(b, 0, c, aLen, bLen);
		return c;
	}

	/***
	 * Read device name and type from txt files
	 */
	public static void readlist() {
		int idxPlant = 0;
		for (String deviceMapLocation : DEVICE_TYPE_MAP_LOCATIONS) {//for each device name/type list(one for each plant)
			try (BufferedReader br = new BufferedReader(new FileReader(deviceMapLocation))) {

				String sCurrentLine;

				while ((sCurrentLine = br.readLine()) != null) {//for each line
					// temp.add(sCurrentLine);
					String name = sCurrentLine.split("#")[0];
					String type = sCurrentLine.split("#")[1];
					deviceInfoList.add(new DeviceInfo(name, type, idxPlant));//pack info into DeviceInfo object,then store into list
				}

			} catch (IOException e) {
				e.printStackTrace();
			}
			idxPlant++;
		}

		// deviceInfoList[counter] = "storageTank";
		// types[counter] = "storagetank";

		deviceInfoList.add(new DeviceInfo("storageTank", "storagetank", -1));//add storagetank separately
	}

	// currently not able to be used because no indication of type in owl file
	// TODO: future with changed owl file (subsystem should has type indication)
	public static void readDeviceListFromOwl() throws IOException, Exception {

	}

	/***
	 * Currently not used.
	 * Funtion to Draw lines on layer Read line info from PipeReader and feed to
	 * info to create_line to create each line as feature on linelayer
	 * 
	 * @throws IOException
	 * @throws Exception
	 */
	public static void drawLines() throws IOException, Exception {

		List linelist = PipeReader.getInstance().getPipelist();
		for (Object lineInfo : linelist) {
			create_line((PipeReader.PipeInfo) lineInfo);
		}

	}

	// todo: delete this when segment pipe owl is completed
	/***
	 * Tmp function to draw line from kml(instead of owl) Read coordinates from
	 * LineKMLReader and add to linelayer
	 * 
	 * @throws Exception
	 * @throws IOException
	 */
	public static void drawLinesFromKML() throws IOException, Exception {
		
		for(LineType linetype: LineType.values()){
		int idxPipe = 0;
		LineKMLReader reader = LineKMLReader.getInstance(linetype);
		Graphic[] adds = new Graphic[reader.coordinates.size()];
		List<Map<String, Object>> AttriMap = reader.dataTables;

		for (List<Point> pipe : reader.coordinates) {
			Polyline pipeLine = new Polyline();
			Map<String, Object> attributes = AttriMap.get(idxPipe);

			pipeLine.startPath(pipe.get(0)); // set

			for (int idxPoint = 1; idxPoint < pipe.size(); idxPoint++) {
				pipeLine.lineTo(pipe.get(idxPoint).getX(), pipe.get(idxPoint).getY());
			}
			Graphic polyglineGraphic = new Graphic(pipeLine, lineSymbols[linetype.getId()], attributes);
			adds[idxPipe] = polyglineGraphic;
			idxPipe++;
		}

		linelayers[linetype.getId()].applyEdits(adds, null, null, new ApplyEditCallback());
		}
	}

	public static int getIndex(String[] array, String item) {
		int index = 0;

		return index;
	}

	public static void delete() {

		for(ArcGISFeatureLayer linelayer : linelayers){
		all_layers.add(linelayer);
		}
		for (int i = 0; i < all_layers.size(); i++) {
			// all_layers.get(i).applyEdits(null, all_features.get(i), null,
			// null);
			int[] ids = all_layers.get(i).getGraphicIDs();
			for (int id : ids) {
				System.out.println(id);
			}

			all_layers.get(i).setSelectionIDs(ids, true);

			all_layers.get(i).applyEdits(null, all_layers.get(i).getSelectedFeatures(), null, new ApplyEditCallback());
		}
	}

	public static void removeElement(Object[] a, int del) {
		System.arraycopy(a, del + 1, a, del, a.length - 1 - del);
	}

}

//