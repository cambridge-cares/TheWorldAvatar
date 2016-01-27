package jParkSim;
/* ArcGIS Online Account that stores hosted feature layers:
 * Username: jparksimulator
 * Password: c4tjpark
 */
/* CURRENTLY KNOWN ISSUES THAT CAN BE IMPROVED:
 * 3. No method to extract layers from LayerList map.getLayers(), have to manually add layers to array ArcGISFeatureLayer[] completeLayerList
 * 4. Cannot draw and edit features yet
 */

// For more information and API reference on the ArcGIS SDK for Java, go to https://developers.arcgis.com/java/
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.DataOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JLayeredPane;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.WindowConstants;
import javax.swing.border.LineBorder;

import com.esri.core.geometry.Envelope;
import com.esri.core.geometry.Point;
import com.esri.core.io.EsriSecurityException;
import com.esri.core.io.UserCredentials;
import com.esri.core.map.Feature;
import com.esri.core.map.FeatureResult;
import com.esri.core.map.Graphic;
import com.esri.core.renderer.SimpleRenderer;
import com.esri.core.symbol.SimpleFillSymbol;
import com.esri.core.symbol.SimpleLineSymbol;
import com.esri.core.symbol.SimpleMarkerSymbol;
import com.esri.core.symbol.SimpleMarkerSymbol.Style;
import com.esri.core.symbol.Symbol;
import com.esri.core.tasks.query.QueryTask;
import com.esri.map.ArcGISFeatureLayer;
import com.esri.map.ArcGISTiledMapServiceLayer;
import com.esri.map.JMap;
import com.esri.map.LayerList;
import com.esri.map.MapEvent;
import com.esri.map.MapEventListenerAdapter;
import com.esri.map.popup.PopupDialog;
import com.esri.map.popup.PopupView;
import com.esri.map.popup.PopupViewEvent;
import com.esri.map.popup.PopupViewListener;
import com.esri.runtime.ArcGISRuntime;
import com.esri.toolkit.JLayerTree;
import com.esri.toolkit.legend.JLegend;
import com.esri.toolkit.overlays.HitTestEvent;
import com.esri.toolkit.overlays.HitTestListener;
import com.esri.toolkit.overlays.HitTestOverlay;

public class JParkSim {
	
	// style of different layers
		final static SimpleFillSymbol Landlotscolor = new SimpleFillSymbol(Color.cyan, new SimpleLineSymbol(Color.cyan, 1), SimpleFillSymbol.Style.NULL);
		final static SimpleFillSymbol Buildingscolor = new SimpleFillSymbol(Color.orange);
		final static SimpleFillSymbol Storagecolor = new SimpleFillSymbol(new Color(139,69,19));
		final static SimpleLineSymbol Pipecolor = new SimpleLineSymbol(Color.pink,3);
		final static SimpleFillSymbol Roadcolor = new SimpleFillSymbol(Color.gray);
		// power grid
		final static SimpleFillSymbol PowerGencolor = new SimpleFillSymbol(Color.red);
		final static SimpleLineSymbol UHTLinescolor = new SimpleLineSymbol(Color.green, 3);
		final static SimpleMarkerSymbol UHTSubstationcolor = new SimpleMarkerSymbol(Color.red, 20, Style.CROSS);
		final static SimpleLineSymbol EHTLinescolor = new SimpleLineSymbol(new Color(204,204,0), 3);
		final static SimpleMarkerSymbol EHTSubstationcolor = new SimpleMarkerSymbol(Color.blue, 20, Style.CIRCLE);
		final static SimpleLineSymbol HTLinescolor = new SimpleLineSymbol(new Color(0,100,0), 3);
		final static SimpleMarkerSymbol LoadPointscolor = new SimpleMarkerSymbol(new Color(127,0,255), 10, Style.DIAMOND);
		final static SimpleMarkerSymbol BusCouplercolor = new SimpleMarkerSymbol(Color.magenta, 10, Style.TRIANGLE);
		// chemical plant
		final static SimpleMarkerSymbol ChemProcesscolor = new SimpleMarkerSymbol(Color.red, 20, Style.CIRCLE);
		final static SimpleFillSymbol PlantReactorcolor = new SimpleFillSymbol(Color.pink);
		final static SimpleFillSymbol Decantercolor = new SimpleFillSymbol(Color.cyan);
		final static SimpleFillSymbol Extractorcolor = new SimpleFillSymbol(new Color(225,134,225));
		final static SimpleFillSymbol FlashDrumcolor = new SimpleFillSymbol(Color.gray);
		final static SimpleFillSymbol Mixercolor = new SimpleFillSymbol(new Color(225,225,20));
		final static SimpleFillSymbol RadFraccolor = new SimpleFillSymbol(new Color(0,150,30));
		final static SimpleFillSymbol PlantBackgroundcolor = new SimpleFillSymbol(new Color(230,230,230));
		final static SimpleLineSymbol GasLinecolor = new SimpleLineSymbol(Color.black, 3);
		final static SimpleLineSymbol AirLinecolor = new SimpleLineSymbol(new Color(200,100,0), 3);
		final static SimpleLineSymbol EnergyStreamcolor = new SimpleLineSymbol(new Color(250,0,250), 3);
		final static SimpleLineSymbol MaterialLinecolor = new SimpleLineSymbol(Color.red, 3);
		final static SimpleLineSymbol WaterLinecolor = new SimpleLineSymbol(Color.blue, 3);
		final static SimpleLineSymbol ElectricityLinecolor = new SimpleLineSymbol(Color.orange, 3);
		final static SimpleFillSymbol Exchangercolor = new SimpleFillSymbol(new Color(100,100,30));
		final static SimpleFillSymbol pumpcolor = new SimpleFillSymbol(new Color(200,100,30));
		final static SimpleFillSymbol blowercolor = new SimpleFillSymbol(new Color(100,50,30));
		final static SimpleFillSymbol valvecolor = new SimpleFillSymbol(new Color(40,130,30));
		final static SimpleFillSymbol splittercolor = new SimpleFillSymbol(new Color(130,20,89));
		
	
	
	private JFrame window;
	private JMap map;
//	private JLayerTree jLayerTree;  //ZL-151207 add layertree
	public static JLayeredPane contentPane;
	// initialize layers
	public static ArcGISFeatureLayer Landlotslayer;
	public static ArcGISFeatureLayer Buildingslayer;
	public static ArcGISFeatureLayer Storagelayer;
	public static ArcGISFeatureLayer Pipelayer;
	public static ArcGISFeatureLayer Roadlayer;
	public static ArcGISFeatureLayer PowerGenlayer;
	public static ArcGISFeatureLayer UHTLineslayer;
	public static ArcGISFeatureLayer UHTSubstationlayer;
	public static ArcGISFeatureLayer EHTLineslayer;  
	public static ArcGISFeatureLayer EHTSubstationlayer;   
	public static ArcGISFeatureLayer HTLineslayer;
	public static ArcGISFeatureLayer LoadPointslayer;
	public static ArcGISFeatureLayer BusCouplerlayer;
	public static ArcGISFeatureLayer ChemProcesslayer;
	public static ArcGISFeatureLayer PlantReactorlayer;
	public static ArcGISFeatureLayer Decanterlayer;
	public static ArcGISFeatureLayer Extractorlayer;
	public static ArcGISFeatureLayer FlashDrumlayer;
	public static ArcGISFeatureLayer Mixerlayer;
	public static ArcGISFeatureLayer RadFraclayer;
	public static ArcGISFeatureLayer PlantBackgroundlayer;
	public static ArcGISFeatureLayer GasLinelayer;
	public static ArcGISFeatureLayer AirLinelayer;
	public static ArcGISFeatureLayer EnergyStreamlayer;
	public static ArcGISFeatureLayer MaterialLinelayer;
	public static ArcGISFeatureLayer WaterLinelayer;
	public static ArcGISFeatureLayer ElectricityLinelayer;
	public static ArcGISFeatureLayer Exchangerlayer;
	public static ArcGISFeatureLayer pumplayer;
	public static ArcGISFeatureLayer blowerlayer;
	public static ArcGISFeatureLayer valvelayer;
	public static ArcGISFeatureLayer splitterlayer;
	
 	public static String httpStringCSV = new String("D:/httpReq.CSV"); // (mjk, 151115) investigating structure of DataOutputStream object
 	public static String httpStringCSV1 = new String("D:/httpReq1.CSV"); // (ZL-151203) investigating structure of DataOutputStream object
 	public static String httpStringCSV2 = new String("D:/httpReq2.CSV"); // (ZL-151203) investigating structure of DataOutputStream object
	
	// method to render all layers in an array using a certain style (multiple layer renderer)
	private void createRenderer(LayerList layers, ArcGISFeatureLayer[] arrayoflayers, Symbol col) {
		for (ArcGISFeatureLayer layer : arrayoflayers) {
			layer.setRenderer(new SimpleRenderer(col));
			layers.add(layer);
		}
	}
	
  public JParkSim() {
	  
    // empty JMap constructor and add a tiled basemap layer
    map = new JMap();
    final LayerList layers = map.getLayers(); // object storing all the map layers (NOT AN ARRAY - use completelayerlist instead)
    ArcGISTiledMapServiceLayer tiledLayer = new ArcGISTiledMapServiceLayer("http://services.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer");
    layers.add(tiledLayer); // add basemap layer
    
    // map centered on Jurong Island
    Point mapCenter = new Point(11543665,141400);
    map.setExtent(new Envelope(mapCenter,7200,5400));
    map.addMapEventListener(new MapEventListenerAdapter() {
    	@Override
    	public void mapReady(MapEvent event) {
    		System.out.println("Map has finished loading");
    	}
    });
  
    // adds layers uploaded onto ArcGIS for Developers
    UserCredentials user = new UserCredentials();
    user.setUserAccount("jparksimulator", "c4tjpark"); // Access secure feature layer service using login username and password
    Landlotslayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/ArcGIS/rest/services/landlots2/FeatureServer/0", user);
    Buildingslayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/ArcGIS/rest/services/Buildings/FeatureServer/0", user);
    Storagelayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/ArcGIS/rest/services/wholestorage/FeatureServer/0", user);
    Pipelayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/ArcGIS/rest/services/pipelines/FeatureServer/0", user);
    Roadlayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/ArcGIS/rest/services/Road/FeatureServer/0", user);
    PowerGenlayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/arcgis/rest/services/PowerGen/FeatureServer/0", user);
    UHTLineslayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/arcgis/rest/services/UHT_Lines_(230kV)/FeatureServer/0", user);
    UHTSubstationlayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/arcgis/rest/services/UHT_Substation_(230_66kV)/FeatureServer/0", user);
    EHTLineslayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/arcgis/rest/services/EHT_Lines/FeatureServer/0", user);
    EHTSubstationlayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/arcgis/rest/services/EHT_Substation_(66_22kV)/FeatureServer/0", user);   
    HTLineslayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/arcgis/rest/services/HT_Lines/FeatureServer/0", user);
    LoadPointslayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/arcgis/rest/services/Load_Points/FeatureServer/0", user);
    BusCouplerlayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/arcgis/rest/services/Bus_Coupler/FeatureServer/0", user);
    ChemProcesslayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/arcgis/rest/services/MyMapService/FeatureServer/0", user);
    PlantBackgroundlayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/ArcGIS/rest/services/PlantBackground/FeatureServer/0", user);
    GasLinelayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/ArcGIS/rest/services/GasLine/FeatureServer/0", user);
    AirLinelayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/ArcGIS/rest/services/AirLine/FeatureServer/0", user);
    EnergyStreamlayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/ArcGIS/rest/services/EnergyStream/FeatureServer/0", user);
    MaterialLinelayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/ArcGIS/rest/services/MaterialLine/FeatureServer/0", user);
    WaterLinelayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/ArcGIS/rest/services/WaterLine/FeatureServer/0", user);
    ElectricityLinelayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/ArcGIS/rest/services/electricity_line/FeatureServer/0", user);
        PlantReactorlayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/ArcGIS/rest/services/PlantReactor/FeatureServer/0", user);
    Decanterlayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/ArcGIS/rest/services/Decanter/FeatureServer/0", user);
    Extractorlayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/ArcGIS/rest/services/Extractor/FeatureServer/0", user);
    FlashDrumlayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/ArcGIS/rest/services/FlashDrum/FeatureServer/0", user);
    Mixerlayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/ArcGIS/rest/services/Mixer/FeatureServer/0", user);
    RadFraclayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/ArcGIS/rest/services/RadFracs/FeatureServer/0", user);
    Exchangerlayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/ArcGIS/rest/services/heat_exchanger/FeatureServer/0", user);
    pumplayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/ArcGIS/rest/services/pump/FeatureServer/0", user);
    blowerlayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/ArcGIS/rest/services/blower/FeatureServer/0", user);
    valvelayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/ArcGIS/rest/services/valve/FeatureServer/0", user);
    splitterlayer = new ArcGISFeatureLayer("http://services6.arcgis.com/MXY8H7lIySnKUlD3/ArcGIS/rest/services/splitter/FeatureServer/0", user);
	// UPDATE THIS LIST whenever new layers are added: first layer is the bottom most layer *see currently known issues #3
    
	ArcGISFeatureLayer[] completeLayerList = {Landlotslayer, Buildingslayer, Storagelayer, Pipelayer, Roadlayer, PowerGenlayer, UHTLineslayer, UHTSubstationlayer,
			EHTLineslayer, EHTSubstationlayer, HTLineslayer, LoadPointslayer, BusCouplerlayer, ChemProcesslayer,PlantBackgroundlayer,
			GasLinelayer,AirLinelayer,EnergyStreamlayer,MaterialLinelayer,ElectricityLinelayer,WaterLinelayer,PlantReactorlayer,Decanterlayer,Extractorlayer,
			FlashDrumlayer,Mixerlayer,RadFraclayer,Exchangerlayer,pumplayer,blowerlayer,valvelayer,splitterlayer};

    // render layers
	createRenderer(layers, new ArcGISFeatureLayer [] {Landlotslayer}, Landlotscolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {Buildingslayer}, Buildingscolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {Storagelayer}, Storagecolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {Pipelayer}, Pipecolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {Roadlayer}, Roadcolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {PowerGenlayer}, PowerGencolor);   
    createRenderer(layers, new ArcGISFeatureLayer [] {UHTLineslayer}, UHTLinescolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {UHTSubstationlayer}, UHTSubstationcolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {EHTLineslayer}, EHTLinescolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {EHTSubstationlayer}, EHTSubstationcolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {HTLineslayer}, HTLinescolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {LoadPointslayer}, LoadPointscolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {BusCouplerlayer}, BusCouplercolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {ChemProcesslayer}, ChemProcesscolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {PlantBackgroundlayer}, PlantBackgroundcolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {GasLinelayer}, GasLinecolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {AirLinelayer}, AirLinecolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {EnergyStreamlayer}, EnergyStreamcolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {MaterialLinelayer}, MaterialLinecolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {WaterLinelayer}, WaterLinecolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {ElectricityLinelayer}, ElectricityLinecolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {PlantReactorlayer}, PlantReactorcolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {Decanterlayer}, Decantercolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {Extractorlayer}, Extractorcolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {FlashDrumlayer}, FlashDrumcolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {Mixerlayer}, Mixercolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {RadFraclayer}, RadFraccolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {Exchangerlayer}, Exchangercolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {pumplayer}, pumpcolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {blowerlayer}, blowercolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {valvelayer}, valvecolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {splitterlayer}, splittercolor);
    
    // initialize window
    window = new JFrame("J-Park Simulator");
    window.setSize(1200, 900);
    window.setLocationRelativeTo(null); // centered on screen
    window.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    window.getContentPane().setLayout(new BorderLayout(0, 0));
    
	// create panel to select layer to edit
    JPanel panel = new JPanel();
    panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
    panel.setSize(220, 80);
    panel.setLocation(260, 10); // located near top left next to legend
    
    // create text
    JTextArea description = new JTextArea("Click on a feature to start editing");
    description.setFont(new Font("Verdana", Font.PLAIN, 11));
    description.setForeground(Color.WHITE);
    description.setBackground(new Color(0, 0, 0, 0));
    description.setEditable(false);
    description.setLineWrap(true);
    description.setWrapStyleWord(true);
    description.setAlignmentX(Component.LEFT_ALIGNMENT);
    
    // create label for dropdown selector
    JLabel lblLayer = new JLabel("Select a feature to edit:");
    lblLayer.setForeground(Color.WHITE);
    lblLayer.setAlignmentX(Component.LEFT_ALIGNMENT);
    // create dropdown selector for layer via key-value pairs
    final Map<String, ArcGISFeatureLayer> editlayer = new LinkedHashMap<>();
    // dropdown options with key = String layer name and value = layer object
    editlayer.put("Landlot", Landlotslayer);
    editlayer.put("Building", Buildingslayer);
    editlayer.put("Storage", Storagelayer);
    editlayer.put("Pipelines", Pipelayer);
    editlayer.put("Public Road", Roadlayer);
    editlayer.put("PowerGen", PowerGenlayer);
    editlayer.put("UHT Line", UHTLineslayer);
    editlayer.put("UHT Substation", UHTSubstationlayer);
    editlayer.put("EHT Line", EHTLineslayer);
    editlayer.put("EHT Substation", EHTSubstationlayer);
    editlayer.put("HT Line", HTLineslayer);
    editlayer.put("Load Point", LoadPointslayer);
    editlayer.put("Bus Coupler", BusCouplerlayer);
    editlayer.put("Chem Process", ChemProcesslayer);
    editlayer.put("Background", PlantBackgroundlayer);
    editlayer.put("GasLine", GasLinelayer);
    editlayer.put("AirLine", AirLinelayer);
    editlayer.put("Energy Stream", EnergyStreamlayer);
    editlayer.put("Material Line", MaterialLinelayer);     
    editlayer.put("WaterLine", WaterLinelayer);
    editlayer.put("Electricity Line", ElectricityLinelayer);
    editlayer.put("ChemReactor", PlantReactorlayer);
    editlayer.put("Decanter", Decanterlayer);
    editlayer.put("Extractor", Extractorlayer);
    editlayer.put("FlashDrum", FlashDrumlayer);
    editlayer.put("Mixer", Mixerlayer);
    editlayer.put("RadFrac", RadFraclayer);
    editlayer.put("Heat Exchanger", Exchangerlayer);
    editlayer.put("Pump", pumplayer);
    editlayer.put("Blower", blowerlayer);
    editlayer.put("Valve", valvelayer);
    editlayer.put("Splitter", splitterlayer);

    final JComboBox<String> cbxLayer = new JComboBox<>(editlayer.keySet().toArray(new String[0]));	// initialize dropdown box
    cbxLayer.setMaximumSize(new Dimension(220, 25));
    cbxLayer.setAlignmentX(Component.LEFT_ALIGNMENT);
    
    
    ArrayList<String[]> editStack = new ArrayList<String[]>();									// create a stack of edited features for PowerWorld to execute on
//	ArrayList<String[]> editStackDataset = new ArrayList<String[]>(); 							// (mjk, 151110) initialise a new variable to capture the field attributes directly from the popup window.
    																							// This would be the direct method involving the applet only, without using the servlet:

    HitTestOverlay[] listenerList = new HitTestOverlay[completeLayerList.length];				// container for listeners arranged by index of layers in completeLayerList
    for (ArcGISFeatureLayer layer : completeLayerList) {										// add event listener to all layers in completeLayerList
        final HitTestOverlay hitTestOverlay = new HitTestOverlay(layer);						// listener is of type HitTestOverlay
        hitTestOverlay.addHitTestListener(new HitTestListener() {								// listens for MOUSE CLICK on feature
        	@Override
        	public void featureHit(HitTestEvent event) {
    	        HitTestOverlay overlay = event.getOverlay();
    	        Graphic hitGraphic = (Graphic) overlay.getHitFeatures().get(overlay.getHitFeatures().size()-1); // get bottom-most graphic hit by mouse-click
    	        try {
    	        	PopupView contentPanel = PopupView.createEditView("Edit Attributes", layer); // create a popup in edit view
    	        	contentPanel.setGraphic(layer, hitGraphic);
    		          for (ArcGISFeatureLayer somelayer : completeLayerList) {					// highlight selected graphic and unselect previously selected graphic by searching all layers
    		        	  if (somelayer.getSelectedFeatures() != null) {						// layers outside map extent will produce null error, ignore these
	    			          for (Graphic graphic : somelayer.getSelectedFeatures()) {			// search for selected features
	    			        	  somelayer.unselect((int) graphic.getId());					// unselect them by graphic Id
	    			          }
    		        	  }
    		          }
    		          layer.select((int) hitGraphic.getId());													// highlight selected graphic
    		          final PopupDialog popup = map.createPopup(new JComponent[]{contentPanel}, hitGraphic);	// create map popup to display the popup view
    		          popup.setTitle("Edit Attributes: " + layer.getName());
    		          popup.setVisible(true);
    		          contentPanel.addPopupViewListener(new PopupViewListener() {
    		              @Override
    		              public void onCommitEdit(PopupViewEvent popupViewEvent, Feature feature) {			// save button
//    		            	  String[] newFeature = new String[] {layer.getName(), String.valueOf(hitGraphic.getAttributes().get("OBJECTID"))}; // newFeature is a new String[] element to be added to editStack (e.g. {Load_Points, 103})
    		            	  String[] newFeature = new String[] {layer.getName(), String.valueOf(hitGraphic.getAttributes().get("FID")), String.valueOf(hitGraphic.getAttributes().get("OBJECTID"))};  //ZL-151209 try to get FID and OBJECTID 
//    		            	  String[] paramFeature = new String[] {layer.getName(), String.valueOf(hitGraphic.getAttributes())};
//    		            	  System.out.println("newFeature[0]=" + newFeature[0] + ", newFeature[1]=" + newFeature[1]"); //ZL-151209
    		            	  System.out.println("newFeature[0]=" + newFeature[0] + ", newFeature[1]=" + newFeature[1] +", newFeature[2]=" + newFeature[2]); //ZL-151209
//    		            	  System.out.println("paramFeature[0]=" + paramFeature[0] + ", paramFeature[1]" + paramFeature[1]);
    		            	  boolean addtoStack = true;
    		            	  System.out.println("editStack size=" + editStack.size());
    		            	  for (int i=0; i<editStack.size(); i++) {							// (mjk, 151120) check through (i) elements in editStack where (i) is the number of modified feature objects in the layers.
    		            		  String itemlayer = editStack.get(i)[0];
    		            		  String graphicFID = editStack.get(i)[1];
    		            		  String graphicOBJECTID = editStack.get(i)[2];
//    		            		  String appCallFlag = editStack.get(i)[2];   //ZL-151208
//    		            		  if (layer.getName().equals(itemlayer) && String.valueOf(hitGraphic.getAttributes().get("OBJECTID")).equals(graphicFID)) {     
    		            		  if (layer.getName().equals(itemlayer) && (String.valueOf(hitGraphic.getAttributes().get("FID")).equals(graphicFID)||String.valueOf(hitGraphic.getAttributes().get("OBJECTID")).equals(graphicOBJECTID))) {	  
    		            			  addtoStack = false; 												// if identical feature is found, don't add to editStack
    		            		  }
    		            	  }  		            		  
    		            	  if (addtoStack) {															// add only if not duplicate
    		            		  editStack.add(newFeature);
    		            	  }
    		            	  popup.close();
    		              }
    		              @Override
    		              public void onCancelEdit(PopupViewEvent popupViewEvent, Feature feature) {	// cancel button
    		            	  layer.unselect((int) hitGraphic.getId());									// unselect feature on cancel
    		            	  popup.close();
    		              }
    		            });
    		          } catch (Exception e) {
    		            e.printStackTrace();
    		          }
        	}
        });
        hitTestOverlay.setActive(false);															// disable all listeners initially to prevent conflict
        listenerList[Arrays.asList(completeLayerList).indexOf(layer)] = hitTestOverlay;				// add listener(overlay) to an array arranged in indexed order
        map.addMapOverlay(hitTestOverlay);															// add all layer listeners to map
    }
    listenerList[0].setActive(true);																// default layer listener enabled is the first one (landlots layer)
    
    cbxLayer.addItemListener(new ItemListener() {													// dropdown list event listener
      @Override
      public void itemStateChanged(ItemEvent arg0) {
        if (arg0.getStateChange() == ItemEvent.SELECTED) {											// whenever dropdown list changes
          for (ArcGISFeatureLayer somelayer : completeLayerList) {									// unselect all graphics
        	  if (somelayer.getSelectedFeatures() != null) {										// ignore layers outside map extent
		          for (Graphic graphic : somelayer.getSelectedFeatures()) {							// search for selected features
		        	  somelayer.unselect((int) graphic.getId());									// unselect them by graphic Id
		          }
        	  }
          }
          for (HitTestOverlay overlay : listenerList) {
        	  overlay.setActive(false); // disable all listeners everytime selected layer is changed (reset)
          }          
          ArcGISFeatureLayer chosenlayer = editlayer.get(cbxLayer.getSelectedItem());
          int index = Arrays.asList(completeLayerList).indexOf(chosenlayer); // get index of selected layer from completeLayerList
          listenerList[index].setActive(true); // enable the listener currently selected
        }
      }
    });

    // Run PowerWorld button
    JButton PWbutton = new JButton("Run PowerWorld");
    PWbutton.addActionListener(new ActionListener() {
    	@Override
    	public void actionPerformed(ActionEvent arg0) {
    		HttpURLConnection urlCon;
    		OutputStreamWriter out;
    		URL url;
    		try {
				url = new URL("http://www.jparksimulator.com/PWServlet/"); // URL of servlet
				urlCon = (HttpURLConnection) url.openConnection();
				urlCon.setRequestMethod("POST");
				urlCon.setDoOutput(true);
				String[] PWFIDs = null;   //ZL-151209 
				for(int i=0; i<editStack.size(); i++){  //ZL-151209
					PWFIDs = new String[] {editStack.get(i)[1]};	 //ZL-151209
				}
//				if (editStack.isEmpty()) {
				if (PWFIDs == null) {  //ZL-151209
					JOptionPane.showMessageDialog(null, "You did not edit any features for PowerWorld!");
				} else {
					out = new OutputStreamWriter(urlCon.getOutputStream(), "UTF-8");
					StringBuilder layers = new StringBuilder();
					StringBuilder FIDs = new StringBuilder();
					StringBuilder OBJECTIDs = new StringBuilder();
					StringBuilder appCallFlag = new StringBuilder(); // (mjk, 151115) creates a flag indicating which function has been called: PowerWorld, parameterised PW, AspenPlus, parameterised AP
					for (String[] item : editStack) { // create comma separated values
						layers.append(item[0]);
						layers.append(",");
						FIDs.append(item[1]);
						FIDs.append(",");
						OBJECTIDs.append(item[2]);
						OBJECTIDs.append(",");
						appCallFlag.append("PW");
						appCallFlag.append(",");
					}
					StringBuilder outputString = new StringBuilder();
					// Only URL encoded string values can be sent over a HTTP connection
					outputString.append(URLEncoder.encode("layers", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(layers.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("FIDs", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(FIDs.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("OBJECTIDs", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(OBJECTIDs.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("appCallFlag", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(appCallFlag.toString(), "UTF-8"));
					System.out.println("outputString=" + outputString);
					
					// Example of comma separated outputString is "layers=Load_Points,Load_Points,&FIDs=103,104,"
					DataOutputStream wr = new DataOutputStream(urlCon.getOutputStream());
					wr.writeBytes(outputString.toString()); // write query string into servlet doPost() method

					FileWriter httpString = null; // (mjk, 151115) testing structure of DataOutputStream object and of wr object
					httpString = new FileWriter(httpStringCSV);
					httpString.append("wr=");
					httpString.append(outputString.toString());
					httpString.flush();				
					httpString.close();

					wr.flush();
					wr.close();
					
					if (urlCon.getResponseCode()==200) {
						JOptionPane.showMessageDialog(null, "PowerWorld has finished running!");
						editStack.clear(); // delete all items in editStack
					} else {
						JOptionPane.showMessageDialog(null, "An error has occurred. HTTP Error: " + urlCon.getResponseCode()
								+ "\nPlease try running PowerWorld again");
					}
					out.close();
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
    		for (ArcGISFeatureLayer layer : completeLayerList) {
    			layer.requery();
    			layer.refresh();
    		}
    	}
    });
    PWbutton.setEnabled(true);
    PWbutton.setVisible(true);
    PWbutton.setSize(150,30);
    PWbutton.setLocation(490, 10);

    
    // Run Parameterised PowerWorld button
    JButton PWPrButton = new JButton("Run Parameterised PW");
    PWPrButton.addActionListener(new ActionListener() {
    	@Override
    	public void actionPerformed(ActionEvent arg0) {
    		HttpURLConnection urlCon;
    		OutputStreamWriter out;
    		URL url;
    		try {
				url = new URL("http://www.jparksimulator.com/PWServlet/"); // URL of servlet
				urlCon = (HttpURLConnection) url.openConnection();
				urlCon.setRequestMethod("POST");
				urlCon.setDoOutput(true);
				
				if (editStack.isEmpty()) {
					JOptionPane.showMessageDialog(null, "You did not edit any features for PowerWorld!");
				} else {
					out = new OutputStreamWriter(urlCon.getOutputStream(), "UTF-8");
					StringBuilder layers = new StringBuilder();
					StringBuilder FIDs = new StringBuilder();
					StringBuilder OBJECTIDs = new StringBuilder();
					StringBuilder appCallFlag = new StringBuilder();
					for (String[] item : editStack) { // create comma separated values
						layers.append(item[0]);
						layers.append(",");
						FIDs.append(item[1]);
						FIDs.append(",");
						OBJECTIDs.append(item[2]);
						OBJECTIDs.append(",");
						appCallFlag.append("PWPr");
						appCallFlag.append(",");
					}
					StringBuilder outputString = new StringBuilder();
					// Only URL encoded string values can be sent over a HTTP connection
					outputString.append(URLEncoder.encode("layers", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(layers.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("FIDs", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(FIDs.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("OBJECTIDs", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(OBJECTIDs.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("appCallFlag", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(appCallFlag.toString(), "UTF-8"));
					System.out.println("outputString=" + outputString);
					
					// Example of comma separated outputString is "layers=Load_Points,Load_Points,&FIDs=103,104,"
					DataOutputStream wr = new DataOutputStream(urlCon.getOutputStream());
					wr.writeBytes(outputString.toString()); // write query string into servlet doPost() method
					
					FileWriter httpString = null; // (mjk, 151115) testing structure of DataOutputStream object and of wr object
					httpString = new FileWriter(httpStringCSV2);
					httpString.append("wr=");
					httpString.append(outputString.toString());
					httpString.flush();				
					httpString.close();
					
					wr.flush();
					wr.close();
					
					if (urlCon.getResponseCode()==200) {
						JOptionPane.showMessageDialog(null, "Parameterised PW has finished running!");
						editStack.clear(); // delete all items in editStack
					} else {
						JOptionPane.showMessageDialog(null, "An error has occurred. HTTP Error: " + urlCon.getResponseCode()
								+ "\nPlease try running Parameterised PW again");
					}
					out.close();
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
    		for (ArcGISFeatureLayer layer : completeLayerList) {
    			layer.requery();
    			layer.refresh();
    		}
    	}
    });
    PWPrButton.setEnabled(true);
    PWPrButton.setVisible(true);
    PWPrButton.setSize(170,30);
    PWPrButton.setLocation(650, 10);

    
    // Run AspenPlus button
    JButton APbutton = new JButton("Run AspenPlus");
    APbutton.addActionListener(new ActionListener() {
    	@Override
    	public void actionPerformed(ActionEvent arg0) {
    		HttpURLConnection urlCon;
    		OutputStreamWriter out;
    		URL url;
    		try {
//				url = new URL("http://www.jparksimulator.com/APServlet/"); // URL of servlet
				url = new URL("http://www.jparksimulator.com/PWServlet/");  //ZL-151203  
				urlCon = (HttpURLConnection) url.openConnection();
				urlCon.setRequestMethod("POST");
				urlCon.setDoOutput(true);
				
				if (editStack.isEmpty()) {
					JOptionPane.showMessageDialog(null, "You did not edit any features for AspenPlus!");
				} else {
					
					out = new OutputStreamWriter(urlCon.getOutputStream(), "UTF-8");
					StringBuilder layers = new StringBuilder();
					StringBuilder FIDs = new StringBuilder();
					StringBuilder OBJECTIDs = new StringBuilder();
					StringBuilder appCallFlag = new StringBuilder();
					for (String[] item : editStack) { // create comma separated values
						layers.append(item[0]);
						layers.append(",");
						FIDs.append(item[1]);
						FIDs.append(",");
						OBJECTIDs.append(item[2]);
						OBJECTIDs.append(",");
						appCallFlag.append("AP");
						appCallFlag.append(",");
					}
					StringBuilder outputString = new StringBuilder();
					// Only URL encoded string values can be sent over a HTTP connection
					outputString.append(URLEncoder.encode("layers", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(layers.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("FIDs", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(FIDs.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("OBJECTIDs", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(OBJECTIDs.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("appCallFlag", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(appCallFlag.toString(), "UTF-8"));
					System.out.println("outputString="+outputString);
					
					// Example of comma separated outputString is "layers=Load_Points,Load_Points,&FIDs=103,104,"
					DataOutputStream wr = new DataOutputStream(urlCon.getOutputStream());
					wr.writeBytes(outputString.toString()); // write query string into servlet doPost() method
					
					FileWriter httpString = null; // (mjk, 151115) testing structure of DataOutputStream object and of wr object
					httpString = new FileWriter(httpStringCSV1);
					httpString.append("wr=");
					httpString.append(outputString.toString());
					httpString.flush();				
					httpString.close();

					wr.flush();
					wr.close();
					
					if (urlCon.getResponseCode()==200) {
						JOptionPane.showMessageDialog(null, "AspenPlus has finished running!");
						editStack.clear(); // delete all items in editStack
					} else {
						JOptionPane.showMessageDialog(null, "An error has occurred. HTTP Error: " + urlCon.getResponseCode()
								+ "\nPlease try running AspenPlus again");
					}
					out.close();
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
    		for (ArcGISFeatureLayer layer : completeLayerList) {
    			layer.requery();
    			layer.refresh();
    		}
    	}
    });
    APbutton.setEnabled(true);
    APbutton.setVisible(true);
    APbutton.setSize(150,30);
    APbutton.setLocation(490, 60);    


    // Run Parameterised AspenPlus button
    JButton APPrButton = new JButton("Run Parameterised AP");
    APPrButton.addActionListener(new ActionListener() {
    	@Override
    	public void actionPerformed(ActionEvent arg0) {
    		HttpURLConnection urlCon;
    		OutputStreamWriter out;
    		URL url;
    		try {
				url = new URL("http://www.jparksimulator.com/APServlet/"); // URL of servlet
				urlCon = (HttpURLConnection) url.openConnection();
				urlCon.setRequestMethod("POST");
				urlCon.setDoOutput(true);
				
				if (editStack.isEmpty()) {
					JOptionPane.showMessageDialog(null, "You did not edit any features for AspenPlus!");
				} else {
					out = new OutputStreamWriter(urlCon.getOutputStream(), "UTF-8");
					StringBuilder layers = new StringBuilder();
					StringBuilder FIDs = new StringBuilder();
					for (String[] item : editStack) { // create comma separated values
						layers.append(item[0]);
						layers.append(",");
						FIDs.append(item[1]);
						FIDs.append(",");
					}
					StringBuilder outputString = new StringBuilder();
					// Only URL encoded string values can be sent over a HTTP connection
					outputString.append(URLEncoder.encode("layers", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(layers.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("FIDs", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(FIDs.toString(), "UTF-8"));
					
					// Example of comma separated outputString is "layers=Load_Points,Load_Points,&FIDs=103,104,"
					DataOutputStream wr = new DataOutputStream(urlCon.getOutputStream());
					wr.writeBytes(outputString.toString()); // write query string into servlet doPost() method
					wr.flush();
					wr.close();
					
					if (urlCon.getResponseCode()==200) {
						JOptionPane.showMessageDialog(null, "Parameterised AP has finished running!");
						editStack.clear(); // delete all items in editStack
					} else {
						JOptionPane.showMessageDialog(null, "An error has occurred. HTTP Error: " + urlCon.getResponseCode()
								+ "\nPlease try running Parameterised AP again");
					}
					out.close();
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
    		for (ArcGISFeatureLayer layer : completeLayerList) {
    			layer.requery();
    			layer.refresh();
    		}
    	}
    });
    APPrButton.setEnabled(true);
    APPrButton.setVisible(true);
    APPrButton.setSize(170,30);
    APPrButton.setLocation(650, 60);


    JButton refreshButton = new JButton("Refresh Map");
    refreshButton.addActionListener(new ActionListener() {
    	@Override
    	public void actionPerformed(ActionEvent arg0) {
    		for (ArcGISFeatureLayer layer : completeLayerList) {
    			layer.requery();
    			layer.refresh();
    		}
    	}
    });
    refreshButton.setEnabled(true);
    refreshButton.setVisible(true);
    refreshButton.setSize(150,30);
    refreshButton.setLocation(830, 10);

    
    // combine text, label and dropdown list into one panel for selecting layer to edit
    panel.setBackground(new Color(0, 0, 0, 180));
    panel.add(description);
    panel.add(Box.createRigidArea(new Dimension(0, 5)));
    panel.add(lblLayer);
    panel.add(Box.createRigidArea(new Dimension(0, 5)));
    panel.add(cbxLayer);
    panel.add(Box.createRigidArea(new Dimension(0, 5)));
    panel.setBorder(BorderFactory.createEmptyBorder(5, 10, 5, 10));
    
    // create legend
    JLegend legend = new JLegend(map);
    legend.setPreferredSize(new Dimension(250, 700));
    legend.setBorder(new LineBorder(new Color(205, 205, 255), 3));
    
    // initialize contentPane and add contents
    contentPane = new JLayeredPane();
    contentPane.setLayout(new BorderLayout(0,0));
    contentPane.setVisible(true);
    contentPane.add(PWbutton);
    contentPane.add(PWPrButton); 
    contentPane.add(APbutton);
    contentPane.add(APPrButton);    
    contentPane.add(refreshButton);
    contentPane.add(panel);
    contentPane.add(legend, BorderLayout.WEST);
    contentPane.add(map, BorderLayout.CENTER);
    window.add(contentPane);
    
    // dispose map just before application window is closed.
    window.addWindowListener(new WindowAdapter() {
      @Override
      public void windowClosing(WindowEvent windowEvent) {
    	  if (!editStack.isEmpty()) { // check if ArcGIS edits have not been saved in PowerWorld .pwb file
        	  int reply = JOptionPane.showConfirmDialog(null, "ArcGIS edits may have not been saved in PowerWorld or AspenPlus. "
        	  		+ "Are you sure you want to close?", "Window Closing", JOptionPane.YES_NO_OPTION);
        	  if (reply == JOptionPane.YES_OPTION) {
    	        super.windowClosing(windowEvent);
    	        map.dispose();
    	        window.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE); // confirm close window
        	  } else { // if reply is NO
        		  window.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE); // don't close window
        	  }
    	  }
      }
    });
  } // of public JParkSim()
  
  /**
   * Starting point of this application.
   * @param args
   */
  public static void main(String[] args) { // main function invoked
    EventQueue.invokeLater(new Runnable() {

      @Override
      public void run() {
        try {
          JParkSim application = new JParkSim(); // instance of this application
          application.window.setVisible(true);
          ArcGISRuntime.setClientID("aSg9q12qgnN4OQq2"); // license app
        } catch (Exception e) {
          e.printStackTrace();
        }
      }
    });
  }
}





//sdvgfgfsd
//fdbgb