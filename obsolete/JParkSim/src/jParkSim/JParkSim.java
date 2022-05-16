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
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeListener;
import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
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
import javax.swing.JTextField;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import javax.swing.border.LineBorder;

//import MouseDrag.MouseDrag;
//import queryWindow.QueryWindow;

import com.esri.core.geometry.Envelope;
import com.esri.core.geometry.MultiPoint;
import com.esri.core.geometry.Point;
import com.esri.core.geometry.Polygon;
import com.esri.core.io.UserCredentials;
import com.esri.core.map.Feature;
import com.esri.core.map.Graphic;
import com.esri.map.TimeAwareLayer;
import com.esri.core.map.TimeOptions.Units;
import com.esri.core.portal.Portal;
import com.esri.core.portal.WebMap;
import com.esri.core.renderer.SimpleRenderer;
import com.esri.core.symbol.SimpleFillSymbol;
import com.esri.core.symbol.SimpleLineSymbol;
import com.esri.core.symbol.SimpleMarkerSymbol;
import com.esri.core.symbol.SimpleMarkerSymbol.Style;
import com.esri.core.symbol.Symbol;
import com.esri.map.ArcGISDynamicMapServiceLayer;
import com.esri.map.ArcGISFeatureLayer;
import com.esri.map.ArcGISTiledMapServiceLayer;
import com.esri.map.GraphicsLayer;
import com.esri.map.GroupLayer;
import com.esri.map.JMap;
import com.esri.map.Layer;
import com.esri.map.LayerEvent;
import com.esri.map.LayerInitializeCompleteEvent;
import com.esri.map.LayerInitializeCompleteListener;
import com.esri.map.LayerList;
import com.esri.map.LayerListEventListenerAdapter;
import com.esri.map.MapEvent;
import com.esri.map.MapEventListenerAdapter;
import com.esri.map.popup.PopupDialog;
import com.esri.map.popup.PopupView;
import com.esri.map.popup.PopupViewEvent;
import com.esri.map.popup.PopupViewListener;
import com.esri.runtime.ArcGISRuntime;
import com.esri.toolkit.legend.JLegend;
import com.esri.toolkit.overlays.HitTestEvent;
import com.esri.toolkit.overlays.HitTestListener;
import com.esri.toolkit.overlays.HitTestOverlay;
import com.esri.toolkit.overlays.InfoPopupOverlay;
import com.esri.toolkit.sliders.JTimeSlider;
import com.esri.toolkit.sliders.JTimeSlider.TimeMode;
import com.esri.core.symbol.PictureMarkerSymbol;


public class JParkSim {
	
	//declare the variable which contains the modification of each layer, such as the color, thickness, icon
	
	// style of different layers
		final static SimpleFillSymbol Landlotscolor = new SimpleFillSymbol(Color.cyan, new SimpleLineSymbol(Color.cyan, 1), SimpleFillSymbol.Style.NULL);
		final static SimpleFillSymbol Buildingscolor = new SimpleFillSymbol(Color.orange);
		final static SimpleFillSymbol Storagecolor = new SimpleFillSymbol(new Color(139,69,19));
		final static SimpleFillSymbol Roadcolor = new SimpleFillSymbol(Color.gray);
		// power grid
		final static SimpleFillSymbol PowerGencolor = new SimpleFillSymbol(Color.red);
		final static SimpleLineSymbol UHTLinescolor = new SimpleLineSymbol(Color.green, 3);
		final static SimpleMarkerSymbol UHTSubstationcolor = new SimpleMarkerSymbol(Color.red, 20, Style.CROSS);
		final static SimpleLineSymbol EHTLinescolor = new SimpleLineSymbol(new Color(204,204,0), 3);
		final static SimpleMarkerSymbol EHTSubstationcolor = new SimpleMarkerSymbol(Color.blue, 20, Style.X);
		final static SimpleLineSymbol HTLinescolor = new SimpleLineSymbol(new Color(0,100,0), 3);
		final static SimpleMarkerSymbol HTSubstation1color = new SimpleMarkerSymbol(Color.green, 15, Style.CIRCLE);
		final static SimpleMarkerSymbol HTSubstation2color = new SimpleMarkerSymbol(Color.black, 15, Style.CIRCLE);
		final static SimpleMarkerSymbol LTSubstation1color = new SimpleMarkerSymbol(Color.cyan, 15, Style.SQUARE);
		final static SimpleMarkerSymbol LTSubstation2color = new SimpleMarkerSymbol(Color.gray, 15, Style.SQUARE);
		final static SimpleMarkerSymbol LoadPointscolor = new SimpleMarkerSymbol(new Color(127,0,255), 10, Style.DIAMOND);
		final static SimpleMarkerSymbol BusCouplercolor = new SimpleMarkerSymbol(Color.magenta, 10, Style.TRIANGLE);
		final static SimpleLineSymbol TLPmaincolor = new SimpleLineSymbol(Color.pink,3);
		final static SimpleLineSymbol TLP2color = new SimpleLineSymbol(new Color(124,252,0), 3,com.esri.core.symbol.SimpleLineSymbol.Style.DOT );
		final static SimpleLineSymbol TLP3color = new SimpleLineSymbol(Color.magenta, 3,com.esri.core.symbol.SimpleLineSymbol.Style.DOT );
		final static SimpleLineSymbol TLP4color = new SimpleLineSymbol(new Color(153,51,255), 3,com.esri.core.symbol.SimpleLineSymbol.Style.DOT );
		final static SimpleLineSymbol TLP2acolor = new SimpleLineSymbol(new Color(0,128,128), 3,com.esri.core.symbol.SimpleLineSymbol.Style.DOT );
		
		// chemical plant
		final static SimpleFillSymbol PlantReactorcolor = new SimpleFillSymbol(Color.pink);
		final static SimpleFillSymbol Decantercolor = new SimpleFillSymbol(Color.cyan);
		final static SimpleFillSymbol Extractorcolor = new SimpleFillSymbol(new Color(225,134,225));
		final static SimpleFillSymbol FlashDrumcolor = new SimpleFillSymbol(Color.gray);
		final static SimpleFillSymbol Mixercolor = new SimpleFillSymbol(Color.blue);
		final static SimpleFillSymbol RadFraccolor = new SimpleFillSymbol(new Color(0,150,30));
		final static SimpleFillSymbol heatercoolercolor = new SimpleFillSymbol(Color.red);
		final static SimpleLineSymbol GasLinecolor = new SimpleLineSymbol(Color.black, 3);
		final static SimpleLineSymbol Fluidcolor = new SimpleLineSymbol(new Color(218,165,32), 3);
		final static SimpleLineSymbol AirLinecolor = new SimpleLineSymbol(new Color(200,100,0), 3);
		final static SimpleLineSymbol EnergyStreamcolor = new SimpleLineSymbol(Color.darkGray, 2, com.esri.core.symbol.SimpleLineSymbol.Style.DASH );
		final static SimpleLineSymbol MaterialLinecolor = new SimpleLineSymbol(Color.red, 3);
		final static SimpleLineSymbol WaterLinecolor = new SimpleLineSymbol(Color.blue, 3);
		final static SimpleFillSymbol Exchangercolor = new SimpleFillSymbol(new Color(100,100,30));
		final static SimpleFillSymbol pumpcolor = new SimpleFillSymbol(new Color(200,100,30));
		final static SimpleFillSymbol blowercolor = new SimpleFillSymbol(new Color(100,50,30));
		final static SimpleFillSymbol valvecolor = new SimpleFillSymbol(new Color(40,130,30));
		final static SimpleFillSymbol splittercolor = new SimpleFillSymbol(new Color(130,20,89));
		final static SimpleFillSymbol vesselcolor = new SimpleFillSymbol(Color.magenta);
		final static SimpleFillSymbol filtercolor = new SimpleFillSymbol(new Color(204,255,153));
		final static SimpleFillSymbol expandercolor = new SimpleFillSymbol(new Color(219,112,147));
		final static SimpleFillSymbol compressorcolor = new SimpleFillSymbol(Color.white);
		final static SimpleLineSymbol steamcolor = new SimpleLineSymbol(new Color(128,0,128), 3);
		final static PictureMarkerSymbol intersectioncolor = new PictureMarkerSymbol("http://static.arcgis.com/images/Symbols/Animated/EnlargeRotatingWhiteMarkerSymbol.png");
		final static PictureMarkerSymbol waterpointcolor = new PictureMarkerSymbol("http://static.arcgis.com/images/Symbols/Animated/EnlargeGradientSymbol.png");
		final static SimpleLineSymbol waternetworkcolor = new SimpleLineSymbol(new Color(0,0,128), 3);
		private JTimeSlider timeSlider;

		
		
	
	private JFrame window;
	private JMap map;
	//try to put new variable
	
	private static GraphicsLayer graphicsLayer;
	final GraphicsLayer graphicsLayer_polygon = new GraphicsLayer();
	
	private MultiPoint planes;
	
				
	//if want to add new map
	  private HashMap<String, String> idMap;
	  private JComboBox mapIds;
	  private Portal arcgisPortal = new Portal("https://www.arcgis.com", null);
	  
//	private JLayerTree jLayerTree;  //ZL-151207 add layertree
	public static JLayeredPane contentPane;
		
	
	// initialize layers for all the arcgis feature layers
	public static ArcGISFeatureLayer Landlotslayer;
	public static ArcGISFeatureLayer Buildingslayer;
	public static ArcGISFeatureLayer Storagelayer;
	public static ArcGISFeatureLayer TLPmainlayer;
	public static ArcGISFeatureLayer Roadlayer;
	public static ArcGISFeatureLayer PowerGenlayer;
	public static ArcGISFeatureLayer UHTLineslayer;
	public static ArcGISFeatureLayer UHTSubstationlayer;
	public static ArcGISFeatureLayer EHTLineslayer;  
	public static ArcGISFeatureLayer EHTSubstationlayer;   
	public static ArcGISFeatureLayer HTLineslayer;
	public static ArcGISFeatureLayer HTSubstation1layer; 
	public static ArcGISFeatureLayer HTSubstation2layer;
	public static ArcGISFeatureLayer LTSubstation1layer;
	public static ArcGISFeatureLayer LTSubstation2layer;
	public static ArcGISFeatureLayer LoadPointslayer;
	public static ArcGISFeatureLayer BusCouplerlayer;
	
	public static ArcGISFeatureLayer PlantReactorlayer;
	public static ArcGISFeatureLayer Decanterlayer;
	public static ArcGISFeatureLayer Extractorlayer;
	public static ArcGISFeatureLayer FlashDrumlayer;
	public static ArcGISFeatureLayer Mixerlayer;
	public static ArcGISFeatureLayer RadFraclayer;
	public static ArcGISFeatureLayer heatercoolerlayer;
	public static ArcGISFeatureLayer GasLinelayer;
	public static ArcGISFeatureLayer AirLinelayer;
	public static ArcGISFeatureLayer Fluidlayer;
	public static ArcGISFeatureLayer EnergyStreamlayer;
	public static ArcGISFeatureLayer MaterialLinelayer;
	public static ArcGISFeatureLayer WaterLinelayer;
	public static ArcGISFeatureLayer TLP2layer;
	public static ArcGISFeatureLayer TLP3layer;
	public static ArcGISFeatureLayer TLP2alayer;
	public static ArcGISFeatureLayer TLP4layer;
	public static ArcGISFeatureLayer Exchangerlayer;
	public static ArcGISFeatureLayer pumplayer;
	public static ArcGISFeatureLayer blowerlayer;
	public static ArcGISFeatureLayer valvelayer;
	public static ArcGISFeatureLayer splitterlayer;
	public static ArcGISFeatureLayer vessellayer;
	public static ArcGISFeatureLayer filterlayer;
	public static ArcGISFeatureLayer expanderlayer;
	public static ArcGISFeatureLayer compressorlayer;
	public static ArcGISFeatureLayer steamlayer;
	public static ArcGISFeatureLayer waterpointlayer;	
	public static ArcGISFeatureLayer waternetworklayer;
	public static ArcGISFeatureLayer intersectionlayer;
	
	
 	//public static String httpStringCSV = new String("D:/httpReq.CSV");              // investigating structure of DataOutputStream object
 	//public static String httpStringCSV1 = new String("D:/httpReq1.CSV");            // investigating structure of DataOutputStream object
 	//public static String httpStringCSV2 = new String("D:/httpReq2.CSV");            // investigating structure of DataOutputStream object
 	public static String GIS = new String("D:/JPS_Code_Test/GIS.CSV");              // csv file to store the GIS coordinate for the unit operations
 	public static String PlantGIS = new String("D:/JPS_Code_Test/PlantGIS.CSV");    // csv file to store the endpoints of the polygons for chemical plant
 	
	// method to render all layers in an array using a certain style (multiple layer renderer)
	private void createRenderer(LayerList layers, ArcGISFeatureLayer[] arrayoflayers, Symbol col) {
		for (ArcGISFeatureLayer layer : arrayoflayers) {
			layer.setRenderer(new SimpleRenderer(col));
			layers.add(layer);
		}
	}
	
	private JTimeSlider createTimeSlider(TimeAwareLayer layer) {
	    JTimeSlider jTimeSlider = new JTimeSlider();
	    jTimeSlider.setTitle("time animation for dispersion");
	    jTimeSlider.addLayer(layer);
	    jTimeSlider.setTimeMode(TimeMode.TimeExtent);
	    jTimeSlider.setPlaybackRate(1000); // 1 second per tick
	    jTimeSlider.setVisible(false);
	    return jTimeSlider;
	  }
	
	 private String wrap(String str) {
		    // create a HTML string that wraps text when longer
		    return "<html><p style='width:200px;'>" + str + "</html>";
		  }
	
	  
	
  public JParkSim() {
	  
	  
	  
	  
	  
    // empty JMap constructor and add a tiled basemap layer
    map = new JMap();
    final LayerList layers = map.getLayers(); // object storing all the map layers (NOT AN ARRAY - use completelayerlist instead)
    ArcGISTiledMapServiceLayer tiledLayer = new ArcGISTiledMapServiceLayer("http://services.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer");
    layers.add(tiledLayer); // add basemap layer


// dynamic map layer for the emission
ArcGISDynamicMapServiceLayer emissionLayer = new ArcGISDynamicMapServiceLayer(
            "http://10.25.188.100:6080/arcgis/rest/services/emission/MapServer");
                layers.add(emissionLayer);
                
              //add dynamic map layer for the sensitivity bar chart
                ArcGISDynamicMapServiceLayer dispersionLayer = new ArcGISDynamicMapServiceLayer(
                        "http://10.25.188.100:6080/arcgis/rest/services/gasdispersion/MapServer");
                            layers.add(dispersionLayer);
                            
                            ArcGISDynamicMapServiceLayer f = new ArcGISDynamicMapServiceLayer("");
    
                
    // map centered on Jurong Island
    Point mapCenter = new Point(11543665,141400);
    map.setExtent(new Envelope(mapCenter,7200,5400));
    map.addMapEventListener(new MapEventListenerAdapter() {
    	@Override
    	public void mapReady(MapEvent event) {
    		System.out.println("Map has finished loading");
    	}
    });
  
    // adds layers source uploaded onto ArcGIS for Developers 
    UserCredentials user = new UserCredentials();
    user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp"); // Access secure feature layer service using login username and password
    Landlotslayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/Landlots/FeatureServer/0", user);
    Buildingslayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/Building/FeatureServer/0", user);
    Storagelayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Storage/FeatureServer/0", user);
    TLPmainlayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/TLPlantmain/FeatureServer/0", user);
    Roadlayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/road/FeatureServer/0", user);
    PowerGenlayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Generators/FeatureServer/0", user);
    UHTLineslayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/UHTLines/FeatureServer/0", user);
    UHTSubstationlayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/UHT_substations/FeatureServer/0", user);
    EHTLineslayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/EHT_Lines/FeatureServer/0", user);
    EHTSubstationlayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/EHT_substation/FeatureServer/0", user);   
    HTLineslayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/HTLines/FeatureServer/0", user);
    HTSubstation1layer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/HTSubstation_22to11/FeatureServer/0", user);
    HTSubstation2layer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/HTSubstation_22to3_4/FeatureServer/0", user);
    LTSubstation1layer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/LTSubstation_3_4to3/FeatureServer/0", user);
    LTSubstation2layer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/LTSubstation_3to0_4/FeatureServer/0", user);
    
    LoadPointslayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/Load_points/FeatureServer/0", user);
    BusCouplerlayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/bus_couplers/FeatureServer/0", user);
    Fluidlayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Working_Fluid/FeatureServer/0", user);
    heatercoolerlayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/heater_cooler/FeatureServer/0", user);
    GasLinelayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/Gas_line/FeatureServer/0", user);
    AirLinelayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/airline/FeatureServer/0", user);
    EnergyStreamlayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/EnergyStream/FeatureServer/0", user);
    MaterialLinelayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/Material_line/FeatureServer/0", user);
    WaterLinelayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/water_line/FeatureServer/0", user);
    TLP2layer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/TLPlant2/FeatureServer/0", user);
    TLP3layer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/TLPlant3/FeatureServer/0", user);
    TLP2alayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/TLPlant2a/FeatureServer/0", user);
    TLP4layer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/TLPlant4/FeatureServer/0", user);
    PlantReactorlayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/Reactor/FeatureServer/0", user);
    Decanterlayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/Decanter/FeatureServer/0",user);
    Extractorlayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Extractor/FeatureServer/0", user);
    FlashDrumlayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Flashdrum/FeatureServer/0", user);
    Mixerlayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/Mixer/FeatureServer/0", user);
    RadFraclayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/RadFrac/FeatureServer/0", user);
    Exchangerlayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/exchanger/FeatureServer/0", user);
    pumplayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/Pump/FeatureServer/0", user);
    blowerlayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/blower/FeatureServer/0", user);
    valvelayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/valve/FeatureServer/0", user);
    splitterlayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/splitter/FeatureServer/0", user);
    vessellayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/vessel/FeatureServer/0", user);
    filterlayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/Filter/FeatureServer/0", user);
    expanderlayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/expander/FeatureServer/0", user);
    compressorlayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/compressor/FeatureServer/0", user);
    steamlayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/steam_interplants/FeatureServer/0", user);
    waterpointlayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/waterpoint/FeatureServer/0", user);
    waternetworklayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/WaterNetwork/FeatureServer/0", user);
    intersectionlayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/RoadIntersection/FeatureServer/0", user);
        
    
    // UPDATE THIS LIST whenever new layers are added: first layer is the bottom most layer *see currently known issues #3 (array containing all the feature layers)
    
	ArcGISFeatureLayer[] completeLayerList = {Landlotslayer, Buildingslayer, Storagelayer, TLPmainlayer, Roadlayer, PowerGenlayer, UHTLineslayer, UHTSubstationlayer,
			EHTLineslayer, EHTSubstationlayer, HTLineslayer,HTSubstation1layer,HTSubstation2layer,LTSubstation1layer,LTSubstation2layer, LoadPointslayer, BusCouplerlayer, heatercoolerlayer,
			GasLinelayer,AirLinelayer,EnergyStreamlayer,MaterialLinelayer,TLP2layer,TLP3layer,TLP2alayer,TLP4layer,WaterLinelayer,PlantReactorlayer,Decanterlayer,Extractorlayer,
			FlashDrumlayer,Mixerlayer,RadFraclayer,Exchangerlayer,pumplayer,blowerlayer,valvelayer,splitterlayer,vessellayer,filterlayer,Fluidlayer,expanderlayer,compressorlayer,steamlayer,waterpointlayer,waternetworklayer,intersectionlayer};

	
    // render layers ( to show the feature layers in the map combined with the modification format declared) 
	
    createRenderer(layers, new ArcGISFeatureLayer [] {Landlotslayer}, Landlotscolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {Buildingslayer}, Buildingscolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {Storagelayer}, Storagecolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {Roadlayer}, Roadcolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {intersectionlayer}, intersectioncolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {PowerGenlayer}, PowerGencolor);   
    createRenderer(layers, new ArcGISFeatureLayer [] {UHTLineslayer}, UHTLinescolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {UHTSubstationlayer}, UHTSubstationcolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {EHTLineslayer}, EHTLinescolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {EHTSubstationlayer}, EHTSubstationcolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {HTLineslayer}, HTLinescolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {HTSubstation1layer}, HTSubstation1color);
    createRenderer(layers, new ArcGISFeatureLayer [] {HTSubstation2layer}, HTSubstation2color);
    createRenderer(layers, new ArcGISFeatureLayer [] {TLPmainlayer}, TLPmaincolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {TLP2layer}, TLP2color);
    createRenderer(layers, new ArcGISFeatureLayer [] {TLP3layer}, TLP3color);
    createRenderer(layers, new ArcGISFeatureLayer [] {TLP2alayer}, TLP2acolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {TLP4layer}, TLP4color);
    createRenderer(layers, new ArcGISFeatureLayer [] {LTSubstation1layer}, LTSubstation1color);
    createRenderer(layers, new ArcGISFeatureLayer [] {LTSubstation2layer}, LTSubstation2color);
    createRenderer(layers, new ArcGISFeatureLayer [] {LoadPointslayer}, LoadPointscolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {BusCouplerlayer}, BusCouplercolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {heatercoolerlayer}, heatercoolercolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {GasLinelayer}, GasLinecolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {AirLinelayer}, AirLinecolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {EnergyStreamlayer}, EnergyStreamcolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {MaterialLinelayer}, MaterialLinecolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {WaterLinelayer}, WaterLinecolor);
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
    createRenderer(layers, new ArcGISFeatureLayer [] {vessellayer}, vesselcolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {filterlayer}, filtercolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {Fluidlayer}, Fluidcolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {expanderlayer}, expandercolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {compressorlayer}, compressorcolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {steamlayer}, steamcolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {waterpointlayer}, waterpointcolor);
    createRenderer(layers, new ArcGISFeatureLayer [] {waternetworklayer}, waternetworkcolor);
       
    //map.getLayers().add(graphlayer);
  //try to add some graphs
    
    //add dynamic map layer for the opex bar chart
    ArcGISDynamicMapServiceLayer highwayLayer = new ArcGISDynamicMapServiceLayer(
            "http://10.25.188.100:6080/arcgis/rest/services/opex/MapServer");
                layers.add(highwayLayer);
                
              //add dynamic map layer for the dispersion bar chart
                ArcGISDynamicMapServiceLayer dispersionanimationLayer = new ArcGISDynamicMapServiceLayer(
                        " 	http://10.25.188.100:6080/arcgis/rest/services/dispersion/MapServer");
                            layers.add(dispersionanimationLayer);
                            
                            dispersionanimationLayer.addLayerInitializeCompleteListener(new LayerInitializeCompleteListener() {

                              @Override
                              public void layerInitializeComplete(LayerInitializeCompleteEvent e) {
                                if (e.getID() == LayerInitializeCompleteEvent.LOCALLAYERCREATE_ERROR) {
                                  String errMsg = "Failed to initialize due to "
                                      + dispersionanimationLayer.getInitializationError();
                                  JOptionPane.showMessageDialog(map, wrap(errMsg), "",
                                      JOptionPane.ERROR_MESSAGE);
                                }
                              }
                            });
                            // create the time slider with the dynamic layer
                        timeSlider = createTimeSlider(dispersionanimationLayer);

                        // set time extent of time slider once the dynamic layer is initialized
                        dispersionanimationLayer.addLayerInitializeCompleteListener(new LayerInitializeCompleteListener() {

                          @Override
                          public void layerInitializeComplete(LayerInitializeCompleteEvent e) {
                            SwingUtilities.invokeLater(new Runnable() {
                              @Override
                              public void run() {
                                timeSlider.setTimeExtent(dispersionanimationLayer.getTimeInfo().getTimeExtent(), 1, Units.Seconds);
                                Calendar calendar = dispersionanimationLayer.getTimeInfo().getTimeExtent().getStartDate();
                                timeSlider.setTimeIntervalStart(calendar);
                                calendar.add(Calendar.SECOND,0);
                                timeSlider.setTimeIntervalEnd(calendar);
                                timeSlider.setVisible(true);
                                
                                //description.setVisible(true);
                                //legend.setVisible(true);
                              }
                            });
                          }
                        });
          
              //add dynamic map layer for the sensitivity bar chart
                ArcGISDynamicMapServiceLayer sensitivityLayer = new ArcGISDynamicMapServiceLayer(
                        "http://10.25.188.100:6080/arcgis/rest/services/sensitivity/MapServer");
                            layers.add(sensitivityLayer);
                            
                            final GraphicsLayer graphicsLayer2 = new GraphicsLayer();
                            graphicsLayer2.setName("simple query graphics");
                            map.addMapEventListener(new MapEventListenerAdapter() {
                              @Override
                              public void mapReady(final MapEvent arg0) {
                                SwingUtilities.invokeLater(new Runnable() {
                                  @Override
                                  public void run() {
                                    addSimpleFillGraphics(graphicsLayer2);
                                    
                                  }
                                });
                              }
                            });

                            //layers.add(graphicsLayer2);
                            
                            
                            
                          
                
                
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
    
    // command to switch the map 22/3/2016
    
    String[] mapStrings = {
        
    "cost of reactors"};
    String[] idStrings = {
         
    "f809dccb780a4af0a506e56aaa84d084"};
    idMap = new HashMap<>();
    for (int i = 0; i < idStrings.length; i++) {
      idMap.put(mapStrings[i], idStrings[i]);
    }

    mapIds = new JComboBox(mapStrings);
    mapIds.setSelectedIndex(0);
    mapIds.setAlignmentX(Component.LEFT_ALIGNMENT);

          
    
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
    
    // dropdown options with key = String layer name and value = layer object (list of the feature layers that can be edited and appear in the drop down box)
    editlayer.put("Landlot", Landlotslayer);
    editlayer.put("Building", Buildingslayer);
    editlayer.put("Public Road", Roadlayer);
    editlayer.put("Road Intersection", intersectionlayer);
    editlayer.put("Storage", Storagelayer);
    editlayer.put("Bus Coupler", BusCouplerlayer);
    editlayer.put("EHT Line", EHTLineslayer);
    editlayer.put("EHT Substation", EHTSubstationlayer);
    editlayer.put("HT Line", HTLineslayer);
    editlayer.put("HT Substation(22kV-11kV)", HTSubstation1layer);
    editlayer.put("HT Substation(22kV-3.4kV)", HTSubstation2layer);
    editlayer.put("Load Point", LoadPointslayer);
    editlayer.put("LT Substation(3.4kV-3kV)", LTSubstation1layer);
    editlayer.put("LT Substation(3kV-0.4kV)", LTSubstation2layer);
    editlayer.put("TLP(main-22kV)", TLPmainlayer);
    editlayer.put("TLP(22kV-11kV)", TLP2alayer);
    editlayer.put("TLP(22kV-3.4kV)", TLP2layer);
    editlayer.put("TLP(3.4kV-3kV)", TLP3layer);
    editlayer.put("TLP(3kV-0.4kV)", TLP4layer);
    editlayer.put("PowerGen", PowerGenlayer);
    editlayer.put("UHT Line", UHTLineslayer);
    editlayer.put("UHT Substation", UHTSubstationlayer);
    editlayer.put("AirLine", AirLinelayer);
    editlayer.put("Energy Stream", EnergyStreamlayer);
    editlayer.put("GasLine", GasLinelayer);
    editlayer.put("Material Line", MaterialLinelayer);     
    editlayer.put("WaterLine", WaterLinelayer);
    editlayer.put("Blower", blowerlayer);
    editlayer.put("compressor", compressorlayer);
    editlayer.put("Decanter", Decanterlayer);
    editlayer.put("Expander/Turbine", expanderlayer);
    editlayer.put("Extractor", Extractorlayer);
    editlayer.put("Filter", filterlayer);
    editlayer.put("FlashDrum", FlashDrumlayer);
    editlayer.put("Heater/Cooler", heatercoolerlayer);
    editlayer.put("Heat Exchanger", Exchangerlayer);
    editlayer.put("Mixer", Mixerlayer);
    editlayer.put("Pump", pumplayer);
    editlayer.put("RadFrac", RadFraclayer);
    editlayer.put("Reactor", PlantReactorlayer);
    editlayer.put("Splitter", splitterlayer);
    editlayer.put("Steam network", steamlayer);
    editlayer.put("Valve", valvelayer);
    editlayer.put("Vessel", vessellayer);
    editlayer.put("Working fluid", Fluidlayer);
    editlayer.put("Water Point", waterpointlayer);
    editlayer.put("Water network", waternetworklayer);
    
    
      
    final JComboBox cbxLayer = new JComboBox(editlayer.keySet().toArray(new String[0]));	// initialize dropdown box
    cbxLayer.setMaximumSize(new Dimension(220, 25));
    cbxLayer.setAlignmentX(Component.LEFT_ALIGNMENT);
    
 // create text
//    JLabel lblLayer2 = new JLabel("feature list to query:");
 //   lblLayer2.setForeground(Color.WHITE);
 //   lblLayer2.setAlignmentX(Component.LEFT_ALIGNMENT);    
    
 // create text
//    JTextArea description3 = new JTextArea("press refresh to delete pin point marking");
//    description3.setFont(new Font("Verdana", Font.PLAIN, 11));
 //   description3.setForeground(Color.WHITE);
 //   description3.setBackground(new Color(0, 0, 0, 0));
 //   description3.setEditable(false);
 //   description3.setLineWrap(true);
  //  description3.setWrapStyleWord(true);
 //   description3.setAlignmentX(Component.LEFT_ALIGNMENT);
    
    
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
//		               	      String[] newFeature = new String[] {layer.getName(), String.valueOf(hitGraphic.getAttributes().get("FID")), String.valueOf(hitGraphic.getAttributes().get("OBJECTID"))};  //ZL-151209 try to get FID and OBJECTID 
//    		            	  String[] newFeature = new String[] {layer.getName(), String.valueOf(hitGraphic.getAttributes().get("OBJECTID")), String.valueOf(hitGraphic.getAttributes().get("boilingpt"))}; 
    		            	  String[] newFeature = new String[] {layer.getName(), String.valueOf(hitGraphic.getAttributes().get("OBJECTID"))}; 
    		            	  System.out.println("newFeature[0]=" + newFeature[0] + ", newFeature[1]=" + newFeature[1]); //ZL-151209
//double y= Double.parseDouble(newFeature[2]);
//double z=2*y;
//System.out.println("new function=" +z);    		            	  
    		            	  //try to expand new button
    		            	  
    		            	  boolean addtoStack = true;
    		            	  System.out.println("editStack size=" + editStack.size());
    		            	  for (int i=0; i<editStack.size(); i++) {							// (mjk, 151120) check through (i) elements in editStack where (i) is the number of modified feature objects in the layers.
    		            		  String itemlayer = editStack.get(i)[0];
    		            		  String graphicOBJECTID = editStack.get(i)[1];
//    		            		  String appCallFlag = editStack.get(i)[2];   //ZL-151208
    		            		  if (layer.getName().equals(itemlayer) && (String.valueOf(hitGraphic.getAttributes().get("OBJECTID")).equals(graphicOBJECTID))) { 
//    		            		  if (layer.getName().equals(itemlayer) && (String.valueOf(hitGraphic.getAttributes().get("FID")).equals(graphicFID)||String.valueOf(hitGraphic.getAttributes().get("OBJECTID")).equals(graphicOBJECTID))) {	  
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
    
    
    
   //try to make new button 
   
JButton change = new JButton("change to fin.data");
change.addActionListener(new ActionListener() {
	@Override
	public void actionPerformed(ActionEvent arg0) {
		
		try {
			String newId = idMap.get(mapIds.getSelectedItem());
        WebMap webMap = WebMap.newInstance(newId, arcgisPortal);
        map.loadWebMap(webMap);
        
        
		} catch (Exception e)  {
			e.printStackTrace();
		}
		
		
	}
});

change.setEnabled(true);
change.setVisible(true);
change.setSize(190,30);
change.setLocation(890, 45);
    



    // Run PowerWorld button
    JButton PWbutton = new JButton("Run PowerWorld");
    PWbutton.addActionListener(new ActionListener() {
    	@Override
    	public void actionPerformed(ActionEvent arg0) {
    		HttpURLConnection urlCon;
    		OutputStreamWriter out;
    		URL url;
    		try {
    			url = new URL("http://172.25.182.41/PWServlet/");
//				url = new URL("http://www.jparksimulator.com/PWServlet/"); // URL of servlet
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
					StringBuilder OBJECTIDs = new StringBuilder();
					StringBuilder appCallFlag = new StringBuilder(); // (mjk, 151115) creates a flag indicating which function has been called: PowerWorld, parameterised PW, AspenPlus, parameterised AP
					for (String[] item : editStack) { // create comma separated values
						layers.append(item[0]);
						layers.append(",");
						OBJECTIDs.append(item[1]);
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
					outputString.append(URLEncoder.encode("OBJECTIDs", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(OBJECTIDs.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("appCallFlag", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(appCallFlag.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("QueryT", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(" ", "UTF-8"));
					System.out.println("outputString=" + outputString);
					
					// Example of comma separated outputString is "layers=Load_Points,Load_Points,&FIDs=103,104,"
					DataOutputStream wr = new DataOutputStream(urlCon.getOutputStream());
					wr.writeBytes(outputString.toString()); // write query string into servlet doPost() method
/**
					FileWriter httpString = null; // (mjk, 151115) testing structure of DataOutputStream object and of wr object
//					httpString = new FileWriter(httpStringCSV);
					httpString.append("wr=");
					httpString.append(outputString.toString());
					httpString.flush();				
					httpString.close();*/

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
    PWbutton.setSize(160,30);
    PWbutton.setLocation(490, 10);

    
    // Run Parameterised PowerWorld button
    JButton PWPrButton = new JButton("Run Pr PowerWorld");
    PWPrButton.addActionListener(new ActionListener() {
    	@Override
    	public void actionPerformed(ActionEvent arg0) {
    		HttpURLConnection urlCon;
    		OutputStreamWriter out;
    		URL url;
    		try {
			//	url = new URL("http://172.25.182.41/PWServlet/"); // URL of servlet
    			url = new URL("http://172.25.182.41/PWServlet_New/"); // URL of servlet
    			
    			urlCon = (HttpURLConnection) url.openConnection();
				urlCon.setRequestMethod("POST");
				urlCon.setDoOutput(true);
				
				if (editStack.isEmpty()) {
					JOptionPane.showMessageDialog(null, "You did not edit any features for PowerWorld!");
				} else {
					out = new OutputStreamWriter(urlCon.getOutputStream(), "UTF-8");
					StringBuilder layers = new StringBuilder();
					StringBuilder OBJECTIDs = new StringBuilder();
					StringBuilder appCallFlag = new StringBuilder();
					for (String[] item : editStack) { // create comma separated values
						layers.append(item[0]);
						layers.append(",");
						OBJECTIDs.append(item[1]);
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
					outputString.append(URLEncoder.encode("OBJECTIDs", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(OBJECTIDs.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("appCallFlag", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(appCallFlag.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("QueryT", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(" ", "UTF-8"));
					System.out.println("outputString=" + outputString);
					
					// Example of comma separated outputString is "layers=Load_Points,Load_Points,&FIDs=103,104,"
					DataOutputStream wr = new DataOutputStream(urlCon.getOutputStream());
					wr.writeBytes(outputString.toString()); // write query string into servlet doPost() method
/*					
					FileWriter httpString = null; // (mjk, 151115) testing structure of DataOutputStream object and of wr object
					httpString = new FileWriter(httpStringCSV2);
					httpString.append("wr=");
					httpString.append(outputString.toString());
					httpString.flush();				
					httpString.close();*/
					
					wr.flush();
					wr.close();
					
					if (urlCon.getResponseCode()==200) {
						JOptionPane.showMessageDialog(null, "PrPowerWorld has finished evaluating!");
						editStack.clear(); // delete all items in editStack
					} else {
						JOptionPane.showMessageDialog(null, "An error has occurred. HTTP Error: " + urlCon.getResponseCode()
								+ "\nPlease try evaluating PrPowerWorld again");
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
    PWPrButton.setLocation(660, 10);

    
    // Run AspenPlus button
    JButton APbutton = new JButton("Run BiodieselPlant-1");
    APbutton.addActionListener(new ActionListener() {
    	@Override
    	public void actionPerformed(ActionEvent arg0) {
    		HttpURLConnection urlCon;
    		OutputStreamWriter out;
    		URL url;
    		try {
//				url = new URL("http://www.jparksimulator.com/APServlet/"); // URL of servlet
				url = new URL("http://172.25.182.41/PWServlet/");  //ZL-151203  
				urlCon = (HttpURLConnection) url.openConnection();
				urlCon.setRequestMethod("POST");
				urlCon.setDoOutput(true);
				
				if (editStack.isEmpty()) {
					JOptionPane.showMessageDialog(null, "You did not edit any features for AspenPlus!");
				} else {
					
					out = new OutputStreamWriter(urlCon.getOutputStream(), "UTF-8");
					StringBuilder layers = new StringBuilder();
					StringBuilder OBJECTIDs = new StringBuilder();
					StringBuilder appCallFlag = new StringBuilder();
					for (String[] item : editStack) { // create comma separated values
						layers.append(item[0]);
						layers.append(",");
						OBJECTIDs.append(item[1]);
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
					outputString.append(URLEncoder.encode("OBJECTIDs", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(OBJECTIDs.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("appCallFlag", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(appCallFlag.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("QueryT", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(" ", "UTF-8"));
					System.out.println("outputString="+outputString);
					
					// Example of comma separated outputString is "layers=Load_Points,Load_Points,&FIDs=103,104,"
					DataOutputStream wr = new DataOutputStream(urlCon.getOutputStream());
					wr.writeBytes(outputString.toString()); // write query string into servlet doPost() method
					/**
					FileWriter httpString = null; // (mjk, 151115) testing structure of DataOutputStream object and of wr object
//					httpString = new FileWriter(httpStringCSV1);
					httpString.append("wr=");
					httpString.append(outputString.toString());
					httpString.flush();				
					httpString.close();*/

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
    APbutton.setSize(160,30);
    APbutton.setLocation(490, 45);    


    // Run Parameterised AspenPlus button
    JButton APPrButton = new JButton("Run Pr BiodieselPlant-1");
    APPrButton.addActionListener(new ActionListener() {
    	@Override
    	public void actionPerformed(ActionEvent arg0) {
    		HttpURLConnection urlCon;
    		OutputStreamWriter out;
    		URL url;
    		try {
    			    			
//    			url = new URL("http://172.25.182.41/PWServlet/");
    			url = new URL("http://172.25.182.41/APWOWHRServlet/");
//    			url = new URL("http://caresremote1.dyndns.org/OPALRTServlet/"); 
//	        	url = new URL("http://www.jparksimulator.com/PWServlet/"); // URL of servlet
				urlCon = (HttpURLConnection) url.openConnection();
				urlCon.setRequestMethod("POST");
				urlCon.setDoOutput(true);
				
				if (editStack.isEmpty()) {
					JOptionPane.showMessageDialog(null, "You did not edit any features for AspenPlus!");
				} else {
					out = new OutputStreamWriter(urlCon.getOutputStream(), "UTF-8");
					StringBuilder layers = new StringBuilder();
					StringBuilder OBJECTIDs = new StringBuilder();
					StringBuilder appCallFlag = new StringBuilder();
					
					for (String[] item : editStack) { // create comma separated values
						layers.append(item[0]);
						layers.append(",");
						OBJECTIDs.append(item[1]);
						OBJECTIDs.append(",");
						appCallFlag.append("PrAP");
						appCallFlag.append(",");
					}
					StringBuilder outputString = new StringBuilder();
					// Only URL encoded string values can be sent over a HTTP connection
					outputString.append(URLEncoder.encode("layers", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(layers.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("OBJECTIDs", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(OBJECTIDs.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("appCallFlag", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(appCallFlag.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("QueryT", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(" ", "UTF-8"));
					System.out.println("outputString="+outputString);
					
					// Example of comma separated outputString is "layers=Load_Points,Load_Points,&FIDs=103,104,"
					DataOutputStream wr = new DataOutputStream(urlCon.getOutputStream());
					wr.writeBytes(outputString.toString()); // write query string into servlet doPost() method
					wr.flush();
					wr.close();
					
					if (urlCon.getResponseCode()==200) {
						JOptionPane.showMessageDialog(null, "I have finished evaluating new operational status for BiodieselPlant-1!");
						editStack.clear(); // delete all items in editStack
					} else {
						JOptionPane.showMessageDialog(null, "An error has occurred. HTTP Error: " + urlCon.getResponseCode()
								+ "\nPlease try running Pr BiodieselPlant-1 again");
					}
					out.close();
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
    		/*for (ArcGISFeatureLayer layer : completeLayerList) {
    			layer.requery();
    			layer.refresh();
    		}*/
    	heatercoolerlayer.requery();
    	heatercoolerlayer.refresh();
    	GasLinelayer.requery();
    	GasLinelayer.refresh();
    	RadFraclayer.requery();
    	RadFraclayer.refresh();
    	}
    });
    APPrButton.setEnabled(true);
    APPrButton.setVisible(true);
    APPrButton.setSize(170,30);
    APPrButton.setLocation(660, 45);
    
 // Run AspenPlus model with heat recovery button
    JButton APHrButton = new JButton("Run BiodieselPlant-2");
    APHrButton.addActionListener(new ActionListener() {
    	@Override
    	public void actionPerformed(ActionEvent arg0) {
    		HttpURLConnection urlCon;
    		OutputStreamWriter out;
    		URL url;
    		try {
    			url = new URL("http://172.25.182.41/PWServlet/");
			//	url = new URL("http://www.jparksimulator.com/PWServlet/"); // URL of servlet
				urlCon = (HttpURLConnection) url.openConnection();
				urlCon.setRequestMethod("POST");
				urlCon.setDoOutput(true);
				
				if (editStack.isEmpty()) {
					JOptionPane.showMessageDialog(null, "You did not edit any features for AspenPlus!");
				} else {
					out = new OutputStreamWriter(urlCon.getOutputStream(), "UTF-8");
					StringBuilder layers = new StringBuilder();
					StringBuilder OBJECTIDs = new StringBuilder();
					StringBuilder appCallFlag = new StringBuilder();
					
					for (String[] item : editStack) { // create comma separated values
						layers.append(item[0]);
						layers.append(",");
						OBJECTIDs.append(item[1]);
						OBJECTIDs.append(",");
						appCallFlag.append("APHR");
						appCallFlag.append(",");
					}
					StringBuilder outputString = new StringBuilder();
					// Only URL encoded string values can be sent over a HTTP connection
					outputString.append(URLEncoder.encode("layers", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(layers.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("OBJECTIDs", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(OBJECTIDs.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("appCallFlag", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(appCallFlag.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("QueryT", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(" ", "UTF-8"));
					System.out.println("outputString="+outputString);
					
					// Example of comma separated outputString is "layers=Load_Points,Load_Points,&FIDs=103,104,"
					DataOutputStream wr = new DataOutputStream(urlCon.getOutputStream());
					wr.writeBytes(outputString.toString()); // write query string into servlet doPost() method
					wr.flush();
					wr.close();
					
					if (urlCon.getResponseCode()==200) {
						JOptionPane.showMessageDialog(null, "AspenPlus model with heat recovery has finished running!");
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
    APHrButton.setEnabled(true);
    APHrButton.setVisible(true);
    APHrButton.setSize(160,30);
    APHrButton.setLocation(490, 80);

    // Run AspenPlus model with heat recovery button
    JButton PrAPHrButton = new JButton("Run Pr BiodieselPlant-2");
    PrAPHrButton.addActionListener(new ActionListener() {
    	@Override
    	public void actionPerformed(ActionEvent arg0) {
    		HttpURLConnection urlCon;
    		OutputStreamWriter out;
    		URL url;
    		try {
    			url = new URL("http://172.25.182.41/APWWHRServlet/");
			//	url = new URL("http://www.jparksimulator.com/PWServlet/"); // URL of servlet
				urlCon = (HttpURLConnection) url.openConnection();
				urlCon.setRequestMethod("POST");
				urlCon.setDoOutput(true);
				
				if (editStack.isEmpty()) {
					JOptionPane.showMessageDialog(null, "You did not edit any features for AspenPlus!");
				} else {
					out = new OutputStreamWriter(urlCon.getOutputStream(), "UTF-8");
					StringBuilder layers = new StringBuilder();
					StringBuilder OBJECTIDs = new StringBuilder();
					StringBuilder appCallFlag = new StringBuilder();
					
					for (String[] item : editStack) { // create comma separated values
						layers.append(item[0]);
						layers.append(",");
						OBJECTIDs.append(item[1]);
						OBJECTIDs.append(",");
						appCallFlag.append("PrAPHR");
						appCallFlag.append(",");
					}
					StringBuilder outputString = new StringBuilder();
					// Only URL encoded string values can be sent over a HTTP connection
					outputString.append(URLEncoder.encode("layers", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(layers.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("OBJECTIDs", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(OBJECTIDs.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("appCallFlag", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(appCallFlag.toString(), "UTF-8"));
//					outputString.append("&");
//					outputString.append(URLEncoder.encode("QueryT", "UTF-8"));
//					outputString.append("=");
//					outputString.append(URLEncoder.encode(" ", "UTF-8"));
					
					// Example of comma separated outputString is "layers=Load_Points,Load_Points,&FIDs=103,104,"
					DataOutputStream wr = new DataOutputStream(urlCon.getOutputStream());
					wr.writeBytes(outputString.toString()); // write query string into servlet doPost() method
					wr.flush();
					wr.close();
					
					if (urlCon.getResponseCode()==200) {
						JOptionPane.showMessageDialog(null, "I have finished evaluating new operational status for BiodieselPlant-2!");
						editStack.clear(); // delete all items in editStack
					} else {
						JOptionPane.showMessageDialog(null, "An error has occurred. HTTP Error: " + urlCon.getResponseCode()
								+ "\nPlease try running Pr BiodieselPlant-2 again");
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
    		/*heatercoolerlayer.requery();
    		heatercoolerlayer.refresh();
    		GasLinelayer.requery();
    		GasLinelayer.refresh();*/
    		
    	}
    });
    PrAPHrButton.setEnabled(true);
    PrAPHrButton.setVisible(true);
    PrAPHrButton.setSize(170,30);
    PrAPHrButton.setLocation(660, 80);
    
 // Run combined AspenPlus and power world model 
    JButton APPWButton = new JButton("Run BiodieselPlant-3");
    APPWButton.addActionListener(new ActionListener() {
    	@Override
    	public void actionPerformed(ActionEvent arg0) {
    		HttpURLConnection urlCon;
    		OutputStreamWriter out;
    		URL url;
    		try {
				url = new URL("http://172.25.182.41/PWServlet/"); // URL of servlet
				urlCon = (HttpURLConnection) url.openConnection();
				urlCon.setRequestMethod("POST");
				urlCon.setDoOutput(true);
				
				if (editStack.isEmpty()) {
					JOptionPane.showMessageDialog(null, "You did not edit any features for AspenPlus or PowerWorld!");
				} else {
					out = new OutputStreamWriter(urlCon.getOutputStream(), "UTF-8");
					StringBuilder layers = new StringBuilder();
					StringBuilder OBJECTIDs = new StringBuilder();
					StringBuilder appCallFlag = new StringBuilder();
					
					for (String[] item : editStack) { // create comma separated values
						layers.append(item[0]);
						layers.append(",");
			 			OBJECTIDs.append(item[1]); // ZHOU CHANGED ITEM[2] TO ITEM[1]
					    OBJECTIDs.append(",");
						appCallFlag.append("APPW");
						appCallFlag.append(",");
					}
					StringBuilder outputString = new StringBuilder();
					// Only URL encoded string values can be sent over a HTTP connection
					outputString.append(URLEncoder.encode("layers", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(layers.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("OBJECTIDs", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(OBJECTIDs.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("appCallFlag", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(appCallFlag.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("QueryT", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(" ", "UTF-8"));
					System.out.println("outputString="+outputString);
					
					// Example of comma separated outputString is "layers=Load_Points,Load_Points,&FIDs=103,104,"
					DataOutputStream wr = new DataOutputStream(urlCon.getOutputStream());
					wr.writeBytes(outputString.toString()); // write query string into servlet doPost() method
					wr.flush();
					wr.close();
					
					if (urlCon.getResponseCode()==200) {
						JOptionPane.showMessageDialog(null, "AP & PW has finished running!");
						editStack.clear(); // delete all items in editStack
					} else {
						JOptionPane.showMessageDialog(null, "An error has occurred. HTTP Error: " + urlCon.getResponseCode()
								+ "\nPlease try running AP+PW model again");
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
    APPWButton.setEnabled(true);
    APPWButton.setVisible(true);
    APPWButton.setSize(160,30);
    APPWButton.setLocation(490, 115);
    
    // Run combined parameterized AspenPlus and power world model 
    JButton PrAPPWButton = new JButton("Run Pr BiodieselPlant-3");
    PrAPPWButton.addActionListener(new ActionListener() {
    	@Override
    	public void actionPerformed(ActionEvent arg0) {
    		HttpURLConnection urlCon;
    		OutputStreamWriter out;
    		URL url;
    		try {
				url = new URL("http://172.25.182.41/PWServlet/"); // URL of servlet
				urlCon = (HttpURLConnection) url.openConnection();
				urlCon.setRequestMethod("POST");
				urlCon.setDoOutput(true);
				
				if (editStack.isEmpty()) {
					JOptionPane.showMessageDialog(null, "You did not edit any features for AspenPlus!");
				} else {
					out = new OutputStreamWriter(urlCon.getOutputStream(), "UTF-8");
					StringBuilder layers = new StringBuilder();
					StringBuilder OBJECTIDs = new StringBuilder();
					StringBuilder appCallFlag = new StringBuilder();
					
					for (String[] item : editStack) { // create comma separated values
						layers.append(item[0]);
						layers.append(",");
			 			OBJECTIDs.append(item[1]); // ZHOU CHANGED ITEM[2] TO ITEM[1]
					    OBJECTIDs.append(",");
						appCallFlag.append("PrAPPW");
						appCallFlag.append(",");
					}
					StringBuilder outputString = new StringBuilder();
					// Only URL encoded string values can be sent over a HTTP connection
					outputString.append(URLEncoder.encode("layers", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(layers.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("OBJECTIDs", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(OBJECTIDs.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("appCallFlag", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(appCallFlag.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("QueryT", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(" ", "UTF-8"));
					System.out.println("outputString="+outputString);
					
					// Example of comma separated outputString is "layers=Load_Points,Load_Points,&FIDs=103,104,"
					DataOutputStream wr = new DataOutputStream(urlCon.getOutputStream());
					wr.writeBytes(outputString.toString()); // write query string into servlet doPost() method
					wr.flush();
					wr.close();
					
					if (urlCon.getResponseCode()==200) {
						JOptionPane.showMessageDialog(null, "I have finished evaluating new operational status for BiodieselPlant-3!");
						editStack.clear(); // delete all items in editStack
					} else {
						JOptionPane.showMessageDialog(null, "An error has occurred. HTTP Error: " + urlCon.getResponseCode()
								+ "\nPlease try running Pr BiodieselPlant-3 again");
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
    		/*heatercoolerlayer.requery();
    		heatercoolerlayer.refresh();
    		PlantReactorlayer.requery();
    		PlantReactorlayer.refresh();
    		MaterialLinelayer.requery();*/
    		
    		
    	}
    	    });
    PrAPPWButton.setEnabled(true);
    PrAPPWButton.setVisible(true);
    PrAPPWButton.setSize(170,30);
    PrAPPWButton.setLocation(660, 115);
/**
 // Run combined parameterized AspenPlus and power world model 
    JButton PrhydroButton = new JButton("Run Pr Hydrocracking");
    PrhydroButton.addActionListener(new ActionListener() {
    	@Override
    	public void actionPerformed(ActionEvent arg0) {
    		HttpURLConnection urlCon;
    		OutputStreamWriter out;
    		URL url;
    		try {
				url = new URL("http://172.25.182.41/PWServlet/"); // URL of servlet
				urlCon = (HttpURLConnection) url.openConnection();
				urlCon.setRequestMethod("POST");
				urlCon.setDoOutput(true);
				
				if (editStack.isEmpty()) {
					JOptionPane.showMessageDialog(null, "You did not edit any features for AspenPlus!");
				} else {
					out = new OutputStreamWriter(urlCon.getOutputStream(), "UTF-8");
					StringBuilder layers = new StringBuilder();
					StringBuilder OBJECTIDs = new StringBuilder();
					StringBuilder appCallFlag = new StringBuilder();
					
					for (String[] item : editStack) { // create comma separated values
						layers.append(item[0]);
						layers.append(",");
			 			OBJECTIDs.append(item[1]); // ZHOU CHANGED ITEM[2] TO ITEM[1]
					    OBJECTIDs.append(",");
						appCallFlag.append("PrAPHC");
						appCallFlag.append(",");
					}
					StringBuilder outputString = new StringBuilder();
					// Only URL encoded string values can be sent over a HTTP connection
					outputString.append(URLEncoder.encode("layers", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(layers.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("OBJECTIDs", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(OBJECTIDs.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("appCallFlag", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(appCallFlag.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("QueryT", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(" ", "UTF-8"));
					System.out.println("outputString="+outputString);
					
					// Example of comma separated outputString is "layers=Load_Points,Load_Points,&FIDs=103,104,"
					DataOutputStream wr = new DataOutputStream(urlCon.getOutputStream());
					wr.writeBytes(outputString.toString()); // write query string into servlet doPost() method
					wr.flush();
					wr.close();
					
					if (urlCon.getResponseCode()==200) {
						heatercoolerlayer.requery();
			    		heatercoolerlayer.refresh();
			    		RadFraclayer.requery();
			    		RadFraclayer.refresh();
						JOptionPane.showMessageDialog(null, "I have finished evaluating new operational status for Hydrocracking!");
						editStack.clear(); // delete all items in editStack
					} else {
						JOptionPane.showMessageDialog(null, "An error has occurred. HTTP Error: " + urlCon.getResponseCode()
								+ "\nPlease try running Pr Hydrocracking again");
					}
					out.close();
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
    	
    		
    	}
    	    });
    PrhydroButton.setEnabled(true);
    PrhydroButton.setVisible(true);
    PrhydroButton.setSize(190,30);
    PrhydroButton.setLocation(690, 150);
    
// Run AspenPlus model with heat recovery button ZL-20160322
    JButton PrAPOButton = new JButton("Run PrAP from OntoCAPE");
    PrAPOButton.addActionListener(new ActionListener() {
    	@Override
    	public void actionPerformed(ActionEvent arg0) {
    		HttpURLConnection urlCon;
    		OutputStreamWriter out;
    		URL url;
    		try {
				url = new URL("http://172.25.182.41/PWServlet/"); // URL of servlet				
				urlCon = (HttpURLConnection) url.openConnection();
				urlCon.setRequestMethod("POST");
				urlCon.setDoOutput(true);
				
				if (editStack.isEmpty()) {
					JOptionPane.showMessageDialog(null, "You did not edit any features for AspenPlus!");
				} else {
					out = new OutputStreamWriter(urlCon.getOutputStream(), "UTF-8");
					StringBuilder layers = new StringBuilder();
					StringBuilder OBJECTIDs = new StringBuilder();
					StringBuilder appCallFlag = new StringBuilder();
					
					for (String[] item : editStack) { // create comma separated values
						layers.append(item[0]);
						layers.append(",");
			 			OBJECTIDs.append(item[1]); // ZHOU CHANGED ITEM[2] TO ITEM[1]
					    OBJECTIDs.append(",");
						appCallFlag.append("PrAPO");
						appCallFlag.append(",");
					}
					StringBuilder outputString = new StringBuilder();
					// Only URL encoded string values can be sent over a HTTP connection
					outputString.append(URLEncoder.encode("layers", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(layers.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("OBJECTIDs", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(OBJECTIDs.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("appCallFlag", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(appCallFlag.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("QueryT", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(" ", "UTF-8"));
					System.out.println("outputString="+outputString);
					
					// Example of comma separated outputString is "layers=Load_Points,Load_Points,&FIDs=103,104,"
					DataOutputStream wr = new DataOutputStream(urlCon.getOutputStream());
					wr.writeBytes(outputString.toString()); // write query string into servlet doPost() method
					wr.flush();
					wr.close();
					
					if (urlCon.getResponseCode()==200) {
						JOptionPane.showMessageDialog(null, "PrAP with OntoCAPE has finished running!");
						editStack.clear(); // delete all items in editStack
					} else {
						JOptionPane.showMessageDialog(null, "An error has occurred. HTTP Error: " + urlCon.getResponseCode()
								+ "\nPlease try running Parameterised AP from OntoCAPE again");
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
    PrAPOButton.setEnabled(true);
    PrAPOButton.setVisible(true);
    PrAPOButton.setSize(190,30);
    PrAPOButton.setLocation(890, 10);
*/
 
 // Run combined parameterized AspenPlus and power world model 
    JButton OPALRT = new JButton("Run OPAL-RT"); 
    OPALRT.addActionListener(new ActionListener() {
    	@Override
    	public void actionPerformed(ActionEvent arg0) {
    		HttpURLConnection urlCon;
    		OutputStreamWriter out;
    		URL url;
    		try {
				url = new URL("http://172.25.182.41/PWServlet/"); // URL of servlet
				urlCon = (HttpURLConnection) url.openConnection();
				urlCon.setRequestMethod("POST");
				urlCon.setDoOutput(true);
				
				if (editStack.isEmpty()) {
					JOptionPane.showMessageDialog(null, "You did not edit any features for PowerWorld!");
				} else {
					out = new OutputStreamWriter(urlCon.getOutputStream(), "UTF-8");
					StringBuilder layers = new StringBuilder();
					StringBuilder OBJECTIDs = new StringBuilder();
					StringBuilder appCallFlag = new StringBuilder();
					
					for (String[] item : editStack) { // create comma separated values
						layers.append(item[0]);
						layers.append(",");
			 			OBJECTIDs.append(item[1]);    // ZHOU CHANGED ITEM[2] TO ITEM[1]
					    OBJECTIDs.append(",");
						appCallFlag.append("OPALRT");
						appCallFlag.append(",");
					}
					StringBuilder outputString = new StringBuilder();
					// Only URL encoded string values can be sent over a HTTP connection
//					outputString.append("ping");
					outputString.append(URLEncoder.encode("layers", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(layers.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("OBJECTIDs", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(OBJECTIDs.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("appCallFlag", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(appCallFlag.toString(), "UTF-8"));
					outputString.append("&");
					outputString.append(URLEncoder.encode("QueryT", "UTF-8"));
					outputString.append("=");
					outputString.append(URLEncoder.encode(" ", "UTF-8"));
					System.out.println("outputString="+outputString);
					
					// Example of comma separated outputString is "layers=Load_Points,Load_Points,&FIDs=103,104,"
					DataOutputStream wr = new DataOutputStream(urlCon.getOutputStream());
					wr.writeBytes(outputString.toString()); // write query string into servlet doPost() method
					wr.flush();
					wr.close();
					
					if (urlCon.getResponseCode()==200) {
						JOptionPane.showMessageDialog(null, "OPALRT has finished running!");
						editStack.clear(); // delete all items in editStack
						try{
							Runtime rt = Runtime.getRuntime();
							//rt.exec("cmd /c start C:/apache-tomcat-8.0.24/webapps/ROOT/download_from_NUS/download_from_NUS.bat");
//							System.out.println("Inputs downloaded.");
							Process ps = rt.exec("cmd /c D:/opalrt/download_from_NUS.bat");
							System.out.println("Downloading commenced.");
							ps.waitFor();
							System.out.println("Downloading finished.");
							if(ps.exitValue()==0){
								System.out.println("Success!");
							}else {
								System.out.println("failed!");
							}
							ps.destroy();
							//rt.exec("cmd /c start C:/apache-tomcat-8.0.24/webapps/ROOT/transmit_files_to_NUS/send_from_NTU.bat");
//							System.out.println("Outputs uploaded."); 
							
							
							}catch (Exception e) {
								e.printStackTrace();
							}
						
						
					} else {
						JOptionPane.showMessageDialog(null, "An error has occurred. HTTP Error: " + urlCon.getResponseCode()
								+ "\nPlease try again");
					}
					out.close();
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
    		
    		//String[] av = {"D:/opalrt/CurrentA.png"};
    		//MouseDrag.Printing(av);
    		
    		
    		
    		for (ArcGISFeatureLayer layer : completeLayerList) {
    			layer.requery();
    			layer.refresh();
    		}
    	}
    });
    OPALRT.setEnabled(true);
    OPALRT.setVisible(true);
    OPALRT.setSize(140,30);
    OPALRT.setLocation(840, 10);    
    
    
    JButton refreshButton = new JButton("Refresh Map");
    refreshButton.addActionListener(new ActionListener() {
    	@Override
    	public void actionPerformed(ActionEvent arg0) {
    		for (ArcGISFeatureLayer layer : completeLayerList) {
    			layer.requery();
    			layer.refresh();
    		}
//    		graphicsLayer.removeAll();
//    		layers.remove(graphicsLayer);
    		
    	}
    });
    refreshButton.setEnabled(true);
    refreshButton.setVisible(true);
    refreshButton.setSize(110,30);
    refreshButton.setLocation(990, 10);
    
    
    graphicsLayer = new GraphicsLayer();
    graphicsLayer.setName("simple graphics");
    
    
    // combine text, label and dropdown list into one panel for selecting layer to edit
    panel.setBackground(new Color(0, 0, 0, 140));
    panel.add(description);
    panel.add(Box.createRigidArea(new Dimension(0, 5)));
    panel.add(lblLayer);
    panel.add(Box.createRigidArea(new Dimension(0, 5)));
    panel.add(cbxLayer);
    panel.add(Box.createRigidArea(new Dimension(0, 5)));
//    panel.add(lblLayer2);
//    panel.add(Box.createRigidArea(new Dimension(0, 5)));
//    panel.add(querylayer);
//    panel.add(Box.createRigidArea(new Dimension(0, 5)));
//    panel.add(description3);
//    panel.add(Box.createRigidArea(new Dimension(0, 5)));
    panel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
    
    // create legend
    JLegend legend = new JLegend(map);
    legend.setPreferredSize(new Dimension(250, 700));
    legend.setBorder(new LineBorder(new Color(205, 205, 255), 3));
    
    // initialize contentPane and add contents
   /**
    * add new query button for pup-up query window ZL
    */
    planes = new MultiPoint();  //define new planes in order to add pin points
    PictureMarkerSymbol planeSymbol = new PictureMarkerSymbol("http://static.arcgis.com/images/Symbols/Basic/GreenShinyPin.png");
    planeSymbol.setSize(40, 40);  //set the size of the pin points
    
    final javax.swing.JButton QueryButton;
    QueryButton = new javax.swing.JButton();
    QueryButton.setFont(new java.awt.Font(" ", 1, 12)); // NOI18N
    QueryButton.setText("Information Query");
    QueryButton.setAlignmentX(Component.CENTER_ALIGNMENT);
    QueryButton.setEnabled(true);
    QueryButton.addActionListener(new ActionListener() {
    	
		@Override
		public void actionPerformed(ActionEvent e) {
			
			QueryGuiNew QueryGUI = new QueryGuiNew();
			QueryGUI.setVisible(true);
			QueryGUI.setLocationRelativeTo(null);	 
	        
	        /** add action listener to the "Show Location" button, so that when user press the button, Location of the queried entity can be shown in the map*/
			QueryGUI.btnShowLocation.addActionListener(new ActionListener(){

				@Override
				public void actionPerformed(ActionEvent e) {
										
					  BufferedReader fileReader = null;                                                   //define a file reader
				      String line = null;
				  	  try {
						fileReader = new BufferedReader(new FileReader(GIS));                             //define source file
						while ((line = fileReader.readLine()) != null) {
					  		  String[] data = line.split(",");                                            //split the lines by comma and extract the x and y coordinates
					  		  planes.add(Double.parseDouble(data[0]),Double.parseDouble(data[1]));	  	  //convert the coordinate from string to double format and add them to planes
					  	  }
					} catch (NumberFormatException | IOException e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					}	  	 	    				  	    				  	  
			        Graphic gPlanes = new Graphic(planes, planeSymbol);
			        graphicsLayer.addGraphic(gPlanes);                                                     //add the pin point layer
			        layers.add(graphicsLayer);                                                             //visualize the pin pint layer
				}
				
	        });
	        
	        /** add action listener to the "btnClear" button, so that when user press the button, Location of the queried entity can be shown in the map*/
			QueryGUI.btnClear.addActionListener(new ActionListener(){
	        	public void actionPerformed(ActionEvent e) {
	        		graphicsLayer.removeAll();
	        		layers.remove(graphicsLayer);
	        	}
			});
	          
		}     	
    });
    QueryButton.setSize(140, 30);
    QueryButton.setLocation(840, 45);
    
/**add new button for the plant optimization function Li ZHOU 27.08.2016*/
    graphicsLayer_polygon.setName("simple query graphics");  //new layer for the polygon
    final javax.swing.JButton PlantOptButton;
    PlantOptButton = new javax.swing.JButton();
    PlantOptButton.setFont(new java.awt.Font(" ", 1, 12)); // NOI18N
    PlantOptButton.setText("Plant Optimization");
    PlantOptButton.setAlignmentX(Component.CENTER_ALIGNMENT);
    PlantOptButton.setEnabled(true);
    PlantOptButton.addActionListener(new ActionListener() {
    	@Override
		public void actionPerformed(ActionEvent e) {
			
    		OptimizationGUI PlantOptGUI = new OptimizationGUI();
    		PlantOptGUI.setVisible(true);
    		PlantOptGUI.setLocationRelativeTo(null);
    		
    		/** add action listener to the "clear button" on the optimization GUI --when "clear button" pressed, clear the new layer with polygon indicating the plant location*/
    		OptimizationGUI.btnClear.addActionListener(new ActionListener(){
				@Override
				public void actionPerformed(ActionEvent e) {
					graphicsLayer_polygon.removeAll();
	        		layers.remove(graphicsLayer_polygon);					
				}    			
    		});
    		
    		/** add action listener to the "Show Location" button, so that when user press the button, Location of the queried entity can be shown in the map*/
    		OptimizationGUI.btnShowPlantLocation.addActionListener(new ActionListener(){

				@Override
				public void actionPerformed(ActionEvent e) {
										
					  BufferedReader fileReader = null;                                             //define a file reader
				      String line = null;
				      Polygon polygon = new Polygon(); 
				      ArrayList<Double> polygon_x = new ArrayList<Double>();                        //create new array list for the x coordinate
					  ArrayList<Double> polygon_y = new ArrayList<Double>();                        //create new array list for the y coordinate
						
				  	  try {
						fileReader = new BufferedReader(new FileReader(PlantGIS));                  //define source file
						line = fileReader.readLine();                                               //skip the header
						boolean flag = true;						
						while ((line = fileReader.readLine()) != null) {
					  		  String[] data = line.split(",");                                      //split the lines by comma and extract the x and y coordinates
					  		  
					  		  if(flag){
					  			polygon_x.add(Double.parseDouble(data[0]));
					  			polygon_y.add(Double.parseDouble(data[1]));	
					  		  } 
					  		  else if (!flag){
					  			polygon_x.add(Double.parseDouble(data[0]));
					  			polygon_y.add(Double.parseDouble(data[1]));	
					  		  }
					  		flag = false;
					  	  }	
					} catch (NumberFormatException | IOException e1) {
						e1.printStackTrace();
					}	
				  	  
				    polygon.startPath(polygon_x.get(0), polygon_y.get(0));
				    polygon.lineTo(polygon_x.get(1), polygon_y.get(1));
				    polygon.lineTo(polygon_x.get(2), polygon_y.get(2));
				    polygon.lineTo(polygon_x.get(3), polygon_y.get(3));
				    polygon.closePathWithLine();
				  	SimpleLineSymbol outline = new SimpleLineSymbol(new Color(0, 200, 0),7, SimpleLineSymbol.Style.SOLID);
				  	SimpleFillSymbol symbol = new SimpleFillSymbol(new Color(255, 255, 255,130), outline);
			        Graphic graphic_polygon = new Graphic(polygon,symbol);
			        graphicsLayer_polygon.addGraphic(graphic_polygon);

			        layers.add(graphicsLayer_polygon);                                              //visualize the polygon layer
				}
				
	        });
    	}
    });
    PlantOptButton.setSize(140, 30);
    PlantOptButton.setLocation(840, 80);
    
    // ZHOU add new buttons
    contentPane = new JLayeredPane();
  
    contentPane.setLayout(new BorderLayout(0,0));
    contentPane.setVisible(true);
    contentPane.add(PWbutton);
    contentPane.add(PWPrButton); 
    contentPane.add(APbutton);
    contentPane.add(APPrButton); 
    contentPane.add(APHrButton);
    contentPane.add(APPWButton);
    contentPane.add(refreshButton);
    //contentPane.add(change);
    contentPane.add(panel);
    contentPane.add(PrAPPWButton);
//    contentPane.add(PrAPOButton);
    contentPane.add(PrAPHrButton);
    contentPane.add(OPALRT);
//    contentPane.add(PrhydroButton);
    contentPane.add(QueryButton);
    contentPane.add(PlantOptButton);
    contentPane.add(map, BorderLayout.CENTER);
    contentPane.add(legend, BorderLayout.WEST);
    //contentPane.add(timeSlider, BorderLayout.SOUTH);
    
  
  //adding the graph here
    //map = createMap();
   //contentPane.add(map);
    
    //only until here
	
    
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
  
  //attach the webmap trial until return jmap 
  private JMap createMap() {

	    final JMap jMap = new JMap();
	        final InfoPopupOverlay popupOverlay = new InfoPopupOverlay();
	    jMap.addMapOverlay(popupOverlay);
	    // grab the ArcGISFeatureLayer when added to the map and associate it with the infopopup overlay
	    jMap.getLayers().addLayerListEventListener(new LayerListEventListenerAdapter() {
	            @Override
	      public void multipleLayersAdded(LayerEvent event) {
	        for (Layer layer : event.getChangedLayers().values()) {
	          if (layer instanceof ArcGISFeatureLayer) {
	            popupOverlay.addLayer(layer);
	          }
	          else if(layer instanceof GroupLayer) {
	            for(Layer groupedLayer: ((GroupLayer) layer).getLayers()) {
	              if(groupedLayer instanceof ArcGISFeatureLayer) {
	                popupOverlay.addLayer(groupedLayer);
	              }
	            }
	          }
	        }
	      }
	            @Override
	      public void layerAdded(LayerEvent event) {
	        Layer layer = event.getChangedLayer();
	        if (layer instanceof ArcGISFeatureLayer) {
	          popupOverlay.addLayer(layer);
	        } else if(layer instanceof GroupLayer) {
	            for(Layer groupedLayer: ((GroupLayer) layer).getLayers()) {
	              if(groupedLayer instanceof ArcGISFeatureLayer) {
	                popupOverlay.addLayer(groupedLayer);
	              }
	            }
	          }
	               }
	    });

	    // create and load the web map
	    /*WebMap webMap = null;
	    try {
	      webMap = WebMap.newInstance(MAP_ID, portal);
	      jMap.loadWebMap(webMap);
	      
	    } catch (Exception e) {
	      e.printStackTrace();
	    }
*/
	    return jMap;
	  }
  
//make graphic for query
  private void addSimpleFillGraphics(GraphicsLayer graphicsLayer2) {
      Polygon polygon = new Polygon();
      polygon.startPath(11541462.271, 140093.15);
      polygon.lineTo(11541482.67, 140116.169);
      polygon.lineTo(11541493.862, 140105.612);
      polygon.lineTo(11541474.098, 140083.07);
      polygon.closePathWithLine();

      SimpleLineSymbol outline = new SimpleLineSymbol(new Color(0, 200, 0),
          7, SimpleLineSymbol.Style.SOLID);
      SimpleFillSymbol symbol = new SimpleFillSymbol(
          new Color(255, 255, 255,130), outline);
      Graphic graphic2 = new Graphic(polygon,symbol);
      graphicsLayer2.addGraphic(graphic2);
  }

  
  
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
 

  
	  /** This new function add new pinpoint layers for the queryed entity
	   * @return 
	   * @throws IOException 
	   * @throws NumberFormatException */ 
	    @SuppressWarnings("resource")
	  public void GISFeature () throws NumberFormatException, IOException {	  	
	        
	      BufferedReader fileReader = null;
	      String line = null;
	  	  fileReader = new BufferedReader(new FileReader(GIS));	  	 	    
	  	    
	  	  while ((line = fileReader.readLine()) != null) {
	  		  String[] data = line.split(",");
	  		  planes.add(Double.parseDouble(data[0]),Double.parseDouble(data[1]));	  	  

	  	  }

	    };
 
  
}





//sdvgfgfsd

//fdbgb