package cam.dev.zhouxiaochi;

import java.util.HashMap;
import java.util.Map;

public class LayerFactoryInfoDictionary {

	private static LayerFactoryInfoDictionary instance = null;
	
	public Map<String, LayerFactoryInfo>  dictionary = new HashMap<String, LayerFactoryInfo>();
	private LayerFactoryInfoDictionary(){
		
		dictionary.put("JurongLandlotsLayer", new LayerFactoryInfo("JurongLandlotsLayer", "kml/LandLots.kml", FeatureServiceUpdater.LayerType.POLYGON, "^LandLotID_\\d+$"));
		dictionary.put("Jurong_WaterNetwork", new LayerFactoryInfo("Jurong_WaterNetwork",  "kml/WaterNetwork.kml", FeatureServiceUpdater.LayerType.POLYLINE, "^WaterPipe_\\d+$"));
		dictionary.put("JurongBuildingLayer", new LayerFactoryInfo("JurongBuildingLayer",  "kml/Buildings.kml", FeatureServiceUpdater.LayerType.POLYGON, "^BuildingID_\\d+$"));
		
		///electrical
		dictionary.put("PowerGenerators", new LayerFactoryInfo("PowerGenerators",  "kml/PowerGen.kml", FeatureServiceUpdater.LayerType.POLYGON, "^PowerGen_\\d+$"));
		dictionary.put("TransmissionLines", new LayerFactoryInfo("TransmissionLines",  "kml/UHT Lines (230kV).kml", FeatureServiceUpdater.LayerType.POLYLINE, "^UHT-\\d+$"));
		dictionary.put("EHTLines", new LayerFactoryInfo("EHTLines",  "kml/EHT Lines.kml", FeatureServiceUpdater.LayerType.POLYLINE, "^EHT-\\d+$"));
		dictionary.put("HTLines", new LayerFactoryInfo("HTLines",  "kml/HT Lines.kml", FeatureServiceUpdater.LayerType.POLYLINE, "^HT-\\d+$"));
		dictionary.put("TLPlant(22kV-11kV)", new LayerFactoryInfo("TLPlant(22kV-11kV)",  "kml/TLPlant(22kV-11kV).kml", FeatureServiceUpdater.LayerType.POLYLINE, "^PHT-[5|6|7|8]$"));
		dictionary.put("TLPlant(22kV-3.4kV)", new LayerFactoryInfo("TLPlant(22kV-3.4kV)", "kml/TLPlant(22kV-3.4kV).kml", FeatureServiceUpdater.LayerType.POLYLINE, "^PHT-[9|10|11|12|13|14|15|16]$"));
		dictionary.put("TLPlant(3.4kV-3kV)", new LayerFactoryInfo("TLPlant(3.4kV-3kV)",  "kml/TLPlant(3.4kV-3kV).kml", FeatureServiceUpdater.LayerType.POLYLINE, "^PLT-[1|2|3|4|5|6|7|8]$"));
		dictionary.put("TLPlant(3kV-0.4kV)", new LayerFactoryInfo("TLPlant(3kV-0.4kV)",  "kml/TLPlant(3kV-0.4kV).kml", FeatureServiceUpdater.LayerType.POLYLINE, "^PLT-[9|10|11|12|13|14|15|16|17|18|19]"));
		dictionary.put("TLPlant(main-22kV)", new LayerFactoryInfo("TLPlant(main-22kV)",  "kml/TLPlant(main-22kV).kml", FeatureServiceUpdater.LayerType.POLYLINE, "^PHT-[1|2|3|4]$"));
		dictionary.put("JurongTransportationSystem", new LayerFactoryInfo("JurongTransportationSystem",  "kml/Public Roads.kml", FeatureServiceUpdater.LayerType.POLYGON, "^\\w+[Road|Avenue|Drive|Highway|Place|view]\\w*$"));

	}
	
	public static Map<String, LayerFactoryInfo> getDictionary(){
		
		if(instance == null){
			instance = new  LayerFactoryInfoDictionary();
		}
	 return instance.dictionary;
	}
	
	
	
	
	
}
