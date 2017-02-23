package cam.dev.zhouxiaochi;

import java.util.HashMap;
import java.util.Map;

import cam.dev.zhouxiaochi.FeatureServiceUpdater.LayerType;

public class LayerFactoryInfo {

	private String   layerName;
	private String   owlSource;
	private String   kmlSource;
	private FeatureServiceUpdater.LayerType   geoType;
	private String   deviceNameRegex;
	public String getKmlSource() {
		return kmlSource;
	}
	public FeatureServiceUpdater.LayerType getGeoType() {
		return geoType;
	}
	public String getDeviceNameRegex() {
		return deviceNameRegex;
	}

	
	public String getOwlSource() {
		return owlSource;
	}
	public void setOwlSource(String owlSource) {
		this.owlSource = owlSource;
	}
	public String getLayerName() {
		return layerName;
	}
	public LayerFactoryInfo(String layerName, String kmlSource, LayerType geoType, String deviceNameRegex) {
		super();
		this.layerName = layerName;
		this.kmlSource = kmlSource;
		this.geoType = geoType;
		this.deviceNameRegex = deviceNameRegex;
	}

	
	
}
