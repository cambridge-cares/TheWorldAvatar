package uk.ac.cam.cares.jps.men;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class MenResult {

	public double objValue = -1.;
	public double totalMaterialPurchaseCost = -1.;
	public double totalMaterialPurchaseCostInternationalMarket = -1.;
	public double totalTransportationCost = -1.;
	public double totalCO2Emission = -1.;	// in ton C02
	public double totalC02EmissionCost = -1.;
	public double totalInstallationCost = -1;
	private String[] columnNames = null;
	private Map<String, List<Double>> mapRawNameToRaw = new HashMap<String, List<Double>>();
	
	public MenResult(String[] columnNames) {
		this.columnNames = columnNames;
	}
	
	public void addRaw(String rawName, List<Double> raw) {
		mapRawNameToRaw.put(rawName, raw);
	}
	
	public List<Double> getRaw(String transportationName) {
		return mapRawNameToRaw.get(transportationName);
	}
	
	public String toString() {
		StringBuffer b = new StringBuffer();
		b.append("MenResult[objvalue=" + objValue);
		b.append(", totalMaterialPurchaseCost=" + totalMaterialPurchaseCost);
		b.append(", totalMaterialPurchaseCostIntMarket=" + totalMaterialPurchaseCostInternationalMarket);
		b.append(", totalTransportationCost=" + totalTransportationCost);
		b.append(", totalC02Emission=" + totalCO2Emission);
		b.append(", totalC02EmissionCost=" + totalC02EmissionCost);
		b.append(", totalInstallationCost=" + totalInstallationCost);
		b.append("]]");
		return b.toString();
	}
	
	public String convertMatrixToString() {
		StringBuffer b = new StringBuffer();
		
		b.append("Type");
		for (String columnName : columnNames) {
			b.append(" ").append(columnName);
		}
		
		//put the content of the table
		for (String key : mapRawNameToRaw.keySet()) {
			String row = "";
			for (double d : mapRawNameToRaw.get(key)) {
				row += " " + d;
			}
			b.append("\n").append(key).append(row);
		}
		
		return b.toString();
	}
}
