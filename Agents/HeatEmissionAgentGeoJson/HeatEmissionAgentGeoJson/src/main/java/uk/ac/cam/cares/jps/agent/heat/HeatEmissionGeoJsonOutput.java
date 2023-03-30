package uk.ac.cam.cares.jps.agent.heat;

import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import com.jayway.jsonpath.JsonPath;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.PrintWriter;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import javax.servlet.annotation.WebServlet;
import java.io.FileWriter;
import java.io.IOException;
import org.springframework.stereotype.Controller;
import geotrellis.proj4.CRS;
import geotrellis.proj4.Transform;
import uk.ac.cam.cares.jps.base.util.CRSTransformer;
import scala.Function2;
import scala.Tuple2;



@Controller
@WebServlet(urlPatterns = {"/performheatgeojson"})

public class HeatEmissionGeoJsonOutput extends JPSAgent {

	private static final long serialVersionUID = 1L;

	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {	
		
		JSONObject jsonMessage = new JSONObject();
			
		StringBuffer Combined = new StringBuffer("{\r\n" + 
        		"\"type\": \"FeatureCollection\",\r\n" + 
        		"\"features\": [\n");
		
		double heat_data = 0;
		JSONObject sample = new JSONObject();
		sample.put("lower_bounds", "8464.256074442204#23588.08319044689#0");
		sample.put("upper_bounds", "17619.669922658715#30520.376177137474#105");
		JSONObject sample1 = new JSONObject();
		sample1.put("job", sample); 
        
        JSONArray heatresult = new JSONArray();
        JSONArray IRIandCO2QueryResult = IRIandCO2Query();
        
        for (int i = 0; i < IRIandCO2QueryResult.length(); i++) {
        	System.out.println("AA");
        	System.out.println(i);
        	String IRI = IRIandCO2QueryResult.getJSONObject(i).getString("IRI");
        	String CO2 = IRIandCO2QueryResult.getJSONObject(i).getString("CO2");
        	String ChemPlant = IRIandCO2QueryResult.getJSONObject(i).getString("chemical_plant");
        	String ChemPlantName = "<"+ChemPlant+">";
    
        	JSONArray plantInfoQueryResult = FuelCEIEfficiency(ChemPlantName);
            String CEI = plantInfoQueryResult.getJSONObject(0).getString("CEI");
            String Efficiency = plantInfoQueryResult.getJSONObject(0).getString("efficiency");
        	
            JSONArray coordiSpatialQueryResult = CoordinateQuery(IRI);  
            String heatcoordi = HeatEmissionCoordinate(coordiSpatialQueryResult);
            String[] heatcoordi_split = heatcoordi.split("#");
            Double x_coordinate =  Double.parseDouble(heatcoordi_split[0]);
            Double y_coordinate =  Double.parseDouble(heatcoordi_split[1]);
            
    		double xi = x_coordinate;
            double yi = y_coordinate;
            
            double[] region_boundary = Boundary(sample1);
            if (region_boundary[0] < x_coordinate && region_boundary[2] > x_coordinate && region_boundary[1] < y_coordinate && region_boundary[3] > y_coordinate) {
                double heatamount = 0.3*Double.parseDouble(CO2)/Double.parseDouble(CEI)*1e12/365/24/3600/1e6*Double.parseDouble(Efficiency); 
                JSONObject row = new JSONObject();
                row.put("Coordinate", heatcoordi);
                row.put("Heat Emission", heatamount);
                heatresult.put(row);   
                heat_data = heatamount;
            }
            
        String inputCRS = "EPSG:24500";
        String outputCRS = "EPSG:4326";
        
        String inputSys = inputCRS.split(":")[1];
        int inputCode = Integer.valueOf(inputSys);
        String outputSys = outputCRS.split(":")[1];
        int outputCode = Integer.valueOf(outputSys);
        CRS sourceCRS = CRS.fromEpsgCode(inputCode);
        CRS targetCRS = CRS.fromEpsgCode(outputCode);
        Function2<Object, Object, Tuple2<Object, Object>> convert = Transform.apply(sourceCRS,targetCRS);
		
        Tuple2<Object, Object> res;
        res = convert.apply(xi,yi);
        double xt = (double) res._1();
        double yt = (double) res._2();
        
        String x_coordi = Double.toString(xt);
        String y_coordi = Double.toString(yt);
        
        String[] xyz_coordi = heatcoordi.split("#"); 	
		String z_coordi =  xyz_coordi[2];
		
                    
        StringBuffer heatemission = new StringBuffer("{\n\"type\": \"Feature\",\n");
        heatemission.append("\"properties\": {\n\"height:m\":").append(z_coordi+","+"\n");
        heatemission.append("\"AH_0:MW\":").append(heat_data).append(",\n");
        heatemission.append("\"AH_1:MW\":").append(heat_data).append(",\n");
        heatemission.append("\"AH_2:MW\":").append(heat_data).append(",\n");
        heatemission.append("\"AH_3:MW\":").append(heat_data).append(",\n");
        heatemission.append("\"AH_4:MW\":").append(heat_data).append(",\n");
        heatemission.append("\"AH_5:MW\":").append(heat_data).append(",\n");
        heatemission.append("\"AH_6:MW\":").append(heat_data).append(",\n");
        heatemission.append("\"AH_7:MW\":").append(heat_data).append(",\n");
        heatemission.append("\"AH_8:MW\":").append(heat_data).append(",\n");
        heatemission.append("\"AH_9:MW\":").append(heat_data).append(",\n");
        heatemission.append("\"AH_10:MW\":").append(heat_data).append(",\n");
        heatemission.append("\"AH_11:MW\":").append(heat_data).append(",\n");
        heatemission.append("\"AH_12:MW\":").append(heat_data).append(",\n");
        heatemission.append("\"AH_13:MW\":").append(heat_data).append(",\n");
        heatemission.append("\"AH_14:MW\":").append(heat_data).append(",\n");
        heatemission.append("\"AH_15:MW\":").append(heat_data).append(",\n");
        heatemission.append("\"AH_16:MW\":").append(heat_data).append(",\n");
        heatemission.append("\"AH_17:MW\":").append(heat_data).append(",\n");
        heatemission.append("\"AH_18:MW\":").append(heat_data).append(",\n");
        heatemission.append("\"AH_19:MW\":").append(heat_data).append(",\n");
        heatemission.append("\"AH_20:MW\":").append(heat_data).append(",\n");
        heatemission.append("\"AH_21:MW\":").append(heat_data).append(",\n");
        heatemission.append("\"AH_22:MW\":").append(heat_data).append(",\n");
        heatemission.append("\"AH_23:MW\":").append(heat_data).append(",\n").append("},\n");
        heatemission.append("\"geometry\": {\n" + "\"type\": \"Point\",\n" +  "\"coordinates\": [\n").append(x_coordi+",\n").append(y_coordi+"\n]\n}\n},");
        Combined.append(heatemission);
        }
        String outputFP = Paths.get(System.getProperty("user.dir"), "output","out.txt").toString();
		String output = Combined.substring(0,Combined.length()-1)+"\n]\n}";  
        File file1 = new File(outputFP);
        FileWriter fw;
		try {
			fw = new FileWriter(file1);
			PrintWriter pw = new PrintWriter(fw);
	        pw.println(output);
	        pw.close(); 
	        jsonMessage.accumulate("Result", "GeoJson heat data outputted.");
		} catch (IOException e) {
			e.printStackTrace();
		}
        
        return jsonMessage;
	}

    public static double[] Boundary(JSONObject inputBounds) {
        String upper_limits = JsonPath.read(inputBounds.toString(), "$.job.upper_bounds");
        String lower_limits = JsonPath.read(inputBounds.toString(), "$.job.lower_bounds");
        String[] upper_split = upper_limits.split("#");
        String[] lower_split = lower_limits.split("#");
        double[] result = new double[4];
        result[0] =  Double.parseDouble(lower_split[0]);
        result[1] =  Double.parseDouble(lower_split[1]);
        result[2] =  Double.parseDouble(upper_split[0]);
        result[3] =  Double.parseDouble(upper_split[1]);
        return result;
    }
    
    public static JSONArray IRIandCO2Query () {
    	StringBuffer IRIandCO2Query = new StringBuffer("PREFIX ns2: <https://www.theworldavatar.com/kg/ontobuiltenv/>\n");
    	IRIandCO2Query.append("PREFIX geo: <http://www.opengis.net/ont/geosparql#>\n");
    	IRIandCO2Query.append("PREFIX kb: <http://www.theworldavatar.com/kb/ontochemplant/>\n");
    	IRIandCO2Query.append("PREFIX ocp: <http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#>\n");
    	IRIandCO2Query.append("PREFIX om:  <http://www.ontology-of-units-of-measure.org/resource/om-2/>\n");
    	IRIandCO2Query.append("SELECT ?chemical_plant ?plant_item ?IRI ?CO2 ?unit WHERE {");
    	IRIandCO2Query.append("?chemical_plant geo:ehContains ?plant_item .");
        IRIandCO2Query.append("?plant_item ns2:hasOntoCityGMLRepresentation ?IRI .");
    	IRIandCO2Query.append("?plant_item ocp:hasIndividualCO2Emission ?x .");
    	IRIandCO2Query.append("?x om:hasNumericalValue ?CO2 .");
    	IRIandCO2Query.append("?x om:hasUnit ?a .");
    	IRIandCO2Query.append("?a om:symbol ?unit .");
    	IRIandCO2Query.append("FILTER regex(str(?plant_item), \"Plant_item\")");
    	IRIandCO2Query.append("FILTER EXISTS {?chemical_plant ocp:hasFuelType <http://www.theworldavatar.com/kb/ontochemplant/Naturalgasliquid>}.");
    	IRIandCO2Query.append("FILTER EXISTS {?chemical_plant ocp:hasFuelType <http://www.theworldavatar.com/kb/ontochemplant/Naturalgasliquid>}.}");
    	JSONArray IRIandCO2QueryResult = AccessAgentCaller.queryStore("jibusinessunits", IRIandCO2Query.toString());
    	return IRIandCO2QueryResult;
    }
    
    public static JSONArray FuelCEIEfficiency (String ChemialPlant) {
    	StringBuffer FuelCEIEffiQuery = new StringBuffer("PREFIX kb: <http://www.theworldavatar.com/kb/ontochemplant/>\n");
    	FuelCEIEffiQuery.append("PREFIX ocp: <http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#>\n");
    	FuelCEIEffiQuery.append("PREFIX om:  <http://www.ontology-of-units-of-measure.org/resource/om-2/>\n");
    	FuelCEIEffiQuery.append("SELECT ?fuel ?CEI ?unit ?efficiency WHERE {");
    	FuelCEIEffiQuery.append(ChemialPlant).append(" ocp:hasThermalEfficiency ?efficiency .");
    	FuelCEIEffiQuery.append(ChemialPlant).append(" ocp:hasFuelType ?fuel .");
    	FuelCEIEffiQuery.append("?fuel ocp:hasCarbonEmissionIndex ?cei .");
    	FuelCEIEffiQuery.append("?cei om:hasNumericalValue ?CEI .");
    	FuelCEIEffiQuery.append("?cei om:hasUnit ?a .");
    	FuelCEIEffiQuery.append("?a om:symbol ?unit .}");
    	JSONArray plantInfoQueryResult = AccessAgentCaller.queryStore("jibusinessunits", FuelCEIEffiQuery.toString());
    	return plantInfoQueryResult;
    }
    
    public static JSONArray CoordinateQuery (String CityFurnitureIRI) {
    	StringBuffer coordinateQuery = new StringBuffer("PREFIX ocgml: <http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#>\n");
    	coordinateQuery.append("SELECT ?geometricIRI ?polygonData WHERE {\n");	
    	coordinateQuery.append("GRAPH <http://www.theworldavatar.com:83/citieskg/namespace/jriEPSG24500/sparql/surfacegeometry/> {?geometricIRI ocgml:GeometryType ?polygonData.\n");
    	coordinateQuery.append("?geometricIRI ocgml:cityObjectId <").append(CityFurnitureIRI).append(">.}}");
    	JSONArray coordiSpatialQueryResult = AccessAgentCaller.queryStore("jriEPSG24500", coordinateQuery.toString());
    	return coordiSpatialQueryResult;
    }
  
    public static String HeatEmissionCoordinate (JSONArray coordiSpatialQueryResult) {
    	String buildingX = "0";
    	String buildingY = "0";
    	String buildingZ = "0";
    	
    	for (int i = 0; i < coordiSpatialQueryResult.length() ; i++) {
            JSONObject coordiS = coordiSpatialQueryResult.getJSONObject(i);
            String coordiData = coordiS.getString("polygonData");
            ArrayList<String> z_values = new ArrayList<>();
            String[] split =coordiData.split("#");
            double sum_x = 0; double sum_y = 0;
            double sum_z = 0; double min_z = 0;
          
            for(Integer j=1; j<=split.length; j++) {
                if(j%3==0){
                    z_values.add(split[j-1]);                
                    sum_x = sum_x + Double.parseDouble(split[j-3]);
                    sum_y = sum_y + Double.parseDouble(split[j-2]);
                    sum_z = sum_z + Double.parseDouble(split[j-1]); 
                    min_z = Math.min(min_z,Double.parseDouble(split[j-1]));
                }
            }
            if (min_z == sum_z/(split.length/3) && !z_values.isEmpty()) {
            	buildingX = String.valueOf(sum_x/(split.length/3));
            	buildingY = String.valueOf(sum_y/(split.length/3));
            	} 
            if (!z_values.isEmpty() && Double.parseDouble(buildingZ) < Double.parseDouble(Collections.max(z_values))) {
            	buildingZ = Collections.max(z_values);
            }
        }
        StringBuffer coordinate = new StringBuffer();
    	coordinate.append(buildingX).append("#").append(buildingY).append("#").append(buildingZ);	
    	return coordinate.toString();
    }
    
    public static String[] ReadCol(int col, String filepath, String delimiter) {
        String currentLine;
        String[] data;
        ArrayList<String> colData = new ArrayList<String>();

        try {
            FileReader fr = new FileReader(filepath);
            BufferedReader br = new BufferedReader(fr);
            while ((currentLine = br.readLine()) != null) {
                data = currentLine.split(delimiter);
                colData.add(data[col]);
            }
        } catch (Exception e) {
            System.out.println(e);
            return null;
        }
        return colData.toArray(new String[0]);
    }  
}
