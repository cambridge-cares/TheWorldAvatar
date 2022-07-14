package uk.ac.cam.cares.jps.agent.heat;

import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import com.jayway.jsonpath.JsonPath;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.update.UpdateRequest;
import org.apache.jena.graph.NodeFactory;

public class HeatEmissionQuery extends JSONObject{
	
	//String inputBounds_sample = "{\"job\":{\"lower_bounds\":\"8464.256074442204#23588.08319044689#0\",\"upper_bounds\":\"17619.669922658715#30520.376177137474#105\"}}\r\n";	
	/**
     * This part is the main method that takes input form HeatEmissionAgent and pass them in the
     * beow mentioned queries. The query in "jibusinessunits" returns the lists of chemical 
     * plants, plant items, IRIs, CO2 emission, fuel CEI and thermal efficiency; the query in 
     * "jriEPSG24500" helps return the heat emission coordinates after "HeatEmissionCoordinate"
     * calculation. After the calculation of heat emission values based on CO2 emission, CEI and 
     * thermal efficiency, we return a final list that contains the heat emission values and 
     * respective coordinates within a specified region.
     */
	
    public static JSONObject performCrossDomainQ(JSONObject jsonObject) throws SQLException {
        JSONArray heatresult = new JSONArray();
        JSONObject heatresult1 = new JSONObject();
        JSONArray IRIandCO2QueryResult = IRIandCO2Query();
        
        for (int i = 0; i < IRIandCO2QueryResult.length(); i++) {
        	String IRI = IRIandCO2QueryResult.getJSONObject(i).getString("IRI");
        	String CO2 = IRIandCO2QueryResult.getJSONObject(i).getString("CO2");
        	String Plant_item = IRIandCO2QueryResult.getJSONObject(i).getString("plant_item");
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
            
            // Calculate the heat emission amount in the unit of MW within the indicated region boundary 
            double[] region_boundary = Boundary(jsonObject);
            if (region_boundary[0] < x_coordinate && region_boundary[2] > x_coordinate && region_boundary[1] < y_coordinate && region_boundary[3] > y_coordinate) {
                double heatamount = Double.parseDouble(CO2)/Double.parseDouble(CEI)*1e12/365/24/3600/1e6*Double.parseDouble(Efficiency); 
                JSONObject row = new JSONObject();
                row.put("Coordinate", heatcoordi);
                row.put("Heat Emission", heatamount);
                heatresult.put(row);
                new HeatEmissionQuery().sparqlUpdate(Plant_item,Double.toString(heatamount));
            }
        }
        heatresult1.put("result", heatresult); 	
        return heatresult1;
    }
    
    // Set up the region boundary
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
    
    // Query all the chemical plants, plant items and respective IRI as well as CO2 emissions
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
    	IRIandCO2Query.append("FILTER regex(str(?plant_item), \"Plant_item\")}");
    	JSONArray IRIandCO2QueryResult = AccessAgentCaller.queryStore("jibusinessunits", IRIandCO2Query.toString());
    	return IRIandCO2QueryResult;
    }
    
    // Chemical plant fuel, CEI and thermal efficiency query
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
    
    // Geometric coordination query
    public static JSONArray CoordinateQuery (String CityFurnitureIRI) {
    	//StringBuffer coordinateQuery = new StringBuffer("PREFIX ocgml: <http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#> SELECT ?s ?o WHERE {GRAPH <http://www.theworldavatar.com:83/citieskg/namespace/jriEPSG24500/sparql/surfacegeometry/>{?s ocgml:GeometryType ?o.?s ocgml:cityObjectId <http://www.theworldavatar.com:83/citieskg/namespace/jriEPSG24500/sparql/cityfurniture/UUID_31385923-9cf7-4e4e-b134-165117b4e3e2/>.}}");
    	StringBuffer coordinateQuery = new StringBuffer("PREFIX ocgml: <http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#>\n");
    	coordinateQuery.append("SELECT ?geometricIRI ?polygonData WHERE {\n");	
    	coordinateQuery.append("GRAPH <http://www.theworldavatar.com:83/citieskg/namespace/jriEPSG24500/sparql/surfacegeometry/> {?geometricIRI ocgml:GeometryType ?polygonData.\n");
    	coordinateQuery.append("?geometricIRI ocgml:cityObjectId <").append(CityFurnitureIRI).append(">.}}");
    	JSONArray coordiSpatialQueryResult = AccessAgentCaller.queryStore("jriEPSG24500", coordinateQuery.toString());
    	return coordiSpatialQueryResult;
    }
  
    // Calculate heat emission in xyz coordinate 
    public static String HeatEmissionCoordinate (JSONArray coordiSpatialQueryResult) {
    	String buildingX = "0";
    	String buildingY = "0";
    	String buildingZ = "0";
    	
    	for (int i = 0; i < coordiSpatialQueryResult.length(); i++) {
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
    
    // Put the heat emssion data into the blazegraph via SPARQL update
    public void sparqlUpdate (String Plant_item, String Heat_value) {
    	String Heat_of_plantitem = Plant_item+"_Heat";
    	String rdf_label = Heat_of_plantitem.substring(47);
    	
    	UpdateBuilder ub =
                new UpdateBuilder()
                        .addPrefix("rdf", "https://www.w3.org/1999/02/22-rdf-syntax-ns")
                        .addPrefix("rdfs", "https://www.w3.org/2000/01/rdf-schema#")
                        .addInsert(NodeFactory.createURI(Plant_item),NodeFactory.createURI("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasGeneratedHeat"), NodeFactory.createURI(Heat_of_plantitem))
                        .addInsert(NodeFactory.createURI(Heat_of_plantitem),"rdf:type",NodeFactory.createURI("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#GeneratedHeat"))
                        .addInsert(NodeFactory.createURI(Heat_of_plantitem),NodeFactory.createURI("http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue"),Heat_value)
                        .addInsert(NodeFactory.createURI(Heat_of_plantitem),"rdfs:label",rdf_label)
    	                .addInsert(NodeFactory.createURI(Heat_of_plantitem),NodeFactory.createURI("http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit"), NodeFactory.createURI("http://www.theworldavatar.com/kb/ontochemplant/MegaWatt"));
    	
    	UpdateRequest ur = ub.buildRequest();
    	System.out.println(ur);
    	AccessAgentCaller.updateStore("http://host.docker.internal:48080/ontochemplant", ur.toString()); 
    	//This line below is for update the data in Claudius 
    	//AccessAgentCaller.updateStore("jibusinessunits", ur.toString()); 
    }
}
