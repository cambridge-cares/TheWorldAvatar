package uk.ac.cam.cares.jps.powsys.electricalnetwork;

import java.io.IOException;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Property;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.powsys.listener.LocalOntologyModelManager;
import uk.ac.cam.cares.jps.powsys.util.Util;

@WebServlet(urlPatterns = {"/AggregationEmissionAgent/aggregateemission"})
public class AggregationEmissionAgent extends JPSAgent{
    /**
	 * 
	 */
	private static final long serialVersionUID =  6859324316966357379L;;
    private static final String EM_RATE = "_EmissionRate";
    //both only called by front end javascript; update to chimney, then query to sum to give to front end
    
    private String getGenInfo() {
    	try {
    	String genInfo = Util.getGenEmission().addVar("?plant")
    			.addPrefix("j2", "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#")
    			.addWhere("?entity", "j2:isSubsystemOf", "?plant")
        		.addFilter("?V_Actual_CO2_Emission > 0.0")
        		.addFilter("?V_Design_CO2_Emission > 0.0")
    			.buildString();
    	return genInfo;}
    	catch (Exception e) {
    		e.printStackTrace();
    		return "";
    	}
    }
    		
    private String getPlantInfo() {

    	try {
    	SelectBuilder sb = new  SelectBuilder().addPrefix("j1", "http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#")
    			.addPrefix("j2","http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#")
    			.addVar("chimney").addWhere("?entity", "a", "j1:PowerPlant").addWhere("?entity" ,"j2:hasSubsystem", "?chimney");
    	return sb.buildString();
    	}
    	catch (Exception e) {
    		e.printStackTrace();
    		return "";
    	}
    }
    private String getChimneyInfo() {
    	String sb = new SelectBuilder().addPrefix("j2", "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#")
	            .addPrefix("j3", "http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#")
	            .addPrefix("j4", "http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#")
	            .addPrefix("j5", "http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#")
	            .addPrefix("j6", "http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/chemical_process_system.owl#")
    	        .addPrefix("j7", "http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#")
    	        .addPrefix("j8", "http://www.theworldavatar.com/ontology/ontocape/material/material.owl#")
	            .addPrefix("j9","http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#")
    	        .addVar("?vheightchimney").addVar("?vdiameterchimney").addVar("?vmassf").addVar("?vtemp").addVar("?vdens")
    	        .addWhere("?entity", "a", "j3:Pipe")
    	        .addWhere("?entity", "j3:hasHeight", "?heightchimney")
    	        .addWhere("?heightchimney", "j2:hasValue", "?vheightchimney")
    	        .addWhere("?entity", "j3:hasInsideDiameter", "?diameterchimney")
    	        .addWhere("?diameterchimney", "j2:hasValue", "?vdiameterchimney")
    	        
	            .addWhere("?entity","j4:realizes", "?proc").addWhere("?proc","j5:hasOutput", "?waste")
    	        .addWhere("?waste" ,"j6:refersToGeneralizedAmount", "?genwaste")
    	        .addWhere("?genwaste" ,"j2:hasProperty", "?massf").addOptional("?massf" ,"j2:hasValue", "?vmassf")
    	        .addWhere("?genwaste" ,"j2:hasSubsystem", "?matamount").addWhere("?matamount" ,"j7:refersToMaterial", "?mat")
    	        .addWhere("?mat" ,"j8:thermodynamicBehavior", "?thermo").addOptional("?thermo" ,"j9:has_temperature", "?temp")
    	        .addWhere("?temp","j2:hasValue", "?vtemp")
    	        .addWhere("?thermo","j9:has_density", "?dens").addWhere("?dens","j2:hasValue", "?vdens")
    	        .buildString();
    	     
    	return sb;
    }
    
    @Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(AggregationEmissionAgent.class);
    }
    Logger logger = LoggerFactory.getLogger(AggregationEmissionAgent.class);
    @Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
	    requestParams = processRequestParameters(requestParams, null);
	    return requestParams;
	}
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams,HttpServletRequest request) {
    	boolean v = validateInput(requestParams); 
    	if (v== false) {
    		return new JSONObject().put("Error", "Input parameters not valid!");
    	}
    	String iriofnetwork = requestParams.getString("electricalnetwork");
    	JSONObject result=updateEmission(iriofnetwork);
	    List<Object> chimneylist = result.getJSONArray("chimney").toList();
	    List<Object> desco2list = result.getJSONArray("designemission").toList();
	    double totalemissionactual=0.0;
	    double totalemissiondesign=0.0;
	    String parametername = "CO2"; //hard coded at the moment
	    Map hmap = LocalOntologyModelManager.getSpeciesMap();
	    OntModel jenaOwlModel = JenaHelper.createModel();
	    for (int x=0;x<chimneylist.size();x++) {
		  	String iriofchimney=chimneylist.get(x).toString();
		  	System.out.println("what is iri of chimney:"+iriofchimney);
		  	jenaOwlModel.read(iriofchimney);
		  	Individual valueofspeciesemissionrate = jenaOwlModel
	          .getIndividual(iriofchimney.split("#")[0] + "#V_" + hmap.get(parametername) + EM_RATE);
		  	Double val=valueofspeciesemissionrate.getPropertyValue((Property) LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_NUMVAL)).asLiteral().getDouble();
		  	totalemissionactual=totalemissionactual+val;
		  	totalemissiondesign=totalemissiondesign+Double.valueOf(desco2list.get(x).toString());
	
	    }
	    JSONObject newresult= new JSONObject();
	    DecimalFormat df = new DecimalFormat("#.####");
	    df.setRoundingMode(RoundingMode.CEILING);
	    newresult.put("actual",df.format(totalemissionactual/1000000*3600)); //from kg/s back to ton/hr
	    newresult.put("design",df.format(totalemissiondesign));
	    return newresult;
    }
    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (requestParams.isEmpty()) {
            throw new BadRequestException();
        }
        try {
        String iriofnetwork = requestParams.getString("electricalnetwork");
        return InputValidator.checkIfValidIRI(iriofnetwork);
        } catch (JSONException ex) {
        	ex.printStackTrace();
        	throw new JSONException("IRI not found");
        }
    }
    
    

    public JSONObject sumEmissionResult(String ENIRI) {
        List<String[]> genList = Util.provideGenlist(ENIRI);
        QueryBroker broker = new QueryBroker();
        List<String> plantunique = new ArrayList<String>();
        List<String> emplantunique = new ArrayList<String>();

        for (int d = 0; d < genList.size(); d++) {
        	String genInfo = getGenInfo();
        	String result = broker.queryFile(genList.get(d)[0], genInfo );
            String[] keys = JenaResultSetFormatter.getKeys(result);
            List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
			if (resultList.size() > 0) {
				if (!plantunique.contains(resultList.get(0)[3])) { // plant=resultList.get(0)[3]
					plantunique.add(resultList.get(0)[3]);
				}
				emplantunique.add(resultList.get(0)[2] + "separate" + resultList.get(0)[1] + "separate" + resultList.get(0)[3]);
			}
        }

        int sizeofplant = plantunique.size();
        System.out.println("uniqueplantsize= " + sizeofplant);
        Double[] plantactco2 = new Double[sizeofplant];
        Double[] plantdesco2 = new Double[sizeofplant];
        int index = 0;
        for (int t = 0; t < plantunique.size(); t++) {
            plantactco2[index] = 0.0;
            plantdesco2[index] = 0.0;

            for (int x = 0; x < emplantunique.size(); x++) {
                String name = emplantunique.get(x).split("separate")[2];
                String value = emplantunique.get(x).split("separate")[1];
                String desvalue = emplantunique.get(x).split("separate")[0];

                if (name.contains(plantunique.get(t))) {
                    plantactco2[index] = plantactco2[index] + Double.valueOf(value);
                    plantdesco2[index] = plantdesco2[index] + Double.valueOf(desvalue);
                    System.out.println("name="+name);
                    System.out.println(value);
                    System.out.println(desvalue);
                }
            }

            index++;
        }

        JSONObject ans = new JSONObject();
        JSONArray plant = new JSONArray();
        JSONArray chimney = new JSONArray();
        JSONArray emission = new JSONArray();
        JSONArray desemission = new JSONArray();
        for (int f = 0; f < plantunique.size(); f++) {
            String result = broker.queryFile(plantunique.get(f),  getPlantInfo());
            System.out.println("filequery= "+plantunique.get(f));
            String[] keys = JenaResultSetFormatter.getKeys(result);
            List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
            String iriofchimney;
            if(resultList.size()>0) {
            	iriofchimney=resultList.get(0)[0];
            }
            else {
            	String plantname=plantunique.get(f).split("#")[1];
            	iriofchimney="http://www.theworldavatar.com/kb/powerplants/"+plantname+"/Chimney-001.owl#Chimney-001";
            	//iriofchimney= QueryBroker.getIriPrefix() + "/powerplants/"+plantname+"/Chimney-001.owl#Chimney-001";
            	String sparqlStart = "PREFIX OCPSYST:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> \r\n"
    					+ "INSERT DATA { \r\n";
    			StringBuffer b = new StringBuffer();
    			b.append("<" + plantunique.get(f) + "> OCPSYST:hasSubsystem <" + iriofchimney + "> . \r\n");

    			String sparql = sparqlStart + b.toString() + "} \r\n";
    			new QueryBroker().updateFile(plantunique.get(f), sparql);
    			b = new StringBuffer();
            }
			if (!AgentLocator.isJPSRunningForTest()) {
				chimney.put(iriofchimney);
			} else {
				chimney.put("http://localhost:8080/kb" + iriofchimney.split("kb")[1]);
			}
            
            
            plant.put(plantunique.get(f));
            emission.put(plantactco2[f]);
            desemission.put(plantdesco2[f]);
        }
        ans.put("plant", plant);
        ans.put("emission", emission);
        ans.put("designemission", desemission);
        ans.put("chimney", chimney);

//		System.out.println(plantunique.get(2));
//		System.out.println("total actco2 for plant 1= " + plantactco2[2]);

        return ans;
    }


    public JSONObject updateEmission(String ENIRI) {//read from the generator to write it to chimney 
    	String chimneyiriName = null;
        
    	JSONObject ans = sumEmissionResult(ENIRI);
        List<Object> chimneylist = ans.getJSONArray("chimney").toList();
        List<Object> emissionlist = ans.getJSONArray("emission").toList();
        int size = chimneylist.size();
        for (int d = 0; d < size; d++) {
			try {
				chimneyiriName=chimneylist.get(d).toString();
				OntModel jenaOwlModel = LocalOntologyModelManager.createChimneyModelForChimneyIRI(chimneyiriName);
				startConversion(jenaOwlModel, chimneyiriName, emissionlist.get(d).toString());
			} catch (IOException e) {
				throw new JPSRuntimeException(e);
			}
        }
		return ans;


    }

    private void startConversion(OntModel jenaOwlModel, String iriOfChimney, String emission)
            throws IOException {
        doConversion(jenaOwlModel, iriOfChimney, emission);
        
        // save the updated model
        //LocalOntologyModelManager.saveToOwl(jenaOwlModel, iriOfChimney); // for each owl file
        String content = JenaHelper.writeToString(jenaOwlModel);
        new QueryBroker().putOld(iriOfChimney, content);

    }

    private void doConversion(OntModel jenaOwlModel, String iriofchimney, String emission) throws JSONException {
    	
    	/**asumption data added to be used by adms (https://www.steelcon.com/en/soedertaejle/):
    	 * diameter=3m
    	 * height= 110m 
    	 * Tout=587oC (from the wiki ccgt)
    	 * density=1.225 kg/m3 (near air)
    	 * efflux rate= 210.3 kg/s (after rough calculation from the wikipedia formula)
    	 */
    	
    	
    	
        Map hmap = LocalOntologyModelManager.getSpeciesMap();
        //reset all the emission rate to be zero
        for (int b = 0; b < hmap.size(); b++) {
            String ks = (String) hmap.get(hmap.keySet().toArray()[b].toString());
            Individual valueofspeciesemissionrate = jenaOwlModel.getIndividual(iriofchimney.split("#")[0] + "#V_" + ks + EM_RATE);
            valueofspeciesemissionrate.setPropertyValue((Property) LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_NUMVAL),
                    jenaOwlModel.createTypedLiteral(Double.valueOf(0)));
        }

      //update the necessary values needed
        String parametername = "CO2"; //hard coded at the moment
        
        Double parametervalue = Double.valueOf(emission)*1000000/3600; //ton per hour to g/s
        if (hmap.get(parametername) != null) {
            Individual valueofspeciesemissionrate = jenaOwlModel
                    .getIndividual(iriofchimney.split("#")[0] + "#V_" + hmap.get(parametername) + EM_RATE);
            valueofspeciesemissionrate.setPropertyValue(
                    (Property) LocalOntologyModelManager.getConcept(LocalOntologyModelManager.CPT_NUMVAL),
                    jenaOwlModel.createTypedLiteral(parametervalue));
        }

        ResultSet resultSet = JenaHelper.query(jenaOwlModel, getChimneyInfo());
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultListfromquery = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		DatatypeProperty numval = jenaOwlModel.getDatatypeProperty(
				"http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
		Individual vheightchimney = jenaOwlModel.getIndividual(resultListfromquery.get(0)[0]);
		vheightchimney.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(110.0)));
		Individual vdiameterchimney = jenaOwlModel.getIndividual(resultListfromquery.get(0)[1]);
		vdiameterchimney.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(3.0)));
		Individual vmassf = jenaOwlModel.getIndividual(resultListfromquery.get(0)[2]);
		vmassf.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(210.3)));
		Individual vtemp = jenaOwlModel.getIndividual(resultListfromquery.get(0)[3]);
		vtemp.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(587.0)));
		Individual vdens = jenaOwlModel.getIndividual(resultListfromquery.get(0)[4]);
		vdens.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(1.225)));
		
		

    }

}
