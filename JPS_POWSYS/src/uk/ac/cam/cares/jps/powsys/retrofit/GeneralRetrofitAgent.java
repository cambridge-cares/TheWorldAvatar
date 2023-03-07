package uk.ac.cam.cares.jps.powsys.retrofit;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.RDFNode;
import org.json.JSONObject;
import org.json.JSONArray;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.query.sparql.JenaModelWrapper;
import uk.ac.cam.cares.jps.base.query.sparql.Paths;
import uk.ac.cam.cares.jps.base.query.sparql.PrefixToUrlMap;
import uk.ac.cam.cares.jps.base.query.sparql.Prefixes;
import uk.ac.cam.cares.jps.base.query.sparql.QueryBuilder;
import uk.ac.cam.cares.jps.base.util.FileUtil;
import uk.ac.cam.cares.jps.powsys.util.Util;
import java.util.Iterator;

public class GeneralRetrofitAgent extends JPSAgent implements Prefixes, Paths {
		
    /**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(GeneralRetrofitAgent.class);
    }
    Logger logger = LoggerFactory.getLogger(GeneralRetrofitAgent.class);
    
    //This method should not be called, but overriden by something else
    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
		return processRequestParameters(requestParams, null);
	}
    
    
    public void retrofit(String electricalNetwork, List<String> nuclearPowerPlants, List<String> substitutionalGenerators) {
		
		
		
		logger.info("finished retrofitting");
	}
	
	protected String getQueryForSingleBus() {
		
		QueryBuilder builder = new QueryBuilder();
		builder.select("?entity", "?x", "?y", "?busnumber", "?busnumbervalue", "?bustypevalue", "?vm", "?vmvalue", "?baseKVvalue");
		builder.a("?entity", OPSREAL, "BusNode");
		builder.prop("?entity", "?x", PGISCOORDX);
		builder.prop("?entity", "?y", PGISCOORDY);
		builder.a("?busnumber", OPSMODE, "BusNumber");
		builder.prop("?entity", "?busnumber", OCPSYST, "isModeledBy", OCPMATH, "hasModelVariable");
		builder.prop("?busnumber", "?busnumbervalue", PVALNUMVAL);
		builder.a("?bustype", OPSMODE, "BusType");
		builder.prop("?entity", "?bustype", OCPSYST, "isModeledBy", OCPMATH, "hasModelVariable");
		builder.prop("?bustype", "?bustypevalue", PVALNUMVAL);
		builder.a("?vm", OPSMODE, "Vm");
		builder.prop("?entity", "?vm", OCPSYST, "isModeledBy", OCPMATH, "hasModelVariable");
		builder.prop("?vm", "?vmvalue", PVALNUMVAL);
		builder.a("?baseKV", OPSMODE, "baseKV");
		builder.prop("?entity", "?baseKV", OCPSYST, "isModeledBy", OCPMATH, "hasModelVariable");
		builder.prop("?baseKV", "?baseKVvalue", PVALNUMVAL);
		logger.info("QUERY:" + builder.build().toString());
		return builder.build().toString();
	}

	protected String getQueryForBusIRI() {
		return "PREFIX OCPSYST: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>" + 
		" SELECT ?entity WHERE{ ?network OCPSYST:hasSubsystem ?entity FILTER CONTAINS(str(?entity), \"EBus\")}";
	}
	
	protected String getQueryForGenerator() {
		QueryBuilder builder = new QueryBuilder();
		builder.select("?entity", "?x", "?y" , "?busnumber", "?busnumbervalue");
		builder.prop("?entity", "?x", PGISCOORDX);
		builder.prop("?entity", "?y", PGISCOORDY);
		builder.a("?busnumber", OPSMODE, "BusNumber");
		builder.prop("?entity", "?busnumber", OCPSYST, "isModeledBy", OCPMATH, "hasModelVariable");
		builder.prop("?busnumber", "?busnumbervalue", PVALNUMVAL);
		
		return builder.build().toString();
	}

	protected String getQueryForGenIRI() {
		return "PREFIX OCPSYST: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>" + 
		" SELECT ?entity WHERE{ ?network OCPSYST:hasSubsystem ?entity FILTER CONTAINS(str(?entity), \"EGen\")}";
	}
	
	protected List<String> getIRIList (ResultSet iri) {
		String qRes = JenaResultSetFormatter.convertToJSONW3CStandard(iri);
		List<String> result = new ArrayList();
		JSONObject jo = new JSONObject(qRes);
		JSONArray array = jo.getJSONObject("results").getJSONArray("bindings");
		for (int i=0; i<array.length(); i++) {
			
			JSONObject row = array.getJSONObject(i);
			for (String current : row.keySet()) {
				String value =  row.getJSONObject(current).getString("value");
				result.add(value);
			}
		}
		logger.info("IRI:" + result);
		
		return result;
	}

	public List<BusInfo> queryBuses(OntModel model) {
		
		List<BusInfo> result = new ArrayList<BusInfo>();
		
		String queryBusIRI = getQueryForBusIRI();

		ResultSet resultSet = JenaHelper.query(model, queryBusIRI);

		List<List<String[]>> buses = new ArrayList<List<String[]>>();

		List <String> iriList = getIRIList(resultSet);
		System.out.println("IRI BUS:" + iriList);
		for (String iri : iriList) {
			OntModel modelBus = JenaHelper.createModel(iri);
			String queryBus = getQueryForSingleBus();

			String resultSingle = JenaResultSetFormatter.convertToJSONW3CStandard(JenaHelper.query(modelBus, queryBus));
			buses.add(JenaResultSetFormatter.convertToListofStringArrays(resultSingle,"entity", "x", "y", "busnumber", "busnumbervalue", "bustypevalue", "vm", "vmvalue", "baseKVvalue"));
			
		}
		
		
		for (List<String[]> bus : buses){
			for (String[] current : bus) {
			
				BusInfo info = new BusInfo();
				info.busIri = current[0];
				
				info.x = Double.valueOf(current[1]);
				info.y = Double.valueOf(current[2]);
				info.busNumberIri = current[3];
				info.busNumber = current[4];
				info.busType = current[5];
				info.voltageMagnitudeIri = current[6];
				info.voltageMagnitude = Double.valueOf(current[7]);
				info.baseKV = Double.valueOf(current[8]);
				
				result.add(info);
			}
		}		
		
		
		return result;
	}
	
	protected BusInfo findFirstSlackBus(List<BusInfo> buses) {
		BusInfo slackBus = null;
		for (BusInfo current : buses) {
			String busType = current.busType;
			if ("3".equals(busType)) {
				logger.info("slack bus was found: busnumber = " + current.busNumber + ", busiri = " + current.busIri);
				slackBus = current;
				break;
			}
		}
		if (slackBus == null) {
			throw new JPSRuntimeException("No slack bus was found in the electrical network. The optimization was aborted.");
		}
		
		return slackBus;
	}

	public void deletePowerGeneratorsFromElectricalNetwork(String electricalNetwork, List<String> generators) {
		
		String sparqlStart = "PREFIX OCPSYST:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> \r\n" + "DELETE DATA { \r\n";
		StringBuffer b = new StringBuffer();
		
		for (int i=1; i<=generators.size(); i++) {
			String current = generators.get(i-1);
			b.append("<" + electricalNetwork + "> OCPSYST:hasSubsystem <" + current + "> . \r\n");
			if ((i % 5 == 0) || i == generators.size()) {
				String sparql = sparqlStart + b.toString() + "} \r\n";
				logger.info("deleting " + (i % 5) + " power generators from electrical network top node\n" + sparql);
				new QueryBroker().updateFile(electricalNetwork, sparql);
				b = new StringBuffer();
			}
		}
	}
	
	public void completePowerGenerator(OntModel model, String powerGenerator) {
		
		logger.info("adding additional attributes to nuclear power generator = " + powerGenerator);

		JenaModelWrapper w = new JenaModelWrapper(model, null);
		
		// read some values from the original OWL file
		RDFNode o = w.getPropertyValue(powerGenerator, PGISCOORDX);
		double x = o.asLiteral().getDouble();
		o = w.getPropertyValue(powerGenerator, PGISCOORDY);
		double y = o.asLiteral().getDouble();
		String[] pathIsSubsystemOf = new String[] {OCPSYST, "isSubsystemOf"};
		o = w.getPropertyValue(powerGenerator, pathIsSubsystemOf);
		logger.info("power generator in retrofit= "+powerGenerator);
		String plantIRI = o.asResource().getURI();
		
		String pgIri = PrefixToUrlMap.getPrefixUrl(OPSMODE) + "Pg";
		String[] pathPg = new String[] {OCPSYST, "isModeledBy", OCPMATH, "hasModelVariable", pgIri, OCPSYST, "hasValue", OCPSYST, "numericalValue"};
		o = w.getPropertyValue(powerGenerator, pathPg);
		double pgValue = o.asLiteral().getDouble();
		
		// replace the template IRI by the IRI of the power generator
		String path = Util.getResourceDir(this) + "/EGen-001_template.owl";
		String content = FileUtil.readFileLocally(path);
		String templateIRI = "http://www.theworldavatar.com/EGen-001_template.owl";
		content = content.replace(templateIRI + "#EGen-001", powerGenerator);
		String powerGeneratorWithoutFragment = powerGenerator;
		int i = powerGenerator.indexOf("#");
		if (i > 0) {
			powerGeneratorWithoutFragment = powerGenerator.substring(0, i);
		}
		content = content.replace(templateIRI, powerGeneratorWithoutFragment);
		
		// read the template into a model and update its values with those from the original OWL file
		model = JenaHelper.createModel();
		JenaHelper.readFromString(content, model);
		w = new JenaModelWrapper(model, null);
				
		w.setPropertyValue(powerGenerator, x, PGISCOORDX);
		w.setPropertyValue(powerGenerator, y, PGISCOORDY);
		w.setPropertyValue(powerGenerator, pgValue, pathPg);
		w.setPropertyValue(powerGenerator, plantIRI, pathIsSubsystemOf);
		
		// overwrite the original OWL file
		content = JenaHelper.writeToString(model);
		new QueryBroker().put(powerGenerator, content);
	}
	
	
	//query new object still not here
	
	public void addGeneratorsToElectricalNetwork(String electricalNetwork, List<GeneratorInfo> generators) {
		
		String sparqlStart = "PREFIX OCPSYST:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> \r\n" + "INSERT DATA { \r\n";
		StringBuffer b = new StringBuffer();
		
		for (int i=1; i<=generators.size(); i++) {
			String current = generators.get(i-1).generatorIri;
			//String instancename=current.split("#")[1];
			//should this one below exist???
			if(!current.contains("jps")) { //only apply for other than nuclear
				System.out.println("current="+current);
				current = QueryBroker.getIriPrefix() + current.split("kb")[1];
			}
			
			b.append("<" + electricalNetwork + "> OCPSYST:hasSubsystem <" + current + "> . \r\n");
			if ((i % 5 == 0) || i == generators.size()) {
				String sparql = sparqlStart + b.toString() + "} \r\n";
				logger.info("inserting " + (i % 5) + " power generators to electrical network top node\n" + sparql);
				new QueryBroker().updateFile(electricalNetwork, sparql);
				b = new StringBuffer();
			}
		}
	}
	
	private double distance(double x1, double y1, double x2, double y2) {
		// TODO-AE SC URGENT 20190429 distance according to WSG84 degree instead of meters?
		 double distance = Math.sqrt(Math.pow(x1 - x2, 2) + Math.pow(y1 - y2, 2));
		 return distance;
	}
	
	protected void connectGeneratorToBus(String generatorIri, String busNumberIri, String busNumber) {
		
		int busNumberValue = Integer.valueOf(busNumber);
		
		OntModel modelGen = JenaHelper.createModel(generatorIri);
		JenaModelWrapper w = new JenaModelWrapper(modelGen, null);
		w.setPropertyValue(busNumberIri, busNumberValue, PVALNUMVAL);
		
		System.out.println("geniri=" +generatorIri);
		String genname=generatorIri.split("#")[1];
		String content = JenaHelper.writeToString(modelGen);
		//String irinew = QueryBroker.getIriPrefix() + "/sgp/pvsingaporenetwork/"+genname+".owl#"+genname;
		String irinew= QueryBroker.getIriPrefix() +generatorIri.split("kb")[1];
		if(!generatorIri.contains("jps")) {
			content=content.replace(generatorIri, irinew);
			System.out.println("newgeniri=" +irinew);
			generatorIri=irinew;
		}
		// overwrite the original OWL file
		//String content = JenaHelper.writeToString(modelGen);
		new QueryBroker().putOld(generatorIri, content);
	}
	
	public void connectGeneratorToOptimalBus(List<BusInfo> buses, List<GeneratorInfo> generators, BusInfo slackBus) {
		logger.info("connecting generators to optimal buses, number of buses = " + buses.size() + ", number of generators = " + generators.size());
		
		// find the closest bus under the following constraints:
		// 1. The closest bus must not be the slack bus
		// 2. The closest bus must have baseKV around 230 kV (say plus minus 1%)
		// map the value to each generator
		double baseKVmin = 230 * 0.99;
		double baseKVmax = 230 * 1.01;
				
		for (GeneratorInfo current : generators) {
							
			// calculate the distance of the current generator to the closest bus
			double distanceToClosestBus = -1;
			BusInfo closestBus = null;
			for (BusInfo currentBus : buses) {
				double dist = distance(current.x, current.y, currentBus.x, currentBus.y);
				boolean isSlackBus = currentBus.busIri.equals(slackBus.busIri);
				boolean isWithin230KVrange = ((currentBus.baseKV >= baseKVmin) && (currentBus.baseKV <= baseKVmax));
				if ( (!isSlackBus) && isWithin230KVrange && (closestBus == null || (dist < distanceToClosestBus))) {
					distanceToClosestBus = dist;
					closestBus = currentBus;
				}
			} 
			
			if (closestBus == null) {
				throw new JPSRuntimeException("no optimal bus was found for generator = " + current.generatorIri);
			}
			
			current.closestBusNumber = closestBus.busNumber;
			current.distanceToClosestBus = distanceToClosestBus;
		}
		
		
		// find the closest bus among all generators from the same plant
		String busNumber = null;
		double minimalDistance = -1;
		for (GeneratorInfo currentGenerator : generators) {
			if ((busNumber == null) || (currentGenerator.distanceToClosestBus < minimalDistance)) {
				System.out.println("bus number choosen= "+busNumber);
				busNumber = currentGenerator.closestBusNumber;
				minimalDistance = currentGenerator.distanceToClosestBus;
			}
		}
		
		
		
		// finally, connect the generators to the buses in the OWL files
		for (GeneratorInfo currentGenerator : generators) {
			logger.info("connecting generator to bus, solar generator = " + currentGenerator.generatorIri + ", bus number = " + busNumber);
			connectGeneratorToBus(currentGenerator.generatorIri, currentGenerator.busNumberIri, busNumber);
		}
		
		
	}

	public void connectNuclearPowerGeneratorsOfPlantsToOptimalBus(List<BusInfo> buses, List<GeneratorInfo> generators, BusInfo slackBus) {
		
		logger.info("connecting generators to optimal buses, number of buses = " + buses.size() + ", number of generators = " + generators.size());
						
		// find the closest bus under the following constraints:
		// 1. The closest bus must not be the slack bus
		// 2. The closest bus must have baseKV around 230 kV (say plus minus 1%)
		double baseKVmin = 230 * 0.99;
		double baseKVmax = 230 * 1.01;
		for (GeneratorInfo current : generators) {
			
			//System.out.println("searching optimal bus for generator = " + current.generatorIri + ", x = " + current.x + ", y = " + current.y + ", current bus number instance = " + current.busNumberIri);
					
			// calculate the distance of the current generator to the closest bus
			double distanceToClosestBus = -1;
			BusInfo closestBus = null;
			for (BusInfo currentBus : buses) {
				double dist = distance(current.x, current.y, currentBus.x, currentBus.y);
				boolean isSlackBus = currentBus.busIri.equals(slackBus.busIri);
				boolean isWithin230KVrange = ((currentBus.baseKV >= baseKVmin) && (currentBus.baseKV <= baseKVmax));
				if ( (!isSlackBus) && isWithin230KVrange && (closestBus == null || (dist < distanceToClosestBus))) {
					distanceToClosestBus = dist;
					closestBus = currentBus;
				}
			} 
			
			if (closestBus == null) {
				throw new JPSRuntimeException("no optimal bus was found for generator = " + current.generatorIri);
			}
			
			current.closestBusNumber = closestBus.busNumber;
			current.distanceToClosestBus = distanceToClosestBus;
		}
				
		// collect all generators that belong to the same power plant
		Map<String, List<GeneratorInfo>> mapFromPlantToGenerators = new HashMap<String, List<GeneratorInfo>>();
		for (GeneratorInfo current : generators) {
			List<GeneratorInfo> list = mapFromPlantToGenerators.get(current.plantIri);
			if (list == null) {
				list = new ArrayList<GeneratorInfo>();
				mapFromPlantToGenerators.put(current.plantIri, list);
			}
			list.add(current);
		}
		
		// connect all generators which belong to the same power plant to the same bus
		for (String currentPlant : mapFromPlantToGenerators.keySet()) {
			
			List<GeneratorInfo> list = mapFromPlantToGenerators.get(currentPlant);
			
			// find the closest bus among all generators from the same plant
			String busNumber = null;
			double minimalDistance = -1;
			for (GeneratorInfo currentGenerator : list) {
				if ((busNumber == null) || (currentGenerator.distanceToClosestBus < minimalDistance)) {
					busNumber = currentGenerator.closestBusNumber;
					minimalDistance = currentGenerator.distanceToClosestBus;
				}
			}
			
			// finally, connect the generators to the buses in the OWL files
			for (GeneratorInfo currentGenerator : list) {
				logger.info("connecting generator to bus, generator = " + currentGenerator.generatorIri + ", bus number = " + busNumber);
				connectGeneratorToBus(currentGenerator.generatorIri, currentGenerator.busNumberIri, busNumber);
			}	
		}
	}
}
