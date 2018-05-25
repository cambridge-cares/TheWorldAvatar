package uk.ac.cam.cares.jps.men;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.gams.api.GAMSDatabase;
import com.gams.api.GAMSJob;
import com.gams.api.GAMSOptions;
import com.gams.api.GAMSParameter;
import com.gams.api.GAMSSet;
import com.gams.api.GAMSVariable;
import com.gams.api.GAMSVariableRecord;
import com.gams.api.GAMSWorkspace;
import com.gams.api.GAMSWorkspaceInfo;

import jdk.management.resource.internal.TotalResourceContext;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.men.entity.FeasibleConnection;
import uk.ac.cam.cares.jps.men.entity.MenCalculationParameters;
import uk.ac.cam.cares.jps.men.entity.Product;
import uk.ac.cam.cares.jps.men.entity.Sink;
import uk.ac.cam.cares.jps.men.entity.Source;
import uk.ac.cam.cares.jps.men.entity.Transportation;

public class MenGamsConverter {
	
	private Logger logger = LoggerFactory.getLogger(MenGamsConverter.class);	
	
	public MenResult calculate(List<Source> sources, List<Sink> sinks, List<FeasibleConnection> feasibleConnections, List<Transportation> transportations, MenCalculationParameters parameters) {
	
		// create a GAMS model
		// -------------------
		
        GAMSWorkspaceInfo  wsInfo  = new GAMSWorkspaceInfo();
         
    	//TODO-AE GAMS produces new files for each optimization. Old files must be deleted. Maybe move the workingDir to another place
    	// to use is also from ADMS and to have a general strategy for deleting old files. Move also location log files there
        String workingDirGams = AgentLocator.getPathToJpsWorkingDir() + "\\JPS_MEN\\gams";
        
        File workingDirectory = new File(workingDirGams);
        workingDirectory.mkdir();
        wsInfo.setWorkingDirectory(workingDirectory.getAbsolutePath());
        // create a workspace
        GAMSWorkspace ws = new GAMSWorkspace(wsInfo);		            
        // add a database and add input data into the database
        GAMSDatabase db = ws.addDatabase();	  								  
        
		// prepare GAMS input data
		// -----------------------
        
        prepareGamsParameterForSources(db, sources);
        prepareGamsParameterForSinks(db, sources, sinks, feasibleConnections, parameters.internationalMarketPriceFactor, parameters.internationalMarketLowestPrice);
        prepareGamsParameterForTransportation(db, transportations);
        prepareGamsParameterForConnections(db, sources, sinks, feasibleConnections, transportations);
		prepareGamsParameterForConstants(db, parameters);
		
		
		// start GAMS Code
		// ---------------
		
        String gamsCode = null;
		try {
			gamsCode = readGamsCode();
		} catch (IOException e) {
			// TODO-AE Auto-generated catch block
			e.printStackTrace();
		}
      
        GAMSJob job = ws.addJobFromString(gamsCode);
        GAMSOptions opt = ws.addOptions();
        opt.defines("gdxincname", db.getName());      

        job.run(opt, db);

        logger.info("parameter= "+parameters);
        
        return createResultFromGams(job, transportations, parameters.carbonTax);
	}
	
	private MenResult createResultFromGams(GAMSJob job, List<Transportation> transportations, double carbonTax) {
        String[] columnNames = new String[] {"TransportationCost", "InstallationCost", "C02Emission"};
		MenResult result = new MenResult(columnNames);
        
        result.objValue = getLevel(job, "ObjValue");
        result.totalMaterialPurchaseCost = getLevel(job, "TOTALPC");
        result.totalMaterialPurchaseCostInternationalMarket = getLevel(job, "TPCintMarket");
        result.totalTransportationCost = getLevel(job, "TOTALTC");
        result.totalCO2Emission = getLevel(job, "TOTALEM");
        result.totalC02EmissionCost = result.totalCO2Emission * carbonTax;
        result.totalInstallationCost = getLevel(job, "TOTALIC");

        
        String[] varNames = new String[] {"TTCTransp", "TICTransp", "TEMTransp"};
        for (Transportation transp : transportations) {
        	String transpName = transp.getName();
        	String transKey = getTransportationTypeKey(transpName);
        	List<Double> raw = new ArrayList<Double>();
        	for (String name : varNames) {
	        	GAMSVariable var = job.OutDB().getVariable(name);
	        	double value = getLevel(var, transKey);
	        	raw.add(value);
        	}    	
        	result.addRaw(transpName, raw);
        }

        logger.info("Result from GAMS calculation: "+result);
        logger.info("Result in matrix= "+result.convertMatrixToString());
        
        return result;
	}
	
	private double getLevel(GAMSVariable variable, String key) {
		for (GAMSVariableRecord rec : variable) {
			if (key.equals(rec.getKeys()[0])) {
				return rec.getLevel();
			}
		}		
		throw new RuntimeException("GAMS variable=" + variable.getName() + " has not record with key = " + key);
	}
	
	private double getLevel(GAMSJob job, String variableName) {
		return job.OutDB().getVariable(variableName).findRecord().getLevel();
	}
	
	/**
	 * This methods adds the GAMS set and one-dimensional GAMS parameters related to sources
	 * 
	 * @param db
	 * @param sources
	 */
	private void prepareGamsParameterForSources(GAMSDatabase db, List<Source> sources) {
        GAMSSet iSet = db.addSet("i", 1, "resource index");        
        GAMSParameter priceParam = db.addParameter("price", 1, "price of resource i");
        GAMSParameter sourceParam = db.addParameter("source", 1, "production mass of resource i");
 
		for (int i = 0; i < sources.size(); i++) {
			Source source = sources.get(i);
			String setEntry = getSourceKey(i, source);
			iSet.addRecord(setEntry); 
			
        	double price = source.getProduct().getPrice();
        	priceParam.addRecord(setEntry).setValue(price);
        	double capacity = source.getProduct().getCapacity();
        	sourceParam.addRecord(setEntry).setValue(capacity);
		}
	}

	/**
	 * This methods adds the GAMS set and one-dimensional GAMS parameters related to sinks
	 * 
	 * @param db
	 * @param sinks
	 */
	private void prepareGamsParameterForSinks(GAMSDatabase db, List<Source> sources, List<Sink> sinks, List<FeasibleConnection> connections, 
			double internationalMarketPriceFactor, boolean internationalMarketLowestPrice) {
		GAMSSet jSet = db.addSet("j", 1, "demand (sink) index");
        GAMSParameter demandParam = db.addParameter("demand", 1, "total mass of demand j");
        GAMSParameter priceIntMarketParam = db.addParameter("priceIntMarket", 1, "the price of international market for remaining demand j");
		for(int j = 0; j < sinks.size(); j++) {
			Sink sink = sinks.get(j);
			String setEntry = getSinkKey(j, sink);
			jSet.addRecord(setEntry);
			
        	double capacity = sink.getProduct().getCapacity();
        	demandParam.addRecord(setEntry).setValue(capacity);
        	double price = getPriceForInternationalMarket(sources, sink, connections, internationalMarketPriceFactor, internationalMarketLowestPrice);
        	priceIntMarketParam.addRecord(setEntry).setValue(price);
		}	
	}
	
	/**
	 * This methods adds the GAMS set and one-dimensional GAMS parameters related to transportation
	 * 
	 * @param db
	 * @param transportations
	 */
	private void prepareGamsParameterForTransportation(GAMSDatabase db, List<Transportation> transportations) {
        //define the the parameter for transportation cost, CO2 emission and installation cost
        GAMSSet ttSet = db.addSet("tt", 1, "transportation type");
        GAMSParameter tcParam = db.addParameter("TCUnit", 1, "transportation cost per t and km");
        // note that the C02 emission is given in gramm
        GAMSParameter emParam = db.addParameter("EMUnit", 1, "CO2 emission in gramm per t and km");
        GAMSParameter icParam = db.addParameter("ICUnit", 1, "installation cost per km");
        
        for (int t = 0; t < transportations.size(); t++) {
        	Transportation transportation = transportations.get(t);
        	String name = transportation.getName();
        	String setEntry = getTransportationTypeKey(name);
        	ttSet.addRecord(setEntry);
        	
        	double transportationCost = transportation.getTransportationCost();
        	tcParam.addRecord(setEntry).setValue(transportationCost);    
        	double emission = transportation.getEmission();
        	emParam.addRecord(setEntry).setValue(emission); 
        	double installationCost = transportation.getInstallationCost();        
            icParam.addRecord(setEntry).setValue(installationCost); 
        }
	}
	
	private void prepareGamsParameterForConstants(GAMSDatabase db, MenCalculationParameters parameters) {
		GAMSSet cSet = db.addSet("ckey", 1, "constant names");
		GAMSParameter param = db.addParameter("constant", 1 , "constant values");
		
		String setEntry = "CARBONTAX";
		cSet.addRecord(setEntry);
		param.addRecord(setEntry).setValue(parameters.carbonTax);  
		
		setEntry = "INTERESTFACTOR";
		cSet.addRecord(setEntry);
		param.addRecord(setEntry).setValue(parameters.interestFactor); 
		
		setEntry = "ANNUALCOSTFACTOR";
		cSet.addRecord(setEntry);
		param.addRecord(setEntry).setValue(parameters.annualCostFactor); 	
	}
	
	/**
	 * This methods adds the GAMS parameters related to connections
	 * 
	 * @param db
	 * @param sources
	 * @param sinks
	 * @param feasibleConnections
	 * @param transportations
	 */
	private void prepareGamsParameterForConnections(GAMSDatabase db, List<Source> sources, List<Sink> sinks, List<FeasibleConnection> feasibleConnections, List<Transportation> transportations) {
        // the GAMS parameter 'connection' is defined as three-dimensional matrix with 
		// 1. source as its first dimension
		// 2. sink as its second dimension and
		// 3. transportation type as its third dimension
        // each matrix value is binary: whenever there is a feasible connection between source and sink using a certain transportation type, 
		// the corresponding matrix entry is set to 1 else to 0
        GAMSParameter connectionParam = db.addParameter("connection", 3, "feasible connections between source i and demand j through transportation type tt");
        // the GAMS parameter 'distance' is defined as two-dimensional matrix with source and sink
        // whenever there is a feasible connection, the corresponding matrix entry is set to the connection distance else no distance is set
        GAMSParameter distanceParam = db.addParameter("distance", 2, "distance between source i and demand j");
        
        for (int j = 0; j < sinks.size(); j++) {
        	for (int i = 0; i < sources.size(); i++) {
        		Source source = sources.get(i);
        		Sink sink = sinks.get(j);
         		
            	FeasibleConnection foundConnection = FeasibleConnection.findConnectionByNames(feasibleConnections, source, sink);    
            	if (foundConnection != null) {
            		String[] indices = new String[]{getSourceKey(i,source), getSinkKey(j, sink)};
            		distanceParam.addRecord(indices).setValue(foundConnection.getDistance());
            	}
            	
            	for (Transportation currentTransp : transportations) {
            		String[] indices = new String[]{getSourceKey(i,source), getSinkKey(j, sink),getTransportationTypeKey(currentTransp.getName())};
				
	        		// TODO-AE move this rule and all transportation types to class data provider 
            		// TODO-AE the second rule means that a sink near sea is on an different island such that truck and landpipelines cant be used
//	        		boolean transportationPossible = currentTransp.getName().equals("Trucks") 
//	        				|| currentTransp.getName().equals("LandPipelines")
//	        				|| (source.isNearSea() && sink.isNearSea());
	        		boolean transportationPossible = false;
	        		if (currentTransp.getName().equals("Trucks") || currentTransp.getName().equals("LandPipelines")) {
	        			transportationPossible = !sink.isNearSea();
	        		} else {
	        			transportationPossible = (sink.isNearSea() && source.isNearSea());
	        		}

	        		
	        		if (foundConnection != null && transportationPossible) {
	        			connectionParam.addRecord(indices).setValue(1);
	        		} else {
	        			connectionParam.addRecord(indices).setValue(0);
	        		}    
            	}
        	}
        }
	}
	
	private String getTransportationTypeKey(String transportationName) {
		return "tt_" + transportationName ;
	}
	
	private String getSourceKey(int i, Source source) {
   		String sourceName = source.getName();
		String sourceProductName = source.getProduct().getName();
		return "i_" + String.valueOf(i) + "_" + sourceName + "_" + sourceProductName;
	}
	
	private String getSinkKey(int j, Sink sink) {
    	String sinkName = sink.getName();
    	String sinkProductName = sink.getProduct().getName();
    	return "j_" + String.valueOf(j) + "_" + sinkName + "_" + sinkProductName;
	}
	
	private String readGamsCode() throws IOException {
		String baseDir = AgentLocator.getCurrentJpsAppDirectory(this);
		return Helper.readFile(baseDir + "\\conf\\MENGAMSCode.txt", StandardCharsets.UTF_8);
	}
	
	public static double getPriceForInternationalMarket(List<Source> sources, Sink sink, List<FeasibleConnection> connections, double priceFactor, boolean lowestPrice) {
		
		// TODO-AE move this method to data provider
		
		double result = 0;
		
		List<FeasibleConnection> orderedConnections = orderByPriceAscending(sources, sink, connections);
		
		int size = orderedConnections.size();
		if (size > 0) {
			int index = (lowestPrice)? 0 : size-1;
			double basePrice = orderedConnections.get(index).getSource().getProduct().getPrice();
			result = priceFactor * basePrice;
		} else {
			//TODO-AE what should happen if there is no sink with a price?
			throw new RuntimeException("No price is found for international market");
		}
		
		return result;
	}
	
	public static List<FeasibleConnection> orderByPriceAscending(List<Source> sources, Sink sink, List<FeasibleConnection> connections) {
		
		List<FeasibleConnection> result = new ArrayList<FeasibleConnection>();
		
		for (FeasibleConnection conn : connections) {
			if ((conn.getSink() == sink)) {
				// add connection to the result array at the right position
				Product product = conn.getSink().getProduct();
				int pos = result.size();
				for (int i=0; i<result.size(); i++) {
					Product productFromArray = result.get(i).getSource().getProduct();
					if (productFromArray.getPrice() > product.getPrice()) {
						pos = i; 
						break;
					} 
				}
				result.add(pos, conn);
			}
		}
		
		return result;
	}
}
