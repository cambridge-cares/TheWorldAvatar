package uk.ac.cam.cares.jps.powsys.retrofit;

import java.util.ArrayList;
import java.util.List;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;

import org.apache.jena.ontology.OntModel;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.log.JPSBaseLogger;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.query.sparql.QueryBuilder;
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.base.util.MiscUtil;
import uk.ac.cam.cares.jps.powsys.electricalnetwork.ENAgent;
import uk.ac.cam.cares.jps.powsys.util.Util;

@WebServlet("/RenewableGenRetrofit")
public class RenewableGeneratorRetrofit extends JPSAgent{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private GeneralRetrofitAgent gRA;
	
	public RenewableGeneratorRetrofit(){
		gRA = new GeneralRetrofitAgent();
	}
    @Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(RenewableGeneratorRetrofit.class);
    }
    Logger logger = LoggerFactory.getLogger(RenewableGeneratorRetrofit.class);
    @Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		requestParams = processRequestParameters(requestParams, null);
		return requestParams;
	}
	@Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {

		if (!validateInput(requestParams)) {
			throw new JSONException("RenewableGeneratorRetrofitAgent:  Input Parameters invalid.");
		}
		JPSBaseLogger.info(this,"Reached ProcessRequestParameters");
		String electricalNetwork = requestParams.getString("electricalnetwork");
		JSONArray ja = requestParams.getJSONArray("RenewableEnergyGenerator");
		List<String> RenewableGenerators = MiscUtil.toList(ja);
		retrofitGenerator(electricalNetwork, RenewableGenerators);
		
		return requestParams;
		
	}
	@Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (requestParams.isEmpty()) {
            throw new BadRequestException();
        }
        try {
	        String ENIRI = requestParams.getString("electricalnetwork");
	        boolean w = InputValidator.checkIfValidIRI(ENIRI);	        
	        JSONArray ja =requestParams.getJSONArray("RenewableEnergyGenerator");
			List<String> RenewableGenerators = MiscUtil.toList(ja);
			if (ja.length()!= 0) {
				for (int i = 0; i< RenewableGenerators.size(); i++) {
					if (RenewableGenerators.get(i)!= null) {
						boolean t = InputValidator.checkIfValidIRI(RenewableGenerators.get(i));
						if (t == false) {
							return false;
						}
					}
				}
			}else {
				return false;
			}
	        return w;
        } catch (JSONException e) {
        	return false;
        }
    }
	/** Queries for list of buses
	 * Find the slack Bus (aka the one where the PV can be connected to
	 * Adds the PV generator to the electrical network
	 * @param electricalNetwork
	 * @param RenewableGenerators
	 */
	public void retrofitGenerator(String electricalNetwork, List<String> RenewableGenerators) {
		OntModel model = Util.readModelGreedy(electricalNetwork);
		List<BusInfo> buses = gRA.queryBuses(model);
		BusInfo slackBus = gRA.findFirstSlackBus(buses);
		//assuming the pv owl file is exist and matched the criteria to be used in OPF simulation
		List<GeneratorInfo> newGenerators = new ArrayList<GeneratorInfo>();
		QueryBroker broker = new QueryBroker();
		for (String currentGen : RenewableGenerators) {
			String generatorIri = currentGen;
			GeneratorInfo info = new GeneratorInfo();
			info.generatorIri = generatorIri;
			String queryGenerator = gRA.getQueryForGenerator();
			System.out.println("myquery= "+queryGenerator);
			String resultGen = broker.queryFile(generatorIri, queryGenerator);
			List<String[]> resultGenAsList = JenaResultSetFormatter.convertToListofStringArrays(resultGen, "entity", "x", "y", "busnumber");
			System.out.println("result size= "+resultGenAsList.size());
			info.x = Double.valueOf(resultGenAsList.get(0)[1]);
			info.y = Double.valueOf(resultGenAsList.get(0)[2]);
			info.busNumberIri = resultGenAsList.get(0)[3];
			System.out.println("bus number iri= "+resultGenAsList.get(0)[3]  );
			
			newGenerators.add(info);
		}
		
		gRA.addGeneratorsToElectricalNetwork(electricalNetwork,newGenerators);
		
		gRA.connectGeneratorToOptimalBus(buses, newGenerators, slackBus);

		
	}
	
}
