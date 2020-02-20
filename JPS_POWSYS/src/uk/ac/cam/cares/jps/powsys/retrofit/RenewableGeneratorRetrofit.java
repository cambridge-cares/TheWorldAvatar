package uk.ac.cam.cares.jps.powsys.retrofit;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.FileUtils;
import org.apache.jena.ontology.OntModel;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.MiscUtil;
import uk.ac.cam.cares.jps.powsys.electricalnetwork.ENAgent;

@WebServlet("/RenewableGenRetrofit")
public class RenewableGeneratorRetrofit extends GeneralRetrofitAgent {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	@Override
	protected void doGetJPS(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		
		JSONObject jo = AgentCaller.readJsonParameter(request);
		String electricalNetwork = jo.getString("electricalnetwork");
		String scenario=jo.getJSONObject("jpscontext").getString("scenariourl").split("/scenario/")[1];	
		System.out.println("current scenario directory= "+scenario);
		
		File f = new File("C://JPS_DATA/workingdir/JPS_SCENARIO/scenario/"+scenario);
		if (f.exists()&& f.isDirectory()) {
			FileUtils.deleteDirectory(f);
		}
		//above is done as the retrofit cannot be done twice for ESS esp.
		JSONArray ja = jo.getJSONArray("RenewableEnergyGenerator");
		List<String> RenewableGenerators = MiscUtil.toList(ja);
		retrofitGenerator(electricalNetwork, RenewableGenerators);
		
		AgentCaller.printToResponse(jo, response);
	}

	public void retrofitGenerator(String electricalNetwork, List<String> RenewableGenerators) {
		
		OntModel model = ENAgent.readModelGreedy(electricalNetwork);
		List<BusInfo> buses = queryBuses(model);
		BusInfo slackBus = findFirstSlackBus(buses);
		
		//assuming the pv owl file is exist and matched the criteria to be used in OPF simulation
		List<GeneratorInfo> newGenerators = new ArrayList<GeneratorInfo>();
		QueryBroker broker = new QueryBroker();
		for (String currentGen : RenewableGenerators) {
			String generatorIri = currentGen;
			GeneratorInfo info = new GeneratorInfo();
			info.generatorIri = generatorIri;
			String queryGenerator = getQueryForGenerator();
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
		
		addGeneratorsToElectricalNetwork(electricalNetwork,newGenerators);
		
		connectGeneratorToOptimalBus(buses, newGenerators, slackBus);

		logger.info("finished retrofitting");
		
	}

}
