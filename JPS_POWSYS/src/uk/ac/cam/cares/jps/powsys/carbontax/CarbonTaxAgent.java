package uk.ac.cam.cares.jps.powsys.carbontax;

import java.io.IOException;
import java.math.BigDecimal;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;

@WebServlet(urlPatterns = { "/optimizeforcarbontax" })
public class CarbonTaxAgent extends JPSHttpServlet {

	private static final long serialVersionUID = -2354646810093235777L;
	private Logger logger = LoggerFactory.getLogger(CarbonTaxAgent.class);

	@Override
	protected void doGetJPS(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
	
		JSONObject jo = AgentCaller.readJsonParameter(request);
		BigDecimal carbontax = jo.getBigDecimal("carbontax");
		logger.info("start optimization for carbon tax = " + carbontax);
		
		JSONObject result = new JSONObject();
		JSONArray ja = new JSONArray();
		ja.put("http://www.theworldavatar.com/kb/powerplants/Keppel_Merlimau_Cogen_Power_Plant_Singapore.owl#Keppel_Merlimau_Cogen_Power_Plant_Singapore");
		ja.put("http://www.theworldavatar.com/kb/powerplants/SembCorp_Pulau_Sakra_CCGT_Cogen_Power_Station_Singapore.owl#SembCorp_Pulau_Sakra_CCGT_Cogen_Power_Station_Singapore");
		ja.put("http://www.theworldavatar.com/kb/powerplants/Jurong_Island_-_PLP_CCGT_Power_Plant_Singapore.owl#Jurong_Island_-_PLP_CCGT_Power_Plant_Singapore");
		ja.put("http://www.theworldavatar.com/kb/powerplants/PowerSeraya_OCGT_Power_Plant_Singapore.owl#PowerSeraya_OCGT_Power_Plant_Singapore");
		ja.put("http://www.theworldavatar.com/kb/powerplants/PowerSeraya_Pulau_Seraya_Oil_Power_Station_Singapore.owl#PowerSeraya_Pulau_Seraya_Oil_Power_Station_Singapore");
		ja.put("http://www.theworldavatar.com/kb/powerplants/PowerSeraya_Pulau_Seraya_CCGT_Cogen_Power_Plant_Singapore.owl#PowerSeraya_Pulau_Seraya_CCGT_Cogen_Power_Plant_Singapore");
		result.put("substitutionalpowerplants", ja);
		
		logger.info("optimization result = " + result);
		
		AgentCaller.printToResponse(result, response);
	}
}
