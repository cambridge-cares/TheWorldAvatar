package uk.ac.cam.cares.jps.episode;

import javax.servlet.annotation.WebServlet;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;


@WebServlet(urlPatterns= "/DispersionModellingAgent")

public class DispersionModellingAgent extends JPSHttpServlet {
	
    /**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@Override
    protected void setLogger() {
        logger = LoggerFactory.getLogger(DispersionModellingAgent.class);
    }
    
    /**
     *  create logger to log changes attached to WasteToEnergyAgent class. 
     */
    Logger logger = LoggerFactory.getLogger(DispersionModellingAgent.class);
	
    @Override
	protected JSONObject processRequestParameters(JSONObject requestParams) {
		String baseUrl= requestParams.getString("baseUrl");
		String wasteIRI=requestParams.getString("wastenetwork");


		return requestParams;
	}
	
	public void createWeatherInput() {
		
	}
	
	public void createEmisisonInput() {
		
	}
	
	public void executeModel() {
		
	}

}
