package uk.ac.cam.cares.jps.discovery.search;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.http.ParseException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.discovery.api.AbstractAgentServiceDescription;
import uk.ac.cam.cares.jps.discovery.api.Agent;
import uk.ac.cam.cares.jps.discovery.api.AgentRequest;
import uk.ac.cam.cares.jps.discovery.api.AgentResponse;
import uk.ac.cam.cares.jps.discovery.api.TypeIRI;
import uk.ac.cam.cares.jps.discovery.factory.DiscoveryFactory;
import uk.ac.cam.cares.jps.discovery.matching.exact.ExactMatcher;
import uk.ac.cam.cares.jps.discovery.util.Helper;
import uk.ac.cam.cares.jps.discovery.util.ISerializer;
import uk.ac.cam.cares.jps.discovery.util.JPSBaseServlet;

@WebServlet(urlPatterns = {"/search", "/call"})
public class SearchAgent extends JPSBaseServlet {
	
	private static final long serialVersionUID = 5462239838527386746L;
	
	Logger logger = LoggerFactory.getLogger(SearchAgent.class);
	private ISerializer serializer = DiscoveryFactory.getSerializer();
	
	@Override
	public void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException, ServletException {

		String path = req.getServletPath();
		logger.info("SearchAgent is called, path = " + path);

		if ("/search".equals(path)) {
			String serializedSearchDescr = req.getParameter("agentrequest");
			AgentRequest agentRequest = serializer.<AgentRequest>convertFrom(serializedSearchDescr).get();
			List<TypeIRI> list = search(agentRequest);
			print(resp, list);
		} else if ("/call".equals(path)) {
			String serializedAgentRequest = req.getParameter("agentrequest");
			AgentResponse agentResponse = call(serializedAgentRequest);
			String serializedAgentResponse = serializer.convertToString(agentResponse);
			print(resp, serializedAgentResponse);
		}
	}
	
	private List<TypeIRI> search(AgentRequest agentRequest) {
		
		List<TypeIRI> result = new ArrayList<TypeIRI>();
		
		ExactMatcher matcher = new ExactMatcher(DiscoveryFactory.getRegistry());
		List<Agent> list = matcher.getMatches(agentRequest);
		for (Agent current : list) {
			result.add(current.getName());
		}
		
		return result;
	}
	
	private AgentResponse call(String serializedAgentRequest) throws ParseException, IOException {
		
		AgentResponse result = null;

		AgentRequest agentRequest = serializer.<AgentRequest>convertFrom(serializedAgentRequest).get();
		List<TypeIRI> list = search(agentRequest);
		if (list.size() > 0) {
			// TODO-AE path vs. local host, this must be clearified. 
			// The agent could also run on 3rd party server or any server different from 
			// where the discovery is located!
			// TODO-AE why the first agent --> evaluate performance of agents
			String address = list.get(0).getValue();
			// TODO-AE this is a complete hack to get the path
			int index = address.indexOf("8080");
			String path = address.substring(index+4);
			logger.info("MYPATH=" + path);
			try {
				String serializedAgentResponse = Helper.executeGet(path, "agentrequest", serializedAgentRequest);
				result = serializer.<AgentResponse>convertFrom(serializedAgentResponse).get();
			} catch (URISyntaxException e) {
				// TODO-AE Auto-generated catch block
				e.printStackTrace();
			}			
		} else {
			result = new AgentResponse();
			// copy original parameters from the search request
			AbstractAgentServiceDescription.copyParameters(agentRequest, result);
		}
		
		return result;
	}
}