package uk.ac.cam.cares.jps.discovery.registry;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.discovery.api.Agent;
import uk.ac.cam.cares.jps.discovery.api.TypeIRI;
import uk.ac.cam.cares.jps.discovery.factory.DiscoveryFactory;
import uk.ac.cam.cares.jps.discovery.util.JPSBaseServlet;
import uk.ac.cam.cares.jps.discovery.util.JavaSerializer;

@WebServlet(urlPatterns = {"/register", "/deregister", "/agents"})
public class RegistryAgent extends JPSBaseServlet {

	private static final long serialVersionUID = -1084832972879292460L;

	Logger logger = LoggerFactory.getLogger(RegistryAgent.class);
	
	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		
		String path = req.getServletPath();
		logger.info("RegistryAgent is called, path = " + path);

		if ("/register".equals(path)) {
			String serializedDescr = req.getParameter("agentdescription");
			// TODO-AE here we still use the Java binary serializer instead of OWL
			// reason: Tests use register method and do not run as there is no deserialization
			// from OWL to Java class AgentDescription yet!
			//String serialized = serializer.convertToString(description);	
			//AgentDescription description = serializer.<AgentDescription>convertFrom(serializedDescr).get();
			Agent description = new JavaSerializer().<Agent>convertFrom(serializedDescr).get();
			DiscoveryFactory.getRegistry().register(description);
		} else if ("/deregister".equals(path)) {
			String agentAddress = req.getParameter("agentaddress");
			DiscoveryFactory.getRegistry().deregister(agentAddress);
		} else if ("/agents".equals(path)) {
			print(resp, getNamesOfAllAgents());
		}
	}	
	
	private List<TypeIRI> getNamesOfAllAgents() throws IOException {
		
		List<TypeIRI> result = new ArrayList<TypeIRI>();
		
		Collection<Agent> list = DiscoveryFactory.getRegistry().getAllAgents();
		for (Agent current : list) {
			result.add(current.getName());
		}
		
		return result;
	}
}
