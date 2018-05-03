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

import uk.ac.cam.cares.jps.discovery.api.AgentDescription;
import uk.ac.cam.cares.jps.discovery.api.TypeIRI;
import uk.ac.cam.cares.jps.discovery.util.ISerializer;
import uk.ac.cam.cares.jps.discovery.util.JPSBaseServlet;
import uk.ac.cam.cares.jps.discovery.util.SerializerFactory;

@WebServlet(urlPatterns = {"/register", "/deregister", "/agents"})
public class RegistryAgent extends JPSBaseServlet {

	private static final long serialVersionUID = -1084832972879292460L;

	Logger logger = LoggerFactory.getLogger(RegistryAgent.class);
	private ISerializer serializer = SerializerFactory.createSerializer();
	
	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		
		String path = req.getServletPath();
		logger.info("RegistryAgent is called, path = " + path);

		if ("/register".equals(path)) {
			String serializedDescr = req.getParameter("agentdescription");
			AgentDescription description = serializer.<AgentDescription>convertFrom(serializedDescr).get();
			SimpleAgentRegistry.getInstance().register(description);
		} else if ("/deregister".equals(path)) {
			String agentAddress = req.getParameter("agentaddress");
			SimpleAgentRegistry.getInstance().deregister(agentAddress);
		} else if ("/agents".equals(path)) {
			print(resp, getAddressesOfAllAgents());
		}
	}	
	
	private List<TypeIRI> getAddressesOfAllAgents() throws IOException {
		
		List<TypeIRI> result = new ArrayList<TypeIRI>();
		
		Collection<AgentDescription> list = SimpleAgentRegistry.getInstance().getAllAgentDescriptions();
		for (AgentDescription current : list) {
			result.add(current.getAddress());
		}
		
		return result;
	}
}
