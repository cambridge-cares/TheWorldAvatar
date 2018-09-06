package uk.ac.cam.cares.jps.agents.discovery;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.agents.ontology.ServiceReader;
import uk.ac.cam.cares.jps.composition.servicemodel.Service;

// TODO-AE URGENT should be a singleton, remove extends after integration in composition
public class ServiceDiscovery extends uk.ac.cam.cares.jps.composition.webserver.ServiceDiscovery {
	
	Logger logger = LoggerFactory.getLogger(ServiceDiscovery.class);
	
	public ServiceDiscovery(String fileDirectory) throws Exception {
		super();
		loadServicePool(fileDirectory);
	}

	public void loadServicePool(String fileDirectory) throws Exception {
		
		logger.debug("loading service pool ...");
		
		ServiceReader reader = new ServiceReader();
		
		File file = new File(fileDirectory);
		for (File current : file.listFiles()) {
			if (current.isFile() && current.getName().endsWith(".owl")) {
				logger.debug("loading " + current);
				InputStream is = new FileInputStream(current);
				List<Service> services = reader.parse(is, null);
				service_pool.addAll(services);
			}
		}
	}
}
