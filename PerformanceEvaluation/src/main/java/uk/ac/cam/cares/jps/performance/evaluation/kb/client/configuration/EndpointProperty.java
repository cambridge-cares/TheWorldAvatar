package uk.ac.cam.cares.jps.performance.evaluation.kb.client.configuration;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

@Configuration
@PropertySource("classpath:endpoint.properties")
public class EndpointProperty {
	/**
	 * With the annotation ${endpoint.urls}, this variable is linked to the<p>
	 * property called endpoint.urls in the property file src/main/resources/<p>
	 * endpoint.properties.
	 */
	@Value("${endpoint.urls}")
	private String endpointURLs;

	/**
	 * Returns the end point URLs set in the property file src/main/resources/<p>
	 * endpoint.properties.
	 * 
	 * @return
	 */
	public String getEndpointURLs() {
		return endpointURLs;
	}
}

