package uk.ac.cam.cares.jps.agent.configuration;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.support.PropertySourcesPlaceholderConfigurer;

@Configuration
@ComponentScan
public class MoDSDataDrivenAgentConfiguration {
	
	@Bean
	public static PropertySourcesPlaceholderConfigurer propertyConfigInDev() {
	    return new PropertySourcesPlaceholderConfigurer();
	  }
}
