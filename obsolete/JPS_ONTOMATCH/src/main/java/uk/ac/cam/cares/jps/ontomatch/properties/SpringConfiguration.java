package uk.ac.cam.cares.jps.ontomatch.properties;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.support.PropertySourcesPlaceholderConfigurer;

@Configuration
@ComponentScan
/***
 * Helper that sets the spring configuration for property file
 * 
 * @author shaocong zhang
 * @version 1.0
 * @since 2020-09-08
 */
public class SpringConfiguration {

	@Bean
	public static PropertySourcesPlaceholderConfigurer propertyConfigInDev() {
		return new PropertySourcesPlaceholderConfigurer();
	}
}