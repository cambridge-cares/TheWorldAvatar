package uk.ac.cam.cares.jps.virtualsensor.configuration;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.support.PropertySourcesPlaceholderConfigurer;

@Configuration
@ComponentScan
public class EpisodeAgentConfiguration {

  @Bean
  public static PropertySourcesPlaceholderConfigurer propertyConfigInDev() {
    return new PropertySourcesPlaceholderConfigurer();
  }
}