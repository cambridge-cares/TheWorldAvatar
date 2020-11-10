package uk.ac.cam.cares.jps.agent.configuration;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.support.PropertySourcesPlaceholderConfigurer;
/**
 * gPROMS Agent developed for setting-up and running gPROMS chemical network on HPC.
 * The input files for gPROMS execution should be placed in user.home//input folder
 * @author Aravind Devanand (aravind@u.nus.edu)
 *
 */
@Configuration
@ComponentScan
public class gPROMSAgentConfiguration {

  @Bean
  public static PropertySourcesPlaceholderConfigurer propertyConfigInDev() {
    return new PropertySourcesPlaceholderConfigurer();
  }
}
