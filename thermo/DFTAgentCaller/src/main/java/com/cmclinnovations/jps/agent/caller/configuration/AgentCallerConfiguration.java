package com.cmclinnovations.jps.agent.caller.configuration;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.support.PropertySourcesPlaceholderConfigurer;

@Configuration
@ComponentScan
public class AgentCallerConfiguration {

  @Bean
  public static PropertySourcesPlaceholderConfigurer propertyConfigInDev() {
    return new PropertySourcesPlaceholderConfigurer();
  }
}