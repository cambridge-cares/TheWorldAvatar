package uk.ac.cam.cares.jps.accessagent.fedq_accessagent;

import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;

@SpringBootApplication
public class IndexApp {

    public static void main(String[] args) {
        SpringApplication.run(IndexApp.class, args);
    }

    @Bean
    public CommandLineRunner run(ApplicationContext context) {
        return args -> {
            IndexAgentCaller controller = context.getBean(IndexAgentCaller.class);
            System.out.print(controller.getValuesCL("mykey").toString());
        };
    }
}