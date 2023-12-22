package com.cmclinnovations.mods.modssimpleagent;

import java.io.IOException;
import java.util.Map;

import javax.xml.bind.JAXBException;

import com.cmclinnovations.mods.modssimpleagent.datamodels.Request;
import com.cmclinnovations.mods.modssimpleagent.simulations.Simulation;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ApplicationContext;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.context.event.EventListener;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.servlet.mvc.method.RequestMappingInfo;
import org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerMapping;

@SpringBootApplication
@RestController
public class ModsSimpleAgentApplication {

    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    Map<RequestMappingInfo, HandlerMethod> endpointMap;

    @EventListener
    public void handleContextRefresh(ContextRefreshedEvent event) {
        ApplicationContext applicationContext = event.getApplicationContext();
        RequestMappingHandlerMapping requestMappingHandlerMapping = applicationContext
                .getBean("requestMappingHandlerMapping", RequestMappingHandlerMapping.class);
        endpointMap = requestMappingHandlerMapping.getHandlerMethods();
    }

    @GetMapping(value = "/")
    public String home() {
        return endpointMap.keySet().toString();
    }

    @PostMapping(value = "/filerequest")
    public String requestbyfile(@RequestParam("file") MultipartFile file) throws IOException, JAXBException {
        // Do something with the file
        String config = new String(file.getBytes());

        return request(config);

    }

    @GetMapping(value = "/request")
    public String request(@RequestParam("query") String config)
            throws IOException, JAXBException {

        Request request = OBJECT_MAPPER.readValue(config, Request.class);

        Simulation sim = Simulation.createSimulation(request);

        sim.run();

        Request reponse = sim.getResponse();

        return OBJECT_MAPPER.writeValueAsString(reponse);
    }

    @GetMapping(value = "/output/request")
    public String getOutput(@RequestParam("query") String config) throws IOException, JAXBException {

        Request request = OBJECT_MAPPER.readValue(config, Request.class);

        Simulation sim = Simulation.retrieveSimulation(request);

        return OBJECT_MAPPER.writeValueAsString(sim.getResults());
    }

    public static void main(String[] args) {
        SpringApplication.run(ModsSimpleAgentApplication.class, args);
    }

}
