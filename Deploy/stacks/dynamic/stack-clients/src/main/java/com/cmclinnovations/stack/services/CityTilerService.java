package com.cmclinnovations.stack.services;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.stack.clients.citydb.CityTilerClient;
import com.cmclinnovations.stack.clients.docker.DockerClient;
import com.cmclinnovations.stack.services.config.ServiceConfig;

public final class CityTilerService extends ContainerService {

    private static final Logger logger = LoggerFactory.getLogger(CityTilerService.class);
    public static final String TYPE = "citytiler";

    public CityTilerService(String stackName, ServiceConfig config) {
        super(stackName, config);
    }

    @Override
    public void doPostStartUpConfiguration() {

        DockerClient dockerClient = DockerClient.getInstance();

        dockerClient.executeSimpleCommand(dockerClient.getContainerId(TYPE), "cp",
                CityTilerClient.COLOUR_CONFIG_FILE, CityTilerClient.DEFAULT_COLOUR_CONFIG_FILE);

        try (InputStream is = CityTilerService.class
                .getResourceAsStream("citytiler/0001-Added-generic_attributes.patch")) {

            ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
            ByteArrayOutputStream errorStream = new ByteArrayOutputStream();

            String patch = new String(is.readAllBytes());

            String execId = dockerClient.createComplexCommand(dockerClient.getContainerId(TYPE), "git", "apply", "-")
                    .withHereDocument(patch)
                    .withOutputStream(outputStream)
                    .withErrorStream(errorStream)
                    .exec();

            handleErrors(errorStream, execId, logger);

        } catch (IOException ex) {
            throw new RuntimeException("Failed to read in citytiler git patch file.", ex);
        }

    }

    private void handleErrors(ByteArrayOutputStream errorStream, String execId, Logger logger) {

        DockerClient dockerClient = DockerClient.getInstance();
        long commandErrorCode = dockerClient.getCommandErrorCode(execId);
        if (0 != commandErrorCode) {
            throw new RuntimeException("Docker exec command returned '" + commandErrorCode
                    + "' and wrote the following to stderr:\n" + errorStream.toString());
        } else {
            logger.warn("Docker exec command returned '0' but wrote the following to stderr:\n{}", errorStream);
        }
    }
}
