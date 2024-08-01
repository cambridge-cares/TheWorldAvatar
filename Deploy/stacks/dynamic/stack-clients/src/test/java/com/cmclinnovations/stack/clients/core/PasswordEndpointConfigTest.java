package com.cmclinnovations.stack.clients.core;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class PasswordEndpointConfigTest {

    @Test
    void getUserDefinedPassword() throws IOException, URISyntaxException {
        Path path = Path.of(PasswordEndpointConfigTest.class.getResource("password").toURI());
        PasswordEndpointConfig config = new PasswordEndpointConfig("test", path.toString());

        Assertions.assertEquals(path.toString(), config.getPasswordFile());
        Assertions.assertEquals(Files.readString(path), config.getPassword());
    }

    @Test
    void getNoPassword() throws IOException {
        PasswordEndpointConfig config = new PasswordEndpointConfig("test", null);

        Assertions.assertNull(config.getPasswordFile());
        Assertions.assertEquals("", config.getPassword());
    }

    @Test
    void getEmptyPasswordFile() throws URISyntaxException, IOException {
        Path path = Path.of(PasswordEndpointConfigTest.class.getResource("empty-file").toURI());
        PasswordEndpointConfig config = new PasswordEndpointConfig("test", path.toString());

        Assertions.assertEquals(path.toString(), config.getPasswordFile());
        Assertions.assertThrows(IllegalArgumentException.class, config::getPassword);
    }

    @Test
    void getNonExistantPasswordFile() {
        PasswordEndpointConfig config = new PasswordEndpointConfig("test", "non-existant-file");

        Assertions.assertEquals("non-existant-file", config.getPasswordFile());
        Assertions.assertThrows(IllegalArgumentException.class, config::getPassword);
    }
}
