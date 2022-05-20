package com.cmclinnovations;

import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;

import com.cmclinnovations.services.ServiceManager;

/**
 * Hello world!
 *
 */
public class App {
    public static void main(String[] args) throws URISyntaxException, IOException {
        String stackName = "stack1";
        URL hostURL = new URL("http://localhost");

        ServiceManager manager = new ServiceManager();
        Stack stack = new Stack(stackName, hostURL, manager);
        stack.hashCode();
    }
}
