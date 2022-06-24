package com.cmclinnovations;

import java.net.MalformedURLException;
import java.net.URL;

import com.cmclinnovations.clients.core.StackClient;
import com.cmclinnovations.services.ServiceManager;

/**
 * Hello world!
 *
 */
public class App {
    public static void main(String[] args) throws MalformedURLException {

        String stackName = StackClient.getStackName();
        URL hostURL = new URL("http://localhost");

        // This is not ideal but required for when trying to read dynamically generated
        // configs
        StackClient.setInStack(false);

        ServiceManager manager = new ServiceManager();
        Stack stack = new Stack(stackName, hostURL, manager);
        stack.hashCode();
    }
}
