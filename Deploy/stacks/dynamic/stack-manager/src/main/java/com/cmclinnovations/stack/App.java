package com.cmclinnovations.stack;

import java.net.MalformedURLException;
import java.net.URL;

import com.cmclinnovations.stack.services.ServiceManager;
import com.cmclinnovations.stack.clients.core.StackClient;

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
