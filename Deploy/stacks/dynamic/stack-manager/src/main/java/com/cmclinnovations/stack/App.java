package com.cmclinnovations.stack;

import java.net.MalformedURLException;
import java.net.URL;

import com.cmclinnovations.stack.clients.core.StackClient;
import com.cmclinnovations.stack.services.ServiceManager;

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

        Stack stack = Stack.create(stackName, hostURL, manager, StackClient.STACK_CONFIG_DIR);

        stack.initialiseServices();
    }
}
