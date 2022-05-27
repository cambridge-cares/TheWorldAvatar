package com.cmclinnovations;

import java.net.MalformedURLException;
import java.net.URL;

import com.cmclinnovations.services.ServiceManager;

/**
 * Hello world!
 *
 */
public class App {
    public static void main(String[] args) throws MalformedURLException {
        String stackName = "stack1";
        URL hostURL = new URL("http://localhost");

        ServiceManager manager = new ServiceManager();
        Stack stack = new Stack(stackName, hostURL, manager);
        stack.hashCode();
    }
}
