package com.cmclinnovations.stack;

import com.cmclinnovations.stack.clients.core.StackClient;
import com.cmclinnovations.stack.services.ServiceManager;

/**
 * Hello world!
 *
 */
public class App {
    public static void main(String[] args) {

        String stackName = StackClient.getStackName();

        ServiceManager manager = new ServiceManager();

        Stack stack = Stack.create(stackName, manager, StackClient.STACK_CONFIG_DIR);

        stack.initialiseServices();
    }
}
