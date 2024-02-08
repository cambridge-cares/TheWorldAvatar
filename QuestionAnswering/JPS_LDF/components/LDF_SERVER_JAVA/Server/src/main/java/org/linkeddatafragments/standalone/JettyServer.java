package org.linkeddatafragments.standalone;

import java.net.URL;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Options;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.servlet.DefaultServlet;
import org.eclipse.jetty.servlet.ServletContextHandler;
import org.eclipse.jetty.servlet.ServletHolder;
import org.linkeddatafragments.servlet.LinkedDataFragmentServlet;

/**
 * <p>
 * Use this class to run as a standalone service. Since it runs the
 * BasicLdfServlet, it is important to have a configuration file in the web.xml
 * as "configFile" init parameter. Default [baseDir]/config.xml</p>
 * <p>
 * This class runs an embedded Jetty servlet container. This way there is no
 * need for a separate servlet container such as Tomcat.</p>
 *
 * <p>
 * Copyright 2014 MMLab, UGent </p>
 *
 * @author Gerald Haesendonck
 * @author Miel Vander Sande
 * @author Bart Hanssens
 */
public class JettyServer {
    private static void printHelp(Options options) {
        HelpFormatter formatter = new HelpFormatter();
        formatter.printHelp("java -jar target/ldf-server.jar [config-example.json] [<options>]",
                    "Starts a standalone LDF Triple Pattern server. Options:", options, "");
    }
    
    /**
     *
     * @param args
     * @throws Exception
     */
    public static void main(String[] args) throws Exception {
        Options options = new Options();
        options.addOption("h", "help", false, "Print this help message and then exit.");
        options.addOption("p", "port", true, "The port the server listents to. The default is 8080.");
        
        CommandLineParser parser = new DefaultParser();
        CommandLine commandLine = parser.parse(options, args);
        
        String config = null;
        if (!commandLine.getArgList().isEmpty()) {
            config = commandLine.getArgs()[0];
        }

        if (commandLine.hasOption('h')) {
            printHelp(options);
            System.exit(-1);
        }    
        
        int port = 8080;
        if (commandLine.hasOption('p')) {
            port = Integer.parseInt(commandLine.getOptionValue('p'));
        }

        // create a new (Jetty) server, and add a servlet handler
        Server server = new Server(port);
        
        // The filesystem paths we will map
        String pwdPath = System.getProperty("user.dir");
        String assetsPath = pwdPath + "/assets";

        // Setup the basic application "context" for this application at "/"
        // This is also known as the handler tree (in jetty speak)
        ServletContextHandler context = new ServletContextHandler(ServletContextHandler.SESSIONS);
        context.setResourceBase(pwdPath);
        context.setContextPath("/");
        server.setHandler(context);

        // add a simple Servlet at "/dynamic/*"
        ServletHolder holderDynamic = new ServletHolder("dynamic", LinkedDataFragmentServlet.class);
        holderDynamic.setInitParameter(LinkedDataFragmentServlet.CFGFILE, config);
        context.addServlet(holderDynamic, "/*");

        // add special pathspec of "/home/" content mapped to the homePath
        ServletHolder holderHome = new ServletHolder("static-home", DefaultServlet.class);
        holderHome.setInitParameter("resourceBase",assetsPath);
        holderHome.setInitParameter("dirAllowed","true");
        holderHome.setInitParameter("pathInfoOnly","true");
        context.addServlet(holderHome,"/assets/*");

        // Lastly, the default servlet for root content (always needed, to satisfy servlet spec)
        // It is important that this is last.
        ServletHolder holderPwd = new ServletHolder("default", DefaultServlet.class);
        holderPwd.setInitParameter("dirAllowed","true");
        context.addServlet(holderPwd,"/");
        
        
        
        // start the server
        server.start();
        System.out.println("Started server, listening at port " + port);

        // The use of server.join() the will make the current thread join and wait until the server is done executing.
        // See http://docs.oracle.com/javase/7/docs/api/java/lang/Thread.html#join()
        server.join();
    }
}
