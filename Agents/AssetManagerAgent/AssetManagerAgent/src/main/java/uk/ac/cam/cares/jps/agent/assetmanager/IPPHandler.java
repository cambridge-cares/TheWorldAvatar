package uk.ac.cam.cares.jps.agent.assetmanager;

import java.net.URL;
import org.cups4j.*;

public class IPPHandler {
    public CupsPrinter cupsPrinter;
    public CupsClient cupsClient;

    IPPHandler(URL PrinterURL) throws Exception {
        this(PrinterURL.toString());
    }


    IPPHandler (String PrinterURLString) throws Exception {
        try {
            URL PrinterURL = new URL(PrinterURLString);
            String [] PrinterURLComponent = PrinterURLString.split("\\/");
            String [] HostPort = PrinterURLComponent[2].split(":");
            String host = HostPort[0];
            int port = Integer.parseInt(HostPort[1]);

            cupsClient = new CupsClient(host, port);
            cupsPrinter = cupsClient.getPrinter(PrinterURL);
        } catch (Exception e) {
            throw new Exception("Failed to init IPP Handler.", e);
        }
        
    }


}
    
