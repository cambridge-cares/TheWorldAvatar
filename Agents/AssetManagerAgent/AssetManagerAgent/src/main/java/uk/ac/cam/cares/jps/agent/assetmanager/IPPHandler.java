package uk.ac.cam.cares.jps.agent.assetmanager;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import org.cups4j.*;

public class IPPHandler {
    public CupsPrinter cupsPrinter;
    public CupsClient cupsClient;

    IPPHandler(String PrinterURLString) throws Exception {
        this(new URL(PrinterURLString));
    }


    IPPHandler (URL PrinterURL) throws Exception {
        try {
            String host = PrinterURL.getHost();
            int port = PrinterURL.getPort();

            cupsClient = new CupsClient(host, port);
            cupsPrinter = cupsClient.getPrinter(PrinterURL);
        } catch (Exception e) {
            throw new Exception("Failed to init IPP Handler.", e);
        }
        
    }

    void printFile (String fileLocation) throws IOException, Exception{
        InputStream inputStream;
        try {
            inputStream = new FileInputStream(fileLocation);
        } catch (Exception e) {
            throw new IOException("Failed to retrieve file.", e);
        }
        PrintJob printJob = new PrintJob.Builder(inputStream).build();

        try {
            PrintRequestResult printRequestResult = cupsPrinter.print(printJob);
            //TODO: Handle failed request based on responses
            
        } catch (Exception e) {
            throw new Exception("Failed to excute print.", e);
        }
        

        inputStream.close();
    }

}
    
