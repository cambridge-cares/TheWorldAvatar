package uk.ac.cam.cares.jps.agent.assetmanager;

import com.google.zxing.*;
import com.google.zxing.client.j2se.MatrixToImageWriter;
import com.google.zxing.common.BitMatrix;
import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.File;
import java.io.IOException;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.file.Paths;
import java.time.*;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;



/**
 * Class for creating and managing QR code printing for the assets
 */
public class QRPrinter {
    //Printing server URL
    static String printingURL;
    //QR storage folder
    static String folderQR;
    //filename encoding
    String charset = "UTF-8";

    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(AssetManagerAgent.class);

    //constructor
    public QRPrinter(String printServerURL, String qrStorageFolder) {
        printingURL = printServerURL;
        folderQR = qrStorageFolder;
    }

    //Create QR code from IRI
    //@SandraDeng
    public static void createQR(String data, String path, String charset, int height, int width) throws WriterException, IOException {
    BitMatrix matrix = new MultiFormatWriter().encode(
            new String(data.getBytes(charset), charset),
            BarcodeFormat.QR_CODE, width, height);

    MatrixToImageWriter.writeToFile(
            matrix,
            path.substring(path.lastIndexOf('.') + 1),
            new File(path));

            //alternative: Send as byte array ->String, decode in server
    }

    //send print request
    void sendPrintingRequest (String iri) throws IOException, WriterException {
        String path = folderQR + "/" + iri.split("/")[iri.split("/").length-1] + ".png";
        //Check the folder
        File f = new File(path);
        if(!(f.exists() && !f.isDirectory())) { 
            //If does not exist in folder, create a new one
            createQR(iri, path, charset, 200, 200);
        }
        
        sendPutRequest(path);

    }

    /**
     * Sends a PUT request for files to the specified url.
     * @param path path to the specified file.
     */
    protected static void sendPutRequest(String path) {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = null;
        try {
            request = HttpRequest.newBuilder()
                    .header("Content-Type","image/png")
                    .uri(URI.create(printingURL+"?fileName=" + path))
                    .PUT(HttpRequest.BodyPublishers.ofFile(Paths.get(path)))
                    .build();
            // Await response before continue executing the rest of the code
            client.send(request, HttpResponse.BodyHandlers.ofString());
        } catch (IOException e) {
            throw new JPSRuntimeException(e.getMessage() + " If connection is refused, the url is likely invalid!");
        } catch (InterruptedException e) {
            throw new RuntimeException("Thread has been interrupted!" + e.getMessage());
        }
    }

}