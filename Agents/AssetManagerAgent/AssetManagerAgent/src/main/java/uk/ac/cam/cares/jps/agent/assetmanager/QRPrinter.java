package uk.ac.cam.cares.jps.agent.assetmanager;

import com.google.zxing.*;
import com.google.zxing.client.j2se.MatrixToImageWriter;
import com.google.zxing.common.BitMatrix;
import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel;

import com.itextpdf.text.Image;
import com.itextpdf.text.pdf.PdfWriter;
import com.itextpdf.text.BadElementException;
import com.itextpdf.text.Document;
import com.itextpdf.text.DocumentException;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import java.net.URI;
import java.net.URISyntaxException;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.file.Path;
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
    //IPP handler for sending request to printer
    IPPHandler ipphandler;

    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(AssetManagerAgent.class);

    //constructor
    public QRPrinter(String printServerURL, String qrStorageFolder) throws Exception {
        printingURL = printServerURL;
        folderQR = qrStorageFolder;
        ipphandler = new IPPHandler(printServerURL);
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

    }

    //send print request
    void sendPrintingRequest (String iriString) throws Exception{
        String PDFPathString;
        // Create/get pdf from IRI
        try {
            String QRpathString = getQRPath (iriString);
            PDFPathString = QRpathString.replace(".png", ".pdf");
        
            Path QRpath = Paths.get(ClassLoader.getSystemResource(QRpathString).toURI());

            Document document = new Document();
            PdfWriter.getInstance(document, new FileOutputStream(PDFPathString));
            document.open();
            Image img = Image.getInstance(QRpath.toAbsolutePath().toString());
            document.add(img);

            document.close();
            
        } catch (URISyntaxException e) {
            // Cant get path
            throw new Exception("Failed to get QR code path", e);
        }
        catch (IOException e){
            throw new Exception("Writer failed to access QR code", e);
        }
        catch (WriterException e){
            throw new Exception("Writer failed to create QR code", e);
        }
        catch (DocumentException e){
            throw new Exception("Failed to load QR code .png", e);
        }

        //Send pdf to printer
        try {
            ipphandler.printFile(PDFPathString);
        } catch (Exception e) {
            throw new Exception("Failed to print QR code.", e);
        }

    }

    String getQRPath (String iriString) throws IOException, WriterException {
        String path = folderQR + "/" + iriString.split("/")[iriString.split("/").length-1] + ".png";
        //Check the folder
        File f = new File(path);
        if(!(f.exists() && !f.isDirectory())) { 
            //If does not exist in folder, create a new one
            createQR(iriString, path, charset, 1000, 1000);
        }

        return path;
    }


}