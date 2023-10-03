package uk.ac.cam.cares.jps.agent.assetmanager;

import com.google.zxing.*;
import com.google.zxing.client.j2se.MatrixToImageWriter;
import com.google.zxing.common.BitMatrix;

import com.itextpdf.io.image.ImageDataFactory;
import com.itextpdf.kernel.geom.PageSize;
import com.itextpdf.kernel.pdf.PdfDocument; 
import com.itextpdf.kernel.pdf.PdfWriter;
import com.itextpdf.layout.element.Image;
import com.itextpdf.layout.element.Paragraph;
import com.itextpdf.layout.Document;
import com.itextpdf.layout.element.AreaBreak;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URL;
import java.net.URLConnection;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.*;
import java.util.*;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;




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
    //The paper is assumed to be portrait
    //The office printer is 1200 ppi
    final Double A4_PIXEL_WIDTH = 9922.; //210 mm
    final Double A4_PIXEL_HEIGHT = 14032.; //297 mm
    //With the given ppi, 1 cm corresponds to:
    final Double PIXEL_PER_CM = 472.44;
    final Double POINT_PER_CM = 28.346;
    //QR per page (sticker size is targetSize x targetSize+2 cm to accomodate ID text)
    Double targetSize;
    int targetSizePX;



    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(AssetManagerAgent.class);

    //constructor
    public QRPrinter(String printServerURL, String qrStorageFolder, Double targetSizeCM) throws Exception {
        printingURL = printServerURL;
        folderQR = qrStorageFolder;
        targetSize = targetSizeCM;
        targetSizePX = new Double(PIXEL_PER_CM * targetSizeCM).intValue();
        LOGGER.info("Starting QR printer...");
        
    }

    public void PrintQRBulk(String[] iriStringArr)throws Exception  {
        LOGGER.info("Started bulk printing for IRIs: " + iriStringArr);
        String filename = "/root/QRCodes.pdf";
        PdfDocument pdfDoc = new PdfDocument(new PdfWriter(filename));
        try (Document document = new Document(pdfDoc, PageSize.A4)) {
            int qrCodeSize = (int) (targetSize * POINT_PER_CM);
            //int qrCodeSize = 200; // Size of each QR code in points (1/72 inch)
            //int A4_HEIGHT = 842;
            //int A4_WIDTH = 595;
            float A4_HEIGHT = PageSize.A4.getHeight();
            float A4_WIDTH = PageSize.A4.getWidth();
            float marginVert = 50;
            float marginHorz = 50;
            LOGGER.debug("PAPER SIZE::"+A4_WIDTH+"x"+A4_HEIGHT+";");
            int MAX_ROW = (int) ((A4_HEIGHT-marginHorz)/(qrCodeSize));
            int MAX_COL = (int) ((A4_WIDTH-marginVert)/(qrCodeSize));
            
            int r =0;
            int c =0;
            for (int counter = 0; counter < iriStringArr.length; counter ++){
                String iriString = iriStringArr[counter];
                float x = c * qrCodeSize + marginHorz/2;
                float y = A4_HEIGHT - (r * qrCodeSize) - marginVert/2;
                // x and y is the bottom left corner apparently so need move y down
                y-=qrCodeSize;
                LOGGER.debug("QR LOCATION::"+iriString+"::"+x+"-"+y+";");

                // Generate QR code image
                try {
                    String filePathQR = getQRPath(iriString);
                    LOGGER.debug("");
                    Image qrCodeImage = new Image(ImageDataFactory.create(filePathQR));
                    qrCodeImage.setFixedPosition(x, y);
                    qrCodeImage.scaleToFit(qrCodeSize, qrCodeSize);
                    document.add(qrCodeImage);
                } catch (Exception e) {
                    throw new Exception("Failed to create QR code for IRI:" + iriString);
                }
                
                c+=1;
                if (c==MAX_COL){
                    c=0;
                    r+=1;
                }
                if(r==MAX_ROW){
                    document.add(new AreaBreak());
                    r=0;
                }
            }
        }
        
        LOGGER.info("Finish creating pdf for printing: "+ filename);

        sendPrintRequest(filename);
    }

    public void PrintQRBulk(Map<String, String> idIRIMap)throws Exception  {
        LOGGER.info("Started bulk printing for ID-IRIs: " + idIRIMap);
        String filename = "/root/QRCodes.pdf";
        PdfDocument pdfDoc = new PdfDocument(new PdfWriter(filename));
        try (Document document = new Document(pdfDoc, PageSize.A4)) {
            int qrCodeSize = (int) (targetSize * POINT_PER_CM);
            //int qrCodeSize = 200; // Size of each QR code in points (1/72 inch)
            //int A4_HEIGHT = 842;
            //int A4_WIDTH = 595;
            float A4_HEIGHT = PageSize.A4.getHeight();
            float A4_WIDTH = PageSize.A4.getWidth();
            float marginVert = 50;
            float marginHorz = 50;
            LOGGER.debug("PAPER SIZE::"+A4_WIDTH+"x"+A4_HEIGHT+";");
            int MAX_ROW = (int) ((A4_HEIGHT-marginHorz)/(qrCodeSize));
            int MAX_COL = (int) ((A4_WIDTH-marginVert)/(qrCodeSize));
            
            int r =0;
            int c =0;
            for (Map.Entry<String, String> entry : idIRIMap.entrySet()){
                String ID = entry.getKey();
                String iriString = entry.getValue();
                float x = c * qrCodeSize + marginHorz/2;
                float y = A4_HEIGHT - (r * qrCodeSize) - marginVert/2;
                // x and y is the bottom left corner apparently so need move y down
                y-=qrCodeSize;
                LOGGER.debug("QR LOCATION::"+iriString+"::"+x+"-"+y+";");

                // Generate QR code image
                try {
                    String filePathQR = getQRPath(iriString);
                    LOGGER.debug("");
                    Image qrCodeImage = new Image(ImageDataFactory.create(filePathQR));
                    qrCodeImage.setFixedPosition(x, y);
                    qrCodeImage.scaleToFit(qrCodeSize, qrCodeSize);
                    document.add(qrCodeImage);

                    Paragraph p = new Paragraph(ID);
                    p.setFixedPosition(x, y, qrCodeSize);
                    document.add(p);

                } catch (Exception e) {
                    throw new Exception("Failed to create QR code for IRI:" + iriString);
                }
                
                c+=1;
                if (c==MAX_COL){
                    c=0;
                    r+=1;
                }
                if(r==MAX_ROW){
                    document.add(new AreaBreak());
                    r=0;
                }
            }
        }
        
        LOGGER.info("Finish creating pdf for printing: "+ filename);

        sendPrintRequest(filename);
    }

    private void sendPrintRequest (String filename) throws Exception {
        LOGGER.info("Sending print request for file: " + filename);
        //Encode pdf to base64
        byte[] input_file;
        try {
            input_file = Files.readAllBytes(Paths.get(filename));
        } catch (Exception e) {
            throw new Exception("Failed to fetch pdf:" + filename);
        }
        

        byte[] encodedBytes = Base64.getEncoder().encode(input_file);
        String encodedString =  new String(encodedBytes);

        //Add base 64 to request and send
        JSONObject body = new JSONObject();
        body.put("rawPDF", encodedString);

        URL url = new URL(printingURL);
        HttpClient client = HttpClient.newHttpClient();

        HttpRequest request = HttpRequest.newBuilder()
            .header("Content-Type", "application/json")
            .uri(URI.create(printingURL))
            .POST(HttpRequest.BodyPublishers.ofString(body.toString()))
            .build();

        HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());

        LOGGER.debug(response);
        if (response.statusCode()!= 200){
            throw new JPSRuntimeException("Printing server failed to print: "  + response.body());
        }
        
    }

    String getQRPath (String iriString) throws IOException, WriterException {
        String path = folderQR + "/" + iriString.split("/")[iriString.split("/").length-1] + ".png";
        //Check the folder
        File f = new File(path);
        if(!(f.exists() && !f.isDirectory())) { 
            //If does not exist in folder, create a new one
            createQR(iriString, path, charset, targetSizePX, targetSizePX);
        }

        return path;
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

        LOGGER.info("Created QR code for IRI: " + data);

    }



}