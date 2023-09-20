package uk.ac.cam.cares.jps.agent.assetmanager;

import com.google.zxing.*;
import com.google.zxing.client.j2se.MatrixToImageWriter;
import com.google.zxing.common.BitMatrix;
import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel;
import com.itextpdf.io.image.ImageData;
import com.itextpdf.io.image.ImageDataFactory;
import com.itextpdf.kernel.pdf.PdfDocument; 
import com.itextpdf.kernel.pdf.PdfWriter; 

import com.itextpdf.layout.Document; 
import com.itextpdf.layout.element.Cell;
import com.itextpdf.layout.element.Image;
import com.itextpdf.layout.element.Table;
import com.itextpdf.layout.properties.UnitValue;

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
    //The paper is assumed to be portrait
    //The office printer is 1200 ppi
    final Double A4_PIXEL_WIDTH = 9922.; //210 mm
    final Double A4_PIXEL_HEIGHT = 14032.; //297 mm
    //With the given ppi, 1 cm corresponds to:
    final Double PIXEL_PER_CM = 472.44;
    //QR per page (sticker size is targetSize x targetSize+2 cm to accomodate ID text)
    Double targetSize;
    int targetSizePX;
    int row, col;

    //IPP handler for sending request to printer
    IPPHandler ipphandler;

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
        row = new Double(A4_PIXEL_WIDTH/(targetSize*PIXEL_PER_CM)).intValue();
        col = new Double(A4_PIXEL_WIDTH/((targetSize+2)*PIXEL_PER_CM)).intValue();
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
    public void sendPrintingRequest (String iriString) throws Exception{       
        String PDFPathString = java.time.LocalDateTime.now().toString() + "_QRCodes.pdf";    
        PdfWriter writer = new PdfWriter(PDFPathString);        
     
        PdfDocument pdf = new PdfDocument(writer);              
        Document document = new Document(pdf);              
    
        String imFile = getQRPath(iriString);       
        ImageData data = ImageDataFactory.create(imFile);                    
        Image image = new Image(data);                        
    
        document.add(image);              
     
        document.close();    

        //Send pdf to printer
        try {
            ipphandler.printFile(PDFPathString);
        } catch (Exception e) {
            throw new Exception("Failed to print QR code.", e);
        }

    }

    public void sendPrintRequest (String[] iriStringArr) throws Exception {
        try{    
            String PDFPathString = java.time.LocalDateTime.now().toString() + "_QRCodes.pdf";
            // Creating a PdfDocument object    
            PdfWriter writer = new PdfWriter(PDFPathString);
                
            // Creating a PdfDocument object      
            PdfDocument pdf = new PdfDocument(writer);                  
            
            // Creating a Document object       
            Document doc = new Document(pdf);                       
            Table table = new Table(UnitValue.createPercentArray(col)).useAllAvailableWidth();
            for (int i=0; i < iriStringArr.length;i++) {
                //if row runs out create a new page.
                if((i/col) >= row){

                }

                String iriString = iriStringArr[i];
                String QRPath = getQRPath(iriString);
                //Add image 
                Cell cell = new Cell();                   
                ImageData data = ImageDataFactory.create(QRPath);        
                Image img = new Image(data);              
                cell.add(img.setAutoScale(true));              
                table.addCell(cell);

                //Add label on the next row
                if (i % col == 0){
                    for(int j =i-col; j % col !=0 ; j++){
                        //add ID here
                    }

                }
            }

            doc.close();
        }
        catch (IOException e){
            throw new Exception("Writer failed to access QR code", e);
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



}