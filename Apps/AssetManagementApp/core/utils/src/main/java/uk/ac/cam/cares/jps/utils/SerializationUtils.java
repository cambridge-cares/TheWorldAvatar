package uk.ac.cam.cares.jps.utils;

import android.content.Context;
import android.net.Uri;
import android.os.ParcelFileDescriptor;

import org.apache.commons.codec.binary.Base64;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

public class SerializationUtils {

//    public static String serializeFileToString(String filePath) {
//        try {
//            byte[] fileBytes = Files.readAllBytes(Paths.get(filePath));
//            return java.util.Base64.getEncoder().encodeToString(fileBytes);
//        } catch (IOException e) {
//            throw new RuntimeException(e);
//        }
//    }

    public static String serializeFileToString(Uri fileUri, Context context) {
        try (ParcelFileDescriptor parcelFileDescriptor = context.getContentResolver().openFileDescriptor(fileUri, "r")) {
            FileInputStream fileInputStream = new FileInputStream(parcelFileDescriptor.getFileDescriptor());

            ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
            byte[] buffer = new byte[1024];
            int bytesRead;
            while ((bytesRead = fileInputStream.read(buffer)) != -1) {
                byteArrayOutputStream.write(buffer, 0, bytesRead);
            }

            String result = java.util.Base64.getEncoder().encodeToString(byteArrayOutputStream.toByteArray());
            byteArrayOutputStream.close();
            return result;
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static String serializeObjectToString(Serializable obj) throws IOException {
        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        ObjectOutputStream objectOutputStream = new ObjectOutputStream(byteArrayOutputStream);

        objectOutputStream.writeObject(obj);
        objectOutputStream.flush();

        String result =  Base64.encodeBase64String(byteArrayOutputStream.toByteArray());

        objectOutputStream.close();
        byteArrayOutputStream.close();
        return result;
    }

    public static Object deserializeStringToObject(String serializedString) throws IOException, ClassNotFoundException {
        byte[] byteArray = Base64.decodeBase64(serializedString);

        ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(byteArray);
        ObjectInputStream objectInputStream = new ObjectInputStream(byteArrayInputStream);

        Object result = objectInputStream.readObject();

        objectInputStream.close();
        byteArrayInputStream.close();

        return result;
    }
}
