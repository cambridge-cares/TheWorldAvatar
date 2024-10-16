package uk.ac.cam.cares.jps.utils.di;

import android.content.Context;

import com.android.volley.RequestQueue;
import com.android.volley.toolbox.Volley;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.zip.GZIPOutputStream;

import javax.inject.Singleton;

import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.components.SingletonComponent;

/**
 * Dependency injection specification for Utils module
 */
@Module
@InstallIn(SingletonComponent.class)
public class UtilsModule {
    @Provides
    @Singleton
    public RequestQueue provideRequestQueue(@ApplicationContext Context applicationContext) {
        return Volley.newRequestQueue(applicationContext);
    }

    /**
     * Compresses a given string into a GZIP-compressed byte array.
     * This method uses the GZIP compression algorithm to reduce the size of the input string.
     *
     * @param data The input string to be compressed.
     * @return A byte array containing the GZIP-compressed data.
     * @throws IOException If an I/O error occurs during the compression process.
     */
    public static byte[] compressData(String data) throws IOException {
        try {
            ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
            GZIPOutputStream gzipOutputStream = new GZIPOutputStream(byteArrayOutputStream);
            gzipOutputStream.write(data.getBytes("UTF-8"));
            gzipOutputStream.close();
            return byteArrayOutputStream.toByteArray();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Computes an MD5 hash of the given string.
     *
     * @param data The input string to hash.
     * @return The MD5 hash as a hexadecimal string.
     */
    public static String computeHash(String data) {
        try {
            MessageDigest digest = MessageDigest.getInstance("MD5");
            byte[] hash = digest.digest(data.getBytes(StandardCharsets.UTF_8));
            // Convert the byte array into a hexadecimal string
            StringBuilder hexString = new StringBuilder();
            for (byte b : hash) {
                hexString.append(String.format("%02x", b));
            }
            return hexString.toString();
        } catch (NoSuchAlgorithmException e) {
            return null;
        }
    }

}
