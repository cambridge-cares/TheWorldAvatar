/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.tools.util;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.io.UnsupportedEncodingException;
import java.nio.channels.FileChannel;

/**
 *
 * @author pb556
 */
public abstract class LineReader {

    protected File file;
    protected final FileChannel channel;
    protected final String encoding;
    protected ByteArrayOutputStream baos = new ByteArrayOutputStream();

    public LineReader(File file, String encoding) throws IOException {
        RandomAccessFile raf = new RandomAccessFile(file, "r");
        channel = raf.getChannel();
        this.encoding = encoding;
        this.file = file;
    }
    
    public abstract String readLine() throws IOException;
    
    protected String bufToString() throws UnsupportedEncodingException {
        if (baos.size() == 0) {
            return "";
        }

        byte[] bytes = baos.toByteArray();
        for (int i = 0; i < bytes.length / 2; i++) {
            byte t = bytes[i];
            bytes[i] = bytes[bytes.length - i - 1];
            bytes[bytes.length - i - 1] = t;
        }

        baos.reset();

        return new String(bytes, encoding);
    }
}
