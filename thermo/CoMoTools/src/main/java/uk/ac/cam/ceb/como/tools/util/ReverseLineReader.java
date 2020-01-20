/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.tools.util;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;

/**
 *
 * @author pb556
 */
public class ReverseLineReader extends LineReader {
    private static final int BUFFER_SIZE = 8192;

    private long filePos;
    private ByteBuffer buf;
    private int bufPos;
    private byte lastLineBreak = '\n';

    public ReverseLineReader(File file, String encoding) throws IOException {
        super(file, encoding);
    }

    @Override
    public String readLine() throws IOException {
        while (true) {
            if (bufPos < 0) {
                if (filePos == 0) {
                    if (baos == null) {
                        return null;
                    }
                    String line = bufToString();
                    baos = null;
                    return line;
                }

                long start = Math.max(filePos - BUFFER_SIZE, 0);
                long end = filePos;
                long len = end - start;

                buf = channel.map(FileChannel.MapMode.READ_ONLY, start, len);
                bufPos = (int) len;
                filePos = start;
            }

            while (bufPos-- > 0) {
                byte c = buf.get(bufPos);
                if (c == '\r' || c == '\n') {
                    if (c != lastLineBreak) {
                        lastLineBreak = c;
                        continue;
                    }
                    lastLineBreak = c;
                    return bufToString();
                }
                baos.write(c);
            }
        }
    }

//    public static void main(String[] args) throws IOException {
//        File file = new File("my.log");
//        ReverseLineReader reader = new ReverseLineReader(file, "UTF-8");
//        String line;
//        while ((line = reader.readLine()) != null) {
//            System.out.println(line);
//        }
//    }
//
//    public static class ReverseLineReaderTest extends TestCase {
//        public void test() throws IOException {
//            File file = new File("utf8test.log");
//            String encoding = "UTF-8";
//
//            FileInputStream fileIn = new FileInputStream(file);
//            Reader fileReader = new InputStreamReader(fileIn, encoding);
//            BufferedReader bufReader = new BufferedReader(fileReader);
//            List<String> lines = new ArrayList<String>();
//            String line;
//            while ((line = bufReader.readLine()) != null) {
//                lines.add(line);
//            }
//            Collections.reverse(lines);
//
//            ReverseLineReader reader = new ReverseLineReader(file, encoding);
//            int pos = 0;
//            while ((line = reader.readLine()) != null) {
//                assertEquals(lines.get(pos++), line);
//            }
//
//            assertEquals(lines.size(), pos);
//        }
//    }
}