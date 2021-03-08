package uk.ac.cam.ceb.como.extra.xom;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import nu.xom.Builder;
import nu.xom.Document;
import nu.xom.Serializer;
import org.apache.commons.io.IOUtils;

/**
 * This class contains supplementary functions which are needed in the programs.
 *
 * @author pb556
 */
public class XMLTools {

    /**
     * A function which read an XML from given file path to XML file and create
     * XML <code>Document</code> object.
     * @param file
     * @return XML <code>Document</code> of the xml file.
     * @throws XMLException
     * @see Document
     */
    public static Document readXML(File file) throws XMLException {
        Document doc = null;
        Builder parser = new Builder();
        try {
            doc = parser.build(file);
        } catch (Exception ex) {
            throw new XMLException(ex);
        }
        return doc;
    }

    /**
     * A function which read an XML from given XML input stream to
     * XML <code>Document</code> object.
     * @param ins XML input stream.
     * @return XML <code>Document</code> of the xml file.
     * @throws XMLException
     * @see Document
     */
    public static Document readXML(InputStream ins) throws XMLException {
        Document doc = null;
        Builder parser = new Builder();
        try {
            doc = parser.build(ins);
        } catch (Exception ex) {
            throw new XMLException("Cannot parse from InputStream", ex);
        }
        return doc;
    }

    /**
     * A function which write a xom document object to XML file. If the directory
     * path does not exist, it tries to create all neccessary non-existing parent
     * directory.
     *
     * @param file
     * @param doc xom Document to be output.
     * @throws FileNotFoundException
     * @throws IOException
     * @see Document
     */
    public static void writeXML(File file, Document doc) throws FileNotFoundException, IOException {
        FileOutputStream fout = null;
        file.getAbsoluteFile().getParentFile().mkdirs();
        try {
            fout = new FileOutputStream(file);
            writeXML(fout, doc);
        } finally {
            IOUtils.closeQuietly(fout);
        }
    }

    /**
     * A function which write a xom document object to an OutputStream object. The
     * OutputStream is internally buffered before being written.
     *
     * @param out an OutputStream object.
     * @param doc xom Document to be output.
     * @throws IOException
     * @see Document
     */
    public static void writeXML(OutputStream out, Document doc) throws IOException {
        BufferedOutputStream buffOut = new BufferedOutputStream(out);
        Serializer ser = new Serializer(buffOut, "UTF-8");
        ser.setIndent(4);
        ser.write(doc);
    }
}
