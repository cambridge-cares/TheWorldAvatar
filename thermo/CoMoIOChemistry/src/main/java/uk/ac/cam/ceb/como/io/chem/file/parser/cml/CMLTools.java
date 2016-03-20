package uk.ac.cam.ceb.como.io.chem.file.parser.cml;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import nu.xom.Document;
import nu.xom.ParsingException;
import org.xmlcml.cml.base.CMLBuilder;

/**
 * This class contains supplementary functions which are needed in the programs.
 *
 * @author pb556
 */
public class CMLTools {

    /**
     * A function which read an CML from given file path to CML file and create
     * <code>Document</code> object.
     *
     * @param cmlString cml string.
     * @return <code>Document</code> of the cml string.
     * @throws Exception
     */
    public static synchronized Document readCML(String cmlString) throws Exception {
        Document doc = null;
        CMLBuilder parser = new CMLBuilder();
        try {
            doc = parser.build(cmlString, null);
        } catch (ParsingException ex) {
            throw new Exception("ParsingException : " + cmlString, ex);
        } catch (IOException ex) {
            throw new Exception("IOException : " + cmlString, ex);
        }
        return doc;
    }

    /**
     * A function which read an CML from given File object to CML file and create
     * <code>Document</code> object.
     *
     * @param cml_file File object of cml file.
     * @return <code>Document</code> of the cml file.
     * @throws Exception
     */
    public static synchronized Document readCML(File cml_file) throws Exception {
        Document doc = null;
        CMLBuilder parser = new CMLBuilder();
        try {
            doc = parser.build(cml_file);
        } catch (ParsingException ex) {
            throw new Exception("ParsingException : " + cml_file.getPath(), ex);
        } catch (IOException ex) {
            throw new Exception("IOException : " + cml_file.getPath(), ex);
        }
        return doc;
    }

    /**
     * A function which read an CML from a given input stream
     * <code>Document</code> object.
     *
     * @param ins XML input stream.
     * @return XML <code>Document</code> of the xml file.
     * @throws Exception
     * @see Document
     */
    public static synchronized Document readCML(InputStream ins) throws Exception {
        Document doc = null;
        CMLBuilder parser = new CMLBuilder();
        try {
            doc = parser.build(ins);
        } catch (ParsingException ex) {
            throw new Exception("ParsingException : Problem with parsing CML from an input stream", ex);
        } catch (IOException ex) {
            throw new Exception("IOException : Problem with reading CML from an input stream", ex);
        }
        return doc;
    }
}
