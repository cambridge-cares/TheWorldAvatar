/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.nist.parser;

import uk.ac.cam.ceb.como.nist.thermochemistry.NISTGasPhaseThermoChemReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.util.StringList;

/**
 *
 * @author pb556
 */
public abstract class NISTParser {

    protected String path = "";
    protected StringList body = new StringList();
    protected Object data = new Object();

    public void setPath(String path) {
        this.path = path;
    }

    public void parse() {
        body = null;
        try {
            body = extractBody();
        } catch (FileNotFoundException ex) {
            Logger.getLogger(NISTGasPhaseThermoChemReader.class.getName()).log(Level.SEVERE, "The file " + path + "does not exist.", ex);
        } catch (IOException ex) {
            Logger.getLogger(NISTGasPhaseThermoChemReader.class.getName()).log(Level.SEVERE, "The file " + path + "could not be read.", ex);
        }
        if (body != null) {
            parseSection(body);
        }
    }
    
    public abstract Object get();

    public StringList extractBody() throws FileNotFoundException, IOException {
        return NISTHTMLReaderHelper.extractHTMLBody(new File(path));
    }

    public abstract void parseSection(StringList body);
}
