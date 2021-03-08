/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.io.chem.file.parser.cml;

import java.io.IOException;
import nu.xom.ParsingException;
import uk.ac.cam.ceb.como.io.chem.file.parser.ChemFileParser;
import org.xmlcml.cml.base.CMLBuilder;
import org.xmlcml.cml.element.CMLMolecule;

/**
 *
 * @author pb556
 */

public class CMLMoleculeParser extends ChemFileParser<CMLMolecule> {

    protected CMLMolecule obj = null;
    
    @Override
    public void parse() throws Exception {
        CMLBuilder p = new CMLBuilder();
        try {
            obj = (CMLMolecule) p.build(getFile()).getRootElement();
        } catch (ParsingException ex) {
            throw new Exception("ParsingException : " + getFile().getAbsolutePath(), ex);
        } catch (IOException ex) {
            throw new Exception("IOException : " + getFile().getAbsolutePath(), ex);
        }
    }

    @Override
    public void clear() throws Exception {
        f = null;
        obj = null;
    }

    @Override
    public CMLMolecule get() throws Exception {
        return obj;
    }
    
}