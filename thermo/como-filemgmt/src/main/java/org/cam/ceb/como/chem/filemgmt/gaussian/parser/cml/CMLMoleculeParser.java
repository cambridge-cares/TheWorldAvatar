/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.gaussian.parser.cml;

import gigatools.extra.cmlxom.CMLException;
import java.io.IOException;
import nu.xom.Document;
import nu.xom.ParsingException;
import org.cam.ceb.como.chem.filemgmt.parser.ChemFileParser;
import org.xmlcml.cml.base.CMLBuilder;
import org.xmlcml.cml.element.CMLMolecule;

/**
 *
 * @author pb556
 */
public class CMLMoleculeParser extends ChemFileParser {

    @Override
    public void parse() throws Exception {
        CMLMolecule doc = null;
        CMLBuilder p = new CMLBuilder();
        try {
            doc = (CMLMolecule) p.build(getFile()).getRootElement();
            obj = doc;
        } catch (ParsingException ex) {
            throw new CMLException("ParsingException : " + getFile().getAbsolutePath(), ex);
        } catch (IOException ex) {
            throw new CMLException("IOException : " + getFile().getAbsolutePath(), ex);
        }
    }
    
}
