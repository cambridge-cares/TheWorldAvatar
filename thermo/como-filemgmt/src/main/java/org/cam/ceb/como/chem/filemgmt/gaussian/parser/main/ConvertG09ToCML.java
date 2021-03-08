/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.gaussian.parser.main;

import org.cam.ceb.como.chem.filemgmt.gaussian.parser.GaussianHRParser;
import gigadot.chom.compchem.CompChem;
import gigadot.chom.compchem.CompChemIOUtils;
import java.io.File;
import java.io.IOException;

/**
 *
 * @author pb556
 */
public class ConvertG09ToCML {
    public static void main(String[] args) throws IOException, Exception {
        GaussianHRParser parser = new GaussianHRParser();
        parser.set("W:\\Backup\\g09\\freq\\success\\freq-fine-species-0529-radical-0-restricted.g09");
        parser.parse();
        CompChem cc = (CompChem) parser.get();
        CompChemIOUtils.write(new File("W:\\Backup\\g09\\freq\\success\\freq-fine-species-0529-radical-0-restricted.cml"), cc);
    }
}
