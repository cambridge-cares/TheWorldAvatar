/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.structure.util.description;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import org.apache.commons.exec.CommandLine;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.SystemUtils;
import org.cam.ceb.como.openbabel.cml.OpenBabelCMLConverter;
import org.cam.ceb.como.openbabel.cml.OpenBabelConverter;
import org.cam.ceb.como.openbabel.cml.OpenBabelException;
import org.xmlcml.cml.element.CMLMolecule;

/**
 *
 * @author pb556
 */
public class InChIBuilder extends OBOutputBuilder {

    public InChIBuilder() {
        super();
    }

    private File getTempDir() {
        return new File(SystemUtils.getUserHome(), ".jbabel/builder/" + UUID.randomUUID());
    }

    @Override
    public String build(CMLMolecule mol) {
        getConverter().SetOutFormat(getOBOutputFormat());
        try {
            // problem with the conversion from CML to InChI: Reading from a CML file
            // results in an automatic addition of hydrogens to the given structure and 
            // finally results in a wrong structural description
            // solution: conversion from CML to mol works and then convert mol to inchi which
            // works as well
            // additional information: the process does not work via terminal nor via this interface
            File tempDir = getTempDir();
            tempDir.mkdirs();
            File input = new File(tempDir, "input.cml");
            File output = new File(tempDir, "input.mol");

            // initialize input file
            try {
                FileUtils.writeStringToFile(input, mol.toXML(), "UTF-8");
            } catch (Exception ex) {
                throw new OpenBabelException("Unable to process the CML molecule " + mol.toXML(), ex);
            }

            try {
                OpenBabelConverter.convert(CommandLine.parse("babel -icml " + input.getAbsolutePath() + " -omol " + output.getAbsolutePath() + " --DoNotAddH"), input, output, false, false);
                input = new File(tempDir, "input.mol");
                output = new File(tempDir, "output.inchi");
                return OpenBabelConverter.convert(CommandLine.parse("babel -imol " + input.getAbsolutePath() + " -oinchi " + output.getAbsolutePath() + " --DoNotAddH"), input, output, false, false);
            } catch (OpenBabelException obe) {
                throw obe;
            } finally {
                FileUtils.deleteQuietly(tempDir);
            }
        } catch (OpenBabelException ex) {
            return null;
        }
    }

    @Override
    protected String getOBOutputFormat() {
        return "inchi";
    }
}
