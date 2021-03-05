/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.io.chem.file.writer.mol;

import org.apache.commons.io.FileUtils;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.xmlcml.cml.element.CMLMolecule;
import uk.ac.cam.ceb.como.io.chem.file.writer.ChemFileWriter;
import uk.ac.cam.ceb.como.openbabel.cml.OpenBabelCMLConverter;

/**
 *
 * @author pb556
 */
public class MolWriter extends ChemFileWriter<CMLMolecule> {

    @Override
    public void write() throws Exception {
        try {
            if (getFile() == null) {
                throw new Exception("No output file defined.");
            }
        } catch (Exception ex) {
            throw new Exception("Invalid output file defined.");
        }
        try {
            if (getContent() == null) {
                throw new Exception("No CMLMolecule defined.");
            }
            CMLMolecule mol = getContent();
            String convert = OpenBabelCMLConverter.convert(mol, "mol");
            FileUtils.writeStringToFile(getFile(), convert);
            return;
        } catch (Exception ex) {
            Logger.getLogger(MolWriter.class.getName()).log(Level.FATAL, null, ex);
        }
        throw new Exception("Unsuccessful writing operation.");
    }
    
    @Override
    public void clear() throws Exception {
        try {
            setContent(null);
        } catch (Exception ex) {
            throw new Exception(ex);
        }
    }
}
