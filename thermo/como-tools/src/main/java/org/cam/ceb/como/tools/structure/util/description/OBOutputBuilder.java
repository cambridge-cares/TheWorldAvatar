/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.structure.util.description;

import gigadot.chom.chem.structure.Compound;
import gigadot.chom.chem.structure.Molecule;
import gigadot.chom.chem.structure.Structure;
import org.cam.ceb.como.openbabel.util.OpenBabelUtil;
import org.cam.ceb.como.tools.structure.util.CompoundConverter;
import org.openbabel.OBConversion;
import org.openbabel.OBMol;
import org.xmlcml.cml.element.CMLMolecule;

/**
 *
 * @author pb556
 */
public abstract class OBOutputBuilder implements CompoundDescriptionBuilderIntf {

    //private OBConversion obconv;
    
    public OBOutputBuilder() {
        System.loadLibrary("openbabel_java");
        //boolean exists = OpenBabelUtil.executableExists();
    }
    
    protected OBConversion getConverter() {
        return new OBConversion();
    }
    
    @Override
    public String build(CMLMolecule mol) {
        OBMol m = CompoundConverter.convertToOBMol(mol);
        OBConversion obconv = getConverter();
        obconv.SetOutFormat(getOBOutputFormat());
        return obconv.WriteString(m).trim();
    }

    @Override
    public String build(Structure mol) {
        return build(CompoundConverter.convert(mol));
    }

    @Override
    public String build(Molecule mol) {
        return build(CompoundConverter.convert(mol));
    }

    @Override
    public String build(Compound mol) {
        return build(CompoundConverter.convert(mol));
    }
    
    protected abstract String getOBOutputFormat();
}