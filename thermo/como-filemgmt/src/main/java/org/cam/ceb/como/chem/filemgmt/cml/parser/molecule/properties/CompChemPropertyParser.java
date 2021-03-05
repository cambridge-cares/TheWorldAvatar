/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.cml.parser.molecule.properties;

import gigadot.chom.compchem.CompChem;
import gigadot.chom.compchem.CompChemWrapper;
import gigadot.chom.compchem.xml.NamespaceUtils;
import java.util.List;
import org.xmlcml.cml.element.CMLProperty;
import org.xmlcml.cml.element.CMLPropertyList;

/**
 *
 * @author pb556
 */
public class CompChemPropertyParser {
    
    protected CompChem cc = null;
    protected CompChemWrapper ccw = null;
    
    public void setCompChem(CompChem cc) {
        this.cc = cc;
        this.ccw = new CompChemWrapper(cc);
    }
    
    public CMLProperty getProperty(String dictRef, String namespace) {
        String lname = NamespaceUtils.getLocalNameFromQName(dictRef);
        String prefix = this.ccw.getCompchem().getPrefixForNamespace(namespace);
        String ldictRef = prefix + ":" + lname;
        CMLPropertyList list = this.ccw.getPropertyList();
        if (list != null) {
            List<CMLProperty> props = list.getPropertyDescendantsByName(ldictRef);
            if (!props.isEmpty()) {
                return props.get(props.size() - 1);
            } else {
                return null;
            }
        } else {
            //logger.warn("PropertyList not found.");
            return null;
        }
    }    
}
