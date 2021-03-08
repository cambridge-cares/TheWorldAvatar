/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.io.chem.file.parser.compchem;

import java.util.List;
import org.xmlcml.cml.element.CMLProperty;
import org.xmlcml.cml.element.CMLPropertyList;
import uk.ac.cam.ceb.como.compchem.CompChem;
import uk.ac.cam.ceb.como.compchem.CompChemWrapper;
import uk.ac.cam.ceb.como.compchem.xml.NamespaceUtils;

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
    
    public CMLProperty getProperty(CompChem cc, String dictRef, String namespace) {
        setCompChem(cc);
        return getProperty(dictRef, namespace);
    } 
}
