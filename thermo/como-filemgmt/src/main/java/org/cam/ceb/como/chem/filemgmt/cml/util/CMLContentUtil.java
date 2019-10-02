/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.cml.util;

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
public class CMLContentUtil {

    public static CMLProperty getProperty(CompChem cc, String dictRef, String namespace) throws Exception {
        CompChemWrapper ccw = new CompChemWrapper(cc);
        String lname = NamespaceUtils.getLocalNameFromQName(dictRef);
        String prefix = ccw.getCompchem().getPrefixForNamespace(namespace);
        String ldictRef = prefix + ":" + lname;
        CMLPropertyList list = ccw.getPropertyList();
        if (list != null) {
            List<CMLProperty> props = list.getPropertyDescendantsByName(ldictRef);
            if (!props.isEmpty()) {
                return props.get(props.size() - 1);
            } else {
                return null;
            }
        } else {
            throw new RuntimeException("No property identified.");
        }
    }
}
