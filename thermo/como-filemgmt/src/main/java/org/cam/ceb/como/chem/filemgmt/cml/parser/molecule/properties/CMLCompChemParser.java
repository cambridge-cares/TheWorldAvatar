package org.cam.ceb.como.chem.filemgmt.cml.parser.molecule.properties;

import org.cam.ceb.como.chem.filemgmt.parser.ChemFileParser;
import gigadot.chom.chem.structure.Compound;
import gigadot.chom.compchem.CompChem;
import gigadot.chom.compchem.CompChemWrapper;
import gigadot.chom.compchem.xml.NamespaceUtils;
import gigatools.extra.cmlxom.CMLException;
import gigatools.extra.cmlxom.CMLTools;
import java.io.File;
import java.util.HashMap;
import java.util.Map;
import nu.xom.Document;
import nu.xom.Nodes;
import org.xmlcml.cml.base.CMLElement;
import org.xmlcml.cml.base.CMLNamespace;
import org.xmlcml.cml.element.CMLModule;
import org.xmlcml.cml.element.CMLProperty;

/**
 *
 * @author pb556
 */
public class CMLCompChemParser extends ChemFileParser {

    private CompChemParser ccParser = new CompChemParser();
    
    protected CompChemWrapper ccw = null;
    
    protected Compound comp = null;
    protected CompChem cc = null;
    
    private Map<String, Integer> atomMap = null;

    /**
     * Read from data from compchem to compound. if compound contains old
     * compchem in compchem map then it will be removed. We use a default
     * extractor scheme implemented in CompChemWrapper.
     *
     * @param this.cc
     * @param compound
     */
    @Override
    public void parse() throws CMLException, Exception {
        atomMap = null;
        
        this.cc = null;
        Document doc = CMLTools.readCML(new File(this.path));
        Nodes mods = doc.query("//cml:module[@convention]", CMLNamespace.CML_XPATH);
        for (int i = 0; i < mods.size(); i++) {
            CMLModule el = (CMLModule) mods.get(i);
            String conventionPrefix = el.getPrefixForNamespace(CompChem.CONVENTION_NS);
            if (conventionPrefix != null || !conventionPrefix.isEmpty()) {
                if (el.getConvention().equals(conventionPrefix + ":compchem")) {
                    Map<String, String> prefix_ns_map = this.findInUsePrefixNamespaceMap(el);
                    el.detach();
                    for (Map.Entry<String, String> entry : prefix_ns_map.entrySet()) {
                        el.addNamespaceDeclaration(entry.getKey(), entry.getValue());
                    }
                    this.cc = new CompChem(el);
                }
            }
        }
        
        if (this.cc == null) {
            throw new Exception("CompChem object could not be created.");
        }

        this.ccParser.setCompChem(this.cc);
        this.ccParser.parse();
        this.comp = this.ccParser.get();
    }
    
    public Compound getCompound() {
        return this.comp;
    }
    
    @Override
    public Object get() {
        return this.cc;
    }
    
    public CompChem getCompChem() {
        return this.cc;
    }

    public CMLProperty getProperty(String dictRef, String namespace) {
        return this.ccParser.getProperty(dictRef, namespace);
    }
    
        private Map<String, String> findInUsePrefixNamespaceMap(CMLModule el) {
        // need to deal with all declare namespace prefix
        Map<String, String> prefix_ns_map = new HashMap<String, String>();
        String[] attrs = {"units", "unitType", "dictRef", "convention", "dataType"};
        for (int j = 0; j < attrs.length; j++) {
            Nodes elnodes = el.query("//*[@" + attrs[j] + "]", CMLNamespace.CML_XPATH);
            for (int k = 0; k < elnodes.size(); k++) {
                CMLElement e = (CMLElement) elnodes.get(k);
                String qname = e.getAttributeValue(attrs[j]);
                try {
                    String prefix = NamespaceUtils.getPrefixFromQName(qname);
                    String ns = e.getNamespaceURIForPrefix(prefix);
                    prefix_ns_map.put(prefix, ns);
                } catch (IllegalArgumentException ex) {
                }
            }
        }
        return prefix_ns_map;
    }
}
