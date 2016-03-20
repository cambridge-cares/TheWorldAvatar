package uk.ac.cam.ceb.como.io.chem.file.parser.cml;

import com.cmclinnovations.io.file.parser.FileParser;
import java.util.HashMap;
import java.util.Map;
import nu.xom.Document;
import nu.xom.Nodes;
import org.xmlcml.cml.base.CMLElement;
import org.xmlcml.cml.base.CMLNamespace;
import org.xmlcml.cml.element.CMLModule;
import org.xmlcml.cml.element.CMLProperty;
import uk.ac.cam.ceb.como.compchem.CompChem;
import uk.ac.cam.ceb.como.compchem.xml.NamespaceUtils;
import uk.ac.cam.ceb.como.io.chem.file.parser.compchem.CompChemParser;

/**
 *
 * @author pb556
 */
public class CMLCompChemParser extends FileParser<CompChem> {

    private CompChemParser ccParser = new CompChemParser();
    private CompChem cc = null;

    /**
     * Read from data from compchem to compound. if compound contains old
     * compchem in compchem map then it will be removed. We use a default
     * extractor scheme implemented in CompChemWrapper.
     *
     * @param this.cc
     * @param compound
     */
    @Override
    public void parse() throws Exception {
            if (getFile() == null) {
                throw new Exception("No input file defined.");
            }
        try {
            this.cc = null;
            Document doc = CMLTools.readCML(getFile());
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
        } catch (Exception ex) {
            throw new Exception("File could not be read.");
        }

        if (this.cc == null) {
            throw new Exception("CompChem object could not be created.");
        }
    }

    @Override
    public CompChem get() {
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

    @Override
    public void clear() throws Exception {
        ccParser = new CompChemParser();
        cc = null;
    }
}
