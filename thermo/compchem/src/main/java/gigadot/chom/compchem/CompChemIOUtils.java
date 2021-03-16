package gigadot.chom.compchem;

import gigadot.chom.compchem.xml.NamespaceUtils;
import gigatools.extra.cmlxom.CMLException;
import gigatools.extra.cmlxom.CMLTools;
import gigatools.extra.io.FileTools;
import gigatools.extra.xom.XMLTools;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Map;
import nu.xom.Document;
import nu.xom.Nodes;
import org.apache.commons.io.IOUtils;
import org.xmlcml.cml.base.CMLElement;
import org.xmlcml.cml.base.CMLNamespace;
import org.xmlcml.cml.element.CMLModule;

/**
 *
 * @author Weerapong
 */
public class CompChemIOUtils {

    /**
     * read an xml file and return CompChem object. if the xml file contains no
     * CompChem module then return null. If the xml file contains more than 1
     * CompChme module, the first one from the xpath query is returned.
     *
     * @param file
     * @return
     * @throws CMLException
     */
    public static CompChem read(File file) throws CMLException {
        Document doc = CMLTools.readCML(file);
        Nodes mods = doc.query("//cml:module[@convention]", CMLNamespace.CML_XPATH);
        for (int i = 0; i < mods.size(); i++) {
            CMLModule el = (CMLModule) mods.get(i);
            String conventionPrefix = el.getPrefixForNamespace(CompChem.CONVENTION_NS);
            if (conventionPrefix != null || !conventionPrefix.isEmpty()) {
                if (el.getConvention().equals(conventionPrefix + ":compchem")) {
                    Map<String, String> prefix_ns_map = findInUsePrefixNamespaceMap(el);
                    el.detach();
                    for (Map.Entry<String, String> entry : prefix_ns_map.entrySet()) {
                        el.addNamespaceDeclaration(entry.getKey(), entry.getValue());
                    }
                    return new CompChem(el);
                }
            }
        }
        return null;
    }

    private static Map<String, String> findInUsePrefixNamespaceMap(CMLModule el) {
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

    public static void write(OutputStream out, CompChem cc) throws IOException {
        XMLTools.writeXML(out, new Document(cc));
    }

    public static void write(File file, CompChem cc) throws IOException {
        FileTools.forceMakeParentDirQuietly(file);
        FileOutputStream fout = new FileOutputStream(file);
        try {
            write(fout, cc);
        } finally {
            IOUtils.closeQuietly(fout);
        }
    }
}
