package uk.ac.cam.ceb.como.compchem.xml;

import org.xmlcml.cml.base.CMLElement;

/**
 *
 * @author pb556
 */

public class NamespaceUtils {

    public static String getPrefixFromQName(final String qname) throws IllegalArgumentException {
        String[] qname_array = qname.split(":");
        if (qname_array.length == 2) {
            String prefix = qname_array[0].trim();
            return prefix;
        } else {
            throw new IllegalArgumentException("String '" + qname + "' is not a valid QName.");
        }
    }

    public static String getLocalNameFromQName(final String qname) throws IllegalArgumentException {
        String[] qname_array = qname.split(":");
        if (qname_array.length == 2) {
            String localname = qname_array[1].trim();
            return localname;
        } else {
            throw new IllegalArgumentException("String '" + qname + "' is not a valid QName.");
        }
    }

    public static String createQName(CMLElement el, final String qname, final String namespace) {
        String[] qname_array = qname.split(":");
        if (qname_array.length == 2) {
            String prefix = qname_array[0].trim();
            if (prefix.isEmpty()) {
                String ns = el.getPrefixForNamespace(namespace);
                if (ns == null || ns.isEmpty()) {
                    throw new IllegalArgumentException("Namespace '" + namespace + "' does not exist. Please define prefix or use existing namespace");
                }
            }
            return getPrefixForKnownNamespace(el, prefix, namespace) + ":" + qname_array[1].trim();
        } else {
            throw new IllegalArgumentException("String '" + qname + "' is not a valid QName.");
        }
    }

    public static String getPrefixForKnownNamespace(CMLElement el, String defaultPrefix, String namespace) {
        el.addNamespaceDeclaration(defaultPrefix, namespace);
        return el.getPrefixForNamespace(namespace);
    }
}