package uk.ac.cam.ceb.como.compchem;

import uk.ac.cam.ceb.como.compchem.xml.NamespaceUtils;
import java.util.List;
import org.xmlcml.cml.element.CMLArray;
import org.xmlcml.cml.element.CMLModule;
import org.xmlcml.cml.element.CMLMolecule;
import org.xmlcml.cml.element.CMLParameter;
import org.xmlcml.cml.element.CMLProperty;
import org.xmlcml.cml.element.CMLScalar;

/**
 *
 * @author pb556
 */
public class ContainerHelper {

    private final CMLModule cc;

    /**
     * cc object is used to query for namespaces and prefixes.
     * @param cc
     */
    public ContainerHelper(CMLModule cc) {
        this.cc = cc;
    }

    public CMLMolecule createMolecule() {
        String prefix = NamespaceUtils.getPrefixForKnownNamespace(cc, "cc", CompChem.CONVENTION_NS);
        CMLMolecule mol = new CMLMolecule();
        mol.setConvention(prefix + ":molecular");
        return mol;
    }

    public String createQName(final String qname, final String namespace) {
        return NamespaceUtils.createQName(cc, qname, namespace);
    }

    public CMLProperty createCMLProperty(final String dictRef, final String dictRefNamespace) {
        CMLProperty property = new CMLProperty();
        property.setDictRef(createQName(dictRef, dictRefNamespace));
        return property;
    }

    /**
     * create a string scalar property
     * @param value
     * @param dictRef
     * @param dictRefNamespace
     * @return
     */
    public CMLProperty createCMLPropertyScalar(final String value, final String dictRef, final String dictRefNamespace) {
        CMLProperty p = createCMLProperty(dictRef, dictRefNamespace);
        CMLScalar scalar = new CMLScalar(value);
        p.addScalar(scalar);
        return p;
    }

    /**
     * create a double scalar property
     * @param value
     * @param unitRef
     * @param dictRef
     * @param dictRefNamespace
     * @return
     */
    public CMLProperty createCMLPropertyScalar(double value, String unitRef, String dictRef, String dictRefNamespace) {
        CMLProperty p = createCMLProperty(dictRef, dictRefNamespace);
        CMLScalar scalar = new CMLScalar(value);
        scalar.setUnits(unitRef);
        p.addScalar(scalar);
        return p;
    }

    /**
     * create an integer scalar property
     * @param value
     * @param unitRef
     * @param dictRef
     * @param dictRefNamespace
     * @return
     */
    public CMLProperty createCMLPropertyScalar(int value, String unitRef, String dictRef, String dictRefNamespace) {
        CMLProperty p = createCMLProperty(dictRef, dictRefNamespace);
        CMLScalar scalar = new CMLScalar(value);
        scalar.setUnits(unitRef);
        p.addScalar(scalar);
        return p;
    }

    public CMLParameter createCMLParameter(final String dictRef, final String dictRefNamespace) {
        CMLParameter parameter = new CMLParameter();
        parameter.setDictRef(createQName(dictRef, dictRefNamespace));
        return parameter;
    }

    /**
     * create a string scalar parameter
     * @param value
     * @param dictRef
     * @param dictRefNamespace
     * @return
     */
    public CMLParameter createCMLParameterScalar(final String value, final String dictRef, final String dictRefNamespace) {
        CMLParameter parameter = createCMLParameter(dictRef, dictRefNamespace);
        CMLScalar scalar = new CMLScalar(value);
        parameter.addScalar(scalar);
        return parameter;
    }

    /**
     * create a double scalar parameter
     * @param value
     * @param unitRef
     * @param dictRef
     * @param dictRefNamespace
     * @return
     */
    public CMLParameter createCMLParameterScalar(final double value, String unitRef, final String dictRef, final String dictRefNamespace) {
        CMLParameter parameter = createCMLParameter(dictRef, dictRefNamespace);
        CMLScalar scalar = new CMLScalar(value);
        scalar.setUnits(unitRef);
        parameter.addScalar(scalar);
        return parameter;
    }

    /**
     * create a cml array of string from string array
     * @param strings
     * @return
     */
    public CMLArray createCMLArray(String[] strings) {
        CMLArray array = new CMLArray(strings);
        array.removeAttribute("delimiter");
        return array;
    }

    /**
     * create a cml array of string from string list
     * @param strings
     * @return
     */
    public CMLArray createCMLArray(List<String> strings) {
        String[] strs = new String[strings.size()];
        for (int i = 0; i < strings.size(); i++) {
            strs[i] = strings.get(i);
        }
        return createCMLArray(strs);
    }

    /**
     * create a cml array of double from double array
     * @param dbs
     * @param unit
     * @param unitNamespace
     * @return
     */
    public CMLArray createCMLArray(double[] dbs, final String unit, final String unitNamespace) {
        CMLArray array = new CMLArray(dbs);
        array.removeAttribute("delimiter");
        String unitQName = createQName(unit, unitNamespace);
        array.setUnits(unitQName);
        return array;
    }

    /**
     * create a cml array of double from double list
     * @param dbs
     * @param unit
     * @param unitNamespace
     * @return
     */
    public CMLArray createCMLArray(List<Double> dbs, final String unit, final String unitNamespace) {
        double[] ds = new double[dbs.size()];
        for (int i = 0; i < dbs.size(); i++) {
            ds[i] = dbs.get(i);
        }
        return createCMLArray(ds, unit, unitNamespace);
    }
}
