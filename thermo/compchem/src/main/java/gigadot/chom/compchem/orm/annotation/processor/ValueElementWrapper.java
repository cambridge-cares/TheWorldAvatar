package gigadot.chom.compchem.orm.annotation.processor;

import org.xmlcml.cml.element.CMLArray;
import org.xmlcml.cml.element.CMLParameter;
import org.xmlcml.cml.element.CMLProperty;
import org.xmlcml.cml.element.CMLScalar;

/**
 *
 * @author wp214
 */
class ValueElementWrapper {
    private final CMLProperty property;
    private final CMLParameter parameter;
    private final String type;

    public ValueElementWrapper(CMLProperty property) {
        this.property = property;
        this.parameter = null;
        type = "property";
    }

    public ValueElementWrapper(CMLParameter parameter) {
        this.property = null;
        this.parameter = parameter;
        type = "parameter";
    }

    /**
     * exists if only one of them is not null.
     * @return
     */
    public boolean exists() {
        return (property != null) != (parameter != null);
    }

    public CMLScalar getScalar() {
        if (property != null) {
            return property.getScalarElements().get(0);
        } else {
            return parameter.getScalarElements().get(0);
        }
    }

    public CMLArray getArray() {
        if (property != null) {
            return property.getArrayElements().get(0);
        } else {
            return parameter.getArrayElements().get(0);
        }
    }

    public String type() {
        return type;
    }

}
