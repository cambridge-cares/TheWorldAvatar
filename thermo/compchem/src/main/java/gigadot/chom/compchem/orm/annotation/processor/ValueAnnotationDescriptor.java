package gigadot.chom.compchem.orm.annotation.processor;

import gigadot.chom.compchem.orm.annotation.ContainerType;
import gigadot.chom.compchem.orm.annotation.Parameter;
import gigadot.chom.compchem.orm.annotation.Property;

/**
 *
 * @author wp214
 */
class ValueAnnotationDescriptor {
    public final String dictRef;
    public final String units;
    public final ContainerType type;
    public final Class converter;
    public final boolean optional;

    public ValueAnnotationDescriptor(Property v) {
        this.dictRef = v.dictRef();
        this.units = v.units();
        this.type = v.type();
        this.converter = v.converter();
        this.optional = v.optional();
    }

    public ValueAnnotationDescriptor(Parameter v) {
        this.dictRef = v.dictRef();
        this.units = v.units();
        this.type = v.type();
        this.converter = v.converter();
        this.optional = v.optional();
    }

}
