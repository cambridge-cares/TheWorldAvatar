package uk.ac.cam.ceb.como.compchem.orm.annotation.processor;

import uk.ac.cam.ceb.como.compchem.orm.annotation.ContainerType;
import uk.ac.cam.ceb.como.compchem.orm.annotation.Parameter;
import uk.ac.cam.ceb.como.compchem.orm.annotation.Property;

/**
 *
 * @author pb556
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
