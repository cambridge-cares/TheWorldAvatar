package gigadot.chom.compchem.orm.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 *
 * @author Weerapong Phadungsukanan
 */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface Parameter {

    public String dictRef();

    public String units() default "";

    public boolean optional() default false;

    public ContainerType type() default ContainerType.Scalar;

    public Class converter() default Object.class;
}
