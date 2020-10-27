package gigadot.chom.compchem.orm.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Use this to annotate a wrapper field in the entity class
 * @author Weerapong Phadungsukanan
 */
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface CompChemWrapperField {
}
