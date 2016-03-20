package uk.ac.cam.ceb.como.compchem.orm.annotation.processor;

import uk.ac.cam.ceb.como.compchem.CompChemWrapper;
import uk.ac.cam.ceb.como.compchem.orm.annotation.CompChemWrapperField;
import uk.ac.cam.ceb.como.compchem.orm.annotation.Prefixes;
import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang.ClassUtils;

/**
 *
 * @author pb556
 */
public class AnnotationUtils {

    public static Map<String, String> getPrefixNamespaceMap(Class<?> entityClass) {
        Prefixes prefiexesAnnotation = entityClass.getAnnotation(Prefixes.class);
        // prefix may be defined at its super class. interface prefix are ignore
        // we only get the first one.
        if (prefiexesAnnotation == null) {
            List<Class> allSuperclasses = ClassUtils.getAllSuperclasses(entityClass);
            for (Class<?> clz : allSuperclasses) {
                prefiexesAnnotation = clz.getAnnotation(Prefixes.class);
                if (prefiexesAnnotation != null) {
                    break;
                }
            }
        }

        String[] prefixes = prefiexesAnnotation.value();
        Map<String, String> prefixNamespaceMap = new HashMap<String, String>();
        for (String prefix : prefixes) {
            int indexOfFirstColon = prefix.indexOf(':');
            String prefixName = prefix.substring(0, indexOfFirstColon);
            String prefixURI = prefix.substring(indexOfFirstColon + 1);
            prefixNamespaceMap.put(prefixName.trim(), prefixURI.trim());
        }
        return prefixNamespaceMap;
    }

    /**
     * get a map but create if not exists
     * @param entity
     * @return
     * @throws RuntimeException
     */
    public static CompChemWrapper getCompChemWrapper(Object entity) throws RuntimeException {
        // get annotated compchem map
        Field[] comchemFields = ReflectionUtils.getAnnotatedDeclaredFields(entity.getClass(), CompChemWrapperField.class);

        // map maybe at its superclass
        if (comchemFields.length == 0) {
            List<Class> allSuperclasses = ClassUtils.getAllSuperclasses(entity.getClass());
            for (Class<?> clz : allSuperclasses) {
                comchemFields = ReflectionUtils.getAnnotatedDeclaredFields(clz, CompChemWrapperField.class);
                if (comchemFields.length > 0) {
                    break;
                }
            }
        }

        if (comchemFields.length != 1) {
            throw new RuntimeException("None or multiple @CompChemWrapperField annotation found.");
        }
        if (!comchemFields[0].getType().equals(CompChemWrapper.class)) {
            throw new RuntimeException("@CompChem must annotate on a field of type CompChemWrapper");
        }
        Object obj = null;
        try {
            comchemFields[0].setAccessible(true);
            obj = comchemFields[0].get(entity);
        } catch (Exception ex) {
        }
        if (obj instanceof CompChemWrapper) {
            return (CompChemWrapper) obj;
        } else {
            return null;
        }
    }

    /**
     * get a map but create if not exists
     * @param entity
     * @param ccw
     * @return
     * @throws RuntimeException
     */
    public static void setCompChemWrapper(Object entity, CompChemWrapper ccw) throws RuntimeException {
        // get annotated compchem map
        Field[] comchemFields = ReflectionUtils.getAnnotatedDeclaredFields(entity.getClass(), CompChemWrapperField.class);

        // map maybe at its superclass
        if (comchemFields.length == 0) {
            List<Class> allSuperclasses = ClassUtils.getAllSuperclasses(entity.getClass());
            for (Class<?> clz : allSuperclasses) {
                comchemFields = ReflectionUtils.getAnnotatedDeclaredFields(clz, CompChemWrapperField.class);
                if (comchemFields.length > 0) {
                    break;
                }
            }
        }

        if (comchemFields.length != 1) {
            throw new RuntimeException("None or multiple @CompChemWrapperField annotation found.");
        }
        if (!comchemFields[0].getType().equals(CompChemWrapper.class)) {
            throw new RuntimeException("@CompChem must annotate on a field of type CompChemWrapper");
        }
        try {
            comchemFields[0].setAccessible(true);
            comchemFields[0].set(entity, ccw);
        } catch (Exception ex) {
            throw new RuntimeException("Unable to set CompChemWrapper", ex);
        }

    }
}
