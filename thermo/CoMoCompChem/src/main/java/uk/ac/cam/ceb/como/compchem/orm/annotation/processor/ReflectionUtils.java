package uk.ac.cam.ceb.como.compchem.orm.annotation.processor;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/**
 *
 * @author pb556
 */

class ReflectionUtils {

    public static Field[] getAnnotatedDeclaredFields(Class clz, Class<? extends Annotation>... annotationClasses) {
        Set<Field> fds = new HashSet<Field>(Arrays.asList(clz.getDeclaredFields()));
        for (Iterator<Field> it = fds.iterator(); it.hasNext();) {
            Field field = it.next();
            // check for given annotations. if a method does not have all the annotation classes
            // we remove the method from the set. whatever left are what we want.
            for (Class<? extends Annotation> annoClz : annotationClasses) {
                if (!field.isAnnotationPresent(annoClz)) {
                    it.remove();
                    break;
                }
            }
        }
        
        return fds.toArray(new Field[fds.size()]);
    }

    public static Field[] getFields(Class clz) {
        Set<Field> fds = new HashSet<Field>();
        fds.addAll(Arrays.asList(clz.getDeclaredFields()));
        fds.addAll(Arrays.asList(clz.getFields()));
        fds.removeAll(Arrays.asList(Object.class.getFields()));
        return fds.toArray(new Field[fds.size()]);
    }
    
    /**
     * get only one annotated method. the returning method is not guaranteed to be the first method in the declaration
     * order. only use this if you are sure that you have only one annotated method per class. null is return if nothing
     * found
     *
     * @param clz
     * @param annotationClasses
     * @return
     */
    
    public static Method getAnnotatedMethod(Class clz, Class<? extends Annotation>... annotationClasses) {
        Method[] annotatedMethods = getAnnotatedMethods(clz, annotationClasses);
        if (annotatedMethods.length > 0) {
            return annotatedMethods[0];
        } else {
            return null;
        }
    }

    /**
     * get all declared methods including the inherited public methods
     * (except those from Object class) that are annotated with a given annotation
     * @param clz
     * @param annotationClasses
     * @return
     */
    
    public static Method[] getAnnotatedMethods(Class clz, Class<? extends Annotation>... annotationClasses) {
        Set<Method> mt = new HashSet<Method>(Arrays.asList(getMethods(clz)));
        for (Iterator<Method> it = mt.iterator(); it.hasNext();) {
            Method method = it.next();
            // check for given annotations. if a method does not have all the annotation classes
            // we remove the method from the set. whatever left are what we want.
            for (Class<? extends Annotation> annoClz : annotationClasses) {
                if (!method.isAnnotationPresent(annoClz)) {
                    it.remove();
                    break;
                }
            }
        }
        return mt.toArray(new Method[mt.size()]);
    }

    /**
     * get all declared methods including the inherited public methods
     * except those from Object class
     * @param clz
     * @return
     */
    
    public static Method[] getMethods(Class clz) {
        Set<Method> mt = new HashSet<Method>();
        // add declared methods first
        mt.addAll(Arrays.asList(clz.getDeclaredMethods()));
        // add method and its inhertited methods later in case that they are inaccessible.
        mt.addAll(Arrays.asList(clz.getMethods()));
        mt.removeAll(Arrays.asList(Object.class.getMethods()));
        return mt.toArray(new Method[mt.size()]);
    }

    public static boolean isGetter(Method method) {
        return method.getName().startsWith("get") && method.getParameterTypes().length == 0;
    }

    public static boolean isSetter(Method method) {
        // is a setter if begin with set and its getter return the same type as its parameter
        return method.getName().startsWith("set") && method.getParameterTypes().length == 1;
    }

    public static String getSetterName(Method method) {
        if (isSetter(method)) {
            return method.getName();
        } else if (isGetter(method)) {
            return "set" + method.getName().substring(3);
        } else {
            return null;
        }
    }

    public static String getGetterName(Method method) {
        if (isGetter(method)) {
            return method.getName();
        } else if (isSetter(method)) {
            return "get" + method.getName().substring(3);
        } else {
            return null;
        }
    }

    public static Method getGetter(Method method) {
        try {
            String getterName = getGetterName(method);
            return method.getDeclaringClass().getMethod(getterName);
        } catch (Exception ex) {
            return null;
        }
    }

    public static Method getSetter(Method method) {
        try {
            String setterName = getSetterName(method);
            Method getter = getGetter(method);
            return method.getDeclaringClass().getMethod(setterName, getter.getReturnType());
        } catch (Exception ex) {
            return null;
        }
    }
}