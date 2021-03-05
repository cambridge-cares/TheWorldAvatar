package uk.ac.cam.ceb.como.compchem.orm.annotation.processor;

import uk.ac.cam.ceb.como.compchem.orm.annotation.Parameter;
import uk.ac.cam.ceb.como.compchem.xml.NamespaceUtils;
import java.util.Map;
import uk.ac.cam.ceb.como.compchem.CompChem;
import uk.ac.cam.ceb.como.compchem.CompChemElementUtils;
import uk.ac.cam.ceb.como.compchem.CompChemWrapper;
import uk.ac.cam.ceb.como.compchem.orm.annotation.Property;
import java.lang.reflect.Method;
import org.xmlcml.cml.base.CMLElement;
import org.xmlcml.cml.element.CMLArray;
import org.xmlcml.cml.element.CMLModule;
import org.xmlcml.cml.element.CMLParameter;
import org.xmlcml.cml.element.CMLProperty;
import org.xmlcml.cml.element.CMLScalar;
import static uk.ac.cam.ceb.como.compchem.orm.annotation.processor.AnnotationUtils.*;
import static uk.ac.cam.ceb.como.compchem.orm.annotation.processor.ReflectionUtils.*;

/**
 *
 * @param <E>
 * @author pb556
 */
public class CompChemAnnotationProcessor<E> {

    private final CompChemWrapper ccw;
    Map<String, String> prefixNamespaceMap = null;

    public CompChemAnnotationProcessor(CompChem compchem) {
        this.ccw = new CompChemWrapper(compchem);
    }

    public CompChemAnnotationProcessor(CompChemWrapper ccw) {
        this.ccw = ccw;
    }

    public CompChemAnnotationProcessor(CompChem compchem, CMLModule job) {
        this.ccw = new CompChemWrapper(compchem, job);
    }

    public CompChemWrapper getCompChemWrapper() {
        return ccw;
    }

    /**
     *
     * @param entityClass it is required that the entity class contains default constructor.
     *                    entityClass must be a concrete class
     * @return
     */
    public E getEntity(Class<E> entityClass) {
        E entity = null;
        try {
            entity = entityClass.newInstance();
        } catch (Exception ex) {
            throw new RuntimeException("Cannot create an instance of entityClass. Make sure it contains default constructor", ex);
        }
        readEntity(entity);
        return entity;
    }

    public void readEntity(E entity) {
        prefixNamespaceMap = getPrefixNamespaceMap(entity.getClass());
        // need to set the CompChemWrapper to entity if you don't want to get runtime error somewhere else.
        AnnotationUtils.setCompChemWrapper(entity, ccw);

        readCompChemPropertiesToEntity(entity);
        readCompChemParameterToEntity(entity);
    }

    private void readCompChemPropertiesToEntity(E entity) {
        // create and read property
        Method[] prepertyMethods = getAnnotatedMethods(entity.getClass(), Property.class);
        for (Method method : prepertyMethods) {
            Property propertyAnnotation = method.getAnnotation(Property.class);
            ValueAnnotationDescriptor vd = new ValueAnnotationDescriptor(propertyAnnotation);
            String prefixFromQName = NamespaceUtils.getPrefixFromQName(vd.dictRef);
            CMLProperty property = CompChemElementUtils.getProperty(ccw.getJob(), vd.dictRef, prefixNamespaceMap.get(prefixFromQName));
            ValueElementWrapper ve = new ValueElementWrapper(property);
            readCompChemValueToEntity(entity, method, ve, vd);
        }
    }

    private void readCompChemParameterToEntity(E entity) {
        // create and read property
        Method[] parameterMethods = getAnnotatedMethods(entity.getClass(), Parameter.class);
        for (Method method : parameterMethods) {
            Parameter parameterAnnotation = method.getAnnotation(Parameter.class);
            ValueAnnotationDescriptor vd = new ValueAnnotationDescriptor(parameterAnnotation);
            String prefixFromQName = NamespaceUtils.getPrefixFromQName(vd.dictRef);
            CMLParameter parameter = CompChemElementUtils.getParameter(ccw.getJob(), vd.dictRef, prefixNamespaceMap.get(prefixFromQName));
            ValueElementWrapper ve = new ValueElementWrapper(parameter);
            readCompChemValueToEntity(entity, method, ve, vd);
        }
    }

    private void readCompChemValueToEntity(E entity, Method method, ValueElementWrapper ve, ValueAnnotationDescriptor vd) {
        Method setter = getSetter(method);
        Method getter = getGetter(method);
        setter.setAccessible(true);
        getter.setAccessible(true);
        // get the value depending on the container type
        if (!ve.exists()) {
            if (!vd.optional) {
                throw new RuntimeException("A required " + ve.type() + " [" + vd.dictRef + "] is missing from compchem [" + method.getName() + "]");
            }
        } else {
            Class<?> returnType = getter.getReturnType();
            Object invokeParameter = null;
            switch (vd.type) {
                case Scalar:
                    CMLScalar sc = ve.getScalar();
                    if (returnType.equals(double.class)) {
                        invokeParameter = sc.getDouble();
                    } else if (returnType.equals(String.class)) {
                        invokeParameter = sc.getString();
                    } else if (returnType.equals(int.class)) {
                        invokeParameter = sc.getInt();
                    } else if (returnType.equals(boolean.class)) {
                        invokeParameter = sc.getBoolean();
                    } else {
                        throw new RuntimeException("unsupported returning type for a cml scalar : " + returnType.getName());
                    }
                    break;
                case Array:
                    CMLArray ar = ve.getArray();
                    if (returnType.equals(double[].class)) {
                        invokeParameter = ar.getDoubles();
                    } else if (returnType.equals(String[].class)) {
                        invokeParameter = ar.getStrings();
                    } else if (returnType.equals(int[].class)) {
                        invokeParameter = ar.getInts();
                    } else if (returnType.equals(boolean[].class)) {
                        invokeParameter = ar.getBooleans();
                    } else {
                        throw new RuntimeException("unsupported returning type for a cml array : " + returnType.getName());
                    }
                    break;
                case Matrix:
                    throw new UnsupportedOperationException("Matrix is not yet supported");
                //break;
                case Object:
                    throw new UnsupportedOperationException("Object is not yet supported");
                //break;
                default:
                    throw new AssertionError();
            }
            try {
                setter.invoke(entity, invokeParameter);
            } catch (Exception ex) {
                throw new RuntimeException("Exception occurs while invoking setter : " + setter.getName(), ex);
            }
        }
    }

    /**
     * Write target to compchem
     * Every time this method is call it update to the internal compchem object. This does not create a new compchem.
     * so if you invoke this method on different entity it will make compchem bigger.
     *
     * @param entity
     * @return
     */
    
    public CompChem getCompChem(E entity) {
        // get prefix namespace map
        prefixNamespaceMap = getPrefixNamespaceMap(entity.getClass());

        writeCompChemPropertiesFromEntity(entity);
        writeCompChemParametersFromEntity(entity);

        return ccw.getCompchem();
    }

    private void writeCompChemPropertiesFromEntity(E entity) {
        // create and save property
        Method[] propertyMethods = getAnnotatedMethods(entity.getClass(), Property.class);
        for (Method method : propertyMethods) {
            Property propertyAnnotation = method.getAnnotation(Property.class);
            ValueAnnotationDescriptor vd = new ValueAnnotationDescriptor(propertyAnnotation);
            CMLElement container = writeCompChemValueFromEntity(method, entity, vd);
            String prefixFromQName = NamespaceUtils.getPrefixFromQName(vd.dictRef);
            // a property is added before we add the container to property. i'll deal with it later
            // potentially cause an orphan property node.
            CMLProperty p = CompChemElementUtils.getOrAddProperty(ccw.getJob(), vd.dictRef, prefixNamespaceMap.get(prefixFromQName));
            p.removeChildren();
            p.appendChild(container);
        }
    }

    private void writeCompChemParametersFromEntity(E entity) {
        // create and save property
        Method[] parameterMethods = getAnnotatedMethods(entity.getClass(), Parameter.class);
        for (Method method : parameterMethods) {
            Parameter parameterAnnotation = method.getAnnotation(Parameter.class);
            ValueAnnotationDescriptor vd = new ValueAnnotationDescriptor(parameterAnnotation);
            CMLElement container = writeCompChemValueFromEntity(method, entity, vd);
            String prefixFromQName = NamespaceUtils.getPrefixFromQName(vd.dictRef);
            CMLParameter p = CompChemElementUtils.getOrAddParameter(ccw.getJob(), vd.dictRef, prefixNamespaceMap.get(prefixFromQName));
            p.removeChildren();
            p.appendChild(container);

        }
    }

    private CMLElement writeCompChemValueFromEntity(Method method, E entity, ValueAnnotationDescriptor vd) {
        CMLElement container = null;
        // get value container
        Method getter = getGetter(method);
        getter.setAccessible(true);
        Object returningObj = null;
        try {
            returningObj = getter.invoke(entity);
        } catch (Exception ex) {
            throw new RuntimeException("Exception occurs while invoking getter : " + getter.getName(), ex);
        }
        switch (vd.type) {
            case Scalar:
                CMLScalar sc = null;
                if (returningObj instanceof Double) {
                    sc = new CMLScalar((Double) returningObj);
                } else if (returningObj instanceof String) {
                    sc = new CMLScalar((String) returningObj);
                } else if (returningObj instanceof Integer) {
                    sc = new CMLScalar((Integer) returningObj);
                } else if (returningObj instanceof Boolean) {
                    sc = new CMLScalar((Boolean) returningObj);
                } else {
                    throw new RuntimeException("unsupported returning type for a cml scalar : " + returningObj.getClass().getName());
                }
                if (!vd.units.isEmpty()) {
                    sc.setUnits(vd.units);
                }
                container = sc;

                break;
            case Array:
                CMLArray array = null;
                if (returningObj instanceof double[]) {
                    array = new CMLArray((double[]) returningObj);
                } else if (returningObj instanceof String[]) {
                    array = new CMLArray((String[]) returningObj);
                } else if (returningObj instanceof int[]) {
                    array = new CMLArray((int[]) returningObj);
                } else if (returningObj instanceof boolean[]) {
                    array = new CMLArray((boolean[]) returningObj);
                } else {
                    throw new RuntimeException("unsupported returning type for a cml array : " + returningObj.getClass().getName());
                }
                if (!vd.units.isEmpty()) {
                    array.setUnits(vd.units);
                }
                container = array;

                break;
            case Matrix:
                throw new UnsupportedOperationException("Matrix is not yet supported");
            //break;
            case Object:
                throw new UnsupportedOperationException("Object is not yet supported");
            //break;
            default:
                throw new AssertionError();
        }
        return container;
    }
}
