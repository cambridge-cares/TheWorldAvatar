package uk.ac.cam.ceb.como.compchem.orm.converter;

/**
 *
 * @param <F>
 * @param <T>
 * @author pb556
 */
public interface Converter<F, T extends Number> {
    T from(F obj);
    F to(T obj);
}
