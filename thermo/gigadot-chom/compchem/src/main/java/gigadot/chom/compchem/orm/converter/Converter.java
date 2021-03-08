package gigadot.chom.compchem.orm.converter;

/**
 *
 * @param <F>
 * @param <T>
 * @author wp214
 */
public interface Converter<F, T extends Number> {
    T from(F obj);
    F to(T obj);
}
