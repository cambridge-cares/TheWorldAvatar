package com.cmclinnovations.mods.modssimpleagent.utils;

import java.util.Comparator;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import com.google.common.collect.Streams;

public final class ListUtils {

    private ListUtils() {
    }

    /**
     * 
     * @param <A>                  Type of the elements in toBeSorted
     * @param <B>                  Type of the elements in definesOrder
     * @param <R>                  Type of the elements to be returned in the
     *                             resultant list
     * @param toBeSorted           Values to be filtered and sorted
     * @param definesOrder         List that contains the required keys in the
     *                             desired order
     * @param keyMappingFunction   Function to map the entries in toBeSorted to
     *                             values comparable with the elements in
     *                             definesOrder
     * @param valueMappingFunction Function to map the entries in toBeSorted to the
     *                             desired results
     * @return
     */
    public static <A, B, R> List<R> filterAndSort(List<A> toBeSorted, List<B> definesOrder,
            Function<A, B> keyMappingFunction, Function<A, R> valueMappingFunction) {
        return toBeSorted.stream()
                .filter(a -> definesOrder.contains(keyMappingFunction.apply(a)))
                .sorted(Comparator.comparingInt(a -> definesOrder.indexOf(keyMappingFunction.apply(a))))
                .map(valueMappingFunction::apply)
                .collect(Collectors.toList());
    }

    /**
     * Replace null entries in baseValues with the corresponding value in
     * fallbackValues
     * 
     * @param <T>            Type of the elements
     * @param baseValues     Primary source of values
     * @param fallbackValues Source of values if equivilent entry in baseValues is
     *                       null
     * @return Copy of baseValues with null entries replaced by corresponding values
     *         from fallbackValues
     */
    public static <T> List<T> replaceNulls(List<T> baseValues, List<T> fallbackValues) {
        return Streams.zip(baseValues.stream(), fallbackValues.stream(),
                (baseValue, fallbackValue) -> (null != baseValue) ? baseValue : fallbackValue)
                .collect(Collectors.toList());
    }
}
