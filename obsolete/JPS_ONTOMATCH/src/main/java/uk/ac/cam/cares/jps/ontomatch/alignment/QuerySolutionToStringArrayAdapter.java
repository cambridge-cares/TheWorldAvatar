package uk.ac.cam.cares.jps.ontomatch.alignment;

import java.util.Iterator;

import org.apache.jena.query.QuerySolution;
/**
 * Adaptor Interface that converts a Jena query solution to a string array, which is less memory costly
 *
 *
 * @author shaocong zhang
 * @version 1.0
 * @since 2020-09-08
 */

public interface QuerySolutionToStringArrayAdapter {
    public Iterator<String[]> adapt(QuerySolution qs);

}
