package uk.ac.cam.cares.jps.ontomatch.alignment;

import java.util.Iterator;
import java.util.Map;

import org.apache.jena.graph.Triple;
import org.apache.jena.query.ResultSet;

/**
 * Object class
 * Iterator that iterates Jena ResultSet as a list of hashmap
 *
 * @author shaocong zhang
 * @version 1.0
 * @since 2020-09-08
 */
public class ResultSetMapIterator implements Iterator<Map> {
	   private ResultSet rs;
	   private QuerySolutionToMapAdapter ad;
	   private Iterator<Map> it = null;
	   public ResultSetMapIterator(ResultSet resultSet, QuerySolutionToMapAdapter adapter) {
	      this.rs = resultSet;
	      this.ad = adapter;
	   }
	   @Override
	   public boolean hasNext() {
	      if(it != null && it.hasNext()){
	         return true;
	      }
	      it = null;
	      return rs.hasNext();
	   }
	   @Override
	   public Map next() {
	       if(it == null){
	           it = ad.adapt(rs.next());
	       }
	       return it.next();
	   }
	}
