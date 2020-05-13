package uk.ac.ceb.como.query;

import org.eclipse.rdf4j.IsolationLevels;
import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.QueryLanguage;
import org.eclipse.rdf4j.query.TupleQuery;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;
import org.eclipse.rdf4j.repository.http.HTTPRepository;

public class QueryManager {

	public static String getQuery(String repositoryUrl, String queryString) {
		
		String numberOfGaussianCalculation = new String();
		
		Repository repository = new HTTPRepository(repositoryUrl);

		repository.initialize();

		RepositoryConnection connection = repository.getConnection();

		try {

			connection.begin(IsolationLevels.SNAPSHOT_READ);

			TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);

			TupleQueryResult result = tupleQuery.evaluate();

			try {

				while (result.hasNext()) {

					BindingSet bindingSet = result.next();
					
					numberOfGaussianCalculation =bindingSet.getValue("sum").stringValue();					
				}

			} catch (Exception e) {

				e.getMessage();

			} finally {

				result.close();
			}

			connection.commit();

		} catch (RepositoryException e) {

			e.printStackTrace();

			connection.rollback();

		} finally {

			connection.close();

			repository.shutDown();

		}

		return numberOfGaussianCalculation;

	}
	
}
