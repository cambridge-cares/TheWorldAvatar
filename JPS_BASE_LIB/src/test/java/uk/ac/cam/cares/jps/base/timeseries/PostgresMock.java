package uk.ac.cam.cares.jps.base.timeseries;

import java.sql.SQLException;

import org.jooq.tools.jdbc.MockDataProvider;
import org.jooq.tools.jdbc.MockExecuteContext;
import org.jooq.tools.jdbc.MockResult;


/**
 * This class mocks the postgres database for unit testing using jOOQ's MockDataProvider
 * @author mh807
 *
 */
public class PostgresMock implements MockDataProvider {

        @Override
        public MockResult[] execute(MockExecuteContext ctx) throws SQLException {
        	
        	// Initialise result: MockResult is used to wrap results of JDBC execution results
        	MockResult[] mock = new MockResult[1];

        	// The execute context contains SQL string(s), bind values, and other meta-data
            String sql = ctx.sql();
            
            // Specify mocked JDBC results depending on query
            if (sql.toUpperCase().startsWith("SELECT COUNT(*)")) {                
            	// Return single empty execution result for all queries counting total number of tables
            	// (i.e. when checking whether central RDB table exists)
            	mock[0] = new MockResult(0);            	
        	} else {
        		// Throw an SQL exception for all other queries
        		throw new SQLException("Mock exception from MockDataProvider");
        	}
        	
            return mock;
        }

}
