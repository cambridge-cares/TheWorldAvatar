package com.cmclinnovations.stack.clients.postgis;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.SQLWarning;
import java.sql.Statement;

import org.junit.Assert;
import org.junit.Test;

public class PostGISClientTest {

    private static final String DATABASE = "test_databse";
    private static final String REASON = "test";

    @Test
    public void testCreateBareDatabase() {
        PostGISClient postGISClient = PostGISClient.getInstance();

        Assert.assertThrows(SQLException.class, () -> postGISClient.createBareDatabase(DATABASE,
                new MockStatement(new SQLException(REASON, "123"), 1)));

        assertDoesNotThrow(() -> postGISClient.createBareDatabase(DATABASE,
                new MockStatement(new SQLException(REASON, "42P04"), 1)));

        assertDoesNotThrow(() -> postGISClient.createBareDatabase(DATABASE,
                new MockStatement(new SQLException(REASON, "42P04"), 1)));

        assertDoesNotThrow(() -> postGISClient.createBareDatabase(DATABASE,
                new MockStatement(new SQLException(REASON, "55006"), 2)));
    }

    private class MockStatement implements Statement {
        SQLException ex;
        int throwTimes;
        int count;

        protected MockStatement(SQLException ex, Integer throwTimes) {
            this.ex = ex;
            this.throwTimes = throwTimes;
            this.count = 0;
        }

        @Override
        public int executeUpdate(String sql) throws SQLException {
            if (ex == null) {
                return 0;
            }
            if (this.count++ < throwTimes) {
                throw ex;
            }

            return 0;
        }

        @Override
        public ResultSet executeQuery(String sql) throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'executeQuery'");
        }

        @Override
        public <T> T unwrap(Class<T> iface) throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'unwrap'");
        }

        @Override
        public boolean isWrapperFor(Class<?> iface) throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'isWrapperFor'");
        }

        @Override
        public void close() throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'close'");
        }

        @Override
        public int getMaxFieldSize() throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'getMaxFieldSize'");
        }

        @Override
        public void setMaxFieldSize(int max) throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'setMaxFieldSize'");
        }

        @Override
        public int getMaxRows() throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'getMaxRows'");
        }

        @Override
        public void setMaxRows(int max) throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'setMaxRows'");
        }

        @Override
        public void setEscapeProcessing(boolean enable) throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'setEscapeProcessing'");
        }

        @Override
        public int getQueryTimeout() throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'getQueryTimeout'");
        }

        @Override
        public void setQueryTimeout(int seconds) throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'setQueryTimeout'");
        }

        @Override
        public void cancel() throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'cancel'");
        }

        @Override
        public SQLWarning getWarnings() throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'getWarnings'");
        }

        @Override
        public void clearWarnings() throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'clearWarnings'");
        }

        @Override
        public void setCursorName(String name) throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'setCursorName'");
        }

        @Override
        public boolean execute(String sql) throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'execute'");
        }

        @Override
        public ResultSet getResultSet() throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'getResultSet'");
        }

        @Override
        public int getUpdateCount() throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'getUpdateCount'");
        }

        @Override
        public boolean getMoreResults() throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'getMoreResults'");
        }

        @Override
        public void setFetchDirection(int direction) throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'setFetchDirection'");
        }

        @Override
        public int getFetchDirection() throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'getFetchDirection'");
        }

        @Override
        public void setFetchSize(int rows) throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'setFetchSize'");
        }

        @Override
        public int getFetchSize() throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'getFetchSize'");
        }

        @Override
        public int getResultSetConcurrency() throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'getResultSetConcurrency'");
        }

        @Override
        public int getResultSetType() throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'getResultSetType'");
        }

        @Override
        public void addBatch(String sql) throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'addBatch'");
        }

        @Override
        public void clearBatch() throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'clearBatch'");
        }

        @Override
        public int[] executeBatch() throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'executeBatch'");
        }

        @Override
        public Connection getConnection() throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'getConnection'");
        }

        @Override
        public boolean getMoreResults(int current) throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'getMoreResults'");
        }

        @Override
        public ResultSet getGeneratedKeys() throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'getGeneratedKeys'");
        }

        @Override
        public int executeUpdate(String sql, int autoGeneratedKeys) throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'executeUpdate'");
        }

        @Override
        public int executeUpdate(String sql, int[] columnIndexes) throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'executeUpdate'");
        }

        @Override
        public int executeUpdate(String sql, String[] columnNames) throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'executeUpdate'");
        }

        @Override
        public boolean execute(String sql, int autoGeneratedKeys) throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'execute'");
        }

        @Override
        public boolean execute(String sql, int[] columnIndexes) throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'execute'");
        }

        @Override
        public boolean execute(String sql, String[] columnNames) throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'execute'");
        }

        @Override
        public int getResultSetHoldability() throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'getResultSetHoldability'");
        }

        @Override
        public boolean isClosed() throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'isClosed'");
        }

        @Override
        public void setPoolable(boolean poolable) throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'setPoolable'");
        }

        @Override
        public boolean isPoolable() throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'isPoolable'");
        }

        @Override
        public void closeOnCompletion() throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'closeOnCompletion'");
        }

        @Override
        public boolean isCloseOnCompletion() throws SQLException {
            throw new UnsupportedOperationException("Unimplemented method 'isCloseOnCompletion'");
        }
    }
}
