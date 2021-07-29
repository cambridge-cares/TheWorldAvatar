package uk.ac.cam.cares.jps.base.log;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.*;
import org.mockito.junit.MockitoJUnitRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class JPSBaseLoggerTest {

    @Mock
    public static Logger logger = Mockito.mock(Logger.class);
    private JPSBaseLogger mockJPS;

    @Before
    public void construct() {
        try (MockedStatic<LoggerFactory> integerMock = Mockito.mockStatic(LoggerFactory.class)) {
            integerMock.when(() -> LoggerFactory.getLogger(JPSBaseLogger.class)).thenReturn(logger);
            mockJPS = new JPSBaseLogger();
        }
    }

    @Test
    public void testDebug() {
        mockJPS.debug(JPSBaseLogger.class, "test debug");
        Mockito.verify(logger).debug("Class test debug");
    }

    @Test
    public void testInfo() {
        mockJPS.info(JPSBaseLogger.class, "test info");
        Mockito.verify(logger).info("Class test info");

    }


    @Test
    public void testErrorException() {
        Exception e = new Exception("test exception");
        StackTraceElement[] stackTraceElements = Thread.currentThread().getStackTrace();
        e.setStackTrace(stackTraceElements);
        mockJPS.error(JPSBaseLogger.class, e);
        Mockito.verify(logger).error(ArgumentMatchers.any(String.class));
    }

    @Test
    public void testError() {
        mockJPS.error(JPSBaseLogger.class, "test error");
        Mockito.verify(logger).error("Class test error");
    }
}