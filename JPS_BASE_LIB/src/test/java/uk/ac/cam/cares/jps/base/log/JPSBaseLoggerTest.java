package uk.ac.cam.cares.jps.base.log;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentMatchers;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class JPSBaseLoggerTest {
    @Test
    public void testLogger() {
        Exception e = new Exception("test exception");
        StackTraceElement[] stackTraceElements = Thread.currentThread().getStackTrace();
        e.setStackTrace(stackTraceElements);
        try (MockedStatic<LoggerFactory> integerMock = Mockito.mockStatic(LoggerFactory.class)) {
            final Logger logger = Mockito.mock(Logger.class);
            integerMock.when(() -> LoggerFactory.getLogger(JPSBaseLogger.class)).thenReturn(logger);

            new JPSBaseLogger().error(JPSBaseLogger.class, e);
            Mockito.verify(logger).error(ArgumentMatchers.any(String.class));

            new JPSBaseLogger().error(JPSBaseLogger.class, "testInfo");
            Mockito.verify(logger).error("Class testInfo");

            new JPSBaseLogger().info(JPSBaseLogger.class, "testInfo");
            Mockito.verify(logger).info("Class testInfo");

            new JPSBaseLogger().debug(JPSBaseLogger.class, "testInfo");
            Mockito.verify(logger).debug("Class testInfo");

            Mockito.verifyNoMoreInteractions(logger);
        }

    }
}