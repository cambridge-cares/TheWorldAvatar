package uk.ac.cam.cares.jps.base.log;

import org.apache.commons.lang3.exception.ExceptionUtils;
import org.junit.*;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.slf4j.Logger;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;

import java.lang.reflect.Field;


public class JPSBaseLoggerTest {

    @Mock
    public static Logger logger = Mockito.mock(Logger.class);

    public static Logger actualLogger;
    private String object;
    private JPSAgent sender;

    @Before
    public void setUp() {
        object = "object";
        sender = new JPSAgent();
    }

    @BeforeClass
    public static void setLoggerToMock() throws NoSuchFieldException, IllegalAccessException {
        Field loggerField = JPSBaseLogger.class.getDeclaredField("logger");
        loggerField.setAccessible(true);
        actualLogger = (Logger) loggerField.get(null);
        loggerField.set(null, logger);
    }

    @AfterClass
    public static void resetLogger() throws NoSuchFieldException, IllegalAccessException {
        Field loggerField = JPSBaseLogger.class.getDeclaredField("logger");
        loggerField.setAccessible(true);
        loggerField.set(null, actualLogger);
        loggerField.setAccessible(false);
    }

    @Test
    public void testDebug() {
        JPSBaseLogger.debug(object, "test debug");
        JPSBaseLogger.debug(sender, "test debug");
        Mockito.verify(logger).debug("String test debug");
        Mockito.verify(logger).debug("JPSAgent test debug");
    }

    @Test
    public void testInfo() {
        JPSBaseLogger.info(object, "test info");
        JPSBaseLogger.info(sender, "test info");
        Mockito.verify(logger).info("String test info");
        Mockito.verify(logger).info("JPSAgent test info");
    }

    @Test
    public void testErrorException() {
        Exception e = new Exception("test exception");
        StackTraceElement[] stackTraceElements = Thread.currentThread().getStackTrace();
        e.setStackTrace(stackTraceElements);
        JPSBaseLogger.error(object, e);
        JPSBaseLogger.error(sender, e);
        Mockito.verify(logger).error("String test exception\n" + ExceptionUtils.getStackTrace(e));
        Mockito.verify(logger).error("JPSAgent test exception\n" + ExceptionUtils.getStackTrace(e));
    }

    @Test
    public void testError() {
        JPSBaseLogger.error(object, "test error");
        JPSBaseLogger.error(sender, "test error");
        Mockito.verify(logger).error("String test error");
        Mockito.verify(logger).error("JPSAgent test error");
    }
}