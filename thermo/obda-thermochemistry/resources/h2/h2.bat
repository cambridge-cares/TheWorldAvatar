@java -cp "h2-1.4.190.jar;%H2DRIVERS%;%CLASSPATH%" org.h2.tools.Console %*
@if errorlevel 1 pause