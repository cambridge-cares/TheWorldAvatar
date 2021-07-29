package uk.ac.cam.cares.jps.base.agent.test;

import junit.framework.TestCase;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;

import javax.servlet.*;
import javax.servlet.http.*;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.security.Principal;
import java.util.Collection;
import java.util.Enumeration;
import java.util.Locale;
import java.util.Map;

public class JPSAgentTest extends TestCase {

    public void testNewPSAgent() {
        JPSAgent jpsa = null;
        try {
            jpsa = new JPSAgent();
        } finally {
            assertNotNull(jpsa);
        }
    }

    public void testNewPSAgentMethods() {
        JPSAgent jpsa = new JPSAgent();
        assertEquals(3, jpsa.getClass().getDeclaredMethods().length);
    }

    public void testProcessRequestParameters() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        JPSAgent jpsa = new JPSAgent();
        assertNotNull(jpsa.getClass().getDeclaredMethod("processRequestParameters", JSONObject.class));
        assertNotNull(jpsa.getClass().getDeclaredMethod("processRequestParameters", JSONObject.class, HttpServletRequest.class));

        Method processRequestParameters = jpsa.getClass().getDeclaredMethod("processRequestParameters", JSONObject.class);
        Method processRequestParametersWithRequest = jpsa.getClass().getDeclaredMethod("processRequestParameters", JSONObject.class, HttpServletRequest.class);

        JSONObject input = new JSONObject();
        HttpServletRequest request = getMockRequest();

        try {
            assertTrue(input.toString().equals(processRequestParameters.invoke(jpsa, input).toString()));
        } catch (InvocationTargetException e) {
            assertEquals("javax.ws.rs.BadRequestException", e.getCause().getCause().getStackTrace()[14].getClassName());
        }

        try {
            assertTrue(input.toString().equals(processRequestParametersWithRequest.invoke(jpsa, input, request).toString()));
        } catch (InvocationTargetException e) {
            assertEquals("javax.ws.rs.BadRequestException", e.getCause().getCause().getStackTrace()[14].getClassName());
        }

    }

    public void testValidateInput() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        JPSAgent jpsa = new JPSAgent();
        assertNotNull(jpsa.getClass().getDeclaredMethod("validateInput", JSONObject.class));
        Method validateInput = jpsa.getClass().getDeclaredMethod("validateInput", JSONObject.class);
        validateInput.setAccessible(true);
        JSONObject input = new JSONObject();

        boolean valid = false;

        try {
            valid = (boolean) validateInput.invoke(jpsa, input);
            assertTrue(valid);
        } catch (InvocationTargetException e) {
            assertEquals("javax.ws.rs.BadRequestException", e.getTargetException().getStackTrace()[6].getClassName());
        }

        input.put("a", "b");

        valid = (boolean) validateInput.invoke(jpsa, input);

        assertTrue(valid);

    }

    public HttpServletRequest getMockRequest() {
        return new HttpServletRequest() {
            @Override
            public String getAuthType() {
                return null;
            }

            @Override
            public Cookie[] getCookies() {
                return new Cookie[0];
            }

            @Override
            public long getDateHeader(String s) {
                return 0;
            }

            @Override
            public String getHeader(String s) {
                return null;
            }

            @Override
            public Enumeration<String> getHeaders(String s) {
                return null;
            }

            @Override
            public Enumeration<String> getHeaderNames() {
                return null;
            }

            @Override
            public int getIntHeader(String s) {
                return 0;
            }

            @Override
            public String getMethod() {
                return null;
            }

            @Override
            public String getPathInfo() {
                return null;
            }

            @Override
            public String getPathTranslated() {
                return null;
            }

            @Override
            public String getContextPath() {
                return null;
            }

            @Override
            public String getQueryString() {
                return null;
            }

            @Override
            public String getRemoteUser() {
                return null;
            }

            @Override
            public boolean isUserInRole(String s) {
                return false;
            }

            @Override
            public Principal getUserPrincipal() {
                return null;
            }

            @Override
            public String getRequestedSessionId() {
                return null;
            }

            @Override
            public String getRequestURI() {
                return null;
            }

            @Override
            public StringBuffer getRequestURL() {
                return null;
            }

            @Override
            public String getServletPath() {
                return null;
            }

            @Override
            public HttpSession getSession(boolean b) {
                return null;
            }

            @Override
            public HttpSession getSession() {
                return null;
            }

            @Override
            public String changeSessionId() {
                return null;
            }

            @Override
            public boolean isRequestedSessionIdValid() {
                return false;
            }

            @Override
            public boolean isRequestedSessionIdFromCookie() {
                return false;
            }

            @Override
            public boolean isRequestedSessionIdFromURL() {
                return false;
            }

            @Override
            public boolean isRequestedSessionIdFromUrl() {
                return false;
            }

            @Override
            public boolean authenticate(HttpServletResponse httpServletResponse) throws IOException, ServletException {
                return false;
            }

            @Override
            public void login(String s, String s1) throws ServletException {

            }

            @Override
            public void logout() throws ServletException {

            }

            @Override
            public Collection<Part> getParts() throws IOException, ServletException {
                return null;
            }

            @Override
            public Part getPart(String s) throws IOException, ServletException {
                return null;
            }

            @Override
            public <T extends HttpUpgradeHandler> T upgrade(Class<T> aClass) throws IOException, ServletException {
                return null;
            }

            @Override
            public Object getAttribute(String s) {
                return null;
            }

            @Override
            public Enumeration<String> getAttributeNames() {
                return null;
            }

            @Override
            public String getCharacterEncoding() {
                return null;
            }

            @Override
            public void setCharacterEncoding(String s) throws UnsupportedEncodingException {

            }

            @Override
            public int getContentLength() {
                return 0;
            }

            @Override
            public long getContentLengthLong() {
                return 0;
            }

            @Override
            public String getContentType() {
                return null;
            }

            @Override
            public ServletInputStream getInputStream() throws IOException {
                return null;
            }

            @Override
            public String getParameter(String s) {
                return null;
            }

            @Override
            public Enumeration<String> getParameterNames() {
                return null;
            }

            @Override
            public String[] getParameterValues(String s) {
                return new String[0];
            }

            @Override
            public Map<String, String[]> getParameterMap() {
                return null;
            }

            @Override
            public String getProtocol() {
                return null;
            }

            @Override
            public String getScheme() {
                return null;
            }

            @Override
            public String getServerName() {
                return null;
            }

            @Override
            public int getServerPort() {
                return 0;
            }

            @Override
            public BufferedReader getReader() throws IOException {
                return null;
            }

            @Override
            public String getRemoteAddr() {
                return null;
            }

            @Override
            public String getRemoteHost() {
                return null;
            }

            @Override
            public void setAttribute(String s, Object o) {

            }

            @Override
            public void removeAttribute(String s) {

            }

            @Override
            public Locale getLocale() {
                return null;
            }

            @Override
            public Enumeration<Locale> getLocales() {
                return null;
            }

            @Override
            public boolean isSecure() {
                return false;
            }

            @Override
            public RequestDispatcher getRequestDispatcher(String s) {
                return null;
            }

            @Override
            public String getRealPath(String s) {
                return null;
            }

            @Override
            public int getRemotePort() {
                return 0;
            }

            @Override
            public String getLocalName() {
                return null;
            }

            @Override
            public String getLocalAddr() {
                return null;
            }

            @Override
            public int getLocalPort() {
                return 0;
            }

            @Override
            public ServletContext getServletContext() {
                return null;
            }

            @Override
            public AsyncContext startAsync() throws IllegalStateException {
                return null;
            }

            @Override
            public AsyncContext startAsync(ServletRequest servletRequest, ServletResponse servletResponse) throws IllegalStateException {
                return null;
            }

            @Override
            public boolean isAsyncStarted() {
                return false;
            }

            @Override
            public boolean isAsyncSupported() {
                return false;
            }

            @Override
            public AsyncContext getAsyncContext() {
                return null;
            }

            @Override
            public DispatcherType getDispatcherType() {
                return null;
            }
        };
    }
}
