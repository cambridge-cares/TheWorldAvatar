package uk.ac.cam.cares.jps.agent.email.mock;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.security.Principal;
import java.util.Collection;
import java.util.Enumeration;
import java.util.Locale;
import java.util.Map;
import javax.servlet.AsyncContext;
import javax.servlet.DispatcherType;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletInputStream;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.servlet.http.HttpUpgradeHandler;
import javax.servlet.http.Part;

/**
 * This is a mock HttpServletRequest class, it implements almost no functionality and is only used
 * for unit tests with the EmailAgent class.
 *
 * @author Michael Hillman
 */
public class MockHttpServletRequest implements HttpServletRequest {

    @Override
    public String getAuthType() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public Cookie[] getCookies() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public long getDateHeader(String arg0) {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public String getHeader(String arg0) {
        // The EmailAgent is expecting a source IP address here, using "localhost" will
        // validate the request even if whitelist mode is enabled.
        return "localhost";
    }

    @Override
    public Enumeration<String> getHeaders(String arg0) {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public Enumeration<String> getHeaderNames() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public int getIntHeader(String arg0) {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public String getMethod() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public String getPathInfo() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public String getPathTranslated() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public String getContextPath() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public String getQueryString() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public String getRemoteUser() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public boolean isUserInRole(String arg0) {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public Principal getUserPrincipal() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public String getRequestedSessionId() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public String getRequestURI() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public StringBuffer getRequestURL() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public String getServletPath() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public HttpSession getSession(boolean arg0) {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public HttpSession getSession() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public String changeSessionId() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public boolean isRequestedSessionIdValid() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public boolean isRequestedSessionIdFromCookie() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public boolean isRequestedSessionIdFromURL() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public boolean isRequestedSessionIdFromUrl() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public boolean authenticate(HttpServletResponse arg0) throws IOException, ServletException {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public void login(String arg0, String arg1) throws ServletException {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public void logout() throws ServletException {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public Collection<Part> getParts() throws IOException, ServletException {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public Part getPart(String arg0) throws IOException, ServletException {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public <T extends HttpUpgradeHandler> T upgrade(Class<T> arg0) throws IOException, ServletException {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public Object getAttribute(String arg0) {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public Enumeration<String> getAttributeNames() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public String getCharacterEncoding() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public void setCharacterEncoding(String arg0) throws UnsupportedEncodingException {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public int getContentLength() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public long getContentLengthLong() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public String getContentType() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public ServletInputStream getInputStream() throws IOException {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public String getParameter(String arg0) {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public Enumeration<String> getParameterNames() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public String[] getParameterValues(String arg0) {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public Map<String, String[]> getParameterMap() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public String getProtocol() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public String getScheme() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public String getServerName() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public int getServerPort() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public BufferedReader getReader() throws IOException {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public String getRemoteAddr() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public String getRemoteHost() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public void setAttribute(String arg0, Object arg1) {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public void removeAttribute(String arg0) {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public Locale getLocale() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public Enumeration<Locale> getLocales() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public boolean isSecure() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public RequestDispatcher getRequestDispatcher(String arg0) {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public String getRealPath(String arg0) {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public int getRemotePort() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public String getLocalName() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public String getLocalAddr() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public int getLocalPort() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public ServletContext getServletContext() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public AsyncContext startAsync() throws IllegalStateException {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public AsyncContext startAsync(ServletRequest arg0, ServletResponse arg1) throws IllegalStateException {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public boolean isAsyncStarted() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public boolean isAsyncSupported() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public AsyncContext getAsyncContext() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

    @Override
    public DispatcherType getDispatcherType() {
        throw new UnsupportedOperationException("Not supported yet."); 
    }

}
// End of class.