/*
 * ====================================================================
 * Project:     openMDX, http://www.openmdx.org/
 * Name:        $Id: ServletPort.java,v 1.23 2012/07/08 13:41:35 wfro Exp $
 * Description: ServletPort 
 * Revision:    $Revision: 1.23 $
 * Owner:       OMEX AG, Switzerland, http://www.omex.ch
 * Date:        $Date: 2012/07/08 13:41:35 $
 * ====================================================================
 *
 * This software is published under the BSD license as listed below.
 * 
 * Copyright (c) 2010-2012, OMEX AG, Switzerland
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or
 * without modification, are permitted provided that the following
 * conditions are met:
 * 
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * 
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in
 *   the documentation and/or other materials provided with the
 *   distribution.
 * 
 * * Neither the name of the openMDX team nor the names of its
 *   contributors may be used to endorse or promote products derived
 *   from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
 * CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 * 
 * ------------------
 * 
 * This product includes software developed by other organizations as
 * listed in the NOTICE file.
 */
package test.org.opencrx.generic;
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.security.Principal;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import javax.resource.ResourceException;
import javax.resource.cci.Interaction;
import javax.servlet.AsyncContext;
import javax.servlet.DispatcherType;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletInputStream;
import javax.servlet.ServletOutputStream;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.servlet.http.Part;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import org.openmdx.application.rest.http.AbstractHttpInteraction;
import org.openmdx.application.rest.http.servlet.RestServlet_2;
import org.openmdx.application.rest.http.spi.Message;
import org.openmdx.base.exception.RuntimeServiceException;
import org.openmdx.base.io.HttpHeaderFieldContent;
import org.openmdx.base.io.HttpHeaderFieldValue;
import org.openmdx.base.naming.Path;
import org.openmdx.base.resource.InteractionSpecs;
import org.openmdx.base.resource.spi.Port;
import org.openmdx.base.resource.spi.RestInteractionSpec;
import org.openmdx.base.rest.cci.RestConnection;
import org.openmdx.base.rest.spi.RestSource;
import org.openmdx.base.rest.stream.RestTarget;
import org.openmdx.base.rest.stream.StandardRestFormatter;
import org.openmdx.base.text.conversion.URITransformation;
import org.openmdx.kernel.exception.BasicException;
import org.openmdx.kernel.id.UUIDs;
import org.w3c.cci2.CharacterLargeObjects;
import org.xml.sax.InputSource;
/**
 * Servlet Port
 */
public class ServletPort implements Port<RestConnection>
{
    /**
     * Constructor 
     *
     * @param initParameters
     * @throws ServletException
     */
    public ServletPort(
        final Map<String,String> initParameters
    ) throws ServletException {
        this.servlet.init(
            new ServletConfig(){
                public String getInitParameter(String name) {
                    return initParameters.get(name);
                }
                @SuppressWarnings("rawtypes")
                public Enumeration getInitParameterNames() {
                    return Collections.enumeration(initParameters.keySet());
                }
                public ServletContext getServletContext() {
                    return null;
                }
                public String getServletName() {
                    return ServletPort.class.getSimpleName();
                }
            }
        );
    }
    /**
     * 
     */
    protected final HttpServlet servlet = new RestServlet_2();
    /**
     * 
     */
    protected static final InteractionSpecs connectionInteractionSpecs = InteractionSpecs.getRestInteractionSpecs(false);
    /**
     * The path to create (virtual) connection objects
     */
    protected static final Path CONNECTION_PATH = new Path("xri://@openmdx*org.openmdx.kernel/connection");
    /**
     * The MIME type to be used<ul>
     * <li>application/vnd.openmdx.wbxml
     * <li>application/xml
     * <li>text/xml
     * </li>
     */
    private static final String MIME_TYPE = "application/vnd.openmdx.wbxml";
    /**
     * 
     */
    protected static String[] NO_VALUES = {};
    /* (non-Javadoc)
     * @see org.openmdx.base.resource.spi.Port#getInteraction(javax.resource.cci.Connection)
     */
    
    @Override
    public Interaction getInteraction(
        RestConnection connection
    ) throws ResourceException {
        return new ServletInteraction(connection);
    }
    
    //------------------------------------------------------------------------
    // Class ServletInteraction
    //------------------------------------------------------------------------
    /**
     * Servlet Interaction
     */
    class ServletInteraction extends AbstractHttpInteraction {
        protected final EmbeddedSession session = new EmbeddedSession();
        protected final String remoteUser = System.getProperty("user.name", "ServletPort");
        protected static final String CONTEXT_PATH = "/ServletPort";
        /**
         * Constructor 
         *
         * @param connection
         * 
         * @throws ServletException 
         */
        protected ServletInteraction(
            RestConnection connection
        ) throws ResourceException {
            super(
                connection,
                "http://test.openmdx.org" + CONTEXT_PATH
            );
        }
        /* (non-Javadoc)
         * @see org.openmdx.base.resource.spi.AbstractInteraction#open()
         */
        @Override
        @SuppressWarnings("rawtypes")
        protected void open(
        ) throws ResourceException {
            ServletMessage message = new ServletMessage(CONNECT_SPEC, CONNECT_XRI);
            Map parameters = message.request.getParameterMap();
            parameters.put("UserName", new String[]{getConnectionUserName()});
            try {
                message.execute();
            } catch (ResourceException exception) {
                throw new ResourceException(exception);
            }
        }
        /* (non-Javadoc)
         * @see org.openmdx.application.rest.http.AbstractHttpInteraction#newMessage(org.openmdx.base.resource.spi.RestInteractionSpec, org.openmdx.base.naming.Path)
         */
        @Override
        protected Message newMessage(
            RestInteractionSpec interactionSpec,
            Path xri
         ){
            return new ServletMessage(
                interactionSpec,
                xri
            );
        }
        /**
         * Embedded Session
         */
        class EmbeddedSession implements HttpSession {
            long accessed = -1;
            long created = System.currentTimeMillis();
            String id = UUIDs.newUUID().toString();
            private final Map<String,Object> attributes = new HashMap<String,Object>();
            private void validate(){
                if(created < 0) {
                    throw new IllegalStateException("The sesson is invalidated");
                }
            }
            /* (non-Javadoc)
             * @see javax.servlet.http.HttpSession#getAttribute(java.lang.String)
             */
            public Object getAttribute(String name) {
                validate();
                return this.attributes.get(name);
            }
            /* (non-Javadoc)
             * @see javax.servlet.http.HttpSession#getAttributeNames()
             */
            @SuppressWarnings("rawtypes")
            public Enumeration getAttributeNames() {
                validate();
                return Collections.enumeration(this.attributes.keySet());
            }
            /* (non-Javadoc)
             * @see javax.servlet.http.HttpSession#getCreationTime()
             */
            public long getCreationTime() {
                validate();
                return this.created;
            }
            /* (non-Javadoc)
             * @see javax.servlet.http.HttpSession#getId()
             */
            public String getId() {
                return this.id;
            }
            /* (non-Javadoc)
             * @see javax.servlet.http.HttpSession#getLastAccessedTime()
             */
            public long getLastAccessedTime() {
                return this.accessed;
            }
            /* (non-Javadoc)
             * @see javax.servlet.http.HttpSession#getMaxInactiveInterval()
             */
            public int getMaxInactiveInterval() {
                throw new UnsupportedOperationException();
            }
            /* (non-Javadoc)
             * @see javax.servlet.http.HttpSession#getServletContext()
             */
            public ServletContext getServletContext() {
                throw new UnsupportedOperationException();
            }
            /**
             * @deprecated
             */
            @SuppressWarnings("dep-ann")
            public javax.servlet.http.HttpSessionContext getSessionContext() {
                throw new UnsupportedOperationException();
            }
            /* (non-Javadoc)
             * @see javax.servlet.http.HttpSession#getValue(java.lang.String)
             */
            public Object getValue(String name) {
                throw new UnsupportedOperationException();
            }
            /* (non-Javadoc)
             * @see javax.servlet.http.HttpSession#getValueNames()
             */
            public String[] getValueNames() {
                throw new UnsupportedOperationException();
            }
            /* (non-Javadoc)
             * @see javax.servlet.http.HttpSession#invalidate()
             */
            public void invalidate() {
                this.created = -1;
            }
            /* (non-Javadoc)
             * @see javax.servlet.http.HttpSession#isNew()
             */
            public boolean isNew() {
                return this.accessed < 0;
            }
            /* (non-Javadoc)
             * @see javax.servlet.http.HttpSession#putValue(java.lang.String, java.lang.Object)
             */
            public void putValue(String name, Object value) {
                throw new UnsupportedOperationException();
            }
            /* (non-Javadoc)
             * @see javax.servlet.http.HttpSession#removeAttribute(java.lang.String)
             */
            public void removeAttribute(String name) {
                validate();
                this.attributes.remove(name);
            }
            /* (non-Javadoc)
             * @see javax.servlet.http.HttpSession#removeValue(java.lang.String)
             */
            public void removeValue(String name) {
                throw new UnsupportedOperationException();
            }
            /* (non-Javadoc)
             * @see javax.servlet.http.HttpSession#setAttribute(java.lang.String, java.lang.Object)
             */
            public void setAttribute(String name, Object value) {
                validate();
                this.attributes.put(name, value);
            }
            /* (non-Javadoc)
             * @see javax.servlet.http.HttpSession#setMaxInactiveInterval(int)
             */
            public void setMaxInactiveInterval(int interval) {
                throw new UnsupportedOperationException();
            }
        }
        /**
         * ServletTarget
         */
        class ServletTarget extends RestTarget {
            /**
             * Constructor 
             *
             * @param contextURL
             */
            protected ServletTarget(
                String contextURL
            ) {
                super(contextURL);
            }
            /**
             * 
             */
            protected final EmbeddedBody body = new EmbeddedBody();
            /* (non-Javadoc)
             * @see org.openmdx.application.rest.http.RestFormat.Target#newWriter()
             */
            @Override
            protected XMLStreamWriter newWriter(
            ) throws XMLStreamException {
                XMLOutputFactory xmlOutputFactory;
                try {
                    xmlOutputFactory = ((StandardRestFormatter)restFormatter).getOutputFactory(MIME_TYPE);
                } catch (BasicException exception) {
                    throw new XMLStreamException(exception);
                }
                return MIME_TYPE.endsWith("/xml") ? xmlOutputFactory.createXMLStreamWriter(
                    this.body.getCharacterSink()
                ) : xmlOutputFactory.createXMLStreamWriter(
                    this.body.getBinarySink()
                );
            }
        }
        /**
         * Servlet Message
         */
        class ServletMessage implements Message {
            /**
             * Interaction Message Constructor
             *
             * @param interactionSpec
             * @param xri
             */
            ServletMessage(
                RestInteractionSpec interactionSpec,
                Path xri
            ){
                this.interactionSpec = interactionSpec;
                String servletPath = xri.toXRI();
                this.servletPath = '/' + servletPath.substring(
                    servletPath.charAt(14) == '!' ? 14 : 15
                );
                if(interactionSpec == DELETE_SPEC) {
                    this.request.getParameterMap().put("FunctionName", new String[]{"DELETE"});
                }
            }
            protected final RestInteractionSpec interactionSpec;
            protected final String servletPath;
            protected final ServletTarget outputTarget = new ServletTarget(ServletInteraction.this.contextURL);
            private final EmbeddedRequest request = new EmbeddedRequest();
            private final EmbeddedResponse response = new EmbeddedResponse();
            /* (non-Javadoc)
             * @see org.openmdx.application.rest.http.AbstractHttpInteraction.Message#execute()
             */
            public int execute(
            ) throws ResourceException {
                try {
                    setRequestField("Content-Type", MIME_TYPE + ";charset=UTF-8");
                    setRequestField("Accept", MIME_TYPE);
                    setRequestField("Accept-Charset", "UTF-8");
                    setRequestField("interaction-verb", Integer.toString(this.interactionSpec.getInteractionVerb()));
                    this.outputTarget.close();
                    ServletPort.this.servlet.service(this.request, this.response);
                    this.response.commit();
                } catch (ServletException exception) {
                    throw new ResourceException(exception);
                } catch (IOException exception) {
                    throw new ResourceException(exception);
                } finally {
                    ServletInteraction.this.session.accessed = System.currentTimeMillis();
                }
                return this.response.status;
            }
            /* (non-Javadoc)
             * @see org.openmdx.application.rest.http.AbstractHttpInteraction.Message#getRequestBody()
             */
            public RestTarget getRequestBody() {
                return this.outputTarget;
            }
            /* (non-Javadoc)
             * @see org.openmdx.application.rest.http.AbstractHttpInteraction.Message#getResponseBody()
             */
            public RestSource getResponseBody(
            ) throws ResourceException {
                InputSource source = this.response.body.getInputSource();
                return new RestSource(
                    ServletInteraction.this.contextURL,
                    source,
                    MIME_TYPE,
                    null
                );
            }
            /* (non-Javadoc)
             * @see org.openmdx.application.rest.http.AbstractHttpInteraction.Message#getResponseField(java.lang.String)
             */
            public String getResponseField(String key) {
                return this.response.headers.get(key);
            }
            /* (non-Javadoc)
             * @see org.openmdx.application.rest.http.AbstractHttpInteraction.Message#setRequestField(java.lang.String, java.lang.String)
             */
            public void setRequestField(String key, String value) {
                this.request.headers.put(key.toLowerCase(), value);
            }
            class EmbeddedRequest implements HttpServletRequest {
                private final Map<String,String> headers = new HashMap<String,String>();
                private final Map<String,String[]> parameters = new HashMap<String,String[]>();
            //  @Override
                public String getAuthType() {
                    throw new UnsupportedOperationException();
                }
            //  @Override
                public String getContextPath() {
                   return CONTEXT_PATH;
                }
            //  @Override
                public Cookie[] getCookies() {
                    throw new UnsupportedOperationException();
                }
            //  @Override
                public long getDateHeader(String name) {
                    return Long.parseLong(getHeader(name));
                }
            //  @Override
                public String getHeader(String name) {
                    return this.headers.get(name.toLowerCase());
                }
            //  @Override
                @SuppressWarnings("rawtypes")
                public Enumeration getHeaderNames() {
                    return Collections.enumeration(
                        this.headers.keySet()
                    );
                }
            //  @Override
                @SuppressWarnings("rawtypes")
                public Enumeration getHeaders(String name) {
                    String header = this.headers.get(name.toLowerCase());
                    final String[] values = header == null ? NO_VALUES : header.split(",");
                    return new Enumeration(){
                        private int cursor = 0;
                    //  @Override
                        public boolean hasMoreElements() {
                            return this.cursor < values.length;
                        }
                    //  @Override
                        public Object nextElement() {
                            return values[this.cursor++].trim();
                        }
                    };
                }
            //  @Override
                public int getIntHeader(String name) {
                    return Integer.parseInt(getHeader(name));
                }
            //  @Override
                public String getMethod() {
                    return ServletMessage.this.interactionSpec.getFunctionName();
                }
            //  @Override
                public String getPathInfo() {
                    return null; // Assume default servlet mapping ("/")
                }
            //  @Override
                public String getPathTranslated() {
                    throw new UnsupportedOperationException();
                }
            //  @Override
                public String getQueryString() {
                    return null;
                }
            //  @Override
                public String getRemoteUser() {
                    return remoteUser;
                }
            //  @Override
                public String getRequestURI() {
                    return getContextPath() + URITransformation.encode(ServletMessage.this.servletPath);
                }
            //  @Override
                public StringBuffer getRequestURL() {
                    return new StringBuffer(
                        ServletInteraction.this.contextURL
                    ).append(
                        URITransformation.encode(ServletMessage.this.servletPath)
                    );
                }
            //  @Override
                public String getRequestedSessionId() {
                    throw new UnsupportedOperationException();
                }
            //  @Override
                public String getServletPath() {
                    return ServletMessage.this.servletPath;
                }
            //  @Override
                public HttpSession getSession() {
                    return getSession(true);
                }
            //  @Override
                public HttpSession getSession(boolean create) {
                    return ServletInteraction.this.session;
                }
            //  @Override
                public Principal getUserPrincipal() {
                    throw new UnsupportedOperationException();
                }
            //  @Override
                public boolean isRequestedSessionIdFromCookie() {
                    throw new UnsupportedOperationException();
                }
            //  @Override
                public boolean isRequestedSessionIdFromURL() {
                    throw new UnsupportedOperationException();
                }
            //  @Override
                public boolean isRequestedSessionIdFromUrl() {
                    throw new UnsupportedOperationException();
                }
            //  @Override
                public boolean isRequestedSessionIdValid() {
                    throw new UnsupportedOperationException();
                }
            //  @Override
                public boolean isUserInRole(String role) {
                    throw new UnsupportedOperationException();
                }
            //  @Override
                public Object getAttribute(String name) {
                    throw new UnsupportedOperationException();
                }
            //  @Override
                @SuppressWarnings("rawtypes")
                public Enumeration getAttributeNames() {
                    throw new UnsupportedOperationException();
                }
            //  @Override
                public String getCharacterEncoding() {
                    HttpHeaderFieldContent contentType = new HttpHeaderFieldValue(
                        request.getHeaders("Content-Type")
                    ).getPreferredContent(
                        null
                    );
                    if(contentType == null) {
                        return null;
                    } else {
                        String characterEncoding = contentType.getParameterValue("charset", null);
                        if(characterEncoding == null) {
                            String mimeType = contentType.getValue().toLowerCase();
                            if(mimeType.startsWith("text/")) {
                                return mimeType.equals("text/xml") ? "us-ascii" : "iso-8859-1";
                            } else {
                                return null;
                            }
                        } else {
                            return characterEncoding;
                        }
                    }
                }
            //  @Override
                public int getContentLength() {
                    String contentLength = headers.get("content-length");
                    try {
                        return contentLength == null ? -1 : Integer.parseInt(contentLength);
                    } catch (NumberFormatException exception) {
                        return -1;
                    }
                }
            //  @Override
                public String getContentType() {
                    return request.getHeader("Content-Type");
                }
            //  @Override
                public ServletInputStream getInputStream(
                ) throws IOException {
                    return ServletMessage.this.outputTarget.body.getBinarySource();
                }
            //  @Override
                public String getLocalAddr(
                ) {
                    throw new UnsupportedOperationException();
                }
            //  @Override
                public String getLocalName() {
                    throw new UnsupportedOperationException();
                }
            //  @Override
                public int getLocalPort() {
                    throw new UnsupportedOperationException();
                }
            //  @Override
                public Locale getLocale() {
                    throw new UnsupportedOperationException();
                }
            //  @Override
                @SuppressWarnings("rawtypes")
                public Enumeration getLocales() {
                    throw new UnsupportedOperationException();
                }
            //  @Override
                public String getParameter(String name) {
                    String[] values = getParameterValues(name);
                    return values == null || values.length == 0 ? null : values[0];
                }
            //  @Override
                @SuppressWarnings("rawtypes")
                public Map getParameterMap() {
                    return this.parameters;
                }
            //  @Override
                @SuppressWarnings("rawtypes")
                public Enumeration getParameterNames() {
                    return Collections.enumeration(this.parameters.keySet());
                }
            //  @Override
                public String[] getParameterValues(String name) {
                    return this.parameters.get(name);
                }
            //  @Override
                public String getProtocol() {
                    throw new UnsupportedOperationException();
                }
            //  @Override
                public BufferedReader getReader(
                ) throws IOException {
                    return new BufferedReader(
                        ServletMessage.this.outputTarget.body.getCharacterSource()
                    );
                }
            //  @Override
                public String getRealPath(
                    String path
                ) {
                    throw new UnsupportedOperationException();
                }
            //  @Override
                public String getRemoteAddr(
                ) {
                    throw new UnsupportedOperationException();
                }
            //  @Override
                public String getRemoteHost(
                ) {
                    throw new UnsupportedOperationException();
                }
            //  @Override
                public int getRemotePort(
                ) {
                    throw new UnsupportedOperationException();
                }
            //  @Override
                public RequestDispatcher getRequestDispatcher(
                    String path
                ) {
                    throw new UnsupportedOperationException();
                }
            //  @Override
                public String getScheme(
                ) {
                    throw new UnsupportedOperationException();
                }
            //  @Override
                public String getServerName(
                ) {
                    throw new UnsupportedOperationException();
                }
            //  @Override
                public int getServerPort(
                ) {
                    throw new UnsupportedOperationException();
                }
            //  @Override
                public boolean isSecure(
                ) {
                    throw new UnsupportedOperationException();
                }
            //  @Override
                public void removeAttribute(
                    String name
                ) {
                    throw new UnsupportedOperationException();
                }
            //  @Override
                public void setAttribute(
                    String name,
                    Object o
                ) {
                    throw new UnsupportedOperationException();
                }
            //  @Override
                public void setCharacterEncoding(
                    String env
                ) throws UnsupportedEncodingException {
                    throw new UnsupportedOperationException();
                }
                /* (non-Javadoc)
                 * @see javax.servlet.ServletRequest#getAsyncContext()
                 */
                @Override
                public AsyncContext getAsyncContext() {
                    throw new UnsupportedOperationException();
                }
                /* (non-Javadoc)
                 * @see javax.servlet.ServletRequest#getDispatcherType()
                 */
                @Override
                public DispatcherType getDispatcherType() {
                    throw new UnsupportedOperationException();
                }
                /* (non-Javadoc)
                 * @see javax.servlet.ServletRequest#getServletContext()
                 */
                @Override
                public ServletContext getServletContext() {
                    throw new UnsupportedOperationException();
                }
                /* (non-Javadoc)
                 * @see javax.servlet.ServletRequest#isAsyncStarted()
                 */
                @Override
                public boolean isAsyncStarted() {
                    throw new UnsupportedOperationException();
                }
                /* (non-Javadoc)
                 * @see javax.servlet.ServletRequest#isAsyncSupported()
                 */
                @Override
                public boolean isAsyncSupported() {
                    throw new UnsupportedOperationException();
                }
                /* (non-Javadoc)
                 * @see javax.servlet.ServletRequest#startAsync()
                 */
                @Override
                public AsyncContext startAsync()
                    throws IllegalStateException {
                    throw new UnsupportedOperationException();
                }
                /* (non-Javadoc)
                 * @see javax.servlet.ServletRequest#startAsync(javax.servlet.ServletRequest, javax.servlet.ServletResponse)
                 */
                @Override
                public AsyncContext startAsync(
                    ServletRequest arg0,
                    ServletResponse arg1)
                    throws IllegalStateException {
                    throw new UnsupportedOperationException();
                }
                /* (non-Javadoc)
                 * @see javax.servlet.http.HttpServletRequest#authenticate(javax.servlet.http.HttpServletResponse)
                 */
                @Override
                public boolean authenticate(HttpServletResponse arg0)
                    throws IOException, ServletException {
                    throw new UnsupportedOperationException();
                }
                /* (non-Javadoc)
                 * @see javax.servlet.http.HttpServletRequest#getPart(java.lang.String)
                 */
                @Override
                public Part getPart(String arg0)
                    throws IOException, ServletException {
                    throw new UnsupportedOperationException();
                }
                /* (non-Javadoc)
                 * @see javax.servlet.http.HttpServletRequest#getParts()
                 */
                @Override
                public Collection<Part> getParts()
                    throws IOException, ServletException {
                    throw new UnsupportedOperationException();
                }
                /* (non-Javadoc)
                 * @see javax.servlet.http.HttpServletRequest#login(java.lang.String, java.lang.String)
                 */
                @Override
                public void login(String arg0, String arg1)
                    throws ServletException {
                    throw new UnsupportedOperationException();
                }
                /* (non-Javadoc)
                 * @see javax.servlet.http.HttpServletRequest#logout()
                 */
                @Override
                public void logout()
                    throws ServletException {
                    throw new UnsupportedOperationException();
                }
            }
            class EmbeddedResponse implements HttpServletResponse {
                protected final Map<String,String> headers = new HashMap<String,String>();
                boolean committed = false;
                int status = HttpServletResponse.SC_OK;
                protected final EmbeddedBody body = new EmbeddedBody();
                private ServletOutputStream binarySink ;
                private PrintWriter characterSink;
                private String contentType;
                private String characterEncoding;
                @SuppressWarnings("unused")
                private int bufferSize = 8192;
                private Locale locale = Locale.getDefault();
                protected void commit(
                ) throws IOException{
                    this.flushBuffer();
                    this.committed = true;
                }
                public void addCookie(
                    Cookie cookie
                ) {
                    throw new UnsupportedOperationException();
                }
                public void addDateHeader(
                    String name,
                    long date
                ) {
                    addHeader(name, Long.toString(date));
               }
                public void addHeader(
                    String name,
                    String value
                ) {
                    this.headers.put(name, value);
                }
                public void addIntHeader(String name, int value) {
                    addHeader(name, Integer.toString(value));
                }
                public boolean containsHeader(String name) {
                    return this.headers.containsKey(name);
                }
                public String encodeRedirectURL(String url) {
                    throw new UnsupportedOperationException();
                }
                public String encodeRedirectUrl(String url) {
                    throw new UnsupportedOperationException();
                }
                public String encodeURL(String url) {
                    throw new UnsupportedOperationException();
                }
                public String encodeUrl(String url) {
                    throw new UnsupportedOperationException();
                }
                public void sendError(
                    int sc
                ) throws IOException {
                    setStatus(sc);
                    commit();
                }
                public void sendError(
                    int sc,
                    String msg
                ) throws IOException {
                    sendError(sc);
                }
                public void sendRedirect(String location)
                    throws IOException {
                    throw new UnsupportedOperationException();
                }
                public void setDateHeader(String name, long date) {
                    throw new UnsupportedOperationException();
                }
                public void setHeader(String name, String value) {
                    throw new UnsupportedOperationException();
                }
                public void setIntHeader(String name, int value) {
                    throw new UnsupportedOperationException();
                }
                public void setStatus(int sc) {
                    this.status = sc;
                }
                public void setStatus(int sc, String sm) {
                    setStatus(sc);
                }
                public void flushBuffer(
                ) throws IOException {
                    if(this.characterSink != null) {
                        this.characterSink.flush();
                    } else if (this.binarySink != null) {
                        this.binarySink.flush();
                    }
                }
                public int getBufferSize() {
                    throw new UnsupportedOperationException();
                }
                public String getCharacterEncoding() {
                    return this.characterEncoding;
                }
                public String getContentType() {
                    return this.contentType;
                }
                public Locale getLocale() {
                    return this.locale;
                }
                public ServletOutputStream getOutputStream(
                ) throws IOException {
                    if(this.characterSink != null) throw new IllegalStateException(
                        "Use either a binary sink or a caharcter sink but not both"
                    );
                    if(this.binarySink == null) {
                        this.binarySink = new ServletOutputStream(){
                            private final OutputStream delegate = EmbeddedResponse.this.body.getBinarySink();
                            @Override
                            public void write(
                                int b
                            ) throws IOException {
                                this.delegate.write(b);
                            }
                            /**
                             * @throws IOException
                             * @see java.io.OutputStream#flush()
                             */
                            @Override
                            public void flush(
                            ) throws IOException {
                                this.delegate.flush();
                            }
                            /**
                             * @param b
                             * @param off
                             * @param len
                             * @throws IOException
                             * @see java.io.OutputStream#write(byte[], int, int)
                             */
                            @Override
                            public void write(byte[] b, int off, int len)
                                throws IOException {
                                this.delegate.write(b, off, len);
                            }
                            /**
                             * @param b
                             * @throws IOException
                             * @see java.io.OutputStream#write(byte[])
                             */
                            @Override
                            public void write(byte[] b)
                                throws IOException {
                                this.delegate.write(b);
                            }
                        };
                    }
                    return this.binarySink;
                }
                public PrintWriter getWriter(
                ) throws IOException {
                    if(this.binarySink != null) throw new IllegalStateException(
                        "Use either a binary sink or a caharcter sink but not both"
                    );
                    if(this.characterSink == null) {
                        Writer delegate = EmbeddedResponse.this.body.isBinary() ?
                            new OutputStreamWriter(EmbeddedResponse.this.body.getBinarySink(), this.getCharacterEncoding()) :
                            EmbeddedResponse.this.body.getCharacterSink();
                        this.characterSink = new PrintWriter(delegate);
                    }
                    return this.characterSink;
                }
                public boolean isCommitted() {
                    return this.committed;
                }
                public void reset() {
                    resetBuffer();
                    this.headers.clear();
                }
                public void resetBuffer() {
                    this.body.reset();
                }
                public void setBufferSize(int size) {
                    this.bufferSize = size;
                }
                public void setCharacterEncoding(String charset) {
                    this.characterEncoding = charset;
                }
                public void setContentLength(int len) {
                    addIntHeader("Content-Length", len);
                }
                public void setContentType(String type) {
                    this.contentType = type;
                }
                public void setLocale(Locale loc) {
                    this.locale = loc;
                }
                /* (non-Javadoc)
                 * @see javax.servlet.http.HttpServletResponse#getHeader(java.lang.String)
                 */
                @Override
                public String getHeader(String arg0) {
                    throw new UnsupportedOperationException();
                }
                /* (non-Javadoc)
                 * @see javax.servlet.http.HttpServletResponse#getHeaderNames()
                 */
                @Override
                public Collection<String> getHeaderNames() {
                    throw new UnsupportedOperationException();
                }
                /* (non-Javadoc)
                 * @see javax.servlet.http.HttpServletResponse#getHeaders(java.lang.String)
                 */
                @Override
                public Collection<String> getHeaders(String arg0) {
                    throw new UnsupportedOperationException();
                }
                /* (non-Javadoc)
                 * @see javax.servlet.http.HttpServletResponse#getStatus()
                 */
                @Override
                public int getStatus() {
                    throw new UnsupportedOperationException();
                }
            }
        }
    }
    //------------------------------------------------------------------------
    // Class EmbeddedBody
    //------------------------------------------------------------------------
    /**
     * Embedded Body
     */
    static class EmbeddedBody {
        private StringWriter characterSink;
        private BinarySink binarySink;
        boolean isEmpty(){
            return this.binarySink == null && this.characterSink == null;
        }
        boolean isBinary(){
            return this.binarySink != null;
        }
        Writer getCharacterSink(){
            if(this.characterSink == null) {
                this.characterSink = new StringWriter();
            }
            return this.characterSink;
        }
        OutputStream getBinarySink(){
            if(this.binarySink == null) {
                this.binarySink = new BinarySink();
            }
            return this.binarySink;
        }
        void reset(){
            if(isBinary()) {
                this.binarySink.reset();
            } else {
                this.characterSink.getBuffer().setLength(0);
            }
        }
        BinarySink asBinarySource(
        ){
            BinarySink buffer = new BinarySink();
            try {
                CharacterLargeObjects.streamCopy(
                    getCharacterSource(),
                    0l,
                    new OutputStreamWriter(buffer, "UTF-8")
                );
            } catch (UnsupportedEncodingException exception) {
                throw new RuntimeServiceException(exception);
            } catch (IOException exception) {
                throw new RuntimeServiceException(exception);
            }
            return buffer;
        }
        /**
         * Retrieve the content
         * 
         * @return the content as <code>ServletInputStream</code>
         */
        ServletInputStream getBinarySource(
        ){
            return new ServletInputStream(
            ) {
                BinarySink source = isBinary() ? binarySink : asBinarySource();
                private final byte[] data = source.getBuffer();
                private final int count = source.size();
                private int cursor = 0;
                private int mark = 0;
                /* (non-Javadoc)
                 * @see java.io.InputStream#read()
                 */
                @Override
                public int read(
                ) throws IOException {
                    return this.cursor < this.count ? this.data[this.cursor++] & 0xff : -1;
                }
                /* (non-Javadoc)
                 * @see java.io.InputStream#available()
                 */
                @Override
                public int available(
                ) throws IOException {
                    return this.count - this.cursor;
                }
                /* (non-Javadoc)
                 * @see java.io.InputStream#mark(int)
                 */
                @Override
                public synchronized void mark(int readlimit) {
                    this.mark = this.cursor;
                }
                /* (non-Javadoc)
                 * @see java.io.InputStream#markSupported()
                 */
                @Override
                public boolean markSupported(
                ) {
                    return true;
                }
                /* (non-Javadoc)
                 * @see java.io.InputStream#read(byte[], int, int)
                 */
                @Override
                public int read(
                    byte[] data,
                    int offset,
                    int length
                ) throws IOException {
                    int count = Math.min(this.count - this.cursor, length);
                    System.arraycopy(this.data, this.cursor, data, offset, count);
                    return count;
                }
                /* (non-Javadoc)
                 * @see java.io.InputStream#reset()
                 */
                @Override
                public synchronized void reset(
                ) throws IOException {
                    this.cursor = this.mark;
                }
                /* (non-Javadoc)
                 * @see java.io.InputStream#skip(long)
                 */
                @Override
                public long skip(
                    long n
                ) throws IOException {
                    long count = Math.min(this.count - this.cursor, n);
                    this.cursor += count;
                    return count;
                }
            };
        }
        /**
         * Retrieve the character content
         * 
         * @return the character content
         */
        Reader getCharacterSource(
        ){
            if(this.isBinary()) {
                try {
                    return new InputStreamReader(getBinarySource(), "UTF-8");
                } catch (UnsupportedEncodingException exception) {
                    throw new RuntimeServiceException(exception);
                }
            } else {
                return new Reader() {
                    private final StringBuffer data = characterSink.getBuffer();
                    private final int count = this.data.length();
                    private int cursor = 0;
                    private int mark = 0;
                    /* (non-Javadoc)
                     * @see java.io.Reader#ready()
                     */
                    @Override
                    public boolean ready(
                    ) throws IOException {
                        return this.cursor < this.count;
                    }
                    /* (non-Javadoc)
                     * @see java.io.Reader#close()
                     */
                    @Override
                    public void close(
                    ) throws IOException {
                        // Nothing to do
                    }
                    /* (non-Javadoc)
                     * @see java.io.Reader#read(char[], int, int)
                     */
                    @Override
                    public int read(
                        char[] data,
                        int offset,
                        int length
                    ) throws IOException {
                        int count = Math.min(length, this.count - this.cursor);
                        this.data.getChars(this.cursor, this.cursor + count, data, offset);
                        this.cursor += count;
                        return count == 0 ? -1 : count;
                    }
                    /* (non-Javadoc)
                     * @see java.io.Reader#mark(int)
                     */
                    @Override
                    public void mark(
                        int readAheadLimit
                    ) throws IOException {
                        this.mark = this.cursor;
                    }
                    /* (non-Javadoc)
                     * @see java.io.Reader#markSupported()
                     */
                    @Override
                    public boolean markSupported(
                    ) {
                        return true;
                    }
                    /* (non-Javadoc)
                     * @see java.io.Reader#read()
                     */
                    @Override
                    public int read(
                    ) throws IOException {
                        return this.cursor < this.count ? this.data.charAt(this.cursor++) : -1;
                    }
                    /* (non-Javadoc)
                     * @see java.io.Reader#skip(long)
                     */
                    @Override
                    public long skip(
                        long n
                    ) throws IOException {
                        long count = Math.min(this.count - this.cursor, n);
                        this.cursor += count;
                        return count;
                    }
                    /* (non-Javadoc)
                     * @see java.io.Reader#reset()
                     */
                    @Override
                    public void reset(
                    ) throws IOException {
                        this.cursor = this.mark;
                    }
                };
            }
        }
        /**
         * Retrieve the content
         * 
         * @return the content as <code>InputSource</code>
         */
        InputSource getInputSource(
        ){
            return
                isEmpty() ? null :
                isBinary() ? new InputSource(getBinarySource()) :
                new InputSource(getCharacterSource());
        }
    }
    //------------------------------------------------------------------------
    // Class BinarySink
    //------------------------------------------------------------------------
    /**
     * A binary sink exposes the superclass' buffer
     */
    static class BinarySink extends ByteArrayOutputStream {
        byte[] getBuffer(){
            return super.buf;
        }
    }
}
