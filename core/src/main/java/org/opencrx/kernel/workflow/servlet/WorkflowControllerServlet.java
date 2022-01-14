/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: WorkflowControllerServlet
 * Owner:       the original authors.
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions 
 * are met:
 * 
 * * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in
 * the documentation and/or other materials provided with the
 * distribution.
 * 
 * * Neither the name of the openCRX team nor the names of the contributors
 * to openCRX may be used to endorse or promote products derived
 * from this software without specific prior written permission
 * 
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
 * This product includes software developed by the Apache Software
 * Foundation (http://www.apache.org/).
 * 
 * This product includes software developed by contributors to
 * openMDX (http://www.openmdx.org/)
 */
package org.opencrx.kernel.workflow.servlet;

import java.io.IOException;
import java.io.PrintWriter;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.opencrx.kernel.admin1.jmi1.ComponentConfiguration;
import org.opencrx.kernel.generic.SecurityKeys;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.base.text.conversion.UUIDConversion;
import org.openmdx.kernel.id.UUIDs;
import org.openmdx.kernel.id.cci.UUIDGenerator;
import org.openmdx.kernel.log.SysLog;

/**
 * The WorkflowControllerServlet periodically pings the configured URLs 
 */  
public class WorkflowControllerServlet 
    extends HttpServlet {

    /**
     * PingRate
     *
     */
    public static class PingRate {
        
        public PingRate(
            long initial
        ) {
            this.rate = initial;
        }
        
        public void setRate(
            long newValue
        ) {
            this.rate = newValue;
        }
        
        public long getRate(
        ) {
            return this.rate;
        }
        
        public void increment(
        ) {
            this.rate++;
        }
        
        public void decrement(
        ) {
            if(this.rate > 1) {
                this.rate--;
            }
        }
        
        private long rate = 1L;
    }
     
    /**
     * WorkflowMonitor
     *
     */
    public class WorkflowMonitor
       implements Runnable {
        
        public WorkflowMonitor(
        ) {
        }
        
        public void run(
        ) {
            // Print all monitored paths
            List<String> paths = new ArrayList<String>();
            for(WorkflowServletConfig workflowServletConfig: WorkflowControllerServlet.this.monitoredWorkflowServletConfigs.values()) {
                paths.add(workflowServletConfig.getPath());
            }                
            System.out.println(new Date().toString() + "  " + COMPONENT_CONFIGURATION_ID + ": monitoring " + paths);
            // Monitor forever
            while(true) {                
                try {
                	if(WorkflowControllerServlet.this.isStopped) {
                        SysLog.detail("Monitor is paused. Waiting for resume.");
                	} else {
	                    for(WorkflowServletConfig workflowServletConfig: WorkflowControllerServlet.this.monitoredWorkflowServletConfigs.values()) {
	                        URL monitoredURL = WorkflowControllerServlet.this.getWorkflowServletURL(
	                            workflowServletConfig.getPath()
	                        );
	                        SysLog.detail("Next execution", Arrays.asList(new Object[]{monitoredURL, workflowServletConfig.getNextExecutionAt()}));
	                        if(new Date().compareTo(workflowServletConfig.getNextExecutionAt()) > 0) {
	                            if(monitoredURL != null) {
	                                try {
	                                    HttpURLConnection connection = (HttpURLConnection)monitoredURL.openConnection();
	                                    connection.setInstanceFollowRedirects(false);
	                                    connection.setDoInput(true);
	                                    connection.setDoOutput(true);
	                                    connection.setRequestMethod("POST");
	                                    int rc = connection.getResponseCode();
	                                    if(rc != HttpURLConnection.HTTP_OK) {
	                                        System.out.println(new Date().toString() + "  " + COMPONENT_CONFIGURATION_ID + ": response code for " + monitoredURL + " " + rc);
	                                    }
	                                    workflowServletConfig.scheduleNextExecution();
	                                } catch(IOException e0) {
	                                    new ServiceException(e0).log();
	                                }
	                            }                            
	                        }
	                    }
                	}
                    // Wait 1 min between monitoring cycles
                    Thread.sleep(60000L);
                } catch(Exception e) {
                    System.out.println(new Date().toString() + "  " + COMPONENT_CONFIGURATION_ID + ": catched exception (for more information see log) " + e.getMessage());
                    ServiceException e0 = new ServiceException(e);
                    SysLog.error(e0.getMessage(), e0.getCause());
                } catch(Error e) {
                    System.out.println(new Date().toString() + "  " + COMPONENT_CONFIGURATION_ID + ": catched error (for more information see log) " + e.getMessage());
                    SysLog.error(e.getMessage(), e.getCause());                    
                }
            }       
        }
    }
    
    /**
     * WorkflowServletConfig
     *
     */
    public class WorkflowServletConfig {
        
        public WorkflowServletConfig(
            String path,
            boolean autostart,
            PingRate pingRate
        ) {
            this.path = path;
            this.isAutostart = autostart;
            this.pingRate = pingRate;
            this.nextExecutionAt = new GregorianCalendar();
            this.nextExecutionAt.setTimeInMillis(
                System.currentTimeMillis() + 60000L
            );
            this.scheduleNextExecution();
        }
    
        public String getPath(
        ) {
            return this.path;
        }
        
        public boolean isAutostart(
        ) {
            return this.isAutostart;
        }
                
        public Date getNextExecutionAt(
        ) {
            return this.nextExecutionAt.getTime();         
        }
        
        public void scheduleNextExecution(
        ) {
            GregorianCalendar nextExecutionAt = new GregorianCalendar();
            nextExecutionAt.setTime(this.nextExecutionAt.getTime());
            while(nextExecutionAt.getTime().compareTo(new Date()) < 0) {
                nextExecutionAt.setTimeInMillis(
                    nextExecutionAt.getTimeInMillis() + this.pingRate.getRate() * 60000L
                );
            }
            this.nextExecutionAt.setTime(nextExecutionAt.getTime());
        }

        private final String path;
        private final boolean isAutostart;
        private final PingRate pingRate;
        private GregorianCalendar nextExecutionAt;
    }

    /**
     * Assert and start workflow monitor.
     * 
     */
    protected void assertAndStartWorkflowMonitor(
    ) {
    	if(this.workflowMonitor == null || !this.workflowMonitor.isAlive()) {
	        this.workflowMonitor = new Thread(
	            new WorkflowMonitor()
	        );
	        this.workflowMonitor.start();
    	}
    }

    /* (non-Javadoc)
     * @see javax.servlet.GenericServlet#init(javax.servlet.ServletConfig)
     */
    public void init(
        ServletConfig config
    ) throws ServletException {

        super.init(config);
        String providerName = new Path(this.getInitParameter("realmSegment")).get(2);
        PersistenceManager pm = null;
        try {
            pm = Utils.getPersistenceManagerFactory().getPersistenceManager(
                SecurityKeys.ROOT_PRINCIPAL,
                null
            );
        } catch(Exception e) {
            throw new ServletException("Can not get connection to provider", e);
        }
        // Get component configuration
        try {
            org.opencrx.kernel.admin1.jmi1.Segment adminSegment = 
                (org.opencrx.kernel.admin1.jmi1.Segment)pm.getObjectById(
                    new Path("xri://@openmdx*org.opencrx.kernel.admin1").getDescendant("provider", providerName, "segment", "Root")
                );
            try {
                this.componentConfiguration = adminSegment.getConfiguration(
                    COMPONENT_CONFIGURATION_ID
                );
            } catch(Exception e) {}
            if(this.componentConfiguration == null) {
                org.opencrx.kernel.admin1.jmi1.ComponentConfiguration componentConfiguration = pm.newInstance(ComponentConfiguration.class);
                componentConfiguration.setName("WorkflowController");
                pm.currentTransaction().begin();
                adminSegment.addConfiguration(
                    false, 
                    COMPONENT_CONFIGURATION_ID,
                    componentConfiguration
                );
                // Default serverURL
                UUIDGenerator uuids = UUIDs.getGenerator();
                org.opencrx.kernel.base.jmi1.StringProperty sp = pm.newInstance( org.opencrx.kernel.base.jmi1.StringProperty.class);
                sp.setName(WorkflowControllerServlet.OPTION_SERVER_URL);
                sp.setDescription("Server URL");
                sp.setStringValue("http://127.0.0.1:8080/opencrx-core-" + providerName);
                componentConfiguration.addProperty(
                    UUIDConversion.toUID(uuids.next()),
                    sp
                );
                // SubscriptionHandler.<provider>.Standard.autostart
                sp = pm.newInstance( org.opencrx.kernel.base.jmi1.StringProperty.class);
                sp.setName(MONITORED_WORKFLOW_SUBSCRIPTIONHANDLER + "." + providerName + ".Standard." + OPTION_AUTOSTART);
                sp.setDescription(MONITORED_WORKFLOW_SUBSCRIPTIONHANDLER + " autostart");
                sp.setStringValue("false");
                componentConfiguration.addProperty(
                    UUIDConversion.toUID(uuids.next()),
                    sp
                );
                // SubscriptionHandler.<provider>.Standard.pingrate
                sp = pm.newInstance( org.opencrx.kernel.base.jmi1.StringProperty.class);
                sp.setName(MONITORED_WORKFLOW_SUBSCRIPTIONHANDLER + "." + providerName + ".Standard." + OPTION_PINGRATE);
                sp.setDescription(MONITORED_WORKFLOW_SUBSCRIPTIONHANDLER + " pingrate");
                sp.setStringValue("2");
                componentConfiguration.addProperty(
                    UUIDConversion.toUID(uuids.next()),
                    sp
                );
                // IndexerServlet.<provider>.Standard.autostart
                sp = pm.newInstance( org.opencrx.kernel.base.jmi1.StringProperty.class);
                sp.setName(MONITORED_WORKFLOW_INDEXER + "." + providerName + ".Standard." + OPTION_AUTOSTART);
                sp.setDescription(MONITORED_WORKFLOW_INDEXER + " autostart");
                sp.setStringValue("false");
                componentConfiguration.addProperty(
                    UUIDConversion.toUID(uuids.next()),
                    sp
                );
                // IndexerServlet.<provider>.Standard.pingrate
                sp = pm.newInstance( org.opencrx.kernel.base.jmi1.StringProperty.class);
                sp.setName(MONITORED_WORKFLOW_INDEXER + "." + providerName + ".Standard." + OPTION_PINGRATE);
                sp.setDescription(MONITORED_WORKFLOW_INDEXER + " pingrate");
                sp.setStringValue("2");
                componentConfiguration.addProperty(
                    UUIDConversion.toUID(uuids.next()),
                    sp                   
                );
                // WorkflowHandler.<provider>.Standard.autostart
                sp = pm.newInstance( org.opencrx.kernel.base.jmi1.StringProperty.class);
                sp.setName(MONITORED_WORKFLOW_WORKFLOWHANDLER + "." + providerName + ".Standard." + OPTION_AUTOSTART);
                sp.setDescription(MONITORED_WORKFLOW_WORKFLOWHANDLER + " autostart");
                sp.setStringValue("false");
                componentConfiguration.addProperty(
                    UUIDConversion.toUID(uuids.next()),
                    sp
                );
                // WorkflowHandler.<provider>.Standard.pingrate
                sp = pm.newInstance( org.opencrx.kernel.base.jmi1.StringProperty.class);
                sp.setName(MONITORED_WORKFLOW_WORKFLOWHANDLER + "." + providerName + ".Standard." + OPTION_PINGRATE);
                sp.setDescription(MONITORED_WORKFLOW_WORKFLOWHANDLER + " pingrate");
                sp.setStringValue("2");
                componentConfiguration.addProperty(
                    UUIDConversion.toUID(uuids.next()),
                    sp
                );
                pm.currentTransaction().commit();
                this.componentConfiguration = adminSegment.getConfiguration(
                    COMPONENT_CONFIGURATION_ID
                );
            }
        } catch(Exception e) {
            throw new ServletException("Can not get component configuration", e);
        }
        // Get set of segments to be monitored
        Set<String> segmentNames = new HashSet<String>();
        try {
            Set<String> excludeRealms = new HashSet<String>();
            for(int ii = 0; ii < 100; ii++) {
                if(this.getInitParameter("excludeRealm[" + ii + "]") != null) {
                    excludeRealms.add(
                        this.getInitParameter("excludeRealm[" + ii + "]")
                    );
                }
            }
            org.openmdx.security.realm1.jmi1.Segment realmSegment = 
                (org.openmdx.security.realm1.jmi1.Segment)pm.getObjectById(
                    new Path(this.getInitParameter("realmSegment"))
                );
            Collection<org.openmdx.security.realm1.jmi1.Realm> realms = realmSegment.getRealm();
            for(org.openmdx.security.realm1.jmi1.Realm realm: realms) {
                if(!excludeRealms.contains(realm.getName())) {
                    segmentNames.add(realm.getName());
                }
            }
        } catch(Exception e) {
            throw new ServletException("Error occured while retrieving realms", e);
        }
        // Create a path to be monitored from each configured path and provider/segment
        try {
        	ConcurrentMap<String,WorkflowServletConfig> workflowServletConfigs = new ConcurrentHashMap<String,WorkflowServletConfig>();
        	ConcurrentMap<String,WorkflowServletConfig> monitoredWorkflowServletConfigs = new ConcurrentHashMap<String,WorkflowServletConfig>();
            for(Iterator<String> j = segmentNames.iterator(); j.hasNext(); ) {
                String segmentName = j.next();
                int ii = 0;
                while(this.getInitParameter("path[" + ii + "]") != null) {
                    String path = this.getInitParameter("path[" + ii + "]");
                    String autostart = this.getComponentConfigProperty(path.substring(1) + "." + providerName + "." + segmentName + "." + OPTION_AUTOSTART);
                    String pingrate = this.getComponentConfigProperty(path.substring(1) + "." + providerName + "." + segmentName + "." + OPTION_PINGRATE);
                    WorkflowServletConfig workflowServletConfig =
                        new WorkflowServletConfig(
                            path + "/execute?provider=" + providerName + "&segment=" + segmentName,
                            autostart != null ? 
                            	Boolean.valueOf(autostart).booleanValue() : 
                            		false,
                            pingrate != null ? 
                            	new PingRate(Long.valueOf(pingrate).longValue()) : 
                            		new PingRate(1L)
                        );                        
                    workflowServletConfigs.put(
                    	workflowServletConfig.getPath(),
                        workflowServletConfig
                    );
                    if(workflowServletConfig.isAutostart()) {
                        monitoredWorkflowServletConfigs.put(
                        	workflowServletConfig.getPath(),
                        	workflowServletConfig
                        );
                    }
                    ii++;
                }
            }
            this.workflowServletConfigs = workflowServletConfigs;
            this.monitoredWorkflowServletConfigs = monitoredWorkflowServletConfigs;
            this.providerName = providerName;
            this.assertAndStartWorkflowMonitor();
    		String autostartConnectors = System.getProperty("org.openmdx.catalina.core.ExtendedService.autostartConnectors");
    		this.isStopped = (autostartConnectors != null) && !Boolean.valueOf(autostartConnectors).booleanValue();
        } catch(Exception e) {
            throw new ServletException("Can not start workflow monitor", e);
        }
    }

    /**
     * Get configuration property.
     * 
     * @param name
     * @return
     */
    private String getComponentConfigProperty(
        String name
    ) {
        String value = null;
        PersistenceManager pm = JDOHelper.getPersistenceManager(this.componentConfiguration);
        for(int i = 0; i < 1; i++) {
        	Collection<org.opencrx.kernel.base.jmi1.Property> properties = this.componentConfiguration.getProperty();
            for(org.opencrx.kernel.base.jmi1.Property p: properties) {
                if(
                    p.getName().equals(name) &&
                    (p instanceof org.opencrx.kernel.base.jmi1.StringProperty)
                ) {
                    value = ((org.opencrx.kernel.base.jmi1.StringProperty)p).getStringValue();
                    break;
                }
            }
            if(value == null) {
                pm.refresh(this.componentConfiguration);
            } else {
                break;
            }
        }
        return value;
    }
    
    /**
     * Get workflow servlet URL.
     * 
     * @param path
     * @return
     * @throws MalformedURLException
     */
    public URL getWorkflowServletURL(
        String path
    ) throws MalformedURLException {
        String servlerURL = this.getComponentConfigProperty(OPTION_SERVER_URL);
        return servlerURL == null ? 
        	null : 
        		new URL(servlerURL + path);
    }
    
    /**
     * Handle command.
     * 
     * @param req
     * @param res
     * @throws ServletException
     * @throws IOException
     */
    protected void handleRequest(
        HttpServletRequest req, 
        HttpServletResponse res
    ) throws ServletException, IOException {
    	// Assert that monitoring thread is alive
    	this.assertAndStartWorkflowMonitor();    	
        // Add/remove to activeURLs
        if(COMMAND_START.equals(req.getPathInfo())) {
            for(WorkflowServletConfig workflowServletConfig: this.workflowServletConfigs.values()) {
                if(URLEncoder.encode(workflowServletConfig.getPath(), "UTF-8").equals(req.getQueryString())) {
                    this.monitoredWorkflowServletConfigs.putIfAbsent(
                    	workflowServletConfig.getPath(),
                    	workflowServletConfig
                    );
                    System.out.println(new Date().toString() + "  " + COMPONENT_CONFIGURATION_ID + ": started " + workflowServletConfig.getPath());                    
                    break;
                }
            }
        } else if(COMMAND_STOP.equals(req.getPathInfo())) {
            URL monitoredURL = this.getWorkflowServletURL(
                URLDecoder.decode(req.getQueryString(), "UTF-8")
            );
            for(
                Iterator<Map.Entry<String,WorkflowServletConfig>> i = this.monitoredWorkflowServletConfigs.entrySet().iterator();
                i.hasNext();
            ) {
            	Map.Entry<String,WorkflowServletConfig> e = i.next();
                WorkflowServletConfig workflowServletConfig = e.getValue();
                URL url = this.getWorkflowServletURL(
                    workflowServletConfig.getPath()
                );
                if(url.equals(monitoredURL)) {
                    i.remove();
                    System.out.println(new Date().toString() + "  " + COMPONENT_CONFIGURATION_ID + ": stopped " + workflowServletConfig.getPath());                    
                    break;
                }
            }
        } else if(COMMAND_PAUSE.equals(req.getPathInfo())) {
        	this.isStopped = true;
            System.out.println(new Date().toString() + "  " + COMPONENT_CONFIGURATION_ID + ": paused " + this.providerName);                    
        } else if(COMMAND_RESUME.equals(req.getPathInfo())) {
        	this.isStopped = false;
            System.out.println(new Date().toString() + "  " + COMPONENT_CONFIGURATION_ID + ": resumed " + this.providerName);                    
        }
        // Show status and commands
        res.setContentType("text/html");
        res.setStatus(HttpServletResponse.SC_OK);
        PrintWriter out = res.getWriter();
        out.println("<html>");
        out.println("<head>");
        out.println("  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">");
        out.println("  <link rel=\"stylesheet\" href=\"../js/bootstrap/css/bootstrap.min.css\">");	
        out.println("  <link rel=\"stylesheet\" href=\"../_style/colors.css\">");
        out.println("  <link rel=\"stylesheet\" href=\"../_style/n2default.css\">");
        out.println("  <link rel=\"stylesheet\" href=\"../_style/ssf.css\">");
        out.println("  <script type=\"text/javascript\" src=\"../js/prototype.js\"></script>");
        out.println("  <link rel='shortcut icon' href='../images/favicon.ico' />");
        out.println("</head>");
        out.println("<body>");
        out.println("  <div id=\"container\">");
        out.println("    <div id=\"wrap\">");
        out.println("      <div id=\"header\" style=\"height:90px;\">");
        out.println("        <div id=\"logoTable\">");
        out.println("          <table id=\"headerlayout\">");
        out.println("            <tr id=\"headRow\">");
        out.println("              <td id=\"head\" colspan=\"2\">");
        out.println("                <table id=\"info\">");
        out.println("                  <tr>");
        out.println("                    <td id=\"headerCellLeft\"><img id=\"logoLeft\" src=\"../images/logoLeft.gif\" alt=\"openCRX\" title=\"\" /></td>");
        out.println("                    <td id=\"headerCellSpacerLeft\"></td>");
        out.println("                    <td id=\"headerCellMiddle\">&nbsp;</td>");
        out.println("                    <td id=\"headerCellRight\"><img id=\"logoRight\" src=\"../images/logoRight.gif\" alt=\"\" title=\"\" /></td>");
        out.println("                  </tr>");
        out.println("                </table>");
        out.println("              </td>");
        out.println("            </tr>");
        out.println("          </table>");
        out.println("        </div>");
        out.println("      </div>");
        out.println("      <div id=\"content-wrap\">");
        out.println("         <div id=\"content\" style=\"padding:0px 0.5em 0px 0.5em;\">");
        out.println("           <h2>Workflow Controller " + this.providerName + "</h2>");
        out.println("           <table class=\"table table-condensed table-striped\" style=\"width:80%\">");
        out.println("             <tr>");
        out.println("               <td><a href=\"" + req.getContextPath() + req.getServletPath() + (this.isStopped ? COMMAND_RESUME : COMMAND_PAUSE) + "\">" + (this.isStopped ? "Resume" : "Pause") + "</a></td><td />");
        out.println("             </tr>");
        out.println("             <tr><td colspan=\"2\"><br /><h3>Active</h3></td></tr>");
        Set<String> sortedPaths = new TreeSet<String>(this.workflowServletConfigs.keySet());
        // Active: print sorted by path
        for(String path: sortedPaths) {
        	WorkflowServletConfig workflowServletConfig = this.workflowServletConfigs.get(path);
            boolean active = this.monitoredWorkflowServletConfigs.keySet().contains(workflowServletConfig.getPath());
            if(active) {
	            out.println("<tr>");
	            out.println("  <td style=\"padding:0px;vertical-align:middle;\">" + workflowServletConfig.getPath() + "</td>");
	            out.println("  <td style=\"padding:0px;\"><a class=\"btn btn-light\" href=\"" + req.getContextPath() + req.getServletPath() + COMMAND_STOP + "?" + URLEncoder.encode(workflowServletConfig.getPath(), "UTF-8") + "\">" + (active ? "Turn Off" : "Turn On") + "</a></td>");
	            out.println("</tr>");
            }
        }
        out.println("<tr><td colspan=\"2\"><br /><h3>Inactive</h3></td></tr>");
        // Inactive: print sorted by path
        for(String path: sortedPaths) {
        	WorkflowServletConfig workflowServletConfig = this.workflowServletConfigs.get(path);
            boolean active = this.monitoredWorkflowServletConfigs.keySet().contains(workflowServletConfig.getPath());
            if(!active) {
	            out.println("<tr>");
	            out.println("  <td style=\"padding:0px;vertical-align:middle;\">" + workflowServletConfig.getPath() + "</td>");
	            out.println("  <td style=\"padding:0px;\"><a class=\"btn btn-light\" href=\"" + req.getContextPath() + req.getServletPath() + COMMAND_START + "?" + URLEncoder.encode(workflowServletConfig.getPath(), "UTF-8") + "\">" + (active ? "Turn Off" : "Turn On") + "</a></td>");
	            out.println("</tr>");
            }
        }
        out.println("          </table>");
        out.println("        </div> <!-- content -->");
        out.println("      </div> <!-- content-wrap -->");
        out.println("    </div> <!-- wrap -->");
        out.println("  </div> <!-- container -->");        
        out.println("</body>");
        out.println("</html>");
    }

    /* (non-Javadoc)
     * @see javax.servlet.http.HttpServlet#doGet(javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse)
     */
    protected void doGet(
        HttpServletRequest req, 
        HttpServletResponse res
    ) throws ServletException, IOException {
        this.handleRequest(
            req,
            res
        );
    }
        
    /* (non-Javadoc)
     * @see javax.servlet.http.HttpServlet#doPost(javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse)
     */
    protected void doPost(
        HttpServletRequest req, 
        HttpServletResponse res
    ) throws ServletException, IOException {
        this.handleRequest(
            req,
            res
        );
    }
        
    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    private static final long serialVersionUID = -2397456308895573603L;
    
    private static final String COMPONENT_CONFIGURATION_ID = "WorkflowController";
    
    private static final String COMMAND_START = "/start";
    private static final String COMMAND_STOP = "/stop";
    private static final String COMMAND_PAUSE = "/pause";
    private static final String COMMAND_RESUME = "/resume";

    private static final String MONITORED_WORKFLOW_INDEXER = "IndexerServlet";
    private static final String MONITORED_WORKFLOW_WORKFLOWHANDLER = "WorkflowHandler";
    private static final String MONITORED_WORKFLOW_SUBSCRIPTIONHANDLER = "SubscriptionHandler";
    
    public static final String OPTION_SERVER_URL = "serverURL";
    public static final String OPTION_AUTOSTART = "autostart";
    public static final String OPTION_PINGRATE = "pingrate";
    public static final String OPTION_EXECUTION_PERIOD = "executionPeriod";
    
    protected String providerName = null;
    protected ConcurrentMap<String,WorkflowServletConfig> workflowServletConfigs = null;
    protected ConcurrentMap<String,WorkflowServletConfig> monitoredWorkflowServletConfigs = null;
    protected Thread workflowMonitor = null;
    protected boolean isStopped = false;
    protected org.opencrx.kernel.admin1.jmi1.ComponentConfiguration componentConfiguration = null;
    
}

//--- End of File -----------------------------------------------------------
