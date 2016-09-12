  /*
   * ====================================================================
   * Project:     openCRX/Core, http://www.opencrx.org/
   * Description: MailImporterServlet
   * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
   * ====================================================================
   *
   * This software is published under the BSD license
   * as listed below.
   * 
   * Copyright (c) 2004-2012, CRIXP Corp., Switzerland
   * All rights reserved.
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
   * * Neither the name of CRIXP Corp. nor the names of the contributors
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
package org.opencrx.application.mail.importer;

import java.io.IOException;
import java.util.Arrays;
import java.util.Date;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.jdo.PersistenceManager;
import javax.jdo.PersistenceManagerFactory;
import javax.mail.Flags;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.internet.MimeMessage;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.opencrx.kernel.activity1.jmi1.ActivityGroup;
import org.opencrx.kernel.activity1.jmi1.ActivityProcess;
import org.opencrx.kernel.activity1.jmi1.ActivityType;
import org.opencrx.kernel.backend.Activities;
import org.opencrx.kernel.backend.Activities.ActivityClass;
import org.opencrx.kernel.backend.Activities.Priority;
import org.opencrx.kernel.backend.Workflows;
import org.opencrx.kernel.base.jmi1.SendAlertParams;
import org.opencrx.kernel.generic.SecurityKeys;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.mof.spi.Model_1Factory;
import org.openmdx.base.naming.Path;
import org.openmdx.kernel.log.SysLog;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

/**
 * The MailImporterServlet imports E-Mails from a configured Mail server
 * and folder as openCRX EMail activities. Activities are created either
 * with a default EMail activity creator or by the activity creator defined
 * by the EMail subject.
 * 
 * The subject line of the wrapper message must be of the form
 * > @<email creator name> [#<activity creator name or activity#>]  <subject>
 * The activity creator name / activity# is optional. If specified an activity is
 * created and the imported E-Mails are linked to this activity.
 */
public class MailImporterServlet 
    extends HttpServlet {

    /* (non-Javadoc)
     * @see javax.servlet.Servlet#init(javax.servlet.ServletConfig)
     */
    public void init(
        ServletConfig config
    ) throws ServletException {
        super.init(config);        
        // initialize model repository
        try {
            Model_1Factory.getModel();
        } catch(Exception e) {
        	SysLog.warning("Can not initialize model repository", e);
        }
        // data connection
        try {
            this.pmf = Utils.getPersistenceManagerFactory();
        } catch(Exception e) {
            throw new ServletException("can not get connection to data provider", e);
        }
    }

    /**
     * Send notification to segment admin.
     * 
     * @param pm
     * @param providerName
     * @param segmentName
     * @param importance
     * @param subject
     * @param message
     * @param params
     */
    protected void notifyAdmin(
        PersistenceManager pm,
        String providerName,
        String segmentName,
        short importance,
        String subject,
        String message,
        String[] params
    ) {
        try {
            Path adminHomeIdentity = new Path(
                "xri://@openmdx*org.opencrx.kernel.home1").getDescendant("provider", providerName, "segment", segmentName, "userHome", SecurityKeys.ADMIN_PRINCIPAL + SecurityKeys.ID_SEPARATOR + segmentName
            );
            UserHome userHome = (UserHome)pm.getObjectById(adminHomeIdentity);
            try {
                pm.currentTransaction().begin();
                message = (message == null || message.length() == 0 ? "" : message + ": ") + Arrays.asList(params);
            	SendAlertParams sendAlertParams = Structures.create(
            		SendAlertParams.class, 
            		Datatypes.member(SendAlertParams.Member.description, message),
            		Datatypes.member(SendAlertParams.Member.importance, importance),
            		Datatypes.member(SendAlertParams.Member.name, "Email Importer [" + providerName + "/" + segmentName + "] " + subject),
            		Datatypes.member(SendAlertParams.Member.resendDelayInSeconds, null),
            		Datatypes.member(SendAlertParams.Member.toUsers, SecurityKeys.ADMIN_PRINCIPAL + SecurityKeys.ID_SEPARATOR + segmentName),
            		Datatypes.member(SendAlertParams.Member.reference, null)
            	);
                userHome.sendAlert(sendAlertParams);
                pm.currentTransaction().commit();
            } catch(Exception e) {
                try {
                    pm.currentTransaction().rollback();                    
                } catch(Exception uncaught) {}
            }
        } catch(Exception e) {
            new ServiceException(e).log();
        }        
    }
    
    /**
     * Import new messages from message store.
     * 
     * @param pm
     * @param providerName
     * @param segmentName
     * @param message
     * @param config
     */
    protected void importMessages(
        PersistenceManager pm,
        String providerName,
        String segmentName,
        MimeMessage message,
        MailImporterConfig config
    ) {        
    	SysLog.info("Importing Message (" + providerName + "/" + segmentName + "): ", message);
        String messageId = "NA";
        try {
            messageId = message.getMessageID();
        } catch(Exception uncaught) {}
        boolean success = true;
        try {
    		// If message subject starts with "> " importMimeMessage() handles 'link to and followUp'
    		Activities.getInstance().importMimeMessage(
    			pm,
	     		providerName, 
	     		segmentName, 
	     		message, 
	     		null // derive E-Mail creator from subject line 
            );
        } catch(Exception e) {
        	SysLog.info(e.getMessage(), e.getCause());                        
            this.notifyAdmin(
                pm,
                providerName,
                segmentName,
                Priority.NORMAL.getValue(),
                "Exception occurred when importing message (" + e.getMessage() + ")",
                new ServiceException(e).toString(),
                new String[]{messageId}
            );
            success = false;
        }
        if(success && config.deleteImportedMessages()) {
    		try {
	            message.setFlag(
	            	Flags.Flag.DELETED,
	            	true
	            );
            } catch (MessagingException uncaught) {}
        }
    }

    /**
     * Import messages to segment.
     * 
     * @param providerName
     * @param segmentName
     * @throws IOException
     */
    protected void importMessages(
        String providerName,
        String segmentName        
    ) throws IOException {
        PersistenceManager pm = null;
        try {
            pm = this.pmf.getPersistenceManager(
                SecurityKeys.ADMIN_PRINCIPAL + SecurityKeys.ID_SEPARATOR + segmentName,
                null
            );
            System.out.println(new Date().toString() + ": " + WORKFLOW_NAME + " " + providerName + "/" + segmentName);
            Workflows.getInstance().initWorkflows(
                pm,
                providerName,
                segmentName
            );
            ActivityProcess emailActivityProcess = Activities.getInstance().initEmailProcess(
            	pm, 
            	providerName, 
            	segmentName,
            	null, // owningGroups
            	SecurityKeys.ACCESS_LEVEL_NA
            );
            ActivityType emailActivityType = Activities.getInstance().initActivityType(
                emailActivityProcess,
                org.opencrx.kernel.backend.Activities.ACTIVITY_TYPE_NAME_EMAILS,
                ActivityClass.EMAIL.getValue(),
                null, // owningGroups
                SecurityKeys.ACCESS_LEVEL_NA
            );
            ActivityGroup emailActivityTracker = Activities.getInstance().initActivityTracker(
                org.opencrx.kernel.backend.Activities.ACTIVITY_TRACKER_NAME_EMAILS, 
                null,
                pm, 
                providerName, 
                segmentName
            );
            Activities.getInstance().initActivityCreator(
                org.opencrx.kernel.backend.Activities.ACTIVITY_CREATOR_NAME_EMAILS, 
                emailActivityType,
                Arrays.asList(emailActivityTracker),
                null
            );
            MailImporterConfig mailImporterConfig = new MailImporterConfig(
                pm,
                providerName,
                segmentName
            );
            MailStore mailStore = new MailStore(mailImporterConfig);
            mailStore.openStore();
            mailStore.openFolder(
                mailImporterConfig.getMailbox()
            );
            Message[] messages = mailStore.getMessages();
            for(int i = 0; i < messages.length; i++) {
                Message message = messages[i];
                if(message instanceof MimeMessage) {
                    MimeMessage mimeMessage = (MimeMessage) message;
                    String messageId = "NA";
                    try {
                        messageId = mimeMessage.getMessageID();
                        this.importMessages(
                            pm,
                            providerName,
                            segmentName,
                            mimeMessage,
                            mailImporterConfig
                        );
                    } catch(Exception e) {
                    	SysLog.info(e.getMessage(), e.getCause());
                        this.notifyAdmin(
                            pm,
                            providerName,
                            segmentName,
                            Priority.HIGH.getValue(),
                            "Import of message " + messageId + " failed (" + e.getMessage() + ")",
                            new ServiceException(e).toString(),
                            new String[]{messageId}
                        );
                    }
                }
            }
            mailStore.closeFolder();
            mailStore.closeStore();
        } catch (Exception e) {
            if(pm != null) {
                this.notifyAdmin(
                    pm,
                    providerName,
                    segmentName,
                    Priority.HIGH.getValue(),
                    "Import of messages failed (" + e.getMessage() + ")",
                    new ServiceException(e).toString(),
                    new String[]{}
                );
            }
            SysLog.warning(new Date() + ": " + WORKFLOW_NAME + " " + providerName + "/" + segmentName + ": ");
            new ServiceException(e).log();
        } finally {
        	if(pm != null) {
        		pm.close();
        	}
    	}
    }

    /**
     * Process an email import request.
     * 
     * @param req the servlet request
     * @param res the servlet response
     * @throws ServletException
     * @throws IOException
     */
    protected void handleRequest(
        HttpServletRequest req, 
        HttpServletResponse res
    ) throws ServletException, IOException {
        String segmentName = req.getParameter("segment");
        String providerName = req.getParameter("provider");
        String id = providerName + "/" + segmentName;
        // run
        if(COMMAND_EXECUTE.equals(req.getPathInfo())) {
            if(!this.runningSegments.containsKey(id)) {
                try {
                    this.runningSegments.put(
                    	id,
                    	Thread.currentThread()
                    );
	                this.importMessages(
	                    providerName,
	                    segmentName
	                );
	            }
	            catch(Exception e) {
	                ServiceException e0 = new ServiceException(e);
	                SysLog.warning("Import messages failed", e0.getMessage());
	                SysLog.warning(e0.getMessage(), e0.getCause());
	            }
	            finally {
	                this.runningSegments.remove(id);
	            }
            }
        	else if(
        		!this.runningSegments.get(id).isAlive() || 
        		this.runningSegments.get(id).isInterrupted()
        	) {
            	Thread t = this.runningSegments.get(id);
        		System.out.println(new Date() + ": " + WORKFLOW_NAME + " " + providerName + "/" + segmentName + ": workflow " + t.getId() + " is alive=" + t.isAlive() + "; interrupted=" + t.isInterrupted() + ". Skipping execution.");
        	}	            
        }
    }

    /* (non-Javadoc)
     * @see javax.servlet.http.HttpServlet#doGet(javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse)
     */
    protected void doGet(
        HttpServletRequest req, 
        HttpServletResponse res
    ) throws ServletException, IOException {
        res.setStatus(HttpServletResponse.SC_OK);
        res.flushBuffer();
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
        res.setStatus(HttpServletResponse.SC_OK);
        res.flushBuffer();
        this.handleRequest(
            req,
            res
        );
    }

    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    private static final long serialVersionUID = -7260829387268368633L;
    
    private static final String WORKFLOW_NAME = "MailImporter";
    private static final String COMMAND_EXECUTE = "/execute";
    
    private PersistenceManagerFactory pmf = null;
    private final Map<String,Thread> runningSegments = new ConcurrentHashMap<String,Thread>();

}

//  --- End of File -----------------------------------------------------------
