/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: Mail workflow
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2014, CRIXP Corp., Switzerland
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
package org.opencrx.application.mail.exporter;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import javax.mail.Address;
import javax.mail.AuthenticationFailedException;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.SendFailedException;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.AddressException;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;
import javax.naming.Context;
import javax.naming.InitialContext;

import org.opencrx.kernel.account1.jmi1.EMailAddress;
import org.opencrx.kernel.activity1.cci2.EMailRecipientQuery;
import org.opencrx.kernel.activity1.jmi1.EMail;
import org.opencrx.kernel.activity1.jmi1.EMailRecipient;
import org.opencrx.kernel.backend.Notifications;
import org.opencrx.kernel.backend.Workflows;
import org.opencrx.kernel.generic.OpenCrxException;
import org.opencrx.kernel.home1.cci2.EMailAccountQuery;
import org.opencrx.kernel.home1.jmi1.EMailAccount;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.opencrx.kernel.home1.jmi1.WfProcessInstance;
import org.opencrx.kernel.utils.Utils;
import org.opencrx.kernel.utils.WorkflowHelper;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.jmi1.BasicObject;
import org.openmdx.base.jmi1.ContextCapable;
import org.openmdx.base.naming.Path;
import org.openmdx.kernel.exception.BasicException;
import org.openmdx.kernel.log.SysLog;
import org.openmdx.uses.org.apache.commons.pool.ObjectPool;
import org.openmdx.uses.org.apache.commons.pool.PoolableObjectFactory;
import org.openmdx.uses.org.apache.commons.pool.impl.GenericObjectPool;

/**
 * MailWorkflow
 *
 */
public abstract class MailWorkflow extends Workflows.AsynchronousWorkflow {
    
	/**
	 * MailTransport
	 *
	 */
	public static class MailTransport {
		
		/**
		 * Constructor.
		 * 
		 * @param name
		 * @param session
		 * @param transport
		 * @param expiresAt
		 */
		public MailTransport(
			String name,
			Session session,
			Transport transport,
			long expiresAt
		) {
			this.name = name;
			this.session = session;
			this.transport = transport;
			this.expiresAt = expiresAt;
		}
		
		/**
		 * Destroy mail transport.
		 * 
		 */
		public void destroy(
		) {
			this.name = null;
			this.session = null;
			if(this.transport != null) {
				try {
					this.transport.close();
				} catch(Exception ignore) {}
			}
			this.transport = null;
		}
		
		/**
		 * @return the session
		 */
		public Session getSession() {
			return session;
		}
		/**
		 * @return the transport
		 */
		public Transport getTransport() {
			return transport;
		}

		/**
		 * @return the expiresAt
		 */
		public long getExpiresAt() {
			return expiresAt;
		}

		/**
		 * @return the name
		 */
		public String getName() {
			return name;
		}

		private String name;
		private long expiresAt;
		private Session session;
		private Transport transport;
		
	}

	/**
	 * The mail workflows are atomic, the may be run in parallel with other 
	 * workflows.
	 */
    @Override
    public boolean isAtomic(
    ) {
    	return true;
    }

	/**
     * Set message content.
     * 
     * @param message
     * @param session
     * @param pm
     * @param targetIdentity
     * @param wfProcessInstanceIdentity
     * @param userHome
     * @param params
     * @return
     * @throws ServiceException
     */
    protected String setContent(
        Message message,
        Session session,
        PersistenceManager pm,
        Path targetIdentity,
        Path wfProcessInstanceIdentity,
        UserHome userHome,
        Map<String,Object> params
    ) throws ServiceException {
        ContextCapable targetObject = null;
        try {
            targetObject = (ContextCapable)pm.getObjectById(targetIdentity);
        } catch(Exception e) {}
        String text = null;
        try {
            text = Notifications.getInstance().getNotificationText(
                pm,
                targetObject,
                wfProcessInstanceIdentity,
                userHome,
                params
            );
            if(text.regionMatches(true, 0, "<!DOCTYPE html", 0, 14)) {
            	message.setContent(text, "text/html; charset=utf-8");
            } else {
            	message.setText(text);
            }
        } catch(MessagingException e) {
            throw new ServiceException(e);
        }
        return text;        
    }

    /**
     * Collect recipients from target and email account and set corresponding 
     * email addresses on message.
     * 
     * @param message
     * @param pm
     * @param targetIdentity
     * @param eMailAccount
     * @param defaultFromAddress
     * @return
     * @throws ServiceException
     */
    protected Address[] setRecipients(
        Message message,
        PersistenceManager pm,
        Path targetIdentity,
        EMailAccount eMailAccount,
        String defaultFromAddress
    ) throws ServiceException {
        Address[] recipients = null;
        try {
            // FROM
            message.setFrom(
                new InternetAddress(
                    eMailAccount.getReplyEMailAddress() == null 
                    	? defaultFromAddress == null 
                    		? "noreply@localhost"
                    		: defaultFromAddress 
                    	: eMailAccount.getReplyEMailAddress()
                )
            );
            // TO
            if(eMailAccount.getName() != null && ! eMailAccount.getName().isEmpty()) {
	            recipients = InternetAddress.parse(eMailAccount.getName());
	            message.setRecipients(
	                Message.RecipientType.TO,
	                recipients
	            );
            } else {
            	recipients = new Address[]{};
            }
        } catch(AddressException e) {
            throw new ServiceException(e);
        } catch(MessagingException e) {
            throw new ServiceException(e);
        }
        return recipients;
    }

    /**
     * If true append prefix to subject.
     * 
     * @return
     */
    abstract boolean useSendMailSubjectPrefix(
    );

    /**
     * Create a log entry which correlates the entry with the email recipient in
     * case target is an email activity.
     * 
     * @param address
     * @param wfProcessInstance
     * @param name
     * @param description
     * @param target
     * @throws ServiceException
     */
    protected void createLogEntry(
    	WfProcessInstance wfProcessInstance,
    	String name,
    	String description,
    	ContextCapable target,
    	Address address
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(wfProcessInstance);    	
    	ContextCapable context = null;
    	if(target instanceof EMail) {
    		EMail email = (EMail)target;
    		EMailRecipientQuery emailRecipientQuery = (EMailRecipientQuery)pm.newQuery(EMailRecipient.class);
    		for(EMailRecipient emailRecipient: email.<EMailRecipient>getEmailRecipient(emailRecipientQuery)) {
    			if(
    				emailRecipient.getParty() instanceof EMailAddress &&
    				((EMailAddress)emailRecipient.getParty()).getEmailAddress().equalsIgnoreCase(address.toString())
    			) {
    				context = emailRecipient.getParty();
    			}
    		}
    	}
        WorkflowHelper.createLogEntry(
            wfProcessInstance,
            name,
            description,
            (BasicObject)context
        );
    }

	/**
	 * Send message using sendmail.
	 * 
	 * @param message
	 * @return
	 */
	protected String doSendmail(
		Message message,
		String envelopeSenderAddress
	) {
		String err = "";
		try {
			// sendmail
			//   -f Sets the envelope sender address.
			//   -t Read message for recipients and ignore the command-line arguments.
			Process p = Runtime.getRuntime().exec("sendmail " + (envelopeSenderAddress == null ? "" : "-f " + envelopeSenderAddress + " ") + "-t", null);
			// Send message via sendmail
			{
				OutputStream os = p.getOutputStream();
				message.writeTo(os);
				os.close();
				if(SysLog.isTraceOn()){
					File sendmaildir = new File("./sendmaildir");
					sendmaildir.mkdirs();
					FileOutputStream fout = new FileOutputStream(new File(sendmaildir, Utils.getUidAsString()));
					message.writeTo(fout);
					fout.close();
				}
			}
			// Get stdout
			{
				BufferedReader out = new BufferedReader(new InputStreamReader(p.getInputStream()));
				String line = null;
				while((line = out.readLine()) != null) {
					err += line + "\n";
				}
			}
			// Get stderr
			{
				BufferedReader out = new BufferedReader(new InputStreamReader(p.getErrorStream()));
				String line = null;
				while((line = out.readLine()) != null) {
					err += line + "\n";
				}
			}
			p.waitFor();
		} catch(Exception e) {
			new ServiceException(e).log();
			err += e.getMessage();
		}
		return err;
	}

	/**
     * Get mail transport from pool.
     * 
     * @param mailServiceName
     * @param fallbackMailServiceName
     * @return
     * @throws ServiceException
     */
    protected static MailTransport getMailTransport(
    	final String mailServiceName,
    	final String fallbackMailServiceName
    ) throws ServiceException {
    	if(!MAIL_TRANSPORT_POOL.containsKey(mailServiceName)) {
    		GenericObjectPool pool = new GenericObjectPool(
				new PoolableObjectFactory(){

					@Override
                    public Object makeObject(
                    ) throws Exception {
			            Context initialContext = new InitialContext();
			            Session session = null;
			            try {
			                session = (Session)initialContext.lookup("java:comp/env" + mailServiceName);
			            } catch(Exception e) {
			            	SysLog.detail("Mail service not found", mailServiceName);
			                // Fallback to mail/provider/<provider>
			                SysLog.detail("Fall back to mail service", mailServiceName);
			                session = (Session)initialContext.lookup("java:comp/env" + fallbackMailServiceName);
			            }
			    		if(session != null) {
			                Transport transport = session.getTransport();
			                String protocol = transport.getURLName().getProtocol();
			                String port = session.getProperty("mail." + protocol + ".port");
			                transport.connect(
			                    session.getProperty("mail." + protocol + ".host"), 
			                    port == null ? -1 : Integer.valueOf(port).intValue(),                           
			                    session.getProperty("mail." + protocol + ".user"), 
			                    session.getProperty("mail." + protocol + ".password")
			                );
			                return new MailTransport(
		                		mailServiceName,
		                		session, 
		                		transport,
		                		System.currentTimeMillis() + TRANSPORT_TTL
		                	);
			    		} else {				    		
			    			throw new ServiceException(
			    				OpenCrxException.DOMAIN,
			    				BasicException.Code.BIND_FAILURE,
			    				"Unable to get mail session",
			    				new BasicException.Parameter("mailService.name", mailServiceName),
			    				new BasicException.Parameter("mailService.fallback.name", fallbackMailServiceName)
			    			);
			    		}
                    }

					@Override
                    public void destroyObject(
                    	Object obj
                    ) throws Exception {
						if(obj instanceof MailTransport) {
							((MailTransport)obj).destroy();
						}
                    }

					@Override
                    public boolean validateObject(
                    	Object obj
                    ) {
                        if(obj instanceof MailTransport) {
                        	MailTransport mailTransport = (MailTransport)obj;
                        	return 
                        		mailTransport.getTransport().isConnected() && 
                    			System.currentTimeMillis() < mailTransport.getExpiresAt();	                    			
                        } else {
                        	return false;
                        }
                    }

					@Override
                    public void activateObject(
                    	Object obj
                    ) throws Exception {
						if(obj instanceof MailTransport) {
                        	@SuppressWarnings("unused")
                            MailTransport mailTransport = (MailTransport)obj;								
						}
                    }

					@Override
                    public void passivateObject(
                    	Object obj
                    ) throws Exception {
						if(obj instanceof MailTransport) {
                        	@SuppressWarnings("unused")
                            MailTransport mailTransport = (MailTransport)obj;								
						}							
                    }
				}
			);
    		pool.setTestOnBorrow(true);
    		MAIL_TRANSPORT_POOL.put(
    			mailServiceName,
    			pool
    		);
    	}
    	try {
    		return (MailTransport)MAIL_TRANSPORT_POOL.get(mailServiceName).borrowObject();
    	} catch(Exception e) {
			throw new ServiceException(
				OpenCrxException.DOMAIN,
				BasicException.Code.BIND_FAILURE,
				"Unable to get mail session",
				new BasicException.Parameter("mailService.name", mailServiceName),
				new BasicException.Parameter("mailService.fallback.name", fallbackMailServiceName)
			);
    	}
    }

    /* (non-Javadoc)
     * @see org.opencrx.kernel.backend.Workflows.AsynchronousWorkflow#execute(org.opencrx.kernel.home1.jmi1.WfProcessInstance)
     */
    @Override
    public void execute(
        WfProcessInstance wfProcessInstance
    ) throws ServiceException {        
    	PersistenceManager pm = JDOHelper.getPersistenceManager(wfProcessInstance);
        MailTransport mailTransport = null;
        Address[] recipients = null;
        ContextCapable target = null;
        try {
            Path wfProcessInstanceIdentity = new Path(wfProcessInstance.refMofId());
            String providerName = wfProcessInstanceIdentity.getSegment(2).toClassicRepresentation();
            String segmentName = wfProcessInstanceIdentity.getSegment(4).toClassicRepresentation();
            Map<String,Object> params = WorkflowHelper.getWorkflowParameters(wfProcessInstance); 
            UserHome userHome = (UserHome)pm.getObjectById(wfProcessInstance.refGetPath().getParent().getParent());            
            // Target object
            Path targetIdentity = new Path(wfProcessInstance.getTargetObject());            
            try {
                target = (ContextCapable)pm.getObjectById(targetIdentity);
            } catch(Exception e) {}            
            // Find user's default email account
            EMailAccountQuery emailAccountQuery = (EMailAccountQuery)pm.newQuery(EMailAccount.class);
            emailAccountQuery.thereExistsIsDefault().isTrue();
            emailAccountQuery.thereExistsIsActive().isTrue();
            List<EMailAccount> eMailAccounts = userHome.getEMailAccount(emailAccountQuery);
            EMailAccount eMailAccountUser = eMailAccounts.isEmpty() 
            	? null 
            	: eMailAccounts.iterator().next();
            // Can not send if no email account is configured
            if(eMailAccountUser == null) {
            	throw new ServiceException(
            		BasicException.Code.DEFAULT_DOMAIN,
            		BasicException.Code.ASSERTION_FAILURE,
            		"Unable to send email. No default email account configured",
            		new BasicException.Parameter("user", userHome.refGetPath().toXRI())
            	);
            } else {
                // Send mail
            	boolean useSendmail = System.getProperty("org.opencrx.usesendmail." + providerName + "." + segmentName) == null
            		? System.getProperty("org.opencrx.usesendmail." + providerName) == null
            			? false
            			: Boolean.valueOf(System.getProperty("org.opencrx.usesendmail." + providerName))
            		: Boolean.valueOf(System.getProperty("org.opencrx.usesendmail." + providerName + "." + segmentName));
            	if(useSendmail) {            			
            		// no-op
            	} else {
            		// Use JavaMail service
	                // Try to get mail service name from user's email account
	                String mailServiceName = eMailAccountUser.getOutgoingMailServiceName();                
	                // Try to get mail service name from workflow configuration
	                if(
	                    ((mailServiceName == null) || mailServiceName.isEmpty()) &&
	                    wfProcessInstance.getProcess() != null
	                ) {
	                	mailServiceName = (String)params.get(MailWorkflow.OPTION_MAIL_SERVICE_NAME);
	                }
	                // If not configured take mail/provider/<provider>/segment/<segment> as default
	                if(mailServiceName == null) {
	                    mailServiceName = "/mail/provider/" + providerName + "/segment/" + segmentName;
	                }
	                String fallbackMailServiceName = "/mail/provider/" + providerName;
	                mailTransport = getMailTransport(mailServiceName, fallbackMailServiceName);
	                if(mailTransport == null) {
	                	throw new ServiceException(
	                		BasicException.Code.DEFAULT_DOMAIN,
	                		BasicException.Code.ASSERTION_FAILURE,
	                		"Unable to get mail transport",
	                		new BasicException.Parameter("mailService.name", mailServiceName),
	                		new BasicException.Parameter("fallbackMailService.name", fallbackMailServiceName)
	                	);
	                }
            	}
            	{
                    MimeMessage message = new MimeMessage(
                    	mailTransport == null 
                    		? null 
                    		: mailTransport.getSession()
                    	);
                    message.setSentDate(new Date());
                    message.setHeader(
                    	"X-Mailer", 
                    	"//OPENCRX//V2//" + providerName + "/" + segmentName
                    );
                    String envelopeSenderAddress = mailTransport == null
                    	? System.getProperty("org.opencrx.sendmail.from." + providerName + "." + segmentName) == null
                    		? System.getProperty("org.opencrx.sendmail.from." + providerName) == null
                    			? null
                    			: System.getProperty("org.opencrx.sendmail.from." + providerName)
                    		: System.getProperty("org.opencrx.sendmail.from." + providerName + "." + segmentName)
                    	: mailTransport.getSession().getProperty("mail.from");
                    recipients = this.setRecipients(
                        message,
                        pm,
                        targetIdentity,
                        eMailAccountUser,
                        envelopeSenderAddress
                    );
                    if(recipients.length > 0) {
                        // subject
                    	String subject = null;
                        message.setSubject(
                            subject = Notifications.getInstance().getNotificationSubject(
                                pm,
                                target,
                                wfProcessInstanceIdentity,
                                userHome,
                                params,
                                this.useSendMailSubjectPrefix()
                            ),
                            "UTF-8"
                        );
                        // content
                        String text = this.setContent(
                            message,
                            mailTransport == null 
                            	? null 
                            	: mailTransport.getSession(),
                            pm,
                            targetIdentity,
                            wfProcessInstanceIdentity,
                            userHome,
                            params
                        );
                        message.saveChanges();
                        if(mailTransport == null) {
                        	 SysLog.detail("Send message using sendmail");
                        	 String err = this.doSendmail(message, envelopeSenderAddress);
                        	 if(!err.isEmpty()) {
                        		 throw new SendFailedException(err);
                        	 }
                        } else {
	                        SysLog.detail("Send message using JavaMail");
	                        mailTransport.getTransport().sendMessage(
	                            message,
	                            message.getAllRecipients()
	                        );
                        }
                        SysLog.detail("Done");
                        WorkflowHelper.createLogEntry(
                            wfProcessInstance,
                            subject,
                            text == null ? null : text.indexOf("\n") > 0 ? text.substring(0, text.indexOf("\n")) + "..." : text
                        );
                    } else {
                        // No recipients. Can not send message
                    	throw new ServiceException(
                    		BasicException.Code.DEFAULT_DOMAIN,
                    		BasicException.Code.ASSERTION_FAILURE,
                    		"No recipients"
                    	);
                    }
                }
            }
        } catch(SendFailedException e) {
        	SysLog.warning("Can not send message to recipients (reason=SendFailedException)", recipients == null ? null : Arrays.asList(recipients));
            ServiceException e0 = new ServiceException(e);
            SysLog.detail(e0.getMessage(), e0.getCause());
            Pattern reasonPattern = Pattern.compile(".*(\\d\\.\\d\\.\\d).*");
            Matcher reasonMatcher = reasonPattern.matcher(e.getMessage());
        	String reason = reasonMatcher.matches() ? reasonMatcher.group(1) : "N/A";
        	if(e.getValidUnsentAddresses() != null) {
	            for(Address address: e.getValidUnsentAddresses()) {
	            	this.createLogEntry(
	            		wfProcessInstance, 
	            		String.format("ERROR: Reason is >%s<", reason),
	            		e.getMessage(), 
	            		target, 
	            		address
	            	);
	            }
        	}
        	if(e.getInvalidAddresses() != null) {
	            for(Address address: e.getInvalidAddresses()) {
	            	this.createLogEntry(
	            		wfProcessInstance, 
	            		String.format("ERROR: Reason is >%s<", reason), 
	            		e.getMessage(), 
	            		target, 
	            		address
	            	);
	            }
        	}
            throw e0;
        } catch(AuthenticationFailedException e) {
        	SysLog.warning("Can not send message to recipients (reason=AuthenticationFailedException)", recipients == null ? null : Arrays.asList(recipients));
            ServiceException e0 = new ServiceException(e);
            SysLog.detail(e0.getMessage(), e0.getCause());
            WorkflowHelper.createLogEntry(
                wfProcessInstance,
                "Can not send mail: AuthenticationFailedException",
                e.getMessage()
            );
            throw e0;
        } catch(AddressException e) {
        	SysLog.warning("Can not send message to recipients (reason=AddressException)", recipients == null ? null : Arrays.asList(recipients));
            ServiceException e0 = new ServiceException(e);
            SysLog.detail(e0.getMessage(), e0.getCause());
            WorkflowHelper.createLogEntry(
                wfProcessInstance,
                "Can not send mail: AddressException",
                e.getMessage()
            );
            throw e0;
        } catch(MessagingException e) {
        	SysLog.warning("Can not send message to recipients (reason=MessagingException)", recipients == null ? null : Arrays.asList(recipients));
            ServiceException e0 = new ServiceException(e);
            SysLog.detail(e0.getMessage(), e0.getCause());
            WorkflowHelper.createLogEntry(
                wfProcessInstance,
                "Can not send mail: MessagingException",
                e.getMessage()
            );
            throw e0;
        } catch(Exception e) {
        	SysLog.warning("Can not send message to recipients (reason=Exception)", recipients == null ? null : Arrays.asList(recipients));
            ServiceException e0 = new ServiceException(e);
            SysLog.detail(e0.getMessage(), e0.getCause());
            WorkflowHelper.createLogEntry(
                wfProcessInstance,
                "Can not send mail: Exception",
                e.getMessage()
            );
            throw e0;
        } finally {
        	// Return transport
        	if(mailTransport != null) {
        		ObjectPool mailTransportPool = MAIL_TRANSPORT_POOL.get(mailTransport.getName());
        		if(mailTransportPool != null) {
        			try {
        				mailTransportPool.returnObject(mailTransport);
        			} catch(Exception ignore) {}
        		}
        	}
        }
    }

    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    // TTL for mail transport
    private static final long TRANSPORT_TTL = 60000L; // 1 minute 
    private static final String OPTION_MAIL_SERVICE_NAME = "mailServiceName";
    private final static Map<String,ObjectPool> MAIL_TRANSPORT_POOL = new ConcurrentHashMap<String,ObjectPool>();
}
