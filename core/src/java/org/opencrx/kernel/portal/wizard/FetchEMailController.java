/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: FetchEMailController
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2013, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.portal.wizard;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import javax.jdo.PersistenceManager;
import javax.mail.Flags;
import javax.mail.Folder;
import javax.mail.Message;
import javax.mail.Session;
import javax.mail.Store;
import javax.mail.internet.MimeMessage;
import javax.mail.search.FlagTerm;

import org.opencrx.kernel.activity1.jmi1.ActivityCreator;
import org.opencrx.kernel.activity1.jmi1.EMail;
import org.opencrx.kernel.backend.Activities;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.portal.servlet.AbstractWizardController;
import org.openmdx.portal.servlet.ObjectReference;

/**
 * FetchEMailController
 *
 */
public class FetchEMailController extends AbstractWizardController {

	/**
	 * FormFields
	 *
	 */
	public static class FormFields {
		/**
		 * @return the host
		 */
		public String getHost() {
			return host;
		}
		/**
		 * @param host the host to set
		 */
		public void setHost(String host) {
			this.host = host;
		}
		/**
		 * @return the protocol
		 */
		public String getProtocol() {
			return protocol;
		}
		/**
		 * @param protocol the protocol to set
		 */
		public void setProtocol(String protocol) {
			this.protocol = protocol;
		}
		/**
		 * @return the port
		 */
		public String getPort() {
			return port;
		}
		/**
		 * @param port the port to set
		 */
		public void setPort(String port) {
			this.port = port;
		}
		/**
		 * @return the user
		 */
		public String getUser() {
			return user;
		}
		/**
		 * @param user the user to set
		 */
		public void setUser(String user) {
			this.user = user;
		}
		/**
		 * @return the password
		 */
		public String getPassword() {
			return password;
		}
		/**
		 * @param password the password to set
		 */
		public void setPassword(String password) {
			this.password = password;
		}
		/**
		 * @return the messageCount
		 */
		public String getMessageCount() {
			return messageCount;
		}
		/**
		 * @param messageCount the messageCount to set
		 */
		public void setMessageCount(String messageCount) {
			this.messageCount = messageCount;
		}
		/**
		 * @return the activityCreatorXri
		 */
		public String getActivityCreatorXri() {
			return activityCreatorXri;
		}
		/**
		 * @param activityCreatorXri the activityCreatorXri to set
		 */
		public void setActivityCreatorXri(String activityCreatorXri) {
			this.activityCreatorXri = activityCreatorXri;
		}
		String host;
		String protocol;
		String port;
		String user;
		String password;
		String messageCount;
		String activityCreatorXri;
	}

	/**
	 * ImportedMessage
	 *
	 */
	public static class ImportedMessage {
		
		/**
		 * Constructor.
		 * 
		 * @param mimeMessage
		 * @param emails
		 */
		public ImportedMessage(
			MimeMessage mimeMessage,
			List<EMail> emails
		) {
			this.mimeMessage = mimeMessage;
			this.emails = emails;
		}
		
		/**
		 * @return the mimeMessage
		 */
		public MimeMessage getMimeMessage() {
			return mimeMessage;
		}
		/**
		 * @param mimeMessage the mimeMessage to set
		 */
		public void setMimeMessage(MimeMessage mimeMessage) {
			this.mimeMessage = mimeMessage;
		}
		/**
		 * @return the emails
		 */
		public List<EMail> getEmails() {
			return emails;
		}
		/**
		 * @param emails the emails to set
		 */
		public void setEmails(List<EMail> emails) {
			this.emails = emails;
		}
		
		private MimeMessage mimeMessage;
		private List<EMail> emails;
	}

	/**
	 * Constructor.
	 * 
	 */
	public FetchEMailController(
	) {
		super();
	}
	
	/**
	 * Cancel action.
	 * 
	 */
	public void doCancel(
	) {
		this.setExitAction(
			 new ObjectReference(this.getObject(), this.getApp()).getSelectObjectAction()
		);		
	}

	/**
	 * Refresh action.
	 * 
	 * @param isInitialized
	 * @param formFields
	 */
	public void doRefresh(
		@RequestParameter(name = "isInitialized") Boolean isInitialized,
		@RequestParameter(type = "Bean") FormFields formFields
	) {
		this.formFields = formFields;
		if(!Boolean.TRUE.equals(isInitialized)) {
			if(this.getObject() instanceof ActivityCreator){
				this.formFields.setActivityCreatorXri(
					this.getObjectIdentity().toXRI()
				);
			}
			if(this.formFields.getHost() == null) {
				this.formFields.setHost("server.mail.com");
			}
			if(this.formFields.getProtocol() == null) {
				this.formFields.setProtocol("imap");
			}
			if(this.formFields.getPort() == null) {
				this.formFields.setPort("143");
			}
			if(this.formFields.getUser() == null) {
				this.formFields.setUser("username");
			}
			if(this.formFields.getPassword() == null) {
				this.formFields.setPassword("password");
			}
			if(this.formFields.getMessageCount() == null) {
				this.formFields.setMessageCount("10");
			}
		}
	}
	
	/**
	 * OK action.
	 * 
	 * @param isInitialized
	 * @param formFields
	 */
	public void doOK(
		@RequestParameter(name = "isInitialized") Boolean isInitialized,
		@RequestParameter(type = "Bean") FormFields formFields
	) {
		PersistenceManager pm = this.getPm();
		this.doRefresh(
			isInitialized, 
			formFields
		);
		int maxMessages = 10;
		try {
			maxMessages = Integer.parseInt(this.formFields.getMessageCount());
		} catch (Exception ignore) {}
		try {
			// Connect to my pop3 inbox in read-only mode
			Properties properties = System.getProperties();
			properties.setProperty("mail." + this.formFields.getProtocol() + ".port", this.formFields.getPort());
			Session msession = Session.getDefaultInstance(properties);
			Store store = msession.getStore(this.formFields.getProtocol());
			store.connect(
				this.formFields.getHost(), 
				this.formFields.getUser(), 
				this.formFields.getPassword()
			);
			Folder inbox = store.getFolder("inbox");
			inbox.open(Folder.READ_WRITE);
			Flags seen = new Flags(Flags.Flag.SEEN);
			FlagTerm unseenFlagTerm = new FlagTerm(seen, false);
			Message[] messages = inbox.search(unseenFlagTerm);		
			ActivityCreator activityCreator = null;
			if(this.formFields.getActivityCreatorXri() != null && !this.formFields.getActivityCreatorXri().isEmpty()) {
				try {
					activityCreator = (ActivityCreator)pm.getObjectById(new Path(this.formFields.getActivityCreatorXri()));
				} catch(Exception ignore) {}
			}
			this.importedMessages = new ArrayList<ImportedMessage>();
			for(int i = 0; i < messages.length && i < maxMessages; i++) {
				if(messages[i] instanceof MimeMessage) {
					MimeMessage mimeMessage = (MimeMessage)messages[i];
					try {
						List<EMail> emails = Activities.getInstance().importMimeMessage(
							pm,
							this.getProviderName(),
							this.getSegmentName(),
							mimeMessage,
							activityCreator // maybe null --> default e-mail creator will be used
						);
						try {
							// Mark as read following hint from http://www.jguru.com/faq/view.jsp?EID=305942
							@SuppressWarnings("unused")
                            MimeMessage copy = new MimeMessage(mimeMessage);
						} catch(Exception ignore) {}
						this.importedMessages.add(
							new ImportedMessage(
								mimeMessage,
								emails
							)
						);
					} catch(Exception e) {
						new ServiceException(e).log();
						this.importedMessages.add(
							new ImportedMessage(
								mimeMessage,
								null
							)
						);
					}
				}
			}
		} catch(Exception e) {
			new ServiceException(e).log();
		}
	}

	/**
	 * @return the formFields
	 */
	public FormFields getFormFields() {
		return formFields;
	}

	/**
	 * @return the messages
	 */
	public List<ImportedMessage> getImportedMessages() {
		return this.importedMessages;
	}

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	public static final String ACTIVITYCREATOR_CLASS = "org:opencrx:kernel:activity1:ActivityCreator";
	
	private FormFields formFields;
	private List<ImportedMessage> importedMessages;
}
