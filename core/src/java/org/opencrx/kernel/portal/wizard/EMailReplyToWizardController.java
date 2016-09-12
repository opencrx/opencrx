/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: EMailReplyToWizardController
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

import java.util.Collection;

import javax.jdo.PersistenceManager;

import org.opencrx.kernel.account1.jmi1.AccountAddress;
import org.opencrx.kernel.activity1.jmi1.ActivityLinkTo;
import org.opencrx.kernel.activity1.jmi1.EMail;
import org.opencrx.kernel.activity1.jmi1.EMailRecipient;
import org.opencrx.kernel.activity1.jmi1.NewActivityParams;
import org.opencrx.kernel.activity1.jmi1.NewActivityResult;
import org.opencrx.kernel.backend.Accounts;
import org.opencrx.kernel.backend.Activities;
import org.opencrx.kernel.backend.UserHomes;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.portal.servlet.AbstractWizardController;
import org.openmdx.portal.servlet.ObjectReference;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

/**
 * EMailReplyToWizardController
 *
 */
public class EMailReplyToWizardController extends AbstractWizardController {

	/**
	 * Constructor.
	 * 
	 */
	public EMailReplyToWizardController(
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
			new ObjectReference(getObject(), getApp()).getSelectObjectAction()
		);		
	}
	
	/**
	 * Refresh action.
	 * 
	 * @throws ServiceException
	 */
	public void doRefresh(
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		if(this.getObject() instanceof EMail) {
			EMail eMailActivity = (EMail)this.getObject();
			if(eMailActivity.getLastAppliedCreator() != null) {
				NewActivityParams params = Structures.create(
					NewActivityParams.class,
					Datatypes.member(NewActivityParams.Member.creationContext, eMailActivity.getCreationContext()),
					Datatypes.member(NewActivityParams.Member.description, eMailActivity.getDescription()),
					Datatypes.member(NewActivityParams.Member.detailedDescription, eMailActivity.getDetailedDescription()),
					Datatypes.member(NewActivityParams.Member.dueBy, eMailActivity.getDueBy()),
					Datatypes.member(NewActivityParams.Member.icalType, eMailActivity.getIcalType()),
					Datatypes.member(NewActivityParams.Member.name, "RE: " + eMailActivity.getName()),
					Datatypes.member(NewActivityParams.Member.priority, eMailActivity.getPriority()),
					Datatypes.member(NewActivityParams.Member.reportingContact, eMailActivity.getReportingContact()),
					Datatypes.member(NewActivityParams.Member.scheduledEnd, eMailActivity.getScheduledEnd()),
					Datatypes.member(NewActivityParams.Member.scheduledStart, eMailActivity.getScheduledStart())
				);
				pm.currentTransaction().begin();
				NewActivityResult result = eMailActivity.getLastAppliedCreator().newActivity(params);
				pm.currentTransaction().commit();
				if(result.getActivity() != null) {
					EMail replyToEMailActivity = (EMail)result.getActivity();
				 	pm.refresh(replyToEMailActivity);
				 	pm.currentTransaction().begin();
				 	// Subject
				 	replyToEMailActivity.setMessageSubject("RE: " + eMailActivity.getMessageSubject());
				 	// Body
				 	if(eMailActivity.getMessageBody() != null) {
					 	StringBuilder newMessageBody = new StringBuilder();
						String[] messageLines = eMailActivity.getMessageBody().split("\n");
						for(String messageLine: messageLines) {
							newMessageBody.append("> " + messageLine + "\n");
						}
					 	replyToEMailActivity.setMessageBody(newMessageBody.toString());
				 	}
				 	// Gateway
				 	replyToEMailActivity.setGateway(eMailActivity.getGateway());
				 	// TO:, CC: Recipients --> CC: Recipients
				 	Collection<EMailRecipient> recipients = eMailActivity.getEmailRecipient();
				 	for(EMailRecipient recipient: recipients) {
				 		if(
				 			recipient.getPartyType() == Activities.PartyType.EMAIL_TO.getValue() ||
				 			recipient.getPartyType() == Activities.PartyType.EMAIL_CC.getValue()
				 		) {
					 		EMailRecipient newRecipient = pm.newInstance(EMailRecipient.class);
					 		newRecipient.setParty(recipient.getParty());
					 		newRecipient.setPartyType(Activities.PartyType.EMAIL_CC.getValue());
					 		replyToEMailActivity.addEmailRecipient(
					 			org.opencrx.kernel.backend.Base.getInstance().getUidAsString(),
					 			newRecipient
					 		);			 			
				 		}
				 	}
				 	// Sender --> TO: recipient
				 	if(eMailActivity.getSender() != null) {
				 		EMailRecipient newRecipient = pm.newInstance(EMailRecipient.class);
				 		newRecipient.setParty(eMailActivity.getSender());
				 		newRecipient.setPartyType(Activities.PartyType.EMAIL_TO.getValue());
				 		replyToEMailActivity.addEmailRecipient(
				 			org.opencrx.kernel.backend.Base.getInstance().getUidAsString(),
				 			newRecipient
				 		);
				 	}
				 	// Current user --> Sender
				 	UserHome userHome = UserHomes.getInstance().getUserHome(
				 		eMailActivity.refGetPath(), 
				 		pm
				 	);
				 	AccountAddress[] accountAddresses = Accounts.getInstance().getMainAddresses(userHome.getContact());
				 	if(accountAddresses != null) {
						replyToEMailActivity.setSender(accountAddresses[Accounts.MAIL_BUSINESS]);			 				
				 	}
				 	// Set link
				 	ActivityLinkTo linkTo = pm.newInstance(ActivityLinkTo.class);
				 	linkTo.setName("#" + eMailActivity.getActivityNumber() + ": " + eMailActivity.getName());
				 	linkTo.setActivityLinkType((short)97); // is derived from
				 	linkTo.setLinkTo(eMailActivity);
				 	replyToEMailActivity.addActivityLinkTo(
				 		org.opencrx.kernel.backend.Base.getInstance().getUidAsString(),
				 		linkTo
				 	);
				 	pm.currentTransaction().commit();
					this.setExitAction(
						new ObjectReference(replyToEMailActivity, getApp()).getSelectObjectAction()
					);						 	
				}
			} else {
				this.forward(
					"Cancel",
					this.getRequest().getParameterMap()
				);				
			}
		} else {
			this.forward(
				"Cancel",
				this.getRequest().getParameterMap()
			);
		}
	}

}
