/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: ExportMailWorkflow
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

import java.util.Date;
import java.util.Map;

import javax.jdo.PersistenceManager;
import javax.mail.BodyPart;
import javax.mail.Message;
import javax.mail.Multipart;
import javax.mail.Part;
import javax.mail.Session;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeMultipart;

import org.opencrx.kernel.backend.Activities;
import org.opencrx.kernel.backend.Notifications;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.jmi1.ContextCapable;
import org.openmdx.base.naming.Path;

/**
 * ExportMailWorkflow
 *
 */
public class ExportMailWorkflow extends MailWorkflow {

    /* (non-Javadoc)
     * @see org.opencrx.application.mail.exporter.MailWorkflow#setContent(javax.mail.Message, javax.mail.Session, javax.jdo.PersistenceManager, org.openmdx.base.naming.Path, org.openmdx.base.naming.Path, org.opencrx.kernel.home1.jmi1.UserHome, java.util.Map)
     */
    @Override
    protected String setContent(
        Message message,
        Session session,
        PersistenceManager pm,
        Path targetIdentity,
        Path wfProcessInstanceIdentity,
        UserHome userHome,
        Map<String,Object> params
    ) throws ServiceException {
        String text = null;
        try {
            ContextCapable target = null;
            try {
                target = (ContextCapable)pm.getObjectById(targetIdentity);
            } catch(Exception e) {}
            text = Notifications.getInstance().getNotificationText(
                pm,
                target,
                wfProcessInstanceIdentity,
                userHome,
                params
            );
            String providerName = wfProcessInstanceIdentity.get(2);
            String segmentName = wfProcessInstanceIdentity.get(4);
            message.setText(text);
            // message text
            Multipart multipart = new MimeMultipart();
            BodyPart bodyPart = new MimeBodyPart();
            bodyPart.setText(text);            
            multipart.addBodyPart(bodyPart);            

            // Add EMail activity as nested message
            if(target instanceof org.opencrx.kernel.activity1.jmi1.EMail) {
                org.opencrx.kernel.activity1.jmi1.EMail emailActivity = 
                    (org.opencrx.kernel.activity1.jmi1.EMail)target;
                bodyPart = new MimeBodyPart();
                bodyPart.setFileName(
                    emailActivity.getMessageSubject() + ".msg"
                );
                // nested message
                MimeMessage nestedMessage = new MimeMessage(session);
                nestedMessage.setHeader(
                	"X-Mailer", 
                	"//OPENCRX//V2//" + providerName + "/" + segmentName
                );
                // nested message subject
                nestedMessage.setSubject(
                    emailActivity.getMessageSubject(),
                    "UTF-8"
                );
                Activities.getInstance().mapMessageContent(
                    emailActivity,
                    nestedMessage
                );                
                nestedMessage.setSentDate(
                    new Date()
                );
                Activities.getInstance().mapMessageRecipients(
                    emailActivity, 
                    nestedMessage
                );
                bodyPart.setDisposition(
                    Part.ATTACHMENT
                );
                bodyPart.setContent(
                    nestedMessage,
                    "message/rfc822"
                );
                multipart.addBodyPart(bodyPart);
            }
            message.setContent(multipart);
        } catch(Exception e) {
            throw new ServiceException(e);
        }
        return text;
    }

	/* (non-Javadoc)
	 * @see org.opencrx.application.mail.exporter.MailWorkflow#useSendMailSubjectPrefix()
	 */
	@Override
    boolean useSendMailSubjectPrefix(
    ) {
	    return false;
    }
        
    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    
}
