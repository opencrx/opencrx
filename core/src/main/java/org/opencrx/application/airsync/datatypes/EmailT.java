/*
 * ====================================================================
 * Project:     openCRX/Application, http://www.opencrx.org/
 * Description: Sync for openCRX
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2010, CRIXP Corp., Switzerland
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
package org.opencrx.application.airsync.datatypes;

import java.util.Date;
import java.util.LinkedList;
import java.util.List;

public class EmailT implements IData {

	private String mimeData;
	private String subject;
	private EmailBodyT body;
	private Date dateReceived;
	private List<AttachmentT> attachements;
	private EventT meetingRequest;
	private MessageClass messageClass;
	private Importance importance;
	private AddressT from;
	private List<AddressT> to;
	private List<AddressT> cc;
	private List<AddressT> bcc;
	private AddressT replyTo;
	private boolean isRead;
	private List<String> categories;	

	@Override
	public DataType getType() {
		return DataType.Email;
	}

	@Override
	public boolean isRead() {
		return isRead;
	}
	
	public void setMimeData(
		String mimeData
	) {
		this.mimeData = mimeData;
	}
	
	public String getMimeData(
	) {
		return this.mimeData;
	}
	
	public AddressT getReplyTo() {
    	return replyTo;
    }

	public void setReplyTo(AddressT replyTo) {
    	this.replyTo = replyTo;
    }

	public String getSubject() {
		return subject;
	}

	public EmailBodyT getBody() {
		return body;
	}

	public AddressT getFrom() {
		return from;
	}

	public void setFrom(AddressT from) {
		this.from = from;
	}

	public Date getDateReceived() {
		return dateReceived;
	}

	public void setDateReceived(Date date) {
		this.dateReceived = date;
	}

	public void setSubject(String subject) {
		this.subject = subject;
	}

	public void setBody(EmailBodyT body) {
		this.body = body;
	}

	public void setTo(List<AddressT> to) {
		if (to != null) {
			this.to = to;
		} else {
			this.to = new LinkedList<AddressT>();
		}
	}

	public List<AddressT> getTo() {
		return to;
	}

	public void setCc(List<AddressT> cc) {
		if (cc != null) {
			this.cc = cc;
		} else {
			this.cc = new LinkedList<AddressT>();
		}
	}

	public List<AddressT> getCc() {
		return cc;
	}

	public List<AddressT> getBcc() {
		return bcc;
	}

	public void setBcc(List<AddressT> bcc) {
		if (bcc != null) {
			this.bcc = bcc;
		} else {
			this.bcc = new LinkedList<AddressT>();
		}
	}

	public EventT getInvitation() {
		return meetingRequest;
	}

	public void setInvitation(EventT invitation) {
		this.meetingRequest = invitation;
		if(this.meetingRequest != null){
			this.messageClass = MessageClass.ScheduleMeetingRequest;
		}
	}

	public void setRead(boolean read) {
		this.isRead = read;
	}

	public MessageClass getMessageClass() {
		return messageClass;
	}

	public void setMessageClass(MessageClass messageClass) {
		this.messageClass = messageClass;
	}
	
	public Importance getImportance() {
		return importance;
	}

	public void setImportance(Importance importance) {
		this.importance = importance;
	}
	
	public List<AttachmentT> getAttachements() {
		return attachements;
	}

	public void setAttachements(List<AttachmentT> attachements) {
		this.attachements = attachements;
	}

	public List<String> getCategories() {
		return categories;
	}

	public void setCategories(List<String> categories) {
		this.categories = categories;
	}
	
}

