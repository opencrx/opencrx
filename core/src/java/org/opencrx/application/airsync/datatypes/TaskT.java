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
import java.util.List;

public class TaskT implements IData {

	private String body;
	private List<String> categories;
	private Boolean complete;
	private Date datecompleted;
	private Date duedate;
	private Date utcduedate;
	private Importance importance;
	private RecurrenceT recurrence;
	private String reminderset;
	private Date remindertime;
	private Sensitivity sensitivity;
	private Date startdate;
	private Date utcstartdate;
	private String subject;
	private boolean isRead;

	@Override
    public DataType getType(
    ) {
		return DataType.Tasks;
    }

	public String getBody() {
    	return body;
    }

	public void setBody(String body) {
    	this.body = body;
    }

	public List<String> getCategories() {
    	return categories;
    }

	public void setCategories(List<String> categories) {
    	this.categories = categories;
    }

	public Boolean getComplete() {
    	return complete;
    }

	public void setComplete(Boolean complete) {
    	this.complete = complete;
    }

	public Date getDatecompleted() {
    	return datecompleted;
    }

	public void setDatecompleted(Date datecompleted) {
    	this.datecompleted = datecompleted;
    }

	public Date getDuedate() {
    	return duedate;
    }

	public void setDuedate(Date duedate) {
    	this.duedate = duedate;
    }

	public Date getUtcduedate() {
    	return utcduedate;
    }

	public void setUtcduedate(Date utcduedate) {
    	this.utcduedate = utcduedate;
    }

	public Importance getImportance() {
    	return importance;
    }

	public void setImportance(Importance importance) {
    	this.importance = importance;
    }

	public RecurrenceT getRecurrence() {
    	return recurrence;
    }

	public void setRecurrence(RecurrenceT recurrence) {
    	this.recurrence = recurrence;
    }

	public String getReminderset() {
    	return reminderset;
    }

	public void setReminderset(String reminderset) {
    	this.reminderset = reminderset;
    }

	public Date getRemindertime() {
    	return remindertime;
    }

	public void setRemindertime(Date remindertime) {
    	this.remindertime = remindertime;
    }

	public Sensitivity getSensitivity() {
    	return sensitivity;
    }

	public void setSensitivity(Sensitivity sensitivity) {
    	this.sensitivity = sensitivity;
    }

	public Date getStartdate() {
    	return startdate;
    }

	public void setStartdate(Date startdate) {
    	this.startdate = startdate;
    }

	public Date getUtcstartdate() {
    	return utcstartdate;
    }

	public void setUtcstartdate(Date utcstartdate) {
    	this.utcstartdate = utcstartdate;
    }

	public String getSubject() {
    	return subject;
    }

	public void setSubject(String subject) {
    	this.subject = subject;
    }

	@Override
    public boolean isRead(
    ) {
		return this.isRead;
    }
	
}
