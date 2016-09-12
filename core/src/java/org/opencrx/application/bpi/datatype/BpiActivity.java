/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: BpiActivity
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
package org.opencrx.application.bpi.datatype;

import java.util.Date;
import java.util.List;

/**
 * BpiActivity
 *
 */
public class BpiActivity extends BpiObject {

	public String getActivityNumber() {
		return activityNumber;
	}
	public void setActivityNumber(String activityNumber) {
		this.activityNumber = activityNumber;
	}
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public String getDescription() {
		return description;
	}
	public void setDescription(String description) {
		this.description = description;
	}
	public String getAdditionalInformation() {
		return additionalInformation;
	}
	public void setAdditionalInformation(String additionalInformation) {
		this.additionalInformation = additionalInformation;
	}
	public Date getScheduledStart() {
		return scheduledStart;
	}
	public void setScheduledStart(Date scheduledStart) {
		this.scheduledStart = scheduledStart;
	}
	public Date getScheduledEnd() {
		return scheduledEnd;
	}
	public void setScheduledEnd(Date scheduledEnd) {
		this.scheduledEnd = scheduledEnd;
	}
	public Short getActivityState() {
		return activityState;
	}
	public void setActivityState(Short activityState) {
		this.activityState = activityState;
	}
	public String getLocation() {
		return location;
	}
	public void setLocation(String location) {
		this.location = location;
	}
	public List<String> getCategory() {
		return category;
	}
	public void setCategory(List<String> category) {
		this.category = category;
	}
	public List<BpiParticipant> getParticipant() {
		return participant;
	}
	public void setParticipant(List<BpiParticipant> participant) {
		this.participant = participant;
	}
	/**
	 * @return the localizedField
	 */
	public List<BpiLocalizedField> getLocalizedField() {
		return localizedField;
	}
	/**
	 * @param localizedField the localizedField to set
	 */
	public void setLocalizedField(List<BpiLocalizedField> localizedField) {
		this.localizedField = localizedField;
	}
	/**
	 * @return the reportingContact
	 */
	public BpiContact getReportingContact() {
		return reportingContact;
	}
	/**
	 * @param reportingContact the reportingContact to set
	 */
	public void setReportingContact(BpiContact reportingContact) {
		this.reportingContact = reportingContact;
	}
	/**
	 * @return the assignedTo
	 */
	public BpiContact getAssignedTo() {
		return assignedTo;
	}
	/**
	 * @param assignedTo the assignedTo to set
	 */
	public void setAssignedTo(BpiContact assignedTo) {
		this.assignedTo = assignedTo;
	}
	/**
	 * @return the processState
	 */
	public String getProcessState() {
		return processState;
	}
	/**
	 * @param processState the processState to set
	 */
	public void setProcessState(String processState) {
		this.processState = processState;
	}
	
	/**
	 * @return the reportingContact2
	 */
	public BpiContact getReportingContact2() {
		return reportingContact2;
	}
	/**
	 * @param reportingContact2 the reportingContact2 to set
	 */
	public void setReportingContact2(BpiContact reportingContact2) {
		this.reportingContact2 = reportingContact2;
	}

	private String activityNumber;
	private String name;
	private String description;
	private String additionalInformation;
	private Date scheduledStart;
	private Date scheduledEnd;
	private Short activityState;
	private String processState;
	private String location;
	private BpiContact reportingContact;
	private BpiContact reportingContact2;
	private BpiContact assignedTo;
	private List<String> category;
	private List<BpiParticipant> participant;
	private List<BpiLocalizedField> localizedField;
	
}
