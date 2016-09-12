/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: openCRX application plugin
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2007, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.portal;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import javax.jdo.JDOHelper;
import javax.jdo.ObjectState;
import javax.jdo.PersistenceManager;
import javax.jmi.reflect.RefObject;

import org.opencrx.kernel.activity1.jmi1.EMailRecipient;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.text.conversion.UUIDConversion;
import org.openmdx.kernel.id.UUIDs;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.DataBinding;

public class EMailRecipientDataBinding extends DataBinding {

	/**
	 * @param parameterString
	 */
	public EMailRecipientDataBinding(
		String parameterString
	) {
		this.index = 0;
		this.partyType = 0;
		String[] params = parameterString.split(";");
		if(params != null) {
			for(String param: params) {
				String[] nv = param.split("=");
				if(nv != null && nv.length == 2) {
					if("index".equals(nv[0])) {
						this.index = Integer.valueOf(nv[1]);
					}
					else if("partyType".equals(nv[0])) {
						this.partyType = Short.valueOf(nv[1]);
					}
				}
			}
		}		
	}

	/**
	 * Get email recipients.
	 * @param eMailActivity
	 * @return
	 */
	protected List<org.opencrx.kernel.activity1.jmi1.EMailRecipient> getEMailRecipients(
		org.opencrx.kernel.activity1.jmi1.EMail eMailActivity
	) {
		PersistenceManager pm = JDOHelper.getPersistenceManager(eMailActivity);
		Map<String,org.opencrx.kernel.activity1.jmi1.EMailRecipient> allRecipients = new TreeMap<String,org.opencrx.kernel.activity1.jmi1.EMailRecipient>();
		Collection<org.opencrx.kernel.activity1.jmi1.AbstractEMailRecipient> recipients = eMailActivity.getEmailRecipient();
		Date now = new Date();
		// Collect persistent and persistent-new parties and sort
		for(org.opencrx.kernel.activity1.jmi1.AbstractEMailRecipient recipient: recipients) {
			if(recipient instanceof org.opencrx.kernel.activity1.jmi1.EMailRecipient) {
				if(recipient.getPartyType() == this.partyType) {
					allRecipients.put(
						JDOHelper.isNew(recipient) ?
							now.getTime() + ":" + recipient.refGetPath().getBase() :
								recipient.getCreatedAt().getTime() + ":" + recipient.refGetPath().getBase(),
						(org.opencrx.kernel.activity1.jmi1.EMailRecipient)recipient
					);
				}
			}
		}
		// Collect deleted parties and sort
		@SuppressWarnings("unchecked")
        Set<RefObject_1_0> deletedObjects = pm.getManagedObjects(EnumSet.of(ObjectState.PERSISTENT_DELETED));
		for(RefObject_1_0 object: deletedObjects) {
			if(
				(object instanceof org.opencrx.kernel.activity1.jmi1.EMailRecipient) &&
				(object.refGetPath().startsWith(eMailActivity.refGetPath()))
			) {
				org.opencrx.kernel.activity1.jmi1.EMailRecipient recipient = (org.opencrx.kernel.activity1.jmi1.EMailRecipient)object;				
				if(recipient.getPartyType() == this.partyType) {
					allRecipients.put(
						recipient.getCreatedAt().getTime() + ":" + recipient.refGetPath().getBase(),
						recipient
					);
				}
			}
		}
		return new ArrayList<org.opencrx.kernel.activity1.jmi1.EMailRecipient>(allRecipients.values());
	}
	
    /**
     * @return
     */
    protected String uuidAsString(
    ) {
        return UUIDConversion.toUID(UUIDs.newUUID());
    }
    
    /* (non-Javadoc)
     * @see org.openmdx.portal.servlet.DataBinding#getValue(javax.jmi.reflect.RefObject, java.lang.String, org.openmdx.portal.servlet.ApplicationContext)
     */
    @Override
	public Object getValue(
		RefObject object, 
		String qualifiedFeatureName,
		ApplicationContext app
	) {
		if(object instanceof org.opencrx.kernel.activity1.jmi1.EMail) {
			org.opencrx.kernel.activity1.jmi1.EMail eMailActivity = (org.opencrx.kernel.activity1.jmi1.EMail)object;
			List<org.opencrx.kernel.activity1.jmi1.EMailRecipient> recipients = this.getEMailRecipients(eMailActivity);
			return this.index < recipients.size() ?
				recipients.get(this.index) :
					null;
		}
		else {
			return null;
		}
    }

    /* (non-Javadoc)
     * @see org.openmdx.portal.servlet.DataBinding#setValue(javax.jmi.reflect.RefObject, java.lang.String, java.lang.Object, org.openmdx.portal.servlet.ApplicationContext)
     */
    @Override
	public void setValue(
		RefObject object, 
		String qualifiedFeatureName, 
		Object newValue,
		ApplicationContext app
	) {
		if(
			(object instanceof org.opencrx.kernel.activity1.jmi1.EMail) &&
			(newValue == null || newValue instanceof org.opencrx.kernel.account1.jmi1.EMailAddress)
		) {
			org.opencrx.kernel.activity1.jmi1.EMail eMailActivity = (org.opencrx.kernel.activity1.jmi1.EMail)object;
			org.opencrx.kernel.account1.jmi1.EMailAddress eMailAddress = (org.opencrx.kernel.account1.jmi1.EMailAddress)newValue;
			List<EMailRecipient> recipients = this.getEMailRecipients(eMailActivity);
			if(this.index < recipients.size()) {
				org.opencrx.kernel.activity1.jmi1.EMailRecipient recipient = recipients.get(this.index);
				if(newValue == null) {
					if(!JDOHelper.isDeleted(recipient)) {
						recipient.refDelete();
					}
				}
				else {
					recipient.setParty(eMailAddress);
				}
			}
			else if(newValue != null) {
				PersistenceManager pm = JDOHelper.getPersistenceManager(eMailActivity);
				org.opencrx.kernel.activity1.jmi1.EMailRecipient recipient = null;
				for(int i = recipients.size(); i < this.index + 1; i++) {
					recipient = pm.newInstance(org.opencrx.kernel.activity1.jmi1.EMailRecipient.class);
					recipient.setPartyType(this.partyType);
					eMailActivity.addEmailRecipient(
						this.uuidAsString(), 
						recipient
					);
				}
				recipient.setParty(eMailAddress);
			}
		}
    }

    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
	private int index;
	private short partyType;
	
}
