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
package org.opencrx.application.caldav;

import java.io.ByteArrayOutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.util.Date;

import javax.servlet.http.HttpServletRequest;

import org.opencrx.application.uses.net.sf.webdav.RequestContext;
import org.opencrx.application.uses.net.sf.webdav.WebDavStore;
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.backend.ICalendar;
import org.opencrx.kernel.home1.jmi1.Timer;
import org.w3c.cci2.BinaryLargeObject;
import org.w3c.cci2.BinaryLargeObjects;

/**
 * WebDAV resource of type Activity.
 *
 */
class ActivityResource extends CalDavResource {

	public ActivityResource(
		RequestContext requestContext,
		Activity activity,
		ActivityCollectionResource parent
	) {
		super(requestContext, activity);
		this.parent = parent;
	}

	/* (non-Javadoc)
	 * @see org.opencrx.application.caldav.CalDavResource#getObject()
	 */
	@Override
    public Activity getObject(
    ) {
        return (Activity)super.getObject();
    }

	/* (non-Javadoc)
	 * @see org.opencrx.application.uses.net.sf.webdav.Resource#isCollection()
	 */
	@Override
    public boolean isCollection(
    ) {
		return false;
    }
	
	/* (non-Javadoc)
	 * @see org.opencrx.application.caldav.CalDavResource#getMimeType()
	 */
	@Override 
	public String getMimeType(
	) {
		return "text/calendar; component=" + (this.parent.getType() == ActivityCollectionResource.Type.VTODO ? "vtodo" : "vevent");			
	}
	
    /* (non-Javadoc)
     * @see org.opencrx.application.caldav.CalDavResource#getName()
     */
    @Override
    public String getName(
    ) {
        return super.getName() + ".ics";
    }

    /* (non-Javadoc)
     * @see org.opencrx.application.uses.net.sf.webdav.Resource#getDisplayName()
     */
    @Override
    public String getDisplayName(
    ) {
    	return this.getObject().getName();
    }
    
    /* (non-Javadoc)
	 * @see org.opencrx.application.caldav.CalDavResource#getLastModified()
	 */
    @Override
    public Date getLastModified(
    ) {
	    Date lastModifiedAt = super.getLastModified();
	    Activity activity = this.getObject();
	    if(activity.getIcal().indexOf(ICalendar.X_OPENCRX_RENDER_ALARMS_TRUE) > 0) {
		    // Because VALERTs are mixed-in, the modification date of an ICAL must be calculated. 
		    // It is the latest modification date of the ICAL itself and of its assigned alarms.
		    for(Timer timer: this.getObject().<Timer>getAssignedTimer()) {
		    	if(timer.getModifiedAt().compareTo(lastModifiedAt) > 0) {
		    		lastModifiedAt = timer.getModifiedAt();
		    	}
		    }
	    }
	    return lastModifiedAt;
    }

	/**
     * Get parent collection of this activity resource.
     * @return
     */
    public ActivityCollectionResource getSyncFeedResource(
    ) {
    	return this.parent;
    }
    
	/* (non-Javadoc)
	 * @see org.opencrx.application.caldav.CalDavResource#getContent()
	 */
	@Override
    public WebDavStore.ResourceContent getContent(
    ) {
		final ByteArrayOutputStream out = new ByteArrayOutputStream();
		PrintWriter pw = null;
		try {
			pw = new PrintWriter(
				new OutputStreamWriter(out, "UTF-8")
			);
		} catch(Exception e) {
			pw = new PrintWriter(out);
		}
		HttpServletRequest req = this.getRequestContext().getHttpServletRequest();
		try {
			ICalendar.getInstance().printCalendar(
				pw, 
				this.getObject(), 
				this.parent.getQueryHelper(),
				this.parent.getRunAs(), 
				false, // eventsOnly 
				req,
				"-caldav-"
			);
		} catch(Exception ignore) {}
		pw.close();
		return new WebDavStore.ResourceContent() {
			@Override
			public Long getLength() {
				return Long.valueOf(out.size());
			}
			@Override
			public BinaryLargeObject getContent() {
				return  BinaryLargeObjects.valueOf(out.toByteArray());
			}
		};
    }

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	private final ActivityCollectionResource parent;
	
}
