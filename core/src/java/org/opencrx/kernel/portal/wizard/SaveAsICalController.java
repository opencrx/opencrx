/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: SaveAsICalController
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

import java.io.FileOutputStream;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.opencrx.kernel.account1.jmi1.Contact;
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.activity1.jmi1.ActivityCategory;
import org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal;
import org.opencrx.kernel.activity1.jmi1.ActivityFilterGroup;
import org.opencrx.kernel.activity1.jmi1.ActivityGroup;
import org.opencrx.kernel.activity1.jmi1.ActivityMilestone;
import org.opencrx.kernel.activity1.jmi1.ActivityTracker;
import org.opencrx.kernel.activity1.jmi1.Resource;
import org.opencrx.kernel.backend.Base;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.portal.servlet.AbstractWizardController;
import org.openmdx.portal.servlet.Action;
import org.openmdx.portal.servlet.ApplicationContext;

/**
 * SaveAsICalController
 *
 */
public class SaveAsICalController extends AbstractWizardController {

	/**
	 * Constructor.
	 * 
	 */
	public SaveAsICalController(
	) {
		super();
	}
	
	/**
	 * Refresh action.
	 * 
	 * @throws ServiceException
	 */
	public void doRefresh(
	) throws ServiceException {
		ApplicationContext app = this.getApp();
		RefObject_1_0 obj = this.getObject();
		String location = Base.getInstance().getUidAsString();
		String downloadFileName = null;
		String mimeType = null;
		boolean hasDownloadFile = false;
		try {
			if(obj instanceof Activity) {
				Activity activity = (Activity)obj;
				downloadFileName = Utils.toFilename(activity.getActivityNumber()) + ".ics";
				mimeType = MIMETYPE_ICAL;	      
				FileOutputStream fileos = new FileOutputStream(app.getTempFileName(location, ""));
				if(activity.getIcal() != null) {
					fileos.write(activity.getIcal().getBytes("UTF-8"));
				}
				fileos.close();
				hasDownloadFile = true;
			} else {
				Collection<Activity> activities = null;
				downloadFileName = null;
				if (
					(obj instanceof ActivityTracker) ||
					(obj instanceof ActivityCategory) ||
					(obj instanceof ActivityMilestone)
				) {
					downloadFileName = ((ActivityGroup)obj).getName();
					activities = ((ActivityGroup)obj).<Activity>getFilteredActivity();
				} else if (obj instanceof ActivityFilterGlobal) {
					downloadFileName = ((ActivityFilterGlobal)obj).getName();
					activities = ((ActivityFilterGlobal)obj).<Activity>getFilteredActivity();
				} else if (obj instanceof ActivityFilterGroup) {
					downloadFileName = ((ActivityFilterGroup)obj).getName();
					activities = ((ActivityFilterGroup)obj).<Activity>getFilteredActivity();
				} else if (obj instanceof Resource) {
					downloadFileName = ((Resource)obj).getName();
					activities = ((Resource)obj).<Activity>getAssignedActivity();
				} else if (obj instanceof UserHome) {
					Contact contact = ((UserHome)obj).getContact();
					if(contact != null) {
						downloadFileName = contact.getFullName();
					} else {
						downloadFileName = obj.refGetPath().getBase();
					}
					activities = ((UserHome)obj).<Activity>getAssignedActivity();
				}
				if(activities != null) {
					downloadFileName = Utils.toFilename(downloadFileName) + ".zip";
					mimeType = MIMETYPE_ZIP;
					FileOutputStream fileos = new FileOutputStream(app.getTempFileName(location, ""));		        
					ZipOutputStream zipos = new ZipOutputStream(fileos);
					Set<String> activityNumbers = new HashSet<String>();
					for(Activity activity: activities) {
						if(!Boolean.TRUE.equals(activity.isDisabled()) && !activityNumbers.contains(activity.getActivityNumber())) {
							String fileName = Utils.toFilename(activity.getActivityNumber()) + ".ics";
							zipos.putNextEntry(new ZipEntry(fileName));
							if(activity.getIcal() != null) {
								zipos.write(activity.getIcal().getBytes("UTF-8"));
							}
							zipos.closeEntry();
							activityNumbers.add(activity.getActivityNumber());
						}
					}
					zipos.close();
					hasDownloadFile = true;
				}
			}
		} catch(Exception e) {
			new ServiceException(e).log();
		}
		if(hasDownloadFile) {
			this.downloadFileAction =
				new Action(
					Action.EVENT_DOWNLOAD_FROM_LOCATION,
					new Action.Parameter[]{
						new Action.Parameter(Action.PARAMETER_LOCATION, location),
						new Action.Parameter(Action.PARAMETER_NAME, downloadFileName),
						new Action.Parameter(Action.PARAMETER_MIME_TYPE, mimeType)
					},
					app.getTexts().getClickToDownloadText() + " " + downloadFileName,
					true
				);
		}
	}

	/**
	 * @return the downloadFileAction
	 */
	public Action getDownloadFileAction() {
		return downloadFileAction;
	}

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	public static final String MIMETYPE_ICAL = "text/calendar";
	public static final String MIMETYPE_ZIP = "application/zip";
	
	private Action downloadFileAction;

}
