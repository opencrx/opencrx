/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: SetTimerOnCrxObjectController
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

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.TimeZone;

import javax.jdo.PersistenceManager;

import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.backend.Base;
import org.opencrx.kernel.backend.UserHomes;
import org.opencrx.kernel.home1.jmi1.Timer;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.openmdx.base.accessor.rest.DirtyObjects;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.portal.servlet.AbstractWizardController;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.ObjectReference;

/**
 * SetTimerOnCrxObjectController
 *
 */
public class SetTimerOnCrxObjectController extends AbstractWizardController {

	/**
	 * Constructor.
	 * 
	 */
	public SetTimerOnCrxObjectController(
	) {
		super();
	}
	
	/**
	 * Return new localized calendar.
	 * 
	 * @return
	 */
	protected GregorianCalendar newCalendar(
	) {
		ApplicationContext app = this.getApp();
		GregorianCalendar cal = new GregorianCalendar(app.getCurrentLocale());
		cal.setTimeZone(TimeZone.getTimeZone(app.getCurrentTimeZone()));
		cal.setMinimalDaysInFirstWeek(4); // this conforms to DIN 1355/ISO 8601
		cal.setFirstDayOfWeek(GregorianCalendar.MONDAY);
		return cal;
	}

	/**
	 * Return new localized calendar and set to given date.
	 * 
	 * @param date
	 * @return
	 */
	protected GregorianCalendar newCalendar(
		Date date
	) {
		GregorianCalendar cal = this.newCalendar();
		cal.setTime(date);
		return cal;
	}

	/**
	 * Increment date by number of minutes.
	 * 
	 * @param date
	 * @param numberOfMinutes
	 * @param app
	 * @return
	 */
	public GregorianCalendar incDate(
		GregorianCalendar cal,
		int numberOfMinutes,
		ApplicationContext app
	) {
		GregorianCalendar newCal = (GregorianCalendar)cal.clone();
		newCal.add(Calendar.MINUTE, numberOfMinutes);
		return newCal;
	}

    /**
     * Return true if user has permission.
     * 
     * @return
     */
    @SuppressWarnings("unused")
    public boolean hasPermission(
	) {
		ApplicationContext app = this.getApp();
  		boolean currentUserIsAdmin = app.getCurrentUserRole().equals(
  			org.opencrx.kernel.generic.SecurityKeys.ADMIN_PRINCIPAL + org.opencrx.kernel.generic.SecurityKeys.ID_SEPARATOR + this.getSegmentName() + "@" + this.getSegmentName()
  		);
		final String currentUserRole = app.getCurrentUserRole();
		boolean permissionOk = true; //currentUserIsAdmin;
		return permissionOk;
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
	 * @param name
	 * @param triggerAt
	 */
	public void doRefresh(
		@RequestParameter(name = "name") String name,
		@RequestParameter(name = "triggerAt") String triggerAt
	) throws ServiceException {
		ApplicationContext app = this.getApp();
		final GregorianCalendar NOW = this.newCalendar();
		this.name = name == null ? app.getLabel(TIMER_CLASS) + " (" + app.getLoginPrincipal() + ")" : name;
		this.triggerAtDateOk = false;
		final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("dd-MM-yyyy HH:mm", app.getCurrentLocale());
		GregorianCalendar proposedTriggerAt = this.incDate(NOW, 15, app);
		if(this.getObject() instanceof Activity) {
			Activity activity = (Activity)this.getObject();
			if(activity.getScheduledStart() != null) {
				proposedTriggerAt = this.incDate(this.newCalendar(activity.getScheduledStart()), -15, app);
				if(proposedTriggerAt.compareTo(NOW) < 0) {
					proposedTriggerAt = this.incDate(NOW, 15, app);
				}
			}
		}
		this.triggerAt = triggerAt == null ? DATE_FORMAT.format(proposedTriggerAt.getTime()) : triggerAt;
		this.triggerAtDate = null;
		try {
	  		if((this.triggerAt != null) && (this.triggerAt.length() == 16)) {
	  			this.triggerAtDate = (GregorianCalendar)NOW.clone();
	  			this.triggerAtDate.set(Calendar.YEAR, Integer.parseInt(this.triggerAt.substring(6, 10)));
	  			this.triggerAtDate.set(Calendar.MONTH, Integer.parseInt(this.triggerAt.substring(3, 5)) - 1);
	  			this.triggerAtDate.set(Calendar.DAY_OF_MONTH, Integer.parseInt(this.triggerAt.substring(0, 2)));
	  			this.triggerAtDate.set(Calendar.HOUR_OF_DAY, Integer.parseInt(this.triggerAt.substring(11, 13)));
	  			this.triggerAtDate.set(Calendar.MINUTE, Integer.parseInt(this.triggerAt.substring(14, 16)));
	  			this.triggerAtDate.set(Calendar.SECOND, 0);
	  			this.triggerAtDateOk = true;
			}
		} catch (Exception e) {}
		this.triggerAtIsInTheFuture = this.triggerAtDateOk && this.triggerAtDate.compareTo(NOW) > 0;		
	}

	/**
	 * OK action.
	 * 
	 * @param name
	 * @param triggerAt
	 * @throws ServiceException
	 */
	public void doOK(
		@RequestParameter(name = "name") String name,
		@RequestParameter(name = "triggerAt") String triggerAt
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		ApplicationContext app = this.getApp();
		this.doRefresh(name, triggerAt);
		if(
			this.hasPermission() && 
			this.triggerAtDateOk &&
			this.name != null && !this.name.isEmpty() &&
			this.triggerAtIsInTheFuture
		) {
			try {
				pm.currentTransaction().begin();
				UserHome currentUserHome = UserHomes.getInstance().getUserHome(this.getObject().refGetPath(), pm, true);		  
				Timer timer = pm.newInstance(Timer.class);
				timer.setName(this.name);
				timer.setTimerStartAt(this.triggerAtDate.getTime());
				timer.setTriggerRepeat(1);
				timer.setTriggerIntervalMinutes(5); /* note that this value MUST be bigger than the ping interval of the subscription handler */
				timer.setDisabled(false);
				timer.setTimerState(new Short((short)10)); // open
				timer.setTimerEndAt(this.incDate(this.triggerAtDate, 60, app).getTime());
				timer.setTarget((org.openmdx.base.jmi1.BasicObject)this.getObject());
				currentUserHome.addTimer(
					Base.getInstance().getUidAsString(),
					timer
				);
	  			pm.currentTransaction().commit();
	  			// Touch in order to properly amend target object
	  			pm.currentTransaction().begin();
	  			DirtyObjects.touch(timer);
	  			pm.currentTransaction().commit();
	  			this.forward(
	  				"Cancel", 
	  				this.getRequest().getParameterMap()
	  			);
			} catch(Exception e) {
				try {
					pm.currentTransaction().rollback();
				} catch(Exception ignore) {}
			}
		}
	}

	/**
	 * @return the triggerAtDate
	 */
	public GregorianCalendar getTriggerAtDate() {
		return triggerAtDate;
	}

	/**
	 * @return the triggerAtDateOk
	 */
	public boolean isTriggerAtDateOk() {
		return triggerAtDateOk;
	}

	/**
	 * @return the triggerAtIsInTheFuture
	 */
	public boolean isTriggerAtIsInTheFuture() {
		return triggerAtIsInTheFuture;
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * @return the triggerAt
	 */
	public String getTriggerAt() {
		return triggerAt;
	}

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	public static final String TIMER_CLASS = "org:opencrx:kernel:home1:Timer";	
	
	private GregorianCalendar triggerAtDate;
	private boolean triggerAtDateOk;
	private boolean triggerAtIsInTheFuture;
	private String name;
	private String triggerAt;
}
