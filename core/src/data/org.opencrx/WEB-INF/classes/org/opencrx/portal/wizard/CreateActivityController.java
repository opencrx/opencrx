/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: CreateActivityController
 * Owner:       the original authors.
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
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
 * * Neither the name of the openCRX team nor the names of the contributors
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
package org.opencrx.portal.wizard;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.jdo.PersistenceManager;

import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.account1.jmi1.Contact;
import org.opencrx.kernel.activity1.jmi1.Activity;
import org.opencrx.kernel.activity1.jmi1.ActivityCreator;
import org.opencrx.kernel.activity1.jmi1.ActivityGroup;
import org.opencrx.kernel.activity1.jmi1.ActivityGroupAssignment;
import org.opencrx.kernel.activity1.jmi1.ActivityLinkTo;
import org.opencrx.kernel.activity1.jmi1.NewActivityParams;
import org.opencrx.kernel.backend.Activities;
import org.opencrx.kernel.backend.ICalendar;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.portal.servlet.Action;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.JsfWizardController;
import org.openmdx.portal.servlet.ObjectReference;
import org.w3c.spi2.Datatypes;

import jakarta.enterprise.context.SessionScoped;
import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.inject.Named;

/**
 * CreateActivityController
 *
 */
@SessionScoped
@Named
public class CreateActivityController extends JsfWizardController implements Serializable {

	private static final long serialVersionUID = -1961913364421129475L;

	public CreateActivityController(
	) {
	}

	/* (non-Javadoc)
	 * @see org.openmdx.portal.servlet.JsfWizardController#newData()
	 */
	public Map<String,Object> newData(
	) throws ServiceException {
		javax.jdo.PersistenceManager pm = this.getPm();
		Path path = this.getObjectIdentity();
		Map<String,Object> data = new HashMap<String,Object>();
		RefObject_1_0 obj = (RefObject_1_0)pm.getObjectById(path);
		if(obj instanceof ActivityCreator) {
			JsfWizardController.ObjectReferenceBean activityCreatorBean = this.newObjectReferenceBean(obj);
			data.put("activityCreator", activityCreatorBean);
			data.put("findActivityCreatorResult", Arrays.asList(activityCreatorBean));
		} else { 
			data.put("activityCreator", new JsfWizardController.ObjectReferenceBean());
		}
		data.put("assignedTo", new JsfWizardController.ObjectReferenceBean());
		data.put("reportingContact", new JsfWizardController.ObjectReferenceBean());
		data.put("reportingAccount", new JsfWizardController.ObjectReferenceBean());
		data.put("activityGroup1", new JsfWizardController.ObjectReferenceBean());
		data.put("activityGroup2", new JsfWizardController.ObjectReferenceBean());
		data.put("activityGroup3", new JsfWizardController.ObjectReferenceBean());
		data.put("priority", Activities.Priority.NORMAL.getValue());
		return data;
	}

	/**
	 * doCreate action.
	 * 
	 * @return
	 * @throws ServiceException
	 */
	public void doCreate(
   		jakarta.faces.event.AjaxBehaviorEvent event
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		ApplicationContext app = this.getApp();
		Path path = this.getObjectIdentity();
		Map<String,Object> dataBean = this.getData();
		RefObject_1_0 obj = (RefObject_1_0)pm.getObjectById(path);
		boolean allMandatoryFieldsSet = true;
		List<String> errors = new ArrayList<String>();
		// Check name
		try {
			String name = String.class.cast(dataBean.get("name"));
			if(name == null || name.isEmpty()) {
				allMandatoryFieldsSet = false;
				errors.add(
					this.createErrorMessage(
						app.getTexts().getErrorTextMandatoryField(),
		            	new String[]{this.getLabel("org:opencrx:kernel:activity1:Activity:name")}
					)
	          	);
			}
		} catch(Exception ignore) {}
		// Check ActivityCreator
		try {
			JsfWizardController.ObjectReferenceBean activityCreator = JsfWizardController.ObjectReferenceBean.class.cast(dataBean.get("activityCreator"));
			if(activityCreator == null || activityCreator.getXri() == null || activityCreator.getXri().isEmpty()) {
				allMandatoryFieldsSet = false;
				errors.add(
					this.createErrorMessage(
						app.getTexts().getErrorTextMandatoryField(),
		            	new String[]{this.getLabel("org:opencrx:kernel:activity1:Activity:lastAppliedCreator")}
		         	 )
				);
			}
		} catch(Exception ignore) {}
		if(allMandatoryFieldsSet) {
			try {
				String name = String.class.cast(dataBean.get("name"));
				Contact reportingContact = null;
				try {
					reportingContact = (Contact)pm.getObjectById(
						new Path(JsfWizardController.ObjectReferenceBean.class.cast(dataBean.get("reportingContact")).getXri())
					);
				} catch (Exception ignore) {}
				Account reportingAccount = null;
				try {
					reportingAccount = (Account)pm.getObjectById(
						new Path(JsfWizardController.ObjectReferenceBean.class.cast(dataBean.get("reportingAccount")).getXri())
					);
				} catch (Exception ignore) {}
				Contact assignedTo = null;
				try {
					assignedTo = (Contact)pm.getObjectById(
						new Path(JsfWizardController.ObjectReferenceBean.class.cast(dataBean.get("assignedTo")).getXri())
					);
				} catch (Exception ignore) {}
				ActivityCreator activityCreator = null;
				try {
					activityCreator = (ActivityCreator)pm.getObjectById(
						new Path(JsfWizardController.ObjectReferenceBean.class.cast(dataBean.get("activityCreator")).getXri())
					);
				} catch(Exception ignore) {}
				short priority = dataBean.get("priority") == null 
					? Activities.Priority.NORMAL.getValue() 
					: (Short)dataBean.get("priority");
				Date dueBy = Date.class.cast(dataBean.get("dueBy"));
				Date scheduledStart = Date.class.cast(dataBean.get("scheduledStart"));
				Date scheduledEnd = Date.class.cast(dataBean.get("scheduledEnd"));
				String misc1 = String.class.cast(dataBean.get("misc1"));
				String misc2 = String.class.cast(dataBean.get("misc2"));
				String misc3 = String.class.cast(dataBean.get("misc3"));
				String description = String.class.cast(dataBean.get("description"));
				String detailedDescription = String.class.cast(dataBean.get("detailedDescription"));
				if(
					(name != null) &&
					(name.trim().length() > 0) &&
					(dataBean.get("activityCreator") != null)
				) {
					org.opencrx.kernel.activity1.jmi1.NewActivityParams params = org.w3c.spi2.Structures.create(
						NewActivityParams.class,
						Datatypes.member(NewActivityParams.Member.description, description),
						Datatypes.member(NewActivityParams.Member.detailedDescription, detailedDescription),
						Datatypes.member(NewActivityParams.Member.dueBy, dueBy),
						Datatypes.member(NewActivityParams.Member.name, name),
						Datatypes.member(NewActivityParams.Member.priority, priority),
						Datatypes.member(NewActivityParams.Member.reportingContact, reportingContact),
						Datatypes.member(NewActivityParams.Member.scheduledEnd, scheduledEnd),
						Datatypes.member(NewActivityParams.Member.scheduledStart, scheduledStart),
						Datatypes.member(NewActivityParams.Member.icalType, ICalendar.ICAL_TYPE_NA)   							
					);
					pm.currentTransaction().begin();
					org.opencrx.kernel.activity1.jmi1.NewActivityResult result = activityCreator.newActivity(params);
					pm.currentTransaction().commit();
					Activity newActivity = (Activity)pm.getObjectById(result.getActivity().refGetPath());
					pm.currentTransaction().begin();
					newActivity.setMisc1(misc1);
					newActivity.setMisc2(misc2);
					newActivity.setMisc3(misc3);
					newActivity.setReportingAccount(reportingAccount);
					if (assignedTo != null) {
						newActivity.setAssignedTo(assignedTo);
					}
					pm.currentTransaction().commit();
					// Create new ActivityLinkTo
	                if(obj instanceof Activity) {
	                	try {
							pm.currentTransaction().begin();
							ActivityLinkTo activityLinkTo = pm.newInstance(ActivityLinkTo.class);
							activityLinkTo.setLinkTo(newActivity);
							activityLinkTo.setName(name);
							activityLinkTo.setActivityLinkType(Activities.ActivityLinkType.RELATES_TO.getValue());
							((Activity)obj).addActivityLinkTo(
								org.opencrx.kernel.backend.Base.getInstance().getUidAsString(),
								activityLinkTo
							);
							pm.currentTransaction().commit();
						} catch (Exception e) {
							try {
								pm.currentTransaction().rollback();
							} catch (Exception er) {}
						}
	                }
	                List<ActivityGroup> activityGroups = new ArrayList<ActivityGroup>();
					try {
						activityGroups.add(
							(ActivityGroup)pm.getObjectById(
								new Path(JsfWizardController.ObjectReferenceBean.class.cast(dataBean.get("activityGroup1")).getXri())
							)
						);
					} catch(Exception ignore) {}
					try {
						activityGroups.add(
							(ActivityGroup)pm.getObjectById(
								new Path(JsfWizardController.ObjectReferenceBean.class.cast(dataBean.get("activityGroup2")).getXri())
							)
						);
					} catch(Exception ignore) {}
					try {
						activityGroups.add(
							(ActivityGroup)pm.getObjectById(
								new Path(JsfWizardController.ObjectReferenceBean.class.cast(dataBean.get("activityGroup3")).getXri())
							)
						);
					} catch(Exception ignore) {}
					for(ActivityGroup activityGroup: activityGroups) {
    					// Verify that this group has not been added already
    					boolean alreadyAssigned = false;
						for(ActivityGroupAssignment assignment: newActivity.<ActivityGroupAssignment>getAssignedGroup()) {
							try {
								if (
									assignment.getActivityGroup() != null &&
									assignment.getActivityGroup().refGetPath().equals(activityGroup.refGetPath())
								) {
									alreadyAssigned = true;
									break;
								}
							} catch (Exception e) {
								new ServiceException(e).log();
							}
						}
						if(!alreadyAssigned) {
							try {
								pm.currentTransaction().begin();
								ActivityGroupAssignment agass = pm.newInstance(ActivityGroupAssignment.class);
								agass.setActivityGroup(activityGroup);
								newActivity.addAssignedGroup(
							        false,
							        Utils.getUidAsString(),
							        agass
							    );
								pm.currentTransaction().commit();
							} catch (Exception e) {
								try {
									pm.currentTransaction().rollback();
								} catch (Exception er) {}
							}
						}
	                }
	                dataBean.put("activity", this.newObjectReferenceBean(result.getActivity()));
				}
			} catch (Exception e) {
				new ServiceException(e).log();
				try {
					Throwable root = e;  
					while (root.getCause() != null) {  
					    root = root.getCause();  
					}
					errors.add(root.toString());
				} catch (Exception e0) {}
				try {
					pm.currentTransaction().rollback();
				} catch (Exception er) {}
			}
		}
		if(errors.isEmpty()) {
	   		try {
	   			Action exitAction = new ObjectReference(
	   				(Activity)pm.getObjectById(new Path(JsfWizardController.ObjectReferenceBean.class.cast(dataBean.get("activity")).getXri())),
	   				this.getApp()
	   			).getSelectObjectAction();
	   			ExternalContext externalContext = FacesContext.getCurrentInstance().getExternalContext();
	   			externalContext.redirect(
	   				externalContext.getRequestContextPath() + "/" + exitAction.getEncodedHRef()
	   			);
	   		} catch(Exception e) {
	   			throw new ServiceException(e);
	   		}
		} else {
			dataBean.put("errors", errors);
		}
	}
   	
}
