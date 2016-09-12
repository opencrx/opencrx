/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: CreateCampaignWizardController
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
package org.opencrx.kernel.portal.wizard;

import java.io.ByteArrayOutputStream;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.opencrx.kernel.account1.cci2.AccountFilterGlobalQuery;
import org.opencrx.kernel.account1.jmi1.AbstractFilterAccount;
import org.opencrx.kernel.account1.jmi1.AccountFilterGlobal;
import org.opencrx.kernel.account1.jmi1.AccountQueryFilterProperty;
import org.opencrx.kernel.activity1.cci2.ActivityGroupRelationshipQuery;
import org.opencrx.kernel.activity1.jmi1.ActivityCreator;
import org.opencrx.kernel.activity1.jmi1.ActivityGroup;
import org.opencrx.kernel.activity1.jmi1.ActivityGroupRelationship;
import org.opencrx.kernel.activity1.jmi1.ActivityTracker;
import org.opencrx.kernel.activity1.jmi1.ActivityType;
import org.opencrx.kernel.backend.Accounts;
import org.opencrx.kernel.backend.Activities;
import org.opencrx.kernel.backend.Activities.ActivityClass;
import org.opencrx.kernel.backend.Cloneable;
import org.opencrx.kernel.backend.UserHomes;
import org.opencrx.kernel.document1.jmi1.Document;
import org.opencrx.kernel.document1.jmi1.MediaContent;
import org.opencrx.kernel.portal.StringPropertyDataBinding;
import org.opencrx.kernel.utils.QueryBuilderUtil;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.ObjectReference;
import org.openmdx.portal.servlet.ViewPort;
import org.openmdx.portal.servlet.ViewPortFactory;
import org.openmdx.portal.servlet.component.TransientObjectView;
import org.w3c.cci2.BinaryLargeObjects;

/**
 * CreateCampaignWizardController
 *
 */
public class CreateCampaignWizardController extends org.openmdx.portal.servlet.AbstractWizardController {

	/**
	 * Constructor.
	 * 
	 */
	public CreateCampaignWizardController(
   	) {
   		super();
   	}
	
	/**
	 * Create campaign.
	 * 
	 * @param activityType
	 * @param name
	 * @param description
	 * @return
	 * @throws ServiceException
	 */
	public static ActivityTracker createCampaign(
		ActivityType activityType,
		String name,
		String description,
		AbstractFilterAccount targetGroupAccountsSelector,
		ActivityTracker campaignTrackerMain,
		boolean initCampaignActivityCreator
	) throws ServiceException {
		PersistenceManager pm = JDOHelper.getPersistenceManager(activityType);
		String providerName = activityType.refGetPath().getSegment(2).toClassicRepresentation();
		String segmentName = activityType.refGetPath().getSegment(4).toClassicRepresentation();
		org.opencrx.kernel.activity1.jmi1.Segment activitySegment = Activities.getInstance().getActivitySegment(pm, providerName, segmentName);
		org.opencrx.security.realm1.jmi1.PrincipalGroup usersPrincipalGroup =
			(org.opencrx.security.realm1.jmi1.PrincipalGroup)org.opencrx.kernel.backend.SecureObject.getInstance().findPrincipal(
				"Users",
				org.opencrx.kernel.backend.SecureObject.getInstance().getRealm(
					pm,
					providerName,
					segmentName
				)
			);
		org.opencrx.security.realm1.jmi1.PrincipalGroup administratorsPrincipalGroup =
			(org.opencrx.security.realm1.jmi1.PrincipalGroup)org.opencrx.kernel.backend.SecureObject.getInstance().findPrincipal(
				"Administrators",
				org.opencrx.kernel.backend.SecureObject.getInstance().getRealm(
					pm,
					providerName,
					segmentName
				)
			);
		List<org.opencrx.security.realm1.jmi1.PrincipalGroup> allUsers = new ArrayList<org.opencrx.security.realm1.jmi1.PrincipalGroup>();
		allUsers.add(usersPrincipalGroup);
		allUsers.add(administratorsPrincipalGroup);
        ActivityTracker campaignTracker = Activities.getInstance().initActivityTracker(
            name,
            allUsers,
            activitySegment
        );
    	// ActivityCreator for specified activityType
        ActivityCreator campaignActivityCreator = null;
        if(initCampaignActivityCreator) {
	    	campaignActivityCreator = Activities.getInstance().initActivityCreator(
	    	    name + " - " + activityType.getName(),
	    	    activityType,
	    	    campaignTrackerMain == null 
	    	    	? Arrays.asList(new ActivityGroup[]{campaignTracker})
	    	    	: Arrays.asList(new ActivityGroup[]{campaignTracker, campaignTrackerMain}),
	    	    allUsers
	    	);
        }
    	// Update creator / tracker
        pm.currentTransaction().begin();
    	campaignTracker.setDescription(description);
    	campaignTracker.setActivityGroupType(Activities.ActivityGroupType.CAMPAIGN.getValue());    	
    	if(campaignTracker.getDefaultCreator() == null) {
    		campaignTracker.setDefaultCreator(campaignActivityCreator);
    	}
    	campaignTracker.setTargetGroupAccounts(targetGroupAccountsSelector);
    	pm.currentTransaction().commit();
    	return campaignTracker;
	}

	/**
	 * Get selectable locale codes for campaign.
	 * 
	 * @return
	 * @throws ServiceException
	 */
	public Map<Short,String> getLocaleCodes(
	) throws ServiceException {
		final Map<Short,String> locales = this.getCodes().getLongTextByCode("locale", (short)0, false);
		Map<Short,String> sortedLocales = new TreeMap<Short,String>(
			new Comparator<Short>(){
				@Override
				public int compare(Short o1, Short o2) {
					return locales.get(o1).trim().compareTo(locales.get(o2).trim());
				}			
			}
		);
		sortedLocales.putAll(locales);
		return sortedLocales;
	}

	/**
	 * Map locale to language code.
	 * 
	 * @param locale
	 * @return
	 */
	public short mapLocaleToLanguageCode(
		short locale
	) {
		String localeShortText = this.getCodes().getShortTextByCode("locale", (short)0, true).get(locale);
		return this.getCodes().findCodeFromValue("[" + localeShortText.substring(0, 2), "language");
	}

	/**
	 * Get predicate which restricts accounts to locale[index]. By default
	 * a predicate of the form 'preferredWrittenLanguage = locale[index].language'
	 * is returned.
	 * 
	 * @param index
	 * @param locales
	 * @return
	 * @throws ServiceException
	 */
	public QueryBuilderUtil.Predicate getRestrictAccountsToLocalePredicate(
		int index,
		List<Short> locales
	) throws ServiceException {
		if(locales.size() <= 1) {
			return null;			
		} else if(index > 0) {
			return new QueryBuilderUtil.SingleValuedAttributePredicate(
				Utils.getUidAsString(), 
				null, // description 
				"org:opencrx:kernel:account1:Contact:preferredWrittenLanguage", 
				QueryBuilderUtil.SingleValuedAttributePredicate.Condition.IS_IN,
				"(" + this.mapLocaleToLanguageCode(locales.get(index)) + ")"
			);
		} else {
			String otherLanguages = Short.toString(this.mapLocaleToLanguageCode(locales.get(1)));
			for(int i = 2; i < locales.size(); i++) {
				otherLanguages += "," + this.mapLocaleToLanguageCode(locales.get(i));
			}
			return new QueryBuilderUtil.SingleValuedAttributePredicate(
				Utils.getUidAsString(), 
				null, // description 
				"org:opencrx:kernel:account1:Contact:preferredWrittenLanguage", 
				QueryBuilderUtil.SingleValuedAttributePredicate.Condition.IS_NOT_IN,
				"(" + otherLanguages + ")"
			);
		}
	}

	/**
	 * Init target group accounts selector for given locale based on existing targetGroupAccountsSelector.
	 * 
	 * @param accountSegment
	 * @param targetGroupAccountsSelector
	 * @param localeIndex
	 * @param selectedLocales
	 * @return
	 * @throws ServiceException
	 */
	public AccountFilterGlobal initTargetGroupAccountsSelector(
		org.opencrx.kernel.account1.jmi1.Segment accountSegment,
		AbstractFilterAccount targetGroupAccountsSelector,
		int localeIndex,
		List<Short> selectedLocales
	) throws ServiceException {
		PersistenceManager pm = JDOHelper.getPersistenceManager(accountSegment);
		AccountFilterGlobal targetGroupAccountsSelectorByLocale = null;		
		if(targetGroupAccountsSelector instanceof AccountFilterGlobal) {
			short locale = selectedLocales.get(localeIndex);
			Map<Short,String> localeShortTexts = this.getCodes().getShortTextByCode("locale", (short)0, true);			
			AccountFilterGlobal source = (AccountFilterGlobal)targetGroupAccountsSelector;
			String targetGroupAccountsSelectorByLocaleName = source.getName() + " [" + localeShortTexts.get(locale) + "]";
			AccountFilterGlobalQuery accountFilterQuery = (AccountFilterGlobalQuery)pm.newQuery(AccountFilterGlobal.class);
			accountFilterQuery.name().equalTo(targetGroupAccountsSelectorByLocaleName);
			List<AccountFilterGlobal> accountFilters = accountSegment.getAccountFilter(accountFilterQuery);
			if(accountFilters.isEmpty()) {
    			// Clone the targetGroupAccountsSelector and add a predicate which restricts to locale
    			pm.currentTransaction().begin();
    			targetGroupAccountsSelectorByLocale = (AccountFilterGlobal)Cloneable.getInstance().cloneObject(
    	    		source,
    	    		accountSegment,
    	    		source.refGetPath().getParent().getLastSegment().toClassicRepresentation(),
    	    		null, // objectMarshallers
    	    		null, // referenceFilterAsString
    	    		UserHomes.getInstance().getUserHome(
    	    			accountSegment.refGetPath(),
    	    			pm
    	    		).getOwningUser(), // get owner from current user's home
    	    		source.getOwningGroup()
    	    	);
    			pm.currentTransaction().commit();
    			pm.currentTransaction().begin();
    			targetGroupAccountsSelectorByLocale.setName(targetGroupAccountsSelectorByLocaleName);
    			QueryBuilderUtil.Predicate restrictAccountsToLocalePredicate = this.getRestrictAccountsToLocalePredicate(
    				localeIndex,
    				selectedLocales
    			);
    			if(restrictAccountsToLocalePredicate != null) {
	    			AccountQueryFilterProperty restrictAccountsToLocaleFilterProperty = pm.newInstance(AccountQueryFilterProperty.class);
	    			restrictAccountsToLocaleFilterProperty.setName("Restrict to locale " + localeShortTexts.get(locale));
	    			restrictAccountsToLocaleFilterProperty.setActive(true);
	    			restrictAccountsToLocaleFilterProperty.setClause(
	    				"/* <pre> */ \n" +
	    				restrictAccountsToLocalePredicate.toSql(
	    					"", 
	    					accountSegment.refGetPath().getDescendant("account"), 
	    					"v"
	    				) +
	    				"/* </pre> */ \n"	    				
	    			);
	    			targetGroupAccountsSelectorByLocale.addAccountFilterProperty(
	    				Utils.getUidAsString(),
	    				restrictAccountsToLocaleFilterProperty
	    			);
    			}
    			pm.currentTransaction().commit();
			} else {
				targetGroupAccountsSelectorByLocale = accountFilters.iterator().next();
			}
		}
		return targetGroupAccountsSelectorByLocale;
	}

	/**
	 * Init campaign e-mail message with given document.
	 * 
	 * @param document
	 * @param campaignTracker
	 * @throws ServiceException
	 */
	protected void initCampaignEMailMessage(
		Document document,
		ActivityTracker campaignTracker
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		ApplicationContext app = this.getApp();
		if(
			document.getContentType() != null &&
			"text/html".equalsIgnoreCase(document.getContentType()) && 
			document.getHeadRevision() instanceof MediaContent &&
			campaignTracker.getActivityCreator().size() == 1
		) {
			ActivityCreator campaignCreator = campaignTracker.<ActivityCreator>getActivityCreator().iterator().next();
			if(campaignCreator.getActivityType().getActivityClass() == ActivityClass.EMAIL.getValue()) {
				MediaContent headRevision = (MediaContent)document.getHeadRevision();
				try {
					ByteArrayOutputStream bos = new ByteArrayOutputStream();
					BinaryLargeObjects.streamCopy(headRevision.getContent().getContent(), 0L, bos);
					String messageSubject = campaignTracker.getDescription() == null
						? campaignTracker.getName()
						: campaignTracker.getDescription();
					String messageBody = new String(bos.toByteArray(), "UTF-8");
					if(
						messageBody.indexOf("<html>") < 0 && 
						messageBody.indexOf("<head>") < 0 && 
						messageBody.indexOf("<body>") < 0
					) {
						messageBody = 
							"<!DOCTYPE html>\n" + 
							"<html>\n" + 
							"  <head>\n" + 
							"    <meta content=\"text/html; charset=\"utf-8\" http-equiv=\"content-type\">\n" + 
							"    <title>" + app.getHtmlEncoder().encode(messageSubject, false) + "</title>\n" + 
							"  </head>\n" + 
							"  <body>\n" +
							messageBody +
							"  </body>\n" +
							"</html>";
					}
					pm.currentTransaction().begin();
					StringPropertyDataBinding stringPropertyDataBinding = new StringPropertyDataBinding();
					stringPropertyDataBinding.setValue(
						campaignCreator,
						":" + BulkActivityManagerApiController.PROPERTY_SET_NAME_SETTINS + "." + this.selectedLocales.get(0) + "!messageSubject", 
						messageSubject
					);
					// Set message body. Split into pieces of 2048 chars
					List<String> messageBodyParts = Utils.splitString(messageBody, 2048);
					int idx = 0;
					for(int j = 0; j < messageBodyParts.size(); j++) {
						try {
							stringPropertyDataBinding.setValue(
								campaignCreator, 
								":" + BulkActivityManagerApiController.PROPERTY_SET_NAME_SETTINS + "." + this.selectedLocales.get(0) + "!messageBody" + j, 
								messageBodyParts.get(j)
							);
							idx++;
						} catch (Exception e) {
							new ServiceException(e).log();
						}
					}
					// Reset unused messageBody properties
					try {
						while (stringPropertyDataBinding.getValue(campaignCreator, ":" + BulkActivityManagerApiController.PROPERTY_SET_NAME_SETTINS + "." + selectedLocales.get(0) + "!messageBody" + idx) != null) {
							org.opencrx.kernel.base.jmi1.Property property = stringPropertyDataBinding.findProperty(
								campaignCreator, 
								":" + BulkActivityManagerApiController.PROPERTY_SET_NAME_SETTINS + "." + this.selectedLocales.get(0) + "!messageBody" + idx
							);
							property.refDelete();
							idx++;
						}
					} catch (Exception e) {
						new ServiceException(e).log();
					}
					pm.currentTransaction().commit();
				} catch(Exception e) {
					new ServiceException(e).log();
					try {
						pm.currentTransaction().rollback();
					} catch(Exception ignore) {}
				}
			}
		}
	}

	/**
	 * OK action.
	 * 
	 * @param formFields
	 * @throws ServiceException
	 */
	public void doOK(
		@RequestParameter(name = "locale0") String locale0,
		@RequestParameter(name = "locale1") String locale1,		
		@RequestParameter(name = "locale2") String locale2,		
		@RequestParameter(name = "locale3") String locale3,		
		@RequestParameter(name = "locale4") String locale4,
		@FormParameter(forms = "CreateCampaignForm") Map<String,Object> formFields
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		ApplicationContext app = this.getApp();
		this.formFields = formFields;
	    String name = (String)formFields.get("org:opencrx:kernel:activity1:ActivityGroup:name");
	    name = name == null ? null : name.trim();
	    String description = (String)formFields.get("org:opencrx:kernel:activity1:ActivityGroup:description");
	    ActivityType activityType = formFields.get("org:opencrx:kernel:activity1:ActivityCreator:activityType") != null ?
   	    	(ActivityType)pm.getObjectById(
     	    	formFields.get("org:opencrx:kernel:activity1:ActivityCreator:activityType")
     	    ) : null;
	    AbstractFilterAccount targetGroupAccountsSelector = formFields.get("org:opencrx:kernel:activity1:ActivityGroup:targetGroupAccounts") != null ?
   	    	(AbstractFilterAccount)pm.getObjectById(
     	    	formFields.get("org:opencrx:kernel:activity1:ActivityGroup:targetGroupAccounts")
     	    ) : null;
    	this.selectedLocales = new ArrayList<Short>(); // language 0 is the default language
    	if(locale0 != null) {
    		try {
    			this.selectedLocales.add(Short.parseShort(locale0));
    		} catch(Exception ignore) {}
    	}
    	if(locale1 != null) {
    		try {
    			this.selectedLocales.add(Short.parseShort(locale1));
    		} catch(Exception ignore) {}
    	}
    	if(locale2 != null) {
    		try {
    			this.selectedLocales.add(Short.parseShort(locale2));
    		} catch(Exception ignore) {}
    	}
    	if(locale3 != null) {
    		try {
    			this.selectedLocales.add(Short.parseShort(locale3));
    		} catch(Exception ignore) {}
    	}
    	if(locale4 != null) {
    		try {
    			this.selectedLocales.add(Short.parseShort(locale4));
    		} catch(Exception ignore) {}
    	}
    	if(this.selectedLocales.isEmpty()) {
    		this.selectedLocales.add((short)0); // en_US
    	}
    	if(name == null || name.isEmpty()) {
    		this.errorMessage += "The field 'Name' is mandatory";
    	}
    	if(activityType == null) {
    		this.errorMessage += "<br />The field 'Activity type' is mandatory";
    	}
	    if(name != null && !name.isEmpty() && activityType != null) {
	    	Map<Short,String> localeShortTexts = this.getCodes().getShortTextByCode("locale", (short)0, true);
	    	org.opencrx.kernel.account1.jmi1.Segment accountSegment = Accounts.getInstance().getAccountSegment(
	    		pm, 
	    		this.getProviderName(), 
	    		this.getSegmentName()
	    	);
	    	ActivityTracker campaignTrackerMain = null;
	    	if(this.selectedLocales.size() > 1) {
	    		campaignTrackerMain = createCampaign(
		    		activityType,
		    		name,
		    		description,
		    		null, // targetGroupAccountsSelector
		    		null, // campaignTrackerMain
		    		false // initCampaignActivityCreator
		    	);
	    	}
	    	// A campaign is created for each locale. This includes an activity tracker, activity creator
	    	// and target group selector.
	    	for(int i = 0; i < this.selectedLocales.size(); i++) {
	    		short locale = this.selectedLocales.get(i);
	    		ActivityTracker campaignTracker = null;
	    		if(this.selectedLocales.size() == 1) {
			    	campaignTracker = createCampaign(
			    		activityType,
			    		name,
			    		description,
			    		targetGroupAccountsSelector,
			    		null, // campaignTrackerMain
			    		true // initActivityCreator
			    	);
			    	campaignTrackerMain = campaignTracker;
			    	if(this.getObject() instanceof Document) {
			    		this.initCampaignEMailMessage(
			    			(Document)this.getObject(),
			    			campaignTracker
			    		);
			    	}
	    		} else {
		    		AccountFilterGlobal targetGroupAccountsSelectorByLocale = this.initTargetGroupAccountsSelector(
		    			accountSegment, 
		    			targetGroupAccountsSelector, 
		    			i, 
		    			this.selectedLocales
		    		);
			    	campaignTracker = createCampaign(
			    		activityType,
			    		name + " [" + localeShortTexts.get(locale) + "]",
			    		description,
			    		targetGroupAccountsSelectorByLocale,
			    		campaignTrackerMain,
			    		true // initActivityCreator
			    	);
			    	// Relationships main campaign <-> campaign
			    	{
			    		// Relationship main campaign --> campaign
			    		ActivityGroupRelationshipQuery activityGroupRelationshipQuery = (ActivityGroupRelationshipQuery)pm.newQuery(ActivityGroupRelationship.class);
			    		activityGroupRelationshipQuery.thereExistsActivityGroup().equalTo(campaignTracker);
			    		if(campaignTrackerMain.getActivityGroupRelationship(activityGroupRelationshipQuery).isEmpty()) {
			    			ActivityGroupRelationship activityGroupRelationship = pm.newInstance(ActivityGroupRelationship.class);
			    			activityGroupRelationship.setName(campaignTracker.getName());
			    			activityGroupRelationship.setRelationshipType((short)0); // none
			    			activityGroupRelationship.setActivityGroup(campaignTracker);
			    			pm.currentTransaction().begin();
			    			campaignTrackerMain.addActivityGroupRelationship(
			    				Utils.getUidAsString(),
			    				activityGroupRelationship
			    			);
			    			pm.currentTransaction().commit();
			    		}
			    		// Relationship campaign --> main campaign
			    		activityGroupRelationshipQuery = (ActivityGroupRelationshipQuery)pm.newQuery(ActivityGroupRelationship.class);
			    		activityGroupRelationshipQuery.thereExistsActivityGroup().equalTo(campaignTrackerMain);
			    		if(campaignTracker.getActivityGroupRelationship(activityGroupRelationshipQuery).isEmpty()) {
			    			ActivityGroupRelationship activityGroupRelationship = pm.newInstance(ActivityGroupRelationship.class);
			    			activityGroupRelationship.setName(campaignTrackerMain.getName());
			    			activityGroupRelationship.setRelationshipType((short)0); // none
			    			activityGroupRelationship.setActivityGroup(campaignTrackerMain);
			    			pm.currentTransaction().begin();
			    			campaignTracker.addActivityGroupRelationship(
			    				Utils.getUidAsString(),
			    				activityGroupRelationship
			    			);
			    			pm.currentTransaction().commit();
			    		}
			    	}
			    	// Copy document for first locale
			    	if(i == 0) {
			    		if(this.getObject() instanceof Document) {
				    		this.initCampaignEMailMessage(
				    			(Document)this.getObject(),
				    			campaignTracker
				    		);
			    		}
			    	}
	    		}
		    	if(campaignTracker.getDefaultCreator() != null) {
		    		pm.currentTransaction().begin();
			    	BulkActivityManagerApiController.initSettings(
			    		campaignTracker.getDefaultCreator(), 
			    		locale
			    	);
			    	pm.currentTransaction().commit();
		    	}
	    	}
	    	// Forward to main tracker
	    	this.setExitAction(
		    	new ObjectReference(campaignTrackerMain, app).getSelectObjectAction()
		    );
			return;
	    }
	}

   	/**
   	 * Cancel action.
   	 * 
   	 * @throws ServiceException
   	 */
   	public void doCancel(
	) throws ServiceException {
		this.setExitAction(
			new ObjectReference(this.getObject(), this.getApp()).getSelectObjectAction()
		);
	}

   	/**
   	 * Get form values.
   	 * 
   	 * @return
   	 */
   	public Map<String,Object> getFormFields(
   	) {
   		return this.formFields;
   	}
   	
	/**
	 * Get view port.
	 * 
	 * @param out
	 * @return
	 */
	public ViewPort getViewPort(
		Writer out
	) {
		if(this.viewPort == null) {
			TransientObjectView view = new TransientObjectView(
				this.getFormFields(),
				this.getApp(),
				this.getObject(),
				this.getPm()
			);
			this.viewPort = ViewPortFactory.openPage(
				view,
				this.getRequest(),
				out
			);			
		}
		return this.viewPort;
	}

	/* (non-Javadoc)
	 * @see org.openmdx.portal.servlet.AbstractWizardController#close()
	 */
    @Override
    public void close(
    ) throws ServiceException {
	    super.close();
	    if(this.viewPort != null) {
	    	this.viewPort.close(false);
	    }
    }

	/**
	 * @return the selectedLocales
	 */
	public List<Short> getSelectedLocales() {
		return selectedLocales;
	}
	
	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------	
	private Map<String,Object> formFields;
	private ViewPort viewPort;
	private List<Short> selectedLocales;
}
