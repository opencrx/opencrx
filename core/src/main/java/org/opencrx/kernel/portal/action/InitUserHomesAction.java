/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: ApplyDefaultUserSettingsAction
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
package org.opencrx.kernel.portal.action;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.StringTokenizer;

import javax.jdo.PersistenceManager;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.opencrx.kernel.account1.jmi1.AccountAddress;
import org.opencrx.kernel.account1.jmi1.EMailAddress;
import org.opencrx.kernel.backend.Accounts;
import org.opencrx.kernel.backend.UserHomes;
import org.opencrx.kernel.backend.Workflows;
import org.opencrx.kernel.generic.SecurityKeys;
import org.opencrx.kernel.home1.cci2.EMailAccountQuery;
import org.opencrx.kernel.home1.cci2.SubscriptionQuery;
import org.opencrx.kernel.home1.jmi1.EMailAccount;
import org.opencrx.kernel.home1.jmi1.Subscription;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.opencrx.kernel.workflow1.cci2.TopicQuery;
import org.opencrx.kernel.workflow1.jmi1.Topic;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.portal.servlet.Action;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.UserSettings;
import org.openmdx.portal.servlet.ViewsCache;
import org.openmdx.portal.servlet.WebKeys;
import org.openmdx.portal.servlet.action.ActionPerformResult;
import org.openmdx.portal.servlet.action.BoundAction;
import org.openmdx.portal.servlet.attribute.ObjectReferenceValue;
import org.openmdx.portal.servlet.component.Grid;
import org.openmdx.portal.servlet.component.ObjectView;
import org.openmdx.portal.servlet.component.ReferencePane;
import org.openmdx.portal.servlet.component.ShowObjectView;
import org.openmdx.portal.servlet.component.UiGrid;

/**
 * ApplyDefaultUserSettingsAction
 *
 */
public class InitUserHomesAction extends BoundAction {

	/**
	 * Constructor.
	 * 
	 */
	public InitUserHomesAction(
	) {
	}
	
	/* (non-Javadoc)
	 * @see org.openmdx.portal.servlet.action.BoundAction#perform(org.openmdx.portal.servlet.view.ObjectView, javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse, java.lang.String, javax.servlet.http.HttpSession, java.util.Map, org.openmdx.portal.servlet.ViewsCache, org.openmdx.portal.servlet.ViewsCache)
	 */
	@Override
    public ActionPerformResult perform(
        ObjectView view,
        HttpServletRequest request,
        HttpServletResponse response,        
        String parameter,
        HttpSession session,
        Map<String,String[]> requestParameters,
        ViewsCache editViewsCache,
        ViewsCache showViewsCache      
    ) throws IOException, ServletException {
        ApplicationContext app = view.getApplicationContext();
        if(view instanceof ShowObjectView) {
            ShowObjectView currentView = (ShowObjectView)view;    	
        	PersistenceManager pm = app.getNewPmData();
	    	try {
	            int paneIndex = -1;
	            try { 
	            	paneIndex = Integer.parseInt(requestParameters.get(Action.PARAMETER_PANE)[0]);
	            } catch(Exception e) {}
	            int referenceIndex = -1;
	            try {
	            	referenceIndex = Integer.parseInt(requestParameters.get(Action.PARAMETER_REFERENCE)[0]);
	            } catch(Exception e) {}
	            List<ReferencePane> referencePanes = currentView.getChildren(ReferencePane.class);
	            if(paneIndex < referencePanes.size()) {
	                currentView.selectReferencePane(paneIndex);
	                referencePanes.get(paneIndex).selectReference(referenceIndex);
	                Grid grid = referencePanes.get(paneIndex).getGrid();
	                if(grid instanceof UiGrid) {
	                	UiGrid uiGrid = (UiGrid)grid;
	                	List<Path> selectedObjectIdentities = new ArrayList<Path>();
	                    StringTokenizer tokenizer = new StringTokenizer(parameter, " ");
	                    while(tokenizer.hasMoreTokens()) {
	                    	try {
		                        selectedObjectIdentities.add(
		                        	new Path(Action.getParameter(tokenizer.nextToken(), Action.PARAMETER_OBJECTXRI))
		                        );
	                    	} catch(Exception e) {}
	                    }
	                    int maxItems = 500; // default maxItems
	                    try {
	                    	maxItems = Integer.parseInt(requestParameters.get(Action.PARAMETER_SIZE)[0]);
	                    } catch(Exception e) {}
	                    Collection<Path> userHomeIdentities = new ArrayList<Path>();
	                    // Set alert states
	            		if(maxItems < Integer.MAX_VALUE) {
            				// Selected objects only
            				if(selectedObjectIdentities != null && !selectedObjectIdentities.isEmpty()) {
            					for(Path identity: selectedObjectIdentities) {
            						userHomeIdentities.add(identity);
            					}
            				} else {
            					// Current page
            					List<UiGrid.GridRow> rows = uiGrid.getRows(pm);
            					for(UiGrid.GridRow row: rows) {
            						List<Object> cells = row.getCells();
            						if(cells != null && !cells.isEmpty()) {
            							Object object = ((ObjectReferenceValue)cells.get(0)).getObject();
            							if(object instanceof RefObject_1_0) {
	            							userHomeIdentities.add(((RefObject_1_0)object).refGetPath());
            							}
            						}
            					}
            				}
            			}
	                    for(Path userHomeIdentity: userHomeIdentities) {
	            			String providerName = userHomeIdentity.getSegment(2).toString();
	            			String segmentName = userHomeIdentity.getSegment(4).toString();
	            			// Assert that current user is segment administrator
	            			if(app.getCurrentUserRole().equals(SecurityKeys.ADMIN_PRINCIPAL + SecurityKeys.ID_SEPARATOR + segmentName + "@" + segmentName)) {
	            				PersistenceManager pmUser = null;
	    	            		try {
									String userPrincipalName = userHomeIdentity.getLastSegment().toString();
									pmUser = pm.getPersistenceManagerFactory().getPersistenceManager(
										userPrincipalName,
										null
									);
									pmUser.currentTransaction().begin();
									UserHome userHome = (UserHome)pmUser.getObjectById(userHomeIdentity);
									Properties userSettings = UserHomes.getInstance().getDefaultUserSettings(userHome);
									// admin settings are prefixed with a * and override the default and user settings
									Properties adminSettings = new Properties();
									for(Iterator<Map.Entry<Object,Object>> i = userSettings.entrySet().iterator(); i.hasNext(); ) {
										Map.Entry<Object,Object> e = i.next();
										String key = (String)e.getKey();
										if(key.startsWith("*")) {
											adminSettings.put(
												key.substring(1), 
												e.getValue()
											);
											i.remove();
										}
									}
									boolean currentUserOwnsHome = app.getCurrentUserRole().equals(userPrincipalName + "@" + segmentName);
									if(currentUserOwnsHome) {
										userSettings.putAll(app.getSettings());
									} else if(userHome.getSettings() != null) {
										Properties settings = new Properties();
										settings.load(
											new ByteArrayInputStream(
												userHome.getSettings().getBytes("UTF-8")
											)
										);
										userSettings.putAll(settings);
									}
									userSettings.putAll(adminSettings);
									String settingTimezone = userSettings.getProperty(UserSettings.TIMEZONE_NAME.getName());
									String settingStoreSettingsOnLogoff = userHome.isStoreSettingsOnLogoff() != null && userHome.isStoreSettingsOnLogoff().booleanValue() ? "true" : "false";
									EMailAccountQuery emailAccountQuery = (EMailAccountQuery)pmUser.newQuery(EMailAccount.class);
									emailAccountQuery.thereExistsIsActive().isTrue();
									emailAccountQuery.thereExistsIsDefault().isTrue();
									List<EMailAccount> emailAccounts = userHome.getEMailAccount(emailAccountQuery);
									EMailAccount defaultEmailAccount = emailAccounts.isEmpty() ? null : emailAccounts.iterator().next();
									String settingEmailAccount = (defaultEmailAccount == null || defaultEmailAccount.getName() == null ? "" : defaultEmailAccount.getName());
									if(settingEmailAccount.isEmpty() && userHome.getContact() != null) {
										AccountAddress[] mainAddresses = Accounts.getInstance().getMainAddresses(userHome.getContact());
										if(mainAddresses[Accounts.MAIL_BUSINESS] != null) {
											settingEmailAccount = ((EMailAddress)mainAddresses[Accounts.MAIL_BUSINESS]).getEmailAddress();
										}
									}
									String settingSendmailSubjectPrefix = (userHome.getSendMailSubjectPrefix() == null ? "[" + providerName + ":" + segmentName + "]" : userHome.getSendMailSubjectPrefix());
									String settingWebAccessUrl = (userHome.getWebAccessUrl() == null ? request.getRequestURL().substring(0, request.getRequestURL().indexOf("/" + WebKeys.SERVLET_NAME)) : userHome.getWebAccessUrl());
									String settingTopNavigationShowMax = userSettings.getProperty(UserSettings.TOP_NAVIGATION_SHOW_MAX.getName(), "6");
									Boolean settingHideWorkspaceDashboard = Boolean.valueOf(userSettings.getProperty(UserSettings.HIDE_WORKSPACE_DASHBOARD.getName()));
									Boolean settingAnchorUserDialog = Boolean.valueOf(userSettings.getProperty(UserSettings.ANCHOR_USER_DIALOG.getName()));
									Action[] rootObjectActions = app.getRootObjectActions();
									List<String> settingRootObjects = new ArrayList<String>();
									// Root objects
									{
										settingRootObjects.add("1");
										int n = 1;
										for(int i = 1; i < rootObjectActions.length; i++) {
											Action action = rootObjectActions[i];
											if(action.getParameter(Action.PARAMETER_REFERENCE).length() == 0) {
												String state = (userSettings.getProperty(UserSettings.ROOT_OBJECT_STATE.getName() + (app.getCurrentPerspective() == 0 ? "" : "[" + Integer.toString(app.getCurrentPerspective()) + "]") + "." + n + ".State", "1").equals("1") ? "1" : "0");
												if(i < app.getRootObject().length && app.getRootObject()[i] instanceof org.opencrx.kernel.home1.jmi1.UserHome) {
													state = "1";
												}
												settingRootObjects.add(
													state == null ? "0" : state
												);
												n++;
											}
										}
									}
									// Topic subscriptions
									Map<String,String> settingSubscriptions = new HashMap<String,String>();
									{
										TopicQuery topicQuery = (TopicQuery)pmUser.newQuery(Topic.class);
										topicQuery.orderByName().ascending();
										topicQuery.forAllDisabled().isFalse();
										org.opencrx.kernel.workflow1.jmi1.Segment workflowSegment = Workflows.getInstance().getWorkflowSegment(pm, providerName, segmentName);
										for(Topic topic: workflowSegment.getTopic(topicQuery)) {
											SubscriptionQuery query = (SubscriptionQuery)pm.newQuery(Subscription.class);
											query.thereExistsTopic().equalTo(topic);
											List<Subscription> subscriptions = userHome.getSubscription(query);
											Subscription subscription = subscriptions.isEmpty() ? null : subscriptions.iterator().next();
											if(subscription != null) {
												Set<Short> eventTypes = new HashSet<Short>();
												eventTypes.addAll(subscription.getEventType());
												String topicId = topic.refGetPath().getLastSegment().toString();
												if(Boolean.TRUE.equals(subscription.isActive())) {
													settingSubscriptions.put("topicIsActive-" + topicId, "1");
												}
												if(eventTypes.contains(Short.valueOf((short)1))) {
													settingSubscriptions.put("topicCreation-" + topicId, "1");
												}
												if(eventTypes.contains(Short.valueOf((short)3))) {
													settingSubscriptions.put("topicReplacement-" + topicId, "3");
												}
												if(eventTypes.contains(Short.valueOf((short)4))) {
													settingSubscriptions.put("topicRemoval-" + topicId, "4");
												}
											}
										}
									}
									System.out.println(new Date() + "  " + this.getClass().getSimpleName() + ": Init user home " + userHomeIdentity);
									UserHomes.getInstance().applyUserSettings(
										userHome,
										app.getCurrentPerspective(),
										userSettings,
										true, // save settings
										userHome.getPrimaryGroup(),
										new UserHomes.OpenCrxUserSettings(
											settingTimezone,
											settingStoreSettingsOnLogoff,
											settingEmailAccount,
											settingSendmailSubjectPrefix,
											settingWebAccessUrl,
											settingTopNavigationShowMax,
											settingHideWorkspaceDashboard,
											settingAnchorUserDialog,
											settingRootObjects,
											settingSubscriptions
										),
										false // do not init user home
									);
	                				pmUser.currentTransaction().commit();
	    	            		} catch(Exception e) {
	    	            			new ServiceException(e).log();
			            			try {
			            				pmUser.currentTransaction().rollback();
			            			} catch(Exception ignore) {}
	            				} finally {
	    	            			if(pmUser != null) {
	    	            				pmUser.close();
	    	            			}
	    	            		}
		            		}
	                    }
	                }
	            }
        	} catch(Exception e) {
        		try {
        			pm.currentTransaction().rollback();
        		} catch(Exception ignore) {}
        	}
	        pm.close();
        }
        return new ActionPerformResult(view);
    }

	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	public static final int EVENT_ID = 111;		
}
