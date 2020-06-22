<%@page import="java.util.regex.Pattern"%>
<%@	page contentType="text/html;charset=UTF-8" language="java" pageEncoding="UTF-8"%>
<%
/*
 * ====================================================================
 * Project:     opencrx, http://www.opencrx.org/f
 * Description: Manage Activities of a Business Process
 * Owner:       CRIXP Corp., Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 *
 * Copyright (c) 2005-2014, CRIXP Corp., Switzerland
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
%><%@ page session="true" import="
java.util.*,
java.io.*,
java.text.*,
javax.naming.Context,
javax.naming.InitialContext,
java.sql.*,
org.openmdx.base.accessor.jmi.cci.*,
org.openmdx.portal.servlet.*,
org.openmdx.portal.servlet.action.*,
org.openmdx.portal.servlet.attribute.*,
org.openmdx.portal.servlet.component.*,
org.openmdx.portal.servlet.control.*,
org.openmdx.portal.servlet.wizards.*,
org.openmdx.base.exception.*,
org.openmdx.base.naming.*,
org.openmdx.base.query.*,
org.openmdx.kernel.log.*,
org.opencrx.kernel.generic.*,
org.opencrx.kernel.portal.*,
org.openmdx.kernel.id.cci.*,
org.openmdx.kernel.id.*,
org.openmdx.uses.org.apache.commons.fileupload.*
"%>

<%!

	enum Command {
		NA,
		CANCEL,
		COMPLETE_ACTIVITY,
		DO_FOLLOWUP,
		PREPARE_FOLLOWUP,
		CREATE_FOLLOWUP,
		CANCEL_FOLLOWUP,
		SET_FAVORITEACTIVITY,
		FILE_UPLOAD,
		EMAIL_UPLOAD,
		LIST_ALL_DOCS,
		LIST_ALL_EMAILS,
		RELOAD
	}

	final String ACCOUNT_CLASS = "org:opencrx:kernel:account1:Account";
	final String CONTACT_CLASS = "org:opencrx:kernel:account1:Contact";
	final String ACTIVITY_CLASS = "org:opencrx:kernel:activity1:Activity";
	final String EMAIL_CLASS = "org:opencrx:kernel:activity1:EMail";
	final String MEDIA_CLASS = "org:opencrx:kernel:document1:Media";
	final String ACTIVITYFOLLOWUP_CLASS = "org:opencrx:kernel:activity1:ActivityFollowUp";
	final String RESOURCE_CLASS = "org:opencrx:kernel:activity1:Resource";

	final String FORM_NAME_DOFOLLOWUP = "doFollowUpForm";
	final String UPLOAD_FILE_FIELD_NAME = "uploadFile";
	final String UPLOAD_EMAIL_FIELD_NAME = "uploadEmail";
	final short ACTIVITY_CLOSED = 20;
	final String CLICK_RELOAD = "$('Reload').click();";

	class ProcessNode {
		  private String name;
		  private org.opencrx.kernel.activity1.jmi1.ActivityProcess nodeActivityProcess;
		  private org.opencrx.kernel.activity1.jmi1.Activity nodeActivity;
		  private List<org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition> validTransitions;
		  private List<ProcessNode> subProcessNodes;
	};
	
	private static boolean completeActivity(
		org.opencrx.kernel.activity1.jmi1.Activity activity,
		javax.jdo.PersistenceManager pm
	) {
		// try to complete activity by executing doFollowUp operations until percentComplete==100
		boolean isComplete = (activity.getActivityState() == (short)20);

		try {
  		boolean madeProgress = true;
  		// idea:
  		// - get all transitions (sorted by newPercentComplete ascending) that take activity out of current state
  		// - execute the first transition
  		// - until new process state == closed or no progress then stop

  		org.opencrx.kernel.activity1.jmi1.ActivityProcess process = null;
  		org.opencrx.kernel.activity1.jmi1.ActivityProcessState processState = activity.getProcessState();
  		org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition nextTransition = null;
  		if (processState != null) {
  		  try {
  		    process = (org.opencrx.kernel.activity1.jmi1.ActivityProcess)pm.getObjectById(new Path(processState.refMofId()).getParent().getParent());
  		  } catch (Exception et) {}
  		  while ((process != null) && (!isComplete) && (madeProgress)) {
  		    madeProgress = false;
          org.opencrx.kernel.activity1.cci2.ActivityProcessTransitionQuery transitionFilter = (org.opencrx.kernel.activity1.cci2.ActivityProcessTransitionQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition.class);
          transitionFilter.orderByNewPercentComplete().ascending();
          transitionFilter.thereExistsPrevState().equalTo(
              activity.getProcessState()
            );
          transitionFilter.thereExistsNewPercentComplete().greaterThan(
              activity.getPercentComplete()
            );
          Iterator j = process.getTransition(transitionFilter).iterator();
          if (j.hasNext()) {
            nextTransition = (org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition)j.next();
            while (j.hasNext()) {
              nextTransition = (org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition)j.next();
            }
            //System.out.println("transition: " + nextTransition.getName());
            // doFollowUp
            pm.currentTransaction().begin();
            org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpParams params = org.w3c.spi2.Structures.create(
				org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpParams.class, 
				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpParams.Member.assignTo, null),
				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpParams.Member.followUpText, "Wizard"),
				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpParams.Member.followUpTitle, null),
				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpParams.Member.parentProcessInstance, null),
				org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpParams.Member.transition, nextTransition)
			);  
            org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpResult result = activity.doFollowUp(params);
            pm.currentTransaction().commit();
            madeProgress = true;
            isComplete = (activity.getActivityState() == (short)20);
          }
        }
      }
    } catch (Exception e) {}
		return isComplete;
	}

	private static boolean openActivity(
		org.opencrx.kernel.activity1.jmi1.Activity activity,
		javax.jdo.PersistenceManager pm,
		org.opencrx.kernel.activity1.jmi1.Activity1Package activityPkg
	) {
		// try to (re)open activity by executing doFollowUp operations until percentComplete<100
		boolean isOpen = (activity.getActivityState() == (short)10);

		try {
  		boolean madeProgress = true;
  		// idea:
  		// - get all transitions (sorted by newPercentComplete descending) that take activity out of current state
  		// - execute the first transition
  		// - until new process state == closed or no progress then stop

  		org.opencrx.kernel.activity1.jmi1.ActivityProcess process = null;
  		org.opencrx.kernel.activity1.jmi1.ActivityProcessState processState = activity.getProcessState();
  		org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition nextTransition = null;
  		if (processState != null) {
  		  try {
  		    process = (org.opencrx.kernel.activity1.jmi1.ActivityProcess)pm.getObjectById(new Path(processState.refMofId()).getParent().getParent());
  		  } catch (Exception et) {}
  		  while ((process != null) && (!isOpen) && (madeProgress)) {
  		    madeProgress = false;
          org.opencrx.kernel.activity1.cci2.ActivityProcessTransitionQuery transitionFilter = activityPkg.createActivityProcessTransitionQuery();
          transitionFilter.orderByNewPercentComplete().descending();
          transitionFilter.thereExistsPrevState().equalTo(
              activity.getProcessState()
            );
          transitionFilter.thereExistsNewPercentComplete().lessThan(
              activity.getPercentComplete()
            );
          Iterator j = process.getTransition(transitionFilter).iterator();
          if (j.hasNext()) {
            nextTransition = (org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition)j.next();
            //System.out.println("transition: " + nextTransition.getName());
            // doFollowUp
            pm.currentTransaction().begin();
            org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpParams params = activityPkg.createActivityDoFollowUpParams(
              null,           // Contact,
              "Wizard",       // String Text
              null,           // String Title
              null,           // parentProcessInstance
              nextTransition  // Transition
            );
            org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpResult result = activity.doFollowUp(params);
            pm.currentTransaction().commit();
            madeProgress = true;
            isOpen = (activity.getActivityState() == (short)10);
          }
        }
      }
    } catch (Exception e) {}
		return isOpen;
	}
	
	public ProcessNode addSubNode(
			ProcessNode node,
			String name,
			org.opencrx.kernel.activity1.jmi1.ActivityProcess nodeActivityProcess,
			org.opencrx.kernel.activity1.jmi1.Activity activity
	) {
			if (node != null) {
					if (node.subProcessNodes == null) {
							node.subProcessNodes = new ArrayList();
					}
					ProcessNode subNode = new ProcessNode();
					subNode.name = name;
					subNode.nodeActivityProcess = nodeActivityProcess;
					subNode.nodeActivity = activity;
					subNode.subProcessNodes = null;
					node.subProcessNodes.add(subNode);
					return subNode;
			}
			return null;		
	}
	

	public ProcessNode getSubProcessActivities( // finds all activities of the respective subprocess instances
			ProcessNode node,
			javax.jdo.PersistenceManager pm
	) {
			if (node == null || node.nodeActivity == null) {return node;}

			org.opencrx.kernel.activity1.jmi1.Activity nodeActivity = node.nodeActivity;
			if (nodeActivity != null) {
					//System.out.println("node activity #" + nodeActivity.getActivityNumber());
					// try to determine dependent activities by following linkedFrom with type "isParentOf" (i.e. 100 - "isChildOf")
					org.opencrx.kernel.activity1.cci2.ActivityLinkFromQuery linkFromQuery = (org.opencrx.kernel.activity1.cci2.ActivityLinkFromQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityLinkFrom.class);
					linkFromQuery.activityLinkType().equalTo(new Short((short)(100 - org.opencrx.kernel.backend.Activities.ActivityLinkType.IS_CHILD_OF.getValue())));
					for (Iterator linkFrom = nodeActivity.getActivityLinkFrom(linkFromQuery).iterator(); linkFrom.hasNext();) {
							try {
									org.opencrx.kernel.activity1.jmi1.ActivityLinkFrom activityLinkFrom = (org.opencrx.kernel.activity1.jmi1.ActivityLinkFrom)linkFrom.next();
									if (activityLinkFrom.getLinkFrom() != null) {
											org.opencrx.kernel.activity1.jmi1.Activity linkedFromActivity = activityLinkFrom.getLinkFrom();
											//System.out.print("   dependent activity #" + linkedFromActivity.getActivityNumber()); 
											// determine matching subProcessNode
											org.opencrx.kernel.activity1.jmi1.ActivityProcess activityProcess = getControllingProcessOfActivity(linkedFromActivity);
											
											boolean abort = false;
											Iterator subProcessNodes = node.subProcessNodes.iterator();
											while (!abort && subProcessNodes.hasNext()) {
													try {
															ProcessNode currentNode = (ProcessNode)subProcessNodes.next();
															if (
																	currentNode.nodeActivityProcess != null &&
																	activityProcess != null &&
																	currentNode.nodeActivity == null &&
																	currentNode.nodeActivityProcess.refMofId().compareTo(activityProcess.refMofId()) == 0
															) {
																	// matching node found
																	String transitionName = null;
																	try {
																			// verify that context matches as well
																			transitionName = ((org.opencrx.kernel.activity1.jmi1.ActivityFollowUp)linkedFromActivity.getCreationContext()).getTransition().getName();
																	} catch (Exception e) {
																			new ServiceException(e).log();
																	}
																	if (currentNode.name != null && transitionName != null && transitionName.compareTo(currentNode.name) == 0) { 
																			//System.out.print("--------------matched activity #" + linkedFromActivity.getActivityNumber() + " to node '" + currentNode.name + "'" );
																			currentNode.nodeActivity = linkedFromActivity;
																			currentNode.validTransitions = getNextTransitionsOfActivity(linkedFromActivity, true, true, pm);
																			currentNode = getSubProcessActivities(currentNode, pm);
																			cleanTransitions(currentNode, pm);
																			abort = true;
																	}
															}
													} catch (Exception e) {
															new ServiceException(e).log();
													}
											}
											//System.out.println("");
									}
							} catch (Exception e) {
									new ServiceException(e).log();
							}
					}
			}
			return node;
	}
	

	public ProcessNode cleanTransitions(
			ProcessNode node,
			javax.jdo.PersistenceManager pm
	) {
			//System.out.println("cleanTransitions of node " + node.name);
			if (node.subProcessNodes != null && !node.subProcessNodes.isEmpty()) {
					int nIdx = node.subProcessNodes.size();
					while (nIdx > 0) {
							nIdx--;
							ProcessNode currentNode = (ProcessNode)node.subProcessNodes.get(nIdx);
							// eliminate creating transition of parent node
							if (node.validTransitions != null && !node.validTransitions.isEmpty()) {
								int idx = node.validTransitions.size();
								boolean match = false;
								while (!match && idx > 0) {
										idx--;
										org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition transition = (org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition)node.validTransitions.get(idx);
										if (transition.getName().compareTo(currentNode.name) == 0 && currentNode.nodeActivity != null) {
												//System.out.println("remove transition " + transition.getName() + " (matched node)");
												match = true;
												node.validTransitions.remove(idx);
										}
								}
							}
							
					}
			}
			return node;
	}
	

	public List<org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition> getNextTransitionsOfActivity(
			org.opencrx.kernel.activity1.jmi1.Activity activity,
			boolean onlySubActivityTransitions,
			boolean orderPercentCompleteIncreasing,
			javax.jdo.PersistenceManager pm
	) {
			List<org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition> transitions = new ArrayList();
			if (activity == null) {return transitions;}
			
			org.opencrx.kernel.activity1.jmi1.ActivityProcess activityProcess = null;
			try {
					activityProcess = activity.getActivityType().getControlledBy();
			} catch (Exception e) {
					System.out.println("activity #" + activity.getActivityNumber() + " with control process error");
					new ServiceException(e).log();
			}
			org.opencrx.kernel.activity1.jmi1.ActivityProcessState processState = null;
			try {
					processState = activity.getProcessState();
			} catch (Exception e) {
					System.out.println("activity #" + activity.getActivityNumber() + " with process state error");
					new ServiceException(e).log();
			}
			if (processState != null) {
					if (onlySubActivityTransitions) {
							org.opencrx.kernel.activity1.cci2.SubActivityTransitionQuery subActivityTransitionQuery = (org.opencrx.kernel.activity1.cci2.SubActivityTransitionQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.SubActivityTransition.class);
							subActivityTransitionQuery.thereExistsPrevState().equalTo(processState);
							if (orderPercentCompleteIncreasing) {
									subActivityTransitionQuery.orderByNewPercentComplete().ascending();
							} else {
									subActivityTransitionQuery.orderByNewPercentComplete().descending();
							}
							subActivityTransitionQuery.orderByName().ascending();
							for(Iterator t = activityProcess.getTransition(subActivityTransitionQuery).iterator(); t.hasNext();) {
									transitions.add((org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition)t.next());
							}
					} else {
							org.opencrx.kernel.activity1.cci2.ActivityProcessTransitionQuery activityProcessTransitionQuery = (org.opencrx.kernel.activity1.cci2.ActivityProcessTransitionQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition.class);
							activityProcessTransitionQuery.thereExistsPrevState().equalTo(processState);
							if (orderPercentCompleteIncreasing) {
									activityProcessTransitionQuery.orderByNewPercentComplete().ascending();
							} else {
									activityProcessTransitionQuery.orderByNewPercentComplete().descending();
							}
							activityProcessTransitionQuery.orderByName().ascending();
							for(Iterator t = activityProcess.getTransition(activityProcessTransitionQuery).iterator(); t.hasNext();) {
									transitions.add((org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition)t.next());
							}
						
					}
			}
			return transitions;
	}

	public ProcessNode getSubProcesses(
			ProcessNode processNode,
			org.opencrx.kernel.activity1.jmi1.ActivityProcess activityProcess,
			javax.jdo.PersistenceManager pm
	) {
			if (activityProcess != null) {
					processNode.subProcessNodes = new ArrayList();
					
					// try to determine subNodes based on SubActivityTransitions
					org.opencrx.kernel.activity1.cci2.SubActivityTransitionQuery subActivityTransitionQuery = (org.opencrx.kernel.activity1.cci2.SubActivityTransitionQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.SubActivityTransition.class);
					subActivityTransitionQuery.orderByNewPercentComplete().ascending();
					subActivityTransitionQuery.orderByName().ascending();
					for (Iterator t = activityProcess.getTransition(subActivityTransitionQuery).iterator(); t.hasNext();) {
							org.opencrx.kernel.activity1.jmi1.SubActivityTransition subActivityTransition =	(org.opencrx.kernel.activity1.jmi1.SubActivityTransition)t.next();
							try {
									org.opencrx.kernel.activity1.jmi1.ActivityProcess subActivityProcess = null;
									org.opencrx.kernel.activity1.jmi1.ActivityCreator activityCreator = subActivityTransition.getActivityCreator();
									if (activityCreator != null & activityCreator.getActivityType() != null && activityCreator.getActivityType().getControlledBy() != null) {
											subActivityProcess = activityCreator.getActivityType().getControlledBy();
									}
									ProcessNode subNode = addSubNode(processNode, (subActivityTransition.getName() != null ? subActivityTransition.getName() : "--"), subActivityProcess, null);
									if (subActivityProcess != null) {
											ProcessNode tempNode = getSubProcesses(subNode, subActivityProcess, pm);
									} else {
											// nothing to do
									}
							} catch (Exception e) {
									try {
										SysLog.warning("bad transition (" + subActivityTransition.getName() + ") with xri = " + subActivityTransition.refMofId(), e.getMessage());
									} catch (Exception el) {}
							}
					}
			}
			return processNode;
	}

	
	public org.opencrx.kernel.activity1.jmi1.ActivityProcess getControllingProcessOfActivity(
			org.opencrx.kernel.activity1.jmi1.Activity activity
	) {
			org.opencrx.kernel.activity1.jmi1.ActivityProcess activityProcess = null;
			try {
					if (
							activity != null &&
							activity.getActivityType() != null &&
							activity.getActivityType().getControlledBy() != null
					) {
						activityProcess = activity.getActivityType().getControlledBy();
					}
			} catch (Exception e) {
					new ServiceException(e).log();
			}
			return activityProcess;
	}
	
	
	public ProcessNode getProcess(
			org.opencrx.kernel.activity1.jmi1.Activity activity, // any activity belonging to the process
			javax.jdo.PersistenceManager pm
	) {
			org.opencrx.kernel.activity1.jmi1.Activity topLevelActivity = activity;
			if (activity != null) {
					// try to determine top-level controlling activity by following linkedTo with type "isChildOf"
					org.opencrx.kernel.activity1.cci2.ActivityLinkToQuery linkToQuery = (org.opencrx.kernel.activity1.cci2.ActivityLinkToQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityLinkTo.class);
					linkToQuery.activityLinkType().equalTo(new Short(org.opencrx.kernel.backend.Activities.ActivityLinkType.IS_CHILD_OF.getValue()));
					boolean abort = false;
					Collection linkTo = topLevelActivity.getActivityLinkTo(linkToQuery);
					while (!abort && topLevelActivity != null && !linkTo.isEmpty()) {
							try {
									org.opencrx.kernel.activity1.jmi1.ActivityLinkTo activityLinkTo = (org.opencrx.kernel.activity1.jmi1.ActivityLinkTo)linkTo.iterator().next(); // note: only 1 linkTo!
									if (activityLinkTo.getLinkTo() != null) {
											topLevelActivity = activityLinkTo.getLinkTo();
											linkTo = topLevelActivity.getActivityLinkTo(linkToQuery);
									} else {
											abort = true;
									}
							} catch (Exception e) {
									abort = true;
							}
					}
			}
	
			// determine process
			org.opencrx.kernel.activity1.jmi1.ActivityProcess activityProcess = getControllingProcessOfActivity(topLevelActivity);
			
			ProcessNode topNode = null;
			if (activityProcess != null) {
					topNode = new ProcessNode();
					topNode.name = activityProcess.getName() != null ? activityProcess.getName() : "--";
					topNode.subProcessNodes = null;
					topNode = getSubProcesses(topNode, activityProcess, pm);
					topNode.nodeActivity = topLevelActivity;
					topNode.validTransitions = getNextTransitionsOfActivity(topLevelActivity, true, true, pm);
					topNode = getSubProcessActivities(topNode, pm);
					topNode = cleanTransitions(topNode, pm);
			}
			return topNode;
	}

	public String getLastElementOfName(
			String name
	) {
			String result = "";
			if (name != null) {
					String[] splittedName = name.split(" - ");
					if (splittedName.length > 0) {
							result = splittedName[splittedName.length-1];
					}
			}
			//System.out.println(name + "  -->  " + result);
			return result;
	}

	public boolean doFollowUp(
			org.opencrx.kernel.activity1.jmi1.Activity activity,
			org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition transition,
			String title,
			String text,
			org.opencrx.kernel.account1.jmi1.Contact assignTo,
			javax.jdo.PersistenceManager pm
	) {
			boolean success = false;
			if(activity != null && transition != null) {
					org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpParams params = org.w3c.spi2.Structures.create(
						org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpParams.class, 
						org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpParams.Member.assignTo, assignTo),
						org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpParams.Member.followUpText, text == null ? null : text.replace("\r\n", "\n")),
						org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpParams.Member.followUpTitle, title),
						org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpParams.Member.parentProcessInstance, null),
						org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpParams.Member.transition, transition)
					);  
					try {
							pm.currentTransaction().begin();
							org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpResult activityDoFollowUpResult = activity.doFollowUp(params);
							pm.currentTransaction().commit();
							if (activityDoFollowUpResult.getFollowUp() instanceof org.opencrx.kernel.activity1.jmi1.SubActivity) {
									pm.currentTransaction().begin();
									org.opencrx.kernel.activity1.jmi1.Activity subActivity = activityDoFollowUpResult.getFollowUp().getActivity();
									subActivity.setReportingAccount(activity.getReportingAccount());
									subActivity.setReportingContact(activity.getReportingContact());
									pm.currentTransaction().commit();
							}
							success = true;
					} catch(Exception e) {
							try {
									new ServiceException(e).log();
									pm.currentTransaction().rollback();
							} catch(Exception e0) {}
					}
			}
			return success;
	}

	public String getAccountEntry(
			org.opencrx.kernel.account1.jmi1.Account account,
			String caption,
			String title,
			UserDefinedView userView,
			ApplicationContext app
	) {
			String result = "<table class='accountEntryTable ' title='" + title  + "'>"
											+ "<caption>" + caption + "</caption>"
											+ "<tr><td class='accountName'>&nbsp;</td></tr>"
											+ "<tr><td class='accountPhone'>&nbsp;</td></tr>"
											+ "<tr><td class='accountPhone'>&nbsp;</td></tr>"
											+ "<tr><td class='accountEMail'>&nbsp;</td></tr>"
											+ "</table>";
			if (account != null) {
					try {
					    DataBinding phoneMobileDataBinding = new PhoneNumberDataBinding("[isMain=(boolean)true];usage=(short)200;automaticParsing=(boolean)true");
					    DataBinding mailBusinessDataBinding = new EmailAddressDataBinding("[isMain=(boolean)true];usage=(short)500;[emailType=(short)1]");
					    DataBinding postalBusinessDataBinding = new PostalAddressDataBinding("[isMain=(boolean)true];usage=(short)500?zeroAsNull=true");
					    DataBinding phoneBusinessDataBinding = new PhoneNumberDataBinding("[isMain=(boolean)true];usage=(short)500;automaticParsing=(boolean)true");
					    
					    String businessPhone = (String)phoneBusinessDataBinding.getValue(account, "org:opencrx:kernel:account1:Account:address*Business!phoneNumberFull");
					    String mobilePhone = (String)phoneMobileDataBinding.getValue(account, "org:opencrx:kernel:account1:Account:address*Mobile!phoneNumberFull");
					    String businessEmail = (String)mailBusinessDataBinding.getValue(account, "org:opencrx:kernel:account1:Account:address*Business!emailAddress");
		
							String accountHref = null;
							Action action = new Action(
									SelectObjectAction.EVENT_ID,
									new Action.Parameter[]{
											new Action.Parameter(Action.PARAMETER_OBJECTXRI, account.refMofId())
									},
									"",
									true // enabled
								);
							accountHref = "../../" + action.getEncodedHRef();
		
							result = "<table class='accountEntryTable " + (account instanceof org.opencrx.kernel.account1.jmi1.LegalEntity ? "legalEntity" : "contact") + "' title='" + title  + "'>"
											+ "<caption>" + caption + "</caption>"
											+ "<tr><td class='accountName'><a href='" + accountHref + "' target='_blank' title='" + app.getHtmlEncoder().encode(new ObjectReference(account, app).getTitle(), false) + "'>"
											+		"<img src='../../images/" + (new ObjectReference(account, app)).getIconKey() + "' border='0' align='absbottom' style='padding-bottom:3px;' /> "
											+		"<b>" + (account.getFullName() != null ? account.getFullName() : "--") + (account.getExtString0() != null ? " (" + account.getExtString0() + ")" : "") + "</b></a>"
											+ "</td></tr>"
											+ "<tr><td class='accountPhone' title='" + userView.getFieldLabel(ACCOUNT_CLASS, "address*Business!phoneNumberFull", app.getCurrentLocaleAsIndex()) + "'>" + (businessPhone != null ? "<a href='tel:"    + businessPhone + "'>" + businessPhone + "</a>" : "--") + "</td></tr>"
											+ "<tr><td class='accountPhone' title='" + userView.getFieldLabel(ACCOUNT_CLASS, "address*Mobile!phoneNumberFull",   app.getCurrentLocaleAsIndex()) + "'>" + (mobilePhone   != null ? "<a href='tel:"    + mobilePhone   + "'>" + mobilePhone   + "</a>" : "--") + "</td></tr>"
											+ "<tr><td class='accountEMail' title='" + userView.getFieldLabel(ACCOUNT_CLASS, "address*Business!emailAddress",    app.getCurrentLocaleAsIndex()) + "'>" + (businessEmail != null ? "<a href='mailto:" + businessEmail + "'>" + businessEmail + "</a>" : "--") + "</td></tr>"
											+ "</table>";
					} catch (Exception e) {
							new ServiceException(e).log();
					}
			}
			return result;
	}
	
	public String getActivityEntry(
			ProcessNode processNode,
			String activityHref,
			UserDefinedView userView,
			javax.jdo.PersistenceManager pm,
			ApplicationContext app
	) {
			try {
					boolean hasValidTransitions = processNode.nodeActivity != null ? (getNextTransitionsOfActivity(processNode.nodeActivity, false, false, pm).size() > 0) : false;
					boolean isOpen = processNode.nodeActivity != null ? (processNode.nodeActivity.getPercentComplete() < 100) : false;
					SimpleDateFormat activityDateFormat = new SimpleDateFormat("dd-MM-yyyy HH:mm", app.getCurrentLocale());
					/*
					String history = "";

					org.opencrx.kernel.activity1.cci2.ActivityFollowUpQuery activityFollowUpFilter = org.opencrx.kernel.utils.Utils.getActivityPackage(pm).createActivityFollowUpQuery();
					activityFollowUpFilter.forAllDisabled().isFalse();
					activityFollowUpFilter.orderByCreatedAt().descending();
					for (
							Iterator j = processNode.nodeActivity.getFollowUp(activityFollowUpFilter).iterator();
							j.hasNext();
					) {
							try {
									// get ActivityFollowUp
									org.opencrx.kernel.activity1.jmi1.ActivityFollowUp activityFollowUp = (org.opencrx.kernel.activity1.jmi1.ActivityFollowUp)j.next();
									history += activityDateFormat.format(activityFollowUp.getCreatedAt()) + "  " 
													+ (activityFollowUp.getTitle() != null ? activityFollowUp.getTitle().replaceAll("&nbsp;", " ").replaceAll("#ERR", "N/P") : "") + "<br>";
							} catch (Exception e) {}
					}
					*/
					
					String result = "<table class='actEntryTable'><tr>";
					result += "<td class='actEntry noact'>" + processNode.name + "</td>";
					result += "<td class='normal actAssignedTo'>" + app.getHtmlEncoder().encode(new ObjectReference(processNode.nodeActivity.getAssignedTo(), app).getTitle(), false) + "</td>";
					result += "<td class='actModifiedAt'>" + processNode.nodeActivity.getPercentComplete() + "%</td>";
					result += "</tr><tr>";
					result += "<td colspan='2' class='normal actEntry'><div class='" + (hasValidTransitions ? "followUp actFollowUp" : "invisible") + "' title='" + app.getLabel(ACTIVITYFOLLOWUP_CLASS)  + "' onclick=\"javascript:"
					 + "$('ACTIVITY_XRI').value='" + (processNode.nodeActivity == null ? "" : processNode.nodeActivity.refMofId()) + "';"
					 + "$('command').value='PREPARE_FOLLOWUP';" + CLICK_RELOAD + "return false;\""
					 + ">" + (hasValidTransitions ? "<img src='../../images/next.gif'/>" : "") + "</div>"
					 + "<div class='" + (isOpen ? "followUp actFollowUp" : "invisible") + "' title='--&gt; 100%' onclick=\"javascript:"
					 + "$('ACTIVITY_XRI').value='" + (processNode.nodeActivity == null ? "" : processNode.nodeActivity.refMofId()) + "';"
					 + "$('command').value='COMPLETE_ACTIVITY';" + CLICK_RELOAD + "return false;\""
					 + ">" + (isOpen ? "<img src='../../images/filter_ok.gif'/>" : "") + "</div>"
					 + "<div style='padding-top:2px;'>&nbsp;#" + processNode.nodeActivity.getActivityNumber() + ": " + getLastElementOfName(processNode.nodeActivity.getName()) + "</div></td>";
					result += "<td class='normal actModifiedAt'>" + activityDateFormat.format(processNode.nodeActivity.getModifiedAt());
					//result += history;
					result += "</td>";
					result += "</tr></table>";
					return result;
			} catch (Exception e) {
					new ServiceException(e).log();
			}
			return "";
	}
	
	public String produceTable(
			ProcessNode processNode,
			boolean showCompleteProcess,
			UserDefinedView userView,
			javax.jdo.PersistenceManager pm,
			ApplicationContext app
	) {
			String result = "";
			if (processNode != null) {
					try {
							String name = "";
							boolean isOpen = false; 
							String actStatus = "";
							String actTitle = "";
							String activityHref = null;
							if (processNode.nodeActivity != null) {
								//System.out.println("table activity #" + processNode.nodeActivity.getActivityNumber());
								isOpen = processNode.nodeActivity.getActivityState() < ACTIVITY_CLOSED;
								if (isOpen) {
										actStatus = "actopen";
								} else {
										actStatus = "actclosed";
								}
								actTitle = "title='" + app.getHtmlEncoder().encode(new ObjectReference(processNode.nodeActivity, app).getTitle(), false) + "'";
								Action action = new Action(
										SelectObjectAction.EVENT_ID,
										new Action.Parameter[]{
												new Action.Parameter(Action.PARAMETER_OBJECTXRI, processNode.nodeActivity.refMofId())
										},
										"",
										true // enabled
									);
								activityHref = "../../" + action.getEncodedHRef();
								name = "<a href='" + activityHref + "' target='_blank' " + actTitle + ">" + getLastElementOfName(processNode.nodeActivity.getName()) + "</a>";
							}
							
							boolean hasValidTransitions = processNode.nodeActivity != null ? (getNextTransitionsOfActivity(processNode.nodeActivity, false, false, pm).size() > 0) : false;;
							String title = "<td title='" + processNode.name + "' class='processName " + actStatus + "' rowspan='" + processNode.subProcessNodes.size() + "'>"
															+ "<div class='actPicker' onclick='javascript:$(\"FAVORITEACTIVITY_XRI\").value=\"" + (processNode.nodeActivity == null ? "" : processNode.nodeActivity.refMofId()) + "\";"
															+ "$(\"DOCSFAVORITEACTIVITY_XRI\").value=\"" + (processNode.nodeActivity == null ? "" : processNode.nodeActivity.refMofId()) + "\";"
															+ "$(\"EMAILFAVORITEACTIVITY_XRI\").value=\"" + (processNode.nodeActivity == null ? "" : processNode.nodeActivity.refMofId()) + "\";"
															+ "$(\"command\").value=\"SET_FAVORITEACTIVITY\";$(\"Reload\").click();'><img src='../../images/favorites.gif'/></div>"
															+ "<div class='processNode noact'>"
															+ processNode.name + "</div>"
															+ "<div class='" + (hasValidTransitions ? "followUp actFollowUp" : "invisible") + "' title='" + app.getLabel(ACTIVITYFOLLOWUP_CLASS)  + "' onclick=\"javascript:"
															+ "$('ACTIVITY_XRI').value='" + (processNode.nodeActivity == null ? "" : processNode.nodeActivity.refMofId()) + "';"
															+ "$('command').value='PREPARE_FOLLOWUP';" + CLICK_RELOAD + "return false;\""
											        + ">" + (hasValidTransitions ? "<img src='../../images/next.gif'/>" : "") + "</div>&nbsp;"
															+"<div><b>" + name + "</b></div>";
							if (processNode.validTransitions != null && !processNode.validTransitions.isEmpty()) {
									title += "<br><br>";
									for(org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition transition: processNode.validTransitions) {
											title += "<div class='transition' title='" + app.getTexts().getNewText() + ": " + transition.getName() + "' onclick=\"javascript:"
															 + "$('ACTIVITY_XRI').value='" + (processNode.nodeActivity == null ? "" : processNode.nodeActivity.refMofId()) + "';"
															 + "$('TRANSITION_XRI').value='" + transition.refMofId() + "';"
															 + "$('command').value='DO_FOLLOWUP';" + CLICK_RELOAD + "\""
											         + "><img src='../../images/Export.gif'/> " + transition.getName() + "</div>";
									}
							}
							
							title += "</td>";
							if (processNode.subProcessNodes != null && processNode.subProcessNodes.size() > 0) {
									result += "<table class='processTable'>";
									for (int r=0; r < processNode.subProcessNodes.size(); r++) {
											ProcessNode currentNode = (ProcessNode)processNode.subProcessNodes.get(r);
											result += "<tr>" + (r == 0 ? title : "");
											result += "<td>" + (showCompleteProcess || currentNode.nodeActivity != null ? produceTable(currentNode, showCompleteProcess, userView, pm, app) : "") + "</td></tr>";  
									}
									result += "</table>";
							} else {
									if (activityHref == null) {
											if (showCompleteProcess) {
													result += "<div class='processNode noact'>" + processNode.name + "</div>";
											}
									} else {
											result += "<div class='processNode " + (isOpen ? "actopen" : "actclosed") + "' " + actTitle + "><a href='" + activityHref + "' target='_blank'>" + getActivityEntry(processNode, activityHref, userView, pm, app) + "</a></div><b>";
									}
							}
					} catch (Exception e) {
							new ServiceException(e).log();
					}
			}
			return result;
	}
	
	public List<org.opencrx.kernel.activity1.jmi1.Activity> getActivities(
		ProcessNode processNode
	) {
		List<org.opencrx.kernel.activity1.jmi1.Activity> result = new ArrayList<org.opencrx.kernel.activity1.jmi1.Activity>();
		if (processNode != null) {
			try {
				if (processNode.nodeActivity != null) {
					result.add(processNode.nodeActivity);
				}
				if (processNode.subProcessNodes != null && processNode.subProcessNodes.size() > 0) {
					for (int r=0; r < processNode.subProcessNodes.size(); r++) {
						ProcessNode currentNode = (ProcessNode)processNode.subProcessNodes.get(r);
						if (currentNode.nodeActivity != null) {
							result.addAll(getActivities(currentNode));
						}
					}
				}
			} catch (Exception e) {
					new ServiceException(e).log();
			}
		}
		return result;
	}

	public List<org.opencrx.kernel.generic.jmi1.Media> getDocuments(
		ProcessNode processNode
	) {
		List<org.opencrx.kernel.generic.jmi1.Media> result = new ArrayList<org.opencrx.kernel.generic.jmi1.Media>();
		if (processNode != null) {
			try {
				if (processNode.nodeActivity != null) {
					for(Iterator d = processNode.nodeActivity.getMedia().iterator(); d.hasNext();) {
						try {
							result.add((org.opencrx.kernel.generic.jmi1.Media)d.next());
						} catch (Exception e) {
							new ServiceException(e).log();
						}
					}
				}
				if (processNode.subProcessNodes != null && processNode.subProcessNodes.size() > 0) {
					for (int r=0; r < processNode.subProcessNodes.size(); r++) {
						ProcessNode currentNode = (ProcessNode)processNode.subProcessNodes.get(r);
						if (currentNode.nodeActivity != null) {
							result.addAll(getDocuments(currentNode));
						}
					}
				}
			} catch (Exception e) {
					new ServiceException(e).log();
			}
		}
		return result;
	}

	public List<org.opencrx.kernel.activity1.jmi1.EMail> getLinkedEMailsOfActivity(
		org.opencrx.kernel.activity1.jmi1.Activity activity,
		javax.jdo.PersistenceManager pm
	) {
		List<org.opencrx.kernel.activity1.jmi1.EMail> result = new ArrayList<org.opencrx.kernel.activity1.jmi1.EMail>();
		try {
			org.opencrx.kernel.activity1.cci2.ActivityLinkFromQuery linkFromQuery = (org.opencrx.kernel.activity1.cci2.ActivityLinkFromQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityLinkFrom.class);
			linkFromQuery.activityLinkType().equalTo(new Short((short)(100 - org.opencrx.kernel.backend.Activities.ActivityLinkType.RELATES_TO.getValue())));
			for (Iterator linkFrom = activity.getActivityLinkFrom(linkFromQuery).iterator(); linkFrom.hasNext();) {
				try {
					org.opencrx.kernel.activity1.jmi1.ActivityLinkFrom activityLinkFrom = (org.opencrx.kernel.activity1.jmi1.ActivityLinkFrom)linkFrom.next();
					if (activityLinkFrom.getLinkFrom() != null && activityLinkFrom.getLinkFrom() instanceof org.opencrx.kernel.activity1.jmi1.EMail) {
						result.add((org.opencrx.kernel.activity1.jmi1.EMail)activityLinkFrom.getLinkFrom());
					}
				} catch (Exception e) {}
			}
		} catch (Exception e) {}
		return result;
	}

	public List<org.opencrx.kernel.activity1.jmi1.EMail> getEMails(
		ProcessNode processNode,
		javax.jdo.PersistenceManager pm
	) {
		List<org.opencrx.kernel.activity1.jmi1.EMail> result = new ArrayList<org.opencrx.kernel.activity1.jmi1.EMail>();
		if (processNode != null) {
			try {
				if (processNode.nodeActivity != null) {
					result.addAll(getLinkedEMailsOfActivity(processNode.nodeActivity, pm));
				}
				if (processNode.subProcessNodes != null && processNode.subProcessNodes.size() > 0) {
					for (int r=0; r < processNode.subProcessNodes.size(); r++) {
						ProcessNode currentNode = (ProcessNode)processNode.subProcessNodes.get(r);
						if (currentNode.nodeActivity != null) {
							result.addAll(getEMails(currentNode, pm));
						}
					}
				}
			} catch (Exception e) {
					new ServiceException(e).log();
			}
		}
		return result;
	}
%>

<%

	final String WIZARD_NAME = "BusinessProcessManager.jsp";
	final String OK = "<img src='../../images/checked.gif' />";
	final String NOTCHECKED = "<img src='../../images/notchecked.gif' />";
	final String MISSING = "<img src='../../images/cancel.gif' />";
	//System.out.println("---------------------------------------------------------------------------------------------");

	// Init
	request.setCharacterEncoding("UTF-8");
	ApplicationContext app = (ApplicationContext)session.getValue(WebKeys.APPLICATION_KEY);
	ViewsCache viewsCache = (ViewsCache)session.getValue(WebKeys.VIEW_CACHE_KEY_SHOW);
	String requestId =	null;
	String objectXri = null;
	String commandAsString = null;
	List filecb = new ArrayList();

	String FAVORITEACTIVITY_XRI = null;

	List<String> roundtripFilesPath = new ArrayList<String>();
	List<String> roundtripFilesName = new ArrayList<String>();
	Map<String,String> addressMap = new TreeMap<String,String>(); // Mapping X.500 --> SMTP
	boolean hasErrors = false;
	List<String> errors = new ArrayList<String>();
	List<String> existingFilesPath = new ArrayList<String>();
	List<String> existingFilesName = new ArrayList<String>();

	if(FileUpload.isMultipartContent(request)) {
			try {
				Map parameterMap = request.getParameterMap();
				if(FileUpload.isMultipartContent(request)) {
					parameterMap = new HashMap();
					DiskFileUpload upload = new DiskFileUpload();
					upload.setHeaderEncoding("UTF-8");
					try {
						List items = upload.parseRequest(
							request,
							200,  // in-memory threshold. Content for fields larger than threshold is written to disk
							50000000, // max request size [overall limit]
						  app.getTempDirectory().getPath()
						);
						int fileCounter = 0;
						for(Iterator i = items.iterator(); i.hasNext(); ) {
						  FileItem item = (FileItem)i.next();
						  if(item.isFormField()) {
							parameterMap.put(
							  item.getFieldName(),
							  new String[]{item.getString("UTF-8")}
							);
						  }
						  else {
							// reset binary
								if("#NULL".equals(item.getName())) {
								  parameterMap.put(
									item.getFieldName(),
									new String[]{item.getName()}
								  );
								}
								// add to parameter map if file received
								else if(item.getSize() > 0) {
									fileCounter++;
								  parameterMap.put(
										item.getFieldName(),
										new String[]{item.getName()}
								  );
								  String location = app.getTempFileName(fileCounter + "." + item.getFieldName(), "");
		
								  // bytes
								  File outFile = new File(location);
								  item.write(outFile);
		
								  // type
								  PrintWriter pw = new PrintWriter(
									new FileOutputStream(location + ".INFO")
								  );
								  pw.println(item.getContentType());
								  int sep = item.getName().lastIndexOf("/");
								  if(sep < 0) {
									sep = item.getName().lastIndexOf("\\");
								  }
								  pw.println(item.getName().substring(sep + 1));
									System.out.println("location = " + location + " / name = " + item.getName().substring(sep + 1));
								  pw.close();
								}
						  }
						}
						int recount = 1;
						while (recount <= fileCounter) {
							boolean isChecked = parameterMap.get("filecb" + recount) != null;
							filecb.add(new Boolean(isChecked));
							recount++;
						}
					}
					catch(FileUploadException e) {
						SysLog.warning("can not upload file", e.getMessage());
					}
				}
				String[] requestIds = (String[])parameterMap.get(Action.PARAMETER_REQUEST_ID);
				requestId = (requestIds == null) || (requestIds.length == 0) ? "" : requestIds[0];
				String[] objectXris = (String[])parameterMap.get("xri");
				objectXri = (objectXris == null) || (objectXris.length == 0) ? "" : objectXris[0];
				String[] favactXris = (String[])parameterMap.get("FAVORITEACTIVITY_XRI");
				FAVORITEACTIVITY_XRI = (favactXris == null) || (favactXris.length == 0) ? "" : favactXris[0];
				String[] commandAsStrings = (String[])parameterMap.get("DOCcommand");
				commandAsString = (commandAsStrings == null) || (commandAsStrings.length == 0) ? "" : commandAsStrings[0];

				// get file paths/names of files that had errors
				int fileIdx = 0;
				while ((String[])parameterMap.get("filepath-" + fileIdx) != null) {
					try {
						String[] paths = (String[])parameterMap.get("filepath-" + fileIdx);
						roundtripFilesPath.add(paths[0]);
						String[] names = (String[])parameterMap.get("filename-" + fileIdx);
						roundtripFilesName.add(names[0]);
						System.out.println("name"+ fileIdx + ": " + names[0]);
					} catch (Exception e) {}
					fileIdx++;
				}

				// get unmatched/provided e-mail addresses and buld address map
				int emailIdx = 0;
				while ((String[])parameterMap.get("email-" + emailIdx) != null) {
					try {
						String[] provided = (String[])parameterMap.get("email-" + emailIdx);
						String[] unmatched = (String[])parameterMap.get("unmatched-" + emailIdx);
						if (!unmatched[0].isEmpty() && !provided[0].isEmpty()) {
							addressMap.put(unmatched[0], provided[0]);
						}
					} catch (Exception e) {}
					emailIdx++;
				}
			
			} catch (Exception e) {
				new ServiceException(e).log();
			}
	} else {
		requestId =	request.getParameter(Action.PARAMETER_REQUEST_ID);
		objectXri = request.getParameter(Action.PARAMETER_OBJECTXRI);
		FAVORITEACTIVITY_XRI = (request.getParameter("FAVORITEACTIVITY_XRI") == null ? "" : request.getParameter("FAVORITEACTIVITY_XRI"));
		commandAsString = request.getParameter("command");
		if (request.getParameter("Fcommand") != null && request.getParameter("Fcommand").length() > 0) {
				commandAsString = request.getParameter("Fcommand");
		}
	}
	
	javax.jdo.PersistenceManager pm = app.getNewPmData();
	String requestIdParam = Action.PARAMETER_REQUEST_ID + "=" + requestId;
	String xriParam = Action.PARAMETER_OBJECTXRI + "=" + java.net.URLEncoder.encode(objectXri);
	if(objectXri == null || app == null || viewsCache.getView(requestId) == null) {
		session.setAttribute(WIZARD_NAME, null);
		response.sendRedirect(
			request.getContextPath() + "/" + WebKeys.SERVLET_NAME
		);
		return;
	}
	Texts_1_0 texts = app.getTexts();
	org.openmdx.portal.servlet.Codes codes = app.getCodes();

	// Get Parameters
	if(commandAsString == null || commandAsString.length() == 0) commandAsString = Command.NA.toString();
	//System.out.println(commandAsString);
	Command command = Command.valueOf(commandAsString);
	RefObject_1_0 obj = (RefObject_1_0)pm.getObjectById(new Path(objectXri));
	String providerName = obj.refGetPath().get(2);
	String segmentName = obj.refGetPath().get(4);
	
	String ACTIVITY_XRI = (request.getParameter("ACTIVITY_XRI") == null ? "" : request.getParameter("ACTIVITY_XRI"));
	if (request.getParameter("FOLLOWUPACTIVITY_XRI") != null && request.getParameter("FOLLOWUPACTIVITY_XRI").length() > 0) {
			ACTIVITY_XRI = request.getParameter("FOLLOWUPACTIVITY_XRI");
			//System.out.println("FOLLOWUPACTIVITY_XRI = " + ACTIVITY_XRI);
	}
	String TRANSITION_XRI = (request.getParameter("TRANSITION_XRI") == null ? "" : request.getParameter("TRANSITION_XRI"));
	//System.out.println("ACTIVITY_XRI = " + ACTIVITY_XRI);
	//System.out.println("TRANSITION_XRI = " + TRANSITION_XRI);

	//System.out.println("FAVORITEACT_XRI = " + FAVORITEACTIVITY_XRI);

	boolean showCompleteProcess = (request.getParameter("showCompleteProcess") != null) && (request.getParameter("showCompleteProcess").length() > 0);
	boolean isFirstCallMain = request.getParameter("isFirstCallMain") == null; // used to properly initialize various options
	if (isFirstCallMain) {
		showCompleteProcess = true;
	}

	// Exit
	if(command == Command.CANCEL) {
		session.setAttribute(WIZARD_NAME, null);
		Action nextAction = new ObjectReference(obj, app).getSelectObjectAction();
		response.sendRedirect(
			request.getContextPath() + "/" + nextAction.getEncodedHRef()
		);
		return;
	}

	org.opencrx.kernel.activity1.jmi1.Activity favoriteActivity = null;
	if(FAVORITEACTIVITY_XRI != null && FAVORITEACTIVITY_XRI.length() > 0) {
		try {
				favoriteActivity = (org.opencrx.kernel.activity1.jmi1.Activity)pm.getObjectById(new Path(FAVORITEACTIVITY_XRI));
		} catch (Exception e) {
				new ServiceException(e).log();
		}
	}

	if(command == Command.DO_FOLLOWUP) {
		org.opencrx.kernel.activity1.jmi1.Activity activity = null;
		org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition transition = null;
		try {
				activity = (org.opencrx.kernel.activity1.jmi1.Activity)pm.getObjectById(new Path(ACTIVITY_XRI));
				transition = (org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition)pm.getObjectById(new Path(TRANSITION_XRI));
		} catch (Exception e) {
				new ServiceException(e).log();
		}
		org.opencrx.kernel.account1.jmi1.Account customer = null;
		if (activity != null && activity.getReportingAccount() != null) {
				try {
						customer = activity.getReportingAccount();
				} catch (Exception e) {
						new ServiceException(e).log();
				}
		}
		boolean isOk = activity != null && doFollowUp(
				activity,
				transition,
				(customer != null && customer.getFullName() != null && customer.getFullName().length() > 0 ? customer.getFullName() : "Kunde fehlt"),
				(transition != null && transition.getName() != null ? transition.getName() : "Name der Transition fehlt"),
				null,
				pm
		);
	}	

	if(command == Command.COMPLETE_ACTIVITY) {
		org.opencrx.kernel.activity1.jmi1.Activity activity = null;
		try {
				activity = (org.opencrx.kernel.activity1.jmi1.Activity)pm.getObjectById(new Path(ACTIVITY_XRI));
		} catch (Exception e) {
				new ServiceException(e).log();
		}
		boolean isOk = activity != null && completeActivity(
				activity,
				pm
		);
	}	

	// get file paths/names of files that had errors

	org.opencrx.kernel.activity1.jmi1.Activity activity = null;
	if (obj instanceof org.opencrx.kernel.activity1.jmi1.Activity) {
			activity = (org.opencrx.kernel.activity1.jmi1.Activity)obj;
	}
	org.opencrx.kernel.account1.jmi1.Segment accountSegment = org.opencrx.kernel.backend.Accounts.getInstance().getAccountSegment(pm, providerName, segmentName);
	org.opencrx.kernel.activity1.jmi1.Segment activitySegment = org.opencrx.kernel.backend.Activities.getInstance().getActivitySegment(pm, providerName, segmentName);
	org.opencrx.kernel.home1.jmi1.Segment homeSegment = org.opencrx.kernel.backend.UserHomes.getInstance().getUserHomeSegment(pm, providerName, segmentName);
	org.opencrx.kernel.home1.jmi1.UserHome currentUserHome = (org.opencrx.kernel.home1.jmi1.UserHome)pm.getObjectById(
		app.getUserHomeIdentityAsPath()
	);
	org.openmdx.security.realm1.jmi1.Realm realm = org.opencrx.kernel.backend.SecureObject.getInstance().getRealm(
		pm,
		providerName,
		segmentName
	);

	UserDefinedView userView = new UserDefinedView(
		pm.getObjectById(new Path(objectXri)),
		app,
		viewsCache.getView(requestId)
	);

%>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html dir="<%= texts.getDir() %>">
<head>
<style type="text/css" media="all">
.processTable {
	table-layout: fixed;
	border-collapse: collapse;
	border: 1px 0px 0px 0px solid grey;
	width: 100%;
	margin-left: 3px;
}

.processTable TD {
	padding: 0px 5px 5px 5px; /* top right bottom left */;
	white-space: nowrap;
	border: 1px 0px 1px 0px solid grey;
	vertical-align: top;
}

.processTable TD.processName {
	width: 150px;
	overflow: hidden;
}

.actopen {
	background-color: #FFC700;
}

.actclosed {
	background-color: #C9FF00;
}

.noact {
	background-color: #849996;
	color: #eee;
}

.drop {
	background-color: #849996;
}

DIV .processHeader {
	background-color: #849996;
	width: 100%;
	padding: 3px 1px 1px 3px;
	overflow:hidden;
}

DIV .processHeader a {
	font-weight: bold;
	font-size:150%;
	color: #eee;
	margin-bottom:3px;
}

DIV .processHeader a:hover {
	color: black;
}

DIV .processHeader DIV{
	display:inline;
	font-weight: bold;
	font-size:150%;
	color: #eee;
	cursor: pointer;
}

.processNode {
	width: 100%;
	padding: 1px 1px 1px 3px;
	font-weight: bold;
	overflow:hidden;
	display:inline-block;
}

.actEntryTable {
	table-layout: fixed;
	border-collapse: collapse;
	width: 100%;
}

.actEntryTable TD {
	padding: 1px 3px; /* top right bottom left */;
	vertical-align: middle;
	white-space: nowrap;
	text-overflow: ellipsis;
	overflow: hidden;
	border: 1px solid grey;
}

.actEntry {
	width: 220px;
	vertical-align: middle;
}

.actFollowUp {
	float: left;
	width: 16px;
	text-align: center;
	cursor:pointer;
}

.followUp {
	background-color: #E9E9E9;
	border: 1px solid black;
	margin: 0 5px 0 0;
}

.invisible {
	display: none;
}

.actHeader {
	
}

.actAssignedTo {
	width: 180px;
}

.actModifiedAt {
	width: 110px;
	text-align: right;
}

.normal {
	font-weight: normal;
}

.transition {
	overflow: hidden;
	cursor: pointer;
}

.actPicker {
	cursor: pointer;
	display:inline-block;
}

DIV.actclosed TD.noact {
	background-color: #83A500;
}

.accountEntryTable {
	float: left;
	margin: 0px 5px 5px 5px;
	border-width: 1px;
	border-spacing: 2px;
	border-style: solid;
	border-color: gray;
	border-collapse: separate;
	width: 161px;
}

.accountEntryTable caption {
	padding: 2px 2px 1px 5px;
	background-color: #eee;
	font-weight: bold;
	text-align: left;
}

.accountEntryTable td {
	border-width: 1px;
	padding: 1px;
	border-style: none;
	border-color: gray;
	vertical-align: middle;
	white-space: nowrap;
	text-overflow: ellipsis;
	overflow: 
}

.legalEntity {
	background-color: #99FFFF;
}

.contact {
	background-color: #FFFB99;
}

.salesRep {;
	
}

.fileDropTable {
	float: left;
	margin: 0px 5px 5px 5px;
	border-width: 1px;
	border-spacing: 2px;
	border-style: solid;
	border-color: #eee;
	border-collapse: separate;
	table-layout: fixed;
	overflow: hidden;
	width: 250px;
}

.fileDropTable caption {
	padding: 2px 2px 1px 5px;
	background-color: #eee;
	font-weight: bold;
	text-align: left;
	white-space: nowrap;
	width: 250px;
}

.fileDropTable tbody {
	border-style: 0px none white;
}

.fileDropTable td {
	border-width: 1px;
	padding: 1px;
	border-style: none;
	border-color: gray;
	vertical-align: middle;
	white-space: nowrap;
	overflow: hidden;
	text-overflow: ellipsis;
}

.fileDrop {
	border: 1px solid #ddd;
}

.emailDropTable {
	float: left;
	margin: 0px 5px 5px 5px;
	border-width: 1px;
	border-spacing: 2px;
	border-style: solid;
	border-color: #eee;
	border-collapse: separate;
	table-layout: fixed;
	overflow: hidden;
	width: 250px;
}

.emailDropTable caption {
	padding: 2px 2px 1px 5px;
	background-color: #eee;
	font-weight: bold;
	text-align: left;
	white-space: nowrap;
	width: 250px;
}

.emailDropTable tbody {
	border-style: 0px none white;
}

.emailDropTable td {
	border-width: 1px;
	padding: 1px;
	border-style: none;
	border-color: gray;
	vertical-align: middle;
	white-space: nowrap;
	overflow: hidden;
	text-overflow: ellipsis;
}

.emailDrop {
	border: 1px solid #ddd;
}

.emailX500Table {
	float: both;
	margin: 0px 5px 5px 5px;
	border-width: 1px;
	border-spacing: 2px;
	border-style: solid;
	border-color: #eee;
	border-collapse: separate;
	table-layout: fixed;
	overflow: hidden;
	width: 98%;
}

.emailX500Table caption {
	padding: 2px 2px 1px 5px;
	background-color: #FFA000;
	font-weight: bold;
	text-align: left;
	vertical-align:middle;
	white-space: nowrap;
	width: 350px;
}

.emailX500Table .col1 {
	width: 150px;
}

.emailX500Table .col2 {
	white-space: nowrap;
}

.emailX500Table tbody {
	border-style: 0px none white;
}

body {
  font-family: "Open Sans", "DejaVu Sans Condensed", "lucida sans", tahoma, verdana, arial, sans-serif;
	padding: 0;
	margin: 0;
}

h1 {
	margin: 0.5em 0em;
	font-size: 150%;
}

h2 {
	font-size: 130%;
	margin: 0.5em 0em;
	text-align: left;
}

textarea,input[type='text'],input[type='password'] {
	width: 100%;
	margin: 0;
	border: 1px solid silver;
	padding: 0;
	font-size: 100%;
  font-family: "Open Sans", "DejaVu Sans Condensed", "lucida sans", tahoma, verdana, arial, sans-serif;
}

input.button {
	-moz-border-radius: 4px;
	-webkit-border-radius: 4x;
	width: 120px;
	border: 1px solid silver;
}

.col1,.col2 {
	float: left;
	width: 49.5%;
}

.small {
	font-size: 8pt;
}

.smallheader {
	text-decoration: underline;
}

.principals tr td {
	vertical-align: top;
	white-space: nowrap;
}
	</style>
	<title>openCRX - Business Process Manager</title>
	<meta http-equiv="Content-Type" content="text/html; charset=utf-8">
	<link rel="stylesheet" href="../../js/bootstrap/css/bootstrap.min.css">	
	<link rel="stylesheet" href="../../_style/colors.css">
	<link rel="stylesheet" href="../../_style/n2default.css">
	<link rel="stylesheet" href="../../_style/ssf.css">
	<link rel="stylesheet" href="../../_style/calendar-small.css">	
	<script language="javascript" type="text/javascript" src="../../js/portal-all.js"></script>
	<!--[if lt IE 7]><script type="text/javascript" src="../../js/iehover-fix.js"></script><![endif]-->
	<script language="javascript" type="text/javascript" src="../../js/calendar/lang/calendar-<%= app.getCurrentLocaleAsString() %>.js"></script>
	<link rel='shortcut icon' href='../../images/favicon.ico' />
</head>
<body>
<div id="container">
<div id="wrap">
<div id="scrollheader" style="height: 90px;">
<div id="logoTable">
<table id="headerlayout">
	<tr id="headRow">
		<td id="head" colspan="2">
		<table id="info">
			<tr>
				<td id="headerCellLeft"><img id="logoLeft"
					src="../../images/logoLeft.gif" alt="openCRX" title="" /></td>
				<td id="headerCellSpacerLeft"></td>
				<td id="headerCellMiddle">&nbsp;</td>
				<td id="headerCellRight"><img id="logoRight"
					src="../../images/logoRight.gif" alt="" title="" /></td>
			</tr>
		</table>
		</td>
	</tr>
</table>
</div>
</div>

<div id="content-wrap">
<div id="content" style="padding: 0px 0.5em 0px 0.5em;">
<%
	int tabIndex = 3000;

// FollowUp
	if(activity != null && (command == Command.PREPARE_FOLLOWUP || command == Command.CREATE_FOLLOWUP)) {
			try {
					activity = (org.opencrx.kernel.activity1.jmi1.Activity)pm.getObjectById(new Path(ACTIVITY_XRI));
			} catch (Exception e) {
					new ServiceException(e).log();
			}
			
    	org.openmdx.ui1.jmi1.FormDefinition doFollowUpFormDefinition = app.getUiFormDefinition(FORM_NAME_DOFOLLOWUP);
    	org.openmdx.portal.servlet.control.FormControl doFollowUpForm = new org.openmdx.portal.servlet.control.FormControl(
    		doFollowUpFormDefinition.refGetPath().getLastSegment().toString(),
    		app.getCurrentLocaleAsString(),
    		app.getCurrentLocaleAsIndex(),
    		app.getUiContext(),
    		doFollowUpFormDefinition
    	);

      Map formValues = new HashMap();
      doFollowUpForm.updateObject(
    		request.getParameterMap(),
    		formValues,
    		app,
    		pm
    	);

		// get additional parameters
		boolean isFirstCall = request.getParameter("isFirstCall") == null; // used to properly initialize various options
		doFollowUpForm.getChildren(UiFieldGroupControl.class);
		if (isFirstCall) {
			// populate form fields related to activity with activity's attribute values
			formValues.put("org:opencrx:kernel:activity1:Activity:assignedTo", activity.getAssignedTo() == null ? null : activity.getAssignedTo().refGetPath());
			formValues.put("org:opencrx:kernel:activity1:Activity:description", activity.getDescription());
			formValues.put("org:opencrx:kernel:activity1:Activity:location", activity.getLocation());
			formValues.put("org:opencrx:kernel:activity1:Activity:priority", activity.getPriority());
			formValues.put("org:opencrx:kernel:activity1:Activity:dueBy", activity.getDueBy());
		}

		if(request.getParameter("resourceContact") != null) {
			org.opencrx.kernel.account1.jmi1.Contact resourceContact = null;
			try {
				resourceContact = (org.opencrx.kernel.account1.jmi1.Contact)pm.getObjectById(new Path(request.getParameter("resourceContact")));
		 	} catch (Exception e) {}
		 	if (resourceContact != null && request.getParameter("fetchResourceContact") != null && request.getParameter("fetchResourceContact").length() > 0) {
				formValues.put("org:opencrx:kernel:activity1:Activity:assignedTo", resourceContact.refGetPath());
		 	}
		}
		 	
 	    org.opencrx.kernel.account1.jmi1.Contact assignedTo = null;
 	    try {
 	    	assignedTo = formValues.get("org:opencrx:kernel:activity1:Activity:assignedTo") != null ?
	 	    	(org.opencrx.kernel.account1.jmi1.Contact)pm.getObjectById(
	 	    		formValues.get("org:opencrx:kernel:activity1:Activity:assignedTo")
	 	    	) : null;
 	    } catch (Exception e) {}

			if(command == Command.CREATE_FOLLOWUP) {
    	    // doFollowUp
    	    org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition transition =
            	(org.opencrx.kernel.activity1.jmi1.ActivityProcessTransition)pm.getObjectById(
            		formValues.get("org:opencrx:kernel:activity1:ActivityDoFollowUpParams:transition")
            	);
    	    String followUpTitle = (String)formValues.get("org:opencrx:kernel:activity1:ActivityDoFollowUpParams:followUpTitle");
    	    String followUpText = (String)formValues.get("org:opencrx:kernel:activity1:ActivityDoFollowUpParams:followUpText");
					org.opencrx.kernel.account1.jmi1.Contact assignTo = formValues.get("org:opencrx:kernel:activity1:ActivityDoFollowUpParams:assignTo") != null ?
    	    	(org.opencrx.kernel.account1.jmi1.Contact)pm.getObjectById(
    	    		formValues.get("org:opencrx:kernel:activity1:ActivityDoFollowUpParams:assignTo")
    	    	) : null;

					// updateActivity
    	    String description = (String)formValues.get("org:opencrx:kernel:activity1:Activity:description");
    	    String location = (String)formValues.get("org:opencrx:kernel:activity1:Activity:location");
    	    Short priority = (Short)formValues.get("org:opencrx:kernel:activity1:Activity:priority");
    	    java.util.Date dueBy = (java.util.Date)formValues.get("org:opencrx:kernel:activity1:Activity:dueBy");

    	    if(transition != null) {
				org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpParams params = org.w3c.spi2.Structures.create(
					org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpParams.class, 
					org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpParams.Member.assignTo, assignTo),
					org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpParams.Member.followUpText, followUpText),
					org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpParams.Member.followUpTitle, followUpTitle),
					org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpParams.Member.parentProcessInstance, null),
					org.w3c.spi2.Datatypes.member(org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpParams.Member.transition, transition)
				); 
		          pm.refresh(activity);
		          pm.currentTransaction().begin();
		    			org.opencrx.kernel.activity1.jmi1.ActivityDoFollowUpResult result = activity.doFollowUp(params);
		    			activity.setAssignedTo(assignedTo);
		    			activity.setDescription(description);
		    			activity.setLocation(location);
		    			activity.setPriority(priority);
		    			activity.setDueBy(dueBy);
		    			pm.currentTransaction().commit();
		    			/*
		    			Action nextAction = new ObjectReference(
		    		    	obj,
		    		    	app
		    		   	).getSelectObjectAction();
		    			response.sendRedirect(
		    				request.getContextPath() + "/" + nextAction.getEncodedHRef()
		    			);
		    			return;
		    			*/
    	    }
    	} else {
    			// prepare FollowUp
		    	TransientObjectView view = new TransientObjectView(
		    		formValues,
		    		app,
		    		(RefObject_1_0)activity,
		    		pm
		    	);
		    	ViewPort p = ViewPortFactory.openPage(
		    		view,
		    		request,
		    		out
		    	);
		    	p.setResourcePathPrefix("../../");
    	
%> <br />
<div class='processNode noact'><%= app.getLabel(ACTIVITYFOLLOWUP_CLASS) %>:<%= app.getHtmlEncoder().encode(new ObjectReference(activity, app).getTitle(), false) %></div>
	<form class="followUp" id="<%= FORM_NAME_DOFOLLOWUP %>"	name="<%= FORM_NAME_DOFOLLOWUP %>" accept-charset="UTF-8" method="POST"	action="<%= "../.." + request.getServletPath() %>">
		<input type="hidden"	name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
		<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>"	value="<%= objectXri %>" /> 
		<input type="hidden" id="Fcommand" name="Fcommand" value="" /> 
		<input type="hidden" name="FOLLOWUPACTIVITY_XRI" id="FOLLOWUPACTIVITY_XRI" value="" />
		<input type="checkbox" style="display: none;" id="isFirstCall" name="isFirstCall" checked="true" />
		<table cellspacing="8" class="tableLayout">
			<tr>
				<td class="cellObject">
				<div class="panel" id="panel<%= FORM_NAME_DOFOLLOWUP %>" style="display:block;overflow:visible;">
<%
					doFollowUpForm.paint(
						p,
						null, // frame
						true // forEditing
					);
					p.flush();
%>
					<table class="fieldGroup">
						<div class="fieldGroupName">&nbsp;</div>
						<tr>
							<td class="<%= CssClass.fieldLabel %>"><span class="nw"><%= app.getLabel(RESOURCE_CLASS) %>:</span>
							</td>
							<td>
								<input type="hidden" id="fetchResourceContact" name="fetchResourceContact" value="" />
								<select	id="resourceContact" name="resourceContact" class="valueL" tabindex="<%= tabIndex++ %>"	onchange="javascript:$('fetchResourceContact').value='override';$('Refresh.Button').click();">
<%
		               // get Resources sorted by name(asc)
		               org.opencrx.kernel.activity1.cci2.ResourceQuery recourceFilter = (org.opencrx.kernel.activity1.cci2.ResourceQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.Resource.class);
		               recourceFilter.orderByName().ascending();
						recourceFilter.forAllDisabled().isFalse();
						int maxResourceToShow = 200;
		              for (
		                 Iterator k = activitySegment.getResource(recourceFilter).iterator();
		                 k.hasNext() && maxResourceToShow > 0;
		                 maxResourceToShow--
		              ) {
		               	try {
		                  // get resource
		            	    org.opencrx.kernel.activity1.jmi1.Resource resource = (org.opencrx.kernel.activity1.jmi1.Resource)k.next();
		            	    org.opencrx.kernel.account1.jmi1.Contact contact = resource.getContact();
		            	    if (contact != null) {
			                  String selectedModifier = ((contact != null ) && (assignedTo != null) && (assignedTo.refMofId().compareTo(contact.refMofId()) == 0)) ? "selected" : "";
%>
												<option <%= selectedModifier %> value="<%= contact.refMofId() %>"><%= resource.getName() + (contact != null ? " (" + contact.getFirstName() + " " + contact.getLastName() + ")": "") %></option>
<%
		            	    }
		               	} catch (Exception e) {}
		              }
%>
								</select>
							</td>
							<td class="addon" />
						</tr>
					</table>
				</div>
				<input type="submit" name="OK.button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="<%= tabIndex++ %>"	value="<%= app.getTexts().getOkTitle() %>" onclick="javascript:$('FOLLOWUPACTIVITY_XRI').value='<%= (activity == null ? "" : activity.refMofId()) %>';$('Fcommand').value='CREATE_FOLLOWUP';this.name='--';" />
				<input type="submit" name="Cancel.button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="<%= tabIndex++ %>" value="<%= app.getTexts().getCancelTitle() %>"	onclick="javascript:$('Fcommand').value='CANCEL_FOLLOWUP';" />
			</td>
		</tr>
	</table>
</form>
<%
    	}
	}
%>

<div>&nbsp;</div>
<%
	ProcessNode processNode = getProcess(activity, pm);
	org.opencrx.kernel.activity1.jmi1.Activity topLevelActivity = processNode.nodeActivity;
	if (favoriteActivity == null) {
			favoriteActivity = topLevelActivity;
	}

	String favoriteActivityHref = null;
	if (favoriteActivity != null) {
		Action action = new Action(
				SelectObjectAction.EVENT_ID,
				new Action.Parameter[]{
						new Action.Parameter(Action.PARAMETER_OBJECTXRI, favoriteActivity.refMofId())
				},
				"",
				true // enabled
			);
		favoriteActivityHref = "../../" + action.getEncodedHRef();
	}
	
	//FileUpload
	if(favoriteActivity != null && (command == Command.FILE_UPLOAD)) {
		int fileCounter = 1;
		String location = app.getTempFileName(fileCounter + "." + UPLOAD_FILE_FIELD_NAME, "");
		while(
			new File(location + ".INFO").exists() &&
			new File(location).exists() &&
			(new File(location).length() > 0)
		) {

			// mimeType and name
			BufferedReader r = new BufferedReader(
			  new FileReader(location + ".INFO")
			);
			String contentMimeType = r.readLine();
			String contentName = r.readLine();
			r.close();
			new File(location + ".INFO").delete();

			if(
				(contentName != null) &&
				(contentName.length() > 0) &&
				(contentMimeType != null) &&
				(contentMimeType.length() > 0)
			) {
				try {
					pm.currentTransaction().begin();

					RefObject_1_0 actobj = (RefObject_1_0)favoriteActivity;
					
					boolean isChecked = false;
					try {
							isChecked = filecb.get(fileCounter-1) != null && ((Boolean)filecb.get(fileCounter-1)).booleanValue();
					} catch (Exception e) {}

					//System.out.println(contentName + " is " + (isChecked ? "" : "not") + " checked");
					// CrxObject
					if (isChecked && actobj instanceof org.opencrx.kernel.generic.jmi1.CrxObject) {
						org.opencrx.kernel.generic.jmi1.CrxObject crxObject =
							(org.opencrx.kernel.generic.jmi1.CrxObject)actobj;
						org.opencrx.kernel.generic.jmi1.Media media = null;
						if(media == null) {
							media = pm.newInstance(org.opencrx.kernel.generic.jmi1.Media.class);
						}
						//media.setDescription(description.length() > 0 ? description : contentName);
						media.setContentName(contentName);
						media.setContentMimeType(contentMimeType);
						media.setContent(
							org.w3c.cci2.BinaryLargeObjects.valueOf(new File(location))
						);
						crxObject.addMedia(
							org.opencrx.kernel.backend.Activities.getInstance().getUidAsString(),
							media
						);
					}

					pm.currentTransaction().commit();
					new File(location).delete();
				}
				catch(Exception e) {
					new ServiceException(e).log();
					try {
						pm.currentTransaction().rollback();
					} catch(Exception e0) {}
				}
			} else {
					System.out.println("empty file at location = " + location);
			}
			fileCounter++;
			location = app.getTempFileName(fileCounter + "." + UPLOAD_FILE_FIELD_NAME, "");
		}
	}

	int numOfDocuments = 0;
	try {
		if (favoriteActivity != null && !favoriteActivity.getMedia().isEmpty()) {
			numOfDocuments = favoriteActivity.getMedia().size();
		}
	} catch (Exception e) {}

		
	//EMailUpload
	if(favoriteActivity != null && (command == Command.EMAIL_UPLOAD)) {
		int fileCounter = 1;
		int roundtripFilesCounter = 0;
		String location = app.getTempFileName(fileCounter + "." + UPLOAD_EMAIL_FIELD_NAME, "");
		while(
			(
				new File(location + ".INFO").exists() &&
				new File(location).exists() &&
				(new File(location).length() > 0)
			)
			||
			(
				roundtripFilesPath.size() > roundtripFilesCounter &&
				roundtripFilesName.size() > roundtripFilesCounter
			)
		) {
			int preErrorCount = errors.size();

			String contentName = null;
			String contentMimeType = null;
			String providedEMailAddress = null;
			
			boolean processNewlyAddedTempFile = (
				new File(location + ".INFO").exists() &&
				new File(location).exists() &&
				(new File(location).length() > 0)
			);
			boolean processRoundtripFile = false;

			if (processNewlyAddedTempFile) {
				// mimeType and name
				BufferedReader r = new BufferedReader(
					new FileReader(location + ".INFO")
				);
				contentMimeType = r.readLine();
				contentName = r.readLine();
				r.close();
				new File(location + ".INFO").delete();
			} else {
				// fetch next roundtripFile
				location = roundtripFilesPath.get(roundtripFilesCounter);
				contentMimeType = "dummy";
				contentName = roundtripFilesName.get(roundtripFilesCounter);
				roundtripFilesCounter++;
				processRoundtripFile = true;
			}

			if(
				(contentName != null) &&
				(contentName.length() > 0) &&
				(contentMimeType != null) &&
				(contentMimeType.length() > 0)
			) {
				try {
					//RefObject_1_0 actobj = (RefObject_1_0)favoriteActivity;
					
					boolean isChecked = false;
					if (processNewlyAddedTempFile) {
						try {
							isChecked = filecb.get(fileCounter-1) != null && ((Boolean)filecb.get(fileCounter-1)).booleanValue();
						} catch (Exception e) {}
					} else {
						isChecked = true;
					}

					//System.out.println(contentName + " is " + (isChecked ? "" : "not") + " checked");
					if (isChecked) {
						javax.mail.internet.MimeMessage msg = null;

						// MSG
						if(contentName != null && contentName.toUpperCase().endsWith(".MSG")) {
							List<String> newErrors = new ArrayList<String>();
							msg = org.opencrx.kernel.utils.MimeUtils.mapMsgToMime(
								new FileInputStream(location),
								accountSegment,
								addressMap, //Collections.<String,String>emptyMap(),
								true, // validateMappedAddresses
								newErrors
							);
							if (!newErrors.isEmpty()) {
								errors.addAll(newErrors);
							}
						}
						// MIME/EML
						else {
							msg = new org.opencrx.kernel.utils.MimeUtils.MimeMessageImpl(
								new FileInputStream(location)
							);																							
						}

						List<org.opencrx.kernel.activity1.jmi1.EMail> emails = null;
						if(preErrorCount == errors.size() && msg != null) {
							emails = org.opencrx.kernel.backend.Activities.getInstance().importMimeMessage(
								pm,
								providerName,
								segmentName,
								msg,
								obj instanceof org.opencrx.kernel.activity1.jmi1.ActivityCreator ? (org.opencrx.kernel.activity1.jmi1.ActivityCreator)obj : null
							);
							new File(location).delete();
						} else {
							hasErrors = true;
							// preserve file path/name for roundtrip
							existingFilesPath.add(location);
							existingFilesName.add(contentName);
						}
						
						//System.out.println("calling importMimeMessage done");
						//System.out.println("emails = " + emails);
						//System.out.println("sendDate = " + msg.getSentDate());
						if (emails != null && !emails.isEmpty()) {
							try {	
								org.opencrx.kernel.activity1.jmi1.EMail importedEMail = (org.opencrx.kernel.activity1.jmi1.EMail)emails.iterator().next();
								// link e-mail to favoriteActivity (if link does not yet exist)
								org.opencrx.kernel.activity1.cci2.ActivityLinkToQuery activityLinkToFilter = (org.opencrx.kernel.activity1.cci2.ActivityLinkToQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityLinkTo.class);
								activityLinkToFilter.activityLinkType().equalTo(
								    new Short(org.opencrx.kernel.backend.Activities.ActivityLinkType.RELATES_TO.getValue())
								);
								activityLinkToFilter.thereExistsLinkTo().equalTo(favoriteActivity);
								List<org.opencrx.kernel.activity1.jmi1.ActivityLinkTo> linkTos = importedEMail.getActivityLinkTo(activityLinkToFilter);
								if (linkTos == null || linkTos.isEmpty()) {
									// add link
									pm.currentTransaction().begin();
									org.opencrx.kernel.activity1.jmi1.ActivityLinkTo activityLinkTo = pm.newInstance(org.opencrx.kernel.activity1.jmi1.ActivityLinkTo.class);
									activityLinkTo.setLinkTo(favoriteActivity);
									activityLinkTo.setName("activity:" + favoriteActivity.getActivityNumber());
									activityLinkTo.setActivityLinkType(org.opencrx.kernel.backend.Activities.ActivityLinkType.RELATES_TO.getValue()); // relates to
									importedEMail.addActivityLinkTo(
										false,
										org.opencrx.kernel.backend.Base.getInstance().getUidAsString(),
										activityLinkTo
									);
									pm.currentTransaction().commit();
								}
							}	catch(Exception e) {
								new org.openmdx.base.exception.ServiceException(e).log();
								try {
									pm.currentTransaction().rollback();
								} catch(Exception e1) {}
							}
						}
					}
				}
				catch(Exception e) {
					new ServiceException(e).log();
					try {
						pm.currentTransaction().rollback();
					} catch(Exception e0) {}
				}
			} else {
					System.out.println("empty file at location = " + location);
			}
			fileCounter++;
			location = app.getTempFileName(fileCounter + "." + UPLOAD_EMAIL_FIELD_NAME, "");
		}
	}

	int numOfEmails = 0;
	if (favoriteActivity != null) {
		try {
			org.opencrx.kernel.activity1.cci2.ActivityLinkFromQuery linkFromQuery = (org.opencrx.kernel.activity1.cci2.ActivityLinkFromQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.ActivityLinkFrom.class);
			linkFromQuery.activityLinkType().equalTo(new Short((short)(100 - org.opencrx.kernel.backend.Activities.ActivityLinkType.RELATES_TO.getValue())));
			for (Iterator linkFrom = favoriteActivity.getActivityLinkFrom(linkFromQuery).iterator(); linkFrom.hasNext();) {
					try {
							org.opencrx.kernel.activity1.jmi1.ActivityLinkFrom activityLinkFrom = (org.opencrx.kernel.activity1.jmi1.ActivityLinkFrom)linkFrom.next();
							if (activityLinkFrom.getLinkFrom() != null && activityLinkFrom.getLinkFrom() instanceof org.opencrx.kernel.activity1.jmi1.EMail) {
								numOfEmails++;
							}
					} catch (Exception e) {}
			}
		} catch (Exception e) {}
	}

	// render header
	String activityHref = null;
	if (processNode.nodeActivity != null) {
		Action action = new Action(
				SelectObjectAction.EVENT_ID,
				new Action.Parameter[]{
						new Action.Parameter(Action.PARAMETER_OBJECTXRI, processNode.nodeActivity.refMofId())
				},
				"",
				true // enabled
			);
		activityHref = "../../" + action.getEncodedHRef();
	}
%>
	<div class="processHeader">
		<a href='<%= activityHref %>' target='_blank'><%= processNode.nodeActivity != null ? "#" + app.getHtmlEncoder().encode(new ObjectReference(processNode.nodeActivity, app).getTitle(), false) : "--" %></a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
		<div onclick="javascript:$('command').value='LIST_ALL_DOCS';<%= CLICK_RELOAD %>"><img	src='../../images/Media.gif' /> <%= app.getLabel("org:opencrx:kernel:generic:DocumentAttachment") %> </div>&nbsp;
		<div onclick="javascript:$('command').value='LIST_ALL_EMAILS';<%= CLICK_RELOAD %>"><img src='../../images/EMail.gif' /> <%= app.getLabel("org:opencrx:kernel:activity1:EMail") %>s </div>
		<input type="button" value="<%= app.getTexts().getCloseText() %>" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" tabindex="<%= tabIndex++ %>" style="float:right;" onclick="javascript:location.href='<%= "../.." + request.getServletPath() + "?" + requestIdParam + "&" + xriParam + "&command=CANCEL" %>';" />
	</div>
<%
	// render overview
	org.opencrx.kernel.account1.jmi1.Account customer = null;
	org.opencrx.kernel.account1.jmi1.Account customerContact = null;
	org.opencrx.kernel.account1.jmi1.Account assignedToContact = null;
	try {
			if (topLevelActivity.getReportingAccount() != null) {
					customer = topLevelActivity.getReportingAccount();
			}
	} catch (Exception e) {
			new ServiceException(e).log();
	}
	try {
			if (topLevelActivity.getReportingContact() != null) {
					customerContact = topLevelActivity.getReportingContact();
			}
	} catch (Exception e) {
			new ServiceException(e).log();
	}
	try {
		if (topLevelActivity.getAssignedTo() != null) {
				assignedToContact = topLevelActivity.getAssignedTo();
		}
	} catch (Exception e) {
			new ServiceException(e).log();
	}

%> <%= getAccountEntry(
						customer,
						userView.getFieldLabel(ACTIVITY_CLASS, "reportingAccount", app.getCurrentLocaleAsIndex()),
						app.getLabel(customer != null ? customer.refClass().refMofId().toString() : ACCOUNT_CLASS),
						userView,
						app
				) %> <%= getAccountEntry(
						customerContact, 
						userView.getFieldLabel(ACTIVITY_CLASS, "reportingContact", app.getCurrentLocaleAsIndex()),
						app.getLabel(customerContact != null ? customerContact.refClass().refMofId().toString() : CONTACT_CLASS), 
						userView,
						app
				) %>
<%
				// get Task Parties
				List<org.opencrx.kernel.activity1.jmi1.TaskParty> taskParties = null;
				try {
						if (topLevelActivity instanceof org.opencrx.kernel.activity1.jmi1.Task) {
								org.opencrx.kernel.activity1.cci2.TaskPartyQuery taskPartyFilter = (org.opencrx.kernel.activity1.cci2.TaskPartyQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.TaskParty.class);
								taskPartyFilter.orderByCreatedAt().ascending();
								taskPartyFilter.partyType().notEqualTo(new Short((short)460)); // exclude organizer
								taskParties = ((org.opencrx.kernel.activity1.jmi1.Task)topLevelActivity).getTaskParty(taskPartyFilter);
						}
				} catch (Exception e) {
						new ServiceException(e).log();
				}
				if (taskParties != null && !taskParties.isEmpty()) {
						for (Iterator tp = taskParties.iterator(); tp.hasNext();) {
								org.opencrx.kernel.activity1.jmi1.TaskParty taskParty = (org.opencrx.kernel.activity1.jmi1.TaskParty)tp.next();
								String role = "--";
								try {
										role = (String)(codes.getLongText("org:opencrx:kernel:activity1:TaskParty:partyType", app.getCurrentLocaleAsIndex(), true, true).get(new Short(taskParty.getPartyType())));
								} catch (Exception e) {}
								if (taskParty.getParty() != null) {
%>									<%= getAccountEntry(
												taskParty.getParty(), 
												role,
												app.getLabel(taskParty.getParty() != null ? taskParty.getParty().refClass().refMofId().toString() : CONTACT_CLASS), 
												userView,
												app
										) %>
<%
								}
						}
				}
				// get Incident Parties
				List<org.opencrx.kernel.activity1.jmi1.IncidentParty> incidentParties = null;
				try {
						if (topLevelActivity instanceof org.opencrx.kernel.activity1.jmi1.Incident) {
								org.opencrx.kernel.activity1.cci2.IncidentPartyQuery incidentPartyFilter = (org.opencrx.kernel.activity1.cci2.IncidentPartyQuery)pm.newQuery(org.opencrx.kernel.activity1.jmi1.IncidentParty.class);
								incidentPartyFilter.orderByCreatedAt().ascending();
								incidentPartyFilter.partyType().notEqualTo(new Short((short)460)); // exclude organizer
								incidentParties = ((org.opencrx.kernel.activity1.jmi1.Incident)topLevelActivity).getIncidentParty(incidentPartyFilter);
						}
				} catch (Exception e) {
						new ServiceException(e).log();
				}
				if (incidentParties != null && !incidentParties.isEmpty()) {
						for (Iterator tp = incidentParties.iterator(); tp.hasNext();) {
								org.opencrx.kernel.activity1.jmi1.IncidentParty incidentParty = (org.opencrx.kernel.activity1.jmi1.IncidentParty)tp.next();
								String role = "--";
								try {
										role = (String)(codes.getLongText("org:opencrx:kernel:activity1:IncidentParty:partyType", app.getCurrentLocaleAsIndex(), true, true).get(new Short(incidentParty.getPartyType())));
								} catch (Exception e) {}
								if (incidentParty.getParty() != null) {
%>									<%= getAccountEntry(
												incidentParty.getParty(), 
												role,
												app.getLabel(incidentParty.getParty() != null ? incidentParty.getParty().refClass().refMofId().toString() : CONTACT_CLASS), 
												userView,
												app
										) %>
<%
								}
						}
				}
%> <%= getAccountEntry(
						assignedToContact, 
						userView.getFieldLabel(ACTIVITY_CLASS, "assignedTo", app.getCurrentLocaleAsIndex()) + " (" + providerName + ")",
						app.getLabel(assignedToContact != null ? assignedToContact.refClass().refMofId().toString() : CONTACT_CLASS), 
						userView,
						app
				) %>

<div style="clear: both; height: 3px;"></div>
<%
/*
List<org.opencrx.kernel.activity1.jmi1.Activity> activities = getActivities(processNode);
if (activities.size() > 0) {
	for(Iterator a = activities.iterator(); a.hasNext();) {
		System.out.println(((org.opencrx.kernel.activity1.jmi1.Activity)a.next()).getActivityNumber());
	}
}
*/
if(command == Command.LIST_ALL_DOCS) {
	try {
		List<org.opencrx.kernel.generic.jmi1.Media> documents = getDocuments(processNode);
		if (documents.size() == 0) {
			command = Command.RELOAD;
		} else {
%>
			<table><tr><td>
			<table id="resultTable" class="gridTableFull">
				<tr class="gridTableHeaderFull"><!-- 6 columns -->
					<td align=left>&nbsp;<%= app.getLabel("org:opencrx:kernel:document1:Document") %></td>
					<td align=left>&nbsp;<%= userView.getFieldLabel(MEDIA_CLASS, "description", app.getCurrentLocaleAsIndex()) %></td>
				</tr>
<%
				Map docsSorted = new TreeMap();
				final String SEPARATOR = "~";
				SimpleDateFormat createdAtDateFormat = new SimpleDateFormat("yyyyMMddHHmm", app.getCurrentLocale());
				
				int counter = 0;
				for(
				    Iterator i = documents.iterator();
				    i.hasNext();
				    counter++
				) {
					org.opencrx.kernel.generic.jmi1.Media media =
						(org.opencrx.kernel.generic.jmi1.Media)i.next();
					docsSorted.put(createdAtDateFormat.format(media.getCreatedAt()), media.refMofId());
				}
				for (
					Iterator i = docsSorted.values().iterator();
					i.hasNext();
				) {
					// get media
					org.opencrx.kernel.generic.jmi1.Media media =
						(org.opencrx.kernel.generic.jmi1.Media)pm.getObjectById(new Path((String)i.next()));;
					ObjectReference mediaRef  = new ObjectReference(media, app);
					String mediaHref = "";
					Action action = new Action(
						SelectObjectAction.EVENT_ID,
						new Action.Parameter[]{
							new Action.Parameter(Action.PARAMETER_OBJECTXRI, media.refMofId())
						},
						"",
						true // enabled
					);
					mediaHref = "../../" + action.getEncodedHRef();
%>
					<tr class="gridTableRow">
					  	<td align=left>
					  		<b><a href="<%= mediaHref %>" target="_blank"><img class="popUpButton" border="1" alt="lookup" title="<%= mediaRef.getLabel() %>" src="../../images/<%= mediaRef.getIconKey() %>" />
					  			<%= media.getContentName() == null ? "--" : media.getContentName() %>
					  		</a></b>
					  	</td>
					  	<td align=left><%= media.getDescription() == null  ? "--" : media.getDescription() %></td>
					</tr>
<%
	    		}
%>
			</table>
			</td></tr></table>
<%
		}
	} catch (Exception e) {
		new ServiceException(e).log();
	}
} else if (command == Command.LIST_ALL_EMAILS) {
	try {
		List<org.opencrx.kernel.activity1.jmi1.EMail> emails = getEMails(processNode, pm);
		if (emails.size() == 0) {
			command = Command.RELOAD;
		} else {
%>
			<table><tr><td>
			<table id="resultTable" class="gridTableFull">
				<tr class="gridTableHeaderFull"><!-- 6 columns -->
					<td align=left>&nbsp;<%= app.getLabel("org:opencrx:kernel:activity1:EMail") %></td>
					<td align=left>&nbsp;<%= userView.getFieldLabel(EMAIL_CLASS, "messageSubject", app.getCurrentLocaleAsIndex()) %></td>
					<td align=left>&nbsp;<%= userView.getFieldLabel(ACTIVITY_CLASS, "assignedTo", app.getCurrentLocaleAsIndex()) %></td>
					<td align=left>&nbsp;<%= userView.getFieldLabel(EMAIL_CLASS, "sendDate", app.getCurrentLocaleAsIndex()) %></td>
				</tr>
<%
				Map emailsSorted = new TreeMap();
				final String SEPARATOR = "~";
				SimpleDateFormat activityDateFormat = new SimpleDateFormat("dd-MMM-yyyy HH:mm", app.getCurrentLocale());
				
				int counter = 0;
				for(
				    Iterator i = emails.iterator();
				    i.hasNext();
				    counter++
				) {
					org.opencrx.kernel.activity1.jmi1.EMail email =
						(org.opencrx.kernel.activity1.jmi1.EMail)i.next();
					emailsSorted.put(email.getActivityNumber(), email.refMofId());
				}
				for (
					Iterator i = emailsSorted.values().iterator();
					i.hasNext();
				) {
					// get email
					org.opencrx.kernel.activity1.jmi1.EMail email =
						(org.opencrx.kernel.activity1.jmi1.EMail)pm.getObjectById(new Path((String)i.next()));;
					ObjectReference emailRef  = new ObjectReference(email, app);
					String emailHref = "";
					Action action = new Action(
						SelectObjectAction.EVENT_ID,
						new Action.Parameter[]{
							new Action.Parameter(Action.PARAMETER_OBJECTXRI, email.refMofId())
						},
						"",
						true // enabled
					);
					emailHref = "../../" + action.getEncodedHRef();
					String body = email.getMessageBody() == null ? "<>" : email.getMessageBody().replace("\"", "'").replace("\n\r", "\n");
					for (Integer k = 0; k<5; k++) {
						body = body.replace("\n\n", "\n");
					}
%>
					<tr class="gridTableRow" title="<%= body %>">
					  	<td align=left><b><a href="<%= emailHref %>" target="_blank"><img class="popUpButton" border="1" alt="lookup" title="<%= emailRef.getLabel() %>" src="../../images/<%= emailRef.getIconKey() %>" /> #<%= email.getActivityNumber() == null ? "--" : email.getActivityNumber() %></a></b></td>
					  	<td align=left><%= email.getMessageSubject() == null ? "--" : email.getMessageSubject() %></td>
					  	<td align=left><%= email.getAssignedTo() == null  ? "--" : (email.getAssignedTo().getFullName() == null ? "--" : email.getAssignedTo().getFullName()) %></td>
					  	<td align=right><%= email.getSendDate() == null  ? "--" : activityDateFormat.format(email.getSendDate()) %></td>
					</tr>
<%
	    		}
%>
			</table>
			</td></tr></table>
<%
		}
	} catch (Exception e) {
		new ServiceException(e).log();
	}
}
%>

<div class="drop" style="padding:2px;">
<%
	if (favoriteActivity != null) {
%>
		&nbsp;&nbsp;<img src='../../images/favorites.gif'/><span class='noact'><%= app.getHtmlEncoder().encode(new ObjectReference(favoriteActivity, app).getTitle(), false) %></span><br>
<%				
	}
%>

	<table class='fileDropTable'>
		<caption>
			<div style='float:right;' onclick="javascript:$('docDrop').style.display='block';this.style.display='none';"><img	src='../../images/filter_down_time.gif' /></div>
			<input type="submit" name="UploadDocs"	id="UploadDocs" style="float: right; visibility: hidden;"	value="<%= app.getTexts().getSaveTitle() %>"	tabindex="<%= tabIndex++ %>" onclick="javascript:$('DOCcommand').value='FILE_UPLOAD';this.name='--';document.forms['DOCS<%= WIZARD_NAME %>'].submit();" />
			<a href='<%= favoriteActivityHref %>' target='_blank'> <img	src='../../images/Media.gif' /> <%= app.getLabel("org:opencrx:kernel:generic:DocumentAttachment") %> (<%= numOfDocuments %>) </a>
		</caption>
		<tr id="docDrop" style="display:none;">
			<td>
				<form id="DOCS<%= WIZARD_NAME %>" name="DOCS<%= WIZARD_NAME %>"	enctype="multipart/form-data" accept-charset="UTF-8" method="POST" action="<%= "../.." + request.getServletPath() %>">
					<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
					<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>"	value="<%= objectXri %>" /> <input type="hidden" name="DOCcommand"	id="DOCcommand" value="NA" /> 
					<input type="hidden" name="FAVORITEACTIVITY_XRI" id="DOCSFAVORITEACTIVITY_XRI" value="<%= FAVORITEACTIVITY_XRI %>" /> 
					<input name="<%= UPLOAD_FILE_FIELD_NAME %>"	id="<%= UPLOAD_FILE_FIELD_NAME %>" size="3" class="fileDrop" title="drop files here" type="file" multiple="multiple" onChange="javascript:$('UploadDocs').style.visibility='visible' ; makeFileList();" />
					<div id="fileList"></div>
					<script type="text/javascript">
								$('<%= UPLOAD_FILE_FIELD_NAME %>').style.height='60px';
							
								function makeFileList() {
									$('<%= UPLOAD_FILE_FIELD_NAME %>').style.height='';
									var input = $("<%= UPLOAD_FILE_FIELD_NAME %>");
									var outerdiv = $("fileList");
									while (outerdiv.hasChildNodes()) {
										outerdiv.removeChild(outerdiv.firstChild);
									}
									for (var i = 0; i < input.files.length; i++) {
										var div = document.createElement("div");
										var cb = document.createElement("input");
										cb.type = "checkbox";
										cb.name = "filecb"+(i+1);
						        cb.id = "filecb"+(i+1);
						        cb.value = input.files[i].name;
						        cb.checked = true;
						        var text = document.createTextNode(input.files[i].name);
										div.appendChild(cb);
										div.appendChild(text);
										outerdiv.appendChild(div);
									}
									if(!outerdiv.hasChildNodes()) {
										outerdiv.innerHTML = '--';
										$('<%= UPLOAD_FILE_FIELD_NAME %>').style.height='60px';
									}
								}
					</script>
				</form>
			</td>
		</tr>
	</table>
	
	<form id="EMAILS<%= WIZARD_NAME %>" name="EMAILS<%= WIZARD_NAME %>"	enctype="multipart/form-data" accept-charset="UTF-8" method="POST"	action="<%= "../.." + request.getServletPath() %>">
		<table class='emailDropTable'>
			<caption>
				<div style='float:right;' onclick="javascript:$('emailDrop').style.display='block';this.style.display='none';"><img	src='../../images/filter_down_time.gif' /></div>
				<input type="submit" name="UploadEmails" id="UploadEmails" style="float: right; visibility: hidden;" value="<%= app.getTexts().getSaveTitle() %>"	tabindex="<%= tabIndex++ %>" onclick="javascript:$('EMAILcommand').value='EMAIL_UPLOAD';this.name='--';document.forms['EMAILS<%= WIZARD_NAME %>'].submit();" />
				<a href='<%= favoriteActivityHref %>' target='_blank'> <img src='../../images/EMail.gif' /> <%= app.getLabel("org:opencrx:kernel:activity1:EMail") %>	(<%= numOfEmails %>)</a>
			</caption>
			<tr id="emailDrop" style="display:none;">
				<td>
						<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" />
						<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>"	value="<%= objectXri %>" />
						<input type="hidden" name="DOCcommand" id="EMAILcommand" value="NA" /> 
						<input type="hidden" name="FAVORITEACTIVITY_XRI" id="EMAILFAVORITEACTIVITY_XRI"	value="<%= FAVORITEACTIVITY_XRI %>" /> 
						<input name="<%= UPLOAD_EMAIL_FIELD_NAME %>" id="<%= UPLOAD_EMAIL_FIELD_NAME %>" size="3" class="fileDrop"	title="drop files here" type="file" multiple="multiple"	onChange="javascript:$('UploadEmails').style.visibility='visible' ; makeEmailList();" />
						<div id="emailList"></div>
						<script type="text/javascript">
									$('<%= UPLOAD_EMAIL_FIELD_NAME %>').style.height='60px';
								
									function makeEmailList() {
										$('<%= UPLOAD_EMAIL_FIELD_NAME %>').style.height='';
										var input = $("<%= UPLOAD_EMAIL_FIELD_NAME %>");
										var outerdiv = $("emailList");
										while (outerdiv.hasChildNodes()) {
											outerdiv.removeChild(outerdiv.firstChild);
										}
										for (var i = 0; i < input.files.length; i++) {
											var div = document.createElement("div");
											var cbtn = document.createElement("input");
											cbtn.type = "checkbox";
											cbtn.name = "filecb"+(i+1);
							        cbtn.id = "filecb"+(i+1);
							        cbtn.value = input.files[i].name;
							        cbtn.checked = true;
							        var text = document.createTextNode(input.files[i].name);
											div.appendChild(cbtn);
											div.appendChild(text);
											outerdiv.appendChild(div);
										}
										if(!outerdiv.hasChildNodes()) {
											outerdiv.innerHTML = '--';
											$('<%= UPLOAD_EMAIL_FIELD_NAME %>').style.height='60px';
										}
									}
						</script>
				</td>
			</tr>
		</table>
<%
		if (hasErrors) {
%>
			<table class="emailX500Table">
				<caption>
					<input type="submit" name="UploadEmailsX500" id="UploadEmails" style="float: right;"" value="<%= app.getTexts().getSaveTitle() %>"	tabindex="<%= tabIndex++ %>" onclick="javascript:$('EMAILcommand').value='EMAIL_UPLOAD';this.name='--';document.forms['EMAILS<%= WIZARD_NAME %>'].submit();" />
					<div style="float:left;padding-top:5px;" >SMTP &lt;--&gt; SMTP / SMTP &lt;--&gt; X.500 </div>
				</caption>
<%
				for (int idx = 0; idx < errors.size(); idx++) {
%>
					<tr>
						<td class="col1"><span class="nw"><%= userView.getFieldLabel("org:opencrx:kernel:activity1:EMailRecipient", "party", app.getCurrentLocaleAsIndex()) %>:</span></td>
						<td class="col2">
<%
							// try to find contacts with machting e-mail
							String locatedEmailAddress = null;
							try {
								org.opencrx.kernel.account1.cci2.EMailAddressQuery emailQuery = (org.opencrx.kernel.account1.cci2.EMailAddressQuery)pm.newQuery(org.opencrx.kernel.account1.jmi1.EMailAddress.class);
								emailQuery.thereExistsEmailAddress().like("(?i)" + errors.get(idx).replace(".", "\\."));
								emailQuery.forAllDisabled().isFalse();
								Collection<org.opencrx.kernel.account1.jmi1.EMailAddress> emailAddresses = accountSegment.getAddress(emailQuery);
								for (Iterator i = emailAddresses.iterator(); i.hasNext() && locatedEmailAddress == null;) {
									try {
										org.opencrx.kernel.account1.jmi1.EMailAddress emailAddress = (org.opencrx.kernel.account1.jmi1.EMailAddress)i.next();
										org.opencrx.kernel.account1.jmi1.Account account = (org.opencrx.kernel.account1.jmi1.Account)pm.getObjectById(new Path(emailAddress.refMofId()).getParent().getParent());
										org.opencrx.kernel.account1.jmi1.AccountAddress[] mainAddresses = org.opencrx.kernel.backend.Accounts.getInstance().getMainAddresses(account);
									    org.opencrx.kernel.account1.jmi1.AccountAddress[] addressesNotMain = org.opencrx.kernel.backend.Accounts.getInstance().getAccountAddresses(
									    		account,
									    		new org.opencrx.kernel.backend.Accounts.AddressFilter(){
									    			@Override
									    			public boolean matches(
									    				org.opencrx.kernel.account1.jmi1.AccountAddress address
									    			) {
									    				boolean isMain = false;
									    				try {
									    					isMain = address.isMain();
									    				} catch(Exception e) {}
									    				return !isMain;
									    			}
									    		},
									    		true // strict
									    );
										
									    if(mainAddresses[org.opencrx.kernel.backend.Accounts.MAIL_BUSINESS] != null) {
									    	locatedEmailAddress = ((org.opencrx.kernel.account1.jmi1.EMailAddress)mainAddresses[org.opencrx.kernel.backend.Accounts.MAIL_BUSINESS]).getEmailAddress();
									    }
									    if((locatedEmailAddress == null || locatedEmailAddress.length() == 0) && addressesNotMain[org.opencrx.kernel.backend.Accounts.MAIL_BUSINESS] != null) {
									    	((org.opencrx.kernel.account1.jmi1.EMailAddress)addressesNotMain[org.opencrx.kernel.backend.Accounts.MAIL_BUSINESS]).getEmailAddress();
									    }
									    if((locatedEmailAddress == null || locatedEmailAddress.length() == 0) && mainAddresses[org.opencrx.kernel.backend.Accounts.MAIL_HOME] != null) {
									    	((org.opencrx.kernel.account1.jmi1.EMailAddress)mainAddresses[org.opencrx.kernel.backend.Accounts.MAIL_HOME]).getEmailAddress();
									    }
									    if((locatedEmailAddress == null || locatedEmailAddress.length() == 0) && addressesNotMain[org.opencrx.kernel.backend.Accounts.MAIL_HOME] != null) {
									    	((org.opencrx.kernel.account1.jmi1.EMailAddress)addressesNotMain[org.opencrx.kernel.backend.Accounts.MAIL_HOME]).getEmailAddress();
									    }
									} catch (Exception e) {
										new ServiceException(e).log();
									}
								}
							} catch (Exception e) {
								new ServiceException(e).log();
							}
%>
							<input type="text" name="email-<%= idx %>" style="width:200px;" size="20" value="<%= locatedEmailAddress == null ? "" : locatedEmailAddress %>" />
							<input type="hidden" name="unmatched-<%= idx %>" value="<%= errors.get(idx) %>" /> 
			 				<span style="white-space:nowrap;overflow:hidden;" title="<%= errors.get(idx) %>"><%= errors.get(idx) %></span>
						</td>
					</tr>
<%
				}
%>
			</table>
			<br>
<%
			// preserve paths and names of existing files for roundtrip
			for (int idx = 0; idx < existingFilesPath.size(); idx++) {
%>
				<input type="hidden" name="filepath-<%= idx %>" value="<%= existingFilesPath.get(idx) %>" />
				<input type="hidden" name="filename-<%= idx %>" value="<%= existingFilesName.get(idx) %>" />
<%
			}
		}
%>
	</form>
</div>

<div style="clear: both; height: 3px;"></div>

<form id="<%= WIZARD_NAME %>" name="<%= WIZARD_NAME %>" method="post"	accept-charset="UTF-8" action="<%= "../.." + request.getServletPath() %>">
	<input type="hidden" name="<%= Action.PARAMETER_REQUEST_ID %>" value="<%= requestId %>" /> 
	<input type="hidden" name="<%= Action.PARAMETER_OBJECTXRI %>" value="<%= objectXri %>" /> 
	<input type="hidden" name="command" id="command" value="NA" /> 
	<input type="hidden" name="FAVORITEACTIVITY_XRI" id="FAVORITEACTIVITY_XRI" value="<%= FAVORITEACTIVITY_XRI %>" /> 
	<input type="hidden" name="ACTIVITY_XRI" id="ACTIVITY_XRI" value="" /> 
	<input type="hidden" name="TRANSITION_XRI" id="TRANSITION_XRI" value="" /> 
	<input type="hidden" name="TITLE" id="TITLE" value="" /> 
	<input type="hidden" name="TEXT" id="TEXT" value="" /> 
	<input type="checkbox" style="display: none;" id="isFirstCallMain" name="isFirstCallMain" checked="true" />
	<input type="Checkbox" name="showCompleteProcess" id="showCompleteProcess" <%= showCompleteProcess ? "checked" : "" %>	tabindex="<%= tabIndex++ %>" value="showCompleteProcess" /> <%= app.getTexts().getShowDetailsTitle() %>&nbsp;&nbsp;
	<input type="submit" name="Reload" id="Reload" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" value="<%= app.getTexts().getReloadText() %>" tabindex="<%= tabIndex++ %>" />
	<input type="button" class="<%= CssClass.btn.toString() %> <%= CssClass.btn_light.toString() %>" value="<%= app.getTexts().getCloseText() %>"	tabindex="<%= tabIndex++ %>" onclick="javascript:location.href='<%= "../.." + request.getServletPath() + "?" + requestIdParam + "&" + xriParam + "&command=CANCEL" %>';" />
	<div style="clear: both; height: 6px;"></div>
	<div class="col1DISABLED">
		<fieldset>
<%
			if (processNode != null) {
%>
				<%= produceTable(processNode, showCompleteProcess, userView, pm, app) %>
<%
			} else {
%>
				<b>no process found</b>
<%
			}
%>
		</fieldset>
		<div>&nbsp;</div>
	</div>
	<br />
</form>
</div>
<!-- content --></div>
<!-- content-wrap --></div>
<!-- wrap --></div>
<!-- container -->
</body>
</html>
<%
if(pm != null) {
	pm.close();
}
%>
