/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: SegmentSetupController
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

import java.io.IOException;
import java.io.Writer;
import java.net.URL;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.opencrx.kernel.account1.jmi1.AccountFilterGlobal;
import org.opencrx.kernel.account1.jmi1.AddressFilterGlobal;
import org.opencrx.kernel.activity1.jmi1.ActivityFilterGlobal;
import org.opencrx.kernel.activity1.jmi1.ActivityProcessState;
import org.opencrx.kernel.backend.Accounts;
import org.opencrx.kernel.backend.Activities;
import org.opencrx.kernel.backend.Admin;
import org.opencrx.kernel.backend.Buildings;
import org.opencrx.kernel.backend.Contracts;
import org.opencrx.kernel.backend.Depots;
import org.opencrx.kernel.backend.Documents;
import org.opencrx.kernel.backend.Products;
import org.opencrx.kernel.backend.SecureObject;
import org.opencrx.kernel.backend.Workflows;
import org.opencrx.kernel.contract1.jmi1.ContractFilterGlobal;
import org.opencrx.kernel.generic.SecurityKeys;
import org.opencrx.kernel.home1.jmi1.QuickAccess;
import org.opencrx.kernel.product1.jmi1.SalesTaxType;
import org.opencrx.security.realm1.jmi1.PrincipalGroup;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.query.ConditionType;
import org.openmdx.base.query.Quantifier;
import org.openmdx.portal.servlet.AbstractWizardController;
import org.openmdx.portal.servlet.Action;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.CssClass;
import org.openmdx.portal.servlet.ObjectReference;
import org.openmdx.portal.servlet.component.View;

/**
 * SegmentSetupController
 *
 */
public class SegmentSetupController extends AbstractWizardController {

	/**
	 * Constructor.
	 * 
	 */
	public SegmentSetupController(
	) {
		super();
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
	
	public void renderSetupReport(
		Writer out
	) throws ServiceException, IOException {
		out.append("<div class=\"row\">");
		out.append("<fieldset>");
		out.append("<div class=\"" + CssClass.fieldGroupName + "\">Activity and Incident Management</div>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");
		out.append("	<tr>");
		out.append("		<th>Activity Processes</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityProcess(Activities.ACTIVITY_PROCESS_NAME_BUG_AND_FEATURE_TRACKING, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_PROCESS_NAME_BUG_AND_FEATURE_TRACKING + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityProcess(Activities.ACTIVITY_PROCESS_NAME_EMAILS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_PROCESS_NAME_EMAILS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityProcess(Activities.ACTIVITY_PROCESS_NAME_BULK_EMAILS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_PROCESS_NAME_BULK_EMAILS + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");
		out.append("	<tr>");
		out.append("		<th>Calendars</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findCalendar(Activities.CALENDAR_NAME_DEFAULT_BUSINESS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.CALENDAR_NAME_DEFAULT_BUSINESS + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");
		out.append("	<tr>");
		out.append("		<th>Activity Types</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityType(Activities.ACTIVITY_TYPE_NAME_BUGS_AND_FEATURES, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_TYPE_NAME_BUGS_AND_FEATURES + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityType(Activities.ACTIVITY_TYPE_NAME_EMAILS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_TYPE_NAME_EMAILS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityType(Activities.ACTIVITY_TYPE_NAME_BULK_EMAILS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_TYPE_NAME_BULK_EMAILS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityType(Activities.ACTIVITY_TYPE_NAME_TASKS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_TYPE_NAME_TASKS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityType(Activities.ACTIVITY_TYPE_NAME_MEETINGS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_TYPE_NAME_MEETINGS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityType(Activities.ACTIVITY_TYPE_NAME_PHONE_CALLS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_TYPE_NAME_PHONE_CALLS + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");		
		out.append("	<tr>");
		out.append("		<th>Activity Trackers</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityTracker(Activities.ACTIVITY_TRACKER_NAME_BUGS_AND_FEATURES, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_TRACKER_NAME_BUGS_AND_FEATURES + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityTracker(Activities.ACTIVITY_TRACKER_NAME_EMAILS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_TRACKER_NAME_EMAILS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityTracker(Activities.ACTIVITY_TRACKER_NAME_TASKS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_TRACKER_NAME_TASKS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityTracker(Activities.ACTIVITY_TRACKER_NAME_POLLS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_TRACKER_NAME_POLLS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityTracker(Activities.ACTIVITY_TRACKER_NAME_MEETING_ROOMS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_TRACKER_NAME_MEETING_ROOMS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityTracker(Activities.ACTIVITY_TRACKER_NAME_MEETINGS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_TRACKER_NAME_MEETINGS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityTracker(Activities.ACTIVITY_TRACKER_NAME_PHONE_CALLS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_TRACKER_NAME_PHONE_CALLS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityTracker(Activities.ACTIVITY_TRACKER_NAME_PUBLIC, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_TRACKER_NAME_PUBLIC + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");		
		out.append("	<tr>");
		out.append("		<th>Activity Creators</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td  class=\"" + (Activities.getInstance().findActivityCreator(Activities.ACTIVITY_CREATOR_NAME_BUGS_AND_FEATURES, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_CREATOR_NAME_BUGS_AND_FEATURES + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityCreator(Activities.ACTIVITY_CREATOR_NAME_EMAILS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_CREATOR_NAME_EMAILS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityCreator(Activities.ACTIVITY_CREATOR_NAME_TASKS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_CREATOR_NAME_TASKS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityCreator(Activities.ACTIVITY_CREATOR_NAME_POLLS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_CREATOR_NAME_POLLS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityCreator(Activities.ACTIVITY_CREATOR_NAME_MEETING_ROOMS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_CREATOR_NAME_MEETING_ROOMS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityCreator(Activities.ACTIVITY_CREATOR_NAME_MEETINGS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_CREATOR_NAME_MEETINGS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityCreator(Activities.ACTIVITY_CREATOR_NAME_PHONE_CALLS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_CREATOR_NAME_PHONE_CALLS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityCreator(Activities.ACTIVITY_CREATOR_NAME_PUBLIC_EMAILS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_CREATOR_NAME_PUBLIC_EMAILS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityCreator(Activities.ACTIVITY_CREATOR_NAME_PUBLIC_TASKS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_CREATOR_NAME_PUBLIC_TASKS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityCreator(Activities.ACTIVITY_CREATOR_NAME_PUBLIC_MEETINGS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_CREATOR_NAME_PUBLIC_MEETINGS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityCreator(Activities.ACTIVITY_CREATOR_NAME_PUBLIC_PHONE_CALLS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Activities.ACTIVITY_CREATOR_NAME_PUBLIC_PHONE_CALLS + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("</fieldset>");
		out.append("<fieldset>");
		out.append("<div class=\"" + CssClass.fieldGroupName + "\">Workflows and Topics</div>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");
		out.append("	<tr>");
		out.append("		<th>Topics</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_ACCOUNT_MODIFICATIONS, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_ACCOUNT_MODIFICATIONS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_ACTIVITY_FOLLOWUP_MODIFICATIONS, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_ACTIVITY_FOLLOWUP_MODIFICATIONS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_ACTIVITY_MODIFICATIONS, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_ACTIVITY_MODIFICATIONS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_ALERT_MODIFICATIONS_EMAIL, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_ALERT_MODIFICATIONS_EMAIL + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_BOOKING_MODIFICATIONS, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_BOOKING_MODIFICATIONS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_COMPETITOR_MODIFICATIONS, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_COMPETITOR_MODIFICATIONS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_COMPOUND_BOOKING_MODIFICATIONS, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_COMPOUND_BOOKING_MODIFICATIONS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_INVOICE_MODIFICATIONS, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_INVOICE_MODIFICATIONS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_LEAD_MODIFICATIONS, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_LEAD_MODIFICATIONS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_OPPORTUNITY_MODIFICATIONS, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_OPPORTUNITY_MODIFICATIONS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_ORGANIZATION_MODIFICATIONS, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_ORGANIZATION_MODIFICATIONS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_PRODUCT_MODIFICATIONS, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_PRODUCT_MODIFICATIONS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_QUOTE_MODIFICATIONS, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_QUOTE_MODIFICATIONS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_TIMER_MODIFICATIONS, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_TIMER_MODIFICATIONS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findTopic(Workflows.TOPIC_NAME_SALES_ORDER_MODIFICATIONS, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.TOPIC_NAME_SALES_ORDER_MODIFICATIONS + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");		
		out.append("	<tr>");
		out.append("		<th>Workflows</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findWfProcess(Workflows.WORKFLOW_NAME_EXPORT_MAIL, this.getWorkflowSegment()) == null && Workflows.getInstance().findWfProcess("org.opencrx.mail.workflow.ExportMailWorkflow", this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.WORKFLOW_NAME_EXPORT_MAIL + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findWfProcess(Workflows.WORKFLOW_NAME_PRINT_CONSOLE, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.WORKFLOW_NAME_PRINT_CONSOLE + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findWfProcess(Workflows.WORKFLOW_NAME_SEND_ALERT, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.WORKFLOW_NAME_SEND_ALERT + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findWfProcess(Workflows.WORKFLOW_NAME_SEND_MAIL, this.getWorkflowSegment()) == null && Workflows.getInstance().findWfProcess("org.opencrx.mail.workflow.SendMailWorkflow", this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.WORKFLOW_NAME_SEND_MAIL + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findWfProcess(Workflows.WORKFLOW_NAME_SEND_MAIL_NOTIFICATION, this.getWorkflowSegment()) == null && Workflows.getInstance().findWfProcess("org.opencrx.mail.workflow.SendMailNotificationWorkflow", this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.WORKFLOW_NAME_SEND_MAIL_NOTIFICATION + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findWfProcess(Workflows.WORKFLOW_NAME_BULK_ACTIVITY_FOLLOWUP, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.WORKFLOW_NAME_BULK_ACTIVITY_FOLLOWUP + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findWfProcess(Workflows.WORKFLOW_NAME_BULK_CREATE_ACTIVITY, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.WORKFLOW_NAME_BULK_CREATE_ACTIVITY + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findWfProcess(Workflows.WORKFLOW_RUN_EXPORT, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.WORKFLOW_RUN_EXPORT + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Workflows.getInstance().findWfProcess(Workflows.WORKFLOW_NAME_RUN_IMPORT, this.getWorkflowSegment()) == null ? MISSING : OK) + "\">" + Workflows.WORKFLOW_NAME_RUN_IMPORT + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("</fieldset>");
		out.append("<fieldset>");
		out.append("<div class=\"" + CssClass.fieldGroupName + "\">Products</div>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");
		out.append("	<tr>");
		out.append("		<th>Pricing Rules</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Products.getInstance().findPricingRule(Products.PRICING_RULE_NAME_LOWEST_PRICE, this.getProductSegment()) == null ? MISSING : OK) + "\">" + Products.PRICING_RULE_NAME_LOWEST_PRICE + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");
		out.append("	<tr>");
		out.append("		<th>Sales Tax Types</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Products.getInstance().findSalesTaxType(Admin.SALES_TAX_TYPE_NAME_8_5, this.getProductSegment()) == null ? MISSING : OK) + "\">" + Admin.SALES_TAX_TYPE_NAME_8_5 + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("</fieldset>");
		out.append("<fieldset>");
		out.append("<div class=\"" + CssClass.fieldGroupName + "\">Contracts</div>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");
		out.append("	<tr>");
		out.append("		<th>Calculation Rules</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Contracts.getInstance().findCalculationRule(Contracts.CALCULATION_RULE_NAME_DEFAULT, this.getContractSegment()) == null ? MISSING : OK) + "\">" + Contracts.CALCULATION_RULE_NAME_DEFAULT + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("</fieldset>");
		out.append("<fieldset>");
		out.append("<div class=\"" + CssClass.fieldGroupName + "\">Reports</div>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");
		out.append("	<tr>");
		out.append("		<th>Account Filters</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Accounts.getInstance().findAccountFilter(Admin.ACCOUNT_FILTER_NAME_ALL, this.getAccountSegment()) == null ? MISSING : OK) + "\">" + Admin.ACCOUNT_FILTER_NAME_ALL + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Accounts.getInstance().findAccountFilter(Admin.ACCOUNT_FILTER_NAME_NO_OR_BROKEN_VCARD, this.getAccountSegment()) == null ? MISSING : OK) + "\">" + Admin.ACCOUNT_FILTER_NAME_NO_OR_BROKEN_VCARD + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Accounts.getInstance().findAddressFilter(Admin.ADDRESS_FILTER_NAME_ALL, this.getAccountSegment()) == null ? MISSING : OK) + "\">" + Admin.ADDRESS_FILTER_NAME_ALL + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");		
		out.append("	<tr>");
		out.append("		<th>Contract Filters</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Contracts.getInstance().findContractFilter(Admin.CONTRACT_FILTER_NAME_LEAD_FORECAST, this.getContractSegment()) == null ? MISSING : OK) + "\">" + Admin.CONTRACT_FILTER_NAME_LEAD_FORECAST + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Contracts.getInstance().findContractFilter(Admin.CONTRACT_FILTER_NAME_OPPORTUNITY_FORECAST, this.getContractSegment()) == null ? MISSING : OK) + "\">" + Admin.CONTRACT_FILTER_NAME_OPPORTUNITY_FORECAST + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Contracts.getInstance().findContractFilter(Admin.CONTRACT_FILTER_NAME_QUOTE_FORECAST, this.getContractSegment()) == null ? MISSING : OK) + "\">" + Admin.CONTRACT_FILTER_NAME_QUOTE_FORECAST + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Contracts.getInstance().findContractFilter(Admin.CONTRACT_FILTER_NAME_WON_LEADS, this.getContractSegment()) == null ? MISSING : OK) + "\">" + Admin.CONTRACT_FILTER_NAME_WON_LEADS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Contracts.getInstance().findContractFilter(Admin.CONTRACT_FILTER_NAME_WON_OPPORTUNITIES, this.getContractSegment()) == null ? MISSING : OK) + "\">" + Admin.CONTRACT_FILTER_NAME_WON_OPPORTUNITIES + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Contracts.getInstance().findContractFilter(Admin.CONTRACT_FILTER_NAME_WON_QUOTES, this.getContractSegment()) == null ? MISSING : OK) + "\">" + Admin.CONTRACT_FILTER_NAME_WON_QUOTES + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("<table class=\"table table-condensed\" style=\"width:100%\">");		
		out.append("	<tr>");
		out.append("		<th>Activity Filters</th>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityFilter(Admin.ACTIVITY_FILTER_NAME_PHONE_CALLS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Admin.ACTIVITY_FILTER_NAME_PHONE_CALLS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityFilter(Admin.ACTIVITY_FILTER_NAME_MEETINGS, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Admin.ACTIVITY_FILTER_NAME_MEETINGS + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityFilter(Admin.ACTIVITY_FILTER_NAME_NEW_ACTIVITIES, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Admin.ACTIVITY_FILTER_NAME_NEW_ACTIVITIES + "</td>");
		out.append("	</tr>");
		out.append("	<tr>");
		out.append("		<td class=\"" + (Activities.getInstance().findActivityFilter(Admin.ACTIVITY_FILTER_NAME_OPEN_ACTIVITIES, this.getActivitySegment()) == null ? MISSING : OK) + "\">" + Admin.ACTIVITY_FILTER_NAME_OPEN_ACTIVITIES + "</td>");
		out.append("	</tr>");
		out.append("</table>");
		out.append("</fieldset>");
		out.append("</div>");
	}

	/**
	 * Refresh action.
	 * 
	 * @throws ServiceException
	 */
	public void doRefresh(
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		ApplicationContext app = this.getApp();
		this.currentUserIsAdmin = app.getCurrentUserRole().equals(
			org.opencrx.kernel.generic.SecurityKeys.ADMIN_PRINCIPAL + org.opencrx.kernel.generic.SecurityKeys.ID_SEPARATOR + this.getSegmentName() + "@" + this.getSegmentName()
		);
		this.accountSegment = Accounts.getInstance().getAccountSegment(pm, this.getProviderName(), this.getSegmentName());
		this.activitySegment = Activities.getInstance().getActivitySegment(pm, this.getProviderName(), this.getSegmentName());
		this.productSegment = Products.getInstance().getProductSegment(pm, this.getProviderName(), this.getSegmentName());
		this.contractSegment = Contracts.getInstance().getContractSegment(pm, this.getProviderName(), this.getSegmentName());
		this.documentSegment = Documents.getInstance().getDocumentSegment(pm, this.getProviderName(), this.getSegmentName());
		this.workflowSegment = Workflows.getInstance().getWorkflowSegment(pm, this.getProviderName(), this.getSegmentName());
		this.depotSegment = Depots.getInstance().getDepotSegment(pm, this.getProviderName(), this.getSegmentName());
		this.buildingSegment = Buildings.getInstance().getBuildingSegment(pm, this.getProviderName(), this.getSegmentName());
		this.userHome = (org.opencrx.kernel.home1.jmi1.UserHome)pm.getObjectById(
			app.getUserHomeIdentityAsPath()
		);
	}

	/**
	 * Setup action.
	 * 
	 * @throws ServiceException
	 */
	public void doSetup(
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		this.doRefresh();
		try {
			Admin.getInstance().initSegments(this.userHome);
		} catch(Exception e) {
			try {
				pm.currentTransaction().rollback();
			} catch(Exception e0) {}
			new ServiceException(e).log();
		}
	}

	/**
	 * @return the currentUserIsAdmin
	 */
	public boolean isCurrentUserIsAdmin() {
		return currentUserIsAdmin;
	}


	/**
	 * @return the accountSegment
	 */
	public org.opencrx.kernel.account1.jmi1.Segment getAccountSegment() {
		return accountSegment;
	}


	/**
	 * @return the activitySegment
	 */
	public org.opencrx.kernel.activity1.jmi1.Segment getActivitySegment() {
		return activitySegment;
	}


	/**
	 * @return the productSegment
	 */
	public org.opencrx.kernel.product1.jmi1.Segment getProductSegment() {
		return productSegment;
	}


	/**
	 * @return the contractSegment
	 */
	public org.opencrx.kernel.contract1.jmi1.Segment getContractSegment() {
		return contractSegment;
	}


	/**
	 * @return the documentSegment
	 */
	public org.opencrx.kernel.document1.jmi1.Segment getDocumentSegment() {
		return documentSegment;
	}

	/**
	 * @return the workflowSegment
	 */
	public org.opencrx.kernel.workflow1.jmi1.Segment getWorkflowSegment() {
		return workflowSegment;
	}	

	/**
	 * @return the userHome
	 */
	public org.opencrx.kernel.home1.jmi1.UserHome getUserHome() {
		return userHome;
	}

	/**
	 * @return the depotSegment
	 */
	public org.opencrx.kernel.depot1.jmi1.Segment getDepotSegment() {
		return depotSegment;
	}
	
	/**
	 * @return the buildingSegment
	 */
	public org.opencrx.kernel.building1.jmi1.Segment getBuildingSegment() {
		return buildingSegment;
	}
	
	public static final String OK = "alert-success";
	public static final String MISSING = "alert-danger";

	private boolean currentUserIsAdmin;
	private org.opencrx.kernel.account1.jmi1.Segment accountSegment;
	private org.opencrx.kernel.activity1.jmi1.Segment activitySegment;
	private org.opencrx.kernel.product1.jmi1.Segment productSegment;
	private org.opencrx.kernel.contract1.jmi1.Segment contractSegment;
	private org.opencrx.kernel.document1.jmi1.Segment documentSegment;	
	private org.opencrx.kernel.depot1.jmi1.Segment depotSegment;	
	private org.opencrx.kernel.building1.jmi1.Segment buildingSegment;
	private org.opencrx.kernel.workflow1.jmi1.Segment workflowSegment;
	private org.opencrx.kernel.home1.jmi1.UserHome userHome;
}
