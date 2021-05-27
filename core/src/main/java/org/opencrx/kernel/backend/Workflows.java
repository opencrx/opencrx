/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: Workflows
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2014, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.backend;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;

import org.opencrx.application.mail.exporter.ExportMailWorkflow;
import org.opencrx.application.mail.exporter.SendMailNotificationWorkflow;
import org.opencrx.application.mail.exporter.SendMailWorkflow;
import org.opencrx.kernel.base.cci2.StringPropertyQuery;
import org.opencrx.kernel.base.jmi1.ObjectCreationAuditEntry;
import org.opencrx.kernel.base.jmi1.ObjectModificationAuditEntry;
import org.opencrx.kernel.base.jmi1.ObjectRemovalAuditEntry;
import org.opencrx.kernel.base.jmi1.StringProperty;
import org.opencrx.kernel.base.jmi1.WorkflowTarget;
import org.opencrx.kernel.generic.OpenCrxException;
import org.opencrx.kernel.home1.jmi1.AppSyncProfile;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.opencrx.kernel.home1.jmi1.WfActionLogEntry;
import org.opencrx.kernel.home1.jmi1.WfBooleanParameter;
import org.opencrx.kernel.home1.jmi1.WfDecimalParameter;
import org.opencrx.kernel.home1.jmi1.WfIntegerParameter;
import org.opencrx.kernel.home1.jmi1.WfProcessInstance;
import org.opencrx.kernel.home1.jmi1.WfStringParameter;
import org.opencrx.kernel.home1.jmi1.WfUriParameter;
import org.opencrx.kernel.utils.ScriptUtils;
import org.opencrx.kernel.workflow.BulkActivityFollowUpWorkflow;
import org.opencrx.kernel.workflow.BulkCreateActivityWorkflow;
import org.opencrx.kernel.workflow.PrintConsole;
import org.opencrx.kernel.workflow.SendAlert;
import org.opencrx.kernel.workflow1.cci2.TopicQuery;
import org.opencrx.kernel.workflow1.cci2.WfProcessQuery;
import org.opencrx.kernel.workflow1.jmi1.ExporterTask;
import org.opencrx.kernel.workflow1.jmi1.ImporterExporterTask;
import org.opencrx.kernel.workflow1.jmi1.ImporterTask;
import org.opencrx.kernel.workflow1.jmi1.RunExportResult;
import org.opencrx.kernel.workflow1.jmi1.RunImportResult;
import org.opencrx.kernel.workflow1.jmi1.Topic;
import org.opencrx.kernel.workflow1.jmi1.WfProcess;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.jmi1.BasicObject;
import org.openmdx.base.jmi1.ContextCapable;
import org.openmdx.base.naming.Path;
import org.openmdx.base.text.conversion.UUIDConversion;
import org.openmdx.kernel.exception.BasicException;
import org.openmdx.kernel.id.UUIDs;
import org.openmdx.kernel.loading.Classes;
import org.openmdx.kernel.log.SysLog;

/**
 * Workflows
 *
 */
public class Workflows extends AbstractImpl {

	/**
	 * Register backend class.
	 * 
	 */
	public static void register(
	) {
		registerImpl(new Workflows());
	}

	/**
	 * Get instance of registered backend class.
	 * 
	 */
	public static Workflows getInstance(
	) throws ServiceException {
		return getInstance(Workflows.class);
	}

	/**
	 * Constructor.
	 * 
	 */
	protected Workflows(
	) {
		
	}

	/**
	 * Abstract class for synchronous workflows. Synchronous workflows are
	 * by definition atomic, i.e. the may be run in parallel with other workflows.
	 * 
	 */
	public static abstract class SynchronousWorkflow {
	    
	    /**
	     * Executes the workflow for the specified target object. 
	     * 
	     * @param wfTarget
	     * @param targetObject
	     * @param wfProcessInstance
	     * @throws ServiceException
	     */
	    public abstract void execute(
	        WorkflowTarget wfTarget,
	        ContextCapable targetObject,
	        WfProcessInstance wfProcessInstance
	    ) throws ServiceException;

	}

	/**
	 * Abstract class for asynchronous workflows.
	 * 
	 */
	public static abstract class AsynchronousWorkflow {
	    
	    /**
	     * Execute the workflow specified by wfProcessInstance. wfProcessInstance may be
	     * modified by execute.
	     * 
	     * @param wfProcessInstance
	     * @throws ServiceException
	     */
	    public abstract void execute(
	        WfProcessInstance wfProcessInstance
	    ) throws ServiceException;

	    /**
	     * Return true if workflow is atomic. Atomic workflows may run in parallel other
	     * (atomic, non-atomic) workflows, i.e. their execution does not have side-effects 
	     * on other workflows. The default for asynchronous workflows is false.
	     * Override this method to change the default value. 
	     * 
	     * @return
	     */
	    public boolean isAtomic(
	    ) {
	    	return false;
	    }
	    
	}

    /**
     * Get event type for given object.
     * 
     * @param object
     * @return
     */
    public static Workflows.EventType getEventType(
        ContextCapable object
    ) {
    	return object instanceof ObjectRemovalAuditEntry 
    		? Workflows.EventType.OBJECT_REMOVAL 
    		: object instanceof ObjectCreationAuditEntry 
    	  		? Workflows.EventType.OBJECT_CREATION 
    	  		: object instanceof ObjectModificationAuditEntry 
    	  			? Workflows.EventType.OBJECT_REPLACEMENT 
    	  			: Workflows.EventType.NONE;
    }

    /**
     * Get workflow segment.
     * 
     * @param pm
     * @param providerName
     * @param segmentName
     * @return
     */
    public org.opencrx.kernel.workflow1.jmi1.Segment getWorkflowSegment(
        PersistenceManager pm,
        String providerName,
        String segmentName
    ) {
        return (org.opencrx.kernel.workflow1.jmi1.Segment)pm.getObjectById(
            new Path("xri://@openmdx*org.opencrx.kernel.workflow1").getDescendant("provider", providerName, "segment", segmentName)
        );
    }
    
    /**
     * Find topic.
     * 
     * @param name
     * @param segment
     * @param pm
     * @return
     * 
     */
    @Deprecated
    public Topic findTopic(
        String name,
        org.opencrx.kernel.workflow1.jmi1.Segment segment,
        javax.jdo.PersistenceManager pm
    ) {
    	return this.findTopic(name, segment);
    }

    /**
     * Find topic.
     * 
     * @param name
     * @param segment
     * @param pm
     * @return
     */
    public Topic findTopic(
        String name,
        org.opencrx.kernel.workflow1.jmi1.Segment segment
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(segment);
        TopicQuery topicQuery = (TopicQuery)pm.newQuery(Topic.class);
        topicQuery.name().equalTo(name);
        List<Topic> topics = segment.getTopic(topicQuery);
        return topics.isEmpty() 
        	? null 
        	: topics.iterator().next();
    }

    /**
     * Find process.
     * 
     * @param name
     * @param segment
     * @param pm
     * @return
     */
    public WfProcess findWfProcess(
        String name,
        org.opencrx.kernel.workflow1.jmi1.Segment segment
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(segment);
        WfProcessQuery wfProcessQuery = (WfProcessQuery)pm.newQuery(WfProcess.class);
        wfProcessQuery.name().equalTo(name);
        List<WfProcess> wfProcesses = segment.getWfProcess(wfProcessQuery);
        return wfProcesses.isEmpty() 
        	? null 
        	: wfProcesses.iterator().next();
    }

    /**
     * Init topic.
     * 
     * @param workflowSegment
     * @param id
     * @param name
     * @param description
     * @param topicPathPattern
     * @param actions
     * @return
     */
    public Topic initTopic(
        org.opencrx.kernel.workflow1.jmi1.Segment workflowSegment,
        String id,
        String name,
        String description,
        String topicPathPattern,
        WfProcess[] actions
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(workflowSegment);
        Topic topic = null;
        try {
            topic = workflowSegment.getTopic(id);
        } catch(Exception e) {}
        // Do not touch existing topics
        if(topic == null) {
            pm.currentTransaction().begin();
            topic = pm.newInstance(Topic.class);
            topic.setName(name);
            topic.setDescription(description);
            topic.setTopicPathPattern(topicPathPattern);
            topic.getPerformAction().addAll(
                Arrays.asList(actions)
            );
            topic.getOwningGroup().addAll(
                workflowSegment.getOwningGroup()
            );
            workflowSegment.addTopic(
                id,
                topic
            );
            pm.currentTransaction().commit();
        }
        return topic;
    }
    
     /**
      * Init workflow.
      * 
     * @param workflowSegment
     * @param id
     * @param name
     * @param description
     * @param isSynchronous
     * @param properties
     * @return
     */
    public WfProcess initWorkflow(
        org.opencrx.kernel.workflow1.jmi1.Segment workflowSegment,
        String id,
        String name,
        String description,
        Boolean isSynchronous,
        org.opencrx.kernel.base.jmi1.Property[] properties 
    ) {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(workflowSegment);
        WfProcess wfProcess = null;
        try {
            wfProcess = (WfProcess)workflowSegment.getWfProcess(id);
        } catch(Exception e) {}
        if(wfProcess == null) {
            // Add process
            pm.currentTransaction().begin();
            wfProcess = pm.newInstance(WfProcess.class);
            wfProcess.setName(name);
            wfProcess.setDescription(description);
            wfProcess.setSynchronous(isSynchronous);
            wfProcess.setPriority((short)0);
            wfProcess.getOwningGroup().addAll(
                workflowSegment.getOwningGroup()
            );
            workflowSegment.addWfProcess(
                false,                     
                id,
                wfProcess
            );
            pm.currentTransaction().commit();
            // Add properties
            if(properties != null) {
                pm.currentTransaction().begin();
                for(int i = 0; i < properties.length; i++) {
                    wfProcess.addProperty(
                        UUIDConversion.toUID(UUIDs.newUUID()),
                        properties[i]
                    );
                }
                pm.currentTransaction().commit();
            }
        }         
        return wfProcess;
    }
    
    /**
     * Init standard workflows.
     * 
     * @param pm
     * @param providerName
     * @param segmentName
     * @throws ServiceException
     */
    public void initWorkflows(
        PersistenceManager pm,
        String providerName,
        String segmentName
    ) throws ServiceException {
        org.opencrx.kernel.workflow1.jmi1.Segment workflowSegment = this.getWorkflowSegment(
            pm, 
            providerName, 
            segmentName
        );
        // ExportMailWorkflow 
        this.initWorkflow(
            workflowSegment,
            WORKFLOW_EXPORT_MAIL,
            ExportMailWorkflow.class.getName(),
            "Export mails",
            Boolean.FALSE,
            null
        );        
        // SendMailWorkflow
        this.initWorkflow(
            workflowSegment,
            WORKFLOW_SEND_MAIL,
            SendMailWorkflow.class.getName(),
            "Send mails",
            Boolean.FALSE,
            null
        );        
        // SendMailNotificationWorkflow
        WfProcess sendMailNotificationWorkflow = this.initWorkflow(
            workflowSegment,
            WORKFLOW_SEND_MAIL_NOTIFICATION,
            SendMailNotificationWorkflow.class.getName(),
            "Send mail notifications",
            Boolean.FALSE,
            null
        );        
        // SendAlert
        WfProcess sendAlertWorkflow = this.initWorkflow(
            workflowSegment,
            WORKFLOW_SEND_ALERT,
            org.opencrx.kernel.workflow.SendAlert.class.getName(),
            "Send alert",
            Boolean.TRUE,
            null
        );        
        // PrintConsole
        this.initWorkflow(
            workflowSegment,
            WORKFLOW_PRINT_CONSOLE,
            org.opencrx.kernel.workflow.PrintConsole.class.getName(),
            "Print to console",
            Boolean.TRUE,
            null
        );
        // RunExport
        this.initWorkflow(
            workflowSegment,
            WORKFLOW_RUN_EXPORT,
            org.opencrx.kernel.workflow.RunExport.class.getName(),
            "Run export",
            Boolean.FALSE,
            null
        );
        // RunImport
        this.initWorkflow(
            workflowSegment,
            WORKFLOW_RUN_IMPORT,
            org.opencrx.kernel.workflow.RunImport.class.getName(),
            "Run import",
            Boolean.FALSE,
            null
        );
        // BulkCreateActivityWorkflow
        @SuppressWarnings("unused")
        WfProcess bulkCreateActivityWorkflow = this.initWorkflow(
            workflowSegment,
            WORKFLOW_BULK_CREATE_ACTIVITY,
            BulkCreateActivityWorkflow.class.getName(),
            "Perform bulk create activities. Parameters are:\n" +
            "* " + BulkCreateActivityWorkflow.OPTION_LOCALE + ": Locale used to apply place holders\n" +
            "* " + BulkCreateActivityWorkflow.OPTION_DEFAULT_PLACEHOLDERS + ": Default place holders as properties\n" +
            "* " + BulkCreateActivityWorkflow.OPTION_CREATION_TYPE + ": Creation type (CREATE,CREATE\\_CONFIRMED,CREATE\\_TEST,CREATE\\_TEST\\_CONFIRMED)\n" +
            "* " + BulkCreateActivityWorkflow.OPTION_ACCOUNTS_SELECTOR + ": Accounts selector (Group,AccountFilter,AddressFilter,AddressGroup)\n" +
            "* " + BulkCreateActivityWorkflow.OPTION_ACTIVITY_NAME + ": Template activity name\n" +
            "* " + BulkCreateActivityWorkflow.OPTION_ACTIVITY_DESCRIPTION + ": Template activity description\n" +
            "* " + BulkCreateActivityWorkflow.OPTION_ACTIVITY_DETAILED_DESCRIPTION + ": Template activity detailed description\n" +
            "* " + BulkCreateActivityWorkflow.OPTION_ACTIVITY_PRIORITY + ": Template activity priority\n" +
            "* " + BulkCreateActivityWorkflow.OPTION_ACTIVITY_SCHEDULED_START + ": Template activity scheduled start\n" +
            "* " + BulkCreateActivityWorkflow.OPTION_ACTIVITY_SCHEDULED_END + ": Template activity scheduled end\n" +
            "* " + BulkCreateActivityWorkflow.OPTION_ACTIVITY_DUE_BY + ": Template activity due by\n" +
            "* " + BulkCreateActivityWorkflow.OPTION_EMAIL_SENDER + ": Template activity email sender\n" +
            "* " + BulkCreateActivityWorkflow.OPTION_EMAIL_MESSAGE_SUBJECT + ": Template activity email subject\n" +
            "* " + BulkCreateActivityWorkflow.OPTION_EMAIL_MESSAGE_BODY + "[0..9]: Template activity message body\n" +
            "* " + BulkCreateActivityWorkflow.OPTION_EMAIL_ADDRESS_USAGE + "[0..9]: Select email addresses with given usage\n" +
            "* " + BulkCreateActivityWorkflow.OPTION_TEST_ACCOUNT + ": Test account when in test mode\n" +
            "* " + BulkCreateActivityWorkflow.OPTION_TEST_EMAIL + "[0..9]: Test email addresses when in test mode\n" +
            "* " + BulkCreateActivityWorkflow.OPTION_EXCLUDE_NO_BULK_EMAIL + ": Skip accounts with 'no bulk email flag' set",
            Boolean.FALSE,
            null
        );
        // BulkActivityFollowUpWorkflow
        @SuppressWarnings("unused")
        WfProcess bulkActivityFollowUpWorkflow = this.initWorkflow(
            workflowSegment,
            WORKFLOW_BULK_ACTIVITY_FOLLOWUP,
            BulkActivityFollowUpWorkflow.class.getName(),
            "Perform bulk activity follow up. Parameters are:\n" +
            "* " + BulkActivityFollowUpWorkflow.OPTION_ACTIVITY + ": Template activity\n" +
            "* " + BulkActivityFollowUpWorkflow.OPTION_TRANSITION + "[0..9]: Process transition\n" + 
            "* " + BulkActivityFollowUpWorkflow.OPTION_FOLLOWUP_TITLE + "[0..9]: Follow up title\n" +
            "* " + BulkActivityFollowUpWorkflow.OPTION_FOLLOWUP_TEXT + "[0..9]: Follow up text\n" +
            "* " + BulkActivityFollowUpWorkflow.OPTION_ASSIGN_TO + ": Assign activity to contact",
            Boolean.FALSE,
            null
        );
        WfProcess[] sendAlertActions = new WfProcess[]{
            sendAlertWorkflow
        };
        WfProcess[] sendMailNotificationsActions = new WfProcess[]{
            sendMailNotificationWorkflow
        };
        this.initTopic(
            workflowSegment,
            "AccountModifications",            
            TOPIC_NAME_ACCOUNT_MODIFICATIONS,
            "Send alert for modified accounts",
            "xri:@openmdx:org.opencrx.kernel.account1/provider/:*/segment/:*/account/:*",
            sendAlertActions
        );
        this.initTopic(
            workflowSegment,
            "ActivityFollowUpModifications",            
            TOPIC_NAME_ACTIVITY_FOLLOWUP_MODIFICATIONS,
            "Send alert for modified activity follow ups",
            "xri:@openmdx:org.opencrx.kernel.activity1/provider/:*/segment/:*/activity/:*/followUp/:*",
            sendAlertActions
        );
        this.initTopic(
            workflowSegment,
            "ActivityModifications",            
            TOPIC_NAME_ACTIVITY_MODIFICATIONS,
            "Send alert for modified activities",
            "xri:@openmdx:org.opencrx.kernel.activity1/provider/:*/segment/:*/activity/:*",
            sendAlertActions
        );
        this.initTopic(
            workflowSegment,
            "BookingModifications",            
            TOPIC_NAME_BOOKING_MODIFICATIONS,
            "Send alert for modified bookings",
            "xri:@openmdx:org.opencrx.kernel.depot1/provider/:*/segment/:*/booking/:*",
            sendAlertActions
        );
        this.initTopic(
            workflowSegment,
            "Competitor Modifications",            
            TOPIC_NAME_COMPETITOR_MODIFICATIONS,
            "Send alert for modified competitors",
            "xri:@openmdx:org.opencrx.kernel.account1/provider/:*/segment/:*/competitor/:*",
            sendAlertActions
        );
        this.initTopic(
            workflowSegment,
            "CompoundBookingModifications",            
            TOPIC_NAME_COMPOUND_BOOKING_MODIFICATIONS,
            "Send alert for modified compound bookings",
            "xri:@openmdx:org.opencrx.kernel.depot1/provider/:*/segment/:*/cb/:*",
            sendAlertActions
        );
        this.initTopic(
            workflowSegment,
            "InvoiceModifications",            
            TOPIC_NAME_INVOICE_MODIFICATIONS,
            "Send alert for modified invoices",
            "xri:@openmdx:org.opencrx.kernel.contract1/provider/:*/segment/:*/invoice/:*",
            sendAlertActions
        );
        this.initTopic(
            workflowSegment,
            "LeadModifications",            
            TOPIC_NAME_LEAD_MODIFICATIONS,
            "Send alert for modified leads",
            "xri:@openmdx:org.opencrx.kernel.contract1/provider/:*/segment/:*/lead/:*",
            sendAlertActions
        );
        this.initTopic(
            workflowSegment,
            "OpportunityModifications",            
            TOPIC_NAME_OPPORTUNITY_MODIFICATIONS,
            "Send alert for modified opportunities",
            "xri:@openmdx:org.opencrx.kernel.contract1/provider/:*/segment/:*/opportunity/:*",
            sendAlertActions
        );
        this.initTopic(
            workflowSegment,
            "OrganizationModifications",            
            TOPIC_NAME_ORGANIZATION_MODIFICATIONS,
            "Send alert for modified organizations",
            "xri:@openmdx:org.opencrx.kernel.account1/provider/:*/segment/:*/organization/:*",
            sendAlertActions
        );
        this.initTopic(
            workflowSegment,
            "ProductModifications",            
            TOPIC_NAME_PRODUCT_MODIFICATIONS,
            "Send alert for modified products",
            "xri:@openmdx:org.opencrx.kernel.product1/provider/:*/segment/:*/product/:*",
            sendAlertActions
        );
        this.initTopic(
            workflowSegment,
            "QuoteModifications",            
            TOPIC_NAME_QUOTE_MODIFICATIONS,
            "Send alert for modified quotes",
            "xri:@openmdx:org.opencrx.kernel.contract1/provider/:*/segment/:*/quote/:*",
            sendAlertActions
        );
        this.initTopic(
            workflowSegment,
            "SalesOrderModifications",            
            TOPIC_NAME_SALES_ORDER_MODIFICATIONS,
            "Send alert for modified sales orders",
            "xri:@openmdx:org.opencrx.kernel.contract1/provider/:*/segment/:*/salesOrder/:*",
            sendAlertActions
        );
        this.initTopic(
            workflowSegment,
            "AlertModifications",            
            TOPIC_NAME_ALERT_MODIFICATIONS_EMAIL,
            "Send mail for new alerts",
            "xri:@openmdx:org.opencrx.kernel.home1/provider/:*/segment/:*/userHome/:*/alert/:*",
            sendMailNotificationsActions
        );
        this.initTopic(
            workflowSegment,
            "TimerModifications",            
            TOPIC_NAME_TIMER_MODIFICATIONS,
            "Send alert when timer is triggered",
            "xri:@openmdx:org.opencrx.kernel.home1/provider/:*/segment/:*/userHome/:*/timer/:*",
            sendAlertActions
        );
    }

    /**
     * Execute the workflow. Create a workflow instance. If the
     * workflow is synchronous execute it immediately. Asynchronous
     * workflows are executed by the workflow handler.
     *
     * @param name
     * @param wfTarget
     * @param wfProcess
     * @param targetObject
     * @param stringParams
     * @param integerParams
     * @param decimalParams
     * @param booleanParams
     * @param uriParams
     * @param parentProcessInstance
     * @return
     * @throws ServiceException
     */
    public WfProcessInstance executeWorkflow(
    	String name,
    	WorkflowTarget wfTarget,
        WfProcess wfProcess,
        ContextCapable targetObject,
        Map<String,Boolean> booleanParams,
        Map<String,String> stringParams,
        Map<String,Integer> integerParams,
        Map<String,BigDecimal> decimalParams,
        Map<String,Date> dateTimeParams,
        Map<String,Path> uriParams,
        WfProcessInstance parentProcessInstance
    ) throws ServiceException {
        if(wfProcess == null) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.WORKFLOW_MISSING_WORKFLOW,
                "Missing workflow"
            );                                                                
        }
    	PersistenceManager pm = JDOHelper.getPersistenceManager(wfTarget);        
        boolean isSynchronous = Boolean.TRUE.equals(wfProcess.isSynchronous());
        // Target
        if(targetObject == null) {
            throw new ServiceException(
                OpenCrxException.DOMAIN,
                OpenCrxException.WORKFLOW_MISSING_TARGET,
                "Missing target object"
            );
        }
        Path targetObjectIdentity = targetObject.refGetPath();
        // Create workflow instance
        Path wfInstanceIdentity = 
            wfTarget.refGetPath().getDescendant(
                new String[]{
                    "wfProcessInstance", 
                    stringParams == null || stringParams.get(PARAM_NAME_TRIGGERED_BY_EVENT_ID) == null 
                    	? this.getUidAsString() 
                    	: stringParams.get(PARAM_NAME_TRIGGERED_BY_EVENT_ID)
                }
            );
        WfProcessInstance processInstance = null;
        // Try to execute workflow in context of existing workflow instance
        try {
        	processInstance = (WfProcessInstance)pm.getObjectById(wfInstanceIdentity);
        } catch(Exception e) {}
        if(processInstance == null) {
        	processInstance = pm.newInstance(WfProcessInstance.class);
        	processInstance.setName(name == null ? wfProcess.getName() : name);
            processInstance.setStepCounter(new Integer(0));
            processInstance.setProcess(wfProcess);
            processInstance.setTargetObject(targetObjectIdentity.toXRI());            
            processInstance.setFailed(Boolean.FALSE);
            processInstance.setParent(parentProcessInstance);
            if(wfTarget instanceof UserHome) {
	            ((UserHome)wfTarget).addWfProcessInstance(
	            	this.getUidAsString(),
	            	processInstance
	            );
            }
        }
        // Add string parameters
        if(stringParams != null) {
	        for(Map.Entry<String,String> stringParam: stringParams.entrySet()) {
	            WfStringParameter parameter = pm.newInstance(WfStringParameter.class);
	            parameter.setName(stringParam.getKey());
	            parameter.setStringValue(stringParam.getValue());
	            processInstance.addParameter(
	            	this.getUidAsString(),
	            	parameter
	            );
	        }
        }
        // Add integer parameters
        if(integerParams != null) {
	        for(Map.Entry<String,Integer> integerParam: integerParams.entrySet()) {
	            WfIntegerParameter parameter = pm.newInstance(WfIntegerParameter.class);
	            parameter.setName(integerParam.getKey());
	            parameter.setIntegerValue(integerParam.getValue());
	            processInstance.addParameter(
	            	this.getUidAsString(),
	            	parameter
	            );	        	
	        }
        }
        // Add decimal parameters
        if(decimalParams != null) {
	        for(Map.Entry<String,BigDecimal> decimalParam: decimalParams.entrySet()) {
	            WfDecimalParameter parameter = pm.newInstance(WfDecimalParameter.class);
	            parameter.setName(decimalParam.getKey());
	            parameter.setDecimalValue(decimalParam.getValue());
	            processInstance.addParameter(
	            	this.getUidAsString(),
	            	parameter
	            );
	        }
        }
        // Add uri parameters
        if(uriParams != null) {
	        for(Map.Entry<String,Path> uriParam: uriParams.entrySet()) {
	            WfUriParameter parameter = pm.newInstance(WfUriParameter.class);
	            parameter.setName(uriParam.getKey());
	            parameter.setUriValue(uriParam.getValue().toXRI());
	            processInstance.addParameter(
	            	this.getUidAsString(),
	            	parameter
	            );
	        }
        }
        // Add boolean parameters
        if(booleanParams != null) {
	        for(Map.Entry<String,Boolean> booleanParam: booleanParams.entrySet()) {
	            WfBooleanParameter parameter = pm.newInstance(WfBooleanParameter.class);
	            parameter.setName(booleanParam.getKey());
	            parameter.setBooleanValue(booleanParam.getValue());
	            processInstance.addParameter(
	            	this.getUidAsString(),
	            	parameter
	            );	        	
	        }
        }
        // Execute workflow if synchronous  
        if(isSynchronous) {
        	SynchronousWorkflow workflow = null;
        	Class<?> workflowClass = null;
        	if(wfProcess.getExecuteScript() == null || wfProcess.getExecuteScript().isEmpty()) {
        		// Take class name of workflow class from WfProcess::name
	            try {
	                workflowClass = Classes.getApplicationClass(wfProcess.getName());
	            	Constructor<?> workflowConstructor = workflowClass.getConstructor(new Class[]{});
	                workflow = (SynchronousWorkflow)workflowConstructor.newInstance(new Object[]{});
	            } catch(NoSuchMethodException e) {
	                new ServiceException(e).log();
	                throw new ServiceException(
	                    OpenCrxException.DOMAIN,
	                    OpenCrxException.WORKFLOW_MISSING_CONSTRUCTOR,
	                    "missing constructor",
	                    new BasicException.Parameter("param0", wfProcess.getName()),
	                    new BasicException.Parameter("param1", e.getMessage())
	                );                                                                                        
	            } catch(ClassNotFoundException e) {
	                new ServiceException(e).log();
	                throw new ServiceException(
	                    OpenCrxException.DOMAIN,
	                    OpenCrxException.WORKFLOW_NO_IMPLEMENTATION,
	                    "implementation not found",
	                    new BasicException.Parameter("param0", wfProcess.getName()),
	                    new BasicException.Parameter("param1", e.getMessage())
	                );                                                                                        
	            } catch(InstantiationException e) {
	                new ServiceException(e).log();
	                throw new ServiceException(
	                    OpenCrxException.DOMAIN,
	                    OpenCrxException.WORKFLOW_CAN_NOT_INSTANTIATE,
	                    "can not instantiate",
	                    new BasicException.Parameter("param0", wfProcess.getName()),
	                    new BasicException.Parameter("param1", e.getMessage())
	                );                                                                                        
	            } catch(IllegalAccessException e) {
	                new ServiceException(e).log();
	                throw new ServiceException(
	                    OpenCrxException.DOMAIN,
	                    OpenCrxException.WORKFLOW_ILLEGAL_ACCESS,
	                    "illegal access",
	                    new BasicException.Parameter("param0", wfProcess.getName()),
	                    new BasicException.Parameter("param1", e.getMessage())
	                );                                                                            
	            } catch(IllegalArgumentException e) {
	                new ServiceException(e).log();
	                throw new ServiceException(
	                    OpenCrxException.DOMAIN,
	                    OpenCrxException.WORKFLOW_ILLEGAL_ARGUMENT,
	                    "illegal argument",
	                    new BasicException.Parameter("param0", wfProcess.getName()),
	                    new BasicException.Parameter("param1", e.getMessage())
	                );                                                                                        
	            } catch(InvocationTargetException e) {
	                throw new ServiceException(
	                    OpenCrxException.DOMAIN,
	                    OpenCrxException.WORKFLOW_CAN_NOT_INVOKE,
	                    "can not invoke",
	                    new BasicException.Parameter("param0", wfProcess.getName()),
	                    new BasicException.Parameter("param1", e.getTargetException().getMessage())
	                );                                                                                        
	            }
        	} else {
        		// Compile workflow script on-the-fly
       			workflowClass = ScriptUtils.getClass(wfProcess.getExecuteScript());
        	}
        	Method executeMethod = null;
        	try {
	            executeMethod = workflowClass.getMethod(
	            	"execute", 
	            	WorkflowTarget.class,
	            	ContextCapable.class,
	            	WfProcessInstance.class
	            );        		
        	} catch(NoSuchMethodException e) {
                new ServiceException(e).log();
                throw new ServiceException(
                    OpenCrxException.DOMAIN,
                    OpenCrxException.WORKFLOW_MISSING_CONSTRUCTOR,
                    "missing constructor",
                    new BasicException.Parameter("param0", wfProcess.getName()),
                    new BasicException.Parameter("param1", e.getMessage())
                );                                                                                        
            }
            // Execute workflow
            try {
	            executeMethod.invoke(
            		workflow,
                    wfTarget,
                    targetObject,
                    processInstance
                );
                // Update workflow instance after successful execution
                processInstance.setStartedOn(new Date());
                processInstance.setLastActivityOn(new Date());
                processInstance.setFailed(Boolean.FALSE);
            } catch(Exception e) {
                ServiceException e0 = new ServiceException(e);
                SysLog.warning(e0.getMessage(), e0.getCause());
                /**
                  * Exceptions are catched in case of synchronous workflows. This prevents a 
                  * transaction rollback. This behaviour is e.g. required in case of activity
                  * process executions. A failed workflow would result in a rollback of activity
                  * status transitions. 
                  * Instead, the status of the workflow execution is updated and a log entry is 
                  * created. This provides enough information by an external workflow handler to 
                  * re-execute the workflow. The workflow is never re-executed if no retries are 
                  * allowed. In this case the failed status is set immediately. 
                  * A workflow implementation must start its own new transaction if the work in 
                  * case of an exception has to be rolled back.
                  */   
                // Update workflow instance. Set timestamp for last activity
                processInstance.setLastActivityOn(new Date());
                Number maxRetries = wfProcess.getMaxRetries();
                if(
                    (maxRetries == null) ||
                    (maxRetries.intValue() == 0)
                ) {
                    processInstance.setStartedOn(new Date());
                    processInstance.setFailed(Boolean.TRUE);
                }
                // Increment stepCounter
                Number stepCounter = processInstance.getStepCounter();
                if(stepCounter == null) {
                    stepCounter = new Integer(0);
                }
                processInstance.setStepCounter(
                    new Integer(stepCounter.intValue() + 1)
                );
                // Create log entry
                WfActionLogEntry logEntry = pm.newInstance(WfActionLogEntry.class);
                logEntry.setName(e0.getMessage());
                logEntry.setCorrelation(
                	targetObject instanceof BasicObject 
                		? (BasicObject)targetObject 
                		: null
                );
                processInstance.addActionLog(
                	this.getUidAsString(),
                	logEntry
                );
            }
        }
        return processInstance;
    }

    /**
     * Get task parameters and override with supplied params.
     * 
     * @param task
     * @param params
     * @return
     */
    protected List<String> getTaskParams(
    	ImporterExporterTask task,
    	List<String> params
    ) {
        // Get default parameters from task properties
        PersistenceManager pm = JDOHelper.getPersistenceManager(task);
        List<String> importExportParams = new ArrayList<String>();
        StringPropertyQuery propertyQuery = (StringPropertyQuery)pm.newQuery(StringProperty.class);
        propertyQuery.orderByName().ascending();
        for(StringProperty property: task.<StringProperty>getProperty(propertyQuery)) {
        	importExportParams.add(property.getStringValue());
        }
        // Override with supplied params
        for(int i = 0; i < params.size(); i++) {
        	if(i >= importExportParams.size()) {
        		importExportParams.add(params.get(i));
        	} else if(params.get(i) != null) {
        		importExportParams.set(i, params.get(i));
        	}
        }
        return importExportParams;
    }

	/**
	 * Run exporter task on target with given parameters.
	 * 
	 * @param exporterTask
	 * @param params
	 * @return
	 * @throws ServiceException
	 */
	public RunExportResult runExport(
		ExporterTask exporterTask,
		List<String> params
	) throws ServiceException {
		try {
	    	Class<?> clazz = ScriptUtils.getClass(exporterTask.getExecuteScript());
	        Method runExportMethod = clazz.getMethod(
	            "runExport",
	            new Class[] {
	            	ExporterTask.class,
	            	String[].class
	            }
	        );
	        List<String> exportParams = this.getTaskParams(exporterTask, params);
	        return (RunExportResult)runExportMethod.invoke(
	            null,
	            new Object[] {
	            	exporterTask,
	            	exportParams.toArray(new String[exportParams.size()])
	            }
	        );
		} catch(Exception e) {
			throw new ServiceException(e);
		}
	}

	/**
	 * Run importer task on target with given parameters.
	 * 
	 * @param importerTask
	 * @param target
	 * @param params
	 * @return
	 * @throws ServiceException
	 */
	public RunImportResult runImport(
		ImporterTask importerTask,
		List<String> params
	) throws ServiceException {
		try {
	    	Class<?> clazz = ScriptUtils.getClass(importerTask.getExecuteScript());
	        Method runImportMethod = clazz.getMethod(
	            "runImport",
	            new Class[] {
	            	ImporterTask.class,
	            	String[].class
	            }
	        );
	        List<String> importParams = this.getTaskParams(importerTask, params);
	        return (RunImportResult)runImportMethod.invoke(
	            null,
	            new Object[] {
	            	importerTask,
	            	importParams.toArray(new String[importParams.size()])
	            }
	        );
		} catch(Exception e) {
			throw new ServiceException(e);
		}
	}

	/**
	 * Synchronize the data between syncSource and syncTarget. Override 
	 * this method for custom-specific logic. Invoke this method from
	 * custom-specific importer tasks.
	 * 
	 * @param syncSource
	 * @param syncTarget
	 * @param statusMessage
	 * @return
	 */
	public short syncData(
		java.sql.Connection syncSource,
		AppSyncProfile syncTarget,
		StringBuilder statusMessage
	) {
		return -1;
	}

    /**
     * EventType
     *
     */
    public enum EventType {
    	
    	NONE((short)0),
    	OBJECT_CREATION((short)1),
    	OBJECT_REPLACEMENT((short)3),
    	OBJECT_REMOVAL((short)4),
    	TIMER((short)5);
    	
		private short value;
    	
		private EventType(
			short value
		) {
			this.value = value;
			
		}
    	
		public short getValue(
		) {
			return this.value;
		}
    }

    //-------------------------------------------------------------------------
    // Members
    //-------------------------------------------------------------------------
    public static final short STATUS_OK = 0;
    public static final short STATUS_FAILED = 1;

    public static final String PARAM_NAME_TRIGGERED_BY_EVENT_ID = "triggeredByEventId";
    public static final String PARAM_NAME_TRIGGERED_BY = "triggeredBy";
    public static final String PARAM_NAME_TRIGGERED_BY_EVENT_TYPE = "triggeredByEventType";
    
    public static final String WORKFLOW_EXPORT_MAIL = "ExportMail";
    public static final String WORKFLOW_SEND_MAIL = "SendMail";
    public static final String WORKFLOW_SEND_MAIL_NOTIFICATION = "SendMailNotification";
    public static final String WORKFLOW_SEND_ALERT = SendAlert.class.getName();
    public static final String WORKFLOW_PRINT_CONSOLE = PrintConsole.class.getName();
    public static final String WORKFLOW_BULK_ACTIVITY_FOLLOWUP = "BulkActivityFollowUp";
    public static final String WORKFLOW_BULK_CREATE_ACTIVITY = "BulkCreateActivity";
    public static final String WORKFLOW_RUN_EXPORT = org.opencrx.kernel.workflow.RunExport.class.getName();
    public static final String WORKFLOW_RUN_IMPORT = org.opencrx.kernel.workflow.RunImport.class.getName();

    public static final String TOPIC_NAME_ACCOUNT_MODIFICATIONS = "Account Modifications";
    public static final String TOPIC_NAME_ACTIVITY_FOLLOWUP_MODIFICATIONS = "Activity Follow Up Modifications";
    public static final String TOPIC_NAME_ACTIVITY_MODIFICATIONS = "Activity Modifications";
    public static final String TOPIC_NAME_ALERT_MODIFICATIONS_EMAIL = "Alert Modifications";
    public static final String TOPIC_NAME_BOOKING_MODIFICATIONS = "Booking Modifications";
    public static final String TOPIC_NAME_COMPETITOR_MODIFICATIONS = "Competitor Modifications";
    public static final String TOPIC_NAME_COMPOUND_BOOKING_MODIFICATIONS = "Compound Booking Modifications";
    public static final String TOPIC_NAME_INVOICE_MODIFICATIONS = "Invoice Modifications";
    public static final String TOPIC_NAME_LEAD_MODIFICATIONS = "Lead Modifications";
    public static final String TOPIC_NAME_OPPORTUNITY_MODIFICATIONS = "Opportunity Modifications";
    public static final String TOPIC_NAME_ORGANIZATION_MODIFICATIONS = "Organization Modifications";
    public static final String TOPIC_NAME_PRODUCT_MODIFICATIONS = "Product Modifications";
    public static final String TOPIC_NAME_QUOTE_MODIFICATIONS = "Quote Modifications";
    public static final String TOPIC_NAME_SALES_ORDER_MODIFICATIONS = "SalesOrder Modifications";
    public static final String TOPIC_NAME_TIMER_MODIFICATIONS = "Timer Modifications (Alert)";
    
    public static final String WORKFLOW_NAME_PRINT_CONSOLE = PrintConsole.class.getName();
    public static final String WORKFLOW_NAME_SEND_ALERT = SendAlert.class.getName();
    public static final String WORKFLOW_NAME_EXPORT_MAIL = ExportMailWorkflow.class.getName();
    public static final String WORKFLOW_NAME_SEND_MAIL_NOTIFICATION = SendMailNotificationWorkflow.class.getName();
    public static final String WORKFLOW_NAME_SEND_MAIL = SendMailWorkflow.class.getName();
    public static final String WORKFLOW_NAME_BULK_ACTIVITY_FOLLOWUP = org.opencrx.kernel.workflow.BulkActivityFollowUpWorkflow.class.getName();
    public static final String WORKFLOW_NAME_BULK_CREATE_ACTIVITY = org.opencrx.kernel.workflow.BulkCreateActivityWorkflow.class.getName();
    public static final String WORKFLOW_NAME_RUN_EXPORT = org.opencrx.kernel.workflow.RunExport.class.getName();
    public static final String WORKFLOW_NAME_RUN_IMPORT = org.opencrx.kernel.workflow.RunImport.class.getName();

}
