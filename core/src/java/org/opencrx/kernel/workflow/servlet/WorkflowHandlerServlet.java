/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: WorkflowHandlerServlet
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2015, CRIXP Corp., Switzerland
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
package org.opencrx.kernel.workflow.servlet;

import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import javax.jdo.PersistenceManagerFactory;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.opencrx.kernel.backend.UserHomes;
import org.opencrx.kernel.backend.Workflows;
import org.opencrx.kernel.base.jmi1.ExecuteWorkflowParams;
import org.opencrx.kernel.generic.SecurityKeys;
import org.opencrx.kernel.home1.cci2.WfProcessInstanceQuery;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.opencrx.kernel.home1.jmi1.WfProcessInstance;
import org.opencrx.kernel.utils.ScriptUtils;
import org.opencrx.kernel.utils.Utils;
import org.opencrx.kernel.workflow1.jmi1.WfProcess;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.jmi1.ContextCapable;
import org.openmdx.base.naming.Path;
import org.openmdx.base.persistence.cci.PersistenceHelper;
import org.openmdx.kernel.loading.Classes;
import org.openmdx.kernel.log.SysLog;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

/**
 * The WorkflowHandlerServlet scans for non-executed 
 * asynchronous workflows. Non-executed workflows are executed and 
 * marked as executed. The workflows to be executed must implement 
 * the interface org.opencrx.kernel.workflow.AsynchWorkflow_1_0.
 */
public class WorkflowHandlerServlet extends HttpServlet {

	/**
	 * ExecutionStatus
	 *
	 */
	enum ExecutionStatus {
		SUCCESS,
		FAILED,
		SKIPPED
	}

    /* (non-Javadoc)
     * @see javax.servlet.GenericServlet#init(javax.servlet.ServletConfig)
     */
    public void init(
        ServletConfig config
    ) throws ServletException {
        super.init(config);
        try {
            this.pmf = Utils.getPersistenceManagerFactory();
        } catch(Exception e) {
            throw new ServletException("Can not get connection to data provider", e);
        }
    }

    /**
     * WorkflowExecutorThread
     *
     */
    public class WorkflowExecutorThread extends Thread {
    
    	/**
    	 * Constructor.
    	 * 
    	 * @param segmentName
    	 * @param wfProcessInstanceIdentities
    	 * @param beginIndex
    	 * @param endIndex
    	 * @param pmf
    	 */
    	public WorkflowExecutorThread(
    		String segmentName,
    		List<Path> wfProcessInstanceIdentities,
    		boolean isAtomic,
    		int beginIndex,
    		int endIndex,
    		PersistenceManagerFactory pmf
    	) {
    		this.wfProcessInstanceIdentities = wfProcessInstanceIdentities;
    		this.isAtomic = isAtomic;
    		this.beginIndex = beginIndex;
    		this.endIndex = endIndex;
    		this.pm = pmf.getPersistenceManager(
                SecurityKeys.ADMIN_PRINCIPAL + SecurityKeys.ID_SEPARATOR + segmentName,
                null
            );
    	}
    	
        /**
         * Execute workflow.
         * 
         * @param wfInstance
         * @return
         */
        private ExecutionStatus executeWorkflow(
            WfProcessInstance wfInstance,
            boolean isAtomic
        ) {
        	PersistenceManager pm = JDOHelper.getPersistenceManager(wfInstance);
        	SysLog.info("Execute", wfInstance.getProcess().getName());        
            try {
            	WfProcess wfProcess = wfInstance.getProcess();
                String workflowName = wfProcess.getName();
                Boolean isSynchronous = wfProcess.isSynchronous();
                // Synchronous workflow
                if(Boolean.TRUE.equals(isSynchronous)) {
                	// Synchronous workflows are by definition atomic
                	if(isAtomic) {
    	                UserHome userHome = (UserHome)pm.getObjectById(
    	                    wfInstance.refGetPath().getParent().getParent()
    	                );
    	                ContextCapable targetObject = null;
    	                try {
    	                	targetObject = (ContextCapable)pm.getObjectById(
    		                	wfInstance.getTargetObject()
    		                );
    	                } catch(Exception e) {}
    	                if(targetObject != null) {
    		                ExecuteWorkflowParams params = Structures.create(
    		                	ExecuteWorkflowParams.class, 
    		                	Datatypes.member(ExecuteWorkflowParams.Member.targetObject, targetObject),
    		                	Datatypes.member(ExecuteWorkflowParams.Member.triggeredByEventId, wfInstance.refGetPath().getLastSegment().toClassicRepresentation()),
    		                	Datatypes.member(ExecuteWorkflowParams.Member.workflow, wfInstance.getProcess())
    		                );
    		                try {
    		                    pm.currentTransaction().begin();
    		                    userHome.executeWorkflow(params);
    		                    pm.currentTransaction().commit();
    		                    pm.refresh(wfInstance);
    		                    // Successful execution if workflow was started (and completed)
    		                    return wfInstance.getStartedOn() == null 
    		                    	? ExecutionStatus.FAILED
    		                    	: ExecutionStatus.SUCCESS;
    		                } catch(Exception e) {
    		                	SysLog.warning("Can not execute workflow. Exception occurred", "Workflow instance=" + wfInstance + "; home=" + userHome.refGetPath());
    		                	SysLog.warning(e.getMessage(), e.getCause());
    		                    try {
    		                        pm.currentTransaction().rollback();
    		                    } catch(Exception e0) {}
    		                    return ExecutionStatus.FAILED;
    		                }
    	                } else {
    	                	SysLog.warning("Can not execute workflow. Target object not accessible", "Workflow instance=" + wfInstance + "; home=" + userHome.refGetPath());
    	                    return ExecutionStatus.FAILED;                	
    	                }
                	} else {
                		return ExecutionStatus.SKIPPED;
                	}
                } else {
                    // Asynchronous workflow
                	if(wfProcess.getExecuteScript() == null || wfProcess.getExecuteScript().isEmpty()) {                	
	                    Workflows.AsynchronousWorkflow workflow = null;            
	                    Class<?> workflowClass = null;
	                    try {
	                        workflowClass = Classes.getApplicationClass(
	                            workflowName
	                        );
	                    } catch(ClassNotFoundException e) {
	                    	SysLog.error("Implementation for workflow " + workflowName + " not found");
	                        return ExecutionStatus.FAILED;          
	                    }
	                    // Look up constructor
	                    Constructor<?> workflowConstructor = null;
	                    try {
	                        workflowConstructor = workflowClass.getConstructor(new Class[]{});
	                    } catch(NoSuchMethodException e) {
	                    	SysLog.error("No constructor found for workflow " + workflowName);
	                    }
	                    // Instantiate workflow
	                    try {
	                        workflow = (Workflows.AsynchronousWorkflow)workflowConstructor.newInstance(new Object[]{});
	                    } catch(InstantiationException e) {
	                    	SysLog.error("Can not create workflow (can not instantiate)", workflowName);
	                        return ExecutionStatus.FAILED;
	                    } catch(IllegalAccessException e) {
	                    	SysLog.error("Can not create workflow (illegal access)", workflowName);
	                        return ExecutionStatus.FAILED;
	                    } catch(IllegalArgumentException e) {
	                    	SysLog.error("Can not create workflow (illegal argument)", workflowName);
	                        return ExecutionStatus.FAILED;
	                    } catch(InvocationTargetException e) {
	                    	SysLog.error("Can not create workflow (can not invoke target)", workflowName + "(" + e.getTargetException().getMessage() + ")");
	                        return ExecutionStatus.FAILED;         
	                    }
	                    if(isAtomic == workflow.isAtomic()) {
	    	                workflow.execute(wfInstance);
	    	                SysLog.info("SUCCESS");
	    	                return ExecutionStatus.SUCCESS;
	                    } else {
	                    	return ExecutionStatus.SKIPPED;                	
	                    }
                	} else {
                		Class<?> workflowClass = ScriptUtils.getClass(wfProcess.getExecuteScript());
                		Method executeMethod = null;
	                	try {
	        	            executeMethod = workflowClass.getMethod(
	        	            	"execute",
	        	            	WfProcessInstance.class
	        	            );
	                	} catch(NoSuchMethodException e) {
	                    	SysLog.error("No method 'public static void execute(WfProcessInstance) {}' defined in script for workflow " + workflowName);	                		
	                	}
                		Method isAtomicMethod = null;
	                	try {
	        	            isAtomicMethod = workflowClass.getMethod(
	        	            	"isAtomic"
	        	            );
	                	} catch(NoSuchMethodException e) {
	                    	SysLog.error("No method 'public static boolean isAtomic() {}' defined in script for workflow " + workflowName);	                		
	                	}	        	        
	                    if(isAtomic == (Boolean)isAtomicMethod.invoke(null)) {
            	            executeMethod.invoke(null, wfInstance);
	    	                SysLog.info("SUCCESS");
	    	                return ExecutionStatus.SUCCESS;
	                    } else {
	                    	return ExecutionStatus.SKIPPED;                	
	                    }
                	}
                }
            } catch(Exception e) {
            	SysLog.warning("FAILED", e.getMessage());
                new ServiceException(e).log();
                return ExecutionStatus.FAILED;
            }
        }
    	
    	/* (non-Javadoc)
		 * @see java.lang.Thread#run()
		 */
        @Override
        public void run(
        ) {
        	try {
        		for(int i = this.beginIndex; i < Math.min(this.endIndex, this.wfProcessInstanceIdentities.size()); i++) {
        			Path wfProcessInstanceIdentity = this.wfProcessInstanceIdentities.get(i);
                	WfProcessInstance wfProcessInstance = (WfProcessInstance)this.pm.getObjectById(wfProcessInstanceIdentity);
                    int stepCounter = wfProcessInstance.getStepCounter() == null
                        ? 0
                        : wfProcessInstance.getStepCounter().intValue();
                    boolean maxRetriesReached =
                        (wfProcessInstance.getProcess().getMaxRetries() != null) &&
                        (stepCounter >= wfProcessInstance.getProcess().getMaxRetries().intValue());
                    // Double retry delay after each unsuccessful execution 
                    int retryDelayMillis = (1 << stepCounter) * 1000;
                    boolean maxRetryDelayReached =
                        retryDelayMillis > MAX_RETRY_DELAY_MILLIS;
                    if(
                        !maxRetriesReached &&
                        !maxRetryDelayReached &&
                        ((wfProcessInstance.getLastActivityOn() == null ? 0L : wfProcessInstance.getLastActivityOn().getTime()) + retryDelayMillis < new Date().getTime())
                    ) {
                    	Date startedOn = new Date();
                    	ExecutionStatus status = this.executeWorkflow(
                    		wfProcessInstance,
                    		this.isAtomic
                    	);
                        if(status == ExecutionStatus.SUCCESS) {
                            try {
                            	this.pm.refresh(wfProcessInstance);
                            	this.pm.currentTransaction().begin();
                                wfProcessInstance.setStartedOn(startedOn);
                                wfProcessInstance.setFailed(Boolean.FALSE);
                                wfProcessInstance.setLastActivityOn(new Date());
                                wfProcessInstance.setStepCounter(
                                    new Integer(wfProcessInstance.getStepCounter().intValue() + 1)
                                );
                                this.pm.currentTransaction().commit();
                            } catch(Exception e) {
                            	SysLog.info(e.getMessage(), e.getCause());
                                try {
                                	this.pm.currentTransaction().rollback();
                                } catch(Exception e0) {}
                            }
                        } else if(status == ExecutionStatus.FAILED) {
                            try {
                            	this.pm.currentTransaction().begin();
                                wfProcessInstance.setLastActivityOn(new Date());
                                wfProcessInstance.setStepCounter(
                                    new Integer(wfProcessInstance.getStepCounter().intValue() + 1)
                                );
                                this.pm.currentTransaction().commit();
                            } catch(Exception e) {
                            	SysLog.info(e.getMessage(), e.getCause());
                                try {
                                	this.pm.currentTransaction().rollback();
                                } catch(Exception e0) {}
                            }
                        } else {
                        	// The execution was skipped. E.g. this happens in case of 
                        	// asynchronous workflows and isAtomic does not match 
                        	// the atomicity of the workflow
                        }
                    }
                    // Execution fails if maxRetryDelayReached || maxRetriesReached 
                    else if(maxRetryDelayReached || maxRetriesReached) {
                        try {
                        	this.pm.currentTransaction().begin();
                            wfProcessInstance.setStartedOn(new Date());
                            wfProcessInstance.setFailed(Boolean.TRUE);                    
                            this.pm.currentTransaction().commit();
                        } catch(Exception e) {
                        	SysLog.info(e.getMessage(), e.getCause());
                            try {
                            	this.pm.currentTransaction().rollback();
                            } catch(Exception e0) {}                        
                        }
                    }        			
        		}
        	} finally {
        		try {
        			this.pm.close();
        		} catch(Exception ignore) {}
        	}
        }

        //-------------------------------------------------------------------
        // Members
        //-------------------------------------------------------------------
		private final List<Path> wfProcessInstanceIdentities;
		private final boolean isAtomic;
    	private final int beginIndex;
    	private final int endIndex;
    	private final PersistenceManager pm;
    }

    /**
     * Process pending workflows.
     * 
     * @param providerName
     * @param segmentName
     * @param isAtomic
     * @param req
     * @param res
     * @throws IOException
     */
    private void processPendingWorklows(
    	String id,
        String providerName,
        String segmentName,
        boolean isAtomic,
        HttpServletRequest req, 
        HttpServletResponse res
    ) throws IOException {
        System.out.println(new Date().toString() + "  " + WORKFLOW_NAME + " " + id);
        SysLog.detail(WORKFLOW_NAME + " " + id);    
        try {
            PersistenceManager pm = this.pmf.getPersistenceManager(
                SecurityKeys.ADMIN_PRINCIPAL + SecurityKeys.ID_SEPARATOR + segmentName,
                null
            );
            Workflows.getInstance().initWorkflows(
                pm,
                providerName,
                segmentName
            );
            // Get identities of workflows to be executed
            List<Path> wfProcessInstanceIdentities = new ArrayList<Path>();
            if(req.getParameter("xri") != null) {
            	String xri = req.getParameter("xri");
            	try {
            		WfProcessInstance wfProcessInstance = (WfProcessInstance)pm.getObjectById(new Path(xri));
            		if(wfProcessInstance.getStartedOn() == null) {
                    	wfProcessInstanceIdentities.add(wfProcessInstance.refGetPath());
            		} else {
                        System.out.println(new Date() + "  " + WORKFLOW_NAME + " " + id + ": Ignoring " + xri + ". Workflow already completed.");            			
            		}
            	} catch(Exception e) {
                    System.out.println(new Date() + "  " + WORKFLOW_NAME + " " + id + ": unable to retrieve workflow " + xri);            		
            	}
            } else {
                org.opencrx.kernel.home1.jmi1.Segment userHomeSegment = UserHomes.getInstance().getUserHomeSegment(pm, providerName, segmentName);
                WfProcessInstanceQuery query = (WfProcessInstanceQuery)PersistenceHelper.newQuery(
                	pm.getExtent(WfProcessInstance.class),
                	userHomeSegment.refGetPath().getDescendant("userHome", ":*", "wfProcessInstance", ":*")
                );
                query.startedOn().isNull();
                query.orderByStepCounter().ascending(); // process first with lower step counter
                query.orderByCreatedAt().ascending(); // first come - first serve
	            List<WfProcessInstance> wfInstances = userHomeSegment.getExtent(query);
	            for(WfProcessInstance wfInstance: wfInstances) {
	            	wfProcessInstanceIdentities.add(
	            		wfInstance.refGetPath()
	            	);
	            	if(wfProcessInstanceIdentities.size() > BATCH_SIZE) {
	            		break;
	            	}
	            }
            }
            {
                SysLog.info("Executing workflows");
        		List<WorkflowExecutorThread> threads = new ArrayList<WorkflowExecutorThread>();
        		// Up to MAX_THREADS for atomic processes
            	if(isAtomic) {
            		int batchSize = (wfProcessInstanceIdentities.size() / MAX_THREADS) + 1;
            		for(int i = 0; i < MAX_THREADS; i++) {
            			WorkflowExecutorThread t = new WorkflowExecutorThread(
            				segmentName,
            				wfProcessInstanceIdentities,
            				isAtomic,
            				i * batchSize,
            				(i + 1) * batchSize,
            				this.pmf
            			);
            			threads.add(t);
            			t.start();
            		}
            	} else {
                	// Only one thread for non-atomic processes
        			WorkflowExecutorThread t = new WorkflowExecutorThread(
        				segmentName,
        				wfProcessInstanceIdentities,
        				isAtomic,
        				0,
        				wfProcessInstanceIdentities.size(),
        				this.pmf
        			);
        			threads.add(t);
        			t.start();
            	}
            	for(Thread t: threads) {
            		try {
            			t.join();
            		} catch(Exception ignore) {}
            	}
            }
            try {
                pm.close();
            } catch(Exception e) {}
        } catch(Exception e) {
            ServiceException e0 = new ServiceException(e);
            System.out.println(new Date() + "  " + WORKFLOW_NAME + " " + id + ": Exception occured " + e0.getMessage() + ". Continuing");
            SysLog.warning("Exception occured " + e0.getMessage() + ". Continuing");
            SysLog.detail(e0.getMessage(), e0.getCause());
        }
    }    

    /**
     * Handle request.
     * 
     * @param request
     * @param response
     * @throws ServletException
     * @throws IOException
     */
    protected void handleRequest(
        HttpServletRequest request, 
        HttpServletResponse response
    ) throws ServletException, IOException {
        if(System.currentTimeMillis() > this.startedAt + 180000L) {
            String segmentName = request.getParameter("segment");
            String providerName = request.getParameter("provider");
            String baseId = providerName + "/" + segmentName;
            if(COMMAND_EXECUTE.equals(request.getPathInfo())) {
            	// Workflows are processed in two queues: one for
            	// atomic workflows and one for non-atomic workflows.
            	// The workflows in each queue are processed sequentially.
            	// By definitin, atomic workflows do not have side-effects
            	// on non-atomic workflows.
            	for(boolean isAtomic: Arrays.asList(false, true)) {
            		String id = baseId + "[isAtomic=" + isAtomic + "]";
	            	if(!runningSegments.containsKey(id)) {
	            		try {
		                    runningSegments.put(
		                    	id,
		                    	Thread.currentThread()
		                    );
		                    this.processPendingWorklows(
		                    	id,
		                        providerName,
		                        segmentName,
		                        isAtomic,
		                        request,
		                        response
		                    );
		                } catch(Exception e) {
		                	SysLog.warning(e.getMessage(), e.getCause());
		                } finally {
		                    runningSegments.remove(id);
		                }
	            	} else if(
	            		!runningSegments.get(id).isAlive() || 
	            		runningSegments.get(id).isInterrupted()
	            	) {
		            	Thread t = runningSegments.get(id);
	            		System.out.println(new Date() + "  " + WORKFLOW_NAME + " " + id + ": workflow " + t.getId() + " is alive=" + t.isAlive() + "; interrupted=" + t.isInterrupted() + ". Skipping execution.");
	            	}
            	}
            } else {
            	SysLog.warning(WORKFLOW_NAME + " " + baseId + ". Ignoring command. Running segments are", runningSegments);                
            }
        }
    }

    /* (non-Javadoc)
     * @see javax.servlet.http.HttpServlet#doGet(javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse)
     */
    protected void doGet(
        HttpServletRequest req, 
        HttpServletResponse res
    ) throws ServletException, IOException {
        res.setStatus(HttpServletResponse.SC_OK);
        res.flushBuffer();
        this.handleRequest(
            req,
            res
        );
    }
        
    /* (non-Javadoc)
     * @see javax.servlet.http.HttpServlet#doPost(javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse)
     */
    protected void doPost(
        HttpServletRequest req, 
        HttpServletResponse res
    ) throws ServletException, IOException {
        res.setStatus(HttpServletResponse.SC_OK);
        res.flushBuffer();
        this.handleRequest(
            req,
            res
        );
    }
        
    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    private static final long serialVersionUID = 1260441904508971604L;

    private static final String WORKFLOW_NAME = "WorkflowHandler";
    private static final String COMMAND_EXECUTE = "/execute";
    private static final long MAX_RETRY_DELAY_MILLIS = 604800000L; // 7 days
    private static final Map<String,Thread> runningSegments = new ConcurrentHashMap<String,Thread>();    
    private static final int BATCH_SIZE = 500;
    private static final int MAX_THREADS = 5;
    
    private PersistenceManagerFactory pmf = null;
    private long startedAt = System.currentTimeMillis();
}

