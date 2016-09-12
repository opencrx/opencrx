/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: IndexerServlet
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2013, CRIXP Corp., Switzerland
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.jdo.PersistenceManager;
import javax.jdo.PersistenceManagerFactory;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.opencrx.kernel.admin1.jmi1.ComponentConfiguration;
import org.opencrx.kernel.backend.Accounts;
import org.opencrx.kernel.backend.Activities;
import org.opencrx.kernel.backend.Buildings;
import org.opencrx.kernel.backend.Contracts;
import org.opencrx.kernel.backend.Depots;
import org.opencrx.kernel.backend.Documents;
import org.opencrx.kernel.backend.Forecasts;
import org.opencrx.kernel.backend.Models;
import org.opencrx.kernel.backend.Products;
import org.opencrx.kernel.backend.UserHomes;
import org.opencrx.kernel.backend.Workflows;
import org.opencrx.kernel.base.jmi1.StringProperty;
import org.opencrx.kernel.base.jmi1.UpdateIndexParams;
import org.opencrx.kernel.base.jmi1.UpdateIndexResult;
import org.opencrx.kernel.generic.SecurityKeys;
import org.opencrx.kernel.layer.persistence.Indexed_2;
import org.opencrx.kernel.utils.ComponentConfigHelper;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.kernel.exception.BasicException;
import org.w3c.spi2.Datatypes;
import org.w3c.spi2.Structures;

/**
 * The IndexerServlet 'listens' for object modifications on incoming
 * audit entries. Modified objects are indexed.
 */  
public class IndexerServlet extends HttpServlet {

    /* (non-Javadoc)
     * @see javax.servlet.GenericServlet#init(javax.servlet.ServletConfig)
     */
    public void init(
        ServletConfig config
    ) throws ServletException {
        super.init(config);        
        try {
            this.pmf = Utils.getPersistenceManagerFactory();
        } catch (Exception e) {
            throw new ServletException("Can not get connection to data provider", e);
        }
    }

    /**
     * Update index. Iterate all segments and invoke operation updateIndex().
     * 
     * @param id
     * @param providerName
     * @param segmentName
     * @param req
     * @param res
     * @throws IOException
     */
    public void updateIndex(
        String id,
        String providerName,
        String segmentName,
        HttpServletRequest req, 
        HttpServletResponse res        
    ) throws IOException {
        System.out.println(new Date().toString() + "  " + WORKFLOW_NAME + " " + providerName + "/" + segmentName);
        try {
            PersistenceManager pm = this.pmf.getPersistenceManager(
                SecurityKeys.ADMIN_PRINCIPAL + SecurityKeys.ID_SEPARATOR + segmentName,
                null
            );
            PersistenceManager rootPm = Utils.getPersistenceManagerFactory().getPersistenceManager(
                SecurityKeys.ROOT_PRINCIPAL,
                null
            );            
            Workflows.getInstance().initWorkflows(
                pm, 
                providerName, 
                segmentName
            );
            ComponentConfiguration componentConfig = 
            	ComponentConfigHelper.getComponentConfiguration(
	            	CONFIGURATION_ID, 
	            	providerName, 
	            	rootPm, 
	            	true,
	            	new String[][]{
	    				{providerName + "." + segmentName + "." + OPTION_KEYWORD_LENGTH_MIN, Integer.toString(Indexed_2.STANDARD_KEYWORD_LENGTH_MIN)},
	    				{providerName + "." + segmentName + "." + OPTION_KEYWORD_LENGTH_MAX, Integer.toString(Indexed_2.STANDARD_KEYWORD_LENGTH_MAX)},
	    				{providerName + "." + segmentName + "." + OPTION_INDEXED_ATTRIBUTES_INCLUDE, ""},
	    				{providerName + "." + segmentName + "." + OPTION_INDEXED_ATTRIBUTES_EXCLUDE, ""}
	            	}
	            );
            StringProperty keywordLengthMinProperty = ComponentConfigHelper.getComponentConfigProperty(
            	providerName + "." + segmentName + "." + OPTION_KEYWORD_LENGTH_MIN,
            	componentConfig
            );
            StringProperty keywordLengthMaxProperty = ComponentConfigHelper.getComponentConfigProperty(
            	providerName + "." + segmentName + "." + OPTION_KEYWORD_LENGTH_MAX,
            	componentConfig
            );
            StringProperty indexedAttributesIncludeProperty = ComponentConfigHelper.getComponentConfigProperty(
            	providerName + "." + segmentName + "." + OPTION_INDEXED_ATTRIBUTES_INCLUDE,
            	componentConfig
            );
            StringProperty indexedAttributesExcludeProperty = ComponentConfigHelper.getComponentConfigProperty(
            	providerName + "." + segmentName + "." + OPTION_INDEXED_ATTRIBUTES_EXCLUDE,
            	componentConfig
            );
            Integer keywordLengthMin = null;
            if(keywordLengthMinProperty != null) {
            	try {
            		keywordLengthMin = Integer.parseInt(keywordLengthMinProperty.getStringValue());
            	} catch(Exception ignore) {}
            }
            Integer keywordLengthMax = null;
            if(keywordLengthMaxProperty != null) {
            	try {
            		keywordLengthMax = Integer.parseInt(keywordLengthMaxProperty.getStringValue());
            	} catch(Exception ignore) {}
            }
            List<String> indexedAttributesInclude = null;
            if(indexedAttributesIncludeProperty != null) {
            	try {
            		indexedAttributesInclude = new ArrayList<String>(
            			Arrays.asList(
            				indexedAttributesIncludeProperty.getStringValue().split(",")
            			)
            		);
            	} catch(Exception ignore) {}
            }
            List<String> indexedAttributesExclude = null;
            if(indexedAttributesExcludeProperty != null) {
            	try {
            		indexedAttributesExclude = new ArrayList<String>(
            			Arrays.asList(
            				indexedAttributesExcludeProperty.getStringValue().split(",")
            			)
            		);
            	} catch(Exception ignore) {}
            }
            List<org.opencrx.kernel.base.jmi1.Indexed> indexedSegments = new ArrayList<org.opencrx.kernel.base.jmi1.Indexed>();
            indexedSegments.add(Accounts.getInstance().getAccountSegment(pm, providerName, segmentName));
            indexedSegments.add(Activities.getInstance().getActivitySegment(pm, providerName, segmentName));
            indexedSegments.add(Buildings.getInstance().getBuildingSegment(pm, providerName, segmentName));
            indexedSegments.add(Contracts.getInstance().getContractSegment(pm, providerName, segmentName));
            indexedSegments.add(Depots.getInstance().getDepotSegment(pm, providerName, segmentName));
            indexedSegments.add(Documents.getInstance().getDocumentSegment(pm, providerName, segmentName));
            indexedSegments.add(Forecasts.getInstance().getForecastSegment(pm, providerName, segmentName));
            indexedSegments.add(Models.getInstance().getModelSegment(pm, providerName, segmentName));
            indexedSegments.add(Products.getInstance().getProductSegment(pm, providerName, segmentName));
            indexedSegments.add(UserHomes.getInstance().getUserHomeSegment(pm, providerName, segmentName));
            for(org.opencrx.kernel.base.jmi1.Indexed indexedSegment: indexedSegments) {
                long startedAt = System.currentTimeMillis();
                try {
        			UpdateIndexParams params = Structures.create(
        				UpdateIndexParams.class, 
        				Datatypes.member(UpdateIndexParams.Member.keywordLengthMin, keywordLengthMin),
        				Datatypes.member(UpdateIndexParams.Member.keywordLengthMax, keywordLengthMax),			
        				Datatypes.member(UpdateIndexParams.Member.indexedAttributesInclude, indexedAttributesInclude),			
        				Datatypes.member(UpdateIndexParams.Member.indexedAttributesExclude, indexedAttributesExclude)			
        			);
                    UpdateIndexResult result = indexedSegment.updateIndex(params);
                    if(result.getNumberOfIndexedObjects() > 0) {
                        long duration = System.currentTimeMillis() - startedAt;
                        System.out.println(new Date().toString() + "  " + WORKFLOW_NAME + " " + providerName + "/" + segmentName + ": Indexed " + indexedSegment.refMofId() + " (" + result.getNumberOfIndexedObjects() + " objects in " + duration + " ms)");
                    }
                } catch(Exception e) {
                    ServiceException e0 = new ServiceException(
                        e,
                        BasicException.Code.DEFAULT_DOMAIN,
                        BasicException.Code.PROCESSING_FAILURE,
                        "Unable to update index",
                        new BasicException.Parameter("segment.xri", indexedSegment.refMofId())
                    );
                    e0.log();
                    System.out.println(new Date() + "  " + WORKFLOW_NAME + " " + providerName + "/" + segmentName + ": exception occured " + e.getMessage() + ". Continuing");
                }        
            }
            try {
                pm.close();
            } catch(Exception e) {}
        } catch(Exception e) {
            new ServiceException(e).log();
            System.out.println(new Date() + "  " + WORKFLOW_NAME + " " + providerName + "/" + segmentName + ": exception occured " + e.getMessage() + ". Continuing");
        }
    }
    
    /**
     * Handle servlet request.
     * 
     * @param req
     * @param res
     * @throws ServletException
     * @throws IOException
     */
    protected void handleRequest(
        HttpServletRequest req, 
        HttpServletResponse res
    ) throws ServletException, IOException {
        if(System.currentTimeMillis() > this.startedAt + STARTUP_DELAY) {
            String segmentName = req.getParameter("segment");
            String providerName = req.getParameter("provider");
            String id = providerName + "/" + segmentName;
            if(COMMAND_EXECUTE.equals(req.getPathInfo())) {
                if(!runningSegments.containsKey(id)) {
	                try {
	                    runningSegments.put(
	                    	id,
	                    	Thread.currentThread()
	                    );
	                    this.updateIndex(
	                        id,
	                        providerName,
	                        segmentName,
	                        req,
	                        res
	                    );
	                } catch(Exception e) {
	                    new ServiceException(e).log();
	                } finally {
	                    runningSegments.remove(id);
	                }
                } else if(
	        		!runningSegments.get(id).isAlive() || 
	        		runningSegments.get(id).isInterrupted()
	        	) {
	            	Thread t = runningSegments.get(id);
	        		System.out.println(new Date() + "  " + WORKFLOW_NAME + " " + providerName + "/" + segmentName + ": workflow " + t.getId() + " is alive=" + t.isAlive() + "; interrupted=" + t.isInterrupted() + ". Skipping execution.");
	        	}
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
    private static final long serialVersionUID = 4441731357561757549L;

    private static final String COMMAND_EXECUTE = "/execute";
    private static final String WORKFLOW_NAME = "Indexer";
    private static final String CONFIGURATION_ID = "Indexer";
    private static final String OPTION_KEYWORD_LENGTH_MIN = "keywordLengthMin";
    private static final String OPTION_KEYWORD_LENGTH_MAX = "keywordLengthMax";
    private static final String OPTION_INDEXED_ATTRIBUTES_INCLUDE = "indexedAttributesInclude";
    private static final String OPTION_INDEXED_ATTRIBUTES_EXCLUDE = "indexedAttributesExclude";
    
    private static final long STARTUP_DELAY = 180000L;
    private static final Map<String,Thread> runningSegments = new ConcurrentHashMap<String,Thread>();
    
    private PersistenceManagerFactory pmf = null;
    private long startedAt = System.currentTimeMillis();

}
