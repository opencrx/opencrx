/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: BpiAdapterServlet
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2012-2014, CRIXP Corp., Switzerland
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
package org.opencrx.application.bpi.adapter;

import java.io.IOException;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.jdo.PersistenceManager;
import javax.jdo.PersistenceManagerFactory;
import javax.naming.NamingException;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;

/**
 * BpiAdapterServlet. Extensible Business Process Integration Servlet.
 * <p>
 * In addition to the openCRX REST servlet which exposes the full API
 * of openCRX, the bpi adapter offers a lightweight, JSON based interface 
 * which simplifies the integration of 3rd party applications.
 * The servlet allows to
 * <ul>
 *   <li>add new actions
 *   <li>override existing actions
 * </ul>
 *
 */
public class BpiAdapterServlet extends HttpServlet {

	/**
	 * Add action to registry.
	 * 
	 * @param pathPattern
	 * @param action
	 */
	protected void registerAction(
		Path pathPattern,
		BpiAction action
	) {
		this.actionRegistry.put(pathPattern, action);
	}

	/* (non-Javadoc)
     * @see javax.servlet.GenericServlet#init(javax.servlet.ServletConfig)
     */
    @Override
    public void init(
        ServletConfig config            
    ) throws ServletException {
        super.init();        
        if(this.pmf == null) {                   
            try {
                Utils.getModel();
                this.pmf = Utils.getPersistenceManagerFactory();
            } catch (NamingException e) {
                throw new ServletException( 
                    "Can not get the initial context", 
                    e
                );
            } catch(ServiceException e) {
                throw new ServletException( 
                    "Can not get persistence manager", 
                    e
                );                
            }   
        }
        this.registerAction(
        	new Path("xri://@openmdx*org.opencrx.application.bpi1").getDescendant("provider", ":*", "segment", ":*", "codeTable", ":*"),
        	new GetCodeTableAction()
        );
        this.registerAction(
        	new Path("xri://@openmdx*org.opencrx.application.bpi1").getDescendant("provider", ":*", "segment", ":*", "contact", ":*"),
        	new GetContactAction()
        );
        this.registerAction(
        	new Path("xri://@openmdx*org.opencrx.application.bpi1").getDescendant("provider", ":*", "segment", ":*", "contact"),
        	new GetContactsAction()
        );
        this.registerAction(
        	new Path("xri://@openmdx*org.opencrx.application.bpi1").getDescendant("provider", ":*", "segment", ":*", "contact", ":*", "membership"),
        	new GetContactMembershipsAction()
        );
        this.registerAction(
        	new Path("xri://@openmdx*org.opencrx.application.bpi1").getDescendant("provider", ":*", "segment", ":*", "organization"),
        	new GetOrganizationsAction()
        );
        this.registerAction(
        	new Path("xri://@openmdx*org.opencrx.application.bpi1").getDescendant("provider", ":*", "segment", ":*", "organization", ":*"),
        	new GetOrganizationAction()
        );
        this.registerAction(
        	new Path("xri://@openmdx*org.opencrx.application.bpi1").getDescendant("provider", ":*", "segment", ":*", "organization", ":*", "member"),
        	new GetOrganizationMembersAction()
        );
        this.registerAction(
        	new Path("xri://@openmdx*org.opencrx.application.bpi1").getDescendant("provider", ":*", "segment", ":*", "activityCreator", ":*"),
        	new GetActivityCreatorAction()
        );
        this.registerAction(
        	new Path("xri://@openmdx*org.opencrx.application.bpi1").getDescendant("provider", ":*", "segment", ":*", "activityCreator", ":*", "createActivity"),
        	new CreateActivityAction()
        );
        this.registerAction(
        	new Path("xri://@openmdx*org.opencrx.application.bpi1").getDescendant("provider", ":*", "segment", ":*", "activity", ":*"),
        	new GetActivityAction()
        );
        this.registerAction(
        	new Path("xri://@openmdx*org.opencrx.application.bpi1").getDescendant("provider", ":*", "segment", ":*", "activity", ":*", "doFollowUp"),
        	new DoFollowUpAction()
        );
        this.registerAction(
        	new Path("xri://@openmdx*org.opencrx.application.bpi1").getDescendant("provider", ":*", "segment", ":*", "contact", ":*", "assignedActivity"),
        	new GetAssignedActivitiesAction()
        );
        this.registerAction(
        	new Path("xri://@openmdx*org.opencrx.application.bpi1").getDescendant("provider", ":*", "segment", ":*", "addressGroup"),
        	new GetAddressGroupsAction()
        );
        this.registerAction(
        	new Path("xri://@openmdx*org.opencrx.application.bpi1").getDescendant("provider", ":*", "segment", ":*", "addressGroup", ":*"),
        	new GetAddressGroupAction()
        );
        this.registerAction(
        	new Path("xri://@openmdx*org.opencrx.application.bpi1").getDescendant("provider", ":*", "segment", ":*", "addressGroup", ":*", "member"),
        	new GetAddressGroupMembersAction()
        );
        this.registerAction(
        	new Path("xri://@openmdx*org.opencrx.application.bpi1").getDescendant("provider", ":*", "segment", ":*", "accountFilter", ":*"),
        	new GetAccountFilterAction()
        );
        this.registerAction(
        	new Path("xri://@openmdx*org.opencrx.application.bpi1").getDescendant("provider", ":*", "segment", ":*", "accountFilter", ":*", "contact"),
        	new GetContactsAction()
        );
        this.registerAction(
        	new Path("xri://@openmdx*org.opencrx.application.bpi1").getDescendant("provider", ":*", "segment", ":*", "accountFilter", ":*", "organization"),
        	new GetOrganizationsAction()
        );
        this.plugIn = this.newPlugIn();
    }

    /**
     * Get persistence manager.
     * 
     * @param req
     * @return
     */
    protected PersistenceManager getPersistenceManager(
        HttpServletRequest req
    ) {
        return req.getUserPrincipal() == null ? null :
            this.pmf.getPersistenceManager(
                req.getUserPrincipal().getName(),
                null
            );
    }

    /**
     * Get extension.
     * 
     * @return
     */
    protected BpiPlugIn newPlugIn(
    ) {
    	return new BpiPlugIn();
    }

    /**
     * Get path.
     * 
     * @param req
     * @return
     */
    protected Path getPath(
   		HttpServletRequest req 
    ) throws ServiceException {
    	String requestURI = req.getRequestURI();
		return new Path("xri://@openmdx*" + requestURI.substring(req.getContextPath().length() + 1));
    }

    /* (non-Javadoc)
	 * @see javax.servlet.http.HttpServlet#doGet(javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse)
	 */
	@Override
	protected void doGet(
		HttpServletRequest req, 
		HttpServletResponse resp
	) throws ServletException, IOException {
		PersistenceManager pm = null;
		long startedAt = System.currentTimeMillis();
		try {
			if(this.logRequests) {
				System.out.println(new Date() + "  " + this.getClass().getSimpleName() + " Processing " + req.getRequestURL() + "?" + req.getQueryString());
			}
			Path path = this.getPath(req);
			pm = this.getPersistenceManager(req);
			boolean done = false;
			for(Map.Entry<Path,BpiAction> action: this.actionRegistry.entrySet()) {
				if(path.isLike(action.getKey())) {
					action.getValue().perform(path, pm, this.plugIn, req, resp);
					done = true;
					break;
				}
			}
			if(!done) {
				super.doGet(req, resp);				
			}
		} catch(Exception e) {
			new ServiceException(e).log();
			resp.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
		} finally {
			if(pm != null) {
				try {
					pm.close();
				} catch(Exception e) {}
			}
		}
		if(logRequests) {
			System.out.println(new Date() + "  " + this.getClass().getSimpleName() + " Execution time " + (System.currentTimeMillis() - startedAt) + " ms");
		}
	}

    /* (non-Javadoc)
     * @see javax.servlet.http.HttpServlet#doPost(javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse)
     */
    @Override
	protected void doPost(
		HttpServletRequest req, 
		HttpServletResponse resp
	) throws ServletException, IOException {
		PersistenceManager pm = null;
		try {
			Path path = this.getPath(req);
			pm = this.getPersistenceManager(req);
			boolean done = false;
			for(Map.Entry<Path,BpiAction> action: this.actionRegistry.entrySet()) {
				if(path.isLike(action.getKey())) {
					action.getValue().perform(path, pm, this.plugIn, req, resp);
					done = true;
					break;
				}
			}
			if(!done) {
				super.doPost(req, resp);				
			}
		} catch(Exception e) {
			try {
				pm.currentTransaction().rollback();
			} catch(Exception ignore) {}
			new ServiceException(e).log();
			resp.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
		} finally {
			if(pm != null) {
				try {
					pm.close();
				} catch(Exception e) {}
			}
		}
	}

	//-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------    
	private static final long serialVersionUID = -98101472441835933L;

	protected final Map<Path,BpiAction> actionRegistry = new HashMap<Path,BpiAction>();

	protected boolean logRequests = false;
	protected BpiPlugIn plugIn = null;
	protected PersistenceManagerFactory pmf = null;

}
