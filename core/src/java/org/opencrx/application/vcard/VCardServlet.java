/*
 * ====================================================================
 * Project:     openCRX/Groupware, http://www.opencrx.org/
 * Description: VCardServlet
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2008, CRIXP Corp., Switzerland
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
package org.opencrx.application.vcard;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringReader;
import java.util.List;

import javax.jdo.PersistenceManager;
import javax.jdo.PersistenceManagerFactory;
import javax.naming.NamingException;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.opencrx.application.carddav.AccountResource;
import org.opencrx.application.uses.ezvcard.Ezvcard;
import org.opencrx.application.uses.ezvcard.VCardVersion;
import org.opencrx.kernel.account1.cci2.AccountQuery;
import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.admin1.jmi1.ComponentConfiguration;
import org.opencrx.kernel.backend.Base;
import org.opencrx.kernel.backend.VCard;
import org.opencrx.kernel.generic.SecurityKeys;
import org.opencrx.kernel.utils.AccountQueryHelper;
import org.opencrx.kernel.utils.ComponentConfigHelper;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.kernel.exception.BasicException;
import org.openmdx.kernel.log.SysLog;

public class VCardServlet extends HttpServlet {

    //-----------------------------------------------------------------------
    @Override
    public void init(
        ServletConfig config            
    ) throws ServletException {
        super.init();        
        if(this.persistenceManagerFactory == null) {                    
            try {
                Utils.getModel();
                this.persistenceManagerFactory = Utils.getPersistenceManagerFactory();
            }
            catch (NamingException e) {
                throw new ServletException( 
                    "Can not get the initial context", 
                    e
                );                
            }
            catch(ServiceException e) {
                throw new ServletException( 
                    "Can not get persistence manager", 
                    e
                );                
            }        
        }            
    }
    
    //-----------------------------------------------------------------------
    protected PersistenceManager getPersistenceManager(
        HttpServletRequest req
    ) {
        return req.getUserPrincipal() == null ?
            null :
            this.persistenceManagerFactory.getPersistenceManager(
                req.getUserPrincipal().getName(),
                null
            );
    }

    //-----------------------------------------------------------------------
    protected PersistenceManager getRootPersistenceManager(
    ) {
        return this.persistenceManagerFactory.getPersistenceManager(
            SecurityKeys.ROOT_PRINCIPAL,
            null
        );    	
    }
    
    //-----------------------------------------------------------------------
    protected org.opencrx.kernel.admin1.jmi1.ComponentConfiguration getComponentConfiguration(
        String providerName,
        PersistenceManager rootPm
    ) {
        return ComponentConfigHelper.getComponentConfiguration(
            CONFIGURATION_ID,
            providerName,
            rootPm,
            false,
            null
        );
    }
    
    //-----------------------------------------------------------------------
    protected AccountQueryHelper getAccountsHelper(
        PersistenceManager pm,
        String requestedAccountFilter
    ) {
        AccountQueryHelper accountsHelper = new AccountQueryHelper(pm);
        if(requestedAccountFilter != null) {
            try {
                accountsHelper.parseQueryId(                        
                    (requestedAccountFilter.startsWith("/") ? "" : "/") + requestedAccountFilter
                );
            }
            catch(Exception  e) {}
        }        
        return accountsHelper;
    }
    
    //-----------------------------------------------------------------------
    protected Account findAccount(
        PersistenceManager pm,
        AccountQueryHelper accountsHelper,
        String uid
    ) {
        AccountQuery query = (AccountQuery)pm.newQuery(Account.class);
        query.thereExistsExternalLink().equalTo(VCard.VCARD_SCHEMA + uid);
        List<Account> accounts = accountsHelper.getAccountSegment().getAccount(query);
        if(accounts.isEmpty()) {
            query = (AccountQuery)pm.newQuery(Account.class);
            query.thereExistsExternalLink().equalTo(VCard.VCARD_SCHEMA + uid.replace('.', '+'));
            accounts = accountsHelper.getAccountSegment().getAccount(query);
            if(accounts.isEmpty()) {
                return null;
            } else {
                return accounts.iterator().next();
            }
        } else {
            return accounts.iterator().next();
        }
    }

    //-----------------------------------------------------------------------
    @Override
    protected void doGet(
        HttpServletRequest req, 
        HttpServletResponse resp
    ) throws ServletException, IOException {
        PersistenceManager pm = this.getPersistenceManager(req);
        PersistenceManager rootPm = this.getRootPersistenceManager();
        if(pm == null) {
            resp.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
            return;
        }        
        String requestedAccountFilter = req.getParameter("id");
        AccountQueryHelper accountsHelper = this.getAccountsHelper(pm, requestedAccountFilter);
        ComponentConfiguration componentConfiguration = 
            this.getComponentConfiguration(
                 accountsHelper.getAccountSegment().refGetPath().get(2),
                 rootPm
            );
        String maxAccountsValue = componentConfiguration == null ? 
            null : 
            ComponentConfigHelper.getComponentConfigProperty("maxAccounts", componentConfiguration).getStringValue();
        int maxAccounts = Integer.valueOf(
            maxAccountsValue == null ? 
                "500" : 
                maxAccountsValue
        ).intValue();
        // Return all accounts in VCF format
        if(req.getRequestURI().endsWith("/vcard") || req.getRequestURI().endsWith("/accounts")) {
        	if(
	            RESOURCE_NAME_ACCOUNTS_VCF.equals(req.getParameter(PARAMETER_NAME_RESOURCE)) ||
	            RESOURCE_TYPE_VCF.equals(req.getParameter(PARAMETER_NAME_TYPE))
	        ) {
	            try {
	                resp.setCharacterEncoding("UTF-8");
	                resp.setStatus(HttpServletResponse.SC_OK);
	                AccountQuery accountQuery = (AccountQuery)pm.newQuery(Account.class);
	                accountQuery.forAllDisabled().isFalse();
	                accountQuery.vcard().isNonNull();
	                PrintWriter p = resp.getWriter();
	                int n = 0;
	                for(Account account: accountsHelper.getFilteredAccounts(accountQuery)) {
	                    String vcard = account.getVcard();
	                    if((vcard != null) && (vcard.indexOf("BEGIN:VCARD") >= 0)) {
	                    	org.opencrx.application.uses.ezvcard.VCard vCard = Ezvcard.parse(vcard).first();
	                    	if(vCard.getUrls().isEmpty()) {
	                        	String url = null;
	                        	try {
	                        		url = Base.getInstance().getAccessUrl(req, "-carddav-", account);
	                        		vCard.addUrl(url);
	                        	} catch(Exception e) {}
	                    	}
	                    	if(vCard.getPhotos().isEmpty()) {
	                    		org.opencrx.application.uses.ezvcard.property.Photo photo = AccountResource.getPhoto(account);
	                    		if(photo != null) {
	                    			vCard.addPhoto(photo);
	                    		}
	                        }
	                    	try {
	                    		Ezvcard.write(vCard).version(VCardVersion.V3_0).go(p);
	                    	} catch(Exception ignore) {}
	                    }
	                    n++;
	                    if(n % 50 == 0) pm.evictAll();                
	                    if(n > maxAccounts) break;
	                }
	                p.flush();
	            }
	            catch(Exception e) {
	                new ServiceException(e).log();
	                resp.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
	            }
        	}
        	else {
                super.doGet(req, resp);        		
        	}
        }
        else {
            super.doGet(req, resp);
        }
        try {
        	if(pm != null) {
        		pm.close();
        	}
        	if(rootPm != null) {
        		rootPm.close();
        	}
        } catch(Exception e) {}
    }
    
    //-----------------------------------------------------------------------
    @Override
    protected void doPut(
        HttpServletRequest req, 
        HttpServletResponse resp
    ) throws ServletException, IOException {
        req.setCharacterEncoding("UTF-8");
        PersistenceManager pm = this.getPersistenceManager(req);
        if(pm == null) {
            resp.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
            return;
        }        
        String filterId = req.getParameter("id");
        AccountQueryHelper accountsHelper = this.getAccountsHelper(pm, filterId);
        if(req.getRequestURI().endsWith("/vcard") || req.getRequestURI().endsWith("/accounts")) {
	        if(
	            RESOURCE_NAME_ACCOUNTS_VCF.equals(req.getParameter(PARAMETER_NAME_RESOURCE)) ||
	            RESOURCE_TYPE_VCF.equals(req.getParameter(PARAMETER_NAME_TYPE))
	        ) {
	        	org.opencrx.kernel.account1.jmi1.Segment accountSegment = accountsHelper.getAccountSegment();
	            resp.setStatus(HttpServletResponse.SC_OK);
	            resp.setCharacterEncoding("UTF-8");
	            BufferedReader reader = new BufferedReader(req.getReader());
	            String l = null;
	            while((l = reader.readLine()) != null) {
	                if(l.toUpperCase().startsWith("BEGIN:VCARD")) {
	                    StringBuilder vcard = new StringBuilder();
	                    vcard.append("BEGIN:VCARD\n");
	                    String uid = null;
	                    String rev = null;
	                    while((l = reader.readLine()) != null) {
	                        vcard.append(l).append("\n");
	                        if(l.startsWith("UID:")) {
	                            uid = l.substring(4);
	                        }
	                        else if(l.startsWith("REV:")) {
	                            rev = l.substring(4);
	                        }
	                        else if(l.startsWith("END:VCARD")) {
	                            break;
	                        }
	                    }
	                    SysLog.trace("VCARD", vcard);
	                    if((uid != null) && (rev != null)) {
	                    	SysLog.detail("Lookup account", uid);
	                        Account account = this.findAccount(
	                            pm,
	                            accountsHelper, 
	                            uid
	                        );
	                        if(account != null) {	  
	                        	try {
		                        	VCard.getInstance().putVCard(
		                        		new BufferedReader(new StringReader(vcard.toString())), 
		                        		accountSegment
		                        	);
	                        	}
	                            catch(Exception e) {
	                            	ServiceException e0 = new ServiceException(
	                            		e,
	                            		BasicException.Code.DEFAULT_DOMAIN,
	                            		BasicException.Code.PROCESSING_FAILURE,
	                            		"Unable to update vcard",
	                            		new BasicException.Parameter("VCARD", vcard)
	                            	);
	                            	e0.log();
	                                try {
	                                    pm.currentTransaction().rollback();
	                                } catch(Exception e1) {}                                    
	                            }
	                        }
	                        else {
	                        	SysLog.detail(
	                                "Skipping ", 
	                                new String[]{
	                                    "UID: " + uid, 
	                                    "REV: " + rev, 
	                                    "Account.number: " + (account == null ? null : account.refMofId()),
	                                    "Account.modifiedAt:" + (account == null ? null : account.getModifiedAt())
	                                }
	                            );
	                        }
	                    }
	                    else {
	                    	SysLog.detail("Skipping", vcard); 
	                    }
	                }                    
	            }
	        }
	        else {
	            super.doPut(req, resp);
	        }	        
        }            
        else {
            super.doPut(req, resp);
        }
        try {
            pm.close();            
        } catch(Exception e) {}        
    }

    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    private static final long serialVersionUID = -7557742069034207175L;
    
    protected final static String CONFIGURATION_ID = "VCardServlet";    
    protected final static String RESOURCE_NAME_ACCOUNTS_VCF = "accounts.vcf";
    protected final static String RESOURCE_TYPE_VCF = "vcf";
    protected final static String PARAMETER_NAME_TYPE = "type";
    protected final static String PARAMETER_NAME_RESOURCE = "resource";
    
    protected PersistenceManagerFactory persistenceManagerFactory = null;
        
}
