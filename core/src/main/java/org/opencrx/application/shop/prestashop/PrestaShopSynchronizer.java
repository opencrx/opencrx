/*
 * ====================================================================
 * Project:     openCRX/Core
 * Description: PrestaShopSynchronizer
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2011, CRIXP Corp., Switzerland
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

package org.opencrx.application.shop.prestashop;

import java.io.IOException;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import javax.jdo.PersistenceManager;
import javax.jdo.PersistenceManagerFactory;
import javax.naming.InitialContext;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.opencrx.kernel.admin1.jmi1.ComponentConfiguration;
import org.opencrx.kernel.base.jmi1.StringProperty;
import org.opencrx.kernel.generic.SecurityKeys;
import org.opencrx.kernel.utils.ComponentConfigHelper;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.kernel.log.SysLog;

public class PrestaShopSynchronizer 
    extends HttpServlet {

	//-----------------------------------------------------------------------
    public void init(
        ServletConfig config
    ) throws ServletException {
        super.init(config);
        String providerName = this.getInitParameter("providerName");
        if(providerName.startsWith("provider/")) {
            providerName = providerName.substring(9);
        }        
        try {
            this.pmf = Utils.getPersistenceManagerFactory();
        }
        catch (Exception e) {
            throw new ServletException("Can not get persistence manager factory", e);
        }
    }

    //-----------------------------------------------------------------------    
    protected void synchronize(
        String id,
        String providerName,
        String segmentName,
        HttpServletRequest req, 
        HttpServletResponse res        
    ) throws IOException {        
        System.out.println(new Date().toString() + ": " + WORKFLOW_NAME + " " + providerName + "/" + segmentName);
        try {
            PersistenceManager rootPm = this.pmf.getPersistenceManager(
                SecurityKeys.ROOT_PRINCIPAL,
                null
            );
            ComponentConfiguration configuration = 
            	ComponentConfigHelper.getComponentConfiguration(
	            	COMPONENT_CONFIGURATION_ID, 
	            	providerName, 
	            	rootPm, 
	            	true, // autoCreate 
	            	null // initialProperties
	            );
            List<String> shopNames = new ArrayList<String>();
            List<String> jdbcResourceNames = new ArrayList<String>();
            List<String> tablePrefixes = new ArrayList<String>();
            List<String> syncKeys = new ArrayList<String>();
            List<String> runAss = new ArrayList<String>();
            List<Boolean> emailUniques = new ArrayList<Boolean>();
            for(int i = 0; i < MAX_STORES_PER_SEGMENT; i++) {
	            // OPTION_SHOP_NAME
	            String shopNameId = providerName + "." + segmentName + "." + OPTION_SHOP_NAME + "[" + i + "]";
	            StringProperty shopNameProperty = ComponentConfigHelper.getComponentConfigProperty(
	            	shopNameId, 
	            	configuration
	            );
	            // OPTION_JDBC_RESOURCE_NAME
	            String jdbcResourceNameId = providerName + "." + segmentName + "." + OPTION_JDBC_RESOURCE_NAME + "[" + i + "]";
	            StringProperty jdbcResourceNameProperty = ComponentConfigHelper.getComponentConfigProperty(
	            	jdbcResourceNameId, 
	            	configuration
	            );
	            // OPTION_TABLE_PREFIX
	            String tablePrefixId = providerName + "." + segmentName + "." + OPTION_TABLE_PREFIX + "[" + i + "]";
	            StringProperty tablePrefixProperty = ComponentConfigHelper.getComponentConfigProperty(
	            	tablePrefixId, 
	            	configuration
	            );
	            // OPTION_SYNC_KEY
	            String syncKeyId = providerName + "." + segmentName + "." + OPTION_SYNC_KEY + "[" + i + "]";
	            StringProperty syncKeyProperty = ComponentConfigHelper.getComponentConfigProperty(
	            	syncKeyId, 
	            	configuration
	            );
	            // OPTION_RUN_AS
	            String runAsId = providerName + "." + segmentName + "." + OPTION_RUN_AS + "[" + i + "]";
	            StringProperty runAsProperty = ComponentConfigHelper.getComponentConfigProperty(
	            	runAsId, 
	            	configuration
	            );
	            // OPTION_EMAIL_UNIQUE
	            String emailUniqueId = providerName + "." + segmentName + "." + OPTION_EMAIL_UNIQUE + "[" + i + "]";
	            StringProperty emailUniqueProperty = ComponentConfigHelper.getComponentConfigProperty(
	            	emailUniqueId, 
	            	configuration
	            );
	            String shopName = shopNameProperty == null ? null : shopNameProperty.getStringValue();
	            if(shopName == null || shopName.isEmpty()) {
	            	break;
	            }
	            String jdbcResourceName = jdbcResourceNameProperty == null ? null : jdbcResourceNameProperty.getStringValue();
	            String tablePrefix = tablePrefixProperty == null ? null : tablePrefixProperty.getStringValue();
	            String syncKey = syncKeyProperty == null ? null : syncKeyProperty.getStringValue();
	            String runAs = runAsProperty == null ? null : runAsProperty.getStringValue();
	            Boolean emailUnique = emailUniqueProperty == null ? null : Boolean.valueOf(emailUniqueProperty.getStringValue());
            	shopNames.add(
            		shopName == null ? "" : shopName
            	);
            	jdbcResourceNames.add(
            		jdbcResourceName == null ? "" : jdbcResourceName
            	);
            	tablePrefixes.add(
            		tablePrefix == null ? "" : tablePrefix
            	);
            	syncKeys.add(
            		syncKey == null ? "0" : syncKey
            	);
            	runAss.add(
            		runAs == null ? SecurityKeys.ADMIN_PRINCIPAL + SecurityKeys.ID_SEPARATOR + segmentName : runAs
            	);
            	emailUniques.add(
            		emailUnique == null ? Boolean.TRUE : emailUnique
            	);
            }
            SysLog.info("Syncing shops", Arrays.asList(providerName, segmentName, shopNames, jdbcResourceNames, syncKeys));
            for(int i = 0; i < shopNames.size(); i++) {
            	String shopName = shopNames.get(i);
            	String jdbcResourceName = jdbcResourceNames.get(i);
            	String tablePrefix = tablePrefixes.get(i);
            	long syncKey = Long.valueOf(syncKeys.get(i));
            	String runAs = runAss.get(i);
            	boolean emailUnique = emailUniques.get(i);
            	if(!shopName.isEmpty()) {
	                if(jdbcResourceName.isEmpty()) {
	                    System.out.println(new Date() + ": " + WORKFLOW_NAME + " " + providerName + "/" + segmentName + ": Option " + OPTION_JDBC_RESOURCE_NAME + " is not set for store " + shopName + ". Ignoring");
	                }
	                else {
	                    InitialContext initialContext = new InitialContext();
	                    javax.sql.DataSource ds = null;
	                    try {
	                    	ds = (javax.sql.DataSource)initialContext.lookup("java:comp/env/" + jdbcResourceName);
	                    } catch(Exception e) {
		                    System.out.println(new Date() + ": " + WORKFLOW_NAME + " " + providerName + "/" + segmentName + ": Unable to get resource for " + jdbcResourceName + ". Reason: " + e.getMessage());
	                    }
	                    if(ds != null) {
	                    	long newSyncKey = System.currentTimeMillis();
		                    System.out.println(new Date() + ": " + WORKFLOW_NAME + " " + providerName + "/" + segmentName + ": Synchronizing shop @" + shopName);		                  		                    
	                    	java.sql.Connection conn = ds.getConnection();
	                        PersistenceManager pm = this.pmf.getPersistenceManager(
	                        	runAs,
	                            null
	                        );
	                        PrestaShopMapper prestaShopMapper =
	                        	new PrestaShopMapper(
	                        		pm,
	                        		providerName,
	                        		segmentName,
	                        		shopName,
	                        		emailUnique,
	                        		conn,
	                        		tablePrefix
	                        	);
	                        // Customer
	                        PreparedStatement ps = conn.prepareStatement(
	                        	"SELECT * FROM " + tablePrefix + "customer WHERE date_upd >= ? AND date_upd < ? ORDER BY date_upd"
	                        );
	                        ps.setTimestamp(1, new java.sql.Timestamp(syncKey));
	                        ps.setTimestamp(2, new java.sql.Timestamp(newSyncKey));
	                        ResultSet rs = ps.executeQuery();
	                        while(rs.next()) {
	                        	System.out.println(new Date().toString() + ": " + WORKFLOW_NAME + " " + providerName + "/" + segmentName + ": Customer " + rs.getInt("id_customer") + " @" + shopName);
	                        	prestaShopMapper.mapCustomer(rs);
	                        }
	                        rs.close();
	                        ps.close();

	                        // Customer addresses
	                        ps = conn.prepareStatement(
	                        	"SELECT * FROM " + tablePrefix + "customer c WHERE id_customer IN (SELECT id_customer FROM " + tablePrefix + "address a WHERE a.id_customer = c.id_customer AND a.date_upd >= ? AND a.date_upd < ?) ORDER BY c.date_upd"
	                        );
	                        ps.setTimestamp(1, new java.sql.Timestamp(syncKey));
	                        ps.setTimestamp(2, new java.sql.Timestamp(newSyncKey));
	                        rs = ps.executeQuery();
	                        while(rs.next()) {
	                        	System.out.println(new Date().toString() + ": " + WORKFLOW_NAME + " " + providerName + "/" + segmentName + ": Customer addresses " + rs.getInt("id_customer") + " @" + shopName);
	                        	prestaShopMapper.mapCustomer(rs);
	                        }
	                        rs.close();
	                        ps.close();

	                        // Product
	                        ps = conn.prepareStatement(
	                        	"SELECT * FROM " + tablePrefix + "product WHERE date_upd >= ? AND date_upd < ? ORDER BY date_upd"
	                        );
	                        ps.setTimestamp(1, new java.sql.Timestamp(syncKey));
	                        ps.setTimestamp(2, new java.sql.Timestamp(newSyncKey));
	                        rs = ps.executeQuery();
	                        while(rs.next()) {
	                        	System.out.println(new Date().toString() + ": " + WORKFLOW_NAME + " " + providerName + "/" + segmentName + ": Product " + rs.getInt("id_product") + " @" + shopName);
	                        	prestaShopMapper.mapProduct(rs);
	                        }
	                        rs.close();
	                        ps.close();	                        
	                        
	                        // Orders
	                        ps = conn.prepareStatement(
	                        	"SELECT * FROM " + tablePrefix + "orders WHERE date_upd >= ? AND date_upd < ? ORDER BY date_upd"
	                        );
	                        ps.setTimestamp(1, new java.sql.Timestamp(syncKey));
	                        ps.setTimestamp(2, new java.sql.Timestamp(newSyncKey));
	                        rs = ps.executeQuery();
	                        while(rs.next()) {
	                        	System.out.println(new Date().toString() + ": " + WORKFLOW_NAME + " " + providerName + "/" + segmentName  + ": Order " + rs.getInt("id_order") + " @" + shopName);
	                        	prestaShopMapper.mapOrder(rs);
	                        }
	                        rs.close();
	                        ps.close();
	                        
	                        // Order history
	                        ps = conn.prepareStatement(
	                        	"SELECT * FROM " + tablePrefix + "order_history WHERE date_add >= ? AND date_add < ? ORDER BY date_add"
	                        );
	                        ps.setTimestamp(1, new java.sql.Timestamp(syncKey));
	                        ps.setTimestamp(2, new java.sql.Timestamp(newSyncKey));
	                        rs = ps.executeQuery();
	                        while(rs.next()) {
	                        	System.out.println(new Date().toString() + ": " + WORKFLOW_NAME + " " + providerName + "/" + segmentName + ": Order History " + rs.getInt("id_order_history") + " @" + shopName);
	                        	prestaShopMapper.mapOrderHistory(rs);
	                        }
	                        rs.close();
	                        ps.close();
	                        
	                        // Done	                        
	                        pm.close();
	                    	conn.close();
	                    	
	                    	// Update syncKey
	        	            String syncKeyId = providerName + "." + segmentName + "." + OPTION_SYNC_KEY + "[" + i + "]";
	        	            StringProperty syncKeyProperty = ComponentConfigHelper.getComponentConfigProperty(
	        	            	syncKeyId, 
	        	            	configuration
	        	            );
	        	            if(syncKeyProperty == null) {
	        	            	syncKeyProperty = ComponentConfigHelper.addComponentConfigProperty(
	        	            		syncKeyId, 
	        	            		"", // stringValue 
	        	            		configuration
	        	            	);
	        	            }
	        	            rootPm.currentTransaction().begin();
	        	            syncKeyProperty.setStringValue(Long.toString(newSyncKey));
	        	            rootPm.currentTransaction().commit();
	                    }
	                }
            	}
            }
            rootPm.close();
        }
        catch(Exception e) {
            new ServiceException(e).log();
            System.out.println(new Date() + ": " + WORKFLOW_NAME + " " + providerName + "/" + segmentName + ": exception occured " + e.getMessage() + ". Continuing");
        }        
    }
    
    //-----------------------------------------------------------------------
    protected void handleRequest(
        HttpServletRequest req, 
        HttpServletResponse res
    ) throws ServletException, IOException {
        if(System.currentTimeMillis() > this.startedAt + 60000L) {
            String segmentName = req.getParameter("segment");
            String providerName = req.getParameter("provider");
            String id = providerName + "/" + segmentName;
            if(
                COMMAND_EXECUTE.equals(req.getPathInfo()) &&
                !this.runningSegments.contains(id)
            ) {
                try {
                    this.runningSegments.add(id);
                    this.synchronize(
                        id,
                        providerName,
                        segmentName,
                        req,
                        res
                    );
                } catch(Exception e) {
                    new ServiceException(e).log();
                }
                finally {
                    this.runningSegments.remove(id);
                }
            }
        }
    }

    //-----------------------------------------------------------------------
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
        
    //-----------------------------------------------------------------------
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
    private static final long serialVersionUID = -2134096761120818592L;
    
    protected static final String COMMAND_EXECUTE = "/execute";
    protected static final String WORKFLOW_NAME = "PrestaShopSynchronizer";
    protected static final String COMPONENT_CONFIGURATION_ID = "PrestaShopSynchronizer";

    protected static final int MAX_STORES_PER_SEGMENT = 10;
    protected static final String OPTION_SHOP_NAME = "shopName";
    protected static final String OPTION_JDBC_RESOURCE_NAME = "jdbcResourceName";
    protected static final String OPTION_TABLE_PREFIX = "tablePrefix";
    protected static final String OPTION_SYNC_KEY = "syncKey";
    protected static final String OPTION_RUN_AS = "runAs";
    protected static final String OPTION_EMAIL_UNIQUE = "emailUnique";
    
    protected PersistenceManagerFactory pmf = null;
    protected final List<String> runningSegments = new ArrayList<String>();
    protected long startedAt = System.currentTimeMillis();

}

//--- End of File -----------------------------------------------------------
