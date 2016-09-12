/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: LDAPSynchronizer
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
package org.opencrx.application.ldap;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
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

import org.opencrx.kernel.account1.cci2.AccountFilterGlobalQuery;
import org.opencrx.kernel.account1.cci2.ContactQuery;
import org.opencrx.kernel.account1.jmi1.AccountAddress;
import org.opencrx.kernel.account1.jmi1.AccountFilterGlobal;
import org.opencrx.kernel.account1.jmi1.Contact;
import org.opencrx.kernel.account1.jmi1.EMailAddress;
import org.opencrx.kernel.admin1.jmi1.ComponentConfiguration;
import org.opencrx.kernel.backend.Accounts;
import org.opencrx.kernel.base.jmi1.StringProperty;
import org.opencrx.kernel.generic.SecurityKeys;
import org.opencrx.kernel.utils.ComponentConfigHelper;
import org.opencrx.kernel.utils.Utils;
import org.opencrx.security.realm1.jmi1.PrincipalGroup;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.kernel.log.SysLog;

/**
 * LDAPSynchronizer
 *
 */
public class LDAPSynchronizer extends HttpServlet {

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
     * Get account filter.
     * 
     * @param accountSegment
     * @return
     * @throws ServiceException
     */
    protected AccountFilterGlobal getAccountFilter(
    	org.opencrx.kernel.account1.jmi1.Segment accountSegment
    ) throws ServiceException {
    	PersistenceManager pm = JDOHelper.getPersistenceManager(accountSegment);
    	AccountFilterGlobalQuery query = (AccountFilterGlobalQuery)pm.newQuery(AccountFilterGlobal.class);
    	query.name().equalTo(LDAPSynchronizer.class.getSimpleName());
    	List<AccountFilterGlobal> accountFilters = accountSegment.getAccountFilter(query);
    	return accountFilters.isEmpty() ? null : accountFilters.iterator().next();
    }

    /**
     * Get uid for given contact.
     * 
     * @param contact
     * @return
     * @throws ServiceException
     */
    protected String getUid(
    	Contact contact
    ) throws ServiceException {
    	return contact.getAliasName();
    }

    /**
     * Get DN for given contact.
     * 
     * @param contact
     * @return
     * @throws ServiceException
     */
    protected String getDN(
    	Contact contact,
    	String baseDNPeople
    ) throws ServiceException {
    	String uid = this.getUid(contact);
    	return uid == null || uid.isEmpty()
    		? null
    		: "uid=" + uid + "," + baseDNPeople;
    }

    /**
     * Get userpassword for given contact.
     * 
     * @param contact
     * @return
     * @throws ServiceException
     */
    protected String getUserpassword(
    	Contact contact
    ) throws ServiceException {
    	String uid = this.getUid(contact);
    	return "changeit-" + uid;
    }

    /**
     * Get employeeNumber for given contact.
     * 
     * @param contact
     * @return
     * @throws ServiceException
     */
    protected String getEmployeeNumber(
    	Contact contact
    ) throws ServiceException {
    	return null;
    }

    /**
     * Write LDIF Add entry for given contact.
     * 
     * @param contact
     * @param pw
     * @return
     * @throws ServiceException
     */
    protected void writeLdifAddEntry(
    	Contact contact,
    	String baseDNPeople,
    	PrintWriter pw
    ) throws ServiceException {
    	pw.println("dn: " + this.getDN(contact, baseDNPeople));
    	pw.println("objectclass: inetOrgPerson");
    	pw.println("cn: " + contact.getFullName());
    	pw.println("sn: " + contact.getLastName());
    	pw.println("uid: " + this.getUid(contact));
    	pw.println("userpassword: " + this.getUserpassword(contact));
    	pw.println();
    }

    /**
     * Write LDIF Modify entry for given contact.
     * 
     * @param contact
     * @param pw
     * @return
     * @throws ServiceException
     */
    protected void writeLdifModifyEntry(
    	Contact contact,
    	String baseDNPeople,
    	PrintWriter pw
    ) throws ServiceException {
    	AccountAddress[] mainAddresses = Accounts.getInstance().getMainAddresses(contact);
    	pw.println("dn: " + this.getDN(contact, baseDNPeople));
    	pw.println("changetype: modify");
    	pw.println("replace: cn");
    	pw.println("cn: " + contact.getFullName());
    	pw.println("-");
    	pw.println("replace: sn");
    	pw.println("sn: " + contact.getLastName());
    	pw.println("-");
    	pw.println("replace: gn");
    	pw.println("gn: " + contact.getFirstName());
    	pw.println("-");
    	if(mainAddresses[Accounts.MAIL_BUSINESS] instanceof EMailAddress) {
        	pw.println("replace: mail");
        	pw.println("mail: " + ((EMailAddress)mainAddresses[Accounts.MAIL_BUSINESS]).getEmailAddress());
        	pw.println("-");
    	}
    	String employeeNumber = this.getEmployeeNumber(contact);
    	if(employeeNumber != null && !employeeNumber.isEmpty()) {
        	pw.println("replace: employeeNumber");
        	pw.println("employeeNumber: " + employeeNumber);
        	pw.println("-");	
    	}
    	pw.println();    	
    }

    /**
     * Write LDIF Delete entry for given contact.
     * 
     * @param contact
     * @param pw
     * @return
     * @throws ServiceException
     */
    protected void writeLdifDeleteEntry(
    	Contact contact,
    	String baseDNPeople,
    	PrintWriter pw
    ) throws ServiceException {
    	pw.println("dn: " + this.getDN(contact, baseDNPeople));
    	pw.println("changetype: delete");
    }

    /**
     * Write error entry for given contact.
     * 
     * @param contact
     * @param e
     * @param pw
     * @return
     * @throws ServiceException
     */
    protected void writeError(
    	Contact contact,
    	String baseDNPeople,
    	Exception e,
    	PrintWriter pw
    ) throws ServiceException {
    	String dn = this.getDN(contact, baseDNPeople);
    	pw.println("DN: " + (dn == null ? "N/A" : dn));
    	pw.println("XRI: " + contact.refGetPath().toXRI());
    	if(e != null) {
	    	pw.println("Error:");
	    	ServiceException e0 = new ServiceException(e);
	    	e0.printStackTrace(pw);
    	}
    }

    /**
     * Get DN for given group.
     * 
     * @param groupName
     * @param baseDNGroups
     * @return
     * @throws ServiceException
     */
    protected String getDN(
    	String groupName,
    	String baseDNGroups
    ) throws ServiceException {
    	return "cn=" + groupName + "," + baseDNGroups;
    }

    /**
     * Write LDIF Add entry for given group.
     * 
     * @param groupName
     * @param baseDNGroups
     * @param pw
     * @return
     * @throws ServiceException
     */
    protected void writeLdifAddEntry(
    	String groupName,
    	String baseDNGroups,
    	PrintWriter pw
    ) throws ServiceException {
    	pw.println("dn: " + this.getDN(groupName, baseDNGroups));
    	pw.println("objectclass: groupofnames");
    	pw.println("cn: " + groupName);
    	pw.println("member: ");
    	pw.println();
    }

    /**
     * Write LDIF Modify entry for given group.
     * 
     * @param groupName
     * @param members
     * @param baseDNGroups
     * @param pw
     * @return
     * @throws ServiceException
     */
    protected void writeLdifModifyEntry(
    	String groupName,
    	List<String[]> members,
    	String baseDNGroups,
    	PrintWriter pw
    ) throws ServiceException {
    	pw.println("dn: " + this.getDN(groupName, baseDNGroups));
    	pw.println("changetype: modify");
    	pw.println("replace: member");
    	for(String[] member: members) {
    		pw.println("member: " + member[0]);
    	}
    	pw.println("-");    	
    	pw.println();
    }

    /**
     * Write error entry for given group.
     * 
     * @param groupName
     * @param baseDNGroups
     * @param e
     * @param pw
     * @return
     * @throws ServiceException
     */
    protected void writeError(
    	String groupName,
    	String baseDNGroups,
    	Exception e,
    	PrintWriter pw
    ) throws ServiceException {
    	String dn = this.getDN(groupName, baseDNGroups);
    	pw.println("DN: " + (dn == null ? "N/A" : dn));
    	if(e != null) {
	    	pw.println("Error:");
	    	ServiceException e0 = new ServiceException(e);
	    	e0.printStackTrace(pw);
    	}
    }

    /**
     * Write error entry for given group.
     * 
     * @param groupName
     * @param baseDNGroups
     * @param validMembers
     * @param duplicateMembers
     * @param pw
     * @return
     * @throws ServiceException
     */
    protected void writeError(
    	String groupName,
    	String baseDNGroups,
    	List<String[]> validMembers,
    	List<String[]> duplicateMembers,
    	PrintWriter pw
    ) throws ServiceException {
    	String dn = this.getDN(groupName, baseDNGroups);
    	pw.println("DN: " + (dn == null ? "N/A" : dn));
    	pw.println();
    	pw.println("Valid members:");
    	for(String[] member: validMembers) {
    		pw.println(member[0] + ": " + member[1]);
    	}
    	pw.println();
    	pw.println("Duplicate members:");
    	for(String[] member: duplicateMembers) {
    		pw.println(member[0] + ": " + member[1]);
    	}
    }

    /**
     * Synchronize items.
     * 
     * @param id
     * @param providerName
     * @param segmentName
     * @param req
     * @param res
     * @throws IOException
     */
    protected void sync(
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
            // OPTION_SYNC_KEY
            String syncKeyId = providerName + "." + segmentName + "." + OPTION_SYNC_KEY;
            StringProperty syncKeyProperty = ComponentConfigHelper.getComponentConfigProperty(
            	syncKeyId, 
            	configuration
            );
            long syncKey = syncKeyProperty == null 
            	? 0L 
            	: Long.valueOf(syncKeyProperty.getStringValue());   
            // OPTION_SYNC_DIR
            String syncDirId = providerName + "." + segmentName + "." + OPTION_SYNC_DIR;
            StringProperty syncDirProperty = ComponentConfigHelper.getComponentConfigProperty(
            	syncDirId, 
            	configuration
            );
            File syncDir = syncDirProperty == null 
            	? new File(new File("ldapdir", providerName), segmentName) 
            	: new File(syncDirProperty.getStringValue());
            syncDir.mkdirs();
            // OPTION_RUN_AS
            String runAsId = providerName + "." + segmentName + "." + OPTION_RUN_AS;
            StringProperty runAsProperty = ComponentConfigHelper.getComponentConfigProperty(
            	runAsId,
            	configuration
            );
            String runAs = runAsProperty == null 
            	? SecurityKeys.ADMIN_PRINCIPAL + SecurityKeys.ID_SEPARATOR + segmentName 
            	: runAsProperty.getStringValue();            
            // BASE_DN_PEOPLE
            String baseDNPeopleId = providerName + "." + segmentName + "." + OPTION_BASE_DN_PEOPLE;
            StringProperty baseDNPeopleProperty = ComponentConfigHelper.getComponentConfigProperty(
            	baseDNPeopleId,
            	configuration
            );
            String baseDNPeople = baseDNPeopleProperty == null
            	? "ou=people,dc=example,dc=com"
            	: baseDNPeopleProperty.getStringValue();
            // BASE_DN_GROUPS
            String baseDNGroupsId = providerName + "." + segmentName + "." + OPTION_BASE_DN_GROUPS;
            StringProperty baseDNGroupsProperty = ComponentConfigHelper.getComponentConfigProperty(
            	baseDNGroupsId,
            	configuration
            );
            String baseDNGroups = baseDNGroupsProperty == null
            	? "ou=groups,dc=example,dc=com"
            	: baseDNGroupsProperty.getStringValue();
            // Synchronize
            PersistenceManager pm = this.pmf.getPersistenceManager(
            	runAs,
            	null
            );
    		org.opencrx.kernel.account1.jmi1.Segment accountSegment = Accounts.getInstance().getAccountSegment(pm, providerName, segmentName);
    		AccountFilterGlobal accountFilter = this.getAccountFilter(accountSegment);
    		if(accountFilter != null) {
	            long newSyncKey = System.currentTimeMillis();
	            // Export people	            
	        	int contactCount = 0;
    			{
    				// Include disabled contacts --> LDIF Delete entry
		            ContactQuery contactQuery = (ContactQuery)pm.newQuery(Contact.class);
		            contactQuery.modifiedAt().greaterThanOrEqualTo(new Date(syncKey));
		            contactQuery.modifiedAt().lessThan(new Date(newSyncKey));		            
		        	List<Contact> contacts = accountFilter.getFilteredAccount(contactQuery);
		        	File fPeople = new File(syncDir, "people");
		        	fPeople.mkdir();
		        	File fPeopleAdd = new File(fPeople, "Add");
		        	fPeopleAdd.mkdir();
		        	File fPeopleModify = new File(fPeople, "Modify");
		        	fPeopleModify.mkdir();
		        	File fPeopleDelete = new File(fPeople, "Delete");
		        	fPeopleDelete.mkdir();
		        	File fPeopleError = new File(fPeople, "Error");
		        	fPeopleError.mkdir();
		        	for(Contact contact: contacts) {
		        		String uid = this.getUid(contact);
		        		if(uid != null && !uid.isEmpty()) {
			        		try {
			        			if(Boolean.TRUE.equals(contact.isDisabled())) {
				        			File fPersonDelete = new File(fPeopleDelete, syncKey + "-" + contactCount + ".ldif");
				        			fPeopleDelete.mkdirs();
				    	        	PrintWriter pwPersonDelete = new PrintWriter(fPersonDelete, "UTF-8");
				        			this.writeLdifDeleteEntry(contact, baseDNPeople, pwPersonDelete);
				        			pwPersonDelete.close();
			        			} else {
				        			File fPersonAdd = new File(fPeopleAdd, syncKey + "-" + contactCount + ".ldif");
				        			fPeopleAdd.mkdirs();
				    	        	PrintWriter pwPersonAdd = new PrintWriter(fPersonAdd, "UTF-8");
				        			this.writeLdifAddEntry(contact, baseDNPeople, pwPersonAdd);
				        			pwPersonAdd.close();
				    	        	File fPersonModify = new File(fPeopleModify, syncKey + "-" + contactCount + ".ldif");
				    	        	fPeopleModify.mkdirs();
				    	        	PrintWriter pwPersonModify = new PrintWriter(fPersonModify, "UTF-8");
				        			this.writeLdifModifyEntry(contact, baseDNPeople, pwPersonModify);
				        			pwPersonModify.close();
			        			}
			        		} catch(Exception e) {
			        			File fPersonError = new File(fPeopleError, syncKey + "-" + contactCount + ".ldif");
			        			fPeopleError.mkdirs();
			    	        	PrintWriter pwPersonError = new PrintWriter(fPersonError, "UTF-8");
			        			this.writeError(contact, baseDNPeople, e, pwPersonError);
			        			pwPersonError.close();
			        		}
		        		} else {
		        			File fPersonError = new File(fPeopleError, syncKey + "-" + contactCount + ".ldif");
		        			fPeopleError.mkdirs();
		    	        	PrintWriter pwPersonError = new PrintWriter(fPersonError, "UTF-8");
		        			this.writeError(contact, baseDNPeople, null, pwPersonError);
		        			pwPersonError.close();
		        		}
		        		contactCount++;
		        	}
    			}
	        	// Export groups
    			{
		        	File fGroups = new File(syncDir, "groups");
		        	fGroups.mkdirs();
		        	File fGroupsAdd = new File(fGroups, "Add");
		        	fGroupsAdd.mkdir();
		        	File fGroupsModify = new File(fGroups, "Modify");
		        	fGroupsModify.mkdir();
		        	File fGroupsError = new File(fGroups, "Error");
		        	fGroupsError.mkdir();
		        	if(contactCount > 0) {
			        	Map<String,List<String[]>> groupMemberships = new HashMap<String,List<String[]>>();
			            ContactQuery contactQuery = (ContactQuery)pm.newQuery(Contact.class);
			            contactQuery.forAllDisabled().isFalse(); // only non-disabled contacts group group memberships
			        	List<Contact> contacts = accountFilter.getFilteredAccount(contactQuery);
			        	for(Contact contact: contacts) {
			        		String uid = this.getUid(contact);
			        		if(uid != null && !uid.isEmpty()) {
			        			String memberDN = this.getDN(contact, baseDNPeople);
			        			try {
				        			for(PrincipalGroup group: contact.<PrincipalGroup>getOwningGroup()) {
				        				String groupName = group.getName();
				        				List<String[]> members = groupMemberships.get(groupName);
				        				if(members == null) {
				        					groupMemberships.put(
				        						groupName,
				        						members = new ArrayList<String[]>()
				        					);				
				        				}
				        				members.add(
				        					new String[]{
				        						memberDN, 
				        						contact.refGetPath().toXRI()
				        					}
				        				);
				        			}
			        			} catch(Exception ignore) {}
			        		}
			        	}
			        	int groupCount = 0;
			        	for(String groupName: groupMemberships.keySet()) {
			        		List<String[]> members =  groupMemberships.get(groupName);
			        		// Check for duplicate group memberships
			        		List<String[]> validMembers = new ArrayList<String[]>();
			        		List<String[]> duplicateMembers = new ArrayList<String[]>();
			        		for(String[] member: members) {
			        			boolean found = false;
			        			for(String[] validMember: validMembers) {
			        				if(member[0].equals(validMember[0])) {
			        					found = true;
			        					break;
			        				}
			        			}
			        			if(found) {
			        				duplicateMembers.add(member);
			        			} else {
			        				validMembers.add(member);
			        			}
			        		}
			        		if(!duplicateMembers.isEmpty()) {
			        			File fGroupError = new File(fGroupsError, syncKey + "-" + groupCount + ".ldif");
			        			fGroupsError.mkdirs();
			    	        	PrintWriter pwGroupError = new PrintWriter(fGroupError, "UTF-8");
			        			this.writeError(groupName, baseDNGroups, validMembers, duplicateMembers, pwGroupError);
			        			pwGroupError.close();
			        		} else {
				        		try {
				        			File fGroupAdd = new File(fGroupsAdd, syncKey + "-" + groupCount + ".ldif");
				        			fGroupsAdd.mkdirs();
				    	        	PrintWriter pwGroupAdd = new PrintWriter(fGroupAdd, "UTF-8");
				        			this.writeLdifAddEntry(groupName, baseDNGroups, pwGroupAdd);
				        			pwGroupAdd.close();
				    	        	File fGroupModify = new File(fGroupsModify, syncKey + "-" + groupCount + ".ldif");
				    	        	fGroupsModify.mkdirs();
				    	        	PrintWriter pwGroupModify = new PrintWriter(fGroupModify, "UTF-8");
				        			this.writeLdifModifyEntry(groupName, members, baseDNGroups, pwGroupModify);
				        			pwGroupModify.close();	        		
				        		} catch(Exception e) {
				        			File fGroupError = new File(fGroupsError, syncKey + "-" + groupCount + ".ldif");
				        			fGroupsError.mkdirs();
				    	        	PrintWriter pwGroupError = new PrintWriter(fGroupError, "UTF-8");
				        			this.writeError(groupName, baseDNGroups, e, pwGroupError);
				        			pwGroupError.close();
				        		}
			        		}
			        		groupCount++;
			        	}
		        	}
    			}
	        	// Update syncKey
	            syncKeyProperty = ComponentConfigHelper.getComponentConfigProperty(
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
        } catch(Exception e) {
            new ServiceException(e).log();
            System.out.println(new Date() + ": " + WORKFLOW_NAME + " " + providerName + "/" + segmentName + ": exception occured " + e.getMessage() + ". Continuing");
        }        
    }

    /**
     * Handle sync request.
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
        if(System.currentTimeMillis() > this.startedAt + 180000L) {
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
	                    this.sync(
	                        id,
	                        providerName,
	                        segmentName,
	                        req,
	                        res
	                    );
	                } catch(Exception e) {
	                	SysLog.warning(e.getMessage(), e.getCause());
	                } finally {
	                    runningSegments.remove(id);
			    		System.out.println(new Date() + ": " + WORKFLOW_NAME + " " + providerName + "/" + segmentName + ": Done.");
	                }
		    	} else if(
		    		!runningSegments.get(id).isAlive() || 
		    		runningSegments.get(id).isInterrupted()
		    	) {
		        	Thread t = runningSegments.get(id);
		    		System.out.println(new Date() + ": " + WORKFLOW_NAME + " " + providerName + "/" + segmentName + ": Workflow " + t.getId() + " is alive=" + t.isAlive() + "; interrupted=" + t.isInterrupted() + ". Skipping execution.");
		    	} else {
		    		System.out.println(new Date() + ": " + WORKFLOW_NAME + " " + providerName + "/" + segmentName + ": Workflow is active. Ignoring command. Running segments are " + runningSegments);
		    	}
		    } else {
		    	SysLog.warning(WORKFLOW_NAME + " " + providerName + "/" + segmentName + ". Ignoring command. Running segments are", runningSegments);                
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
	private static final long serialVersionUID = 8358818934432722015L;

    protected static final String COMMAND_EXECUTE = "/execute";
    protected static final String WORKFLOW_NAME = "LDAPSynchronizer";
    protected static final String COMPONENT_CONFIGURATION_ID = "LDAPSynchronizer";
    protected static final Map<String,Thread> runningSegments = new ConcurrentHashMap<String,Thread>();

    protected static final String OPTION_SYNC_KEY = "syncKey";
    protected static final String OPTION_SYNC_DIR = "syncDir";
    protected static final String OPTION_RUN_AS = "runAs";
    protected static final String OPTION_BASE_DN_PEOPLE = "baseDNPeople";
    protected static final String OPTION_BASE_DN_GROUPS = "baseDNGroups";

    protected PersistenceManagerFactory pmf = null;
    protected long startedAt = System.currentTimeMillis();

}

//--- End of File -----------------------------------------------------------
