/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: LDAPSession
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2010, CRIXP Corp., Switzerland
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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.net.Socket;
import java.net.SocketTimeoutException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.jdo.PersistenceManager;

import org.opencrx.application.adapter.AbstractServer;
import org.opencrx.application.adapter.AbstractSession;
import org.opencrx.kernel.account1.cci2.AccountQuery;
import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.account1.jmi1.AccountAddress;
import org.opencrx.kernel.account1.jmi1.Contact;
import org.opencrx.kernel.account1.jmi1.EMailAddress;
import org.opencrx.kernel.account1.jmi1.PhoneNumber;
import org.opencrx.kernel.account1.jmi1.PostalAddress;
import org.opencrx.kernel.account1.jmi1.WebAddress;
import org.opencrx.kernel.backend.Accounts;
import org.opencrx.kernel.utils.AccountQueryHelper;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.base.persistence.cci.UserObjects;
import org.openmdx.kernel.exception.BasicException;
import org.openmdx.kernel.log.SysLog;
import org.openmdx.portal.servlet.Codes;

import com.sun.jndi.ldap.Ber;
import com.sun.jndi.ldap.BerDecoder;
import com.sun.jndi.ldap.BerEncoder;

/**
 * Handle a LDAP session.
 */
public class LDAPSession extends AbstractSession {
		
    //-----------------------------------------------------------------------
    public LDAPSession(
        Socket socket, 
        AbstractServer server
    ) {
    	super(
    		socket,
    		server
    	);
    }

    //-----------------------------------------------------------------------
    public void stop(
    ) {
    	if(this.socket != null) {
    		// Closing the socket stops the thread
    		try {
    			this.socket.close();
    			this.socket = null;
    		} catch(Exception e) {}    		
    	}
    }
    
    //-----------------------------------------------------------------------
    protected boolean isLdapV3(
    ) {
        return ldapVersion == LDAP_VERSION3;
    }

    //-----------------------------------------------------------------------
    @Override
    public void run(
    ) {
        if(this.socket == null || !this.socket.isConnected()) {
            System.out.println("Unable to start conversation, invalid connection passed to client handler");
        } 
        else {
        	SysLog.info("Session started for client", this.socket.getInetAddress().getHostAddress());
            try {
		        int bytesread;
		        int bytesleft;
		        int br;
		        int offset;
		        boolean eos;
                this.out = new PrintStream(this.socket.getOutputStream());
                this.in = this.socket.getInputStream();    	
		        byte[] inbuf = new byte[2048];   // Buffer for reading incoming bytes
	            while (true) {
	                offset = 0;	
	                // check that it is the beginning of a sequence
	                bytesread = this.in.read(inbuf, offset, 1);
	                if (bytesread < 0) {
	                    break; // EOF
	                }
	                if (inbuf[offset++] != (Ber.ASN_SEQUENCE | Ber.ASN_CONSTRUCTOR)) {
	                    continue;
	                }
	                // get length of sequence
	                bytesread = this.in.read(inbuf, offset, 1);
	                if (bytesread < 0) {
	                    break; // EOF
	                }
	                int seqlen = inbuf[offset++]; // Length of ASN sequence
	                // if high bit is on, length is encoded in the
	                // subsequent length bytes and the number of length bytes
	                // is equal to & 0x80 (i.e. length byte with high bit off).
	                if ((seqlen & 0x80) == 0x80) {
	                    int seqlenlen = seqlen & 0x7f;  // number of length bytes
	                    bytesread = 0;
	                    eos = false;
	                    // Read all length bytes
	                    while (bytesread < seqlenlen) {
	                        br = this.in.read(
	                        	inbuf, 
	                        	offset + bytesread,
	                            seqlenlen - bytesread
	                        );
	                        if (br < 0) {
	                            eos = true;
	                            break; // EOF
	                        }
	                        bytesread += br;
	                    }
	                    // end-of-stream reached before length bytes are read
	                    if (eos) {
	                        break;  // EOF
	                    }
	                    // Add contents of length bytes to determine length
	                    seqlen = 0;
	                    for (int i = 0; i < seqlenlen; i++) {
	                        seqlen = (seqlen << 8) + (inbuf[offset + i] & 0xff);
	                    }
	                    offset += bytesread;
	                }
	                // read in seqlen bytes
	                bytesleft = seqlen;
	                if ((offset + bytesleft) > inbuf.length) {
	                    byte[] nbuf = new byte[offset + bytesleft];
	                    System.arraycopy(inbuf, 0, nbuf, 0, offset);
	                    inbuf = nbuf;
	                }
	                while (bytesleft > 0) {
	                    bytesread = this.in.read(inbuf, offset, bytesleft);
	                    if (bytesread < 0) {
	                        break; // EOF
	                    }
	                    offset += bytesread;
	                    bytesleft -= bytesread;
	                }
	                //Ber.dumpBER(System.out, "request\n", inbuf, 0, offset);
	                handleRequest(new BerDecoder(inbuf, 0, offset));
	            }
            } 
            catch (Exception e) {
                if(!(e instanceof SocketTimeoutException)) {
                    ServiceException e0 = new ServiceException(e);
                    SysLog.detail(e0.getMessage(), e0.getCause());
                }
            }
            finally {                
                try {
                    // Make sure to close connection when thread terminates. 
                    // Otherwise we may have open connections with no listening threads.
                    this.socket.close();
                } 
                catch(Exception e) {}                
            }
        }
    }

    //-----------------------------------------------------------------------
    protected String getProviderName(
    ) {
    	return this.server.getProviderName();
    }

    //-----------------------------------------------------------------------
    protected Codes getCodes(
    	PersistenceManager pm
    ) {
    	return new Codes(
			(RefObject_1_0)pm.getObjectById(
				new Path("xri://@openmdx*org.opencrx.kernel.code1/provider/" +  LDAPSession.this.getProviderName() + "/segment/Root")
			)
		);
    }
    
    //-----------------------------------------------------------------------
    protected void handleRequest(
    	BerDecoder reqBer
    ) throws IOException {
        int currentMessageId = 0;
        try {
            reqBer.parseSeq(null);
            currentMessageId = reqBer.parseInt();
            int requestOperation = reqBer.peekByte();
            // Bind
            if(requestOperation == LDAP_REQ_BIND) {
                reqBer.parseSeq(null);
                this.ldapVersion = reqBer.parseInt();
                String userName = reqBer.parseString(isLdapV3());
                String password = reqBer.parseStringWithTag(Ber.ASN_CONTEXT, isLdapV3(), null);
                if(
                	(userName.length() > 0) && 
                	(password.length() > 0) &&
                	this.login(userName, password)
                ) {
                    this.sendClient(currentMessageId, LDAP_REP_BIND, LDAP_SUCCESS, "");
                }
                else {
                	this.sendClient(currentMessageId, LDAP_REP_BIND, LDAP_INVALID_CREDENTIALS, "");
                }
            }
            // Unbind
            else if(requestOperation == LDAP_REQ_UNBIND) {
                SysLog.detail("LOG_LDAP_REQ_UNBIND", currentMessageId);
                this.socket.close(); 
                this.logout();
                return;
            } 
            // Search
            else if (requestOperation == LDAP_REQ_SEARCH) {
                PersistenceManager pm = AbstractSession.newPersistenceManager(
                	this.server.getPersistenceManagerFactory(), 
                	this.username
                );            	
                reqBer.parseSeq(null);
                String dn = reqBer.parseString(isLdapV3());
                if(pm != null) {
                    int scope = reqBer.parseEnumeration();
                    reqBer.parseEnumeration();
                    int sizeLimit = reqBer.parseInt();
                    if (sizeLimit > SIZE_LIMIT || sizeLimit == 0) {
                        sizeLimit = SIZE_LIMIT;
                    }
                    int timeLimit = reqBer.parseInt();
                    reqBer.parseBoolean();
	                LDAPQuery ldapQuery = this.parseQuery(
	                	dn,
	                	reqBer,
	                	pm
	                );
	                if(ldapQuery != null) {
	                	ldapQuery.process(
		                	dn,
		                	currentMessageId,
	                		scope,
	                		sizeLimit, 
	                		timeLimit
	                	);
	                    LDAPSession.this.sendClient(currentMessageId, LDAP_REP_RESULT, LDAP_SUCCESS, "");	                	
	                }
	                else {
	                    LDAPSession.this.sendClient(currentMessageId, LDAP_REP_RESULT, LDAP_OTHER, "Invalid query");	  
	                }
            	}
            	else {
                    LDAPSession.this.sendClient(currentMessageId, LDAP_REP_RESULT, LDAP_OTHER, "Anonymous access forbidden");	  
            	}
                pm.close();
            } 
            // Abandon
            else if (requestOperation == LDAP_REQ_ABANDON) {
                SysLog.detail("LOG_LDAP_REQ_ABANDON_SEARCH", currentMessageId);
            } 
            // Unsupported
            else {
                SysLog.detail("LOG_LDAP_UNSUPPORTED_OPERATION", requestOperation);
                this.sendClient(currentMessageId, LDAP_REP_RESULT, LDAP_OTHER, "Unsupported operation");
            }
        } 
        catch (IOException e) {
            try {
                this.sendErr(currentMessageId, LDAP_REP_RESULT, e);
            } catch (IOException e2) {
                SysLog.detail("LOG_EXCEPTION_SENDING_ERROR_TO_CLIENT", e2);
            }
            throw e;
        }
    }

    //-----------------------------------------------------------------------
    protected LDAPQuery parseQuery(
    	String dn,
    	BerDecoder reqBer,
    	PersistenceManager pm
    ) throws IOException {
    	// Account filter
    	{
	    	Matcher personsMatcher = PERSONS_FILTER_PATTERN.matcher(dn);
	    	// Get accounts
	    	if(personsMatcher.matches()) {
	    		AccountQueryHelper accountsQuery = new AccountQueryHelper(pm);
	    		try {
		    		accountsQuery.parseQueryId(
		    			"/" + this.server.getProviderName() + "/" + this.segmentName + "/filter/" + personsMatcher.group(1)
		    		);
		    		LDAPFilter filter = this.parseFilter(reqBer);
		    		return new LDAPAccountQuery(
		    			accountsQuery,
		    			filter
		    		);
	    		} catch(Exception e) {
	    			new ServiceException(e).log();
	    		}
	    	}
    	}
    	// Account group
    	{
	    	Matcher personsMatcher = PERSONS_GROUP_PATTERN.matcher(dn);
	    	// Get accounts
	    	if(personsMatcher.matches()) {
	    		AccountQueryHelper accountsQuery = new AccountQueryHelper(pm);
	    		try {
		    		accountsQuery.parseQueryId(
		    			"/" + this.server.getProviderName() + "/" + this.segmentName + "/group/" + personsMatcher.group(1)
		    		);
		    		LDAPFilter filter = this.parseFilter(reqBer);
		    		return new LDAPAccountQuery(
		    			accountsQuery,
		    			filter
		    		);
	    		} catch(Exception e) {
	    			new ServiceException(e).log();
	    		}
	    	}
    	}
    	return null;
    }
    
    //-----------------------------------------------------------------------
    protected LDAPFilter parseFilter(
    	BerDecoder reqBer
    ) throws ServiceException {
    	try {
	    	LDAPFilter ldapFilter = null;
	        if(reqBer.peekByte() == LDAP_FILTER_PRESENT) {
	            String attributeName = reqBer.parseStringWithTag(LDAP_FILTER_PRESENT, isLdapV3(), null).toLowerCase();
	            ldapFilter = new LDAPSimpleFilter(attributeName);
	        } 
	        else {
	            int[] seqSize = new int[1];
	            int ldapFilterType = reqBer.parseSeq(seqSize);
	            int end = reqBer.getParsePosition() + seqSize[0];
	            ldapFilter = this.parseNestedFilter(reqBer, ldapFilterType, end);
	        }
	        return ldapFilter;
    	}
    	catch(Exception e) {
    		throw new ServiceException(e);
    	}
    }
    
    //-----------------------------------------------------------------------
    protected LDAPFilter parseNestedFilter(
    	BerDecoder reqBer, 
    	int ldapFilterType, 
    	int end
    ) throws ServiceException {
    	try {
	    	LDAPFilter nestedFilter;
	        if((ldapFilterType == LDAP_FILTER_OR) || (ldapFilterType == LDAP_FILTER_AND)) {
	            nestedFilter = new LDAPCompoundFilter(ldapFilterType);
	            while (reqBer.getParsePosition() < end && reqBer.bytesLeft() > 0) {
	                if (reqBer.peekByte() == LDAP_FILTER_PRESENT) {
	                    String attributeName = reqBer.parseStringWithTag(LDAP_FILTER_PRESENT, this.isLdapV3(), null).toLowerCase();
	                    ((LDAPCompoundFilter)nestedFilter).add(
	                    	new LDAPSimpleFilter(attributeName)
	                    );
	                } 
	                else {
		                int[] seqSize = new int[1];
		                int ldapFilterOperator = reqBer.parseSeq(seqSize);
		                int subEnd = reqBer.getParsePosition() + seqSize[0];
		                ((LDAPCompoundFilter)nestedFilter).add(
		                	this.parseNestedFilter(reqBer, ldapFilterOperator, subEnd)
		                );
	                }
	            }
	        } 
	        else {
	            nestedFilter = this.parseSimpleFilter(reqBer, ldapFilterType);
	        }
	        return nestedFilter;
    	}
    	catch(Exception e) {
    		throw new ServiceException(e);
    	}
    }
    
    //-----------------------------------------------------------------------    
    protected LDAPFilter parseSimpleFilter(
    	BerDecoder reqBer, 
    	int ldapFilterOperator
    ) throws ServiceException {
    	try {
	        String attributeName = reqBer.parseString(isLdapV3()).toLowerCase();
	        int ldapFilterMode = 0;
	        StringBuilder value = new StringBuilder();
	        if (ldapFilterOperator == LDAP_FILTER_SUBSTRINGS) {
	            // Thunderbird sends values with space as separate strings, rebuild value
	            int[] seqSize = new int[1];
	            /*LBER_SEQUENCE*/
	            reqBer.parseSeq(seqSize);
	            int end = reqBer.getParsePosition() + seqSize[0];
	            while (reqBer.getParsePosition() < end && reqBer.bytesLeft() > 0) {
	                ldapFilterMode = reqBer.peekByte();
	                if(value.length() > 0) {
	                    value.append(' ');
	                }
	                value.append(reqBer.parseStringWithTag(ldapFilterMode, this.isLdapV3(), null));
	            }
	        } 
	        else if (ldapFilterOperator == LDAP_FILTER_EQUALITY) {
	            value.append(reqBer.parseString(this.isLdapV3()));
	        } 
	        else {
	            SysLog.warning("LOG_LDAP_UNSUPPORTED_FILTER_VALUE");
	        }
	        String sValue = value.toString();
	        return new LDAPSimpleFilter(
	        	attributeName, 
	        	sValue, 
	        	ldapFilterOperator, 
	        	ldapFilterMode
	        );
    	}
    	catch(Exception e) {
    		throw new ServiceException(e);
    	}
    }

    //-----------------------------------------------------------------------
    protected void sendEntry(
    	int currentMessageId, 
    	String dn, 
    	Map<String,Object> attributes
    ) throws ServiceException {
    	try {
	    	BerEncoder berEncoder = berEncoders.get();
	        // synchronize on responseBer
	    	berEncoder.reset();
	    	berEncoder.beginSeq(Ber.ASN_SEQUENCE | Ber.ASN_CONSTRUCTOR);
	    	berEncoder.encodeInt(currentMessageId);
	    	berEncoder.beginSeq(LDAP_REP_SEARCH);
	    	berEncoder.encodeString(dn, isLdapV3());
	    	berEncoder.beginSeq(LBER_SEQUENCE);
	        for (Map.Entry<String, Object> entry : attributes.entrySet()) {
	        	berEncoder.beginSeq(LBER_SEQUENCE);
	        	berEncoder.encodeString(entry.getKey(), isLdapV3());
	        	berEncoder.beginSeq(LBER_SET);
	            Object values = entry.getValue();
	            if (values instanceof String) {
	            	berEncoder.encodeString((String) values, isLdapV3());
	            } 
	            else if (values instanceof List<?>) {
	                for (Object value : (List<?>) values) {
	                	berEncoder.encodeString((String) value, isLdapV3());
	                }
	            } 
	            else {
	                throw new ServiceException(
	                	BasicException.Code.DEFAULT_DOMAIN,
	                	BasicException.Code.ASSERTION_FAILURE,
	                	"EXCEPTION_UNSUPPORTED_VALUE",
	                	new BasicException.Parameter("values", values)
	                );
	            }
	            berEncoder.endSeq();
	            berEncoder.endSeq();
	        }
	        berEncoder.endSeq();
	        berEncoder.endSeq();
	        berEncoder.endSeq();
	        this.sendResponse(berEncoder);
    	}
    	catch(IOException e) {
    		throw new ServiceException(e);
    	}
    }

    //-----------------------------------------------------------------------
    protected void sendErr(
    	int currentMessageId, 
    	int responseOperation, 
    	Exception e
    ) throws IOException {
        String message = e.getMessage();
        if (message == null) {
            message = e.toString();
        }
        this.sendClient(currentMessageId, responseOperation, LDAP_OTHER, message);
    }

    //-----------------------------------------------------------------------
    protected void sendClient(
    	int currentMessageId, 
    	int responseOperation, 
    	int status, 
    	String message
    ) throws IOException {
    	BerEncoder berEncoder = berEncoders.get();
    	berEncoder.reset();
    	berEncoder.beginSeq(Ber.ASN_SEQUENCE | Ber.ASN_CONSTRUCTOR);
    	berEncoder.encodeInt(currentMessageId);
    	berEncoder.beginSeq(responseOperation);
    	berEncoder.encodeInt(status, LBER_ENUMERATED);
        // dn
    	berEncoder.encodeString("", isLdapV3());
        // error message
    	berEncoder.encodeString(message, isLdapV3());
    	berEncoder.endSeq();
    	berEncoder.endSeq();
        sendResponse(berEncoder);
    }

    //-----------------------------------------------------------------------
    protected void sendResponse(
    	BerEncoder berEncoder
    ) throws IOException {
        //Ber.dumpBER(System.out, ">\n", berEncoder.getBuf(), 0, berEncoder.getDataLen());
        this.out.write(berEncoder.getBuf(), 0, berEncoder.getDataLen());
        this.out.flush();
    }

    //-----------------------------------------------------------------------
    static interface LDAPQuery {
    	
        public void process(
        	String dn,
        	int messageId,
    		int scope,
    		int sizeLimit, 
    		int timeLimit
    	);        	
        
    }

    //-----------------------------------------------------------------------
    class LDAPAccountQuery implements LDAPQuery {
    	
    	public LDAPAccountQuery(
    		AccountQueryHelper accountsFilterHelper,
    		LDAPFilter ldapFilter
    	) {
    		this.accountsFilterHelper = accountsFilterHelper;
    		this.ldapFilter = ldapFilter;
    	}

    	private Object toStreet(
    		PostalAddress postalAddress
    	) {
    		return postalAddress.getPostalStreet();
    	}
    	
    	private String getCountryName(
    		PostalAddress postalAddress
    	) {
    		Codes codes = LDAPSession.this.getCodes(this.accountsFilterHelper.getPersistenceManager());
        	return codes == null ? 
        		Short.toString(postalAddress.getPostalCountry()) :
        			(String)codes.getLongText(
        				"country", 
        				(short)0, 
        				true, 
        				true
        			).get(
        				postalAddress.getPostalCountry()
        			);
    	}
    	
		@Override
        public void process(
        	String dn,
        	int messageId,
        	int scope, 
        	int sizeLimit, 
        	int timeLimit
        ) {
			PersistenceManager pm = this.accountsFilterHelper.getPersistenceManager();
			pm = pm.getPersistenceManagerFactory().getPersistenceManager(
				UserObjects.getPrincipalChain(pm).toString(),
				null
			);
			AccountQuery accountQuery = (AccountQuery)pm.newQuery(Account.class);			
			if(this.ldapFilter != null) {
				if(this.ldapFilter instanceof LDAPCompoundFilter) {
					LDAPCompoundFilter compoundFilter = (LDAPCompoundFilter)this.ldapFilter;
					for(LDAPFilter criteria: compoundFilter.criteria) {
						if(criteria instanceof LDAPSimpleFilter) {
							LDAPSimpleFilter simpleFilter = (LDAPSimpleFilter)criteria;
							if("cn".equals(simpleFilter.getAttributeName())) {
								accountQuery.thereExistsFullName().like(
									"(?i).*" + simpleFilter.value + ".*"
								);
							}
						} else if(criteria instanceof LDAPCompoundFilter) {
							LDAPCompoundFilter nestedCompoundFilter = (LDAPCompoundFilter)criteria;
							for(LDAPFilter nestedCriteria: nestedCompoundFilter.criteria) {
								if(nestedCriteria instanceof LDAPSimpleFilter) {
									LDAPSimpleFilter simpleFilter = (LDAPSimpleFilter)nestedCriteria;
									if("cn".equals(simpleFilter.getAttributeName())) {
										accountQuery.thereExistsFullName().like(
											"(?i).*" + simpleFilter.value + ".*"
										);
									}									
								}
							}							
						}
					}
				}
			}
			int n = 0;
			for(Account account: this.accountsFilterHelper.getFilteredAccounts(accountQuery)) {
				try {
					Map<String,Object> attributes = new HashMap<String,Object>();
					AccountAddress[] addresses = Accounts.getInstance().getMainAddresses(account);
					attributes.put("uid", account.refGetPath().getBase());
					attributes.put("objectclass", PERSON_OBJECT_CLASSES);
					boolean hasEMailBusiness = false;
					if(addresses[Accounts.MAIL_BUSINESS] != null) {
						attributes.put("mail", ((EMailAddress)addresses[Accounts.MAIL_BUSINESS]).getEmailAddress());
						hasEMailBusiness = true;
					}
					if(addresses[Accounts.MAIL_HOME] != null) {
						if(!hasEMailBusiness) {
							attributes.put("mail", ((EMailAddress)addresses[Accounts.MAIL_HOME]).getEmailAddress());
						}
						else {
							attributes.put("email2", ((EMailAddress)addresses[Accounts.MAIL_HOME]).getEmailAddress());							
						}
					}
			        attributes.put("cn", account.getFullName());
			        attributes.put("displayName", account.getFullName());
			        boolean hasPhoneNumberBusiness = false;
					if(addresses[Accounts.PHONE_BUSINESS] != null) {
						attributes.put("telephoneNumber", ((PhoneNumber)addresses[Accounts.PHONE_BUSINESS]).getPhoneNumberFull());
						hasPhoneNumberBusiness = true;
					}
					if(addresses[Accounts.PHONE_HOME] != null) {
						if(!hasPhoneNumberBusiness) {
							attributes.put("telephoneNumber", ((PhoneNumber)addresses[Accounts.PHONE_HOME]).getPhoneNumberFull());
						}
						attributes.put("homephone", ((PhoneNumber)addresses[Accounts.PHONE_HOME]).getPhoneNumberFull());
					}
					if(account instanceof Contact) {
						Contact contact = (Contact)account;
						if(contact.getOrganization() != null) {
							attributes.put("company", contact.getOrganization());
							attributes.put("o", contact.getOrganization());
						}
						if(contact.getJobTitle() != null) {
					        attributes.put("title", contact.getJobTitle());							
						}
						if(contact.getJobRole() != null) {
					        attributes.put("organizationalRole", contact.getJobRole());							
						}
						if(contact.getFirstName() != null) {
					        attributes.put("givenName", contact.getFirstName());							
						}
						if(contact.getLastName() != null) {
					        attributes.put("sn", contact.getLastName());							
						}
					}
					boolean hasPostalBusiness = false;
					if(addresses[Accounts.POSTAL_BUSINESS] != null) {
						PostalAddress postalAddress = (PostalAddress)addresses[Accounts.POSTAL_BUSINESS];
						Object street = this.toStreet(postalAddress);
						attributes.put("street", street);
						attributes.put("streetAddress", street);
						if(postalAddress.getPostalState() != null) {
							attributes.put("st", postalAddress.getPostalState());
						}
						if(postalAddress.getPostalCode() != null) {
							attributes.put("postalCode", postalAddress.getPostalCode());
						}
						if(postalAddress.getPostalCode() != null) { 
							attributes.put("l", postalAddress.getPostalCity());
						}						
				        attributes.put(
				        	"c",
				        	this.getCountryName(postalAddress)
				        );												
				        attributes.put(
				        	"countryname", 
				        	this.getCountryName(postalAddress)
				        );
						if(street instanceof List<?> && ((List<?>)street).size() > 0) {
							attributes.put("mozillaWorkstreet", ((List<?>)street).get(0));
						}
						if(street instanceof List<?> && ((List<?>)street).size() > 1) {
							attributes.put("mozillaWorkStreet2", ((List<?>)street).get(1));								
						}
				        hasPostalBusiness = true;
					}
					if(addresses[Accounts.POSTAL_HOME] != null) {
						PostalAddress postalAddress = (PostalAddress)addresses[Accounts.POSTAL_HOME];
						Object street = this.toStreet(postalAddress);
						if(!hasPostalBusiness) {
							attributes.put("street", street);
							attributes.put("streetAddress", street);
							if(postalAddress.getPostalState() != null) {
								attributes.put("st", postalAddress.getPostalState());
							}
							if(postalAddress.getPostalCode() != null) {
								attributes.put("postalCode", postalAddress.getPostalCode());
							}
							if(postalAddress.getPostalCode() != null) { 
								attributes.put("l", postalAddress.getPostalCity());
							}						
					        attributes.put(
					        	"c", 
					        	this.getCountryName(postalAddress)
					        );
						}
						if(street instanceof List<?> && ((List<?>)street).size() > 0) {
							attributes.put("mozillaHomeStreet", ((List<?>)street).get(0));
						}
						if(street instanceof List<?> && ((List<?>)street).size() > 1) {
							attributes.put("mozillaHomeStreet2", ((List<?>)street).get(1));								
						}
						if(postalAddress.getPostalState() != null) {
							attributes.put("mozillaHomeState", postalAddress.getPostalState());
						}
						if(postalAddress.getPostalCode() != null) {
							attributes.put("mozillaHomePostalCode", postalAddress.getPostalCode());
						}
						if(postalAddress.getPostalCode() != null) { 
							attributes.put("mozillaHomeLocalityName", postalAddress.getPostalCity());
						}						
				        attributes.put(
				        	"countryname", 
				        	this.getCountryName(postalAddress)
				        );
				        attributes.put(
				        	"mozillaHomeCountryName", 
				        	this.getCountryName(postalAddress)
				        );
					}
					if(addresses[Accounts.MOBILE] != null) {
						attributes.put("mobile", ((PhoneNumber)addresses[Accounts.MOBILE]).getPhoneNumberFull());						
					}
					if(addresses[Accounts.WEB_BUSINESS] != null) {
						attributes.put("mozillaWorkUrl", ((WebAddress)addresses[Accounts.WEB_BUSINESS]).getWebUrl());
					}
					if(addresses[Accounts.WEB_HOME] != null) {
						attributes.put("mozillaHomeUrl", ((WebAddress)addresses[Accounts.WEB_HOME]).getWebUrl());
					}
					if(account.getDescription() != null) {
						attributes.put("description", account.getDescription());
					}
					if(account.getAliasName() != null) {
						attributes.put("mozillanickname", account.getAliasName());
					}
					LDAPSession.this.sendEntry(
						messageId, 
						"cn=" + attributes.get("cn") + "," + dn, 
						attributes
					);					
					n++;
					if(n > sizeLimit) break;
				} catch(Exception e) {
					new ServiceException(e).log();
				}
			}	        
        }
		
		private final AccountQueryHelper accountsFilterHelper;
		private final LDAPFilter ldapFilter;
		
	}

    //-----------------------------------------------------------------------
    interface LDAPFilter {

    	public String getSearchFilter();
    	
    	public boolean isFullSearch();
    	
    }
    
    //-----------------------------------------------------------------------
    static class LDAPCompoundFilter implements LDAPFilter {
    	
        final Set<LDAPFilter> criteria = new HashSet<LDAPFilter>();
        final int type;

        LDAPCompoundFilter(
        	int filterType
        ) {
            this.type = filterType;
        }

        @Override
        public String toString(
        ) {
            StringBuilder buffer = new StringBuilder();
            if (type == LDAP_FILTER_OR) {
                buffer.append("(|");
            } else {
                buffer.append("(&");
            }
            for (LDAPFilter child : criteria) {
                buffer.append(child.toString());
            }
            buffer.append(')');

            return buffer.toString();
        }

        /**
         * Add child filter
         *
         * @param filter inner filter
         */
        public void add(
        	LDAPFilter filter
        ) {
            criteria.add(filter);
        }

        /**
         * This is only a full search if every child
         * is also a full search
         *
         * @return true if full search filter
         */
        public boolean isFullSearch(
        ) {
            for (LDAPFilter child : criteria) {
                if (!child.isFullSearch()) {
                    return false;
                }
            }
            return true;
        }

        /**
         * Build search filter for Contacts folder search.
         * Use Exchange SEARCH syntax
         *
         * @return contact search filter
         */
        public String getSearchFilter(
        ) {
            StringBuilder buffer = new StringBuilder();
            String op;
            if (type == LDAP_FILTER_OR) {
                op = " OR ";
            } else {
                op = " AND ";
            }
            buffer.append('(');
            for (LDAPFilter child : criteria) {
                String childFilter = child.getSearchFilter();
                if (childFilter != null) {
                    if (buffer.length() > 1) {
                        buffer.append(op);
                    }
                    buffer.append(childFilter);
                }
            }
            // empty filter
            if (buffer.length() == 1) {
                return null;
            }
            buffer.append(')');
            return buffer.toString();
        }

    }
    
    //-----------------------------------------------------------------------
    static class LDAPSimpleFilter implements LDAPFilter {
    	
        static final String STAR = "*";
        final String attributeName;
        final String value;
        final int mode;
        final int operator;

        LDAPSimpleFilter(
        	String attributeName
        ) {
            this.attributeName = attributeName;
            this.value = LDAPSimpleFilter.STAR;
            this.operator = LDAP_FILTER_SUBSTRINGS;
            this.mode = 0;
        }

        LDAPSimpleFilter(
        	String attributeName, 
        	String value, 
        	int ldapFilterOperator, 
        	int ldapFilterMode
        ) {
            this.attributeName = attributeName;
            this.value = value;
            this.operator = ldapFilterOperator;
            this.mode = ldapFilterMode;
        }

        @Override
        public boolean isFullSearch(
        ) {
            return "objectclass".equals(attributeName) && STAR.equals(value);
        }

        @Override
        public String toString(
        ) {
            StringBuilder buffer = new StringBuilder();
            buffer.append('(');
            buffer.append(attributeName);
            buffer.append('=');
            if (LDAPSimpleFilter.STAR.equals(value)) {
                buffer.append(LDAPSimpleFilter.STAR);
            } 
            else if (operator == LDAP_FILTER_SUBSTRINGS) {
                if (mode == LDAP_SUBSTRING_FINAL || mode == LDAP_SUBSTRING_ANY) {
                    buffer.append(LDAPSimpleFilter.STAR);
                }
                buffer.append(value);
                if (mode == LDAP_SUBSTRING_INITIAL || mode == LDAP_SUBSTRING_ANY) {
                    buffer.append(LDAPSimpleFilter.STAR);
                }
            } else {
                buffer.append(value);
            }

            buffer.append(')');
            return buffer.toString();
        }

        @Override
        public String getSearchFilter(
        ) {
            StringBuilder buffer;
            buffer = new StringBuilder();
            buffer.append('"').append(this.getAttributeName()).append('"');
            if (operator == LDAP_FILTER_EQUALITY) {
                buffer.append("='").append(value).append('\'');
            } 
            else if ("*".equals(value)) {
                buffer.append(" is not null");
            } 
            else {
                buffer.append(" LIKE '");
                if (mode == LDAP_SUBSTRING_FINAL || mode == LDAP_SUBSTRING_ANY) {
                    buffer.append('%');
                }
                buffer.append(value.replaceAll("'", "''"));
                // endsWith not supported by exchange, always append %
                buffer.append('%');
                buffer.append('\'');
            }
            return buffer.toString();
        }

        public String getAttributeName(
        ) {
            return attributeName;
        }

    }

    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    public static final int SIZE_LIMIT = 1000;
    
    static final Pattern PERSONS_FILTER_PATTERN = Pattern.compile("ou=filter/(.+?),ou=Persons");
    static final Pattern PERSONS_GROUP_PATTERN = Pattern.compile("ou=group/(.+?),ou=Persons");

    static final List<String> PERSON_OBJECT_CLASSES = Arrays.asList(
    	"top", 
    	"person", 
    	"organizationalPerson", 
    	"inetOrgPerson", 
    	"user", 
    	"mozillaAbPerson",
    	"mozillaAddressBookEntry"
    );

    // LDAP version
    static final int LDAP_VERSION2 = 0x02;
    static final int LDAP_VERSION3 = 0x03;

    // LDAP request operations
    static final int LDAP_REQ_BIND = 0x60;
    static final int LDAP_REQ_SEARCH = 0x63;
    static final int LDAP_REQ_UNBIND = 0x42;
    static final int LDAP_REQ_ABANDON = 0x50;

    // LDAP response operations
    static final int LDAP_REP_BIND = 0x61;
    static final int LDAP_REP_SEARCH = 0x64;
    static final int LDAP_REP_RESULT = 0x65;

    // LDAP return codes
    static final int LDAP_OTHER = 80;
    static final int LDAP_SUCCESS = 0;
    static final int LDAP_SIZE_LIMIT_EXCEEDED = 4;
    static final int LDAP_INVALID_CREDENTIALS = 49;

    static final int LDAP_FILTER_AND = 0xa0;
    static final int LDAP_FILTER_OR = 0xa1;

    // LDAP filter operators (only LDAP_FILTER_SUBSTRINGS is supported)
    static final int LDAP_FILTER_SUBSTRINGS = 0xa4;
    static final int LDAP_FILTER_GE = 0xa5;
    static final int LDAP_FILTER_LE = 0xa6;
    static final int LDAP_FILTER_PRESENT = 0x87;
    static final int LDAP_FILTER_APPROX = 0xa8;
    static final int LDAP_FILTER_EQUALITY = 0xa3;

    // LDAP filter mode
    static final int LDAP_SUBSTRING_INITIAL = 0x80;
    static final int LDAP_SUBSTRING_ANY = 0x81;
    static final int LDAP_SUBSTRING_FINAL = 0x82;

    // BER data types
    static final int LBER_ENUMERATED = 0x0a;
    static final int LBER_SET = 0x31;
    static final int LBER_SEQUENCE = 0x30;

    // LDAP search scope
    static final int SCOPE_BASE_OBJECT = 0;
    static final int SCOPE_ONE_LEVEL = 1;
    static final int SCOPE_SUBTREE = 2;

    private static ThreadLocal<BerEncoder> berEncoders = new ThreadLocal<BerEncoder>() {
        protected synchronized BerEncoder initialValue() {
            return new BerEncoder();
        }
    };
    
    int ldapVersion = LDAP_VERSION3;

    protected OutputStream out = null;
    protected InputStream in = null;
    
}
