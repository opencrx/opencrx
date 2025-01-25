/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: DoPropfind
 * Owner:       the original authors.
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
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
 * * Neither the name of the openCRX team nor the names of the contributors
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
package org.opencrx.application.caldav;

import java.util.List;
import java.util.Map;

import javax.jdo.JDOHelper;
import javax.jdo.PersistenceManager;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import org.opencrx.application.caldav.ActivityCollectionResource.Type;
import org.opencrx.application.uses.net.sf.webdav.RequestContext;
import org.opencrx.application.uses.net.sf.webdav.Resource;
import org.opencrx.application.uses.net.sf.webdav.WebDavStore;
import org.opencrx.application.uses.net.sf.webdav.fromcatalina.XMLWriter;
import org.opencrx.kernel.home1.cci2.EMailAccountQuery;
import org.opencrx.kernel.home1.jmi1.EMailAccount;
import org.opencrx.kernel.home1.jmi1.SyncProfile;
import org.opencrx.kernel.home1.jmi1.UserHome;

/**
 * DoPropfind
 *
 */
public class DoPropfind extends org.opencrx.application.uses.net.sf.webdav.methods.DoPropfind {

	/**
	 * Constructor.
	 * 
	 * @param store
	 */
	public DoPropfind(
		WebDavStore store 
	) {
	    super(store);
    }
	
	/* (non-Javadoc)
	 * @see org.opencrx.application.uses.net.sf.webdav.methods.DoPropfind#getNamespaces()
	 */
	@Override
    protected Map<String, String> getNamespaces(
    ) {
		Map<String,String> namespaces = super.getNamespaces();
		namespaces.put("urn:ietf:params:xml:ns:caldav", "C");
		namespaces.put("http://calendarserver.org/ns/", "CS");
		namespaces.put("http://apple.com/ns/ical/", "A");
		return namespaces;
    }

	/* (non-Javadoc)
	 * @see org.opencrx.application.uses.net.sf.webdav.methods.DoPropfind#writeCollectionType(org.opencrx.application.uses.net.sf.webdav.RequestContext, org.opencrx.application.uses.net.sf.webdav.fromcatalina.XMLWriter, org.opencrx.application.uses.net.sf.webdav.Resource)
	 */
	@Override
    protected void writeCollectionType(
    	RequestContext requestContext, 
    	XMLWriter writer, 
    	Resource res
    ) {
		if(res instanceof ActivityCollectionResource) {
			writer.writeElement("urn:ietf:params:xml:ns:caldav:calendar", XMLWriter.NO_CONTENT);
		}
    }

	/* (non-Javadoc)
	 * @see org.opencrx.application.uses.net.sf.webdav.methods.WebDavMethod#getVersion()
	 */
	@Override
    protected String getVersion(
    ) {
    	return "1, 2, calendar-access";
    }
	
	/* (non-Javadoc)
	 * @see org.opencrx.application.uses.net.sf.webdav.methods.DoPropfind#handleExtension(org.opencrx.application.uses.net.sf.webdav.RequestContext, org.opencrx.application.uses.net.sf.webdav.fromcatalina.XMLWriter, java.lang.String, org.opencrx.application.uses.net.sf.webdav.Resource, java.lang.String)
	 */
	@Override
    protected boolean handleExtension(
    	RequestContext requestContext,
    	XMLWriter writer,
    	String contextPath,
    	Resource res,
    	String property
    ) {
		HttpServletRequest req = requestContext.getHttpServletRequest();
		HttpServletResponse resp = requestContext.getHttpServletResponse();
		if(res instanceof CalendarProfileResource) {
			SyncProfile syncProfile = ((CalendarProfileResource)res).getObject();
			PersistenceManager pm = JDOHelper.getPersistenceManager(syncProfile);
    		UserHome userHome = (UserHome)pm.getObjectById(
    			syncProfile.refGetPath().getParent().getParent()
    		);
    		String providerName = userHome.refGetPath().getSegment(2).toString();
    		String segmentName = userHome.refGetPath().getSegment(4).toString();
			if(property.indexOf("current-user-principal") > 0) {
				String principalUrl = this.encodeURL(resp, this.getHRef(req, "/" + providerName + "/" + segmentName + "/user/" + userHome.refGetPath().getLastSegment() + "/profile/" + syncProfile.getName(), true));
                writer.writeElement("DAV::principal-URL", XMLWriter.OPENING);
                writer.writeElement("DAV::href", XMLWriter.OPENING);
                writer.writeText(principalUrl);
                writer.writeElement("DAV::href", XMLWriter.CLOSING);
                writer.writeElement("DAV::principal-URL", XMLWriter.CLOSING);
                writer.writeElement("DAV::current-user-principal", XMLWriter.OPENING);
                writer.writeElement("DAV::href", XMLWriter.OPENING);
                writer.writeText(principalUrl);
                writer.writeElement("DAV::href", XMLWriter.CLOSING);
                writer.writeElement("DAV::current-user-principal", XMLWriter.CLOSING);
                return true;
			} else if(property.indexOf("calendar-user-address-set") > 0) {
        		EMailAccountQuery emailAccountQuery = (EMailAccountQuery)pm.newQuery(EMailAccount.class);
        		emailAccountQuery.thereExistsIsDefault().isTrue();
        		emailAccountQuery.thereExistsIsActive().isTrue();
        		List<EMailAccount> emailAccounts = userHome.getEMailAccount(emailAccountQuery);
        		EMailAccount defaultEMailAccount = emailAccounts.isEmpty() 
        			? null 
        			: emailAccounts.iterator().next();
	            writer.writeElement("urn:ietf:params:xml:ns:caldav:calendar-user-address-set", XMLWriter.OPENING);
	            writer.writeElement("DAV::href", XMLWriter.OPENING);
	            writer.writeData("mailto:" + (defaultEMailAccount == null ? userHome.refGetPath().getLastSegment() + "@" + req.getServerName() : defaultEMailAccount.getName()));
	            writer.writeElement("DAV::href", XMLWriter.CLOSING);
	            writer.writeElement("DAV::href", XMLWriter.OPENING);
	            writer.writeText(this.encodeURL(resp, this.getHRef(req, req.getServletPath(), true)));
	            writer.writeElement("DAV::href", XMLWriter.CLOSING);
	            writer.writeElement("urn:ietf:params:xml:ns:caldav:calendar-user-address-set", XMLWriter.CLOSING);
	            return true;
			} else if(property.indexOf("calendar-home-set") > 0) {
	            writer.writeElement("urn:ietf:params:xml:ns:caldav:calendar-home-set", XMLWriter.OPENING);
	            writer.writeElement("DAV::href", XMLWriter.OPENING);
	            writer.writeText(this.encodeURL(resp, this.getHRef(req, "/" + providerName + "/" + segmentName + "/" + userHome.refGetPath().getLastSegment() + "/" + res.getName(), true)));
	            writer.writeElement("DAV::href", XMLWriter.CLOSING);
	            writer.writeElement("urn:ietf:params:xml:ns:caldav:calendar-home-set", XMLWriter.CLOSING);
	            return true;
			} else if(property.indexOf("principal-URL") > 0) {
	            writer.writeElement("DAV::principal-URL", XMLWriter.OPENING);
	            writer.writeElement("DAV::href", XMLWriter.OPENING);
	            writer.writeText(this.encodeURL(resp, this.getHRef(req, "/" + providerName + "/" + segmentName + "/user/" + userHome.refGetPath().getLastSegment() + "/profile/" + syncProfile.getName(), true)));
	            writer.writeElement("DAV::href", XMLWriter.CLOSING);
	            writer.writeElement("DAV::principal-URL", XMLWriter.CLOSING);
	            return true;				
			} else {
				return false;
			}
		} else if(res instanceof ActivityCollectionResource) {
			ActivityCollectionResource activityCollectionResource = (ActivityCollectionResource)res;
			if(property.indexOf("calendar-description") > 0) {
				writer.writeElement("urn:ietf:params:xml:ns:caldav:calendar-description", XMLWriter.OPENING);
	            writer.writeData(res.getDisplayName());				
				writer.writeElement("urn:ietf:params:xml:ns:caldav:calendar-description", XMLWriter.CLOSING);
				return true;
			} else if(property.indexOf("calendar-color") > 0) {
				if(activityCollectionResource.getBackColor() != null) {
					String[] backColors = activityCollectionResource.getBackColor().split(",");
					String backColor = backColors[0];
					if(activityCollectionResource.getType() == Type.VTODO) {
						backColor = backColors.length > 1 ? backColors[1] : backColors[0];
					}
					writer.writeElement("http://apple.com/ns/ical/:calendar-color", XMLWriter.OPENING);
					writer.writeData(backColor);
					writer.writeElement("http://apple.com/ns/ical/:calendar-color", XMLWriter.CLOSING);
					return true;
				}
			} else if(property.indexOf("current-user-privilege-set") > 0) {
				writer.writeElement("DAV::current-user-privilege-set", XMLWriter.OPENING);
				writer.writeElement("DAV::privilege", XMLWriter.OPENING);
				if(
					Boolean.TRUE.equals(activityCollectionResource.allowAddDelete()) &&
					Boolean.TRUE.equals(activityCollectionResource.allowChange())
				) {
					writer.writeElement("DAV::all", XMLWriter.NO_CONTENT);
				} else if(
					Boolean.TRUE.equals(activityCollectionResource.allowChange())
				) {
					writer.writeElement("DAV::write-content", XMLWriter.NO_CONTENT);
				} else {
					writer.writeElement("DAV::read", XMLWriter.NO_CONTENT);
				}
				writer.writeElement("DAV::privilege", XMLWriter.CLOSING);
				writer.writeElement("DAV::current-user-privilege-set", XMLWriter.CLOSING);
				return true;
			} else if(property.indexOf("supported-calendar-component-set") > 0) {
				writer.writeElement("urn:ietf:params:xml:ns:caldav:supported-calendar-component-set", XMLWriter.OPENING);
				if(activityCollectionResource.getType() == ActivityCollectionResource.Type.VEVENT) {
					writer.writeText("<C:comp name=\"VEVENT\"/>");					
				} else {
					writer.writeText("<C:comp name=\"VTODO\"/>");					
				}
				writer.writeElement("urn:ietf:params:xml:ns:caldav:supported-calendar-component-set", XMLWriter.CLOSING);	
				return true;
			} else if(property.indexOf("getctag") > 0) {
				writer.writeElement("CS::getctag", XMLWriter.OPENING);
	            writer.writeText(Long.toString(System.currentTimeMillis()));
				writer.writeElement("CS::getctag", XMLWriter.CLOSING);
				return true;
			}
		} else if(res instanceof ActivityResource) {
			if(property.indexOf("getctag") > 0) {
				writer.writeElement("http://calendarserver.org/ns/:getctag", XMLWriter.OPENING);
	            writer.writeText(this.getETag(res));				
				writer.writeElement("http://calendarserver.org/ns/:getctag", XMLWriter.CLOSING);				
				return true;
			} else if(property.indexOf("owner") > 0) {
				return true;
			} else if(property.indexOf("supported-report-set") > 0) {
				return true;
			} else if(property.indexOf("supported-calendar-component-set") > 0) {
				return true;
			}
		}
		return false;
    }

}
