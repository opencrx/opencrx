/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: AccountResource
 * Owner:       CRIXP AG, Switzerland, http://www.crixp.com
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Copyright (c) 2004-2012, CRIXP Corp., Switzerland
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
package org.opencrx.application.carddav;

import java.io.ByteArrayOutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;

import javax.servlet.http.HttpServletRequest;

import org.opencrx.application.uses.ezvcard.Ezvcard;
import org.opencrx.application.uses.ezvcard.VCardVersion;
import org.opencrx.application.uses.net.sf.webdav.RequestContext;
import org.opencrx.application.uses.net.sf.webdav.WebDavStore;
import org.opencrx.kernel.account1.jmi1.Account;
import org.opencrx.kernel.backend.Base;
import org.opencrx.kernel.backend.VCard;
import org.opencrx.kernel.document1.jmi1.Media;
import org.openmdx.base.exception.ServiceException;
import org.w3c.cci2.BinaryLargeObject;
import org.w3c.cci2.BinaryLargeObjects;

/**
 * CardDAV resource of type Account.
 *
 */
public class AccountResource extends CardDavResource {

	public AccountResource(
		RequestContext requestContext,
		Account account,
		AccountCollectionResource parent
	) {
		super(requestContext, account);
		this.parent = parent;
	}

	/* (non-Javadoc)
	 * @see org.opencrx.application.carddav.CardDavResource#getObject()
	 */
	@Override
    public Account getObject(
    ) {
        return (Account)super.getObject();
    }

	/* (non-Javadoc)
	 * @see org.opencrx.application.uses.net.sf.webdav.Resource#isCollection()
	 */
	@Override
    public boolean isCollection(
    ) {
		return false;
    }
	
	/* (non-Javadoc)
	 * @see org.opencrx.application.carddav.CardDavResource#getMimeType()
	 */
	@Override 
	public String getMimeType(
	) {
		return VCard.MIME_TYPE;			
	}
	
    /* (non-Javadoc)
     * @see org.opencrx.application.carddav.CardDavResource#getName()
     */
    @Override
    public String getName(
    ) {
        return super.getName() + ".vcf";
    }

    /* (non-Javadoc)
     * @see org.opencrx.application.uses.net.sf.webdav.Resource#getDisplayName()
     */
    @Override
    public String getDisplayName(
    ) {
    	return this.getObject().getFullName();
    }
    
    public AccountCollectionResource getAccountCollectionResource(
    ) {
    	return this.parent;
    }
    
    /**
     * Get PHOTO property for given account if account.getPicture() is set.
     * 
     * @param p
     * @param account
     * @throws ServiceException
     */
    public static org.opencrx.application.uses.ezvcard.property.Photo getPhoto(
    	Account account
    ) {
    	if(account.getPicture() != null) {
    		Media picture = account.getPicture();
    		String mimeType = picture.getContentMimeType();
    		if(mimeType != null && mimeType.indexOf("/") > 0) {
    			try {
	    			return new org.opencrx.application.uses.ezvcard.property.Photo(
	    				picture.getContent().getContent(),
	    				org.opencrx.application.uses.ezvcard.parameter.ImageType.get(null, mimeType, null)
	    			);
    			} catch(Exception ignore) {}
    		}
    	}
    	return null;
    }

	/* (non-Javadoc)
	 * @see org.opencrx.application.carddav.CardDavResource#getContent()
	 */
	@Override
    public WebDavStore.ResourceContent getContent(
    ) {
		final ByteArrayOutputStream out = new ByteArrayOutputStream();
		PrintWriter p = null;
		try {
			p = new PrintWriter(new OutputStreamWriter(out, "UTF-8"));
		} catch(Exception e) {
			p = new PrintWriter(out);
		}
		Account account = this.getObject();
		HttpServletRequest req = this.getRequestContext().getHttpServletRequest();
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
        		org.opencrx.application.uses.ezvcard.property.Photo photo = getPhoto(account);
        		if(photo != null) {
        			vCard.addPhoto(photo);
        		}
            }
        	try {
        		Ezvcard.write(vCard).version(VCardVersion.V3_0).go(p);
        	} catch(Exception ignore) {}
        }
		p.close();
		return new WebDavStore.ResourceContent() {
			@Override
			public Long getLength() {
				return Long.valueOf(out.size());
			}
			
			@Override
			public BinaryLargeObject getContent() {
				return BinaryLargeObjects.valueOf(out.toByteArray());
			}
		};
    }

	private final AccountCollectionResource parent;
	
}