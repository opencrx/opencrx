/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: SegmentSetupController
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
package org.opencrx.portal.wizard;

import java.io.IOException;
import java.io.Writer;

import javax.jdo.PersistenceManager;

import org.opencrx.kernel.backend.Admin;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.JspWizardController;
import org.openmdx.portal.servlet.ObjectReference;

/**
 * SegmentSetupController
 *
 */
public class SegmentSetupController extends JspWizardController {

	/**
	 * Constructor.
	 * 
	 */
	public SegmentSetupController(
	) {
		super();
	}

	/**
	 * Cancel action.
	 * 
	 */
	public void doCancel(
	) {
		this.setExitAction(
			new ObjectReference(this.getObject(), this.getApp()).getSelectObjectAction()
		);		
	}
	
	public void renderSetupReport(
		Writer out
	) throws ServiceException, IOException {
		out.append(this.report);
	}

	/**
	 * Refresh action.
	 * 
	 * @throws ServiceException
	 */
	public void doRefresh(
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		ApplicationContext app = this.getApp();
		this.currentUserIsAdmin = app.getCurrentUserRole().equals(
			org.opencrx.kernel.generic.SecurityKeys.ADMIN_PRINCIPAL + org.opencrx.kernel.generic.SecurityKeys.ID_SEPARATOR + this.getSegmentName() + "@" + this.getSegmentName()
		);
		this.userHome = (org.opencrx.kernel.home1.jmi1.UserHome)pm.getObjectById(
			app.getUserHomeIdentityAsPath()
		);
	}

	/**
	 * Setup action.
	 * 
	 * @throws ServiceException
	 */
	public void doSetup(
	) throws ServiceException {
		PersistenceManager pm = this.getPm();
		this.doRefresh();
		try {
			this.report = Admin.getInstance().initSegments(this.userHome);
		} catch(Exception e) {
			try {
				pm.currentTransaction().rollback();
			} catch(Exception e0) {}
			new ServiceException(e).log();
		}
	}

	/**
	 * @return the currentUserIsAdmin
	 */
	public boolean isCurrentUserIsAdmin() {
		return currentUserIsAdmin;
	}

	/**
	 * @return the userHome
	 */
	public org.opencrx.kernel.home1.jmi1.UserHome getUserHome() {
		return userHome;
	}

	private boolean currentUserIsAdmin;
	private String report = "";
	private org.opencrx.kernel.home1.jmi1.UserHome userHome;
}
