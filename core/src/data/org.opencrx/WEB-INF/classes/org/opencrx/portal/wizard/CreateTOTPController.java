/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: Manage2FAController
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

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.net.URLEncoder;
import java.util.Date;
import java.util.Hashtable;
import java.util.Properties;

import javax.imageio.ImageIO;
import javax.jdo.PersistenceManager;

import org.opencrx.kernel.backend.UserHomes;
import org.opencrx.kernel.home1.jmi1.UserHome;
import org.opencrx.kernel.utils.TOTP;
import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;
import org.openmdx.kernel.id.UUIDs;
import org.openmdx.portal.servlet.Action;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.JspWizardController;
import org.openmdx.portal.servlet.ObjectReference;
import org.w3c.cci2.BinaryLargeObjects;

import com.google.zxing.BarcodeFormat;
import com.google.zxing.EncodeHintType;
import com.google.zxing.common.BitMatrix;
import com.google.zxing.qrcode.QRCodeWriter;
import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel;

/**
 * Manage2FAController
 *
 */
public class CreateTOTPController extends JspWizardController {

	/**
	 * Constructor.
	 * 
	 */
	public CreateTOTPController(
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
	
	/**
	 * Refresh action.
	 * 
	 * @param stringToEncode
	 * @param location
	 */
	public void doRefresh(
		@JspWizardController.RequestParameter(name = "secretKey") String secretKey,
		@JspWizardController.RequestParameter(name = "account") String account,
		@JspWizardController.RequestParameter(name = "issuer") String issuer,
		@JspWizardController.RequestParameter(name = "location") String location		
	) {
		ApplicationContext app = this.getApp();
		Path userHomeIdentity = app.getUserHomeIdentityAsPath();
		// secretKey
		this.secretKey = secretKey;
		if(this.secretKey == null || this.secretKey.isEmpty()) {
			this.secretKey = org.opencrx.kernel.utils.TOTP.generateSecretKey();
		}
		// account
		this.account = account;
		if(this.account == null || this.account.isEmpty()) {
			this.account = userHomeIdentity.getLastSegment().toString();
		}
		// issuer
		this.issuer = issuer;
		if(this.issuer == null || this.issuer.isEmpty()) {
			this.issuer = this.getProviderName() + "/" + this.getSegmentName();
		}
		// location
		this.location = location;
		if(this.location == null) {
			this.location = UUIDs.getGenerator().next().toString();
		}
	}

	/**
	 * OK action.
	 * 
	 * @param secretKey
	 * @param account
	 * @param issuer
	 */
	public void doOK(
		@JspWizardController.RequestParameter(name = "secretKey") String secretKey,
		@JspWizardController.RequestParameter(name = "account") String account,
		@JspWizardController.RequestParameter(name = "issuer") String issuer,
		@JspWizardController.RequestParameter(name = "location") String location		
	) {
		ApplicationContext app = this.getApp();
		this.doRefresh(
			secretKey, 
			account,
			issuer,
			location
		);
		boolean hasDownloadFile = false;
		String downloadFileName = null;
		String mimeType = null;
		try {
			String authUrl = "otpauth://totp/"
	    		+ URLEncoder.encode(issuer + ":" + account, "UTF-8").replace("+", "%20")
	            + "?secret=" + URLEncoder.encode(secretKey, "UTF-8").replace("+", "%20")
	            + "&issuer=" + URLEncoder.encode(issuer, "UTF-8").replace("+", "%20");			
			// Store TOTP info as media
			{
				Properties info = new Properties();
				info.setProperty("secretKey", secretKey);
				info.setProperty("account", account);
				info.setProperty("issuer", issuer);
				info.setProperty("url", authUrl);
				ByteArrayOutputStream content = new ByteArrayOutputStream();
				info.store(
					content,
					CreateTOTPController.class.getSimpleName() + " @ " + new Date()
				);
				content.close();
				try {
					PersistenceManager pm = this.getPm();
					UserHome userHome = (UserHome)pm.getObjectById(app.getUserHomeIdentityAsPath());
					pm.currentTransaction().begin();
					UserHomes.getInstance().createOrUpdateMedia(
						userHome,
						TOTP.class.getSimpleName(),
						"text/plain",
						BinaryLargeObjects.valueOf(content.toByteArray())
					);
				} catch(Exception e) {
					try {
						pm.currentTransaction().rollback();
					} catch(Exception ignore) {}
				} finally {
					pm.currentTransaction().commit();
				}
			}
			// Create QR code
			{
				downloadFileName = Utils.toFilename(
					app.getPortalExtension().getTitle(
						this.getObject(), 
						app.getCurrentLocaleAsIndex(), 
						app.getCurrentLocaleAsString(), 
						true, 
						app
					)
				) + "." + QR_FILE_EXT;
				mimeType = QR_MIME_TYPE;
				File f = new File(app.getTempFileName(this.location, ""));
				Hashtable<EncodeHintType, ErrorCorrectionLevel> configuration = new Hashtable<EncodeHintType, ErrorCorrectionLevel>();
				configuration.put(EncodeHintType.ERROR_CORRECTION, ErrorCorrectionLevel.L);
				QRCodeWriter qrCodeWriter = new QRCodeWriter();
				BitMatrix byteMatrix = qrCodeWriter.encode(
					authUrl,
					BarcodeFormat.QR_CODE,
					QR_SIZE,
					QR_SIZE,
					configuration
				);
				int bmWidth = byteMatrix.getWidth();
				BufferedImage image = new BufferedImage(bmWidth, bmWidth, BufferedImage.TYPE_INT_RGB);
				image.createGraphics();
				Graphics2D graphics = (Graphics2D) image.getGraphics();
				graphics.setColor(Color.WHITE);
				graphics.fillRect(0, 0, bmWidth, bmWidth);
				graphics.setColor(Color.BLACK);
				for (int i = 0; i < bmWidth; i++) {
					for (int j = 0; j < bmWidth; j++) {
						if (byteMatrix.get(i, j)) {
							graphics.fillRect(i, j, 1, 1);
						}
					}
				}
				ImageIO.write(image, QR_FILE_EXT, f);
				hasDownloadFile = true;
				// Store TOTP QR-Code
				try {
					PersistenceManager pm = this.getPm();
					UserHome userHome = (UserHome)pm.getObjectById(app.getUserHomeIdentityAsPath());
					pm.currentTransaction().begin();
					ByteArrayOutputStream imageContent = new ByteArrayOutputStream();
					ImageIO.write(image, QR_FILE_EXT, imageContent);
					UserHomes.getInstance().createOrUpdateMedia(
						userHome,
						Utils.toFilename(TOTP.class.getSimpleName() + "_" + account + "@" + issuer.replace("/", "_")) + "." + QR_FILE_EXT,
						QR_MIME_TYPE,
						BinaryLargeObjects.valueOf(imageContent.toByteArray())
					);
				} catch(Exception e) {
					try {
						pm.currentTransaction().rollback();
					} catch(Exception ignore) {}
				} finally {
					pm.currentTransaction().commit();
				}
			}
		} catch (Exception e) {
			new ServiceException(e).log();
		}
		if(hasDownloadFile) {
			this.downloadFileAction =
				new Action(
					Action.EVENT_DOWNLOAD_FROM_LOCATION,
					new Action.Parameter[]{
						new Action.Parameter(Action.PARAMETER_LOCATION, this.location),
						new Action.Parameter(Action.PARAMETER_NAME, downloadFileName),
						new Action.Parameter(Action.PARAMETER_MIME_TYPE, mimeType)
					},
					app.getTexts().getClickToDownloadText() + " " + downloadFileName,
					true
				);
		}
	}

	/**
	 * @return the secretKey
	 */
	public String getSecretKey() {
		return secretKey;
	}

	/**
	 * @return the account
	 */
	public String getAccount() {
		return account;
	}

	/**
	 * @return the issuer
	 */
	public String getIssuer() {
		return issuer;
	}

	/**
	 * @return the location
	 */
	public String getLocation() {
		return location;
	}

	/**
	 * @return the downloadFileAction
	 */
	public Action getDownloadFileAction() {
		return downloadFileAction;
	}
	
	//-----------------------------------------------------------------------
	// Members
	//-----------------------------------------------------------------------
	public static final String QR_FILE_EXT = "png";
	public static final String QR_MIME_TYPE = "image/" + QR_FILE_EXT;
	public static final int QR_SIZE = 200;

	private String secretKey;
	private String account;
	private String issuer;
	private String location;
	private Action downloadFileAction;
}
