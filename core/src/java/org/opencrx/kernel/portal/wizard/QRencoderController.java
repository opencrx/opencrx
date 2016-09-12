/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: QRencoderController
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
package org.opencrx.kernel.portal.wizard;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.Hashtable;

import javax.imageio.ImageIO;

import org.opencrx.kernel.utils.Utils;
import org.openmdx.base.accessor.jmi.cci.RefObject_1_0;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.kernel.id.UUIDs;
import org.openmdx.portal.servlet.AbstractWizardController;
import org.openmdx.portal.servlet.Action;
import org.openmdx.portal.servlet.ApplicationContext;
import org.openmdx.portal.servlet.ObjectReference;

import com.google.zxing.BarcodeFormat;
import com.google.zxing.EncodeHintType;
import com.google.zxing.common.BitMatrix;
import com.google.zxing.qrcode.QRCodeWriter;
import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel;

/**
 * QRencoderController
 *
 */
public class QRencoderController extends AbstractWizardController {

	/**
	 * Constructor.
	 * 
	 */
	public QRencoderController(
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
		@RequestParameter(name = "stringToEncode") String stringToEncode,
		@RequestParameter(name = "location") String location
	) {
		RefObject_1_0 obj = this.getObject();
		ApplicationContext app = this.getApp();
		this.stringToEncode = stringToEncode;
		if(this.stringToEncode == null || this.stringToEncode.isEmpty()) {
			String urlBase = this.getRequest().getRequestURL().toString();
		    try {
		    	urlBase = urlBase.substring(0, urlBase.indexOf(this.getRequest().getServletPath()));
		    } catch (Exception ignore) {}
		    Action selectObjectAction = new ObjectReference(obj, app).getSelectObjectAction();		    
		    this.stringToEncode = urlBase + "/" + selectObjectAction.getEncodedHRef();
		}
		this.location = location;
		if(this.location == null) {
			this.location = UUIDs.getGenerator().next().toString();
		}
	}

	/**
	 * OK action.
	 * 
	 * @param stringToEncode
	 * @param location
	 */
	public void doOK(
		@RequestParameter(name = "stringToEncode") String stringToEncode,
		@RequestParameter(name = "location") String location
	) {
		ApplicationContext app = this.getApp();
		this.doRefresh(
			stringToEncode, 
			location
		);
		boolean hasDownloadFile = false;
		String downloadFileName = null;
		String mimeType = null;
		try {
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
				this.stringToEncode,
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
	 * @return the stringToEncode
	 */
	public String getStringToEncode() {
		return stringToEncode;
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

	private String stringToEncode;
	private String location;
	private Action downloadFileAction;
}
