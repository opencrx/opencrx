package org.opencrx.application.uses.ezvcard.io.xml;

import javax.xml.namespace.QName;

import org.opencrx.application.uses.ezvcard.VCardVersion;

/**
 * Contains the XML element names of some of the standard xCard elements.
 * @author Michael Angstadt
 */
public interface XCardQNames {
	public static final String NAMESPACE = VCardVersion.V4_0.getXmlNamespace();
	public static final QName VCARDS = new QName(NAMESPACE, "vcards");
	public static final QName VCARD = new QName(NAMESPACE, "vcard");
	public static final QName GROUP = new QName(NAMESPACE, "group");
	public static final QName PARAMETERS = new QName(NAMESPACE, "parameters");
}
