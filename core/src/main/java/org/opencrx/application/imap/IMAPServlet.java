package org.opencrx.application.imap;

import javax.jdo.PersistenceManagerFactory;

import org.opencrx.application.adapter.AbstractServer;
import org.opencrx.application.adapter.AbstractServlet;

/**
 * IMAPServlet
 */
public class IMAPServlet extends AbstractServlet {

	/* (non-Javadoc)
	 * @see org.opencrx.application.adapter.AbstractServlet#getConfigurationId()
	 */
	@Override
    public String getConfigurationId(
    ) {
		return "IMAPServlet";
    }

	/* (non-Javadoc)
	 * @see org.opencrx.application.adapter.AbstractServlet#getPortNumber(java.lang.String)
	 */
	@Override
    public int getPortNumber(
    	String configuredPortNumber
    ) {
		return configuredPortNumber == null ? 
			143 : 
				(configuredPortNumber.startsWith("imap:") ? 
					Integer.valueOf(configuredPortNumber.substring(5)) : 
						Integer.valueOf(configuredPortNumber));		
    }

	/* (non-Javadoc)
	 * @see org.opencrx.application.adapter.AbstractServlet#newServer(javax.jdo.PersistenceManagerFactory, java.lang.String, java.lang.String, int, java.lang.String, java.lang.String, java.lang.String, java.lang.String, boolean, int)
	 */
	@Override
    public AbstractServer newServer(
    	PersistenceManagerFactory pmf, 
    	String providerName, 
	    String bindAddress,
	    int portNumber,
	    String sslKeystoreFile,
	    String sslKeystoreType,
	    String sslKeystorePass,
	    String sslKeyPass,
	    String sslTruststoreFile,
	    String sslTruststorePass,
	    String sslTruststoreType,
	    Boolean sslNeedClientAuth,
    	boolean isDebug,
    	int delayOnStartup
    ) {
		return new IMAPServer(
			pmf,
			providerName,
		    bindAddress,
		    portNumber,
		    sslKeystoreFile,
		    sslKeystoreType,
		    sslKeystorePass,
		    sslKeyPass,
		    sslTruststoreFile,
		    sslTruststorePass,
		    sslTruststoreType,
		    sslNeedClientAuth,
			isDebug,
			delayOnStartup
		);
    }

    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    private static final long serialVersionUID = 3271417510604705711L;

}
