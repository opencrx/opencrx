/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: AbstractServer
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
package org.opencrx.application.adapter;

import java.io.FileInputStream;
import java.io.IOException;
import java.net.Inet4Address;
import java.net.ServerSocket;
import java.net.Socket;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import javax.jdo.PersistenceManagerFactory;
import javax.net.ServerSocketFactory;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLServerSocket;
import javax.net.ssl.TrustManagerFactory;

import org.openmdx.base.exception.ServiceException;

/**
 * AbstractServer
 */
public abstract class AbstractServer implements Runnable {
    
	/**
	 * Constructor.
	 * @param serverName
	 * @param pmf
	 * @param providerName
	 * @param bindAddress
	 * @param portNumber
	 * @param sslKeystoreFile
	 * @param sslKeystoreType
	 * @param sslKeystorePass
	 * @param sslKeyPass
	 * @param sslTruststoreFile
	 * @param sslTruststorePass
	 * @param sslTruststoreType
	 * @param sslNeedClientAuth
	 * @param isDebug
	 * @param delayOnStartup
	 */
	protected AbstractServer(
		String serverName,
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
		this.serverName = serverName;
	    this.pmf = pmf;
	    this.bindAddress = bindAddress;
		this.portNumber = portNumber;
		this.sslKeystoreFile = sslKeystoreFile;
		this.sslKeystoreType = sslKeystoreType;
		this.sslKeystorePass = sslKeystorePass;
		this.sslKeyPass = sslKeyPass;
		this.sslTruststoreFile = sslTruststoreFile;
		this.sslTruststorePass = sslTruststorePass;
		this.sslTruststoreType = sslTruststoreType;
		this.sslNeedClientAuth = sslNeedClientAuth;
		this.providerName = providerName;
		this.isDebug = isDebug;
		this.delayOnStartup = delayOnStartup;
	}
	
	/**
	 * Create new session for socket for given server.
	 * @param socket
	 * @param server
	 * @return
	 */
	public abstract AbstractSession newSession(
		Socket socket,
		AbstractServer server
	);
	
    /**
     * Bind socket.
     * @return
     * @throws ServiceException
     */
    public boolean bind(
    ) throws ServiceException {
    	boolean isSsl = false;
        ServerSocketFactory serverSocketFactory;
        if(this.sslKeystoreFile == null || this.sslKeystoreFile.isEmpty()) {
            serverSocketFactory = ServerSocketFactory.getDefault();
        } else {
        	isSsl = true;
            FileInputStream keyStoreInputStream = null;
            FileInputStream trustStoreInputStream = null;
            try {
                keyStoreInputStream = new FileInputStream(this.sslKeystoreFile);
                KeyStore keystore = KeyStore.getInstance(this.sslKeystoreType);
                keystore.load(
                	keyStoreInputStream,
                    this.sslKeystorePass.toCharArray()
                );
                KeyManagerFactory kmf = KeyManagerFactory.getInstance(
                	KeyManagerFactory.getDefaultAlgorithm()
                );
                kmf.init(
                	keystore, 
                	this.sslKeyPass.toCharArray()
                );
                SSLContext sslContext = SSLContext.getInstance("SSLv3");
                TrustManagerFactory tmf = null;
                if(this.sslTruststoreFile != null && !this.sslTruststoreFile.isEmpty()) {
                    trustStoreInputStream = new FileInputStream(this.sslTruststoreFile);
                    KeyStore truststore = KeyStore.getInstance(this.sslTruststoreType);
                    truststore.load(
                    	trustStoreInputStream,
                        this.sslTruststorePass.toCharArray()
                    );
                    tmf = TrustManagerFactory.getInstance(
                    	TrustManagerFactory.getDefaultAlgorithm()
                    );
                    tmf.init(truststore);
                }                
                sslContext.init(
                	kmf.getKeyManagers(), 
                	tmf == null ? null : tmf.getTrustManagers(), 
                	null
                );
                serverSocketFactory = sslContext.getServerSocketFactory();
            } catch (IOException e) {
                throw new ServiceException(e);
            } catch (GeneralSecurityException e) {
            	throw new ServiceException(e);
            } finally {
                if (keyStoreInputStream != null) {
                    try {
                        keyStoreInputStream.close();
                    } 
                    catch (IOException exc) {}
                }
            }
        }
        try {
            if(this.bindAddress == null || this.bindAddress.isEmpty()) {
                this.serverSocket = serverSocketFactory.createServerSocket(this.portNumber);
            } else {
                this.serverSocket = serverSocketFactory.createServerSocket(
                	this.portNumber, 
                	0, 
                	Inet4Address.getByName(this.bindAddress)
                );
            }
            if(Boolean.TRUE.equals(this.sslNeedClientAuth)) {
                ((SSLServerSocket)this.serverSocket).setNeedClientAuth(true);
            }
        }
        catch (IOException e) {
        	throw new ServiceException(e);
        }
        return isSsl;
    }

	/* (non-Javadoc)
	 * @see java.lang.Runnable#run()
	 */
    @Override
	public void run(
	) {
		try {
			Thread.sleep(this.delayOnStartup * 1000L);
		} catch(Exception e) {}		
        while(true) {
        	if(this.serverSocket == null) {
        		try {
        			Thread.sleep(5000L);
        		} catch(Exception e) {}
        	}
        	else {
        		try {
	                Socket socket = this.serverSocket.accept();
	                socket.setSoTimeout(30000);
	                AbstractSession session = newSession(socket, this);
	                Thread clientHandler = new Thread(session);
	                clientHandler.start();
	                for(Iterator<Entry<Thread,AbstractSession>> i = this.sessions.entrySet().iterator(); i.hasNext(); ) {
	                	Entry<Thread,AbstractSession> e = i.next();
	                	if(!e.getKey().isAlive()) {
	                		i.remove();
	                	}
	                }
	                this.sessions.put(
	                	clientHandler,
	                	session
	                );
        		} catch(Exception e) {
        			new ServiceException(e).log();
            		try {
            			Thread.sleep(5000L);
            		} catch(Exception e1) {}
        		}
        	}
        }
	}

	/**
	 * Pause session.
	 */
	public void pause(
	) {
		// Close server socket
		try {
			ServerSocket serverSocket = this.serverSocket;
			this.serverSocket = null;
			if(serverSocket != null) {
				serverSocket.close();
			}
		}
		catch(IOException e) {}			
        System.out.println(this.serverName + " " + this.providerName + " stopped listening on port " + this.portNumber);            
		// Stop all sessions
		for(AbstractSession session: this.sessions.values()) {
			session.stop();
		}
		this.sessions.clear();
	}
	
	/**
	 * Resume session.
	 */
	public void resume(
	) {
		boolean isSsl = false;
		try {
			
			if(this.serverSocket == null || this.serverSocket.isClosed()) {
				this.sessions.clear();
				isSsl = this.bind();		
				System.out.println(this.serverName + " " + this.providerName + " is listening on " + (isSsl ? "SSL " : "") + "port " + this.portNumber);
			}
		}
		catch(Exception e) {
			new ServiceException(e).log();
	        System.out.println(this.serverName + " " + this.providerName + " bind failed for " + (isSsl ? "SSL " : "") + "port " + this.portNumber + ". See log for more information.");            			
		}
	}
	
	/**
	 * Get configured provider name.
	 * @return
	 */
	public String getProviderName(
	) {
	    return this.providerName;
	}
	
    /**
     * Return true if debug mode.
     * @return
     */
    public boolean isDebug(
    ) {
        return this.isDebug;
    }
    
	/**
	 * Get persistence manager factory.
	 * @return
	 */
	public PersistenceManagerFactory getPersistenceManagerFactory(
	) {
	    return this.pmf;
	}
	
    //-----------------------------------------------------------------------
	// Members
    //-----------------------------------------------------------------------
	protected final String serverName;
    protected final String bindAddress;
	protected final String sslKeystoreFile;
	protected final String sslKeystoreType;
	protected final String sslKeystorePass;	
	protected final String sslKeyPass;
	protected final String sslTruststoreFile;
	protected final String sslTruststoreType;
	protected final String sslTruststorePass;
	protected final Boolean sslNeedClientAuth;
    protected final int portNumber;
    protected final String providerName;
    protected final boolean isDebug;
    protected final int delayOnStartup;
    protected final Map<Thread,AbstractSession> sessions = new ConcurrentHashMap<Thread,AbstractSession>();
    protected ServerSocket serverSocket = null;
    protected final PersistenceManagerFactory pmf;
    
}
