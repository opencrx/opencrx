/**
 * The ClientHandler class is responsible for replying to client requests.
 */
package org.opencrx.application.imap;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.net.Socket;
import java.net.SocketTimeoutException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.mail.Address;
import javax.mail.BodyPart;
import javax.mail.Flags;
import javax.mail.Folder;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Multipart;
import javax.mail.internet.AddressException;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeMultipart;
import javax.mail.internet.MimePart;
import javax.mail.internet.MimeUtility;
import javax.mail.search.AndTerm;
import javax.mail.search.BodyTerm;
import javax.mail.search.ComparisonTerm;
import javax.mail.search.DateTerm;
import javax.mail.search.FlagTerm;
import javax.mail.search.FromTerm;
import javax.mail.search.HeaderTerm;
import javax.mail.search.NotTerm;
import javax.mail.search.OrTerm;
import javax.mail.search.ReceivedDateTerm;
import javax.mail.search.RecipientTerm;
import javax.mail.search.SearchTerm;
import javax.mail.search.SentDateTerm;
import javax.mail.search.SizeTerm;
import javax.mail.search.SubjectTerm;

import org.opencrx.application.adapter.AbstractServer;
import org.opencrx.application.adapter.AbstractSession;
import org.opencrx.kernel.utils.MimeUtils;
import org.opencrx.kernel.utils.MimeUtils.MimeMessageImpl;
import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.io.QuotaByteArrayOutputStream;
import org.openmdx.base.text.conversion.UUIDConversion;
import org.openmdx.kernel.id.UUIDs;
import org.openmdx.kernel.log.SysLog;
import org.openmdx.kernel.text.format.HexadecimalFormatter;
import org.w3c.format.DateTimeFormat;

/**
 * IMAPSession.
 * 
 */
public class IMAPSession extends AbstractSession {

	/**
	 * SessionState.
	 * 
	 */
	public enum SessionState {
	    NOT_AUTHENTICATED,
	    AUTHENTICATED
	}
	
    /**
     * Constructor.
     * 
     * @param client
     * @param server
     */
    public IMAPSession(
        Socket client, 
        AbstractServer server
    ) {
    	super(
    		client,
    		server
    	);
    }

    /**
     * Get server.
     * 
     * @return
     */
    private IMAPServer getServer(
    ) {
    	return (IMAPServer)this.server;
    }
    
    /**
     * Read line from socket up to MAX_LINE_LENGTH.
     * 
     * @return
     * @throws IOException
     */
    private String readLine(
    ) throws IOException {
        StringBuilder line = new StringBuilder();
        while(true) {
            char c = (char)this.in.read();
            if(c == '\r') {
                c = (char)this.in.read();
            }
            if(c == '\n') {
                break;
            }
            if(c <= 0 || c > 255) {
                return null;
            } else {
	            line.append(c);
	            if(line.length() > MAX_LINE_LENGTH) {            	
	            	SysLog.info("Error: line too long. Details:", Arrays.asList(this.username, line.length(), line.substring(0, 80) + "..."));
	            	// Handle silently. Do not throw exception here.
	            	break;
	            }
            }
        }
        return line.toString();
    }

    /* (non-Javadoc)
     * @see java.lang.Runnable#run()
     */
    @Override
    public void run(
    ) {
        if(this.socket == null || !this.socket.isConnected()) {
            System.out.println("Unable to start conversation, invalid connection passed to client handler");
        } else {
        	SysLog.info("Session started for client", this.socket.getInetAddress().getHostAddress());
            try {
                this.out = new PrintStream(this.socket.getOutputStream());
                this.in = this.socket.getInputStream();
                this.println("* OK [CAPABILTY IMAP4rev1 IDLE] OPENCRX");
                String line = this.readLine();
                Pattern pattern = Pattern.compile("([a-zA-Z0-9]+) ([a-zA-Z0-9]+)(.*)");
                while (line != null) {
                    if((line.indexOf("LOGIN") > 0) && (line.indexOf(" ") > 0)) {
                        System.out.println(">>> IMAPServer[" + this.socket.getInetAddress() + "]\n" + line.substring(0, line.lastIndexOf(" ")));
                    } else if(line.indexOf("LOGOUT") > 0) {
                        System.out.println(">>> IMAPServer[" + this.socket.getInetAddress() + "]\n" + line + " " + this.username);
                    }
                    if(this.getServer().isDebug()) {
                        System.out.println(">>> IMAPServer[" + this.socket.getInetAddress() + "]\n" + line);
                    }
                    Matcher matcher = pattern.matcher(line);
                    if(matcher.find()) {
                        String tag = matcher.group(1);
                        String command = matcher.group(2);
                        String params = matcher.group(3);
                        if(!this.handleCommand(tag, command, params)) {                            
                            this.socket.close();                       
                            return;
                        }
                    }
                    line = this.readLine();
                }
                SysLog.info("connection closed", socket.getInetAddress().getHostAddress());
            } catch (Exception e) {
                if(!(e instanceof SocketTimeoutException)) {
                    ServiceException e0 = new ServiceException(e);
                    SysLog.warning(e0.getMessage(), e0.getCause());
                }
            } finally {                
                try {
                    // Make sure to close connection when thread terminates. 
                    // Otherwise we may have open connections with no listening threads.
                    this.socket.close();
                } catch(Exception e) {}                
            }
        }
    }

    /**
     * SearchTermParser
     */
    static class SearchTermParser {
    	
	    static class Position {
	    	public int value = 0;
	    }
	
	    private static void skipWhitespaces(
	    	String searchString,
	    	Position pos
	    ) {
			while(
				pos.value < searchString.length() && 
				Character.isWhitespace(searchString.charAt(pos.value))
			) {
				pos.value++;
			}    	
	    }
	
	    private static String parseString(
	    	String searchString,
	    	Position pos
	    ) {
	    	skipWhitespaces(
	    		searchString, 
	    		pos
	    	);
	    	String s = "";
	    	if(pos.value < searchString.length()) {
	    		if(searchString.charAt(pos.value) == '"') {
	    			pos.value++;
	    			while(searchString.charAt(pos.value) != '"') {
	    				s += searchString.charAt(pos.value++);
	    			}
	    			pos.value++;
	    		}
	    	}
	    	return s;
	    }
	    
	    private static String parseIdentifier(
	    	String searchString,
	    	Position pos
	    ) {
	    	skipWhitespaces(
	    		searchString, 
	    		pos
	    	);
	    	String s = "";
	    	while(
	    		pos.value < searchString.length() && 
	    		!Character.isWhitespace(searchString.charAt(pos.value))
	    	) {
				s += searchString.charAt(pos.value++);
	    	}
	    	return s;
	    }
	    
	    private static SearchTerm parseSearchTerm(
	    	String query,
	    	Position pos
	    ) throws ParseException, AddressException {
	    	skipWhitespaces(query, pos);
			// At end
			if(pos.value >= query.length()) return null;
			// <sequence set>
	    	if(Character.isDigit(query.charAt(pos.value))) {
	    		/*String number =*/parseIdentifier(query, pos);
	    		return null;
	    	} else if(query.startsWith("ALL", pos.value)) {
		    	// ALL
	    		pos.value += 3;
	        	List<SearchTerm> terms = new ArrayList<SearchTerm>();        	
	        	while(pos.value < query.length()) {
	        		SearchTerm term = parseSearchTerm(
	    				query,
	    				pos
	    			);
	        		if(term != null) {
	        			terms.add(term);
	        		}
	        	}
	        	return new AndTerm(
	        		terms.toArray(new SearchTerm[terms.size()])
	        	);    		
	    	} else if(query.startsWith("ANSWERED", pos.value)) {
		    	// ANSWERED
	    		pos.value += 8;
	    		Flags flags = new Flags();
	    		flags.add(Flags.Flag.ANSWERED);
	    		return new FlagTerm(flags, true);    			
	    	} else if(query.startsWith("BCC", pos.value)) {
		    	// BCC
	    		pos.value += 3;
	    		return new RecipientTerm(
	    			Message.RecipientType.BCC,
	    			new InternetAddress(parseString(query, pos))
	    		);
	    	} else if(query.startsWith("BEFORE", pos.value)) {
		    	// BEFORE
	    		pos.value += 6;
	    		return new ReceivedDateTerm(
	    			DateTerm.LE,
	    			DateTimeFormat.BASIC_UTC_FORMAT.parse(parseIdentifier(query, pos))
	    		);
	    	} else if(query.startsWith("BODY", pos.value)) {
		    	// BODY
	    		pos.value += 4;
	    		return new BodyTerm(parseIdentifier(query, pos));
	    	} else if(query.startsWith("CC", pos.value)) {
		    	// CC
	    		pos.value += 2;
	    		return new RecipientTerm(
	    			Message.RecipientType.CC,
	    			new InternetAddress(parseString(query, pos))
	    		);
	    	} else if(query.startsWith("DELETED", pos.value)) {
		    	// DELETED
	    		pos.value += 7;
	    		Flags flags = new Flags();
	    		flags.add(Flags.Flag.DELETED);
	    		return new FlagTerm(flags, true);    			
	    	} else if(query.startsWith("DRAFT", pos.value)) {
		    	// DRAFT
	    		pos.value += 5;
	    		Flags flags = new Flags();
	    		flags.add(Flags.Flag.DRAFT);
	    		return new FlagTerm(flags, true);    			
	    	} else if(query.startsWith("FLAGGED", pos.value)) {
		    	// FLAGGED
	    		pos.value += 7;
	    		Flags flags = new Flags();
	    		flags.add(Flags.Flag.FLAGGED);
	    		return new FlagTerm(flags, true);    			
	    	} else if(query.startsWith("FROM", pos.value)) {
		    	// FROM
	    		pos.value += 4;
	    		return new FromTerm(
	    			new InternetAddress(parseString(query, pos))
	    		);
	    	} else if(query.startsWith("HEADER", pos.value)) {
		    	// HEADER    	
	    		pos.value += 6;
	    		String headerName = parseIdentifier(query, pos);
	    		String pattern = parseString(query, pos);
	    		return new HeaderTerm(
	    			headerName,
	    			pattern
	    		);
	    	} else if(query.startsWith("KEYWORD", pos.value)) {
		    	// KEYWORD
	    		pos.value += 7;
	    		/*String flag = */parseIdentifier(query, pos);
	    		return null;
	    	} else if(query.startsWith("LARGER", pos.value)) {
		    	// LARGER
	    		pos.value += 6;
	    		String size = parseIdentifier(query, pos);
	    		return new SizeTerm(
	    			ComparisonTerm.GT,
	    			Integer.parseInt(size, 8)
	    		);
	    	} else if(query.startsWith("NEW", pos.value)) {
		    	// NEW
	    		pos.value += 3;
	    		return null;
	    	} else if(query.startsWith("NOT", pos.value)) {
		    	// NOT
	    		pos.value += 3;
	    		return new NotTerm(
	    			parseSearchTerm(query, pos)
	    		);
	    	} else if(query.startsWith("OLD", pos.value)) {
		    	// OLD
	    		pos.value += 3;
	    		Flags flags = new Flags();
	    		flags.add(Flags.Flag.RECENT);
	    		return new FlagTerm(flags, false);    			
	    	} else if(query.startsWith("ON", pos.value)) {
		    	// ON
	    		pos.value += 2;
	    		return new ReceivedDateTerm(
	    			DateTerm.EQ,
	    			DateTimeFormat.BASIC_UTC_FORMAT.parse(parseIdentifier(query, pos))
	    		);
	    	} else if(query.startsWith("OR", pos.value)) {
		    	// OR
	    		pos.value += 2;
	    		List<SearchTerm> terms = new ArrayList<SearchTerm>();
	    		for(int i = 0; i < 2; i++) {
		    		SearchTerm term = parseSearchTerm(
		    			query,
		    			pos
		    		);
		    		if(term != null) {
		    			terms.add(term);
		    		}
	    		}
	        	return new OrTerm(
	        		terms.toArray(new SearchTerm[terms.size()])
	        	);
	    	} else if(query.startsWith("RECENT", pos.value)) {
		    	// RECENT
	    		pos.value += 6;
	    		Flags flags = new Flags();
	    		flags.add(Flags.Flag.RECENT);
	    		return new FlagTerm(flags, true);    			
	    	} else if(query.startsWith("SEEN", pos.value)) {
		    	// SEEN
	    		pos.value += 4;
	    		Flags flags = new Flags();
	    		flags.add(Flags.Flag.SEEN);
	    		return new FlagTerm(flags, true);    			
	    	} else if(query.startsWith("SENTBEFORE", pos.value)) {
		    	// SENTBEFORE
	    		pos.value += 10;
	    		return new SentDateTerm(
	    			DateTerm.LT,
	    			DateTimeFormat.BASIC_UTC_FORMAT.parse(parseIdentifier(query, pos))
	    		);
	    	} else if(query.startsWith("SENTON", pos.value)) {
		    	// SENTON
	    		pos.value += 6;
	    		return new SentDateTerm(
	    			DateTerm.EQ,
	    			DateTimeFormat.BASIC_UTC_FORMAT.parse(parseIdentifier(query, pos))
	    		);
	    	} else if(query.startsWith("SENTSINCE", pos.value)) {
		    	// SENTSINCE
	    		pos.value += 9;
	    		return new SentDateTerm(
	    			DateTerm.GE,
	    			DateTimeFormat.BASIC_UTC_FORMAT.parse(parseIdentifier(query, pos))
	    		);
	    	} else if(query.startsWith("SINCE", pos.value)) {
		    	// SINCE
	    		pos.value += 5;
	    		return new ReceivedDateTerm(
	    			DateTerm.GE,
	    			DateTimeFormat.BASIC_UTC_FORMAT.parse(parseIdentifier(query, pos))
	    		);
	    	} else if(query.startsWith("SMALLER", pos.value)) {
		    	// SMALLER
	    		pos.value += 7;
	    		String size = parseIdentifier(query, pos);
	    		return new SizeTerm(
	    			ComparisonTerm.LT,
	    			Integer.parseInt(size, 8)
	    		);
	    	} else if(query.startsWith("SUBJECT", pos.value)) {
		    	// SUBJECT
	    		pos.value += 7;
	    		String s = parseString(query, pos);    		
	    		return new SubjectTerm(s);
	    	} else if(query.startsWith("TEXT", pos.value)) {
		    	// TEXT
	    		pos.value += 5;
	    		String s = parseString(query, pos);    		
	    		return new BodyTerm(s);
	    	} else if(query.startsWith("TO", pos.value)) {
		    	// TO
	    		pos.value += 2;
	    		return new RecipientTerm(
	    			Message.RecipientType.TO,
	    			new InternetAddress(parseString(query, pos))
	    		);
	    	} else if(query.startsWith("UID", pos.value)) {
		    	// UID
	    		pos.value += 2;
	    		String uid = parseIdentifier(query, pos);
	    		while(uid != null && uid.length() > 0 && Character.isDigit(uid.charAt(0))) {
	        		uid = parseIdentifier(query, pos);
	    		}
	    		return null;
	    	} else if(query.startsWith("UNANSWERED", pos.value)) {
		    	// UNANSWERED
	    		pos.value += 10;
	    		Flags flags = new Flags();
	    		flags.add(Flags.Flag.ANSWERED);
	    		return new FlagTerm(flags, false);    			
	    	} else if(query.startsWith("UNDELETED", pos.value)) {
		    	// UNDELETED
	    		pos.value += 9;
	    		Flags flags = new Flags();
	    		flags.add(Flags.Flag.DELETED);
	    		return new FlagTerm(flags, false);    			
	    	} else if(query.startsWith("UNDRAFT", pos.value)) {
		    	// UNDRAFT
	    		pos.value += 7;
	    		Flags flags = new Flags();
	    		flags.add(Flags.Flag.DRAFT);
	    		return new FlagTerm(flags, false);    			
	    	} else if(query.startsWith("UNFLAGGED", pos.value)) {
		    	// UNFLAGGED
	    		pos.value += 9;
	    		Flags flags = new Flags();
	    		flags.add(Flags.Flag.FLAGGED);
	    		return new FlagTerm(flags, false);    			
	    	} else if(query.startsWith("UNKEYWORD", pos.value)) {
		    	// UNKEYWORD
	    		pos.value += 9;
	    		/*String flag = */parseIdentifier(query, pos);
	    		return null;
	    	} else if(query.startsWith("UNSEEN", pos.value)) {
		    	// UNSEEN
	    		pos.value += 6;
	    		Flags flags = new Flags();
	    		flags.add(Flags.Flag.SEEN);
	    		return new FlagTerm(flags, false);    			
	    	} else {
		    	// Unknown token. Skip.
	    		parseIdentifier(query, pos);
	    		return null;
	    	}
	    }
	    
	    public static SearchTerm parseSearchTerm(
	    	String query
	    ) throws MessagingException, ParseException {
	    	Position position = new Position();
	    	query = query.trim();
	    	if(!query.startsWith("ALL ")) {
	    		query = "ALL " + query;
	    	}
    		return parseSearchTerm(
				query,
				position
			);
	    }
    }
    
    /**
     * Process the command and send a response if appropriate...
     * 
     * @param tag
     * @param command
     * @param params
     * @return
     * @throws MessagingException
     */
    public boolean handleCommand(
        String tag, 
        String command, 
        String params
    ) throws MessagingException {
		command = command.toUpperCase();
		if("CAPABILITY".equals(command)) {
			this.println("* CAPABILITY IMAP4rev1 IDLE");
			this.println(tag + " OK CAPABILITY complete");
		} else if("NOOP".equals(command)) {
			this.println(tag + " OK NOOP completed");
		} else if("LOGOUT".equals(command)) {
			this.println("* BYE IMAP4rev1 Server logging out");
			this.println(tag + " OK LOGOUT complete");
			this.logout();
			try  {
				return false;
			} catch (Exception e) {
				new ServiceException(e).log();
			}
		} else {
			switch(this.state) {
				case NOT_AUTHENTICATED: {
					if("LOGIN".equals(command)) {
						String username = "";
						String password = "";
						try {
							String[] bits = params.split(" ");
							username = bits[1].replace("\"", "");
							password = bits[2].replace("\"", "");
						} catch (Exception e) {
							this.println(tag + " BAD parameters");
							return true;
						}
						if(this.login(username, password)) {
							this.state = SessionState.AUTHENTICATED;
							this.println(tag + " OK User logged in");
							// Default folder is INBOX
							this.selectedFolder = this.getFolder("INBOX");
						}  else {
							this.println(tag + " NO LOGIN failed.");
						}
					} else {
						this.unrecognizedCommand(tag, command);
					}
					break;
				}
				case AUTHENTICATED: {
					if("SELECT".equals(command)) {
						params = params.replace("\"", "");
						params = params.trim().toUpperCase();
						this.selectedFolder = null;
						IMAPFolderImpl folder = this.getFolder(params);
						if(folder != null) {
							this.selectedFolder = folder;
							this.println("* " + folder.getMessageCount() + " EXISTS");
							this.println("* 0 RECENT");
							this.println("* OK [UIDVALIDITY " + folder.getUIDValidity() + "] UID validity status");
							this.println(tag + " OK [" + (folder.getMode() == Folder.READ_ONLY ? "READ-ONLY" : "READ-WRITE") + "] complete");
						}
						if(this.selectedFolder == null) {
						    this.println(tag + " NO SELECT failed, no mailbox with that name");
						}
					} else if ("STATUS".equals(command)) {
						if(params.indexOf(" (") > 0) {
							params = params.substring(0, params.indexOf(" ("));
						}
						params = params.replace("\"", "");
						params = params.trim().toUpperCase();
						IMAPFolderImpl folder = this.getFolder(params);
	                    if(folder != null) {
							this.selectedFolder = folder;
	                        this.println("* " + folder.getMessageCount() + " EXISTS");
							this.println("* 0 RECENT");
							this.println("* OK [UIDVALIDITY " + folder.getUIDValidity() + "] UID validity status");
							this.println(tag + " OK [" + (folder.getMode() == Folder.READ_ONLY ? "READ-ONLY" : "READ-WRITE") + "] complete");
						} else {
							this.println(tag + " NO STATUS failed, no mailbox with that name");
						}
					} else if("EXAMINE".equals(command)) {
						params = params.replace("\"", "");
						params = params.trim().toUpperCase();
	                    IMAPFolderImpl folder = this.getFolder(params);
	                    if(folder != null) {
							this.selectedFolder = folder;
	                        this.println("* " + folder.getMessageCount() + " EXISTS");
	                        this.println("* 0 RECENT");
	                        this.println("* OK [UIDVALIDITY " + folder.getUIDValidity() + "] UID validity status");
	                        this.println(tag + " OK [" + (folder.getMode() == Folder.READ_ONLY ? "READ-ONLY" : "READ-WRITE") + "] complete");
	                    } else {
							this.println(tag + " NO EXAMINE failed, no mailbox with that name");
						}
					} else if("CREATE".equals(command)) {
						this.println(tag + " NO command not supported");
					} else if("DELETE".equals(command)) {
						this.println(tag + " NO command not supported");
					} else if("RENAME".equals(command)) {
						this.println(tag + " NO command not supported");
					} else if("LIST".equals(command)) {
	                    Pattern pattern = Pattern.compile(" \"([a-zA-Z0-9]*)\" \"([a-zA-Z0-9*%]*)\"");
	                    Matcher matcher = pattern.matcher(params);
	                    if (matcher.find()) {
	                        String folderName = matcher.group(1);
	                        String query = matcher.group(2);
	                        if("".equals(folderName)) {
	                        	Map<String,String> availableFolders = this.getServer().getAvailableFolders(this.segmentName);
	                            for(String folder: availableFolders.keySet()) {
	                                if(
	                                    folder.equals(query) ||
	                                    ((query.length() == 0) || (query.endsWith("*") || query.endsWith("%")) && folder.startsWith(query.substring(0, query.length()-1)))
	                                ) {
	                                    this.println("* LIST () \"/\" \"" + folder + "\"");
	                                }
	    						}
	                        }
						}
	                    this.println(tag + " OK LIST complete");
					} else if("LSUB".equals(command)) {
	                    for(Folder folder: this.getSubscribedFolders()) {
	                    	println("* LSUB () \"/\" \"" + folder.getFullName() + "\"");
						}
						this.println(tag + " OK LSUB complete");
					} else if("SUBSCRIBE".equals(command)) {
	                    params = params.replace("\"", "");
	                    params = params.trim();
	                	Map<String,String> availableFolders = this.getServer().getAvailableFolders(this.segmentName);
	                    if(availableFolders.containsKey(params)) {
	                        this.subscribeFolder(
	                            params, 
	                            availableFolders
	                        );                     
	                        this.println(tag + " OK SUBSCRIBE complete");                    
	                    } else {
	                        this.println(tag + " NO SUBSCRIBE invalid folder name");                                            
	                    }
					} else if("UNSUBSCRIBE".equals(command)) {
	                    params = params.replace("\"", "");
	                    params = params.trim();
	                    List<IMAPFolderImpl> subscribedFolders = this.getSubscribedFolders();
	                    boolean found = false;
	                    for(IMAPFolderImpl folder: subscribedFolders) {
	                        if(folder.getFullName().equals(params)) {
	                            this.unsubscribeFolder(
	                                params
	                            );
	                            found = true;
	                            break;
	                        }
	                    }
	                    if(found) {
	                        this.println(tag + " OK UNSUBSCRIBE complete");
	                    } else {
	                        this.println(tag + " NO UNSUBSCRIBE invalid folder name");                        
	                    }
					} else if("APPEND".equals(command)) {
						boolean hasDate = true;
					    Pattern pattern = Pattern.compile(" \"(.*)\"(?: \\((.*)\\))? \"(.*)\" \\{([0-9]+)\\}");
					    Matcher matcher = pattern.matcher(params);
					    boolean matches = matcher.find();
					    // Date is optional, e.g. KMail
					    if(!matches) {
					    	hasDate = false;
					    	pattern = Pattern.compile(" \"(.*)\"(?: \\((.*)\\))? \\{([0-9]+)\\}");
					    	matcher = pattern.matcher(params);
					    	matches = matcher.find();
					    }
					    if(matches) {
					        try {
	    				        int size = Integer.valueOf(matcher.group(hasDate ? 4 : 3));
	    				        IMAPFolderImpl folder = this.getFolder(matcher.group(1));
	    				        String date = hasDate ? matcher.group(3) : null;
	    				        if(folder != null) {
	        	                    this.println("+ OK");
	        	                    if(this.getServer().isDebug()) {
	        	                        System.out.println("Reading " + size + " bytes");
	        	                    }
	        	                    byte[] msg = new byte[size];
	        	                    boolean success = true;
	        	                    int i = 0;
	        	                    for(i = 0; i < size; i++) {
	        	                        if(this.getServer().isDebug() && (i > 0) && (i % 1000 == 0)) {
	        	                            System.out.println(i + " bytes");
	        	                        }
	        	                        int c = this.in.read();
	        	                        if(c < 0) {
	        	                        	success = false;
	        	                        	break;
	        	                        }
	        	                        msg[i] = (byte)c;
	        	                    }
	        	                    if(this.getServer().isDebug()) {
	        	                        System.out.println(new String(msg, "iso-8859-1"));
	        	                        System.out.println();
	        	                        System.out.println(new HexadecimalFormatter(msg, 0, size).toString());
	                                    System.out.println();        	                        
	        	                        System.out.flush();
	        	                    }
	        	                    // Import if upload is successful
	        	                    if(success) {
	                                    Message message = new MimeUtils.MimeMessageImpl(
	                                        new ByteArrayInputStream(msg)
	                                    );
	                                    if(date != null) {
	                                    	message.setHeader("Date", date);
	                                    }
	                                    folder.appendMessages(new Message[]{message});
	                                    this.println(tag + " OK APPEND complete");        	                        
	        	                    } else {
	                                    this.println(tag + " NO invalid message");                                  	                        
	        	                    }
	    				        } else {
	                                this.println(tag + " NO folder not found");                                				            
	    				        }
					        } catch(Exception e) {
			                    this.println(tag + " NO invalid message");				            
					        }
					    } else {
					        this.println(tag + " NO invalid parameter");
					    }
					} else if("CHECK".equals(command)) {
						this.println(tag + " OK CHECK complete");
					} else if("CLOSE".equals(command)) {
						this.println(tag + " OK CLOSE complete");
						this.state = SessionState.AUTHENTICATED;
					} else if("EXPUNGE".equals(command)) {
						this.println(tag + " NO EXPUNGE command not supported");
					} else if("SEARCH".equals(command)) {
					    SearchTerm searchTerm = null;
					    try {
					    	searchTerm = SearchTermParser.parseSearchTerm(
					    		params.substring(6)
						    );
					    } catch(Exception e) {}
					    Message[] messages = this.selectedFolder.search(searchTerm);
					    String ids = "";
					    for(Message message: messages) {
                            if(message instanceof MimeMessage) {
                            	ids += " " + message.getMessageNumber();
                            }
					    }
                        this.println("* SEARCH" + ids);
                        this.println(tag + " OK SEARCH completed");
					} else if("IDLE".equals(command)) {
						long timeoutAtMillis = System.currentTimeMillis() + 30*60*1000L;
						boolean idling = false;
						int oldCount = this.selectedFolder.getMessageCount();
						this.println("+ idling");						
						idling: while((idling = (System.currentTimeMillis() < timeoutAtMillis))) {
							// Sleep for 10s and wait for DONE
							try {
								for(int i = 0; i < 20; i++) {
									String l = null;
									try {
										if(this.in.available() > 0) {
											l = this.readLine();
										}
									} catch(Exception e) {}
									if("DONE".equalsIgnoreCase(l)) {
										this.println(tag + " OK IDLE terminated");
										break idling;
									}									
									Thread.sleep(500);
								}
							} catch(Exception e) {}
							int count = this.selectedFolder.getMessageCount();							
							if(count != oldCount) {
								if(count < oldCount) {
									this.println("* "+ Integer.toString(oldCount - count) + " EXPUNGE");
								}
								this.println("* "+ Integer.toString(count) + " EXISTS");
								oldCount = count;
							}
						}
						// Timeout --> LOGOUT
						if(!idling) {
							this.println("* BYE IMAP4rev1 Server logging out");
							this.println(tag + " OK LOGOUT complete");
							this.logout();
							try  {
								return false;
							} catch (Exception e) {
								new ServiceException(e).log();
							}
						}
					} else if("FETCH".equals(command)) {
                        // FETCH n1,n2, ...
                        int posCommands = params.indexOf("(");
					    StringTokenizer p = new StringTokenizer(params.substring(0, posCommands), " ", false);
					    if(p.countTokens() == 1) {
                            List<int[]> messageNumbers = new ArrayList<int[]>();
                            StringTokenizer g = new StringTokenizer(p.nextToken(), ",", false);
                            while(g.hasMoreTokens()) {
                                try {
                                    String number = g.nextToken();
                                    if(number != null) {
                                        int start = -1;
                                        int end = -1;
                                        if(number.indexOf(":") > 0) {
                                            String startAsString = number.substring(0, number.indexOf(":"));
                                            start = "*".equals(startAsString) ? 0 : Integer.valueOf(startAsString);
                                            String endAsString = number.substring(number.indexOf(":") + 1);        
                                            end = "*".equals(endAsString) ? 0 : Integer.valueOf(endAsString);
                                        } else {
                                            start = Integer.valueOf(number);
                                            end = start;
                                        }
                                        messageNumbers.add(new int[]{start,end});
                                    }
                                } catch(Exception e) {}                                    
                            }                                
                            String fetchParams = params.substring(posCommands);   
                            for(int[] messageNumber: messageNumbers) {
                                for(Message message: this.selectedFolder.getMessages(messageNumber[0], messageNumber[1])) {
                                    if(message instanceof MimeMessage) {
                                        List<String> commands = this.getFetchCommands(fetchParams);
                                        this.print("* " + message.getMessageNumber() + " FETCH (");
                                        int n = 0;
                                        for(String cmd: commands) {
                                            boolean success = this.processFetchCommand(
                                                cmd, 
                                                (MimeMessage)message, 
                                                n
                                            );
                                            if(success) n++;
                                        }
                                        this.println(")");
                                    }
                                }
                            }
                            this.println(tag + " OK FETCH complete");
                        }
					} else if("STORE".equals(command)) {
						this.println(tag + " NO STORE command not supported");
					} else if("COPY".equals(command)) {
						this.println(tag + " NO COPY command not supported");
					} else if("UID".equals(command)) {
					    String uidCommand = params.trim().toUpperCase();
						if(uidCommand.startsWith("FETCH")) {
						    int posCommands = params.indexOf("(");
						    StringTokenizer p = new StringTokenizer(params.substring(0, posCommands), " ", false);
						    if(p.countTokens() == 2) {
                                List<long[]> messageUids = new ArrayList<long[]>();
                                p.nextToken();
                                StringTokenizer g = new StringTokenizer(p.nextToken(), ",", false);
                                while(g.hasMoreTokens()) {
                                    try {
                                        String uid = g.nextToken();
                                        if(uid != null) {
                                            long start = -1;
                                            long end = -1;
                                            if(uid.indexOf(":") > 0) {
                                                String startAsString = uid.substring(0, uid.indexOf(":"));
                                                start = "*".equals(startAsString) ? 0L : Long.valueOf(startAsString);
                                                String endAsString = uid.substring(uid.indexOf(":") + 1);                        
                                                end = "*".equals(endAsString) ? MAX_ACTIVITY_NUMBER : Long.valueOf(endAsString);
                                            } else {
                                                start = Long.valueOf(uid);
                                                end = start;
                                            }
                                            messageUids.add(new long[]{start,end});
                                        }
                                    } catch(Exception e) {}                                    
                                }                                
                                String fetchParams = params.substring(posCommands);
                                for(long[] messageUid: messageUids) {
                                    for(Message message: this.selectedFolder.getMessagesByUID(messageUid[0], messageUid[1])) {                                    
                                        if(message instanceof MimeMessage) {
                                            List<String> commands = this.getFetchCommands(fetchParams);
                                            if(!commands.contains("UID")) {
                                                commands.add(0, "UID");
                                            }
                                            this.print("* " + message.getMessageNumber() + " FETCH (");
                                            int n = 0;
                                            for(String cmd: commands) {
                                                boolean success = this.processFetchCommand(
                                                    cmd, 
                                                    (MimeMessage)message, 
                                                    n
                                                );
                                                if(success) n++;
                                            }
                                            this.println(")");
                                        }
                                    }
                                }
                                this.println(tag + " OK UID complete");
                            }
						} else if(uidCommand.startsWith("SEARCH")) {
						    SearchTerm searchTerm = null;
						    try {
						    	searchTerm = SearchTermParser.parseSearchTerm(
						    		uidCommand.substring(6)
							    );
						    } catch(Exception e) {}
						    Message[] messages = this.selectedFolder.search(searchTerm);
						    String ids = "";
						    for(Message message: messages) {
	                            if(message instanceof MimeMessage) {
	                            	ids += " " + message.getMessageNumber();
	                            }
						    }
	                        this.println("* SEARCH" + ids);
	                        this.println(tag + " OK SEARCH completed");
						} else if("COPY".equals(uidCommand)) {
	                        Pattern pattern = Pattern.compile(" (?:COPY|copy) ([0-9*\\:]+)(?:,([0-9*\\:]+))* \"(.*)\"");
	                        Matcher matcher = pattern.matcher(params);
	                        if(matcher.find()) {
	                            IMAPFolderImpl folder = this.getFolder(matcher.group(matcher.groupCount()));
                                if(folder != null) {
                                    List<long[]> messageUids = new ArrayList<long[]>();
                                    for(int i = 1; i < matcher.groupCount(); i++) {
                                        try {
                                            String uid = matcher.group(i);
                                            if(uid != null) {
                                                long start = -1;
                                                long end = -1;
                                                if(uid.indexOf(":") > 0) {
                                                    String startAsString = uid.substring(0, uid.indexOf(":"));
                                                    start = "*".equals(startAsString) ? 0L : Long.valueOf(startAsString);
                                                    String endAsString = uid.substring(uid.indexOf(":") + 1);                        
                                                    end = "*".equals(endAsString) ? MAX_ACTIVITY_NUMBER : Long.valueOf(endAsString);
                                                } else {
                                                    start = Long.valueOf(uid);
                                                    end = start;
                                                }
                                                messageUids.add(new long[]{start,end});
                                            }
                                        } catch(Exception e) {}                                    
                                    }                                
                                    for(long[] messageUid: messageUids) {
                                        List<MimeMessage> newMessages = new ArrayList<MimeMessage>(); 
                                        for(Message message: this.selectedFolder.getMessagesByUID(messageUid[0], messageUid[1])) {
                                            if(message instanceof MimeMessage) {
                                                newMessages.add(new MimeMessageImpl((MimeMessage)message));
                                            }
                                        }
                                        folder.appendMessages(
                                            newMessages.toArray(new MimeMessage[newMessages.size()])
                                        );
                                    }
                                }
	                        }
                            this.println(tag + " OK UID complete");
	                    } else {
	                        this.println(tag + " NO UID" + params + " command not supported");                     
	                    }
					} else {
						this.unrecognizedCommand(tag, command);
					}
				}
			}
		}
		return true;
	}

    /**
     * Unrecognized command.
     * 
     * @param tag
     * @param command
     */
    public void unrecognizedCommand(
        String tag, 
        String command
    ) {
    	SysLog.detail("Could not", tag + " " + command);
        this.println(tag + " BAD " + command);
    }

    /**
     * Parse fetch commands.
     * 
     * @param fetchParams
     * @return
     */
    public List<String> getFetchCommands(
        String fetchParams
    ) {
        List<String> fetchCommands = new ArrayList<String>();
        Pattern pattern = Pattern.compile("[a-zA-Z0-9\\.]+(\\[\\])?(\\[[a-zA-Z0-9\\.()\\- ]+\\])?");
        Matcher matcher = pattern.matcher(fetchParams);
        while(matcher.find()) {
            String newCommand = matcher.group(0);
            if(!newCommand.startsWith("BODY")) {
                fetchCommands.add(0, newCommand);
            } else {
                fetchCommands.add(newCommand);
            }
            fetchParams = fetchParams.substring(newCommand.length()).trim();
            matcher = pattern.matcher(fetchParams);
        }
        return fetchCommands;
    }

    /**
     * Get mime body part as RFC822 stream.
     * 
     * @param part
     * @param ignoreHeaders
     * @param out
     */
    protected void getBodyAsRFC822(
        MimePart part,
        boolean ignoreHeaders,
        QuotaByteArrayOutputStream out
    ) {
        try {
            out.reset();
            if(ignoreHeaders) {
                OutputStream os = MimeUtility.encode(out, part.getEncoding());
                part.getDataHandler().writeTo(os);
                os.flush();           
            } else {
                part.writeTo(out);
                out.close();
            }
        } catch (Exception e) {
            new ServiceException(e).log();
        }
    }
    
    /**
     * Get body length.
     * 
     * @param part
     * @param ignoreHeaders
     * @return
     */
    protected int getBodyLength(
        MimePart part,
        boolean ignoreHeaders
    ) {
        try {
            QuotaByteArrayOutputStream bos = byteOutputStreams.get();
            bos.reset();
            if(ignoreHeaders) {
                OutputStream os = MimeUtility.encode(bos, part.getEncoding());
                part.getDataHandler().writeTo(os);
                os.flush();           
            } else {
                part.writeTo(bos);
                bos.close();
            }
            return bos.size();
        } catch (Exception e) {
            new ServiceException(e).log();
        }
        return 0;
    }
    
    /**
     * Get message flags.
     * 
     * @param message
     * @return
     * @throws MessagingException
     */
    protected String getMessageFlags(
        Message message
    ) throws MessagingException {
        return "\\Seen";
    }
    
    /**
     * Process fetch command.
     * 
     * @param params
     * @param message
     * @param count
     * @return
     * @throws MessagingException
     */
    public boolean processFetchCommand(
        String params, 
        MimeMessage message,
        int count
    ) throws MessagingException {
        String command = params;
        if (params.indexOf(" ") != -1) {
            command = params.substring(0, params.indexOf(" "));
            params = params.substring(params.indexOf(" ")).trim();
        }
        command = command.toUpperCase();
        if(command.equals("FLAGS")) {
            if(count > 0) this.print(" ");
            this.print("FLAGS (" + this.getMessageFlags(message) + ")");
            return true;
        } else if(command.equals("RFC822")) {
            if(count > 0) this.print(" ");
            QuotaByteArrayOutputStream messageRFC822 = byteOutputStreams.get(); 
            this.getBodyAsRFC822(message, false, messageRFC822);
            this.println("RFC822 {" + (messageRFC822.size() + 2) + "}");
            this.printBytes(messageRFC822);
            this.println("");
            return true;
        } else if(command.equals("RFC822.HEADER")) {
            String temp = MimeUtils.getHeadersAsRFC822(
                message, 
                MimeUtils.STANDARD_HEADER_FIELDS
            );
            if(count > 0) this.print(" ");
            this.println("RFC822 {" + (temp.length() + 2) + "}");
            this.print(temp);
            return true;
        } else if(command.equals("RFC822.SIZE")) {
            if(count > 0) this.print(" ");
            int length = this.getBodyLength(message, false);            
            this.print("RFC822.SIZE " + length);
            return true;
        } else if(command.equals("UID")) {
            if(count > 0) this.print(" ");
            this.print("UID " + this.selectedFolder.getUID(message));
            return true;
        } else if(command.equals("BODY[]") || command.equals("BODY.PEEK[]")) {
            if(count > 0) this.print(" ");
            QuotaByteArrayOutputStream messageRFC822 = byteOutputStreams.get(); 
            this.getBodyAsRFC822(message, false, messageRFC822);
            this.println("BODY[] {" + (messageRFC822.size() + 2) + "}");
            this.printBytes(messageRFC822);
            this.println("");
            return true;
        } else if(command.equals("BODY[HEADER]")) {
            String temp = MimeUtils.getHeadersAsRFC822(
                message, 
                MimeUtils.STANDARD_HEADER_FIELDS
            );
            if(count > 0) this.print(" ");
            this.println("BODY[HEADER] {" + (temp.length() + 2) + "}");
            this.print(temp);
            return true;
        } else if(command.matches("BODY\\.PEEK\\[([0-9]+)(?:\\.MIME)?\\]")) {
            try {
                if(message.getContent() instanceof MimeMultipart) {
                    Pattern pattern = Pattern.compile("BODY\\.PEEK\\[([0-9]+)(?:\\.MIME)?\\]");
                    Matcher matcher = pattern.matcher(command);
                    if(matcher.find()) {
                        int partId = Integer.valueOf(matcher.group(1)).intValue();
                        if(command.indexOf(".MIME") > 0) {
                            String temp = MimeUtils.getHeadersAsRFC822(
                                ((MimeMultipart)message.getContent()).getBodyPart(partId - 1), 
                                new String[]{"Content-Type", "Content-Disposition", "Content-Transfer-Encoding"}
                            );                                                
                            if(count > 0) this.print(" ");
                            this.println("BODY[" + partId + ".MIME] {" + (temp.length() + 2) + "}");
                            this.println(temp);
                            return true;
                        } else {
                            QuotaByteArrayOutputStream temp = byteOutputStreams.get();                         	
                            this.getBodyAsRFC822(
                                (MimePart)((MimeMultipart)message.getContent()).getBodyPart(partId - 1),
                                true,
                                temp
                            );
                            if(count > 0) this.print(" ");
                            this.println("BODY[" + partId + "] {" + (temp.size() + 2) + "}");
                            this.printBytes(temp);
                            this.println("");
                            return true;
                        }
                    }
                }
            } catch(IOException e) {
                throw new MessagingException(e.getMessage());
            }
        } else if(command.equals("BODY[TEXT]")) {
            if(count > 0) this.print(" ");
            QuotaByteArrayOutputStream messageRFC822 = byteOutputStreams.get();             
            this.getBodyAsRFC822(message, false, messageRFC822);
            this.println("BODY[TEXT] {" + (messageRFC822.size() + 2) + "}");
            this.printBytes(messageRFC822);
            this.println("");
            return true;
        } else if(command.equals("BODY[HEADER.FIELDS")) {
            params = params.replace("]", "");
            params = params.replace("(", "");
            params = params.replace(")", "");
            params = params.replace("\"", "");
            if(count > 0) this.print(" ");
            this.print("BODY[HEADER.FIELDS (");
            String[] fields = params.split(" ");
            int n = 0;
            for (int i = 0; i < fields.length; i++) {
                if(n > 0) this.print(" ");
                this.print("\"" + fields[i].toUpperCase() + "\"");
                n++;
            }
            this.print(")] ");
            String temp = MimeUtils.getHeadersAsRFC822(
                message, 
                fields
            );
            this.println("{" + (temp.length() + 2) + "}");
            this.println(temp);
            return true;
        } else if(command.equals("BODY[HEADER.FIELDS.NOT")) {
            params = params.replace("]", "");
            params = params.replace("(", "");
            params = params.replace(")", "");
            params = params.replace("\"", "");
            String[] fields = params.split(" ");
            String temp = MimeUtils.getHeadersAsRFC822(
                message, 
                fields
            );
            if(count > 0) this.print(" ");
            this.println("BODY[HEADER.FIELDS.NOT" + params + " {" + (temp.length() + 2) + "}");
            this.println(temp);
            return true;
        } else if(command.equals("BODY.PEEK[TEXT]")) {
            if(count > 0) this.print(" ");
            QuotaByteArrayOutputStream messageRFC822 = byteOutputStreams.get();             
            this.getBodyAsRFC822(message, false, messageRFC822);
            this.println("BODY[TEXT] {" + (messageRFC822.size() + 2) + "}");
            this.printBytes(messageRFC822);
            this.println("");
            return true;
        } else if(command.equals("BODY.PEEK[HEADER]")) {
            String temp = MimeUtils.getHeadersAsRFC822(
                message, 
                MimeUtils.STANDARD_HEADER_FIELDS
            );
            if(count > 0) this.print(" ");
            this.println("BODY[HEADER] {" + (temp.length() + 2) + "}");
            this.println(temp);
            return true;
        } else if(command.equals("BODY.PEEK[HEADER.FIELDS")) {
            params = params.replace("]", "");
            params = params.replace("(", "");
            params = params.replace(")", "");
            params = params.replace("\"", "");
            if(count > 0) this.print(" ");
            this.print("BODY[HEADER.FIELDS (");
            String[] fields = params.split(" ");
            int n = 0;
            for(int i = 0; i < fields.length; i++) {
                if(n > 0) this.print(" ");
                this.print("\"" + fields[i].toUpperCase() + "\"");
                n++;
            }
            this.print(")] ");
            String temp = MimeUtils.getHeadersAsRFC822(
                message, 
                fields
            );
            this.println("{" + (temp.length() + 2) + "}");
            this.println(temp);
            return true;
        } else if(command.equals("BODY.PEEK[HEADER.FIELDS.NOT")) {
            params = params.replace("]", "");
            params = params.replace("(", "");
            params = params.replace(")", "");
            params = params.replace("\"", "");
            String[] fields = params.split(" ");
            String temp = MimeUtils.getHeadersAsRFC822(
                message, 
                fields
            );
            if(count > 0) this.print(" ");
            this.println("BODY[HEADER.FIELDS.NOT" + params + " {" + (temp.length() + 2) + "}");
            this.println(temp);
            return true;
        } else if(command.equals("INTERNALDATE")) {
            if(count > 0) this.print(" ");
            this.print("INTERNALDATE ");
            this.printList(message.getHeader("Date"), true);
            return true;
        } else if(command.equals("ENVELOPE")) {
            if(count > 0) this.print(" ");
            this.print("ENVELOPE (");
            this.printList(message.getHeader("Date"), true);
            this.print(" ");
            String[] subjects = message.getHeader("Subject");
            if(subjects != null && subjects.length > 0) {
                String subject = subjects[0].replace("\r\n", " ");
                subject = subject.replace("\r", " ");
                subject = subject.replace("\n", " ");
                this.printList(new String[]{subject}, true);
            }
            try {
                this.printAddresses(message.getFrom(), 1);
            } catch(Exception e) {
                this.printAddresses(null, 1);
            }
            try {
                this.printAddresses(message.getFrom(), 1);
            } catch(Exception e) {
                this.printAddresses(null, 1);
            }
            try {
                this.printAddresses(message.getFrom(), 1);
            } catch(Exception e) {
                this.printAddresses(null, 1);
            }
            try {
                this.printAddresses(message.getAllRecipients(), 1);                
            } catch(Exception e) {
                this.printAddresses(null, 1);
            }            
            this.print(" NIL NIL NIL ");
            this.printList(message.getHeader("Message-Id"), true);
            this.print(")");
            return true;
        } else if(command.equals("BODYSTRUCTURE")) {
            if(count > 0) this.print(" ");
            this.print("BODYSTRUCTURE ");
            this.printMessageStructure(message);
            return true;
        } 
        return false;
    }

    /**
     * Update subscriptions.
     * 
     * @param folders
     */
    private void amendSubscriptions(
        List<IMAPFolderImpl> folders            
    ) {
        try {
            File mailDir = IMAPFolderImpl.getMailDir(this.username);
            mailDir.mkdirs();        
            File subscriptionsFile = new File(mailDir, ".SUBSCRIPTIONS-" + this.getServer().getProviderName());
            if(!subscriptionsFile.exists()) {
                PrintStream ps = new PrintStream(subscriptionsFile);
                ps.println();
                ps.close();
            }
            PrintStream ps = new PrintStream(subscriptionsFile);
            for(IMAPFolderImpl f: folders) {
                ps.println(f.getFullName());
            }
            ps.close();                
        } catch(Exception e) {
            new ServiceException(e).log();
        }       
    }
    
    /**
     * Unsubscribe folder.
     * 
     * @param name
     * @throws MessagingException
     */
    private void unsubscribeFolder(
        String name
    ) throws MessagingException {
        List<IMAPFolderImpl> folders = this.getSubscribedFolders();
        for(Iterator<IMAPFolderImpl> i = folders.iterator(); i.hasNext(); ) {
            IMAPFolderImpl folder = i.next();
            if(folder.getFullName().equals(name)) {
                // Mark folder as unsubscribed (can be deleted)
                if(!"INBOX".equalsIgnoreCase(name)) {
                    try {
                        File dest = new File(
                            folder.folderDir.getParentFile(),
                            folder.folderDir.getName() + "-" + UUIDConversion.toUID(UUIDs.newUUID()) + ".UNSUBSCRIBE"
                        );
                        folder.folderDir.renameTo(dest);
                    } catch(Exception e) {
                        new ServiceException(e).log();
                    }                    
                }
                i.remove();
                break;
            }
        }
        this.amendSubscriptions(folders);
    }
    
    /**
     * Subscribe folder.
     * 
     * @param name
     * @param availableFolders
     * @throws MessagingException
     */
    private void subscribeFolder(
        String name,
        Map<String,String> availableFolders
    ) throws MessagingException {
        List<IMAPFolderImpl> folders = this.getSubscribedFolders();
        // Check whether already subscribed
        for(IMAPFolderImpl folder: folders) {
            if(name.equals(folder.getFullName())) {
                return;
            }
        }
        // Add folder
        IMAPFolderImpl folder = new IMAPFolderImpl(
            name,
            availableFolders.get(name),
            this.username,
            this.getServer().getPersistenceManagerFactory()
        );
        folders.add(folder);
        this.amendSubscriptions(folders);
        folder.synchronizeMailDir();
    }
    
    /**
     * Get subscribed folders.
     * 
     * @return
     * @throws MessagingException
     */
    private List<IMAPFolderImpl> getSubscribedFolders(
    ) throws MessagingException {   
        List<IMAPFolderImpl> folders = new ArrayList<IMAPFolderImpl>();        
        try {
            File mailDir = IMAPFolderImpl.getMailDir(this.username);
            mailDir.mkdirs();
            File subscriptionsFile = new File(mailDir, ".SUBSCRIPTIONS-" + this.getServer().getProviderName());
            if(!subscriptionsFile.exists()) {
                subscriptionsFile = new File(mailDir, ".SUBSCRIPTIONS");            	
            }
            folders.add(
                new IMAPFolderImpl(
                    "INBOX",
                    "INBOX",
                    this.username,
                    this.getServer().getPersistenceManagerFactory()
                )
            );
            if(subscriptionsFile.exists()) {
            	Map<String,String> availableFolders = this.getServer().getAvailableFolders(this.segmentName);
                BufferedReader reader = new BufferedReader(
                    new InputStreamReader(
                        new FileInputStream(subscriptionsFile)
                    )
                );
                while(reader.ready()) {
                    String name = reader.readLine();
                    if(availableFolders.containsKey(name)) {
                        try {
                            IMAPFolderImpl folder = new IMAPFolderImpl(
                                name,
                                availableFolders.get(name),
                                this.username,
                                this.getServer().getPersistenceManagerFactory()
                            );                        
                            folders.add(folder);
                        }
                        catch(Exception e) {}
                    }
                }
                reader.close();
            }
        } catch(Exception e) {
            new ServiceException(e).log();
        }
        return folders;
    }
                
    /**
     * Get folder.
     * 
     * @param name
     * @return
     * @throws MessagingException
     */
    private IMAPFolderImpl getFolder(
        String name
    ) throws MessagingException {
    	if("INBOX".equals(name)) {
            return new IMAPFolderImpl(
                "INBOX",
                "INBOX",
                this.username,
                this.getServer().getPersistenceManagerFactory()
            );    		
    	} else {
	        for(Map.Entry<String,String> entry: this.getServer().getAvailableFolders(this.segmentName).entrySet()) {
	        	String folderName = entry.getKey();
	        	String folderId = entry.getValue();
	            if(
	                folderName.equalsIgnoreCase(name) || 
	                // Required for some Outlook versions
	                folderName.replace("/", "").equalsIgnoreCase(name)
	            ) {
	                return new IMAPFolderImpl(
	                    name,
	                    folderId,
	                    this.username,
	                    this.getServer().getPersistenceManagerFactory()
	                );                                    	
	            }
	        }
    	}
        return null;
    }
    
    /**
     * Output string to response stream.
     * 
     * @param s
     */
    protected void println(
        String s
    ) {      
        try {
            if(this.getServer().isDebug()) {
                System.out.println(s);
                System.out.flush();
            }
            this.out.write(s.getBytes("US-ASCII"));
            this.out.write("\r\n".getBytes("US-ASCII"));
            this.out.flush();
        } catch (Exception e) {
            new ServiceException(e).log();
        }
    }

    /**
     * Output string to response stream.
     * 
     * @param s
     */
    protected void print(
        String s
    ) {
        try {        	
            if(this.getServer().isDebug()) {
                System.out.print(s);
                System.out.flush();
            }
            this.out.write(s.getBytes("US-ASCII"));
            this.out.flush();
        } catch (Exception e) {
            new ServiceException(e).log();
        }
    }

    /**
     * Print bytes to response stream.
     * 
     * @param bytes
     */
    protected void printBytes(
        QuotaByteArrayOutputStream bytes
    ) {
        try {
            if(this.getServer().isDebug()) {
                System.out.print(new String(bytes.getBuffer(), 0, bytes.size(), "UTF-8"));
                System.out.flush();
            }
            bytes.writeTo(this.out);
            this.out.flush();
        } catch (Exception e) {
            new ServiceException(e).log();
        }
    }

    /**
     * Print string list to response stream.
     * @param values
     * @param nested
     */
    protected void printList(
        String[] values,
        boolean nested
    ) {
        if(values == null) {
            this.print("NIL");
            return;
        }
        if(nested && values.length > 1) this.print("(");
        for(int i = 0; i < values.length; i++) {
            if(i > 0) this.print(" ");
            try {
                if(values[i].startsWith("\"")) {
                    this.print(MimeUtility.encodeText(values[i], "UTF-8", null));
                }
                else {
                    this.print("\"" + MimeUtility.encodeText(values[i].trim(), "UTF-8", null) + "\"");                
                }
            } 
            catch(UnsupportedEncodingException e) {}
        }
        if(nested && values.length > 1) this.print(")");
    }
    
    /**
     * Print mime message to response stream.
     * 
     * @param message
     * @throws MessagingException
     */
    protected void printMessageStructure(
        Message message
    ) throws MessagingException {
        try {
            this.print("(");
            if(message.getContent() instanceof Multipart) {
                Multipart mp = (Multipart)message.getContent();
                for(int i = 0; i < mp.getCount(); i++) {
                    this.printBodyPartStructure(mp.getBodyPart(i));
                }
                if(message.getContentType() == null) {
                    this.print(" NIL");
                } else {
                    String[] contentType = new String[0];
                    try {
                        message.getHeader("Content-Type")[0].split(";");
                    } 
                    catch(Exception e) { /* don't care if something is wrong when fetching the content type */ }
                    this.print(" \"mixed\"");
                    if(contentType.length > 1) {
                        this.print(" ");
                        int pos = contentType[1].indexOf("=");
                        this.printList(
                            new String[]{
                                contentType[1].substring(0, pos),
                                contentType[1].substring(pos + 1)                                
                            },
                            true
                        );
                    }
                }
                this.print(" NIL NIL");
            } else if(message.getContent() instanceof BodyPart) {
                this.printBodyPartStructure(
                    (BodyPart)message.getContent()
                );
            }
            this.print(")");
        } catch (Exception e) {
            new ServiceException(e).log();
        }
    }

    /**
     * Print mime body part to response stream.
     * 
     * @param part
     * @throws MessagingException
     */
    protected void printBodyPartStructure(
        BodyPart part
    ) throws MessagingException {
        this.print("(");
        String[] contentType = part.getContentType().split(";");        
        if(contentType.length > 0) {
            String[] mimeType = contentType[0].split("/");
            this.printList(mimeType, false);
        }
        if(contentType.length > 1) {
            this.print(" ");
            int pos = contentType[1].indexOf("=");
            this.printList(
                new String[]{
                    contentType[1].substring(0, pos),
                    contentType[1].substring(pos + 1)
                },
                true
            ); 
        }
        this.print(" NIL NIL ");
        this.printList(part.getHeader("Content-Transfer-Encoding"), true);
        if(part.getSize() > 0) {
            this.print(" " + part.getSize());
        } else {
            this.print(" NIL");
        }
        this.print(" NIL");
        if(part.getDisposition() == null) {
            this.print(" NIL");
        } else {
            this.print(" (");
            String[] disposition = part.getHeader("Content-Disposition")[0].split(";");            
            for(int i = 0; i < disposition.length; i++) {
                if(i > 0) this.print(" ");
                int pos = disposition[i].indexOf("=");
                if(pos > 0) {
                    this.printList(
                        new String[]{
                            disposition[i].substring(0, pos),
                            disposition[i].substring(pos + 1)                     
                        }, 
                        true
                    );
                } else {
                    this.printList(new String[]{disposition[i]}, true);
                }
            }
            this.print(")");
        }                                           
        this.print(" NIL");
        this.print(")");
    }

    /**
     * Print addresses to response stream.
     * 
     * @param addresses
     * @param max
     */
    protected void printAddresses(
        Address[] addresses,
        int max
    ) {
        if(addresses == null) {
            this.print(" ((NIL NIL NIL NIL))");            
        } else {
            int n = 0;
            for(Address address: addresses) {
                if(address instanceof InternetAddress) {
                    InternetAddress inetAddress = (InternetAddress)address;
                    int pos = inetAddress.getAddress().indexOf("@");
                    String personalName = inetAddress.getPersonal();
                    if(personalName != null) {
                        if(personalName.startsWith("'")) {
                            personalName = personalName.substring(1);
                        }
                        if(personalName.endsWith("'")) {
                            personalName = personalName.substring(0, personalName.length() - 1);
                        }
                    }
                    if(pos > 0) {
                        this.print(" ((NIL NIL \"" + inetAddress.getAddress().substring(0, pos) + "\" \"" + inetAddress.getAddress().substring(pos + 1) + "\"))");
                    } else {
                        this.print(" ((NIL NIL \"" + inetAddress.getAddress() + "\" NIL))");                        
                    }
                }
                n++;
                if(n >= max) break;
            }
        }
    }
    
    /**
     * Return true if socket is connected.
     * 
     * @return
     */
    public boolean isConnected(
    ) {
        return (this.socket.isConnected());
    }

    //-----------------------------------------------------------------------
    // Members
    //-----------------------------------------------------------------------
    private static final int MAX_LINE_LENGTH = 2048;
    
    public static final long MAX_ACTIVITY_NUMBER = 9999999999L;
    
    protected OutputStream out = null;
    protected InputStream in = null;
    protected SessionState state = SessionState.NOT_AUTHENTICATED;
    protected IMAPFolderImpl selectedFolder = null;

    private static ThreadLocal<QuotaByteArrayOutputStream> byteOutputStreams = new ThreadLocal<QuotaByteArrayOutputStream>() {
        protected synchronized QuotaByteArrayOutputStream initialValue() {
            return new QuotaByteArrayOutputStream(IMAPSession.class.getName());
        }
    };
    
}
