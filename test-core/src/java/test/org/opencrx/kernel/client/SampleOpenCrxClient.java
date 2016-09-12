package test.org.opencrx.kernel.client;
import javax.jdo.PersistenceManager;
import javax.jdo.PersistenceManagerFactory;
import javax.naming.NamingException;

import org.openmdx.base.exception.ServiceException;
import org.openmdx.base.naming.Path;

/**
 * Sample openCRX client program.
 *
 */
public class SampleOpenCrxClient {

	public static void main(
		String[] args
	) throws NamingException, ServiceException {
		String connectionUrl = "http://127.0.0.1:8080/opencrx-rest-CRX/";
		String userName = "admin-Standard";
		String password = "admin-Standard";
		PersistenceManagerFactory pmf = org.opencrx.kernel.utils.Utils.getPersistenceManagerFactoryProxy(
			connectionUrl,
			userName,
			password,
			"application/vnd.openmdx.wbxml" // or 'text/xml' for plain xml protocol
		);
		PersistenceManager pm = pmf.getPersistenceManager(userName, null);
		org.opencrx.kernel.account1.jmi1.Segment accountSegment = 
			(org.opencrx.kernel.account1.jmi1.Segment)pm.getObjectById(
				new Path("xri://@openmdx*org.opencrx.kernel.account1/provider/CRX/segment/Standard")
			);
		org.opencrx.kernel.account1.cci2.ContactQuery contactQuery = 
			(org.opencrx.kernel.account1.cci2.ContactQuery)pm.newQuery(org.opencrx.kernel.account1.jmi1.Contact.class);
		contactQuery.orderByFullName().ascending();
		contactQuery.thereExistsFullName().like("G.*");
		int count = 0;
		for (org.opencrx.kernel.account1.jmi1.Contact contact : accountSegment.<org.opencrx.kernel.account1.jmi1.Contact> getAccount(contactQuery)) {
			System.out.println(contact.refGetPath().toXRI() + ": " + contact.getFullName());
			count++;
			if (count > 100) {
				break;
			}
		}
		pm.close();
	}

}
