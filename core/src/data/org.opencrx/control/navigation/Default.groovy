import org.openmdx.portal.servlet.*;
import org.openmdx.portal.servlet.texts.*;
import org.openmdx.portal.servlet.component.*;
import org.openmdx.portal.servlet.control.*;

p.write("<div id=\"breadcrum\">");
org.openmdx.portal.servlet.control.NavigationControl.paintClose(p, forEditing);
org.openmdx.portal.servlet.control.NavigationControl.paintNext(p, forEditing);
org.openmdx.portal.servlet.control.NavigationControl.paintPrev(p, forEditing);
org.openmdx.portal.servlet.control.NavigationControl.paintPrint(p, forEditing);
org.openmdx.portal.servlet.control.NavigationControl.paintHeaderHider(p, forEditing);
org.openmdx.portal.servlet.control.NavigationControl.paintSelectPerspectives(p, forEditing);
org.opencrx.kernel.portal.control.NavigationControl.paintShareButton(p, forEditing);
org.opencrx.kernel.portal.control.NavigationControl.paintAlertBox(p, forEditing);
org.openmdx.portal.servlet.control.NavigationControl.paintBreadcrum(p, forEditing);
p.write("</div> <!-- breadcrum -->");
