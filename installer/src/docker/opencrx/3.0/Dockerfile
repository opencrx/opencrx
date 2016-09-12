FROM dockerfile/java:openjdk-7-jdk
MAINTAINER docker@opencrx.org
ENV VERSION 3.0.0
ENV PLATFORM jre-1.7
ENV HOME /root

# Install ant, apache2, nullmailer
RUN apt-get update && apt-get install -y \
  ant \
  apache2 \
  libapache2-mod-jk \
  nullmailer

# Do not expose port 8080 of Tomcat --> front Tomcat with Apache using AJP
RUN echo "<VirtualHost *:80>" > /etc/apache2/sites-available/000-default.conf \
  && echo "  ServerAdmin webmaster@localhost" >> /etc/apache2/sites-available/000-default.conf \
  && echo "  DocumentRoot /var/www/html" >> /etc/apache2/sites-available/000-default.conf \
  && echo "  LogLevel info" >> /etc/apache2/sites-available/000-default.conf \
  && echo "  ErrorLog \${APACHE_LOG_DIR}/error.log" >> /etc/apache2/sites-available/000-default.conf \
  && echo "  CustomLog \${APACHE_LOG_DIR}/access.log combined" >> /etc/apache2/sites-available/000-default.conf \
  && echo "  JkMount /* ajp13_worker" >> /etc/apache2/sites-available/000-default.conf \
  && echo "</VirtualHost>" >> /etc/apache2/sites-available/000-default.conf \
  && apache2ctl restart

# Get and install openCRX
RUN mkdir $HOME/opt \
  && mkdir $HOME/temp \
  && wget -O $HOME/temp/opencrxServer-$VERSION-installer.$PLATFORM.jar http://heanet.dl.sourceforge.net/project/opencrx/opencrx/$VERSION/opencrxServer-$VERSION-installer.$PLATFORM.jar \
  && echo "INSTALL_PATH=$HOME/opt/opencrx" > $HOME/temp/opencrx-install.properties \
  && echo "ANTPath=/usr/bin/ant" >> $HOME/temp/opencrx-install.properties \
  && java -jar $HOME/temp/opencrxServer-$VERSION-installer.$PLATFORM.jar -options $HOME/temp/opencrx-install.properties \
  && ls -al $HOME/opt/opencrx \
  && rm -rf $HOME/temp

VOLUME ["/root/opt/opencrx/data", "/root/opt/opencrx/apache-tomee-webprofile-1.7.2/logs", "/root/opt/opencrx/apache-tomee-webprofile-1.7.2/temp", "/root/opt/opencrx/apache-tomee-webprofile-1.7.2/work", "/root/opt/opencrx/apache-tomee-webprofile-1.7.2/airsyncdir", "/root/opt/opencrx/apache-tomee-webprofile-1.7.2/maildir"]
COPY ./docker-entrypoint.sh /
RUN chmod a+x /docker-entrypoint.sh
ENTRYPOINT ["/docker-entrypoint.sh"]
EXPOSE 80 8001 8009
CMD cd /root/opt/opencrx/apache-tomee-webprofile-1.7.2/bin;./opencrx.sh run
