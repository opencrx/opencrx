#!/bin/bash
ifconfig
apache2ctl restart
/etc/init.d/nullmailer restart

exec "$@"
