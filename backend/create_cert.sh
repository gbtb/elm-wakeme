#!/bin/sh
#https://www.humankode.com/asp-net-core/develop-locally-with-https-self-signed-certificates-and-asp-net-cor
sudo openssl req -x509 -nodes -days 365 -newkey rsa:2048 -keyout localhost.key -out localhost.crt -config localhost.conf -passin pass:SslPassword
sudo openssl pkcs12 -export -out localhost.pfx -inkey localhost.key -in localhost.crt
#del existed
certutil -D -d sql:${HOME}/.pki/nssdb -n "localhost"
certutil -d sql:$HOME/.pki/nssdb -A -t "P,," -n "localhost" -i localhost.crt
certutil -L -d sql:${HOME}/.pki/nssdb