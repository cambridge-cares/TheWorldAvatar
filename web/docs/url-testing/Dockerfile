FROM nginx:mainline-alpine

# Installations
RUN apk update && apk --no-cache add nano openssl

# Copy in custom NGINX configs
COPY ./nginx/redirect.conf /etc/nginx/templates/redirect.conf.template
COPY ./nginx/nginx.conf /etc/nginx/nginx.conf

# Generate self-signed certificate
WORKDIR /etc/nginx/conf.d
COPY ./nginx/certificate.sh ./
RUN ./certificate.sh

# Serve static content for debugging
RUN mkdir -p /var/www/html
COPY ./nginx/index.html /var/www/html/index.html
RUN chmod 775 /var/www/html

# Expose port 80
EXPOSE 80