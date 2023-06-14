FROM nginx:stable-alpine
COPY nginx.conf /etc/nginx/nginx.conf

RUN mkdir -p /var/www/html/static/
COPY ./static /var/www/html/static/
