##### Base configuration #####
FROM php:7.4-apache as base

# Meta data
LABEL authors = "support@cmclinnovations.com"
LABEL version = "0.0.1"
LABEL description = "KG Website"

# Enable Apache Rewrite + Expires Module
RUN a2enmod rewrite expires && \
    sed -i 's/ServerTokens OS/ServerTokens ProductOnly/g' \
    /etc/apache2/conf-available/security.conf
	
# Install dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    unzip \
    libfreetype6-dev \
    libjpeg62-turbo-dev \
    libpng-dev \
    libyaml-dev \
    libzip4 \
    libzip-dev \
    zlib1g-dev \
    libicu-dev \
    g++ \
    git \
    cron \
    vim \
    && docker-php-ext-install opcache \
    && docker-php-ext-configure intl \
    && docker-php-ext-install intl \
    && docker-php-ext-configure gd --with-freetype --with-jpeg \
    && docker-php-ext-install -j$(nproc) gd \
    && docker-php-ext-install zip \
    && rm -rf /var/lib/apt/lists/*
	
# Set recommended PHP.ini settings
RUN { \
    echo 'opcache.memory_consumption=128'; \
    echo 'opcache.interned_strings_buffer=8'; \
    echo 'opcache.max_accelerated_files=4000'; \
    echo 'opcache.revalidate_freq=2'; \
    echo 'opcache.fast_shutdown=1'; \
    echo 'opcache.enable_cli=1'; \
    echo 'upload_max_filesize=128M'; \
    echo 'post_max_size=128M'; \
    echo 'expose_php=off'; \
    } > /usr/local/etc/php/conf.d/php-recommended.ini

# Install user cache and YAML support
RUN pecl install apcu \
    && pecl install yaml-2.0.4 \
    && docker-php-ext-enable apcu yaml
	
# Install sendmail
RUN apt-get update && apt-get install -y sendmail sendmail-cf m4

# Install bash
RUN apt-get update && apt-get install -y bash

# Copy in site files
COPY site /var/www/html/

# Update permissions
RUN chown -R www-data:www-data /var/www/
RUn chmod -R 775 /var/www/