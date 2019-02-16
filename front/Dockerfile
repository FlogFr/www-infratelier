FROM debian:stretch-slim
MAINTAINER "florian.grignon@gmail.com"

# add the front into the /var/www directory
ADD ./www/ /var/www
RUN rm -fr /var/www/dist
ADD ./dist /var/www/dist

CMD ["sleep", "infinity"]
